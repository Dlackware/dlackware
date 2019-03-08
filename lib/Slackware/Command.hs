{-# LANGUAGE OverloadedStrings #-}
module Slackware.Command ( build
                         , downloadSource
                         , install
                         ) where

import Slackware.Arch ( grepSlackBuild
                      , uname
                      )
import Slackware.CompileOrder ( Step(..)
                              , parseCompileOrder
                              )
import Slackware.Log ( Level(..)
                     , console
                     )
import qualified Slackware.Config as Config
import Slackware.Package
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Exception ( IOException
                         , try
                         )
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Crypto.Hash ( Digest
                   , MD5
                   , hashlazy
                   )
import Data.Default.Class (def)
import Data.Either (fromRight)
import Data.Foldable ( foldlM
                     , foldrM
                     )
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Network.HTTP.Req ( HttpException
                        , Req
                        , runReq
                        )
import Slackware.Info ( parseInfoFile
                      , PackageInfo(..)
                      )
import Slackware.Download ( filename
                          , get
                          )
import Slackware.Error (PackageError(..))
import System.Directory ( createDirectoryIfMissing
                        , doesFileExist
                        , getCurrentDirectory
                        , setCurrentDirectory
                        )
import System.Environment (getEnvironment)
import System.Exit ( ExitCode(..)
                   , exitFailure
                   )
import System.FilePath ( FilePath
                       , (</>)
                       , (<.>)
                       , joinPath
                       , takeDirectory
                       )
import System.IO.Error (tryIOError)
import System.Process ( CreateProcess(..)
                      , StdStream(..)
                      , CmdSpec(..)
                      , createProcess
                      , readProcess
                      , waitForProcess
                      , callProcess
                      )
import Text.Megaparsec (parse)

md5sum :: BSL.ByteString -> Digest MD5
md5sum = hashlazy

runSlackBuild :: FilePath -> [(String, String)] -> IO CreateProcess
runSlackBuild slackBuild environment = do
    old <- getEnvironment

    return $ CreateProcess
        { cmdspec = RawCommand ("." </> slackBuild) []
        , cwd = Nothing
        , env = Just $ mappend old environment
        , std_in = Inherit
        , std_out = Inherit
        , std_err = Inherit
        , close_fds = False
        , create_group = False
        , delegate_ctlc = False
        , detach_console = False
        , create_new_console = False
        , new_session = False
        , child_group = Nothing
        , child_user = Nothing
        , use_process_jobs = False
        }

installpkg :: String -> String -> ExceptT PackageError (ReaderT PackageEnvironment IO) ()
installpkg old fullPkgName =
    tryIO >>= either (throwE . InstallError) return
    where
        tryIO = liftIO $ tryIOError callProcess'
        fullPath = "/var/cache/dlackware/" ++ fullPkgName ++ ".txz"
        callProcess' = callProcess "/sbin/upgradepkg"
            ["--reinstall", "--install-new", old ++ fullPath]

buildFullPackageName :: String -> PackageInfo -> String -> String -> String
buildFullPackageName pkgName pkg arch buildNumber
    = pkgName ++ "-" ++ version pkg
    ++ "-" ++ arch
    ++ "-" ++ buildNumber ++ "_dlack"

buildPackage :: PackageAction
buildPackage pkg (old, pkgName) = do
    unameM' <- lift $ asks unameM
    let slackBuild = pkgName <.> "SlackBuild"
    (buildNumber, arch) <- grepSlackBuild unameM' <$> liftIO (T.IO.readFile slackBuild)

    let fullPkgName = buildFullPackageName pkgName pkg arch buildNumber
    let pkgtoolsDb = "/var/lib/pkgtools/packages/" ++ fullPkgName

    alreadyInstalled <- liftIO $ doesFileExist pkgtoolsDb
    if alreadyInstalled
    then return ()
    else do
        liftIO $ console Info $ T.append "Building package " $ T.pack pkgName

        downloadPackageSource pkg (old, pkgName)
        (_, _, _, processHandle)
            <- liftIO $ runSlackBuild slackBuild [("VERSION", version pkg)]
            >>= createProcess

        code <- liftIO $ waitForProcess processHandle
        case code of
            ExitSuccess -> installpkg old fullPkgName
            _ -> throwE BuildError

installPackage :: PackageAction
installPackage pkg (old, pkgName) = do
    let slackBuild = pkgName <.> "SlackBuild"
    unameM' <- lift $ asks unameM
    (buildNumber, arch) <- grepSlackBuild unameM' <$> liftIO (T.IO.readFile slackBuild)

    installpkg old $ buildFullPackageName pkgName pkg arch buildNumber

downloadPackageSource :: PackageAction
downloadPackageSource pkg (_, pkgName) = do
    liftIO $ console Info $ T.append "Downloading the sources for " $ T.pack pkgName

    downloadUrls
        <- let f (x, y) acc = case get x of
                (Just x') -> do
                    checksumOrE <- liftIO $ tryReadChecksum x
                    return $ case checksumOrE of
                        (Right checksum) | checksum == y -> acc
                        _ -> (x', y) : acc
                Nothing -> throwE UnsupportedDownload
            in foldrM f [] $ zip (downloads pkg) (checksums pkg)

    caught <- liftIO $ tryDownload $ fst <$> downloadUrls
    sums <- either (throwE . DownloadError) return caught
    when (sums /= (snd <$> downloadUrls)) $ throwE ChecksumMismatch

    where tryDownload :: [Req (Digest MD5)] -> IO (Either HttpException [Digest MD5])
          tryDownload = try . mapM (runReq def)
          tryReadChecksum :: C8.ByteString -> IO (Either IOException (Digest MD5))
          tryReadChecksum = try . fmap md5sum . BSL.readFile . C8.unpack . filename

doCompileOrder :: String -> Config.Config -> PackageAction -> String -> IO ()
doCompileOrder unameM' config action compileOrder = do
    content <- C8.readFile compileOrder

    maybeError <- foldlM packageAction (Right ()) $ packageList content
    case maybeError of
      Left message -> console Fatal (T.pack $ show message) >> exitFailure
      Right () -> return ()

    where
        packageList content = fromRight [] $ parseCompileOrder compileOrder content
        packageAction (Left x) = const $ return $ Left x
        packageAction _ = flip runReaderT (PackageEnvironment unameM' config)
                        . runExceptT
                        . doPackage action (takeDirectory compileOrder)

doPackage :: PackageAction
          -> FilePath
          -> Step
          -> ExceptT PackageError (ReaderT PackageEnvironment IO) ()
doPackage packageAction repo step = do
    oldDirectory <- liftIO getCurrentDirectory
    liftIO $ setCurrentDirectory $ repo </> pkgName

    let infoFile = joinPath [repo, pkgName, pkgName <.> "info"]
    content <- liftIO $ C8.readFile infoFile

    pkg <- case parse parseInfoFile infoFile content of
        Left left -> throwE $ ParseError left
        Right pkg -> return pkg

    packageAction pkg exploded

    liftIO $ setCurrentDirectory oldDirectory
    where
        explodePackageName (PackageName Nothing new) = ("", C8.unpack new)
        explodePackageName (PackageName (Just old) new) = (C8.unpack old ++ "%", C8.unpack new)
        exploded = explodePackageName step
        pkgName = snd exploded

readConfiguration :: IO Config.Config
readConfiguration = do
    configContent <- BSL.readFile "etc/dlackware.yaml"
    let config = fromRight undefined $ Config.parseConfig configContent

    createDirectoryIfMissing True $ T.unpack $ Config.loggingDirectory config
    return config

collectRunInformation :: IO (String, Config.Config)
collectRunInformation = do
    config <- readConfiguration
    unameM' <- uname <$> readProcess "/usr/bin/uname" ["-m"] ""
    return (unameM', config)

getCompileOrders :: Config.Config -> [FilePath]
getCompileOrders config =
    let f x = T.unpack (Config.reposRoot config) </> T.unpack x
     in fmap f (Config.repos config)

build :: IO ()
build = do
    (unameM', config) <- collectRunInformation
    createDirectoryIfMissing False $ T.unpack $ Config.temporaryDirectory config

    let compileOrders = getCompileOrders config
     in mapM_ (doCompileOrder unameM' config buildPackage) compileOrders

downloadSource :: IO ()
downloadSource = do
    (unameM', config) <- collectRunInformation

    let compileOrders = getCompileOrders config
     in mapM_ (doCompileOrder unameM' config downloadPackageSource) compileOrders

install :: IO ()
install = do
    (unameM', config) <- collectRunInformation

    let compileOrders = getCompileOrders config
     in mapM_ (doCompileOrder unameM' config installPackage) compileOrders
