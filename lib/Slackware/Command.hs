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
import Config ( Config(..)
              , parseConfig
              )
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Exception ( IOException
                         , try
                         )
import Control.Monad.Trans.Except ( ExceptT(..)
                                  , runExceptT
                                  , throwE
                                  , withExceptT
                                  )
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
import Slackware.Package ( parseInfoFile
                         , Package(..)
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

type PackageAction = Package -> (String, String) -> ExceptT PackageError IO ()

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

installpkg :: String -> String -> ExceptT PackageError IO ()
installpkg old fullPkgName = withExceptT InstallError $ tryIO callProcess'
    where
        tryIO = ExceptT . tryIOError
        fullPath = "/var/cache/dlackware/" ++ fullPkgName ++ ".txz"
        callProcess' = callProcess "/sbin/upgradepkg"
            ["--reinstall", "--install-new", old ++ fullPath]

buildFullPackageName :: String -> Package -> String -> String -> String
buildFullPackageName pkgName pkg arch buildNumber
    = pkgName ++ "-" ++ version pkg
    ++ "-" ++ arch
    ++ "-" ++ buildNumber ++ "_dlack"

buildPackage :: String -> PackageAction
buildPackage unameM pkg (old, pkgName) = do
    let slackBuild = pkgName <.> "SlackBuild"
    (buildNumber, arch) <- grepSlackBuild unameM <$> liftIO (T.IO.readFile slackBuild)

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

installPackage :: String -> PackageAction
installPackage unameM pkg (old, pkgName) = do
    let slackBuild = pkgName <.> "SlackBuild"
    (buildNumber, arch) <- grepSlackBuild unameM <$> liftIO (T.IO.readFile slackBuild)

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
    sums <- case caught of
        Left e -> throwE $ DownloadError e
        Right unit -> return unit

    when (sums /= (snd <$> downloadUrls)) $ throwE ChecksumMismatch

    where tryDownload :: [Req (Digest MD5)] -> IO (Either HttpException [Digest MD5])
          tryDownload = try . mapM (runReq def)
          tryReadChecksum :: C8.ByteString -> IO (Either IOException (Digest MD5))
          tryReadChecksum = try . fmap md5sum . BSL.readFile . C8.unpack . filename

doCompileOrder :: PackageAction -> String -> IO ()
doCompileOrder action compileOrder = do
    content <- C8.readFile compileOrder

    maybeError <- foldlM packageAction (Right ()) $ packageList content
    case maybeError of
      Left message -> console Fatal (T.pack $ show message) >> exitFailure
      _ -> return ()

    where
        packageList content = fromRight [] $ parseCompileOrder compileOrder content
        packageAction (Left x) = const $ return $ Left x
        packageAction _ = runExceptT . doPackage action (takeDirectory compileOrder)

doPackage :: PackageAction
          -> FilePath
          -> Step
          -> ExceptT PackageError IO ()
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

getCompileOrders :: IO [FilePath]
getCompileOrders = do
    configContent <- BSL.readFile "etc/dlackware.yaml"
    let config = fromRight undefined $ parseConfig configContent
    let f x = T.unpack (reposRoot config) </> T.unpack x
    return $ fmap f (repos config)

build :: IO ()
build = do
    createDirectoryIfMissing False "/tmp/dlackware"
    compileOrders <- getCompileOrders
    unameM <- uname <$> readProcess "/usr/bin/uname" ["-m"] ""
    mapM_ (doCompileOrder $ buildPackage unameM) compileOrders

downloadSource :: IO ()
downloadSource = do
    compileOrders <- getCompileOrders
    mapM_ (doCompileOrder downloadPackageSource) compileOrders

install :: IO ()
install = do
    compileOrders <- getCompileOrders
    unameM <- uname <$> readProcess "/usr/bin/uname" ["-m"] ""
    mapM_ (doCompileOrder $ installPackage unameM) compileOrders
