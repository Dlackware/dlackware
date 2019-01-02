module Slackware.Command ( build
                         , downloadSource
                         , install
                         ) where

import           Arch ( grepSlackBuild
                      , uname
                      )
import           CompileOrder ( Step(..)
                              , parseCompileOrder
                              )
import           Config ( Config(..)
                        , parseConfig
                        )
import Control.Monad.IO.Class (liftIO)
import           Control.Exception (try)
import Control.Monad.Trans.Except ( ExceptT(..)
                                  , runExceptT
                                  , throwE
                                  , withExceptT
                                  )
import           Crypto.Hash ( hashlazy
                             , Digest
                             , MD5
                             )
import           Data.Default.Class (def)
import           Data.Either (fromRight)
import Data.Foldable (foldrM)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Network.HTTP.Req ( HttpException
                                  , runReq
                                  )
import           Slackware.Package ( parseInfoFile
                                   , Package(..)
                                   )
import           Slackware.Download (get)
import System.Directory ( createDirectoryIfMissing
                        , getCurrentDirectory
                        , setCurrentDirectory
                        )
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
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

type PackageAction = FilePath -> (String, String) -> ExceptT String IO ()

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

md5sum :: BSL.ByteString -> Digest MD5
md5sum = hashlazy

installpkg :: (String, String) -> Package -> String -> String -> ExceptT String IO ()
installpkg (old, pkgName) pkg arch buildNumber = withExceptT show $ tryIO callProcess'
    where
        tryIO = ExceptT . tryIOError
        fullPath = "/var/cache/dlackware/" ++ pkgName ++ "-" ++ (version pkg)
            ++ "-" ++ arch ++ "-" ++ buildNumber ++ "_dlack.txz"
        callProcess' = callProcess "/sbin/upgradepkg"
            ["--reinstall", "--install-new", old ++ fullPath]

buildPackage :: PackageAction
buildPackage repo (old, pkgName) = do
    liftIO $ putStrLn $ "Building package " ++ pkgName

    let infoFile = joinPath [repo, pkgName, pkgName <.> "info"]
    content <- liftIO $ C8.readFile infoFile

    pkg <- case parse parseInfoFile infoFile content of
        Left left -> throwE $ show left
        Right pkg -> return pkg

    let slackBuild = pkgName <.> "SlackBuild"

    oldDirectory <- liftIO $ getCurrentDirectory
    liftIO $ setCurrentDirectory $ repo </> pkgName

    let tarballs = downloads pkg

    downloadUrls
        <- case foldrM (\x -> ((:) <$> x <*>) . pure) [] (get <$> tarballs) of
            Nothing -> throwE ""
            Just downloadUrls -> return downloadUrls

    caught <- liftIO ((try (mapM_ (runReq def) downloadUrls)) :: IO (Either HttpException ()))
    _ <- case caught of
        Left e -> throwE $ show e
        Right unit -> return unit

    let filenames = (C8.unpack . snd . (C8.breakEnd ('/' ==))) <$> tarballs
    sums <- liftIO $ mapM (fmap md5sum . BSL.readFile) filenames

    _ <- if (show <$> sums) /= (C8.unpack <$> checksums pkg)
        then throwE "Checksum mismatch"
        else return ()

    (buildNumber, archNoarch) <- liftIO $ grepSlackBuild <$> (readFile slackBuild)
    unameM <- liftIO $ readProcess "/usr/bin/uname" ["-m"] ""
    let arch = if (length archNoarch) > 1
            then archNoarch
            else uname unameM

    (_, _, _, processHandle) <- liftIO $ (runSlackBuild slackBuild [("VERSION", version pkg)]) >>= createProcess
    code <- liftIO $ waitForProcess processHandle

    case code of
        ExitSuccess -> do
            liftIO $ setCurrentDirectory oldDirectory
            installpkg (old, pkgName) pkg arch buildNumber

        _ -> throwE ""

installPackage :: PackageAction
installPackage repo (old, pkgName) = do
    let infoFile = joinPath [repo, pkgName, pkgName <.> "info"]
    content <- liftIO $ C8.readFile infoFile

    pkg <- case parse parseInfoFile infoFile content of
                Left left -> throwE $ show left
                Right pkg -> return pkg

    let slackBuild = pkgName <.> "SlackBuild"

    oldDirectory <- liftIO getCurrentDirectory
    liftIO $ setCurrentDirectory $ repo </> pkgName

    (buildNumber, archNoarch) <- grepSlackBuild <$> (liftIO $ readFile slackBuild)
    unameM <- liftIO $ readProcess "/usr/bin/uname" ["-m"] ""
    let arch = if (length archNoarch) > 1
            then archNoarch
            else uname unameM

    liftIO $ setCurrentDirectory oldDirectory
    installpkg (old, pkgName) pkg arch buildNumber

downloadPackageSource :: PackageAction
downloadPackageSource repo (_, pkgName) = do
    let infoFile = joinPath [repo, pkgName, pkgName <.> "info"]
    content <- liftIO $ C8.readFile infoFile

    pkg <- case parse parseInfoFile infoFile content of
                Left left -> throwE $ show left
                Right pkg -> return pkg

    liftIO $ do
        setCurrentDirectory $ repo </> pkgName
        mapM_ ((runReq def) . fromJust . get) (downloads pkg)

doCompileOrder :: PackageAction -> String -> IO ()
doCompileOrder doPackage compileOrder = do
    content <- C8.readFile compileOrder

    maybeError <- f $ fromRight [] $ parseCompileOrder compileOrder content
    case maybeError of
      Left message -> fail $ "Build errored: " ++ message
      _ -> return ()

    where
        f [] = return $ Right ()
        f (x:xs) = do
            maybeError <- runExceptT $ packageAction $ explodePackageName x
            case maybeError of 
                Left err -> return $ Left err
                Right _ -> f xs
        explodePackageName (PackageName Nothing new) = ("", C8.unpack new)
        explodePackageName (PackageName (Just old) new) = ((C8.unpack old) ++ "%", C8.unpack new)
        packageAction = doPackage $ takeDirectory compileOrder

getCompileOrders :: IO [FilePath]
getCompileOrders = do
    configContent <- BSL.readFile "etc/dlackware.yaml"
    let config = fromRight undefined $ parseConfig configContent
    let f x = (T.unpack $ reposRoot config) </> (T.unpack x)
    return $ fmap f (repos config)

build :: IO ()
build = do
    createDirectoryIfMissing False "/tmp/dlackware"
    compileOrders <- getCompileOrders
    mapM_ (doCompileOrder buildPackage) compileOrders

downloadSource :: IO ()
downloadSource = do
    compileOrders <- getCompileOrders
    mapM_ (doCompileOrder downloadPackageSource) compileOrders

install :: IO ()
install = do
    compileOrders <- getCompileOrders
    mapM_ (doCompileOrder installPackage) compileOrders
