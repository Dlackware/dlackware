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
import           Crypto.Hash ( hashlazy
                             , Digest
                             , MD5
                             )
import           Data.Default.Class (def)
import           Data.Either (fromRight)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Network.HTTP.Req (runReq)
import           Slackware.Package ( parseInfoFile
                                   , Package(..)
                                   )
import           Slackware.Download (get)
import System.Directory ( createDirectoryIfMissing
                        , getCurrentDirectory
                        , setCurrentDirectory
                        )
import System.Environment (getEnvironment)
import System.FilePath ( FilePath
                       , (</>)
                       , (<.>)
                       , joinPath
                       , takeDirectory
                       )
import System.Process ( CreateProcess(..)
                      , StdStream(..)
                      , CmdSpec(..)
                      , createProcess
                      , readProcess
                      , waitForProcess
                      , callProcess
                      )
import Text.Megaparsec (parse)

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

buildPackage :: FilePath -> (String, String) -> IO ()
buildPackage repo (old, pkgName) = do
    let infoFile = joinPath [repo, pkgName, pkgName <.> "info"]
    content <- readFile infoFile

    let pkg = case parse parseInfoFile infoFile content of
                  Left left -> error $ show left
                  Right right -> right

    let slackBuild = pkgName <.> "SlackBuild"

    oldDirectory <- getCurrentDirectory
    setCurrentDirectory $ repo </> pkgName

    let tarballs = C8.pack <$> (downloads pkg)
    mapM_ ((runReq def) . fromJust . get) tarballs

    let filenames = (C8.unpack . snd . (C8.breakEnd ('/' ==))) <$> tarballs
    sums <- mapM (fmap md5sum . BSL.readFile) filenames
    if (show <$> sums) == (checksums pkg)
       then return ()
       else error "Checksum mismatch"

    (buildNumber, archNoarch) <- grepSlackBuild <$> (readFile slackBuild)
    unameM <- readProcess "/usr/bin/uname" ["-m"] ""
    let arch = if (length archNoarch) > 1
               then archNoarch
               else uname unameM

    let fullPath = "/var/cache/dlackware/" ++ pkgName ++ "-" ++ (version pkg)
                ++ "-" ++ arch ++ "-" ++ buildNumber ++ "_dlack.txz"

    (_, _, _, processHandle) <- (runSlackBuild slackBuild [("VERSION", version pkg)]) >>= createProcess
    _ <- waitForProcess processHandle

    setCurrentDirectory oldDirectory

    callProcess "/sbin/upgradepkg" ["--reinstall", "--install-new", old ++ fullPath]

installPackage :: FilePath -> (String, String) -> IO ()
installPackage repo (old, pkgName) = do
    let infoFile = joinPath [repo, pkgName, pkgName <.> "info"]
    content <- readFile infoFile

    let pkg = case parse parseInfoFile infoFile content of
                  Left left -> error $ show left
                  Right right -> right

    let slackBuild = pkgName <.> "SlackBuild"

    oldDirectory <- getCurrentDirectory
    setCurrentDirectory $ repo </> pkgName

    (buildNumber, archNoarch) <- grepSlackBuild <$> (readFile slackBuild)
    unameM <- readProcess "/usr/bin/uname" ["-m"] ""
    let arch = if (length archNoarch) > 1
               then archNoarch
               else uname unameM

    let fullPath = "/var/cache/dlackware/" ++ pkgName ++ "-" ++ (version pkg)
                ++ "-" ++ arch ++ "-" ++ buildNumber ++ "_dlack.txz"

    setCurrentDirectory oldDirectory

    callProcess "/sbin/upgradepkg" ["--reinstall", "--install-new", old ++ fullPath]

downloadPackageSource :: FilePath -> (String, String) -> IO ()
downloadPackageSource repo (_, pkgName) = do
    let infoFile = joinPath [repo, pkgName, pkgName <.> "info"]
    content <- readFile infoFile

    let pkg = case parse parseInfoFile infoFile content of
                  Left left -> error $ show left
                  Right right -> right

    setCurrentDirectory $ repo </> pkgName

    mapM_ ((runReq def) . fromJust . get) (C8.pack <$> downloads pkg)

doCompileOrder :: (FilePath -> (String, String) -> IO ()) -> String -> IO ()
doCompileOrder doPackage compileOrder = do
    content <- C8.readFile compileOrder

    mapM_ (doPackage $ takeDirectory compileOrder) (pkgs content)
        where pkgs content = foldr f [] $ fromRight [] $ parseCompileOrder compileOrder content
              f (PackageName Nothing new) acc = ("", C8.unpack new) : acc
              f (PackageName (Just old) new) acc = ((C8.unpack old) ++ "%", C8.unpack new) : acc

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
