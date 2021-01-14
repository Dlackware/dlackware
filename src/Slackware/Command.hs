{-# LANGUAGE OverloadedStrings #-}
module Slackware.Command
    ( build
    , downloadSource
    , install
    , readConfiguration
    ) where

import Conduit (ZipSink(..), liftIO, stdoutC, withSinkFile)
import Control.Monad (void)
import Slackware.Arch
import Slackware.CompileOrder
import Slackware.Log
import qualified Slackware.Config as Config
import Slackware.Package
import Control.Exception (IOException, throw, try)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Crypto.Hash (Digest, MD5, hashlazy)
import Data.Foldable (foldrM, traverse_)
import Data.Either (fromRight)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Text.IO
import Slackware.Info
import Slackware.Download
import Slackware.Error
import Slackware.Process (outErrProcess, runSlackBuild)
import qualified Slackware.Version as Version
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , getCurrentDirectory
    , setCurrentDirectory
    )
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), (<.>), joinPath, takeDirectory)
import System.IO.Error (tryIOError)
import System.Process (readProcess, callProcess)
import Text.Megaparsec (errorBundlePretty, parse)
import Text.URI (URI)

md5sum :: BSL.ByteString -> Digest MD5
md5sum = hashlazy

installpkg :: Text -> String -> ActionT ()
installpkg old fullPkgName = do
    processReturn <- liftIO $ tryIOError upgradepkg
    either (throw . PackageError fullPkgName . InstallError) pure processReturn
  where
    fullPath = concat
        [ T.unpack old
        , "/var/cache/dlackware/"
        , fullPkgName
        , ".txz"
        ]
    upgradepkg = callProcess "/sbin/upgradepkg"
        ["--reinstall", "--install-new", fullPath]

buildFullPackageName :: PackageInfo -> String -> String -> String
buildFullPackageName pkg arch buildNumber
    = pkgname pkg ++ "-" ++ T.unpack (version pkg)
    ++ "-" ++ arch
    ++ "-" ++ buildNumber ++ "_dlack"

buildPackage :: Command
buildPackage pkg old = do
    unameM' <- asks machine
    let slackBuild = pkgname pkg <.> "SlackBuild"
    (buildNumber, arch) <- grepSlackBuild unameM' <$> liftIO (Text.IO.readFile slackBuild)

    let fullPkgName = buildFullPackageName pkg arch buildNumber
    let pkgtoolsDb = "/var/lib/pkgtools/packages/" ++ fullPkgName

    alreadyInstalled <- liftIO $ doesFileExist pkgtoolsDb
    if alreadyInstalled
    then pure False
    else do
        liftIO $ console Info $ T.append "Building package " $ T.pack $ pkgname pkg

        _ <- downloadPackageSource pkg old

        loggingDirectory' <- asks loggingDirectory
        let logFile = loggingDirectory'
                  </> (pkgname pkg ++ "-" ++ T.unpack (version pkg) ++ ".log")
        code <- liftIO $ withSinkFile logFile $ \sink -> do
            let output = getZipSink $ ZipSink stdoutC *> ZipSink sink
            cp <- liftIO $ runSlackBuild slackBuild [("VERSION", T.unpack $ version pkg)]
            outErrProcess cp output

        case code of
            ExitSuccess -> installpkg old fullPkgName >> pure True
            _ -> throw $ PackageError (pkgname pkg) BuildError

installPackage :: Command
installPackage pkg old = do
    let slackBuild = pkgname pkg <.> "SlackBuild"
    unameM' <- asks machine
    (buildNumber, arch) <- grepSlackBuild unameM' <$> liftIO (Text.IO.readFile slackBuild)

    installpkg old $ buildFullPackageName pkg arch buildNumber
    pure True

downloadPackageSource :: Command
downloadPackageSource pkg _ = do
    liftIO $ console Info $ T.append "Downloading the sources for " $ T.pack $ pkgname pkg

    urls <- foldrM tryExistingDownload [] $ zip (downloads pkg) (checksums pkg)
    downloaded <- case downloadAll (fst <$> urls) of
        (Just request) -> return request
        Nothing -> throw $ PackageError (pkgname pkg) UnsupportedDownload

    caught <- liftIO $ try downloaded
    sums <- case caught of
        (Left e) -> throw $ PackageError (pkgname pkg) $ DownloadError e
        (Right result) -> return result

    if sums /= (snd <$> urls)
       then throw $ PackageError (pkgname pkg) ChecksumMismatch
       else pure True

  where
    tryExistingDownload (url, checksum) acc = do
        checksumOrE <- liftIO $ tryReadChecksum url
        case checksumOrE of
            (Right checksum') | checksum' == checksum -> return acc
            _ -> return $ (url, checksum) : acc
    tryReadChecksum :: URI -> IO (Either IOException (Digest MD5))
    tryReadChecksum = try . fmap md5sum . BSL.readFile . filename

doCompileOrder :: Command -> String -> ReaderT Environment IO Bool
doCompileOrder command compileOrder = do
    compileOrderPath' <- asks compileOrderPath
    content <- liftIO $ Text.IO.readFile compileOrderPath'

    packageList <- case parseCompileOrder compileOrderPath' content of
      Right right -> return right
      Left left -> liftIO $ do
          console Fatal $ T.pack $ errorBundlePretty left
          exitFailure

    or <$> action packageList
  where
    compileOrderPath = (</> compileOrder) . root
    action packageList = do
        compileOrderDirectory <- asks (takeDirectory . compileOrderPath)
        let doAction = doPackage command compileOrderDirectory
         in traverse doAction packageList

doPackage :: Command -> FilePath -> Step -> ActionT Bool
doPackage command repo step = do
    oldDirectory <- liftIO getCurrentDirectory
    liftIO $ setCurrentDirectory $ repo </> T.unpack pkgName

    let infoFile = joinPath [repo, T.unpack pkgName, T.unpack pkgName <.> "info"]
    content <- liftIO $ C8.readFile infoFile

    pkg <- case parse parseInfoFile infoFile content of
        Left left -> throw $ PackageError (T.unpack pkgName) $ ParseError left
        Right pkg -> return pkg
    evaluated <- command pkg (fst exploded)

    liftIO $ setCurrentDirectory oldDirectory

    pure evaluated
  where
    explodePackageName (PackageName Nothing new) = ("", new)
    explodePackageName (PackageName (Just old) new) = (T.snoc old '%', new)
    exploded = explodePackageName step
    pkgName = snd exploded

readConfiguration :: IO Config.Config
readConfiguration = do
    configContent <- BSL.readFile Config.configPath
    config <- case Config.parseConfig Config.configPath configContent of
        Left x -> console Fatal x >> exitFailure
        Right x -> return x

    createDirectoryIfMissing True $ T.unpack $ Config.loggingDirectory config
    return config

collectRunInformation :: IO Environment
collectRunInformation = do
    config <- readConfiguration
    unameM' <- uname <$> readProcess "/usr/bin/uname" ["-m"] ""
    Environment unameM' config <$> collectVersions
  where
    collectVersions = do
        let versionsFile = "etc/versions"
        versionsFileExist <- doesFileExist versionsFile
        if versionsFileExist
        then fromRight mempty
            . parse Version.versions versionsFile
            <$> Text.IO.readFile versionsFile
        else pure mempty

-- | Build all packages specified in the configuration.
build :: IO ()
build = do
    environment <- collectRunInformation
    createDirectoryIfMissing False $ temporaryDirectory environment

    traverse_ (buildCompileOrder environment) $ repositories environment
  where
    buildCompileOrder environment compileOrder = do
        void $ liftIO
            $ runReaderT (doCompileOrder buildPackage compileOrder) environment

-- | Only download the sources.
downloadSource :: IO ()
downloadSource = do
    environment <- collectRunInformation

    let downloadPackageSource' =
            flip runReaderT environment . doCompileOrder downloadPackageSource
     in traverse_ downloadPackageSource' $ repositories environment

-- | Install pre-built packages.
install :: IO ()
install = do
    environment <- collectRunInformation

    let installPackage' =
            flip runReaderT environment . doCompileOrder installPackage
     in traverse_ installPackage' $ repositories environment
