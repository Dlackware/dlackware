{-# LANGUAGE OverloadedStrings #-}
module Slackware.Command
    ( build
    , downloadSource
    , install
    , readConfiguration
    , updateGnome
    ) where

import Conduit (ZipSink(..), liftIO, stdoutC, withSinkFile)
import Control.Monad (foldM, void)
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
import Data.YAML (Pos, decode)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Slackware.Info
import Slackware.Download
import Slackware.Error
import Slackware.Process (outErrProcess, runSlackBuild)
import qualified Slackware.Version as Version
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getCurrentDirectory
    , listDirectory
    , setCurrentDirectory
    )
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), (<.>), joinPath, splitExtension, takeDirectory)
import System.IO.Error (catchIOError, tryIOError)
import System.Process (readProcess, callProcess)
import Text.Megaparsec (errorBundlePretty, parse)
import Text.URI (URI(..), mkURI)

md5sum :: BSL.ByteString -> Digest MD5
md5sum = hashlazy

installpkg :: Text -> String -> ActionT ()
installpkg old fullPkgName = do
    processReturn <- liftIO $ tryIOError upgradepkg
    either (throw . PackageError fullPkgName . InstallError) pure processReturn
  where
    fullPath = concat
        [ Text.unpack old
        , "/var/cache/dlackware/"
        , fullPkgName
        , ".txz"
        ]
    upgradepkg = callProcess "/sbin/upgradepkg"
        ["--reinstall", "--install-new", fullPath]

buildFullPackageName :: PackageInfo -> String -> String -> String
buildFullPackageName pkg arch buildNumber
    = pkgname pkg ++ "-" ++ Text.unpack (version pkg)
    ++ "-" ++ arch
    ++ "-" ++ buildNumber ++ "_dlack"

upgradePackage :: Command
upgradePackage pkg _ = do
    versionMap <- asks versions
    streamMap <- asks streams
    let maybeToVersion = Map.lookup (Text.pack $ pkgname pkg) versionMap
    let maybeStream = Map.lookup (Text.pack $ pkgname pkg) streamMap

    case (maybeToVersion, maybeStream) of
        (Just toVersion, Just stream) -> do
            streamContents <- liftIO $ BSL.readFile stream
            let parsedStream = decode streamContents :: Either (Pos, String) [Version.BuildStream]

            case parsedStream of
                Right [buildStream] -> do
                    let newDownloads = replaceURL . Version.url <$> Version.sources buildStream
                    newChecksums <- liftIO $ fromJust $ downloadAll newDownloads

                    let infoFile = pkgname pkg <.> "info"
                    let newPackage = update pkg toVersion newDownloads newChecksums
                    if pkg == newPackage
                    then do
                        liftIO $ Text.IO.writeFile infoFile (generate newPackage)
                        pure True
                    else pure False
                _ -> pure False

        _ -> pure False
  where
    replaceURL = fromJust
        . mkURI
        . Text.replace "gnome_downloads:" "https://download.gnome.org/sources/"


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
        liftIO $ console Info $ Text.append "Building package " $ Text.pack $ pkgname pkg

        _ <- downloadPackageSource pkg old

        loggingDirectory' <- asks loggingDirectory
        let logFile = loggingDirectory'
                  </> (pkgname pkg ++ "-" ++ Text.unpack (version pkg) ++ ".log")
        code <- liftIO $ withSinkFile logFile $ \sink -> do
            let output = getZipSink $ ZipSink stdoutC *> ZipSink sink
            cp <- liftIO $ runSlackBuild slackBuild [("VERSION", Text.unpack $ version pkg)]
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
    liftIO $ console Info $ Text.append "Downloading the sources for " $ Text.pack $ pkgname pkg

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
          console Fatal $ Text.pack $ errorBundlePretty left
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
    liftIO $ setCurrentDirectory $ repo </> Text.unpack pkgName

    let infoFile = joinPath [repo, Text.unpack pkgName, Text.unpack pkgName <.> "info"]
    content <- liftIO $ C8.readFile infoFile

    pkg <- case parse parseInfoFile infoFile content of
        Left left -> throw $ PackageError (Text.unpack pkgName) $ ParseError left
        Right pkg -> return pkg
    evaluated <- command pkg (fst exploded)

    liftIO $ setCurrentDirectory oldDirectory

    pure evaluated
  where
    explodePackageName (PackageName Nothing new) = ("", new)
    explodePackageName (PackageName (Just old) new) = (Text.snoc old '%', new)
    exploded = explodePackageName step
    pkgName = snd exploded

readConfiguration :: IO Config.Config
readConfiguration = do
    configContent <- BSL.readFile Config.configPath
    config <- case Config.parseConfig Config.configPath configContent of
        Left x -> console Fatal x >> exitFailure
        Right x -> return x

    createDirectoryIfMissing True $ Text.unpack $ Config.loggingDirectory config
    return config

collectRunInformation :: IO Environment
collectRunInformation = do
    config <- readConfiguration
    unameM' <- uname <$> readProcess "/usr/bin/uname" ["-m"] ""
    versions' <- collectVersions
    currentDirectory <- getCurrentDirectory
    let streamRoot = currentDirectory </> "etc/gnome"
    streams' <- collectStreams streamRoot `catchIOError` const (pure mempty)
    pure $ Environment unameM' config versions' streams'
  where
    collectVersions = do
        let versionsFile = "etc/versions"
        versionsFileExist <- doesFileExist versionsFile
        if versionsFileExist
        then fromRight mempty
            . parse Version.versions versionsFile
            <$> Text.IO.readFile versionsFile
        else pure mempty
    collectStreams :: FilePath -> IO (Map Text FilePath)
    collectStreams directory = do
        rootDirectory <- listDirectory directory
        foldM (forEachFile directory) mempty rootDirectory
    forEachFile rootDirectory accumulator file = do
        let filePath = rootDirectory </> file
        isDirectory <- doesDirectoryExist filePath
        case isDirectory of
            True -> (accumulator <>) <$> collectStreams filePath
            False
                | (basename, ".bst") <- splitExtension file ->
                    pure $ Map.insert (Text.pack basename) filePath accumulator
                | otherwise -> pure accumulator

-- | Build all packages specified in the configuration.
build :: IO ()
build = do
    environment <- collectRunInformation
    createDirectoryIfMissing False $ temporaryDirectory environment

    traverse_ (buildCompileOrder environment) $ repositories environment
  where
    buildCompileOrder environment compileOrder =
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

updateGnome :: Maybe String -> IO ()
updateGnome _gnomeVersion = do
    environment <- collectRunInformation

    let upgradePackage' =
            flip runReaderT environment . doCompileOrder upgradePackage
     in traverse_ upgradePackage' $ repositories environment
