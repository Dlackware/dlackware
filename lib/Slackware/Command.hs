{-# LANGUAGE OverloadedStrings #-}
module Slackware.Command ( build
                         , downloadSource
                         , install
                         ) where

import Conduit
import Control.Monad (when)
import Prelude hiding (break)
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
import Control.Monad.IO.Class (liftIO)
import Control.Exception ( IOException
                         , try
                         )
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Crypto.Hash ( Digest
                   , MD5
                   , hashlazy
                   )
import Data.Foldable (foldrM, for_, traverse_)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Network.HTTP.Req ( HttpException
                        , Req
                        , defaultHttpConfig
                        , runReq
                        )
import Slackware.Info ( parseInfoFile
                      , PackageInfo(..)
                      )
import Slackware.Download ( filename
                          , get
                          )
import Slackware.Error
import Slackware.Process ( outErrProcess
                         , runSlackBuild
                         )
import System.Directory ( createDirectoryIfMissing
                        , doesFileExist
                        , getCurrentDirectory
                        , setCurrentDirectory
                        )
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
import System.Process ( readProcess
                      , callProcess
                      )
import Text.Megaparsec ( errorBundlePretty
                       , parse
                       )

md5sum :: BSL.ByteString -> Digest MD5
md5sum = hashlazy

installpkg
    :: Text
    -> String
    -> ActionT ()
installpkg old fullPkgName = do
    processReturn <- liftIO $ tryIOError upgradepkg
    either throw pure processReturn
  where
    throw = throwE . PackageError fullPkgName . InstallError
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
    unameM' <- lift $ asks unameM
    let slackBuild = pkgname pkg <.> "SlackBuild"
    (buildNumber, arch) <- grepSlackBuild unameM' <$> liftIO (T.IO.readFile slackBuild)

    let fullPkgName = buildFullPackageName pkg arch buildNumber
    let pkgtoolsDb = "/var/lib/pkgtools/packages/" ++ fullPkgName

    alreadyInstalled <- liftIO $ doesFileExist pkgtoolsDb
    if alreadyInstalled
    then pure False
    else do
        liftIO $ console Info $ T.append "Building package " $ T.pack $ pkgname pkg

        _ <- downloadPackageSource pkg old

        loggingDirectory' <- lift $ asks loggingDirectory
        let logFile = loggingDirectory'
                  </> (pkgname pkg ++ "-" ++ T.unpack (version pkg) ++ ".log")
        code <- liftIO $ withSinkFile logFile $ \sink -> do
            let output = getZipSink $ ZipSink stdoutC *> ZipSink sink
            cp <- liftIO $ runSlackBuild slackBuild [("VERSION", T.unpack $ version pkg)]
            outErrProcess cp output

        case code of
            ExitSuccess -> installpkg old fullPkgName >> pure True
            _ -> throwE $ PackageError (pkgname pkg) BuildError

installPackage :: Command
installPackage pkg old = do
    let slackBuild = pkgname pkg <.> "SlackBuild"
    unameM' <- lift $ asks unameM
    (buildNumber, arch) <- grepSlackBuild unameM' <$> liftIO (T.IO.readFile slackBuild)

    installpkg old $ buildFullPackageName pkg arch buildNumber
    pure True

downloadPackageSource :: Command
downloadPackageSource pkg _ = do
    liftIO $ console Info $ T.append "Downloading the sources for " $ T.pack $ pkgname pkg

    downloadUrls
        <- let f (x, y) acc = case get x of
                (Just x') -> do
                    checksumOrE <- liftIO $ tryReadChecksum x
                    return $ case checksumOrE of
                        (Right checksum) | checksum == y -> acc
                        _ -> (x', y) : acc
                Nothing -> throwE $ PackageError (pkgname pkg) UnsupportedDownload
            in foldrM f [] $ zip (downloads pkg) (checksums pkg)

    caught <- liftIO $ tryDownload $ fst <$> downloadUrls
    sums <- either (throwE . PackageError (pkgname pkg) . DownloadError) return caught
    if sums /= (snd <$> downloadUrls)
       then throwE $ PackageError (pkgname pkg) ChecksumMismatch
       else pure True

  where
    tryDownload :: [Req (Digest MD5)] -> IO (Either HttpException [Digest MD5])
    tryDownload = try . traverse (runReq defaultHttpConfig)
    tryReadChecksum :: Text -> IO (Either IOException (Digest MD5))
    tryReadChecksum = try . fmap md5sum . BSL.readFile . T.unpack . filename

doCompileOrder :: Command -> String -> ReaderT Environment IO Bool
doCompileOrder command compileOrder = do
    compileOrderPath' <- asks compileOrderPath
    content <- liftIO $ T.IO.readFile compileOrderPath'

    packageList <- case parseCompileOrder compileOrderPath' content of
      Right right -> return right
      Left left -> liftIO $ do
          console Fatal $ T.pack $ errorBundlePretty left
          exitFailure

    maybeError <- action packageList
    case maybeError of
      Left message -> liftIO $ do
          console Fatal $ showPackageError message
          exitFailure
      Right evaluated -> pure $ or evaluated

  where
    compileOrderPath = (</> compileOrder) . root
    action :: [Step] -> ReaderT Environment IO (Either PackageError [Bool])
    action packageList = do
        compileOrderDirectory <- asks (takeDirectory . compileOrderPath)
        let doAction = doPackage command compileOrderDirectory
         in runExceptT $ traverse doAction packageList

doPackage :: Command -> FilePath -> Step -> ActionT Bool
doPackage command repo step = do
    oldDirectory <- liftIO getCurrentDirectory
    liftIO $ setCurrentDirectory $ repo </> T.unpack pkgName

    let infoFile = joinPath [repo, T.unpack pkgName, T.unpack pkgName <.> "info"]
    content <- liftIO $ C8.readFile infoFile

    pkg <- case parse parseInfoFile infoFile content of
        Left left -> throwE $ PackageError (T.unpack pkgName) $ ParseError left
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
    return $ Environment unameM' config

-- | Build all packages specified in the configuration.
build :: IO ()
build = do
    environment <- collectRunInformation
    createDirectoryIfMissing False $ temporaryDirectory environment

    runContT (forRepository environment (buildCompileOrder environment)) return
  where
    buildCompileOrder environment break compileOrder = do
        builtAny <- liftIO
            $ runReaderT (doCompileOrder buildPackage compileOrder) environment

        let condition = compileOrder == "systemd/compile-order" && builtAny
        when condition $ reboot break

    forRepository environment buildCompileOrder' =
        callCC $ for_ (repositories environment) . buildCompileOrder'

reboot :: (() -> ContT () IO ()) -> ContT () IO ()
reboot break = do
    _ <- liftIO $ T.IO.putStrLn "The Computer must be restarted before \
        \building can continue. Would you like to reboot now? (Yes/No)"
    answer <- liftIO T.IO.getLine
    when (answer == "Yes") $ liftIO $ callProcess "/sbin/reboot" mempty
    break ()

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
