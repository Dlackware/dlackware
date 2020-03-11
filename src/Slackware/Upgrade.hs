{-# LANGUAGE OverloadedStrings #-}
module Slackware.Upgrade where

import Control.Monad (void)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as Text.IO
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import Data.List (find)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Network.HTTP.Req (defaultHttpConfig, runReq)
import Slackware.Config as Config
import Slackware.CompileOrder
import Slackware.Download
import Slackware.Info
import Slackware.Log (Level(..), console)
import qualified Slackware.Version as Version
import System.Directory ( createDirectoryIfMissing
                        , getCurrentDirectory
                        , setCurrentDirectory
                        )
import System.Exit (exitFailure)
import System.FilePath ( (</>)
                       , (<.>)
                       , takeDirectory
                       , takeFileName
                       )
import System.Process (callCommand)
import Text.Megaparsec (errorBundlePretty, parse)

readConfiguration :: IO Config.Config
readConfiguration = do
    configContent <- BSL.readFile configPath
    let config = fromRight undefined $ Config.parseConfig configPath configContent

    createDirectoryIfMissing True $ T.unpack $ Config.loggingDirectory config
    return config

getCompileOrders :: Config.Config -> [FilePath]
getCompileOrders config =
    let f x = T.unpack (Config.reposRoot config) </> T.unpack x
     in fmap f (Config.repos config)


doCompileOrder :: String -> FilePath -> IO Bool
doCompileOrder needle compileOrder = do
    content <- Text.IO.readFile compileOrder
    list <- packageList content
    return $ isJust $ find lookup' list
  where
    lookup' (PackageName _ new) = new == T.pack needle
    packageList content =
        case parseCompileOrder compileOrder content of
            Left left -> do
                console Fatal $ T.pack $ errorBundlePretty left
                exitFailure
            Right right -> return right

findM :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = do
    b <- f x
    if b then (return . Just) x else findM f xs

upgrade :: String -> String -> IO ()
upgrade pkgnam toVersion = do
    config <- readConfiguration

    let compileOrders = getCompileOrders config
    maybeMatchingCompileOrder <- findM (doCompileOrder pkgnam) compileOrders
    let matchingCompileOrder = fromMaybe lookupError maybeMatchingCompileOrder

    cwd <- getCurrentDirectory
    _ <- setCurrentDirectory $ takeDirectory matchingCompileOrder </> pkgnam

    let infoFile = pkgnam <.> "info"
    content <- C8.readFile infoFile

    pkg <- case parse parseInfoFile infoFile content of
        Left _ -> error "Unable to parse the .info file"
        Right pkg -> return pkg

    let newDownloads = updateDownloadVersion (version pkg) (T.pack toVersion)
            <$> downloads pkg
    newChecksums <- traverse (runReq defaultHttpConfig)
        $ fromJust . get <$> newDownloads

    let newPackage = pkg { downloads = newDownloads, checksums = newChecksums }
    Text.IO.writeFile infoFile $ generate newPackage

    let group = takeFileName . takeDirectory $ matchingCompileOrder
    _ <- callCommand $ "git add . && git commit -m \""
        ++ group ++ "/" ++ pkgnam
        ++ ": Updated for version " ++ toVersion ++ "\""
    setCurrentDirectory cwd

      where
        lookupError = error $ unwords [pkgnam, " wasn't found in any compile order"]

upgradeAll :: IO ()
upgradeAll = do
    versions' <- Text.IO.readFile "etc/versions"
    case parse Version.versions "etc/versions" versions' of
            Left e -> error $ errorBundlePretty e
            Right parsedVersions -> void $ Map.traverseWithKey upgrade' parsedVersions
  where
    upgrade' name version' = upgrade (T.unpack name) (T.unpack version')
