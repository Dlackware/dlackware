{-# LANGUAGE OverloadedStrings #-}
module Slackware.Upgrade where

import Control.Exception (throw)
import Control.Monad (unless, void)
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Map.Strict as Map
import Data.List (find)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Slackware.Command
import Slackware.CompileOrder
import Slackware.Config as Config
import Slackware.Download
import Slackware.Error
import Slackware.Info
import Slackware.Log (Level(..), console)
import qualified Slackware.Version as Version
import System.Directory (getCurrentDirectory , setCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>), takeDirectory, takeFileName)
import System.Process (callCommand)
import Text.Megaparsec (errorBundlePretty, parse)

getCompileOrders :: Config.Config -> [FilePath]
getCompileOrders config =
    let f x = Text.unpack (Config.reposRoot config) </> Text.unpack x
     in fmap f (Config.repos config)


doCompileOrder :: String -> FilePath -> IO Bool
doCompileOrder needle compileOrder = do
    content <- Text.IO.readFile compileOrder
    list <- packageList content
    return $ isJust $ find lookup' list
  where
    lookup' (PackageName _ new) = new == Text.pack needle
    packageList content =
        case parseCompileOrder compileOrder content of
            Left left -> do
                console Fatal $ Text.pack $ errorBundlePretty left
                exitFailure
            Right right -> return right

findM :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = do
    b <- f x
    if b then (return . Just) x else findM f xs

upgrade :: String -> Text -> Maybe String -> IO ()
upgrade pkgnam toVersion gnomeVersion = do
    config <- readConfiguration

    let compileOrders = getCompileOrders config
    maybeMatchingCompileOrder <- findM (doCompileOrder pkgnam) compileOrders
    let matchingCompileOrder = fromMaybe lookupError maybeMatchingCompileOrder

    cwd <- getCurrentDirectory
    _ <- setCurrentDirectory $ takeDirectory matchingCompileOrder </> pkgnam

    let infoFile = pkgnam <.> "info"
    content <- C8.readFile infoFile

    let pkg = either (throw . PackageError pkgnam . ParseError) id
            $ parse parseInfoFile infoFile content

    let newDownloads = updateDownloadVersion pkg toVersion gnomeVersion
    newChecksums <- fromJust $ downloadAll newDownloads

    let newPackage = update pkg toVersion newDownloads newChecksums
    let gitCommand = "git add . && git commit -m \""
            ++ (takeFileName . takeDirectory $ matchingCompileOrder)
            ++ "/" ++ pkgnam
            ++ ": Updated for version " ++ Text.unpack toVersion ++ "\""
    unless (pkg == newPackage)
        $ Text.IO.writeFile infoFile (generate newPackage)
        >> callCommand gitCommand
    setCurrentDirectory cwd

  where
    lookupError = throw $ PackageError pkgnam Missing

upgradeAll :: Maybe String -> IO ()
upgradeAll gnomeVersion = do
    versions' <- Text.IO.readFile "etc/versions"
    case parse Version.versions "etc/versions" versions' of
        Left e -> error $ errorBundlePretty e
        Right parsedVersions -> void
            $ Map.traverseWithKey upgradeOne parsedVersions
  where
    upgradeOne pkgnam toVersion =
        upgrade (Text.unpack pkgnam) toVersion gnomeVersion
