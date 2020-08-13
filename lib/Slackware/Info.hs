{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Slackware.Info
    ( PackageInfo(..)
    , generate
    , parseInfoFile
    , update
    , updateDownloadVersion
    ) where

import Control.Monad.Combinators (sepBy)
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Lazy.Text
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder as Text (Builder)
import Crypto.Hash (Digest, MD5, digestFromByteString)
import Data.Void (Void)
import Data.Word (Word8)
import Numeric (readHex, showHex)
import Text.Megaparsec (Parsec, count, eof, takeWhile1P)
import Text.Megaparsec.Byte (space, string, hexDigitChar)
import Text.URI
    ( Authority(..)
    , URI(..)
    , mkPathPiece
    , parserBs
    , render
    , unRText
    )

type GenParser = Parsec Void ByteString

data PackageInfo = PackageInfo
    { pkgname :: String
    , version :: Text
    , homepage :: Text
    , downloads :: [URI]
    , checksums :: [Digest MD5]
    } deriving (Eq, Show)

variableEntry :: ByteString -> GenParser ByteString
variableEntry variable = string (Char8.append variable "=\"")
    *> takeWhile1P Nothing (0x22 /=)
    <* string "\"\n"

variableSeparator :: GenParser ()
variableSeparator = string " \\" *> space

packageDownloads :: GenParser [URI]
packageDownloads = string "DOWNLOAD=\""
    *> sepBy parserBs variableSeparator
    <* string "\"\n"

hexDigit :: GenParser Word8
hexDigit =
    let digitPair = count 2 hexDigitChar
     in fst . head . readHex . fmap (toEnum . fromIntegral) <$> digitPair

packageChecksum :: GenParser ByteString
packageChecksum = ByteString.pack <$> count 16 hexDigit

packageChecksums :: GenParser [ByteString]
packageChecksums = string "MD5SUM=\""
    *> sepBy packageChecksum variableSeparator
    <* string "\"\n"

parseInfoFile :: GenParser PackageInfo
parseInfoFile = PackageInfo
    <$> (Char8.unpack <$> variableEntry "PKGNAM")
    <*> (Text.decodeUtf8 <$> variableEntry "VERSION")
    <*> (Text.decodeUtf8 <$> variableEntry "HOMEPAGE")
    <*> packageDownloads
    <*> (mapMaybe digestFromByteString <$> packageChecksums)
    <* eof 

updateDownloadVersion :: PackageInfo -> Text -> Maybe String -> [URI]
updateDownloadVersion package toVersion gnomeVersion
    = updateDownload (version package) toVersion gnomeVersion
    <$> downloads package

updateDownload :: Text -> Text -> Maybe String -> URI -> URI
updateDownload fromVersion toVersion gnomeVersion
    = updateCoreVersion fromVersion toVersion gnomeVersion
    . updatePackageVersion fromVersion toVersion gnomeVersion

updatePackageVersion :: Text -> Text -> Maybe String -> URI -> URI
updatePackageVersion fromVersion toVersion _gnomeVersion download = download
    { uriPath = uriPath download >>= traverse (traverse updatePathPiece)
    }
  where
    updatePathPiece = mkPathPiece
        . Text.replace fromMajor toMajor
        . Text.replace fromVersion toVersion
        . unRText
    fromMajor = major fromVersion
    toMajor = major toVersion

major :: Text -> Text
major = Text.intercalate "." . take 2 . Text.splitOn "."

updateCoreVersion :: Text -> Text -> Maybe String -> URI -> URI
updateCoreVersion _fromVersion _toVersion (Just gnomeVersion) download
    | Just (False, pathPieces) <- uriPath download
    , (beforeCore, afterCore) <- NonEmpty.break (comparePathPiece "core") pathPieces
    , _ : _ : _ : sources : afterSources <- afterCore
    , comparePathPiece "sources" sources && not (null afterSources)
    , Right (Authority{..}) <- uriAuthority download
    , ".gnome.org" `Text.isSuffixOf` unRText authHost
    , Nothing <- authPort =
        download { uriPath = buildPath beforeCore afterSources }
  where
    comparePathPiece this that = Just that == mkPathPiece this
    buildPath beforeCore afterSources = do
        core <- mkPathPiece "core"
        let textGnomeVersion = Text.pack gnomeVersion
        minorGnomeVersion <- mkPathPiece $ major textGnomeVersion
        patchGnomeVersion <- mkPathPiece textGnomeVersion
        sources <- mkPathPiece "sources"
        let afterCore = core : minorGnomeVersion : patchGnomeVersion : sources : afterSources
        (False,) <$> NonEmpty.nonEmpty (beforeCore ++ afterCore)
updateCoreVersion _ _ _ download = download            

update :: PackageInfo -> Text -> [URI] -> [Digest MD5] -> PackageInfo
update old toVersion downloads' checksums' = old
    { version = toVersion
    , downloads = downloads'
    , checksums = checksums'
    }

generate :: PackageInfo -> Text
generate pkg = Lazy.Text.toStrict $ Text.Builder.toLazyText builder
  where
    digestToText = Text.pack . foldr hexAppender "" . ByteArray.unpack
    hexAppender x acc
      | x > 15 = showHex x acc
      | otherwise = '0' : showHex x acc
    builder = "PKGNAM=\"" <> Text.Builder.fromString (pkgname pkg) <> "\"\n"
        <> "VERSION=\"" <> Text.Builder.fromText (version pkg) <> "\"\n"
        <> "HOMEPAGE=\"" <> Text.Builder.fromText (homepage pkg) <> "\"\n"
        <> generateMultiEntry "DOWNLOAD" (render <$> downloads pkg)
        <> generateMultiEntry "MD5SUM" (digestToText <$> checksums pkg)

generateMultiEntry :: Text -> [Text] -> Text.Builder
generateMultiEntry name entries = Text.Builder.fromText name
    <> "=\""
    <> Text.Builder.fromText (Text.intercalate separator entries)
    <> "\"\n"
  where
    padLength = Text.length name + 2
    separator = " \\\n" <> Text.replicate padLength " "
