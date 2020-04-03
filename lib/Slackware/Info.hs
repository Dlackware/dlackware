{-# LANGUAGE OverloadedStrings #-}
module Slackware.Info
    ( PackageInfo(..)
    , generate
    , parseInfoFile
    , update
    , updateDownloadVersion
    ) where

import Control.Monad.Combinators (sepBy)
import Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
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
import Text.URI (URI(..), mkPathPiece, parserBs, render, unRText)

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

updateDownloadVersion :: PackageInfo -> Text -> [URI]
updateDownloadVersion package toVersion = replacePath <$> downloads package
  where
    replacePath download = download
        { uriPath = uriPath download >>= traverse (traverse updatePathPiece)
        }
    updatePathPiece = mkPathPiece
        . Text.replace fromMajor toMajor
        . Text.replace (version package) toVersion
        . unRText
    major = Text.init . fst . Text.breakOnEnd "."
    fromMajor = major $ version package
    toMajor = major toVersion

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
