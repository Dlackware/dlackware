{-# LANGUAGE OverloadedStrings #-}
module Slackware.Info
    ( PackageInfo(..)
    , parseInfoFile
    ) where

import Control.Monad.Combinators (many, sepBy, skipMany)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Crypto.Hash (Digest, MD5, digestFromByteString)
import Data.Void (Void)
import Data.Word (Word8)
import Numeric (readHex)
import Text.Megaparsec (Parsec, count, eof, takeWhile1P)
import Text.Megaparsec.Byte (space, string, hexDigitChar)
import Text.URI (URI(..), parserBs)

type GenParser = Parsec Void C8.ByteString

data PackageInfo = PackageInfo
    { pkgname :: String
    , version :: T.Text
    , homepage :: T.Text
    , downloads :: [URI]
    , checksums :: [Digest MD5]
    } deriving (Eq, Show)

variableEntry :: C8.ByteString -> GenParser C8.ByteString
variableEntry variable = do
    _ <- string (C8.append variable "=\"")
    result <- takeWhile1P Nothing (0x22 /=)
    _ <- string "\"\n"
    return result

variableSeparator :: GenParser ()
variableSeparator = string " \\" *> space

packageDownloads :: GenParser [URI]
packageDownloads = do
    _ <- string "DOWNLOAD=\""
    result <- sepBy parserBs variableSeparator
    _ <- string "\"\n"
    return result

hexDigit :: GenParser Word8
hexDigit = do
    digit1 <- hexDigitChar
    digit2 <- hexDigitChar
    return $ fst $ head $ readHex [toChar digit1, toChar digit2]
        where toChar = toEnum . fromIntegral

packageChecksum :: GenParser B.ByteString
packageChecksum = do
    result <- count 16 hexDigit
    skipMany $ string " \\"
    space
    return $ B.pack result

packageChecksums :: GenParser [C8.ByteString]
packageChecksums = do
    _ <- string "MD5SUM=\""
    result <- many packageChecksum
    _ <- string "\"\n"
    return result

parseInfoFile :: GenParser PackageInfo
parseInfoFile = do
    pkgname' <- variableEntry "PKGNAM"
    version' <- variableEntry "VERSION"
    homepage' <- variableEntry "HOMEPAGE"
    download <- packageDownloads
    md5sum <- packageChecksums
    eof

    let md5sums = catMaybes $ digestFromByteString <$> md5sum
    return $ PackageInfo
        (C8.unpack pkgname')
        (E.decodeUtf8 version')
        (E.decodeUtf8 homepage')
        download
        md5sums
