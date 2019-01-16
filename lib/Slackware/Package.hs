{-# LANGUAGE OverloadedStrings #-}

module Slackware.Package ( Package(..)
                         , parseInfoFile
                         ) where

import Control.Monad.Combinators ( some
                                 , skipMany
                                 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (catMaybes)
import Crypto.Hash ( Digest
                   , MD5
                   , digestFromByteString
                   )
import Data.Void (Void)
import Data.Word (Word8)
import Numeric (readHex)
import Text.Megaparsec ( Parsec
                       , count
                       , eof
                       , takeWhile1P
                       )
import Text.Megaparsec.Byte ( space
                            , string
                            , hexDigitChar
                            )

type GenParser = Parsec Void C8.ByteString

data Package = Package { version :: String
                       , homepage :: C8.ByteString
                       , downloads :: [C8.ByteString]
                       , checksums :: [Digest MD5]
                       }

variableEntry :: C8.ByteString -> GenParser C8.ByteString
variableEntry variable = do
    _ <- string (C8.append variable "=\"")
    result <- takeWhile1P Nothing (0x22 /=)
    _ <- string "\"\n"
    return result

packageDownload :: GenParser C8.ByteString
packageDownload = do
    result <- takeWhile1P Nothing $ and . ((/=) <$> [0xA, 0x20, 0x22, 0x5C] <*>) . pure
    skipMany $ string " \\"
    space
    return result

packageDownloads :: GenParser [C8.ByteString]
packageDownloads = do
    _ <- string "DOWNLOAD=\""
    result <- some packageDownload
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
    result <- some packageChecksum
    _ <- string "\"\n"
    return result

parseInfoFile :: GenParser Package
parseInfoFile = do
    _ <- variableEntry "PKGNAM"
    version' <- variableEntry "VERSION"
    homepage' <- variableEntry "HOMEPAGE"
    download <- packageDownloads
    md5sum <- packageChecksums
    eof

    let md5sums = catMaybes $ digestFromByteString <$> md5sum
    return $ Package (C8.unpack version') homepage' download md5sums
