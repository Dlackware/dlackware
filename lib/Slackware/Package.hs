{-# LANGUAGE OverloadedStrings #-}

module Slackware.Package ( Package(..)
                         , parseInfoFile
                         ) where

import Control.Monad.Combinators ( many
                                 , some
                                 , skipMany
                                 )
import qualified Data.ByteString.Char8 as C8
import Data.Void (Void)
import Text.Megaparsec ( Parsec
                       , eof
                       , takeWhile1P
                       )
import Text.Megaparsec.Byte ( space
                            , string
                            )

type GenParser = Parsec Void C8.ByteString

data Package = Package { version :: String
                       , homepage :: C8.ByteString
                       , downloads :: [C8.ByteString]
                       , checksums :: [C8.ByteString]
                       }

packageName :: GenParser C8.ByteString
packageName = do
    _ <- string "PKGNAM=\""
    result <- takeWhile1P Nothing (0x22 /=)
    _ <- string "\"\n"
    return result

packageVersion :: GenParser C8.ByteString
packageVersion = do
    _ <- string "VERSION=\""
    result <- takeWhile1P Nothing (0x22 /=)
    _ <- string "\"\n"
    return result

packageHomepage :: GenParser C8.ByteString
packageHomepage = do
    _ <- string "HOMEPAGE=\""
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

packageChecksums :: GenParser [C8.ByteString]
packageChecksums = do
    _ <- string "MD5SUM=\""
    result <- some packageDownload
    _ <- string "\"\n"
    return result

parseInfoFile :: GenParser Package
parseInfoFile = do
    _ <- packageName
    version' <- packageVersion
    homepage' <- packageHomepage
    download <- packageDownloads
    md5sum <- packageChecksums
    eof
    return $ Package (C8.unpack version') homepage' download md5sum
