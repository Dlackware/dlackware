module Slackware.Package ( Package(..)
                         , parseInfoFile
                         ) where

import Control.Monad.Combinators ( many
                                 , some
                                 , skipMany
                                 )
import Data.Void (Void)
import Text.Megaparsec ( Parsec
                       , eof
                       )
import Text.Megaparsec.Char ( noneOf
                            , space
                            , string
                            )

type GenParser = Parsec Void String

data Package = Package { version :: String
                       , homepage :: String
                       , downloads :: [String]
                       , checksums :: [String]
                       }

packageName :: GenParser String
packageName = do
    _ <- string "PKGNAM=\""
    result <- many (noneOf "\"")
    _ <- string "\"\n"
    return result

packageVersion :: GenParser String
packageVersion = do
    _ <- string "VERSION=\""
    result <- many (noneOf "\"")
    _ <- string "\"\n"
    return result

packageHomepage :: GenParser String
packageHomepage = do
    _ <- string "HOMEPAGE=\""
    result <- many (noneOf "\"")
    _ <- string "\"\n"
    return result

packageDownload :: GenParser String
packageDownload = do
    result <- some (noneOf "\\\" \n")
    skipMany $ string " \\"
    skipMany space
    return result

packageDownloads :: GenParser [String]
packageDownloads = do
    _ <- string "DOWNLOAD=\""
    result <- some packageDownload
    _ <- string "\"\n"
    return result

packageChecksum :: GenParser String
packageChecksum = do
    result <- some (noneOf "\\\" \n")
    skipMany $ string " \\"
    skipMany space
    return result

packageChecksums :: GenParser [String]
packageChecksums = do
    _ <- string "MD5SUM=\""
    result <- some packageChecksum
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
    return $ Package version' homepage' download md5sum
