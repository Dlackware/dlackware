module Slackware.Package ( Package(..)
                         , parseInfoFile
                         ) where

import Text.ParserCombinators.Parsec ( GenParser
                                     , eof
                                     , string
                                     , many1
                                     , space
                                     , skipMany
                                     , many
                                     , noneOf
                                     )

data Package = Package { version :: String
                       , homepage :: String
                       , downloads :: [String]
                       , checksums :: [String]
                       }

packageName :: GenParser Char st String
packageName = do
    _ <- string "PKGNAM=\""
    result <- many (noneOf "\"")
    _ <- string "\"\n"
    return result

packageVersion :: GenParser Char st String
packageVersion = do
    _ <- string "VERSION=\""
    result <- many (noneOf "\"")
    _ <- string "\"\n"
    return result

packageHomepage :: GenParser Char st String
packageHomepage = do
    _ <- string "HOMEPAGE=\""
    result <- many (noneOf "\"")
    _ <- string "\"\n"
    return result

packageDownload :: GenParser Char st String
packageDownload = do
    result <- many1 (noneOf "\\\" \n")
    skipMany $ string " \\"
    skipMany space
    return result

packageDownloads :: GenParser Char st [String]
packageDownloads = do
    _ <- string "DOWNLOAD=\""
    result <- many1 packageDownload
    _ <- string "\"\n"
    return result

packageChecksum :: GenParser Char st String
packageChecksum = do
    result <- many1 (noneOf "\\\" \n")
    skipMany $ string " \\"
    skipMany space
    return result

packageChecksums :: GenParser Char st [String]
packageChecksums = do
    _ <- string "MD5SUM=\""
    result <- many1 packageChecksum
    _ <- string "\"\n"
    return result

parseInfoFile :: GenParser Char st Package
parseInfoFile = do
    _ <- packageName
    version' <- packageVersion
    homepage' <- packageHomepage
    download <- packageDownloads
    md5sum <- packageChecksums
    eof
    return $ Package version' homepage' download md5sum
