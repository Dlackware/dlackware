module Package (parseInfoFile) where

import Text.ParserCombinators.Parsec ( GenParser
                                     , eof
                                     , string
                                     , many1
                                     , space
                                     , skipMany
                                     , many
                                     , noneOf
                                     )

packageName :: GenParser Char st String
packageName = do
    string "PKGNAM=\""
    result <- many (noneOf "\"")
    string "\"\n"
    return result

packageVersion :: GenParser Char st String
packageVersion = do
    string "VERSION=\""
    result <- many (noneOf "\"")
    string "\"\n"
    return result

packageHomepage :: GenParser Char st String
packageHomepage = do
    string "HOMEPAGE=\""
    result <- many (noneOf "\"")
    string "\"\n"
    return result

packageDownload :: GenParser Char st String
packageDownload = do
    result <- many1 (noneOf "\\\" \n")
    skipMany $ string " \\"
    skipMany space
    return result

packageDownloads :: GenParser Char st [String]
packageDownloads = do
    string "DOWNLOAD=\""
    result <- many1 packageDownload
    string "\"\n"
    return result

packageChecksum :: GenParser Char st String
packageChecksum = do
    result <- many1 (noneOf "\\\" \n")
    skipMany $ string " \\"
    skipMany space
    return result

packageChecksums :: GenParser Char st [String]
packageChecksums = do
    string "MD5SUM=\""
    result <- many1 packageChecksum
    string "\"\n"
    return result

parseInfoFile :: GenParser Char st (String, String, String, [String], [String])
parseInfoFile = do
    name <- packageName
    version <- packageVersion
    homepage <- packageHomepage
    download <- packageDownloads
    md5sum <- packageChecksums
    eof
    return (name, version, homepage, download, md5sum)
