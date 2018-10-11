module Arch ( parseArch
            , grepSlackBuild
            , uname
            ) where

import Data.Either (fromRight)
import Data.List (isPrefixOf)
import System.Process (readProcess)
import Text.ParserCombinators.Parsec ( GenParser
                                     , ParseError
                                     , char
                                     , digit
                                     , string
                                     , parse
                                     , many
                                     , many1
                                     , anyChar
                                     , choice
                                     , eof
                                     )

x86 :: GenParser Char st String
x86 = char 'i' >> digit >> string "86" >> return "i586"

arm :: GenParser Char st String
arm = string "arm" >> many anyChar >> return "arm"

parseArch :: String -> Either ParseError String
parseArch = parse parser mempty
    where parser = do
            arch <- choice [x86, arm]
            eof
            return arch

uname :: IO String
uname = do
    unameM <- readProcess "/usr/bin/uname" ["-m"] ""
    let unameM' = init unameM -- Remove newline
     in return $ fromRight unameM' $ parseArch unameM'

buildNumber :: GenParser Char st String
buildNumber = do
    _ <- string "BUILD=${BUILD:-"
    n <- many1 digit
    _ <- char '}'
    return n

grepSlackBuild :: String -> (String, String)
grepSlackBuild slackBuild
  = foldr f mempty $ lines slackBuild
      where
          f line (build, arch)
              | "BUILD=" `isPrefixOf` line = (parseBuildNumber line, arch)
              | "ARCH=" `isPrefixOf` line = (build, "noarch")
              | otherwise = (build, arch)
          parseBuildNumber = fromRight mempty . parse buildNumber mempty
