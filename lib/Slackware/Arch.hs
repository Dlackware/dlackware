{-# LANGUAGE OverloadedStrings #-}
module Slackware.Arch ( parseArch
                      , grepSlackBuild
                      , uname
                      ) where

import Control.Monad.Combinators (some)
import Data.Either (fromRight)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec ( Parsec
                       , anySingle
                       , parse
                       , many
                       , choice
                       , eof
                       )
import Text.Megaparsec.Char ( char
                            , digitChar
                            , string
                            )
import Text.Megaparsec.Error (ParseErrorBundle)

type GenParser = Parsec Void String

x86 :: GenParser String
x86 = char 'i' >> digitChar >> string "86" >> return "i586"

arm :: GenParser String
arm = string "arm" >> many anySingle >> return "arm"

parseArch :: String -> Either (ParseErrorBundle String Void) String
parseArch = parse parser mempty
    where parser = do
            arch <- choice [x86, arm]
            eof
            return arch

uname :: String -> String
uname unameM =
    let unameM' = init unameM -- Remove newline
     in fromRight unameM' $ parseArch unameM'

buildNumber :: GenParser String
buildNumber = do
    _ <- string "BUILD=${BUILD:-"
    n <- some digitChar
    _ <- char '}'
    return n

grepSlackBuild :: String -> T.Text -> (String, String)
grepSlackBuild unameM slackBuild
  = foldr f (mempty, unameM) $ T.lines slackBuild
      where
          f line (build, arch)
              | "BUILD=" `T.isPrefixOf` line = (parseBuildNumber (T.unpack line), arch)
              | "ARCH=" `T.isPrefixOf` line = (build, "noarch")
              | otherwise = (build, arch)
          parseBuildNumber = fromRight mempty . parse buildNumber mempty
