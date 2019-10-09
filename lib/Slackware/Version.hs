{-# LANGUAGE OverloadedStrings #-}

-- | Data and parser for Gnome "versions" files.
module Slackware.Version
    ( versions
    ) where

import Control.Monad.Combinators (someTill)
import qualified Data.Map.Lazy as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec ( Parsec
                       , eof
                       , optional
                       , takeWhile1P
                       )
import Text.Megaparsec.Char ( char
                            , eol
                            , newline
                            )
import Text.Megaparsec.Char.Lexer (skipLineComment)

type GenParser = Parsec Void Text

identifier :: String -> GenParser Text
identifier tokenName = takeWhile1P (Just tokenName) predicate
  where
    predicate delimiter = delimiter /= ':' && delimiter /= '\n'

-- | Gnome "versions" file parser.
versions :: GenParser (Map Text Text)
versions = do
    _ <- optional $ skipLineComment "## " >> eol
    versions' <- someTill (version <* eol) newline <* eof
    return $ Map.fromList versions'

version :: GenParser (Text, Text)
version = do
    _ <- parseBlock "category"
    name <- parseBlock "package name"
    version' <- parseBlock "package version"
    return (name, version')
      where
        parseBlock description = identifier description <* char ':'
