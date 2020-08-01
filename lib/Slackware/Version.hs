{-# LANGUAGE OverloadedStrings #-}

-- | Data and parser for Gnome "versions" files.
module Slackware.Version
    ( versions
    ) where

import Control.Applicative (Alternative(..))
import qualified Data.Map.Lazy as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, optional, takeWhile1P)
import Text.Megaparsec.Char (char, eol, space1)
import Text.Megaparsec.Char.Lexer (lexeme, skipLineComment)

type GenParser = Parsec Void Text

identifier :: String -> GenParser Text
identifier tokenName = takeWhile1P (Just tokenName) predicate
  where
    predicate delimiter = delimiter /= ':' && delimiter /= '\n'

-- | Gnome "versions" file parser.
versions :: GenParser (Map Text Text)
versions = Map.fromList <$> versions'
  where
    versions' = optional (skipLineComment "## " >> eol)
        *> many (lexeme space1 version)
        <* eof

version :: GenParser (Text, Text)
version = parseBlock "category" *> nameVersion
  where
    parseBlock description = identifier description <* char ':'
    nameVersion = (,)
        <$> parseBlock "package name"
        <*> parseBlock "package version"
