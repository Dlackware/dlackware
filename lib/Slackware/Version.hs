{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data and parser for Gnome "versions" files.
module Slackware.Version
    ( Version(..)
    , versions
    ) where

import Control.Monad.Combinators (sepBy)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec ( Parsec
                       , takeWhile1P
                       )
import Text.Megaparsec.Char ( char
                            , eol
                            )

type GenParser = Parsec Void Text

-- | Stores the package name and version.
data Version = Version
    { name :: Text -- ^ Package name.
    , version :: Text -- ^ Package version.
    } deriving Eq

instance Show Version where
    show Version {..} = Text.unpack $ Text.intercalate ":" [name, version]

identifier :: String -> GenParser Text
identifier tokenName = takeWhile1P (Just tokenName) (/= ':')

-- | Gnome "versions" file parser.
versions :: GenParser [Version]
versions = versionP `sepBy` eol

versionP :: GenParser Version
versionP = do
    _ <- parseBlock "category"
    name' <- parseBlock "package name"
    version' <- parseBlock "package version"
    return $ Version {name = name', version = version'}
      where
        parseBlock description = identifier description <* char ':'
