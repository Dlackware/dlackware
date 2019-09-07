-- | Data and parser for Gnome "versions" files.
module Slackware.Version
    ( Version(..)
    , versions
    ) where

import Control.Monad.Combinators (sepBy)
import Data.Text (Text)
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
    } deriving (Eq, Show)

identifier :: String -> GenParser Text
identifier tokenName = takeWhile1P (Just tokenName) (/= ':')

-- | Gnome "versions" file parser.
versions :: GenParser [Version]
versions = versionP `sepBy` eol

versionP :: GenParser Version
versionP = do
    _ <- identifier "category"
    _ <- char ':'
    name' <- identifier "package name"
    _ <- char ':'
    version' <- identifier "package version"
    _ <- char ':'
    return $ Version { name = name', version = version' }
