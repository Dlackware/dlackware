{-# LANGUAGE OverloadedStrings #-}
module Slackware.CompileOrder ( Step(..)
                              , parseCompileOrder
                              ) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators (many, optional)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup (Semigroup(..))
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Void (Void)
import Prelude hiding (lines)
import Text.Megaparsec (Parsec, failure, parse, takeWhile1P)
import Text.Megaparsec.Char (newline, char)
import Text.Megaparsec.Char.Lexer (skipLineComment)
import Text.Megaparsec.Error (ErrorItem(..), ParseErrorBundle)

type GenParser = Parsec Void Text

data Step = PackageName (Maybe Text) Text
    deriving Eq

instance Semigroup Step where
    (PackageName a1 a2) <> (PackageName b1 b2)
      = PackageName (a1 <> b1) (a2 <> b2)

instance Monoid Step where
    mempty = PackageName mempty mempty
    mappend = (<>)

instance Show Step where
    show (PackageName Nothing new) = Text.unpack new
    show (PackageName (Just old) new) =
        Text.unpack old ++ ('%' : Text.unpack new)

percent :: GenParser Char
percent = char '%'

package :: GenParser (Maybe Step)
package = do
    old <- takeWhile1P Nothing $ \c -> c /= '\n' && c /= '%'
    new <- optional $ percent *> takeWhile1P Nothing (/= '\n')
    return $ Just $ PackageName (old <$ new) $ fromMaybe old new

emptyLine :: GenParser (Maybe Step)
emptyLine = newline >> mempty

comment :: GenParser (Maybe Step)
comment = skipLineComment "#" >> mempty

line :: GenParser (Maybe Step)
line = emptyLine
   <|> comment
   <|> package

parseCompileOrder ::
    String ->
    Text ->
    Either (ParseErrorBundle Text Void) (NonEmpty Step)
parseCompileOrder = parse $ do
    lines <- NonEmpty.nonEmpty . catMaybes <$> many line
    maybe (failure (Just EndOfInput) mempty) pure lines
