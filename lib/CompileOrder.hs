{-# LANGUAGE OverloadedStrings #-}
module CompileOrder ( Step(..)
                    , parseCompileOrder
                    ) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators ( many
                                 , optional
                                 )
import qualified Data.ByteString.Char8 as C8
import Data.Maybe ( catMaybes
                  , fromMaybe
                  )
import Data.Semigroup (Semigroup(..))
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec ( Parsec
                       , ParseError
                       , parse
                       , takeWhile1P
                       )
import Text.Megaparsec.Byte ( newline
                            , char
                            )
import Text.Megaparsec.Byte.Lexer (skipLineComment)

type GenParser = Parsec Void C8.ByteString

data Step = PackageName (Maybe C8.ByteString) C8.ByteString

instance Eq Step where
    (PackageName a1 a2) == (PackageName b1 b2) = a1 == b1 && a2 == b2

instance Semigroup Step where
    (PackageName a1 a2) <> (PackageName b1 b2)
      = PackageName (a1 <> b1) (a2 <> b2)

instance Monoid Step where
    mempty = PackageName mempty mempty
    mappend = (<>)

instance Show Step where
    show (PackageName Nothing new) = C8.unpack new
    show (PackageName (Just old) new) = (C8.unpack old) ++ ('%' : (C8.unpack new))

percent :: GenParser Word8
percent = char 0x25

package :: GenParser (Maybe Step)
package = do
    old <- takeWhile1P Nothing $ \c -> c /= 0xA && c /= 0x25
    new <- optional $ percent *> (takeWhile1P Nothing (/= 0xA))
    return $ Just $ PackageName ((const old) <$> new) (fromMaybe old new)

emptyLine :: GenParser (Maybe Step)
emptyLine = newline >> mempty

comment :: GenParser (Maybe Step)
comment = skipLineComment "#" >> mempty

line :: GenParser (Maybe Step)
line = emptyLine
   <|> comment
   <|> package

parseCompileOrder :: String -> C8.ByteString -> Either (ParseError Word8 Void) [Step]
parseCompileOrder = (parse $ catMaybes <$> many line)
