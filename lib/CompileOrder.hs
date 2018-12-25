module CompileOrder ( Step(..)
                    , parseCompileOrder
                    ) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators ( many
                                 , optional
                                 , some
                                 )
import Data.Maybe ( catMaybes
                  , fromMaybe
                  )
import Data.Semigroup (Semigroup(..))
import Data.Void (Void)
import Text.Megaparsec ( Parsec
                       , ParseError
                       , parse
                       )
import Text.Megaparsec.Char ( noneOf
                            , char
                            , newline
                            )

type GenParser = Parsec Void String

data Step = PackageName (Maybe String) String

instance Eq Step where
    (PackageName a1 a2) == (PackageName b1 b2) = a1 == b1 && a2 == b2

instance Semigroup Step where
    (PackageName a1 a2) <> (PackageName b1 b2)
      = PackageName (a1 <> b1) (a2 <> b2)

instance Monoid Step where
    mempty = PackageName mempty mempty
    mappend = (<>)

instance Show Step where
    show (PackageName Nothing new) = new
    show (PackageName (Just old) new) = old ++ ('%' : new)

package :: GenParser (Maybe Step)
package = do
    old <- some $ noneOf "%\n"
    new <- optional $ char '%' *> (some $ noneOf "\n")
    return $ Just $ PackageName ((const old) <$> new) (fromMaybe old new)

emptyLine :: GenParser (Maybe Step)
emptyLine = newline >> mempty

comment :: GenParser (Maybe Step)
comment = char '#' *> (many $ noneOf "\n") >> mempty

line :: GenParser (Maybe Step)
line = emptyLine
   <|> comment
   <|> package

parseCompileOrder :: String -> String -> Either (ParseError Char Void) [Step]
parseCompileOrder = parse $ catMaybes <$> many line
