module CompileOrder ( Step(..)
                    , parseCompileOrder
                    ) where

import Data.Maybe ( catMaybes
                  , fromMaybe
                  )
import Data.Semigroup (Semigroup(..))
import Text.ParserCombinators.Parsec ( GenParser
                                     , char
                                     , many
                                     , many1
                                     , noneOf
                                     , parse
                                     , (<|>)
                                     , newline
                                     , optionMaybe
                                     )
import Text.Parsec.Error (ParseError)

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

package :: GenParser Char st (Maybe Step)
package = do
    old <- many1 $ noneOf "%\n"
    new <- optionMaybe $ char '%' *> (many1 $ noneOf "\n")
    return $ Just $ PackageName ((const old) <$> new) (fromMaybe old new)

emptyLine :: GenParser Char st (Maybe Step)
emptyLine = newline >> mempty

comment :: GenParser Char st (Maybe Step)
comment = char '#' *> (many $ noneOf "\n") >> mempty

line :: GenParser Char st (Maybe Step)
line = emptyLine
   <|> comment
   <|> package

parseCompileOrder :: String -> String -> Either ParseError [Step]
parseCompileOrder = parse $ catMaybes <$> many line
