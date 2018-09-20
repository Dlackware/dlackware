module Config ( Config(..)
              , parseConfig
              ) where

import           Data.YAML ( FromYAML
                           , decode
                           , withMap
                           , parseYAML
                           , (.:)
                           )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T

data Config = Config
    { reposRoot  :: T.Text
    , repos      :: [T.Text]
    } deriving Show

instance Eq Config where
    c1 == c2 = reposRoot c1 == reposRoot c2
            && repos c1 == repos c2

instance FromYAML Config where
    parseYAML = withMap "Config" $ \m -> Config
        <$> m .: (T.pack "reposRoot")
        <*> m .: (T.pack "repos")

parseConfig :: BS.ByteString -> Either String Config
parseConfig source =
    case decode source of
      Left err     -> Left err
      Right []     -> Left "Configuration is empty"
      Right (x:[]) -> Right x
      _            -> Left "Expected only one document"
