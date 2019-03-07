module Slackware.Config ( Config(..)
                        , parseConfig
                        ) where

import Data.YAML ( FromYAML
                 , decode
                 , withMap
                 , parseYAML
                 , (.:)
                 )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T

data Config = Config
    { reposRoot  :: T.Text
    , loggingDirectory :: T.Text
    , temporaryDirectory :: T.Text
    , repos      :: [T.Text]
    } deriving (Eq, Show)

instance FromYAML Config where
    parseYAML = withMap "Config" $ \m -> Config
        <$> m .: T.pack "reposRoot"
        <*> m .: T.pack "loggingDirectory"
        <*> m .: T.pack "temporaryDirectory"
        <*> m .: T.pack "repos"

parseConfig :: BS.ByteString -> Either String Config
parseConfig source =
    case decode source of
      Left err  -> Left err
      Right []  -> Left "Configuration is empty"
      Right [x] -> Right x
      _         -> Left "Expected only one document"
