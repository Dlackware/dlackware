{-# LANGUAGE OverloadedStrings #-}
module Slackware.Config ( Config(..)
                        , configPath
                        , parseConfig
                        ) where

import Data.YAML ( FromYAML
                 , decode
                 , withMap
                 , parseYAML
                 , (.:)
                 )
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL.Builder

data Config = Config
    { reposRoot :: Text
    , loggingDirectory :: Text
    , temporaryDirectory :: Text
    , repos :: [Text]
    , gnomeVersion :: Text
    } deriving (Eq, Show)

-- | Returns configuration path.
configPath :: String
configPath = "etc/dlackware.yaml"

instance FromYAML Config where
    parseYAML = withMap "Config" $ \m -> Config
        <$> m .: "reposRoot"
        <*> m .: "loggingDirectory"
        <*> m .: "temporaryDirectory"
        <*> m .: "repos"
        <*> m .: "gnomeVersion"

buildErrorMessage :: String -> String -> Text
buildErrorMessage path msg = TL.toStrict . TL.Builder.toLazyText
    $ TL.Builder.fromString path <> ": " <> TL.Builder.fromString msg

parseConfig :: String -> BS.ByteString -> Either Text Config
parseConfig path source =
    case decode source of
      Left (_, err)  -> Left $ buildErrorMessage path err
      Right []  -> Left $ buildErrorMessage path "configuration is empty"
      Right [x] -> Right x
      _         -> Left $ buildErrorMessage path "expected only one document"
