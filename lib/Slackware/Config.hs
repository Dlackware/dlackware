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
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL.Builder

data Config = Config
    { reposRoot  :: T.Text
    , loggingDirectory :: T.Text
    , temporaryDirectory :: T.Text
    , repos      :: [T.Text]
    } deriving (Eq, Show)

-- | Returns configuration path.
configPath :: String
configPath = "etc/dlackware.yaml"

instance FromYAML Config where
    parseYAML = withMap "Config" $ \m -> Config
        <$> m .: T.pack "reposRoot"
        <*> m .: T.pack "loggingDirectory"
        <*> m .: T.pack "temporaryDirectory"
        <*> m .: T.pack "repos"

buildErrorMessage :: String -> String -> T.Text
buildErrorMessage path msg = TL.toStrict . TL.Builder.toLazyText
    $ TL.Builder.fromString path <> ": " <> TL.Builder.fromString msg

parseConfig :: String -> BS.ByteString -> Either T.Text Config
parseConfig path source =
    case decode source of
      Left (_, err)  -> Left $ buildErrorMessage path err
      Right []  -> Left $ buildErrorMessage path "configuration is empty"
      Right [x] -> Right x
      _         -> Left $ buildErrorMessage path "expected only one document"
