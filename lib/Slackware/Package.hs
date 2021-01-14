module Slackware.Package
    ( ActionT
    , Command
    , Environment(..)
    , loggingDirectory
    , machine
    , repositories
    , root
    , temporaryDirectory
    , versions
    ) where

import Control.Monad.Trans.Reader
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Slackware.Config as Config
import Slackware.Info

data Environment = Environment String Config.Config (Map Text Text)

type ActionT = ReaderT Environment IO

type Command
    = PackageInfo
    -> Text
    -> ActionT Bool

versions :: Environment -> Map Text Text
versions (Environment _ _ versions') = versions'

machine :: Environment -> String
machine (Environment unameM' _ _) = unameM'

loggingDirectory :: Environment -> String
loggingDirectory (Environment _ config _) =
    Text.unpack $ Config.loggingDirectory config

temporaryDirectory :: Environment -> String
temporaryDirectory (Environment _ config _) =
    Text.unpack $ Config.temporaryDirectory config

repositories :: Environment -> [String]
repositories (Environment _ config _) = Text.unpack <$> Config.repos config

root :: Environment -> String
root (Environment _ config _) = Text.unpack $ Config.reposRoot config
