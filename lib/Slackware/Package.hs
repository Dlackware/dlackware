module Slackware.Package
    ( ActionT
    , Command
    , Environment(..)
    , loggingDirectory
    , repositories
    , root
    , temporaryDirectory
    , unameM
    ) where

import Control.Monad.Trans.Reader
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Slackware.Config as Config
import Slackware.Info

data Environment = Environment String Config.Config

type ActionT = ReaderT Environment IO

type Command
    = PackageInfo
    -> Text
    -> ActionT Bool

unameM :: Environment -> String
unameM (Environment unameM' _) = unameM'

loggingDirectory :: Environment -> String
loggingDirectory (Environment _ config) =
    Text.unpack $ Config.loggingDirectory config

temporaryDirectory :: Environment -> String
temporaryDirectory (Environment _ config) =
    Text.unpack $ Config.temporaryDirectory config

repositories :: Environment -> [String]
repositories (Environment _ config) = Text.unpack <$> Config.repos config

root :: Environment -> String
root (Environment _ config) = Text.unpack $ Config.reposRoot config
