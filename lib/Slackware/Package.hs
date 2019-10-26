module Slackware.Package ( PackageAction
                         , Environment(..)
                         , loggingDirectory
                         , repositories
                         , root
                         , temporaryDirectory
                         , unameM
                         ) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Slackware.Config as Config
import Slackware.Info
import Slackware.Error

data Environment = Environment String Config.Config

type PackageAction
    = PackageInfo
    -> Text
    -> ExceptT PackageError (ReaderT Environment IO) Bool

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
