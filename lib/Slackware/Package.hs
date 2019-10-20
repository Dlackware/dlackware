module Slackware.Package ( PackageAction
                         , PackageEnvironment(..)
                         , loggingDirectory
                         , repositories
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

data PackageEnvironment = PackageEnvironment String Config.Config

type PackageAction
    = PackageInfo
    -> Text
    -> ExceptT PackageError (ReaderT PackageEnvironment IO) Bool

unameM :: PackageEnvironment -> String
unameM (PackageEnvironment unameM' _) = unameM'

loggingDirectory :: PackageEnvironment -> String
loggingDirectory (PackageEnvironment _ config) =
    Text.unpack $ Config.loggingDirectory config

temporaryDirectory :: PackageEnvironment -> String
temporaryDirectory (PackageEnvironment _ config) =
    Text.unpack $ Config.temporaryDirectory config

repositories :: PackageEnvironment -> [String]
repositories (PackageEnvironment _ config) = Text.unpack <$> Config.repos config
