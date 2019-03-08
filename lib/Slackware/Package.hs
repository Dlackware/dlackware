module Slackware.Package ( PackageAction
                         , PackageEnvironment(..)
                         , loggingDirectory
                         , unameM
                         ) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Text as T
import qualified Slackware.Config
import Slackware.Info
import Slackware.Error

data PackageEnvironment = PackageEnvironment String Slackware.Config.Config

type PackageAction = PackageInfo
                  -> (String, String)
                  -> ExceptT PackageError (ReaderT PackageEnvironment IO) ()

unameM :: PackageEnvironment -> String
unameM (PackageEnvironment unameM' _) = unameM'

loggingDirectory :: PackageEnvironment -> T.Text
loggingDirectory (PackageEnvironment _ config)
    = Slackware.Config.loggingDirectory config
