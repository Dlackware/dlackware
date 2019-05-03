module Slackware.Package ( PackageAction
                         , PackageEnvironment(..)
                         , loggingDirectory
                         , unameM
                         ) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.ByteString.Char8 as C8
import Data.Text as T
import qualified Slackware.Config as Config
import Slackware.Info
import Slackware.Error

data PackageEnvironment = PackageEnvironment String Config.Config

type PackageAction = PackageInfo
                  -> T.Text
                  -> ExceptT PackageError (ReaderT PackageEnvironment IO) ()

unameM :: PackageEnvironment -> String
unameM (PackageEnvironment unameM' _) = unameM'

loggingDirectory :: PackageEnvironment -> String
loggingDirectory (PackageEnvironment _ config)
    = T.unpack $ Config.loggingDirectory config
