{-# LANGUAGE ExistentialQuantification #-}
{-#  LANGUAGE OverloadedStrings #-}

-- | Error handling.
module Slackware.Error
    ( BuildSystemException(..)
    , PackageError(..)
    , PackageErrorType(..)
    , showPackageError
    ) where

import Control.Exception (Exception(..), displayException)
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (cast)
import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle(..), errorBundlePretty)
import Network.HTTP.Req (HttpException(..))

-- | Root exception type for all the handled exceptions.
data BuildSystemException = forall e. Exception e => BuildSystemException e

instance Show BuildSystemException where
    show (BuildSystemException e) = show e

instance Exception BuildSystemException

-- | Types of the errors that can occur when buildingg a package.
data PackageErrorType
    = ChecksumMismatch
    | UnsupportedDownload
    | BuildError
    | InstallError IOError
    | DownloadError HttpException
    | ParseError (ParseErrorBundle C8.ByteString Void)
    | Missing

instance Show PackageErrorType where
    show ChecksumMismatch = "Checksum mismatch"
    show UnsupportedDownload = "Found unsupported download URL type"
    show BuildError = "Built package installation failed"
    show (InstallError e) = displayException e
    show (DownloadError e) = displayException e
    show (ParseError e) = errorBundlePretty e
    show Missing = "Not found in any compile order"

-- | Error buidling a package.
data PackageError = PackageError String PackageErrorType

instance Show PackageError where
    show (PackageError pkgName errorType) =
        pkgName ++ ": " ++ show errorType

instance Exception PackageError where
    toException = toException . BuildSystemException
    fromException x = do
        BuildSystemException a <- fromException x
        cast a

-- | Show an error as 'Text'.
showPackageError :: BuildSystemException -> Text
showPackageError = Text.pack . displayException
