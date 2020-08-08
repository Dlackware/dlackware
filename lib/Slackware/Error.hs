{-#  LANGUAGE OverloadedStrings #-}
module Slackware.Error
    ( PackageError(..)
    , PackageErrorType(..)
    , showPackageError
    ) where

import Control.Exception (Exception(..), displayException)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle(..), errorBundlePretty)
import Network.HTTP.Req (HttpException(..))

data PackageErrorType
    = ChecksumMismatch
    |  UnsupportedDownload
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

data PackageError = PackageError String PackageErrorType

instance Show PackageError where
    show (PackageError pkgName errorType) =
        pkgName ++ ": " ++ (show errorType)

instance Exception PackageError

showPackageError :: PackageError -> T.Text
showPackageError = T.pack . show
