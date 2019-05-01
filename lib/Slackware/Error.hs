module Slackware.Error ( PackageError(..)
                       , PackageErrorType(..)
                       ) where

import qualified Data.ByteString.Char8 as C8
import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle(..))
import Network.HTTP.Req (HttpException)

data PackageErrorType
  = ChecksumMismatch
  | UnsupportedDownload
  | BuildError
  | InstallError IOError
  | DownloadError HttpException
  | ParseError (ParseErrorBundle C8.ByteString Void)

instance Show PackageErrorType where
    show ChecksumMismatch = "Checksum mismatch"
    show UnsupportedDownload = "Found unsupported download URL type"
    show BuildError = "Built package installation failed"
    show (InstallError e) = show e
    show (DownloadError e) = show e
    show (ParseError e) = show e

data PackageError = PackageError String PackageErrorType

instance Show PackageError where
    show (PackageError pkgName errorType) = pkgName ++ ": " ++ show errorType
