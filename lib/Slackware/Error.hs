{-#  LANGUAGE OverloadedStrings #-}
module Slackware.Error ( PackageError(..)
                       , PackageErrorType(..)
                       , showPackageError
                       ) where

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec.Error ( ParseErrorBundle(..)
                             , errorBundlePretty
                             )
import Network.HTTP.Req (HttpException)

data PackageErrorType
  = ChecksumMismatch
  | UnsupportedDownload
  | BuildError
  | InstallError IOError
  | DownloadError HttpException
  | ParseError (ParseErrorBundle C8.ByteString Void)

showPackageErrorType :: PackageErrorType -> T.Text
showPackageErrorType ChecksumMismatch = "Checksum mismatch"
showPackageErrorType UnsupportedDownload = "Found unsupported download URL type"
showPackageErrorType BuildError = "Built package installation failed"
showPackageErrorType (InstallError e) = T.pack $ show e
showPackageErrorType (DownloadError e) = T.pack $ show e
showPackageErrorType (ParseError e) = T.pack $ errorBundlePretty e

data PackageError = PackageError String PackageErrorType

showPackageError :: PackageError -> T.Text
showPackageError (PackageError pkgName errorType) = T.concat
    [ T.pack pkgName
    , ": "
    , showPackageErrorType errorType
    ]
