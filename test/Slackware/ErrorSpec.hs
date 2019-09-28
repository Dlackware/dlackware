{-# LANGUAGE OverloadedStrings #-}
module Slackware.ErrorSpec
    ( spec
    ) where

import qualified Data.Text as Text
import Slackware.Error
import Slackware.Info
import Test.Hspec (Spec, describe, it, shouldNotSatisfy, shouldSatisfy)
import Text.Megaparsec (parse)

spec :: Spec
spec =
    describe "showPackageError" $ do
        it "has an error message for checksum mismatch" $
             let actual = showPackageError $ PackageError "" ChecksumMismatch 
              in actual `shouldNotSatisfy` Text.null
        it "has an error message for unsupported download" $
             let actual = showPackageError $ PackageError "" UnsupportedDownload
              in actual `shouldNotSatisfy` Text.null
        it "has an error message for build error" $
             let actual = showPackageError $ PackageError "" BuildError
              in actual `shouldNotSatisfy` Text.null

        it "prepends the error with the package name" $
            let actual = showPackageError $ PackageError "pkg" ChecksumMismatch
             in actual `shouldSatisfy` Text.isPrefixOf "pkg: "

        it "shows info file parse errors" $
            let Left bundle = parse parseInfoFile "" "PRGNAM=\"pkgnam\"\n"
                actual = showPackageError $ PackageError "" $ ParseError bundle
             in actual `shouldSatisfy` ((> 1) . length . Text.lines)
