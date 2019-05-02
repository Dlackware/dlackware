{-# LANGUAGE OverloadedStrings #-}
module Slackware.ErrorSpec (spec) where

import qualified Data.Text as T
import Slackware.Error
import Slackware.Info
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  )
import Text.Megaparsec (parse)

spec :: Spec
spec =
    describe "showPackageError" $ do
        it "has an error message for checksum mismatch" $
             let actual = showPackageError $ PackageError "" ChecksumMismatch 
              in T.null actual `shouldBe` False
        it "has an error message for unsupported download" $
             let actual = showPackageError $ PackageError "" UnsupportedDownload
              in T.null actual `shouldBe` False
        it "has an error message for build error" $
             let actual = showPackageError $ PackageError "" BuildError
              in T.null actual `shouldBe` False

        it "prepends the error with the package name" $
            let pkgName = "pkg"
                actual = showPackageError $ PackageError pkgName ChecksumMismatch
             in T.isPrefixOf (T.pack $ pkgName ++ ": ") actual `shouldBe` True

        it "shows info file parse errors" $ do
            let info = "PRGNAM=\"pkgnam\"\n"
                Left bundle = parse parseInfoFile "" info
                actual = showPackageError $ PackageError "" $ ParseError bundle
            length (T.lines actual) > 1 `shouldBe` True
