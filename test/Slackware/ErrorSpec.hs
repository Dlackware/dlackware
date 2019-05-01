module Slackware.ErrorSpec (spec) where

import Slackware.Error
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  )

spec :: Spec
spec = do
    describe "show PackageErrorType" $ do
        it "has an error message for checksum mismatch" $
             let actual = show ChecksumMismatch 
              in null actual `shouldBe` False
        it "has an error message for unsupported download" $
             let actual = show UnsupportedDownload
              in null actual `shouldBe` False
        it "has an error message for build error" $
             let actual = show BuildError
              in null actual `shouldBe` False

    describe "show PackageError" $
        it "prepends the error with the package name" $
            let pkgName = "pkg"
                actual = show $ PackageError pkgName ChecksumMismatch
             in take (length pkgName + 2) actual `shouldBe` pkgName ++ ": "
