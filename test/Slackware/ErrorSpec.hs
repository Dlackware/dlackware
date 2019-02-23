module Slackware.ErrorSpec (spec) where

import Slackware.Error (PackageError(..))
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  )

spec :: Spec
spec =
    describe "show PackageError" $ do
        it "has an error message for checksum mismatch" $
             let actual = show ChecksumMismatch 
              in null actual `shouldBe` False
        it "has an error message for unsupported download" $
             let actual = show UnsupportedDownload
              in null actual `shouldBe` False
        it "has an error message for build error" $
             let actual = show BuildError
              in null actual `shouldBe` False
