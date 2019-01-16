module Slackware.ArchSpec (spec) where

import Slackware.Arch ( grepSlackBuild
                      , parseArch
                      , uname
                      )
import           Data.Either (isLeft)
import           Test.Hspec ( Spec
                            , describe
                            , it
                            , shouldBe
                            )

spec :: Spec
spec = do
    describe "parseArch" $ do
        it "returns i586 for i586" $
            let actual = "i586"
                expected = "i586"
             in parseArch actual `shouldBe` Right expected

        it "returns i586 for i?86" $
            let actual = "i686"
                expected = "i586"
             in parseArch actual `shouldBe` Right expected

        it "returns arm for arm*" $
            let actual = "armv7l"
                expected = "arm"
             in parseArch actual `shouldBe` Right expected

        it "parses x86_64" $
            let actual = "x86_64"
             in isLeft (parseArch actual) `shouldBe` True

    describe "uname" $
        it "returns uname output if the parser fails" $
            let actual = "x86_64\n"
                expected = "x86_64"
             in uname actual `shouldBe` expected

    describe "grepSlackBuild" $
        it "extracts build number" $
            let actual = grepSlackBuild "BUILD=${BUILD:-1}"
                expected = ("1", "")
             in actual `shouldBe` expected
