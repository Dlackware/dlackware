module ArchSpec (spec) where

import           Arch ( grepSlackBuild
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
        it "returns i586 for i586" $ do
            let actual = "i586"
            let expected = "i586"
            (parseArch actual) `shouldBe` (Right expected)

        it "returns i586 for i?86" $ do
            let actual = "i686"
            let expected = "i586"
            (parseArch actual) `shouldBe` (Right expected)

        it "returns arm for arm*" $ do
            let actual = "armv7l"
            let expected = "arm"
            (parseArch actual) `shouldBe` (Right expected)

        it "parses x86_64" $ do
            let actual = "x86_64"
            (isLeft $ parseArch actual) `shouldBe` True

    describe "uname" $ do
        it "returns uname output if the parser fails" $ do
            let actual = "x86_64\n"
            let expected = "x86_64"
            (uname actual) `shouldBe` expected

    describe "grepSlackBuild" $ do
        it "extracts build number" $ do
            let actual = grepSlackBuild "BUILD=${BUILD:-1}"
            let expected = ("1", "")

            actual `shouldBe` expected
