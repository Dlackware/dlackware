module ArchSpec (spec) where

import           Arch ( parseArch
                      , uname
                      )
import           Data.Either (isLeft)
import           System.Process (readProcess)
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
        it "returns supported architecture" $ do
            actual <- uname 
            unameM <- readProcess "/usr/bin/uname" ["-m"] ""
            let expected = ["i586", "arm", init $ unameM]

            (actual `elem` expected) `shouldBe` True
