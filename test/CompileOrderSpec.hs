{-# LANGUAGE OverloadedStrings #-}
module CompileOrderSpec (spec) where

import           CompileOrder ( Step(..)
                              , parseCompileOrder
                              )
import           Data.Either (isLeft)
import           Test.Hspec ( Spec
                            , describe
                            , it
                            , shouldBe
                            )

spec :: Spec
spec = do
    describe "parseCompileOrder" $ do
        it "splits into lines" $ do
            let actual = "package1\n\
                         \package2\n"
            let expected = [ PackageName Nothing "package1"
                           , PackageName Nothing "package2"
                           ]
            (parseCompileOrder "" actual) `shouldBe` (Right expected)

        it "parses empty file" $ do
            let actual = ""
            let expected = []
            (parseCompileOrder "" actual) `shouldBe` (Right expected)

        it "skips empty lines" $ do
            let actual = "\npackage\n\n"
            let expected = [PackageName Nothing "package"]
            (parseCompileOrder "" actual) `shouldBe` (Right expected)

        it "skips comments" $ do
            let actual = "# Comment\n"
            let expected = []
            (parseCompileOrder "" actual) `shouldBe` (Right expected)

        it "parses replacement package" $ do
            let actual = "a%b"
            let expected = [PackageName (Just "a") "b"]
            (parseCompileOrder "" actual) `shouldBe` (Right expected)

        it "discards invalid replacement package" $ do
            let actual = "a%"
            (isLeft $ parseCompileOrder "" actual) `shouldBe` True

    describe "show Step" $ do
        it "shows only new package" $ do
            let actual = PackageName Nothing "package"
            let expected = "package"
            (show actual) `shouldBe` expected

        it "shows old and replacement packages" $ do
            let actual = PackageName (Just "old") "new"
            let expected = "old%new"
            (show actual) `shouldBe` expected
