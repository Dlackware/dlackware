{-# LANGUAGE OverloadedStrings #-}
module Slackware.CompileOrderSpec (spec) where

import Slackware.CompileOrder ( Step(..)
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
        it "splits into lines" $
            let actual = "package1\n\
                         \package2\n"
                expected = [ PackageName Nothing "package1"
                           , PackageName Nothing "package2"
                           ]
             in parseCompileOrder "" actual `shouldBe` Right expected

        it "parses empty file" $
            let actual = ""
                expected = []
             in parseCompileOrder "" actual `shouldBe` Right expected

        it "skips empty lines" $
            let actual = "\npackage\n\n"
                expected = [PackageName Nothing "package"]
             in parseCompileOrder "" actual `shouldBe` Right expected

        it "skips comments" $
            let actual = "# Comment\n"
                expected = []
             in parseCompileOrder "" actual `shouldBe` Right expected

        it "parses replacement package" $
            let actual = "a%b"
                expected = [PackageName (Just "a") "b"]
             in parseCompileOrder "" actual `shouldBe` Right expected

        it "discards invalid replacement package" $
            let actual = "a%"
             in isLeft (parseCompileOrder "" actual) `shouldBe` True

    describe "show Step" $ do
        it "shows only new package" $
            let actual = PackageName Nothing "package"
                expected = "package"
             in show actual `shouldBe` expected

        it "shows old and replacement packages" $
            let actual = PackageName (Just "old") "new"
                expected = "old%new"
             in show actual `shouldBe` expected
