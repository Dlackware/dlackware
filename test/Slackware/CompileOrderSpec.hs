{-# LANGUAGE OverloadedStrings #-}
module Slackware.CompileOrderSpec
    ( spec
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Slackware.CompileOrder
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn)

spec :: Spec
spec = do
    describe "parseCompileOrder" $ do
        it "splits into lines" $
            let actual = "package1\n\
                         \package2\n"
                expected = PackageName Nothing "package1"
                    :| [PackageName Nothing "package2"]
             in parseCompileOrder "" actual `shouldParse` expected

        it "fails on an empty file" $
            parseCompileOrder "" `shouldFailOn` ""

        it "skips empty lines" $
            let actual = "\npackage\n\n"
                expected = PackageName Nothing "package" :| []
             in parseCompileOrder "" actual `shouldParse` expected

        it "skips comments" $
            let actual = "# Comment\n\
                         \package\n"
                expected = PackageName Nothing "package" :| []
             in parseCompileOrder "" actual `shouldParse` expected

        it "parses replacement package" $
            let actual = "a%b"
                expected = PackageName (Just "a") "b" :| []
             in parseCompileOrder "" actual `shouldParse` expected

        it "discards invalid replacement package" $
            let actual = "a%"
             in parseCompileOrder "" `shouldFailOn` actual

    describe "show Step" $ do
        it "shows only new package" $
            let actual = PackageName Nothing "package"
                expected = "package"
             in show actual `shouldBe` expected

        it "shows old and replacement packages" $
            let actual = PackageName (Just "old") "new"
                expected = "old%new"
             in show actual `shouldBe` expected
