{-# LANGUAGE OverloadedStrings #-}
module Slackware.VersionSpec (spec) where

import Data.Either (fromRight, isLeft, isRight)
import Slackware.Version
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  , shouldSatisfy
                  )
import Text.Megaparsec (parse)

spec :: Spec
spec = do
    describe "parse" $ do
        it "parses into a valid Version structure" $
            let actual = parse versions "" "core:gdm:3.33.92:\n\n"
                expected = Right [Version "gdm" "3.33.92"]
             in actual `shouldBe` expected
        it "parses multiple entries" $
            let actual = parse versions "" "core:gdm:3.33.92:\n\
                                           \core:gedit:3.33.92:\n\n"
             in fromRight 0 (length <$> actual) `shouldBe` 2
        it "skips comment line" $
            let actual = parse versions "" "core:gdm:3.33.92:\n\n"
                expected = Right [Version "gdm" "3.33.92"]
             in actual `shouldBe` expected
        it "fails on invalid lines" $
            let actual = parse versions "" "CORE\n\
                                           \core:gdm:3.33.92:\n\n"
             in actual `shouldSatisfy` isLeft
        it "skips leading comment" $
            let actual = parse versions "" "## CORE\n\
                                           \core:gdm:3.33.92:\n\n"
             in actual `shouldSatisfy` isRight

    describe "show" $
        it "prints the name and the version separated by a colon" $
            let actual = show $ Version "gdm" "3.33.92"
             in actual `shouldBe` "gdm:3.33.92"
