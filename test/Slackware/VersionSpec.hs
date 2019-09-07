{-# LANGUAGE OverloadedStrings #-}
module Slackware.VersionSpec (spec) where

import Data.Either (fromRight)
import Slackware.Version
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  )
import Text.Megaparsec (parse)

spec :: Spec
spec =
    describe "parse" $ do
        it "parses into a valid Version structure" $
            let actual = parse versions "" "core:gdm:3.33.92:"
                expected = Right [Version "gdm" "3.33.92"]
             in actual `shouldBe` expected
        it "parses multiple entries" $
            let actual = parse versions "" "core:gdm:3.33.92:\n\
                                           \core:gedit:3.33.92:"
             in fromRight 0 (length <$> actual) `shouldBe` 2
