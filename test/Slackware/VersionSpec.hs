{-# LANGUAGE OverloadedStrings #-}
module Slackware.VersionSpec (spec) where

import qualified Data.Map.Strict as Map
import Slackware.Version
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec ( parseSatisfies
                             , shouldFailOn
                             , shouldParse
                             , shouldSucceedOn
                             )
import Text.Megaparsec (parse)

spec :: Spec
spec
    = describe "parse" $ do
        it "parses into a valid Version structure" $
            let actual = parse versions "" "core:gdm:3.33.92:\n\n"
                expected = Map.singleton "gdm" "3.33.92"
             in actual `shouldParse` expected
        it "parses multiple entries" $
            let actual = parse versions "" "core:gdm:3.33.92:\n\
                                           \core:gedit:3.33.92:\n\n"
             in actual `parseSatisfies` ((== 2) . length)
        it "skips comment line" $
            let actual = parse versions "" "core:gdm:3.33.92:\n\n"
                expected = Map.singleton "gdm" "3.33.92"
             in actual `shouldParse` expected
        it "fails on invalid lines" $
            let content = "CORE\n\
                          \core:gdm:3.33.92:\n\n"
             in parse versions "" `shouldFailOn` content
        it "skips leading comment" $
            let content = "## CORE\n\
                          \core:gdm:3.33.92:\n\n"
             in parse versions "" `shouldSucceedOn` content
        it "doesn't expect a trailing newline" $
            let content = "core:gdm:3.33.92:\n"
             in parse versions "" `shouldSucceedOn` content
        it "forces parsing till the end of file" $
            let content = "core:gdm:3.33.92:\ninvalid-entry\n"
             in parse versions "" `shouldFailOn` content
