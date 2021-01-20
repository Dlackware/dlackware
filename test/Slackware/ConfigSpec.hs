{-# LANGUAGE OverloadedStrings #-}
module Slackware.ConfigSpec
    ( spec
    ) where

import Control.Arrow (left)
import qualified Data.ByteString.Lazy.Char8 as C8L
import Data.Either (isLeft)
import qualified Data.Text as Text
import Slackware.Config
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

validConfiguration :: C8L.ByteString
validConfiguration = "reposRoot: /opt/dlackware-scripts\n\
                     \loggingDirectory: /var/log/dlackware\n\
                     \temporaryDirectory: /tmp/dlackware\n\
                     \gnomeVersion: 3.38.3\n\
                     \repos:\n\
                     \- repo1"

spec :: Spec
spec =
    describe "parseConfig" $ do
        it "parses valid configuration" $ do
            let expected = Config
                    { reposRoot = "/opt/dlackware-scripts"
                    , loggingDirectory = "/var/log/dlackware"
                    , temporaryDirectory = "/tmp/dlackware"
                    , repos = ["repo1"]
                    , gnomeVersion = "3.38.3"
                    }
            parseConfig "" validConfiguration `shouldBe` Right expected

        it "rejects empty configuration" $ do
            let actual = C8L.empty
            let expected = ": configuration is empty"
            parseConfig "" actual `shouldBe` Left expected

        it "rejects multiple Yaml documents" $ do
            let actual = C8L.intercalate "\n---\n" $ replicate 2 validConfiguration
            let expected = ": expected only one document"
            parseConfig "" actual `shouldBe` Left expected

        it "rejects incomplete configuration" $ do
            let actual = "reposRoot: /opt/dlackware-scripts"
            parseConfig "" actual `shouldSatisfy` isLeft

        it "returns error with the source filename" $ do
            let parsed = parseConfig "file.yaml" "reposR: /opt/dlackware-scripts"
            let actual = left (Text.isPrefixOf "file.yaml: ") parsed

            actual `shouldBe` Left True

        it "returns custom error with the source filename" $ do
            let actual = parseConfig "file.yaml" ""

            actual `shouldBe` Left "file.yaml: configuration is empty"
