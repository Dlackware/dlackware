{-# LANGUAGE OverloadedStrings #-}
module Slackware.PackageSpec (spec) where

import qualified Data.ByteString.Char8 as C8
import Data.Either ( fromRight
                   , isRight
                   )
import Data.Void (Void)
import Slackware.Package ( Package(..)
                         , parseInfoFile
                         )
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  )
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (ParseErrorBundle)

parseInfoFile' :: C8.ByteString -> Either (ParseErrorBundle C8.ByteString Void) Package
parseInfoFile' = (parse parseInfoFile) ""

infoDownload1 :: C8.ByteString
infoDownload1 = "PKGNAM=\"pkgnam\"\n\
            \VERSION=\"1.2.3\"\n\
            \HOMEPAGE=\"homepage\"\n\
            \DOWNLOAD=\"https://dlackware.com/download.tar.gz\"\n\
            \MD5SUM=\"0102030405060708090a0b0c0d0e0f10\"\n"

spec :: Spec
spec = do
    describe "parseInfoFile" $ do
        it "returns package on a valid input" $ do
            isRight (parseInfoFile' infoDownload1) `shouldBe` True

        it "returns an array with one element if one download is given" $ do
            let length' = length . checksums
                actual = length' <$> (parseInfoFile' infoDownload1)
             in fromRight 0 actual `shouldBe` 1

        it "translates checksum characters into the binary format" $ do
            let actual = (show . head . checksums) <$> (parseInfoFile' infoDownload1)
                expected = "0102030405060708090a0b0c0d0e0f10"
             in fromRight "" actual `shouldBe` expected
