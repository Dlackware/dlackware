{-# LANGUAGE OverloadedStrings #-}
module Slackware.InfoSpec
    ( spec
    ) where

import Data.ByteString.Char8 (ByteString)
import Data.Void (Void)
import Slackware.Info
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (parseSatisfies, shouldSucceedOn)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (ParseErrorBundle)

parseInfoFile'
    :: ByteString
    -> Either (ParseErrorBundle ByteString Void) PackageInfo
parseInfoFile' = parse parseInfoFile ""

infoDownload1 :: ByteString
infoDownload1 = "PKGNAM=\"pkgnam\"\n\
            \VERSION=\"1.2.3\"\n\
            \HOMEPAGE=\"homepage\"\n\
            \DOWNLOAD=\"https://dlackware.com/download.tar.gz\"\n\
            \MD5SUM=\"0102030405060708090a0b0c0d0e0f10\"\n"

spec :: Spec
spec =
    describe "parseInfoFile" $ do
        it "returns package on a valid input" $
            parseInfoFile' `shouldSucceedOn` infoDownload1

        it "returns an array with one element if one download is given" $
            let condition = (== 1) . length . checksums
             in parseInfoFile' infoDownload1 `parseSatisfies` condition

        it "translates checksum characters into the binary format" $
            let expected = "0102030405060708090a0b0c0d0e0f10"
                condition = (== expected) . show . head . checksums
             in parseInfoFile' infoDownload1 `parseSatisfies` condition

        it "accepts an empty downloads list" $
            let infoDownload0 = "PKGNAM=\"pkgnam\"\n\
                \VERSION=\"1.2.3\"\n\
                \HOMEPAGE=\"homepage\"\n\
                \DOWNLOAD=\"\"\n\
                \MD5SUM=\"\"\n"
             in parseInfoFile' `shouldSucceedOn` infoDownload0
