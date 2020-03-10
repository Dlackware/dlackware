{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
module Slackware.InfoSpec
    ( spec
    ) where

import Crypto.Hash (digestFromByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (maybeToList)
import qualified Data.Text.Encoding as Text
import Data.Void (Void)
import Slackware.Info
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (parseSatisfies, shouldSucceedOn)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.URI (mkURI)

parseInfoFile'
    :: ByteString
    -> Either (ParseErrorBundle ByteString Void) PackageInfo
parseInfoFile' = parse parseInfoFile ""

infoDownload0 :: ByteString
infoDownload0 = "PKGNAM=\"pkgnam\"\n\
    \VERSION=\"1.2.3\"\n\
    \HOMEPAGE=\"homepage\"\n\
    \DOWNLOAD=\"\"\n\
    \MD5SUM=\"\"\n"

infoDownload1 :: ByteString
infoDownload1 = "PKGNAM=\"pkgnam\"\n\
    \VERSION=\"1.2.3\"\n\
    \HOMEPAGE=\"homepage\"\n\
    \DOWNLOAD=\"https://dlackware.com/download.tar.gz\"\n\
    \MD5SUM=\"0102030405060708090a0b0c0d0e0f10\"\n"

maybeToDoubleList :: forall a. Maybe a -> [a]
maybeToDoubleList xs = [y | x <- maybeToList xs, y <- [x, x]]

spec :: Spec
spec = do
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
            parseInfoFile' `shouldSucceedOn` infoDownload0

    describe "generate" $ do
        it "generates an .info file without downloads" $
            let given =  PackageInfo "pkgnam" "1.2.3" "homepage" [] []
             in generate given `shouldBe` Text.decodeUtf8 infoDownload0

        it "splits multiple downloads into multiple lines" $
            let downloads' = maybeToDoubleList
                    $ mkURI "https://dlackware.com/download.tar.gz"
                checksums' = maybeToDoubleList
                    $ digestFromByteString (ByteString.pack [1.. 16])
                given = PackageInfo
                    "pkgnam" "1.2.3" "homepage" downloads' checksums'
                expected = "PKGNAM=\"pkgnam\"\n\
                    \VERSION=\"1.2.3\"\n\
                    \HOMEPAGE=\"homepage\"\n\
                    \DOWNLOAD=\"https://dlackware.com/download.tar.gz \\\n\
                    \          https://dlackware.com/download.tar.gz\"\n\
                    \MD5SUM=\"0102030405060708090a0b0c0d0e0f10 \\\n\
                    \        0102030405060708090a0b0c0d0e0f10\"\n"
             in generate given `shouldBe` expected

        it "prints the checksum as a sequence of hexadecimal numbers" $
            let downloads' = maybeToList
                    $ mkURI "https://dlackware.com/download.tar.gz"
                checksums' = maybeToList
                    $ digestFromByteString (ByteString.pack [1.. 16])
                given = PackageInfo
                    "pkgnam" "1.2.3" "homepage" downloads' checksums'
             in generate given `shouldBe` Text.decodeUtf8 infoDownload1
