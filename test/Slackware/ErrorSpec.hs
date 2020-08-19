{-# LANGUAGE OverloadedStrings #-}
module Slackware.ErrorSpec
    ( spec
    ) where

import Data.List (isPrefixOf)
import Slackware.Error
import Slackware.Info
import Test.Hspec (Spec, describe, it, shouldNotSatisfy, shouldSatisfy)
import Text.Megaparsec (parse)

spec :: Spec
spec =
    describe "showPackageError" $ do
        it "has an error message for checksum mismatch" $
             let actual = show $ PackageError "" ChecksumMismatch 
              in actual `shouldNotSatisfy` null
        it "has an error message for unsupported download" $
             let actual = show $ PackageError "" UnsupportedDownload
              in actual `shouldNotSatisfy` null
        it "has an error message for build error" $
             let actual = show $ PackageError "" BuildError
              in actual `shouldNotSatisfy` null

        it "prepends the error with the package name" $
            let actual = show $ PackageError "pkg" ChecksumMismatch
             in actual `shouldSatisfy` isPrefixOf "pkg: "

        it "shows info file parse errors" $
            let Left bundle = parse parseInfoFile "" "PRGNAM=\"pkgnam\"\n"
                actual = show $ PackageError "" $ ParseError bundle
             in actual `shouldSatisfy` ((> 1) . length . lines)
