{-# LANGUAGE OverloadedStrings #-}
module Slackware.DownloadSpec (spec) where

import Slackware.Download (filename)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.URI (mkURI)

spec :: Spec
spec =
    describe "filename" $
        it "returns the filename of the downloaded file" $ do
            actual <- mkURI "http://some.url/dir/filename.tar.gz"
            let expected = "filename.tar.gz"
             in filename actual `shouldBe` expected
