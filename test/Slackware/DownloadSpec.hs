module Slackware.DownloadSpec (spec) where

import qualified Data.ByteString.Char8 as C8
import           Slackware.Download (filename)
import           Test.Hspec ( Spec
                            , describe
                            , it
                            , shouldBe
                            )

spec :: Spec
spec =
    describe "filename" $
        it "returns the filename of the downloaded file" $
            let expected = C8.pack "filename.tar.gz"
                actual = filename $ C8.pack "http://some.url/dir/filename.tar.gz"
             in actual `shouldBe` expected
