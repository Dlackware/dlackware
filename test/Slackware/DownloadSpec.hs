module Slackware.DownloadSpec (spec) where

import qualified Data.Text as T
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
            let expected = T.pack "filename.tar.gz"
                actual = filename $ T.pack "http://some.url/dir/filename.tar.gz"
             in actual `shouldBe` expected
