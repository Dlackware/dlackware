module Slackware.DownloadSpec (spec) where

import qualified Data.ByteString.Char8 as C8
import           Slackware.Download (filename)
import           Test.Hspec ( Spec
                            , describe
                            , it
                            , shouldBe
                            )

spec :: Spec
spec = do
    describe "filename" $ do
        it "returns the filename of the downloaded file" $ do
            let expected = C8.pack "filename.tar.gz"
            let actual = filename $ C8.pack "http://some.url/dir/filename.tar.gz"
            actual `shouldBe` expected
