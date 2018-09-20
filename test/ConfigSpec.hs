module ConfigSpec (spec) where

import           Config ( Config(..)
                        , parseConfig
                        )
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Either (isLeft)
import qualified Data.Text as T
import           Test.Hspec ( Spec
                            , describe
                            , it
                            , shouldBe
                            )

spec :: Spec
spec = do
    describe "parseConfig" $ do
        it "parses valid configuration" $ do
            let actual = BS.pack "reposRoot: /opt/dlackware-scripts\n\
                                 \repos:\n\
                                 \- repo1"
            let expected = Config
                    { reposRoot = (T.pack "/opt/dlackware-scripts")
                    , repos     = [(T.pack "repo1")]
                    }
            (parseConfig actual) `shouldBe` (Right expected)

        it "rejects empty configuration" $ do
            let actual = BS.empty
            let expected = "Configuration is empty"
            (parseConfig actual) `shouldBe` (Left expected)

        it "rejects multiple Yaml documents" $ do
            let actual = BS.pack "reposRoot: /opt/dlackware-scripts\n\
                                 \repos: []\n\
                                 \---\n\
                                 \reposRoot: /opt/dlackware-scripts\n\
                                 \repos: []"
            let expected = "Expected only one document"
            (parseConfig actual) `shouldBe` (Left expected)

        it "rejects incomplete configuration" $ do
            let actual = BS.pack "reposRoot: /opt/dlackware-scripts"
            (isLeft $ parseConfig actual) `shouldBe` True
