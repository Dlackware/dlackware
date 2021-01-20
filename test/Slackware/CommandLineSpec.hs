module Slackware.CommandLineSpec
    ( spec
    ) where

import Options.Applicative (ParserResult(..))
import Slackware.CommandLine
import Test.Hspec
    ( Expectation
    , Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    )

shouldParse :: ParserResult Program -> Program -> Expectation
shouldParse (Success success) expected = success `shouldBe` expected
shouldParse (Failure failure) expected
    = expectationFailure
    $ "expected: " ++ show expected
    ++ "\nbut parsing failed with:\n" ++ show failure
shouldParse (CompletionInvoked completion) expected
    = expectationFailure
    $ "expected: " ++ show expected
    ++ "\nbut parsing failed with:\n" ++ show completion

spec :: Spec
spec = do
    describe "opts" $ do
        it "parses update-gnome command" $
            let actual = ["update-gnome"]
                expected = UpgradeAll
             in execOptsParserPure actual `shouldParse` expected
