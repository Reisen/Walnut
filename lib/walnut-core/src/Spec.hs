import Protolude
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main
  = hspec $ do
      describe "Source" $ do
        describe "zmqSource" $ do
          it "validates" $ do
            10 `shouldBe` 10

          it "validates more" $ do
            10 `shouldBe` 10
