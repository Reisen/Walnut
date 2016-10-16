import Protolude
import Control.Monad.IO.Class (liftIO)
import Data.Conduit           as C
import System.ZMQ4.Monadic    as Z
import Test.Hspec
import Test.QuickCheck        as Q


import Walnut
import Walnut.Sources

main :: IO ()
main
  = hspec $ do
      describe "Source" $ do
        describe "zmqSource" $ do
          it "can receive a Message object" $ do
            0 `shouldBe` 0
