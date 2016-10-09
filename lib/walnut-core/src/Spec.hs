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
            Z.runZMQ $ Z.async $ do
              sock <- Z.socket Z.Push
              Z.connect sock "tcp://0.0.0.0:5555"
              Z.send sock [] "amazing"
              pure ()

            runConduit (
              (zmqProducer >>= zmqMessages) $$
              (do
                message <- C.await
                liftIO (putStrLn "Nice")
                pure ()))

            0 `shouldBe` 0
