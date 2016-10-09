module Main where

import           Protolude
import qualified Walnut       as W

import qualified Data.Conduit as C
import           Data.Conduit (($$))


messageSink
  :: (MonadIO m)
  => C.Sink W.Message m ()

messageSink
  = do
      message <- C.await
      print message
      messageSink


main :: IO ()
main
  = do
      -- | Create a ZMQ Message source, the mvar produced here will continuously
      --   produce 'Message' data until the end of time.
      mvar <- W.zmqProducer
      let zmqSource = (W.zmqMessages mvar :: C.Source IO W.Message)
      zmqSource $$ messageSink
      putStrLn ("Hello Walnut" :: Text)
