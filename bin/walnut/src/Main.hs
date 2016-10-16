module Main where

import           Protolude
import qualified Walnut       as W

import           Data.Conduit (($$), ($=))


main :: IO ()
main
  = do
      -- | Create a ZMQ Message source, the mvar produced here will continuously
      --   produce 'Message' data until the end of time.
      (mvarR, mvarS) <- W.zmqSockets

      -- | Run our Conduit.
      W.zmqReceive mvarR
        $= W.printer
        $$ W.zmqForward mvarS
