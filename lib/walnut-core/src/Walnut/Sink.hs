module Walnut.Sink
  ( zmqForward
  ) where

import           Walnut.Internal.Types

import           Protolude
import           Control.Monad.IO.Class  (MonadIO)
import qualified Control.Concurrent.MVar as M
import qualified Data.Conduit            as C
import qualified Data.Conduit.List       as CL


zmqForward
  :: (MonadIO m)
  => M.MVar Message
  -> C.Sink Message m ()

zmqForward mvar
  = CL.mapM_ (liftIO . M.putMVar mvar)
