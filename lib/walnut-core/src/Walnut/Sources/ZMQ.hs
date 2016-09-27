module Walnut.Sources.ZMQ
  ( zmqMessages
  , zmqProducer
  ) where

import           Walnut.Internal.Types

import           Protolude
import           Control.Monad.IO.Class  (liftIO, MonadIO)
import qualified Control.Concurrent.MVar as M
import qualified Data.Conduit            as C
import qualified System.ZMQ4.Monadic     as Z

zmqMessages
  :: (MonadIO m)
  => M.MVar Message
  -> C.Source m Message

zmqMessages mvar
  = do
      message <- liftIO (M.takeMVar mvar)
      C.yield message
      zmqMessages mvar


zmqProducer
  :: IO (M.MVar Message)

zmqProducer
  = Z.runZMQ $ do
      sock <- Z.socket Z.Pair
      mvar <- liftIO M.newEmptyMVar
      _    <- Z.async $ forever $ do
        message <- undefined <$> Z.receive sock
        liftIO (putMVar mvar message)

      pure mvar

