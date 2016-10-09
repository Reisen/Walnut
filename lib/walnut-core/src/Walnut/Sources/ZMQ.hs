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
  :: (MonadIO m)
  => m (M.MVar Message)

zmqProducer
  = Z.runZMQ $ do
      sock <- Z.socket Z.Pull
      mvar <- liftIO M.newEmptyMVar
      Z.bind sock "tcp://0.0.0.0:5555"
      _    <- Z.async $ forever $ do
        message <- const (Message "" "" "" "") <$> Z.receive sock
        liftIO (putMVar mvar message)

      pure mvar

