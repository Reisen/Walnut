module Walnut.Sources.ZMQ
  ( zmqReceive
  , zmqSockets
  ) where

import           Walnut.Internal.Types

import           Protolude
import           Control.Monad.IO.Class  (liftIO, MonadIO)
import qualified Control.Concurrent.MVar as M
import qualified Data.ByteString.Lazy    as L
import qualified Data.Conduit            as C
import qualified System.ZMQ4.Monadic     as Z


zmqReceive
  :: (MonadIO m)
  => M.MVar Message
  -> C.Source m Message

zmqReceive mvar
  = do
      message <- liftIO (M.takeMVar mvar)
      C.yield message
      zmqReceive mvar


zmqSockets
  :: (MonadIO m)
  => m (M.MVar Message, M.MVar Message)

zmqSockets
  = Z.runZMQ $ do
      sockR <- Z.socket Z.Pull
      mvarR <- liftIO M.newEmptyMVar
      Z.bind sockR "tcp://0.0.0.0:5555"
      _ <- Z.async $ forever
        (parseMessage . L.fromStrict <$> Z.receive sockR
          >>= \case
            Just m  -> liftIO (putMVar mvarR m)
            Nothing -> pure ())

      sockS <- Z.socket Z.Push
      mvarS <- liftIO M.newEmptyMVar
      Z.bind sockS "tcp://0.0.0.0:5556"
      _ <- Z.async $ forever $ do
        message <- liftIO (M.takeMVar mvarS)
        Z.send sockS [] (L.toStrict . compileMessage $ message)

      pure (mvarR, mvarS)

