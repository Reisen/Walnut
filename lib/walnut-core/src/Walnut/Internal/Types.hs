module Walnut.Internal.Types where

import Protolude

data WalnutEnv
  = WalnutEnv
      { envZMQ :: (Int, Text)
      }

newtype Walnut m a
  = Walnut
      { runWalnut :: ReaderT WalnutEnv m a
      } deriving
      ( Monad
      , Functor
      , Applicative
      )

data Message
  = Message
      { messageNetwork :: Text
      , messageChannel :: Text
      , messageUser    :: Text
      , messageContent :: Text
      } deriving (Show, Eq)
