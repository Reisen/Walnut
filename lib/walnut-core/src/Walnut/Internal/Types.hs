module Walnut.Internal.Types where

import           Protolude
import           Data.Aeson           ((.:), (.=))
import qualified Control.Monad        as M
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as L


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
      { messageNetwork :: !Text
      , messageChannel :: !Text
      , messageUser    :: !Text
      , messageContent :: !Text
      } deriving (Show, Eq)

instance A.FromJSON Message where
  parseJSON (A.Object v)
    = Message
        <$> (v .: "network")
        <*> (v .: "channel")
        <*> (v .: "user")
        <*> (v .: "content")

  parseJSON _ = M.mzero

instance A.ToJSON Message where
  toJSON Message{..}
    = A.object
        [ "network" .= messageNetwork
        , "channel" .= messageChannel
        , "user"    .= messageUser
        , "content" .= messageContent
        ]

parseMessage
  :: L.ByteString
  -> Maybe Message

parseMessage = A.decode

compileMessage
  :: Message
  -> L.ByteString

compileMessage = A.encode
