module Walnut.Internal.Types where

import           Protolude
import qualified Data.Text as T

data Message
  = Message
      { messageNetwork :: T.Text
      , messageChannel :: T.Text
      , messageUser    :: T.Text
      , messageContent :: T.Text
      } deriving (Show, Eq)
