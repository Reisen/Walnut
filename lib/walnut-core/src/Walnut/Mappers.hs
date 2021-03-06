module Walnut.Mappers where

import           Walnut.Internal.Types

import           Protolude
import           Control.Monad.IO.Class  (MonadIO)
import qualified Data.Conduit            as C
import qualified Data.Conduit.List       as CL

mapPlugins
  :: C.Conduit Message m Message

mapPlugins
  = undefined

printer
  :: (MonadIO m)
  => C.Conduit Message m Message

printer
  = CL.iterM (liftIO . print)
