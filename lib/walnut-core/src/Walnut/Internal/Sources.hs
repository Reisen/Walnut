module Walnut.Internal.Sources where

import           Protolude
import qualified Data.Conduit        as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Text           as T



receive
  :: ( Monad m
     , MonadIO m
     )

  => C.Source m T.Text

receive
  = undefined
