module Walnut
    ( Walnut
    , StateW(..)
    ) where

--------------------------------------------------------------------------------
import           Plugin
import           Nanomsg                  (Socket, Pub, Pull)
import           Control.Monad.State.Lazy (StateT)



--------------------------------------------------------------------------------
type Walnut = StateT StateW IO
data StateW = StateW
    { statePlugins :: [Plugin]
    , statePublish :: Socket Pub
    , stateListen  :: Socket Pull
    }
