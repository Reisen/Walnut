module Plugin.Plugin
    ( Callback
    , Plugin(..)
    , Message(..)
    , runPlugins
    ) where

--------------------------------------------------------------------------------
import           Data.Maybe               (mapMaybe)
import           Data.Serialize
import           Data.MessagePack
import qualified Data.ByteString.Char8 as C

import           Protocol.Message

--------------------------------------------------------------------------------
type Callback = Message -> Maybe Message

data Plugin = Plugin
    { pluginName :: String
    , pluginHook :: Callback
    }


--------------------------------------------------------------------------------
runPlugins :: Message -> [Plugin] -> [Message]
runPlugins v = mapMaybe $ ($ v) . pluginHook
