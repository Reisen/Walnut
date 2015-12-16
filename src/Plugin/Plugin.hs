module Plugin.Plugin
    ( Callback
    , Plugin(..)
    , Message(..)
    , packMessage
    , unpackMessage
    , runPlugins
    ) where

--------------------------------------------------------------------------------
import           Data.Maybe               (mapMaybe)
import           Data.Serialize
import           Data.MessagePack
import qualified Data.ByteString.Char8 as C



--------------------------------------------------------------------------------
data Message = Message
    { messageFrom :: String
    , messageTo   :: String
    , messageTag  :: String
    , messageData :: String
    } deriving (Show)

{-
 - Convert a message object into an ObjectArray, in the MessagePack format for
 - sending over the wire.
 -}
packMessage :: Message -> C.ByteString
packMessage Message{..} = encode $ ObjectArray
    [ (ObjectString . C.pack) messageFrom
    , (ObjectString . C.pack) messageTo
    , (ObjectString . C.pack) messageTag
    , (ObjectString . C.pack) messageData
    ]

{-
 - Unpack a MessagePack object into a Message. This uses a bit of a dirty
 - pattern matching index function. I'm certain, CERTAIN there are better ways
 - to do this but this works, I will come back to this in the future.
 -}
unpackMessage :: C.ByteString -> Maybe Message
unpackMessage = (check <$>) . either (const Nothing) Just . decode
    where
        check :: Object -> Message
        check (ObjectArray
            [ (ObjectString a)
            , (ObjectString b)
            , (ObjectString c)
            , (ObjectString d)
            ]
            ) = Message
                { messageFrom = C.unpack a
                , messageTo   = C.unpack b
                , messageTag  = C.unpack c
                , messageData = C.unpack d
                }

        {-
         - The data part of any message is expected to be just that, data. So
         - to handle any other message formats we'll just ignore the message
         - sent and throw out an error instead.
         -}
        check _ = Message
            { messageFrom = "walnut"
            , messageTo   = "*"
            , messageTag  = "error"
            , messageData = "Malformed message."
            }

{-
 - When a plugin wants to process a message, it works with this very
 - basic message format. The message data might be a more complicated
 - format but the plugin will have to deal with that in some manner
 - outside the plugin system.
 -}
type Callback = Message -> Maybe Message

{-
 - An actual Plugin has a name, and a Callback function which can process a
 - message and return a result. Note that messages that get processed could be
 - from any protocol, come from anywhere, and be sent anywhere, so this is very
 - flexible.
 -}
data Plugin = Plugin
    { pluginName :: String
    , pluginHook :: Callback
    }


--------------------------------------------------------------------------------
runPlugins :: Message -> [Plugin] -> [Message]
runPlugins v = mapMaybe $ ($ v) . pluginHook
