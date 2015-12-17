module Plugin.Router
    ( router
    ) where

--------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as B

import           Plugin.Plugin
import           Protocol.Command
import           Protocol.Chat

--------------------------------------------------------------------------------
router :: Callback
router m@Message{..}
    | messageTag /= "message" = Nothing
    | otherwise = do
        message <- (debed messageData) :: Maybe Chat
        command <- (findCommand . chatLine) message
        pure Message
            { messageTo   = "router"
            , messageFrom = "*"
            , messageTag  = "command"
            , messageData = embed command { commandMessage = messageData }
            }
