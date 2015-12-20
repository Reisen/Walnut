module Plugin.Router
    ( router
    , chain
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
            { messageTo   = "*"
            , messageFrom = "router"
            , messageTag  = "command"
            , messageData = embed command { commandMessage = messageData }
            }


chain :: Callback
chain m@Message{..}
    | messageTag /= "response" = Nothing
    | otherwise = do
        command <- (debed messageData) :: Maybe Command
        message <- (debed . commandMessage) command :: Maybe Chat
        case length (commandList command) of
            0 -> Nothing
            1 -> pure Message
                { messageTo   = "protocol." ++ (chatProtocol message)
                , messageFrom = "command"
                , messageTag  = "message"
                , messageData = embed $ message
                    { chatFrom = chatTo message
                    , chatTo   = chatFrom message
                    , chatLine = head (commandList command)
                    }
                }
            _ -> let list = commandList command in pure Message
                { messageTo   = "*"
                , messageFrom = "router"
                , messageTag  = "command"
                , messageData = embed $ command
                    { commandList = B.concat [(head . tail) list, " ", head list] : drop 2 list }
                }

