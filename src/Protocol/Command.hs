module Protocol.Command
    ( Command(..)
    , embed
    , debed
    , findCommand
    ) where


--------------------------------------------------------------------------------
import qualified Data.Text as T
import           Data.Text.Encoding       (decodeUtf8', encodeUtf8)
import           Data.Serialize
import           Data.MessagePack
import qualified Data.ByteString.Char8 as B

import           Protocol.Protocol


--------------------------------------------------------------------------------
data Command = Command
    { commandMessage :: B.ByteString
    , commandList    :: [B.ByteString]
    } deriving (Show)


instance Transportable Command where
    embed :: Command -> B.ByteString
    embed Command{..} = encode $ ObjectArray
        [ ObjectString commandMessage
        , ObjectArray (map ObjectString commandList)
        ]

    debed :: B.ByteString -> Maybe Command
    debed m = either (const Nothing) Just (decode m) >>= check
        where
            check :: Object -> Maybe Command
            check (ObjectArray
                [ ObjectString message
                , ObjectArray commands
                ]
                ) = Just Command
                    { commandMessage = message
                    , commandList    = map (\(ObjectString s) -> s) commands
                    }
            check _ = Nothing


findCommand :: B.ByteString -> Maybe Command
findCommand (decodeUtf8' -> Left _) = Nothing
findCommand (decodeUtf8' -> Right line)
    | T.head line /= '.' = Nothing
    | otherwise = do
        pure Command
            { commandMessage = B.pack ""
            , commandList = commands
            }

        where
            pipes    = T.splitOn "|" line
            commands = map (encodeUtf8 . T.strip) pipes
