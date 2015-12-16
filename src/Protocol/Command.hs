module Protocol.Command
    ( Command(..)
    , embed
    , debed
    ) where

--------------------------------------------------------------------------------
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
