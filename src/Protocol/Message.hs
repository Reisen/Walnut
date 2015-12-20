module Protocol.Message
    ( Message(..)
    , embed
    , debed
    ) where


--------------------------------------------------------------------------------
import           Data.Serialize
import           Data.MessagePack
import qualified Data.ByteString.Char8 as B

import           Protocol.Protocol


--------------------------------------------------------------------------------
data Message = Message
    { messageFrom :: String
    , messageTo   :: String
    , messageTag  :: String
    , messageData :: B.ByteString
    } deriving (Show)


instance Transportable Message where
    embed :: Message -> B.ByteString
    embed Message{..} = encode $ ObjectArray
        [ (ObjectString . B.pack) messageFrom
        , (ObjectString . B.pack) messageTo
        , (ObjectString . B.pack) messageTag
        , ObjectString messageData
        ]

    debed :: B.ByteString -> Maybe Message
    debed m = either (const Nothing) Just (decode m) >>= check
        where
            check :: Object -> Maybe Message
            check (ObjectArray
                [ ObjectString a
                , ObjectString b
                , ObjectString c
                , ObjectString d
                ]
                ) = Just Message
                    { messageFrom = B.unpack a
                    , messageTo   = B.unpack b
                    , messageTag  = B.unpack c
                    , messageData = d
                    }

            check _ = Nothing
