module Protocol.Chat
    ( Chat(..)
    , embed
    , debed
    ) where


--------------------------------------------------------------------------------
import           Data.Serialize
import           Data.MessagePack
import qualified Data.ByteString.Char8 as B

import           Protocol.Protocol


--------------------------------------------------------------------------------
data Chat = Chat
    { chatProtocol :: String
    , chatFrom     :: B.ByteString
    , chatTo       :: B.ByteString
    , chatLine     :: B.ByteString
    , chatMeta     :: B.ByteString
    }


instance Transportable Chat where
    embed :: Chat -> B.ByteString
    embed Chat{..} = encode $ ObjectArray
        [ (ObjectString . B.pack) chatProtocol
        , ObjectString chatFrom
        , ObjectString chatTo
        , ObjectString chatLine
        , ObjectString chatMeta
        ]

    debed :: B.ByteString -> Maybe Chat
    debed m = either (const Nothing) Just (decode m) >>= check
        where
            check :: Object -> Maybe Chat
            check (ObjectArray
                [ ObjectString a
                , ObjectString b
                , ObjectString c
                , ObjectString d
                , ObjectString e
                ]
                ) = Just Chat
                    { chatProtocol = B.unpack a
                    , chatFrom     = b
                    , chatTo       = c
                    , chatLine     = d
                    , chatMeta     = e
                    }

            check _ = Nothing
