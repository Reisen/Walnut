module Walnut.Protocol
    ( Message
    , encode
    , decode
    ) where

import Control.Applicative
import Data.ByteString.Char8 as BC
import Data.Attoparsec.ByteString.Char8 as ABC


data Message = Message
    { messageTag     :: String
    , messageFrom    :: String
    , messageTo      :: String
    , messageArgs    :: [String]
    , messagePayload :: String }


parseTag :: Parser ByteString
parseTag = takeTill (not . isSpace)


parseLocations :: Parser (ByteString, ByteString)
parseLocations = do
    from ← takeTill (=='!')
    char '!'
    to ← takeTill (==' ')
    pure (from, to)


parseMessage :: Parser Message
parseMessage = do
    tag        ← parseTag
    (from, to) ← parseLocations
    args       ← parseArgs
    payload    ← parsePayload
    pure Message
        { messageTag     = tag
        , messageFrom    = from
        , messageTo      = to
        , messageArgs    = args
        , messagePayload = payload }


decode :: String → Maybe Message
decode = parseOnly parseMessage
