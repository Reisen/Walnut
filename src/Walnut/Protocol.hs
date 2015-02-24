module Walnut.Protocol
    ( Message(..)
    , encode
    , decode
    , setSender
    ) where

import Text.Printf
import Control.Applicative
import Data.ByteString.Char8 as BC (ByteString, pack, unpack)
import Data.Attoparsec.ByteString.Char8 as ABC


data Message = Message
    { messageTag     :: String
    , messageFrom    :: String
    , messageTo      :: String
    , messageArgs    :: [String]
    , messagePayload :: String
    } deriving (Show)


setSender :: String → Message → Message
setSender s m = m { messageFrom = s }


parseTag :: Parser ByteString
parseTag = takeTill isSpace


parseLocations :: Parser (ByteString, ByteString)
parseLocations = do
    _    ← char ' '
    from ← takeTill (=='!')
    _    ← char '!'
    to   ← takeTill isSpace
    pure (from, to)


parseArgCount :: Parser Int
parseArgCount = char ' ' >> decimal


parseArgs :: Int → Parser [ByteString]
parseArgs 0 = pure []
parseArgs n = do
    _    ← char ' '
    arg  ← takeTill isSpace
    rest ← parseArgs (n - 1)
    pure (arg : rest)


parsePayload :: Parser ByteString
parsePayload = char ' ' >> takeByteString


parseMessage :: Parser Message
parseMessage = do
    tag        ← parseTag
    (from, to) ← parseLocations
    argCount   ← parseArgCount
    args       ← parseArgs argCount
    payload    ← parsePayload
    pure Message
        { messageTag     = unpack tag
        , messageFrom    = unpack from
        , messageTo      = unpack to
        , messageArgs    = map unpack args
        , messagePayload = unpack payload }


decode :: ByteString → Maybe Message
decode line = case parseOnly parseMessage line of
    Right v → Just v
    _       → Nothing


encode :: Message → ByteString
encode Message
    { messageTag     = tag
    , messageFrom    = from
    , messageTo      = to
    , messageArgs    = args
    , messagePayload = payload } =
    let argsc = length args
        argsv = unwords args in
        pack (printf "%s %s!%s %d %s %s" tag from to argsc argsv payload)
