module Walnut.Protocol
    ( Message
    , encode
    , decode
    ) where

import Control.Applicative
import Data.ByteString.Char8 as BC hiding (map)
import Data.Attoparsec.ByteString.Char8 as ABC


data Message = Message
    { messageTag     :: String
    , messageFrom    :: String
    , messageTo      :: String
    , messageArgs    :: [String]
    , messagePayload :: String
    } deriving (Show)


parseTag :: Parser ByteString
parseTag = takeTill isSpace


parseLocations :: Parser (ByteString, ByteString)
parseLocations = do
    char ' '
    from ← takeTill (=='!')
    char '!'
    to ← takeTill isSpace
    pure (from, to)


parseArgCount :: Parser Int
parseArgCount = char ' ' >> decimal


parseArgs :: Int → Parser [ByteString]
parseArgs 0 = pure []
parseArgs n = do
    char ' '
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
    Left _  → Nothing
    Right v → Just v


encode :: a
encode = undefined
