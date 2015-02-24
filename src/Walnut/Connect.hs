{-# LANGUAGE NoOverloadedStrings #-}

module Walnut.Connect
    ( Server(..)
    , Conn(..)
    , connect
    , endline
    , sendIRC
    , recvIRC
    , convIRC
    ) where

import Data.List
import System.IO
import Text.Printf
import Text.Regex.PCRE
import Control.Monad
import Control.Applicative
import Network.Connection
import qualified Data.ByteString.Char8 as BC

import Walnut.Config
import Walnut.Protocol


data Conn = Conn
    { connContext :: ConnectionContext
    , connNetwork :: Connection }


connect :: Server → IO Conn
connect server = do
    context ← initConnectionContext
    network ← connectTo context ConnectionParams
        { connectionHostname  = serverHost server
        , connectionPort      = fromIntegral (serverPort server)
        , connectionUseSecure = Just $ TLSSettingsSimple True False True
        , connectionUseSocks  = Nothing }

    -- Send Server Password
    maybe (pure ()) (connectionPut network . endline . ("PASS "++)) (serverPass server)

    -- Send User Registration & Join Channels
    let nick = serverNick server
    connectionPut network . endline $ "NICK " ++ nick
    connectionPut network . endline $ printf "USER %s %s %s :%s" nick nick nick nick
    forM_ (serverChan server) $ connectionPut network . endline . ("JOIN "++)
    pure Conn
        { connContext = context
        , connNetwork = network }


endline :: String → BC.ByteString
endline = BC.pack . (++"\n")


sendIRC :: Conn → String → IO ()
sendIRC c = connectionPut (connNetwork c) . endline


recvIRC :: Conn → IO [String]
recvIRC c = (split . BC.unpack) `fmap` connectionGetLine 128 (connNetwork c)
    where split = (tail . head) . (=~ "^(:\\S+)?\\s*(\\S+)\\s+(.*)\\r?$")


convIRC :: [String] → Maybe Message
convIRC msg@[prefix, command, args] = Just Message
    { messageTag     = "IRC:" ++ command
    , messageFrom    = ""
    , messageTo      = ""
    , messageArgs    = []
    , messagePayload = unwords msg }
