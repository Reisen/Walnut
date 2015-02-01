{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network
import System.IO
import System.Timeout
import System.ZMQ4.Monadic
import Control.Arrow
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Text.Regex.PCRE
import qualified Data.ByteString.Char8 as C8



{- Create an encoded protocol message from a source, destination and a payload.
 - Automatically packed to be sent accross the network. -}
encodeRouter :: String -> String -> String -> C8.ByteString
encodeRouter f t s =
    let pattern   = "^(:\\S+)?\\s*(\\S+)\\s+(.*)\\r?$" :: String
        (_,_,_,g) = (s =~ pattern)                     :: (String, String, String, [String]) in

    C8.pack $ (g !! 1) ++ "(" ++ f ++ "," ++ t ++ ")" ++ s



{- Routing thread. Uses a ZMQ Response socket to receive messages and pipes
 - them back out through a ZMQ Publish socket for consumption. This ends up
 - working as a kind of broadcaster for messages. -}
route :: IO ()
route = runZMQ $ do
    liftIO (putStrLn "Creating Req -> Publisher Bridge")

    -- Open Publisher and Responder sockets.
    publisher <- socket Pub
    responder <- socket Rep
    bind publisher "tcp://0.0.0.0:9890"
    bind responder "tcp://0.0.0.0:9891"

    -- Bridge Req messages out into the Publisher socket.
    forever $ do
        msg <- receive responder
        send publisher [] msg
        send responder [] "R"



{- Main thread. This handles the actual heavy lifting of dealing with IRC
 - networking and config parsing. -}
main :: IO ()
main = do
    -- Run the routing thread.
    forkIO route

    -- Connect to IRC. Needs to work with a config parser, fix this.
    network <- connectTo "address.co.uk" (PortNumber $ fromIntegral 6667)
    mapM_ (hPutStrLn network) [
        "NICK hasknut",
        "USER hasknut hasknut hasknut :hasknut",
        "JOIN #hasknut"
        ]

    -- Start a ZMQ context so we can start routing messages.
    runZMQ $ do
        -- Connect to the ZMQ Publisher in the routing thread so that we
        -- can forward messages from plugins back out to IRC networks.
        sub <- socket Sub
        connect sub "tcp://0.0.0.0:9890"
        subscribe sub "W-OUT"

        -- Workhorse for actually routing.
        async $ forever $ do
            line <- receive sub
            let payload = tail . dropWhile (')'/=) . C8.unpack $ line
            liftIO (hPutStrLn network payload)

        -- Opens a ZMQ Request socket to the Responder in the routing
        -- thread. Messages received from networks are forwarded over
        -- this socket to be broadcast to plugins.
        req <- socket Req
        connect req "tcp://0.0.0.0:9891"

        -- Workhorse for actually routing.
        forever $ do
            let encoder = encodeRouter "8ace3bea" "*"
            message <- liftIO (hGetLine network)
            send req [] (encoder message)
            receive req
