{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Network.Connection
import System.IO
import System.Timeout
import System.ZMQ4.Monadic
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Text.Printf
import Text.Regex.PCRE
import Data.Aeson

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8Lazy

import Config



{- Create an encoded protocol message from a source, destination and a payload.
 - Automatically packed to be sent accross the network. -}
encodeRouter :: String -> String -> String -> C8.ByteString
encodeRouter f t s =
    let pattern = "^(:\\S+)?\\s*(\\S+)\\s+(.*)\\r?$" :: String
        matches = (tail . head) (s =~ pattern)       :: [String]
        format  = "IRC:%s(%s,%s)%s"
        in

    C8.pack $ printf format (matches !! 1) f t (reverse . tail . reverse $ s)



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



ircPack :: String -> C8.ByteString
ircPack = C8.pack . (++"\n")



ircLoop :: Either String Config -> IO ()
ircLoop (Left err)   = putStrLn err
ircLoop (Right conf) = do
    -- Run the routing thread.
    forkIO route

    -- Connect to networks in the configuration file.
    networks <- flip mapM (servers conf) $ \server -> do
        -- Extract Configuration
        let address = serverAddress server
            port    = fromIntegral (serverPort server)
            pass    = serverPass server
            nick    = serverNick server
            chans   = serverChans server
            ssl     = serverSSL server

        -- Connect to the network.
        context <- initConnectionContext
        network <- connectTo context $ ConnectionParams {
            connectionHostname  = address,
            connectionPort      = port,
            connectionUseSecure = Just $ TLSSettingsSimple True False True,
            connectionUseSocks  = Nothing
            }

        -- Auth if Necessary
        case pass of
            Just p  -> connectionPut network $ ircPack ("PASS " ++ p)
            Nothing -> return ()

        -- Send Connection Information
        connectionPut network $ ircPack ("NICK " ++ nick)
        connectionPut network $ ircPack (printf "USER %s %s %s :%s" nick nick nick nick)
        mapM_ (connectionPut network . ircPack . ("JOIN "++)) chans

        -- Append to Network List
        return (address, network)

    -- Create a ZMQ Context. This happens now so we can share it among the
    -- various threads.
    runZMQ $ do
        -- This is why lightweight threads are awesome. Spawn a thread to
        -- handle each network.
        flip mapM_ networks $ \(name, network) ->
            async $ do
                req <- socket Req
                connect req "tcp://0.0.0.0:9891"
                forever $ do
                    let encoder = encodeRouter name "*"
                    message <- liftIO (connectionGetLine 128 network)
                    send req [] (encoder . C8.unpack $ message)
                    receive req

        -- And now we sit around waiting for published messages to route back
        -- out into the wilderness.
        sub <- socket Sub
        connect sub "tcp://0.0.0.0:9890"
        subscribe sub "WAR:FORWARD"

        forever $ do
            line <- receive sub
            let payload     = tail . dropWhile (')'/=) . C8.unpack $ line
                argsplits   = tail . dropWhile (','/=) . C8.unpack $ line
                destination = takeWhile (')'/=) $ argsplits
                target      = lookup destination networks

            case target of
                Just s  -> liftIO (connectionPut s . ircPack $ payload)
                Nothing -> return ()



{- Main thread. This handles the actual heavy lifting of dealing with IRC
 - networking and config parsing. -}
main :: IO ()
main = readFile "config" >>= ircLoop . eitherDecode . C8Lazy.pack
