module Main where


--------------------------------------------------------------------------------
import           Nanomsg
import           Data.Maybe               (maybe)
import           Text.Printf              (printf)
import           Control.Monad
import           Control.Concurrent
import           Data.MessagePack
import qualified Network.IRC           as IRC
import           Network.Connection
import qualified Data.ByteString.Char8 as B

import qualified Protocol.Message


--------------------------------------------------------------------------------
data Server = Server
    { server :: String
    , port   :: Int
    , nick   :: String
    , ssl    :: Bool
    , pass   :: Maybe String
    , chan   :: [String]
    } deriving (Read)


data Conn = Conn
    { context :: ConnectionContext
    , network :: Connection
    , config  :: Server
    }


main :: IO ()
main = do
    file <- readFile "config"
    loop (read file :: [Server])


loop :: [Server] -> IO ()
loop configs =
    withSocket Sub $ \pull ->
    withSocket Push $ \push -> do
        connect push "tcp://127.0.0.1:5006"
        connect pull "tcp://127.0.0.1:5005"

        -- Initialize Connections
        networks <- forM configs $ \s -> do
            let tls  = TLSSettingsSimple True False True
            context <- initConnectionContext
            network <- connectTo context ConnectionParams
                { connectionHostname  = server s
                , connectionPort      = fromIntegral (port s)
                , connectionUseSecure = if ssl s then Just tls else Nothing
                , connectionUseSocks  = Nothing
                }

            maybe
                (pure ())
                (connectionPut network . B.pack . printf "PASS %s\n")
                (pass s)

            connectionPut network . B.pack $ printf "NICK %s\n" (nick s)
            connectionPut network . B.pack $ printf "USER %s %s %s :%s\n" (nick s) (nick s) (nick s) (nick s)
            pure Conn
                { context = context
                , network = network
                , config  = s
                }

        -- Handle Connections
        forM_ networks $ \n -> forkIO $ forever $ do
            line <- IRC.decode <$> connectionGetLine 512 (network n)
            flip (maybe (pure ())) line $ \line ->
                let message = IRC.msg_command line in
                case message of
                    "PING"    -> handlePing n line
                    "001"     -> handleJoin n line
                    otherwise -> pure ()

        -- Handle Nanomsg messaging.
        forever $ do
            threadDelay 1000000


--------------------------------------------------------------------------------
handlePing :: Conn -> IRC.Message -> IO ()
handlePing c m = do
    putStrLn "Responding to PING."
    connectionPut conn . B.pack $ printf "PING %s\n" (B.unpack . head $ args)
    where
        conn = network c
        args = IRC.msg_params m


handleJoin :: Conn -> IRC.Message -> IO ()
handleJoin c m = do
    putStrLn "Joining Channels."
    forM_ channels (connectionPut conn . B.pack . printf "JOIN %s\n")
    where
        conn     = network c
        channels = (chan . config) c
