module Main where

import           Protolude
import           Data.List               (unwords)
import           Data.String             (String)
import qualified Control.Concurrent.MVar as M
import qualified Data.ByteString.Char8   as BC
import qualified Data.Text               as T
import qualified Data.Text.IO            as I
import qualified Network.Connection      as C
import qualified Network.IRC             as NI
import qualified Walnut                  as W
import qualified Walnut.Binary           as B


data Connection
  = Connection
      { connContext :: C.ConnectionContext
      , connNetwork :: C.Connection
      , connConfig  :: Server
      }


data Server
  = Server
      { serverAddress :: !String
      , serverPort    :: !Int
      , serverNick    :: !String
      , serverSSL     :: !Bool
      , serverPass    :: !(Maybe String)
      , serverChan    :: ![String]
      }

    deriving (Read)


main :: IO ()
main
  = do
      config <- T.unpack <$> I.readFile "irc.config"
      case readMaybe config :: Maybe [Server] of
        Just c  -> irc c
        Nothing -> pure ()


irc :: [Server] -> IO ()
irc servers
  = do
      -- | Create ZMQ MVars, one to send and one to receive
      (mvarR, mvarS) <- B.zmqSockets

      -- | Connect to IRC Networks. Each network will produce a Connection
      --   object that contains all the context required to access the
      --   underlying connection.
      networks <- forM servers $ \s -> do
        -- Perform Connection to server, unsafe SSL.
        let tls  = C.TLSSettingsSimple True False True
        context <- C.initConnectionContext
        network <- C.connectTo context C.ConnectionParams
          { connectionHostname  = serverAddress s
          , connectionPort      = fromIntegral (serverPort s)
          , connectionUseSecure = if serverSSL s then Just tls else Nothing
          , connectionUseSocks  = Nothing
          }

        -- Password if presented.
        maybe
          (pure ())
          (C.connectionPut network . BC.pack .
            (\pass -> "PASS " <> pass <> "\n"))

          (serverPass s)

        -- Set our client nick.
        C.connectionPut network $ BC.pack
          (unwords
            [ "NICK "
            , serverNick s
            , "\n"
            ])

        -- Set our client user.
        C.connectionPut network $ BC.pack
          (unwords
            [ "USER "
            , serverNick s , " "
            , serverNick s , " "
            , serverNick s , " :"
            , serverNick s
            , "\n"
            ])

        -- Produce the Connection.
        pure Connection
          { connContext = context
          , connNetwork = network
          , connConfig  = s
          }

      forM_ networks $ \network -> forkIO $ forever $ do
        mayLine <- NI.decode <$> C.connectionGetLine 512 (connNetwork network)
        flip (maybe (pure ())) mayLine $ \line ->
          case NI.msg_command line of
            "PING"    -> do
              C.connectionPut (connNetwork network) $ BC.pack
                (unwords
                  [ "PONG "
                  , (BC.unpack . head) (NI.msg_params line)
                  , "\n"
                  ])

            -- "001"     -> handleJoin network line
            -- "PRIVMSG" | isJust (NI.msg_prefix line) -> pure ()
            otherwise ->
              M.putMVar mvarS W.Message
                { messageNetwork = (T.pack . serverAddress . connConfig) network
                , messageChannel = ""
                , messageUser    = ""
                , messageContent = ""
                }

          --
      forever $ do
        message <- M.takeMVar mvarR
        print message
