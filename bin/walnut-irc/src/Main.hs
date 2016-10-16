module Main where

import           Protolude
import qualified Control.Concurrent.MVar as M
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as E
import qualified Data.Text.IO            as I
import qualified Network.IRC.Client      as IC
import qualified Walnut.Binary           as B


data Server
  = Server
      { serverAddress :: !Text
      , serverPort    :: !Int
      , serverNick    :: !Text
      , serverSSL     :: !Bool
      , serverPass    :: !(Maybe Text)
      , serverChan    :: ![Text]
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
      (mvarR, _) <- B.zmqSockets

      -- | Connect to IRC Networks. Each network will produce a Connection
      --   object that contains all the context required to access the
      --   underlying connection.
      _ <- forM_ servers $ \Server{..} -> do
        let connect = bool IC.connect IC.connectWithTLS serverSSL
        let serverConfig = IC.InstanceConfig
              { _nick          = serverNick
              , _username      = serverNick
              , _realname      = serverNick
              , _password      = serverPass
              , _channels      = serverChan
              , _ctcpVer       = "Walnut"
              , _eventHandlers = IC.defaultEventHandlers
              , _ignore        = []
              }

        server <- connect (E.encodeUtf8 serverAddress) serverPort 1
        forkIO $ IC.start server serverConfig

      forever $ do
        message <- M.takeMVar mvarR
        print message
