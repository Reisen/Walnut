module Main where

import Data.IORef
import Data.Aeson (eitherDecode)
import System.ZMQ4
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BLC

import Walnut.Bot
import Walnut.Util
import Walnut.Config
import Walnut.Protocol
import Walnut.Connect hiding (connect)
import qualified Walnut.Connect (connect)


core :: Either String Config → IO ()
core (Left  e) = putStrLn ("Error Parsing: " ++ e)
core (Right c) =
    withContext          $ \ctxt →
    withSocket ctxt Pull $ \tap  →
    withSocket ctxt Pub  $ \pub  → forever $ do
    opened ← newIORef []
    _ ← forkIO $
        pool (servers c) $ \network →
        withSocket ctxt Push $ \sink →
        withSocket ctxt Sub  $ \pull → do
            putStrLn ("Network Thread Started: " ++ serverHost network)
            connect sink "tcp://0.0.0.0:9891"
            connect pull "tcp://0.0.0.0:9890"
            conn ← Walnut.Connect.connect network
            modifyIORef opened (replace (serverHost network) conn)
            handle (\(e :: SomeException) → print e >> pure network) $ forever $ do
                incoming ← recvIRC conn
                case encode . setSender (serverHost network) <$> convIRC incoming of
                    Just v  → send sink [] v
                    Nothing → pure ()

    bind tap "tcp://0.0.0.0:9891"
    bind pub "tcp://0.0.0.0:9890"
    forever $ do
        cns ← readIORef opened
        msg ← receive tap
        send pub [] msg
        maybe (pure ()) (uncurry sendIRC) $ do
            dsg ← decode msg
            con ← lookup (messageTo dsg) cns
            pure (con, messagePayload dsg)


main :: IO ()
main = readFile "config" >>= core . eitherDecode . BLC.pack
