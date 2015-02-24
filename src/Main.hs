module Main where

import Data.Aeson
import System.ZMQ4
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Applicative
--import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC

import Walnut.Bot
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
    _ ← forkIO $
        pool (servers c) $ \network →
        withSocket ctxt Push $ \sink →
        withSocket ctxt Sub  $ \pull → do
            connect sink "tcp://0.0.0.0:9891"
            connect pull "tcp://0.0.0.0:9890"
            conn ← Walnut.Connect.connect network
            handle (\(e :: SomeException) → print e >> pure network) $ forever $ do
                incoming ← recvIRC conn
                putStrLn (show incoming)
                case Walnut.Protocol.encode <$> convIRC incoming of
                    Just v  → send sink [] v
                    Nothing → pure ()

    bind tap "tcp://0.0.0.0:9891"
    bind pub "tcp://0.0.0.0:9890"
    forever (receive tap >>= send pub [])


main :: IO ()
main = readFile "config" >>= core . eitherDecode . BLC.pack
