module Main where

import Data.Aeson
import System.IO
import System.ZMQ4
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import Control.Concurrent
import Control.Applicative
import qualified Data.ByteString.Char8 as BC
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
    pool (servers c)     $ \network →
    withSocket ctxt Push $ \push →
    withSocket ctxt Sub  $ \pull → do
        connect push "tcp://0.0.0.0:9891"
        connect pull "tcp://0.0.0.0:9890"
        conn ← Walnut.Connect.connect network
        handle
            (\(e :: SomeException) → pure network) $
            forever $ do
                message  ← recvIRC conn
                putStrLn (show message)


main :: IO ()
main = readFile "config" >>= core . eitherDecode . BLC.pack
