module Main where

import Data.Aeson
import System.IO
import System.ZMQ4
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Applicative
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC

import Walnut.Bot
import Walnut.Config
import Walnut.Connect


core :: Either String Config → IO ()
core (Left  e) = putStrLn ("Error Parsing: " ++ e)
core (Right c) =
    withContext      $ \ctxt →
    pool (servers c) $ \network → do
        conn ← Walnut.Connect.connect network
        forever $
            withSocket ctxt Push $ \push →
            withSocket ctxt Sub  $ \pull → do
            message ← recvIRC conn
            putStrLn (show message)


main :: IO ()
main = readFile "config" >>= core . eitherDecode . BLC.pack
