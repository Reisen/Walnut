{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Config (
    Server(..),
    Config(..)
    )
where

import Data.Aeson
import Control.Monad
import Control.Applicative

{- Decodes from a JSON server value. Automatically handled by Aeson. -}
data Server  = Server {
    serverAddress :: String,
    serverPort    :: Int,
    serverPass    :: Maybe String,
    serverChans   :: [String]
    } deriving (Show)


{- Decodes from a JSON config value. Automatically handled by Aeson. -}
data Config  = Config {
    servers :: [Server]
    } deriving (Show)


{- Function for decoding Server blocks in config files. -}
instance FromJSON Server where
    parseJSON (Object v) =
        Server              <$>
            v .:  "address" <*>
            v .:  "port"    <*>
            v .:? "pass"    <*>
            v .:  "chans"

    parseJSON _ = mzero


{- Function for decoding config files. -}
instance FromJSON Config where
    parseJSON (Object v) =
        Config <$> v .: "servers"

    parseJSON _ = mzero

