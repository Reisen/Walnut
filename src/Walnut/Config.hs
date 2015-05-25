module Walnut.Config
    ( Server(..)
    , Config(..)
    ) where

import Data.Aeson
import Control.Monad


data Server = Server
    { serverHost :: String
    , serverPort :: Int
    , serverNick :: String
    , serverPass :: Maybe String
    , serverChan :: [String]
    , serverSSL  :: Maybe Bool }

data Config = Config
    { servers ∷ [Server] }



instance FromJSON Server where
    parseJSON (Object v) =
        Server              <$>
            v .:  "address" <*>
            v .:  "port"    <*>
            v .:  "nick"    <*>
            v .:? "pass"    <*>
            v .:  "chans"   <*>
            v .:? "ssl"

    parseJSON _ = mzero

instance FromJSON Config where
    parseJSON (Object v) =
        Config <$>
            v .: "servers"

    parseJSON _ = mzero
