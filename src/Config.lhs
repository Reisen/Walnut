> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE UnicodeSyntax #-}

> module Config (
>     Server(..),
>     Config(..)
>     )
> where




Imports
--------------------------------------------------------------------------------

> import Data.Aeson
> import Control.Monad
> import Control.Applicative




The configuration file needs to contain valid JSON information for the plugin to
handle, Aeson is used here to define Haskell data types that encode all the type
information. Aeson will deal with parsing, and also reporting errors if the type
doesn't match the config.

> data Server = Server {
>     serverAddress ∷ String,
>     serverPort    ∷ Int,
>     serverNick    ∷ String,
>     serverPass    ∷ Maybe String,
>     serverChans   ∷ [String],
>     serverSSL     ∷ Maybe Bool
>     } deriving (Show)
>
> data Config = Config {
>     servers ∷ [Server]
>     } deriving (Show)




These instances are used for doing the actual parsing, .: fields are required
and .:? fields are optional. Parsing will fail for required fields that aren't
in the config.

> instance FromJSON Server where
>     parseJSON (Object v) =
>         Server              <$>
>             v .:  "address" <*>
>             v .:  "port"    <*>
>             v .:  "nick"    <*>
>             v .:? "pass"    <*>
>             v .:  "chans"   <*>
>             v .:? "ssl"
>
>     parseJSON _ = mzero
>
> instance FromJSON Config where
>     parseJSON (Object v) =
>         Config <$> v .: "servers"
>
>     parseJSON _ = mzero
