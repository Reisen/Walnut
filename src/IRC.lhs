> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE UnicodeSyntax #-}

> module IRC (
>     connect,
>     format,
>     parse
>     )
> where




Imports
--------------------------------------------------------------------------------

> import Network.Connection
> import Text.Printf
> import Text.Regex.PCRE
> import Data.Maybe
> import Control.Applicative
> import Control.Monad

> import qualified Data.ByteString.Char8 as C8

> import Config




Module Exports
--------------------------------------------------------------------------------
All IRC messages need to a newline appended. Haskell would do this for us if we
used hPutStrLn, but using hs-connect means we have to do this. So a small helper
function is here. It also encodes so the data can be sent immediately.

> format ∷ String → C8.ByteString
> format = C8.pack . (++"\n")




Incoming IRC messages are parsed here so that we can extract the command from
them for use as a tag in broadcast messages.

> parse ∷ String → [String]
> parse = (tail . head) . (=~ pattern)
>     where
>         pattern = "^(:\\S+)?\\s*(\\S+)\\s+(.*)\\r?$" ∷ String




Connecting to IRC is done through information parsed from the configuration file
and so callers must pass the structure defined in Config.

> connect ∷ Server → IO (String, Connection)
> connect server = do

We could just extract details from the server object as needed, but for cleaner
code we'll just do it all here.

>     let address  = serverAddress server
>         port     = fromIntegral (serverPort server)
>         password = serverPass server
>         nick     = serverNick server
>         chans    = serverChans server
>         ssl      = serverSSL server

Opening a connection to the server is done with hs-connect, which handles all
SSL details without having to think about it here.

>     context ← initConnectionContext
>     network ← connectTo context $ ConnectionParams {
>         connectionHostname  = address,
>         connectionPort      = port,
>         connectionUseSecure = Just $ TLSSettingsSimple True False True,
>         connectionUseSocks  = Nothing
>         }

Authentication is optional, but must come first if it is present.

>     maybe (pure ()) (connectionPut network . format . ("PASS "++)) password

The first set of connection parameters are compulsary, servers will disconnect
clients that don't register. So we do it here, because there's no real reason
to ever _NOT_ do it. May as well join channels here as well.

>     connectionPut network . format $ ("NICK " ++ nick)
>     connectionPut network . format $ (printf "USER %s %s %s :%s" nick nick nick nick)
>     mapM_ (connectionPut network . format . ("JOIN "++)) chans
>     pure (address, network)
