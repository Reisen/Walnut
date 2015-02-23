> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE UnicodeSyntax #-}
> module Main where




Imports
--------------------------------------------------------------------------------

> import Network.Connection
> import System.IO
> import System.Timeout
> import System.ZMQ4.Monadic
> import Control.Exception
> import Control.Applicative
> import Control.Monad
> import Control.Concurrent
> import Control.Concurrent.MVar
> import Text.Printf
> import Text.Regex.PCRE
> import Data.Aeson
> import Data.List (intersperse)
> import Data.List.Split

> import qualified Data.ByteString.Char8 as C8
> import qualified Data.ByteString.Lazy.Char8 as C8Lazy

> import Config
> import qualified IRC




Routing Thread
--------------------------------------------------------------------------------
A routing thread exists to simply take messages and broadcast them through
Pub/Sub. This is essentially a small tag based message broker that plugins
can use to communicate.

> route ∷ IO ()
> route = runZMQ $ do

Two sockets are required, one as a sink that other plugins can push their
messages to, and the other the publisher for broadcasting.

>     pub  ← socket Pub
>     sink ← socket Pull
>     bind pub  "tcp://0.0.0.0:9890"
>     bind sink "tcp://0.0.0.0:9891"

With these two sockets, we simply setup a piping operation that reads and
forwards forever.

>     forever $ do
>         msg ← receive sink
>         send pub [] msg




Plugin Thread
--------------------------------------------------------------------------------
This thread is a plugin, it could be written in another language and in another
process, but it is the main goal of this project so it is written here and ran
as a seperate thread. The job of this plugin is to be a bridge between plugins
and IRC networks.

We need a parsing function for parsing routed messages so that we can parse IPC
calls to this plugin.

> data Message = Message {
>     msgTag  ∷ String,
>     msgFrom ∷ String,
>     msgTo   ∷ String,
>     msgArgs ∷ [String],
>     msgPay  ∷ String
>     }
>     deriving (Show)

> parse ∷ String → Message
> parse s = Message {
>     msgTag  = parts !! 0,
>     msgFrom = from,
>     msgTo   = to,
>     msgArgs = args,
>     msgPay  = payload
>     }
>     where
>         parts     = splitOn " " s
>         [from,to] = splitOn "!" (parts !! 1)
>         count     = read (parts !! 2)
>         args      = (take count . drop 3) parts
>         payload   = unwords . drop (count + 3) $ parts

Also need a function that does the reverse, and packs messages into routing
format for sending.

> pack ∷ Message → String
> pack s = (unwords . words) $ printf "%s %s!%s %d %s %s" a0 a1 a2 a3 a4 a5
>     where
>         a0 = msgTag s
>         a1 = msgFrom s
>         a2 = msgTo s
>         a3 = length (msgArgs s)
>         a4 = (concat . intersperse " ") (msgArgs s)
>         a5 = msgPay s



> bridgeO :: Either String Config → IO ()
> bridgeO (Left err)   = putStrLn ("Error Parsing: " ++ err)
> bridgeO (Right conf) = runZMQ $ do
>     forM_ (servers conf) $ \address -> async $ do
>         push ← socket Push
>         connect push "tcp://0.0.0.0:9891"
>         forever $ do
>             (_, network) <- liftIO (IRC.connect address)
>             forever $ do
>                 message ← liftIO (connectionGetLine 128 network)
>                 let irc = IRC.parse (C8.unpack message)
>                     pay = C8.unpack message
>                     msg = Message ("IRC:" ++ (irc !! 1)) (serverAddress address) "*" [] pay
>
>                 send push [] (C8.pack . pack $ msg)


> bridgeR :: Either String Config → IO ()
> bridgeR (Left err)   = putStrLn ("Error Parsing: " ++ err)
> bridgeR (Right conf) = runZMQ $ do
>     sync ← liftIO (newEmptyMVar)
>     forM_ (servers conf) $ \network → do
>         liftIO $ flip forkFinally (\_ -> pure ()) $ do
>             (name, network) ← IRC.connect network
>             forever $ do
>                 message ← connectionGetLine 128 network
>                 let irc = IRC.parse (C8.unpack message)
>                     pay = C8.unpack message
>                     cmd = irc !! 1
>                     msg = Message ("IRC:" ++ cmd) name "*" [] pay
>
>                 putMVar sync msg
>
>     async $ do
>         push ← socket Push
>         connect push "tcp://0.0.0.0:9891"
>         forever $ do
>             out ← liftIO (takeMVar sync)
>             send push [] (C8.pack . pack $ out)
>
>     sub ← socket Sub
>     connect sub "tcp://0.0.0.0:9890"
>     subscribe sub "IPC:CALL"
>
>     forever $ do
>         line ← receive sub
>         let parsed = parse (C8.unpack line)
>             args   = msgArgs parsed
>
>         case (args !! 0) of
>             method    → liftIO (putStrLn $ "No handler for IPC: " ++ method)

Now we can use these in the main plugin thread itself.

> bridge ∷ Either String Config → IO ()
> bridge (Left err)   = putStrLn ("Error Parsing: " ++ err)
> bridge (Right conf) = do

A set of networks is needed to start with. Connecting to these networks is done
in a seperate module.

>     networks ← mapM IRC.connect (servers conf)

The rest we do in a ZMQ context, this is so that we can share the context among
all the threads that are about to be spawned.

>     runZMQ $ do

Spawn a thread for each network, pushing messages out from each network into
the push queue for the route thread to broadcast.

>         flip mapM_ networks $ \(name, network) →
>             async $ do
>                 push ← socket Push
>                 connect push "tcp://0.0.0.0:9891"
>                 forever $ do
>                     message ← liftIO (connectionGetLine 128 network)
>                     let irc = IRC.parse (C8.unpack message)
>                         pay = C8.unpack message
>                         msg = Message ("IRC:" ++ (irc !! 1)) name "*" [] pay
>
>                     send push [] (C8.pack . pack $ msg)

We now also subscribe to the publisher, we are a plugin and we care about all
messages other plugins want us to forward back out. We do this by listening to
IPC calls that ask us to forward.

>         sub ← socket Sub
>         connect sub "tcp://0.0.0.0:9890"
>         subscribe sub "IPC:CALL"

Loop forever, parsing IPC calls and forwarding messages out into IRC networks
when they are received.

>         forever $ do
>             line ← receive sub
>             let parsed = parse (C8.unpack line)
>                 args   = msgArgs parsed
>
>             case (args !! 0) of
>                 "forward" →
>                     case lookup (msgTo parsed) networks of
>                         Just n  → liftIO (connectionPut n . IRC.format . msgPay $ parsed)
>                         Nothing → pure ()
>
>                 method    → liftIO (putStrLn $ "No handler for IPC: " ++ method)




Main Process
--------------------------------------------------------------------------------
The main function doesn't need to do much, so it forks the router, then reads a
configuration file into the main plugin thread from main.

> main ∷ IO ()
> main = forkIO route >> readFile "config" >>= bridge . eitherDecode . C8Lazy.pack
