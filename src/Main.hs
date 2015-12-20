module Main where


--------------------------------------------------------------------------------
import           Nanomsg
import           Data.Maybe               (fromMaybe)
import           Data.MessagePack
import           Control.Monad            (forM_)
import           Control.Concurrent       (threadDelay)
import qualified Data.ByteString.Char8    as C
import           Control.Monad.State.Lazy (runStateT, liftIO, gets, get, put, modify, lift)

import           Plugin
import           Walnut
import           Protocol.Message
import           Protocol.Protocol


--------------------------------------------------------------------------------
main :: IO ((), StateW)
main =
    withSocket Pub $ \push ->
    withSocket Pull $ \pull -> do
        bind push "tcp://*:5005"
        bind pull "tcp://*:5006"
        runStateT loop StateW
            { statePlugins = map (uncurry Plugin) plugins
            , statePublish = push
            , stateListen  = pull
            }

        where
            loop :: Walnut ()
            loop = do
                listen   <- gets stateListen
                publish  <- gets statePublish
                plugins  <- gets statePlugins
                message  <- lift (recv listen)
                let reply = maybe [] (`runPlugins` plugins) (debed message)

                flip (>>) loop $ lift (
                    forM_ reply $ \r ->
                        send publish (embed r))
