module Main where

--------------------------------------------------------------------------------
import           Nanomsg
import           Data.Maybe               (fromMaybe)
import           Data.MessagePack
import           Control.Monad            (forM_)
import           Control.Concurrent       (threadDelay)
import qualified Data.ByteString.Char8    as C
import           Control.Monad.State.Lazy (runStateT, liftIO, gets, get, put, modify, lift)

--------------------------------------------------------------------------------
import           Plugin
import           Walnut



--------------------------------------------------------------------------------
main :: IO ((), StateW)
main =
    withSocket Pub $ \s ->
    withSocket Pull $ \p -> do
        bind s "tcp://*:5005"
        bind p "tcp://*:5006"
        runStateT loop StateW
            { statePlugins = map (uncurry Plugin) plugins
            , statePublish = s
            , stateListen  = p
            }

        where
            loop :: Walnut ()
            loop = do
                listen   <- gets stateListen
                publish  <- gets statePublish
                plugins  <- gets statePlugins
                message  <- lift (recv listen)
                let reply = maybe [] (`runPlugins` plugins) (unpackMessage message)

                flip (>>) loop $ lift (
                    forM_ reply $ \r ->
                        send publish (packMessage r))
