module Walnut.Bot
    ( Worker(..)
    , worker
    , pool
    ) where

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Applicative

import Walnut.Config


data Worker a = Worker
    { workerID :: ThreadId
    , workerMV :: MVar ()
    , workerIN :: a }


worker :: a → (a → IO ()) → IO (Worker a)
worker input handler = do
    workmv ← newEmptyMVar
    thread ← forkIO $ do
        putMVar workmv =<< catch
            (handler input)
            (\e → (putStrLn . show) (e :: SomeException))

    pure Worker
        { workerID = thread
        , workerMV = workmv
        , workerIN = input }


pool :: forall a. [a] → (a → IO ()) → IO ()
pool inputs handler = forM inputs (flip worker handler) >>= monitor
    where monitor :: [Worker a] → IO ()
          monitor workers = do
              threadDelay 64000000
              workers ← forM workers $ \w → do
                  output ← tryTakeMVar (workerMV w)
                  case output of
                      Just _  → worker (workerIN w) handler
                      Nothing → pure w

              monitor workers
