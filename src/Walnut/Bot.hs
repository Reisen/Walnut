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
    , workerMV :: MVar (Maybe a) }


worker :: a → (a → IO b) → IO (Worker b)
worker input handler = do
    workmv ← newEmptyMVar
    thread ← forkIO $
        putMVar workmv =<< catch
            (handler input >>= pure . Just)
            (\(e :: SomeException) → pure Nothing)

    pure Worker
        { workerID = thread
        , workerMV = workmv }


pool :: forall a. [a] → (a → IO a) → IO ()
pool inputs handler = forM inputs (`worker` handler) >>= monitor
    where monitor :: [Worker a] → IO ()
          monitor workers = do
              threadDelay 1000000
              (=<<) monitor $ forM workers $ \w → do
                  output ← tryTakeMVar (workerMV w)
                  case output of
                      Just (Just v) → worker v handler
                      otherwise     → pure w
