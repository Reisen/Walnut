module Main where

import           Protolude
import qualified Data.Text                  as T
import qualified Data.Text.IO               as I

data IRCConfig
  = IRCConfig
      { servers :: [Int]
      , counter :: Int
      }

    deriving (Read)

main :: IO ()
main
  = do
      config <- T.unpack <$> I.readFile "irc.toml"
      case readMaybe config :: Maybe IRCConfig of
        Just c  -> irc c
        Nothing -> pure ()

irc :: IRCConfig -> IO ()
irc = undefined
