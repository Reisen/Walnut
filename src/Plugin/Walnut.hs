module Plugin.Walnut
    ( forward
    , status
    ) where

--------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as C

--------------------------------------------------------------------------------
import           Plugin.Plugin



--------------------------------------------------------------------------------
forward :: Callback
forward = Just

status :: Callback
status m@Message{..}
    | messageTag == "STATUS" = Just m
    | otherwise              = Nothing
