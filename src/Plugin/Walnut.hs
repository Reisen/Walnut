module Plugin.Walnut
    ( forward
    , status
    ) where


--------------------------------------------------------------------------------
import           Plugin.Plugin


--------------------------------------------------------------------------------
forward :: Callback
forward = Just


status :: Callback
status m@Message{..}
    | messageTag == "status" = Just m
    | otherwise              = Nothing
