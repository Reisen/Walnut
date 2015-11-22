module Plugin.IRC
    ( hook
    , ping
    ) where

--------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as C

--------------------------------------------------------------------------------
import           Plugin.Plugin            (Callback)



--------------------------------------------------------------------------------
hook :: Callback
hook = Just

ping :: Callback
ping v = Nothing
