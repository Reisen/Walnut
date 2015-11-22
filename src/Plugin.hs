module Plugin
    ( module IRC
    , module Walnut
    , module Plugin.Plugin
    , plugins
    ) where

--------------------------------------------------------------------------------
import           Plugin.Plugin
import qualified Plugin.IRC    as IRC
import qualified Plugin.Walnut as Walnut



--------------------------------------------------------------------------------
plugins =
    [ ("WAL.FORWARD", Walnut.forward)
    , ("WAL.STATUS",  Walnut.status)
    ]
