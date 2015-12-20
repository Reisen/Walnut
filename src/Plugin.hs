module Plugin
    ( module Plugin.Plugin
    , plugins
    ) where


--------------------------------------------------------------------------------
import           Plugin.Plugin
import qualified Plugin.Walnut as Walnut
import qualified Plugin.Router as Router


--------------------------------------------------------------------------------
plugins =
    [ ("walnut.forward", Walnut.forward)
    , ("walnut.status",  Walnut.status)
    , ("walnut.router",  Router.router)
    , ("walnut.chain",   Router.chain)
    ]
