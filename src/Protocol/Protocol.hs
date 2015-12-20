module Protocol.Protocol
    ( Transportable(..)
    ) where


--------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as B


--------------------------------------------------------------------------------
class Transportable a where
    embed :: a -> B.ByteString
    debed :: B.ByteString -> Maybe a
