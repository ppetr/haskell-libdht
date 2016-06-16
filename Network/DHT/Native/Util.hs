module Network.DHT.Native.Util where

import           Control.Monad.Codensity
import           Foreign.C.Types (CInt)
import qualified Foreign.Marshal.Alloc as P
import qualified Foreign.Ptr as P
import qualified Foreign.Storable as P

afIPv4, afIPv6 :: CInt
afIPv4 = 2
afIPv6 = 10

alloca :: (P.Storable a) => Codensity IO (P.Ptr a)
alloca = Codensity P.alloca
