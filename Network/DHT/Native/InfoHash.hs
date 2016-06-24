module Network.DHT.Native.InfoHash
    ( InfoHash()
    , toInfoHash
    , fromInfoHash
    ) where

import           Control.DeepSeq (NFData(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Maybe (mapMaybe)
import           Data.Serialize
import           Foreign.Marshal.Utils (copyBytes)
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable(..))

newtype InfoHash = InfoHash { fromInfoHash :: BS.ByteString }
  deriving (Eq, Ord)

instance Show InfoHash where
    showsPrec p (InfoHash bs) = showsPrec p (B16.encode bs)

instance Read InfoHash where
    readsPrec p = mapMaybe g . readsPrec p
      where
        g (hex, rest) | BS.null bad = Just (InfoHash bs, rest)
                      | otherwise = Nothing
          where
            (bs, bad) = B16.decode hex

instance Bounded InfoHash where
    minBound = InfoHash $ BS.replicate infoHashLength 0
    maxBound = InfoHash $ BS.replicate infoHashLength 0xff

instance NFData InfoHash where
    rnf (InfoHash bs) = rnf bs

instance Serialize InfoHash where
    get = InfoHash <$> get
    put (InfoHash bs) = put bs

-- | Stores the info hash as a 20-byte sequence.
instance Storable InfoHash where
    sizeOf _ = infoHashLength
    alignment _ = 1
    peek ptr =
        InfoHash <$> BS.packCStringLen (castPtr ptr, infoHashLength)
    poke ptr (InfoHash bs) =
        BS.useAsCStringLen bs $ \(src, len) ->
            copyBytes (castPtr ptr) src (max len infoHashLength)

infoHashLength :: Int
infoHashLength = 20

-- | Converts a 'ByteString' to an 'InfoHash'. It assumes that it has the
-- correct length of 20 bytes. Should the length be accidentally more, it's
-- truncated.
toInfoHash :: BS.ByteString -> InfoHash
toInfoHash = InfoHash . BS.take infoHashLength
