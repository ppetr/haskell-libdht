module Network.DHT.Native.Callback where

import           Control.Applicative (many)
import qualified Data.ByteString as BS
import           Data.Serialize.Get
import           Network.Socket (SockAddr(..))

import qualified Network.DHT.Native.FFI as F

-- TODO: Move somewhere
type InfoHash = BS.ByteString

data SearchResult
    = Done4
    | FoundNodes4 [SockAddr]
    | Done6
    | FoundNodes6 [SockAddr]
  deriving (Eq, Ord, Show)

makeCallback :: (InfoHash -> SearchResult -> IO ()) -> F.Callback ()
makeCallback f _ event hash val vallen = do
    ih <- BS.packCStringLen (hash, 20)
    -- TODO: Event number constants
    case event of
        0 -> return ()
        1 -> parse parseCompactIPv4 >>= (f ih . FoundNodes4)
        2 -> parse parseCompactIPv6 >>= (f ih . FoundNodes6)
        3 -> f ih Done4
        4 -> f ih Done6
        _ -> return () -- log unknown event
  where
    -- | Reads the given structure multiple times. 'fail's the computation
    -- if a parsing error occurs.
    parse :: Get a -> IO [a]
    parse f | vallen > 0 =
                (either fail return <$> runGet (many f))
                =<< BS.packCStringLen (val, fromIntegral vallen)
            | otherwise = return []

-- | Parses a 6-byte compact IPv4 address.
parseCompactIPv4 :: Get SockAddr
parseCompactIPv4 = do
    -- HostAddress is somewhat misleadingly documented as "host byte order".
    -- More correctly it seems that for little-endian architectures the byte
    -- order in HostAddresss is reversed. So here we employ a little trick:
    -- The address in the input stream is in network (big-endian) byte order,
    -- but we read it as if it were in host byte order. This flips the order on
    -- LE architectures, getting what SockAddrInet expects.
    addr <- getWord32host
    port <- getInt16be
    return $ SockAddrInet (fromIntegral port) addr

-- | Parses a 18-byte compact IPv6 address.
parseCompactIPv6 :: Get SockAddr
parseCompactIPv6 = do
    addr <- (,,,) <$> getWord32be <*> getWord32be <*> getWord32be <*> getWord32be
    port <- getInt16be
    return $ SockAddrInet6 (fromIntegral port) 0 addr 0
