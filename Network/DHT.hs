{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forM_)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Network.Socket.Internal

-- typedef void
-- dht_callback(void *closure, int event,
--              const unsigned char *info_hash,
--              const void *data, size_t data_len);
type Callback a =
     Ptr a -- ^ closure
  -> CInt -- ^ event
  -> CString -- ^ info_hash
  -> CString -- ^ data
  -> CSize -- ^ data_len
  -> IO ()

foreign import ccall "wrapper"
  mkCallback :: Callback a -> IO (FunPtr (Callback a))

-- int dht_init(int s, int s6, const unsigned char *id, const unsigned char *v);
foreign import ccall unsafe "dht/dht.h dht_init"
  dht_init :: CInt -- ^ socket handle
           -> CInt -- ^ ipv6 socket handle
           -> CString -- ^ node id, 20 octet array
           -> CString -- ^ v
           -> IO Int

-- int dht_insert_node(const unsigned char *id, struct sockaddr *sa, int salen);
foreign import ccall unsafe "dht/dht.h dht_insert_node" dht_insert_node
  :: CString -- ^ id
  -> Ptr SockAddr -- ^ sockaddr
  -> CInt -- ^ salen
  -> IO Int

-- int dht_ping_node(struct sockaddr *sa, int salen);
foreign import ccall unsafe "dht/dht.h dht_ping_node" dht_ping_node
  :: Ptr SockAddr
  -> CInt
  -> IO Int

-- int dht_periodic(const void *buf, size_t buflen,
--                  const struct sockaddr *from, int fromlen,
--                  time_t *tosleep, dht_callback *callback, void *closure);
foreign import ccall unsafe "dht/dht.h dht_periodic" dht_periodic
  :: CString -- ^ buf
  -> CSize -- ^ buflen
  -> Ptr SockAddr -- ^ from
  -> CInt -- ^ fromlen
  -> Ptr CTime -- ^ tosleep
  -> FunPtr (Callback a) -- ^ callback
  -> Ptr a -- ^ closure
  -> IO Int

-- int dht_search(const unsigned char *id, int port, int af,
--                dht_callback *callback, void *closure);
foreign import ccall unsafe "dht/dht.h dht_search" dht_search
  :: CString -- ^ id
  -> CInt -- ^ port
  -> CInt -- ^ af
  -> FunPtr (Callback a) -- ^ callback
  -> Ptr a -- ^ closure
  -> IO Int

-- int dht_nodes(int af,
--               int *good_return, int *dubious_return, int *cached_return,
--               int *incoming_return);
-- void dht_dump_tables(FILE *f);
-- int dht_get_nodes(struct sockaddr_in *sin, int *num,
--                   struct sockaddr_in6 *sin6, int *num6);
-- int dht_uninit(void);
foreign import ccall unsafe "dht/dht.h dht_uninit"
  dht_uninit :: IO Int


main :: IO ()
main = do
  nodeId <- newCString "01234567890123456789"
  dht_init 0 0 nodeId nullPtr
  flip finally dht_uninit $
    alloca $ \tosleep'p -> do
      forM_ [0..9] $ \_ -> do
        poke tosleep'p 0
        dht_periodic nullPtr 0 nullPtr 0 tosleep'p nullFunPtr nullPtr
        tosleep <- peek tosleep'p
        putStrLn $ "Should sleep: " ++ show tosleep
        -- TODO: Portable!
        let (CTime tosleep'i) = tosleep
        threadDelay (fromIntegral tosleep'i * 1000000)
