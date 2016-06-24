module Network.DHT.Native.FFI where

import           Control.Monad (void)
import           Data.Word (Word32)
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           Network.Socket hiding (send, sendTo, recv, recvFrom)

import           Network.DHT.Native.InfoHash (InfoHash())

-- * The callback function

-- typedef void
-- dht_callback(void *closure, int event,
--              const unsigned char *info_hash,
--              const void *data, size_t data_len);
type Callback a =
     Ptr a -- ^ closure
  -> CInt -- ^ event
  -> Ptr InfoHash -- ^ info_hash
  -> CString -- ^ data
  -> CSize -- ^ data_len
  -> IO ()

foreign import ccall "wrapper"
  mkCallback :: Callback a -> IO (FunPtr (Callback a))

-- * Foreign imports from the native library

-- int dht_init(int s, int s6, const unsigned char *id, const unsigned char *v);
foreign import ccall safe "dht/dht.h dht_init" dht_init_ffi
    :: CInt -- ^ socket handle
    -> CInt -- ^ ipv6 socket handle
    -> CString -- ^ node id, 20 octet array
    -> CString -- ^ v
    -> IO Int

dht_init
    :: CInt -- ^ socket handle
    -> CInt -- ^ ipv6 socket handle
    -> CString -- ^ node id, 20 octet array
    -> CString -- ^ v
    -> IO ()
dht_init socket socket6 nodeId v =
    void . throwErrnoIfMinus1 "dht_init"
        $ dht_init_ffi socket socket6 nodeId v


-- int dht_insert_node(const unsigned char *id, struct sockaddr *sa, int salen);
foreign import ccall safe "dht/dht.h dht_insert_node" dht_insert_node_ffi
    :: CString -- ^ id
    -> Ptr SockAddr -- ^ sockaddr
    -> CInt -- ^ salen
    -> IO Int

dht_insert_node
    :: CString -- ^ id
    -> Ptr SockAddr -- ^ sockaddr
    -> CInt -- ^ salen
    -> IO ()
dht_insert_node id sockaddr salen =
    void . throwErrnoIfMinus1 "dht_insert_node"
        $ dht_insert_node_ffi id sockaddr salen


-- int dht_ping_node(struct sockaddr *sa, int salen);
foreign import ccall safe "dht/dht.h dht_ping_node" dht_ping_node_ffi
    :: Ptr SockAddr
    -> CInt
    -> IO Int

dht_ping_node
  :: Ptr SockAddr
  -> CInt
  -> IO ()
dht_ping_node sa salen =
    void . throwErrnoIfMinus1 "dht_ping_node"
        $ dht_ping_node_ffi sa salen


-- int dht_periodic(const void *buf, size_t buflen,
--                  const struct sockaddr *from, int fromlen,
--                  time_t *tosleep, dht_callback *callback, void *closure);
foreign import ccall safe "dht/dht.h dht_periodic" dht_periodic_ffi
    :: CString -- ^ buf
    -> CSize -- ^ buflen
    -> Ptr SockAddr -- ^ from
    -> CInt -- ^ fromlen
    -> Ptr CTime -- ^ tosleep
    -> FunPtr (Callback a) -- ^ callback
    -> Ptr a -- ^ closure
    -> IO Int

dht_periodic
    :: CString -- ^ buf
    -> CSize -- ^ buflen
    -> Ptr SockAddr -- ^ from
    -> CInt -- ^ fromlen
    -> Ptr CTime -- ^ tosleep
    -> FunPtr (Callback a) -- ^ callback
    -> Ptr a -- ^ closure
    -> IO ()
dht_periodic buf buflen from fromlen tosleep callback closure =
    void . throwErrnoIfMinus1 "dht_periodic"
        $ dht_periodic_ffi buf buflen from fromlen tosleep callback closure


-- int dht_search(const unsigned char *id, int port, int af,
--                dht_callback *callback, void *closure);
foreign import ccall safe "dht/dht.h dht_search" dht_search_ffi
    :: CString -- ^ id
    -> CInt -- ^ port
    -> CInt -- ^ af
    -> FunPtr (Callback a) -- ^ callback
    -> Ptr a -- ^ closure
    -> IO Int

dht_search
    :: CString -- ^ id
    -> CInt -- ^ port
    -> CInt -- ^ af
    -> FunPtr (Callback a) -- ^ callback
    -> Ptr a -- ^ closure
    -> IO ()
dht_search id port af callback closure =
    void . throwErrnoIfMinus1 "dht_search"
        $ dht_search_ffi id port af callback closure


-- int dht_nodes(int af,
--               int *good_return, int *dubious_return, int *cached_return,
--               int *incoming_return);
foreign import ccall safe "dht/dht.h dht_nodes" dht_nodes_ffi
    :: CInt -- ^ af
    -> Ptr CInt -- good
    -> Ptr CInt -- dubious
    -> Ptr CInt -- cached
    -> Ptr CInt -- incoming
    -> IO Int

dht_nodes
    :: CInt -- ^ af
    -> Ptr CInt -- good
    -> Ptr CInt -- dubious
    -> Ptr CInt -- cached
    -> Ptr CInt -- incoming
    -> IO ()
dht_nodes af good dubious cached incoming =
    void . throwErrnoIfMinus1 "dht_nodes"
        $ dht_nodes_ffi af good dubious cached incoming


-- void dht_dump_tables(FILE *f);

-- int dht_get_nodes(struct sockaddr_in *sin, int *num,
--                   struct sockaddr_in6 *sin6, int *num6);
foreign import ccall safe "dht/dht.h dht_get_nodes" dht_get_nodes_ffi
    :: Ptr SockAddr -- ^ sockaddr_in
    -> Ptr CInt -- ^ num
    -> Ptr SockAddr -- ^ sockaddr_in6
    -> Ptr CInt -- ^ num6
    -> IO Int

dht_get_nodes
    :: Ptr SockAddr -- ^ sockaddr_in
    -> Ptr CInt -- ^ num
    -> Ptr SockAddr -- ^ sockaddr_in6
    -> Ptr CInt -- ^ num6
    -> IO ()
dht_get_nodes sa num sa6 num6 =
    void . throwErrnoIfMinus1 "dht_get_nodes"
        $ dht_get_nodes_ffi sa num sa6 num6

-- int dht_uninit(void);
foreign import ccall safe "dht/dht.h dht_uninit" dht_uninit_ffi
    :: IO Int

dht_uninit :: IO ()
dht_uninit = void . throwErrnoIfMinus1 "dht_uninit" $ dht_uninit_ffi

-- * Debugging, custom code

foreign import ccall safe "dht_impl/dht_impl.h dht_debug_to_stderr"
  dht_debug_to_stderr :: IO ()

-- * Auxiliary utility functions

foreign import ccall unsafe "htonl"
    htonl :: Word32 -> Word32
