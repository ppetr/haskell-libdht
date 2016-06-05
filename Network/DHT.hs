{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base16 as B16
import Data.Word (byteSwap32)
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
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
foreign import ccall safe "dht/dht.h dht_init"
  dht_init :: CInt -- ^ socket handle
           -> CInt -- ^ ipv6 socket handle
           -> CString -- ^ node id, 20 octet array
           -> CString -- ^ v
           -> IO Int

-- int dht_insert_node(const unsigned char *id, struct sockaddr *sa, int salen);
foreign import ccall safe "dht/dht.h dht_insert_node" dht_insert_node
  :: CString -- ^ id
  -> Ptr SockAddr -- ^ sockaddr
  -> CInt -- ^ salen
  -> IO Int

-- int dht_ping_node(struct sockaddr *sa, int salen);
foreign import ccall safe "dht/dht.h dht_ping_node" dht_ping_node
  :: Ptr SockAddr
  -> CInt
  -> IO Int

-- int dht_periodic(const void *buf, size_t buflen,
--                  const struct sockaddr *from, int fromlen,
--                  time_t *tosleep, dht_callback *callback, void *closure);
foreign import ccall safe "dht/dht.h dht_periodic" dht_periodic
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
foreign import ccall safe "dht/dht.h dht_search" dht_search
  :: CString -- ^ id
  -> CInt -- ^ port
  -> CInt -- ^ af
  -> FunPtr (Callback a) -- ^ callback
  -> Ptr a -- ^ closure
  -> IO Int

-- int dht_nodes(int af,
--               int *good_return, int *dubious_return, int *cached_return,
--               int *incoming_return);
foreign import ccall safe "dht/dht.h dht_nodes" dht_nodes
  :: CInt -- ^ af
  -> Ptr CInt -- good
  -> Ptr CInt -- dubious
  -> Ptr CInt -- cached
  -> Ptr CInt -- incoming
  -> IO Int

-- void dht_dump_tables(FILE *f);
-- int dht_get_nodes(struct sockaddr_in *sin, int *num,
--                   struct sockaddr_in6 *sin6, int *num6);
-- int dht_uninit(void);
foreign import ccall safe "dht/dht.h dht_uninit"
  dht_uninit :: IO Int

-- * Debugging, custom code

foreign import ccall safe "dht_impl/dht_impl.h dht_debug_to_stderr"
  dht_debug_to_stderr :: IO ()

-- *

bootstrapNodes :: IO [SockAddr]
bootstrapNodes = liftM concat . forM nodes $ \host -> do
    liftM (map addrAddress) $ lookup host
  where
    lookup host = do
      let hints = defaultHints { addrFlags = [AI_ALL] }
      getAddrInfo (Just hints) (Just host) (Just "6881")
    nodes =
      [ "router.utorrent.com"
      , "router.bittorrent.com"
      , "dht.transmissionbt.com"
      , "dht.aelitis.com"
      ]
{-
  -- [ SockAddrInet 6881 (87 * 0x1000000 + 98 * 0x10000 + 162 * 0x100 + 88)
  [ SockAddrInet 6881 (87 + 98 * 0x100 + 162 * 0x10000 + 88 * 0x1000000)
  , SockAddrInet 6881 (82 + 221 * 0x100 + 103 * 0x10000 + 244 * 0x1000000)
  ]
-}
  
{-
  [ SockAddrInet 6881 "router.utorrent.com"
  , SockAddrInet 6881 "router.bittorrent.com"
  , SockAddrInet 6881 "dht.transmissionbt.com"
  , SockAddrInet 6881 "router.bitcomet.com"
  , SockAddrInet 6881 "dht.aelitis.com"
  ]
-}

unwrapIpv6 :: SockAddr -> SockAddr
unwrapIpv6 (SockAddrInet6 port _ (0, 0, 0xffff, ipv4addr) _) =
  SockAddrInet port (byteSwap32 ipv4addr)
unwrapIpv6 sa = sa

main :: IO ()
main = do
  -- set debugging
  dht_debug_to_stderr

  nodeId <- newCString "01234567890123456789"
  s <- socket AF_INET6 Datagram defaultProtocol
  bind s $ SockAddrInet6 aNY_PORT 0 iN6ADDR_ANY 0
  {-
  s <- socket AF_INET Datagram defaultProtocol
  bind s $ SockAddrInet aNY_PORT $ 0 -- 192 + 168 * 0x100 + 17 * 0x10000 + 103 * 0x1000000
  -}
  let fd = fdSocket s
  throwErrnoIfMinus1 "dht_init" $ dht_init fd fd nodeId nullPtr
  flip finally dht_uninit $ do

    bnodes <- bootstrapNodes
    forM_ bnodes $ \sa -> do
      putStrLn $ "Bootstrapping node " ++ show sa
      withSockAddr sa $ \saptr salen ->
        throwErrnoIfMinus1 "dht_ping_node" $ dht_ping_node saptr (fromIntegral salen)

    timeoutMVar <- newMVar 0
    dataMVar <- newEmptyMVar
    -- TODO: Kill threads on exit
    -- waking thread
    _ <- forkIO . forever $ do
        -- TODO: Randomize a bit
        t <- takeMVar timeoutMVar
        threadDelay (fromIntegral t * 1000000)
        tryPutMVar dataMVar Nothing
    -- reading thread
    _ <- forkIO . forever $ do
        putStrLn "Waiting to receive data"
        (d, f) <- recvFrom s 4096
        let f' = unwrapIpv6 f
        putStrLn $ "Received data from " ++ show f'
        putMVar dataMVar (Just (d, f'))
        
    callback <- mkCallback $ \cptr ev ihash dataptr datalen -> do
      s <- peekCAStringLen (dataptr, fromIntegral datalen)
      hash <- peekCAStringLen (ihash, 20)
      putStrLn $ "Event " ++ show ev ++ ", data length " ++ show datalen
                 ++ ", data " ++ s ++ ", hash " ++ hash

    flip finally (freeHaskellFunPtr callback) $ do
      alloca $ \tosleep'p -> do
        throwErrnoIfMinus1 "dht_periodic/0"
          $ dht_periodic nullPtr 0 nullPtr 0 tosleep'p callback nullPtr

        forever $ do
          input'm <- takeMVar dataMVar
          case input'm of
              Nothing -> 
                  throwErrnoIfMinus1 "dht_periodic/0"
                  $ dht_periodic nullPtr 0 nullPtr 0 tosleep'p callback nullPtr
              Just (bs, sa) ->
                  BS.useAsCStringLen bs $ \(ptr, len) ->
                      withSockAddr sa $ \saptr salen -> do
                          throwErrnoIfMinus1 "dht_periodic/0"
                          $ dht_periodic ptr (fromIntegral len)
                                         saptr (fromIntegral salen)
                                         tosleep'p callback nullPtr
                                
          tosleep <- peek tosleep'p
          putStrLn $ "Should sleep: " ++ show tosleep
          -- TODO: Portable!
          let (CTime tosleep'i) = tosleep

          -- print nodes stats
          let status af = 
                alloca $ \goodptr ->
                  alloca $ \dubiousptr ->
                    alloca $ \cachedptr ->
                      alloca $ \incomingptr -> do
                        throwErrnoIfMinus1 "dht_nodes"
                          $ dht_nodes af goodptr dubiousptr cachedptr incomingptr
                        good <- peek goodptr
                        dubious <- peek dubiousptr
                        cached <- peek cachedptr
                        incoming <- peek incomingptr
                        putStrLn $ "AF:" ++ show af
                                   ++ " good = " ++ show good
                                   ++ ", dubious = " ++ show dubious
                                   ++ ", cached = " ++ show cached
                                   ++ ", incoming = " ++ show incoming
                        return (good, dubious)

          (g4, d4) <- status 2 -- ipv4
          (g6, d6) <- status 10 -- ipv6

          -- TEST
          when ((g4 + g6 >= 4) && (d6 + d6 >= 30)) $ do
            putStrLn "Starting search <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
            -- debian-8.5.0-amd64-netinst.iso.torrent
            let (ihash, _) = B16.decode "47b9ad52c009f3bd562ffc6da40e5c55d3fb47f3"
            print $ BS.length ihash
            BS.useAsCString ihash $ \hash ->
              throwErrnoIfMinus1_ "dht_search"
                $ dht_search hash 0 {-af=-}10 callback nullPtr

          tryPutMVar timeoutMVar tosleep'i
