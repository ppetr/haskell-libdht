{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base16 as B16
import Data.Monoid
import Data.Word (Word32)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.Socket.Internal

import Network.DHT.Native
import Network.DHT.Native.FFI

-- * Bootstrapping

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

-- * Utilities

hostToNetwork :: Word32 -> Word32
hostToNetwork = htonl

unwrapIpv6 :: SockAddr -> SockAddr
unwrapIpv6 (SockAddrInet6 port _ (0, 0, 0xffff, ipv4addr) _) =
  SockAddrInet port (hostToNetwork ipv4addr)
unwrapIpv6 sa = sa

withSockAddrArray_ :: Family -> Int -> (Ptr SockAddr -> Ptr CInt -> IO a) -> IO (a, [SockAddr])
withSockAddrArray_ family count act =
    alloca $ \numptr ->
      allocaBytes (sizeOfSockAddrByFamily family * count) $ \saptr -> do
        poke numptr (fromIntegral count)
        r <- act saptr numptr
        -- retrieve the results
        c <- fromIntegral <$> peek numptr
        sas <- loopPtr saptr c
        return (r, sas)
  where
    loopPtr :: Ptr SockAddr -> Int -> IO [SockAddr]
    loopPtr ptr 0 = return []
    loopPtr ptr n = do
      sa <- peekSockAddr ptr
      (sa :) <$> loopPtr (plusPtr ptr (sizeOfSockAddr sa)) (n - 1)

-- * Main

main :: IO ()
main = do
  -- set debugging
  dht_debug_to_stderr

  searchOpen <- newMVar ()

  nodeId <- newCString "01234567890123456789"
  s <- socket AF_INET6 Datagram defaultProtocol
  bind s $ SockAddrInet6 aNY_PORT 0 iN6ADDR_ANY 0
  {-
  s <- socket AF_INET Datagram defaultProtocol
  bind s $ SockAddrInet aNY_PORT $ 0 -- 192 + 168 * 0x100 + 17 * 0x10000 + 103 * 0x1000000
  -}
  port <- socketPort s
  putStrLn $ "Listening on port " ++ show port
  let fd = fdSocket s
  dht_init fd fd nodeId nullPtr
  flip finally dht_uninit $ do
    bnodes <- bootstrapNodes
    forM_ bnodes $ \sa -> do
      putStrLn $ "Bootstrapping node " ++ show sa
      withSockAddr sa $ \saptr salen ->
        dht_ping_node saptr (fromIntegral salen)

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
        
    callback <- mkCallback . makeCallback $ \ihash result -> do
      putStrLn $ ">>> Event: " ++ show ihash ++ " -> " ++ show result

    flip finally (freeHaskellFunPtr callback) $ do
      alloca $ \tosleep'p -> do
        dht_periodic nullPtr 0 nullPtr 0 tosleep'p callback nullPtr

        forever $ do
          input'm <- takeMVar dataMVar
          case input'm of
              Nothing -> 
                  dht_periodic nullPtr 0 nullPtr 0 tosleep'p callback nullPtr
              Just (bs, sa) ->
                  BS.useAsCStringLen bs $ \(ptr, len) ->
                      withSockAddr sa $ \saptr salen -> do
                          dht_periodic ptr (fromIntegral len)
                                         saptr (fromIntegral salen)
                                         tosleep'p callback nullPtr
                                
          tosleep <- peek tosleep'p
          putStrLn $ "Should sleep: " ++ show tosleep
          -- TODO: Portable!
          let (CTime tosleep'i) = tosleep

          -- print nodes stats
          stats <- nodeStats
          putStrLn $ "Node stats: " ++ show stats

          -- Dump the table
          ((_, nodes6), nodes4) <- withSockAddrArray_ AF_INET 1024 $ \sa4 num4 -> do
            withSockAddrArray_ AF_INET6 1024 $ \sa6 num6 ->
              dht_get_nodes sa4 num4 sa6 num6
          print (nodes4, nodes6)

          -- TEST
          started <- isEmptyMVar searchOpen
          when (not started && enoughNodesForSearch (uncurry mappend stats)) $ do
            takeMVar searchOpen
            putStrLn "Starting search <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
            -- debian-8.5.0-amd64-netinst.iso.torrent
            let (ihash, _) = B16.decode "47b9ad52c009f3bd562ffc6da40e5c55d3fb47f3"
            print $ BS.length ihash
            BS.useAsCString ihash $ \hash ->
                dht_search hash 0 {-af=-}2 callback nullPtr
            BS.useAsCString ihash $ \hash ->
                dht_search hash 0 {-af=-}10 callback nullPtr

          tryPutMVar timeoutMVar tosleep'i
