module Network.DHT.Native.Stats where

import           Control.Monad.Codensity
import           Control.Monad.Trans
import           Data.Monoid
import qualified Foreign.Storable as P

import           Network.DHT.Native.FFI
import           Network.DHT.Native.Util

data NodeStats = NodeStats
    { nodeStatsGood :: !Int
    , nodeStatsDubious :: !Int
    , nodeStatsCached :: !Int
    , nodeStatsIncoming :: !Int
    }
  deriving (Eq, Ord, Show, Read)

instance Monoid NodeStats where
    mempty = NodeStats 0 0 0 0
    mappend (NodeStats g1 d1 c1 i1) (NodeStats g2 d2 c2 i2) =
        NodeStats (g1 + g2) (d1 + d2) (c1 + c2) (i1 + i2)

nodeStats :: IO (NodeStats, NodeStats)
nodeStats = (,) <$> stats afIPv4 <*> stats afIPv6
  where
    stats af = lowerCodensity $ do
        (goodptr, dubiousptr, cachedptr, incomingptr) <-
            (,,,) <$> alloca <*> alloca <*> alloca <*> alloca
        lift $ dht_nodes af goodptr dubiousptr cachedptr incomingptr
        NodeStats <$> peek' goodptr <*> peek' dubiousptr
                  <*> peek' cachedptr <*> peek' incomingptr
    peek' ptr = lift (fromIntegral <$> P.peek ptr)

-- | Returns 'True' if there are enough nodes to start a search.
-- Currently the heuristic is that there are at least 4 good nodes and at least
-- 30 good+dubious ones.
enoughNodesForSearch :: NodeStats -> Bool
enoughNodesForSearch (NodeStats g d _ _) =
    (g >= 4) && (g + d >= 30)
