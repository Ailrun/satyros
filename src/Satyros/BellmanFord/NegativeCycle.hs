module Satyros.BellmanFord.NegativeCycle where

import           Control.Lens               (at, uses)
import           Control.Monad              (forM_, when)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromJust)
import           Satyros.BellmanFord.Effect (BellmanFord, negativeCycleCheck,
                                             negativeCycleFind,
                                             negativeCyclePass)
import           Satyros.BellmanFord.Store  (IDLGraph,
                                             PositiveInfiniteInt (Finite),
                                             addPositiveInfiniteInt)
import qualified Satyros.QFIDL              as QFIDL

negativeCycle :: IDLGraph -> BellmanFord ()
negativeCycle graph = do
  forM_ (Map.toList graph) $ \((f, t), w) -> do
    negativeCycleCheck (f, t)
    (pf, df) <- uses (at f) fromJust
    (_, dt) <- uses (at t) fromJust
    when (addPositiveInfiniteInt df (Finite w) < dt) $ do
      clc <- getCycleFrom f pf f
      negativeCycleFind clc
  negativeCyclePass
  where
    getCycleFrom f p n
      | f == p = pure [QFIDL.LessThanEqualTo (fromJust p) (fromJust n) (graph Map.! (p, n))]
      | otherwise = do
          (pp, _) <- uses (at p) fromJust
          (QFIDL.LessThanEqualTo (fromJust p) (fromJust n) (graph Map.! (p, n)) :) <$> getCycleFrom f pp p
