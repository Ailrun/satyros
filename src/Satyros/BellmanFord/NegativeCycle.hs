module Satyros.BellmanFord.NegativeCycle where

import           Control.Lens                 (at, uses)
import           Control.Monad                (forM_, when)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust)
import           Satyros.BellmanFord.Effect   (BellmanFord, negativeCycleCheck,
                                               negativeCycleFind,
                                               negativeCyclePass)
import           Satyros.BellmanFord.IDLGraph (IDLGraph,
                                               PositiveInfiniteInt (Finite),
                                               addPositiveInfiniteInt)

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
      | f == p = pure [(p, n, graph Map.! (p, n))]
      | otherwise = do
          (pp, _) <- uses (at p) fromJust
          ((p, n, graph Map.! (p, n)) :) <$> getCycleFrom f pp p
