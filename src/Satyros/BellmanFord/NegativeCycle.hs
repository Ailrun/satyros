module Satyros.BellmanFord.NegativeCycle where

import           Control.Lens                (at, uses)
import           Control.Monad               (forM_, when)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import qualified Data.Set                    as Set
import           Satyros.BellmanFord.Effect  (BellmanFord, negativeCycleCheck,
                                              negativeCycleFind,
                                              negativeCyclePass)
import           Satyros.BellmanFord.Storage (IDLGraph)
import qualified Satyros.QFIDL               as QFIDL

negativeCycle :: IDLGraph -> BellmanFord ()
negativeCycle graph = do
  forM_ (Map.toList graph) $ \((f, t), w) -> do
    negativeCycleCheck (f, t)
    (pf, df) <- uses (at f) fromJust
    (_, dt) <- uses (at t) fromJust
    when (df + w < dt) $ do
      clc <- getCycleFrom (Set.singleton f) pf
      negativeCycleFind clc
  negativeCyclePass
  where
    getCycleFrom visited p
      | Set.member p visited = do
          (pp, _) <- uses (at p) fromJust
          go p pp p
      | otherwise = do
          (pp, _) <- uses (at p) fromJust
          getCycleFrom (Set.insert p visited) pp

    go f p n
      | f == p = pure [QFIDL.LessThanEqualTo (fromJust p) (fromJust n) (graph Map.! (p, n))]
      | otherwise = do
          (pp, _) <- uses (at p) fromJust
          (QFIDL.LessThanEqualTo (fromJust p) (fromJust n) (graph Map.! (p, n)) :) <$> go f pp p
