{-# LANGUAGE ViewPatterns #-}
module Satyros.BellmanFord.Propagation where

import           Control.Lens                (_1, at, each, uses, (.=), (^..))
import           Control.Monad               (forM_, when)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import qualified Data.Set                    as Set
import           Satyros.BellmanFord.Effect  (BellmanFord, propagationCheck,
                                              propagationEnd,
                                              propagationFindShorter,
                                              propagationNth)
import           Satyros.BellmanFord.Storage (IDLGraph)

propagation :: IDLGraph -> BellmanFord ()
propagation (Map.toList -> graph) = do
  forM_ [1..Set.size (Set.fromList (graph ^.. each . _1))] $ \n -> do
    propagationNth n
    forM_ graph $ \((f, t), w) -> do
      propagationCheck (f, t)
      (_, df) <- uses (at f) fromJust
      (pt, dt) <- uses (at t) fromJust
      when (df + w < dt) $ do
        at t .= Just (f, df + w)
        propagationFindShorter t (pt, dt)
  propagationEnd
