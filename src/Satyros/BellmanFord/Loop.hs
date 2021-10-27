{-# LANGUAGE ViewPatterns #-}
module Satyros.BellmanFord.Loop where

import           Control.Lens                 (_1, at, each, uses, (.=), (^..))
import           Control.Monad                (forM_, when)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust)
import qualified Data.Set                     as Set
import           Satyros.BellmanFord.Effect   (BellmanFord, loopCheck, loopEnd,
                                               loopFindShorter, loopNth)
import           Satyros.BellmanFord.IDLGraph (IDLGraph,
                                               PositiveInfiniteInt (Finite),
                                               addPositiveInfiniteInt)

loop :: IDLGraph -> BellmanFord ()
loop (Map.toList -> graph) = do
  forM_ [1..Set.size (Set.fromList (graph ^.. each . _1))] $ \n -> do
    loopNth n
    forM_ graph $ \((f, t), w) -> do
      loopCheck (f, t)
      (_, df) <- uses (at f) fromJust
      (pt, dt) <- uses (at t) fromJust
      when (addPositiveInfiniteInt df (Finite w) < dt) $ do
        at t .= Just (f, addPositiveInfiniteInt df (Finite w))
        loopFindShorter t (pt, dt)
  loopEnd
