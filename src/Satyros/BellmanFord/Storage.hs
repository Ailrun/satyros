module Satyros.BellmanFord.Storage
  ( Storage
  , IDLGraph
  , IDLGraphVertex
  , IDLWeightMap
  , rootIDLGraphVertex
  , initializeStorage
  , storageToValues
  ) where

import           Data.Functor  (($>))
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe    (mapMaybe)
import qualified Data.Set      as Set
import qualified Satyros.QFIDL as QFIDL

type Storage = IDLWeightMap

type IDLGraph = Map (IDLGraphVertex, IDLGraphVertex) Int
type IDLGraphVertex = Maybe QFIDL.Variable
type IDLWeightMap = Map IDLGraphVertex (IDLGraphVertex, Int)

rootIDLGraphVertex :: IDLGraphVertex
rootIDLGraphVertex = Nothing

initializeStorage :: [QFIDL.Expressed] -> (IDLGraph, Storage)
initializeStorage es = (Map.fromList $ [((rootIDLGraphVertex, rootIDLGraphVertex), 0)] <> ((, 0) . (rootIDLGraphVertex, ) . Just <$> vars) <> edges, Map.fromList . ((rootIDLGraphVertex, (rootIDLGraphVertex, 0)) :) $ ((,) <*> (, 0)) . Just <$> vars)
  where
    vars = Set.toList . Set.unions $ fmap QFIDL.variablesInExpressed es

    edges = (\(QFIDL.LessThanEqualTo x1 x2 v) -> ((Just x1, Just x2), v)) <$> es

storageToValues :: IDLWeightMap -> [Int]
storageToValues m
  | Just QFIDL.ZeroVariable `Map.member` m = map (subtract $ head vs) (tail vs)
  | otherwise = vs
  where
    vs = mapMaybe (\x -> fst x $> negate (snd (snd x))) . Map.toAscList $ m
