module Satyros.BellmanFord.Store where

import           Data.Map      (Map)
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Satyros.QFIDL as QFIDL

type Store = IDLWeightMap

type IDLGraph = Map (IDLGraphVertex, IDLGraphVertex) Int
type IDLGraphVertex = Maybe QFIDL.Variable
type IDLWeightMap = Map IDLGraphVertex (IDLGraphVertex, PositiveInfiniteInt)

rootIDLGraphVertex :: IDLGraphVertex
rootIDLGraphVertex = Nothing

initializeStore :: [QFIDL.Expressed] -> (IDLGraph, Store)
initializeStore es = (Map.fromList $ [((rootIDLGraphVertex, rootIDLGraphVertex), 0)] <> ((, 0) . (rootIDLGraphVertex, ) . Just <$> vars) <> edges, Map.fromList . ((rootIDLGraphVertex, (rootIDLGraphVertex, Finite 0)) :) $ ((,) <*> (, PositiveInfinity)) . Just <$> vars)
  where
    vars = Set.toList . Set.unions $ fmap QFIDL.variablesInExpressed es

    edges = (\(QFIDL.LessThanEqualTo x1 x2 v) -> ((Just x1, Just x2), v)) <$> es

storeToValues :: IDLWeightMap -> [Int]
storeToValues m
  | Just QFIDL.ZeroVariable `Map.member` m = vs
  | otherwise = map (subtract $ head vs) (tail vs)
  where
    vs = mapMaybe (\x -> fst x >> negate <$> toInt (snd (snd x))) . Map.toAscList $ m

data PositiveInfiniteInt
  = Finite Int
  | PositiveInfinity
  deriving stock (Eq, Ord, Show)

addPositiveInfiniteInt :: PositiveInfiniteInt -> PositiveInfiniteInt -> PositiveInfiniteInt
addPositiveInfiniteInt (Finite n)       (Finite m)       = Finite (n + m)
addPositiveInfiniteInt _                PositiveInfinity = PositiveInfinity
addPositiveInfiniteInt PositiveInfinity _                = PositiveInfinity

toInt :: PositiveInfiniteInt -> Maybe Int
toInt (Finite n)       = Just n
toInt PositiveInfinity = Nothing
