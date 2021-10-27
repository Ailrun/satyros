module Satyros.BellmanFord.IDLGraph where

import           Data.Map    (Map)
import qualified Satyros.CNF as QFIDL

data PositiveInfiniteInt
  = Finite Int
  | PositiveInfinity
  deriving stock (Eq, Ord, Show)

addPositiveInfiniteInt :: PositiveInfiniteInt -> PositiveInfiniteInt -> PositiveInfiniteInt
addPositiveInfiniteInt (Finite n)       (Finite m)       = Finite (n + m)
addPositiveInfiniteInt _                PositiveInfinity = PositiveInfinity
addPositiveInfiniteInt PositiveInfinity _                = PositiveInfinity

type IDLGraph = Map (IDLGraphVertex, IDLGraphVertex) Int
type IDLGraphVertex = Maybe QFIDL.Variable
type IDLWeightMap = Map IDLGraphVertex (IDLGraphVertex, PositiveInfiniteInt)

rootIDLGraphVertex :: IDLGraphVertex
rootIDLGraphVertex = Nothing
