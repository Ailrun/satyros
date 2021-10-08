module Satyros.DPLL.State where

import           Data.IntMap.Strict (IntMap)
import           Data.IntSet        (IntSet)
import           Data.Vector        (Vector)
import qualified Satyros.CNF        as CNF
import           System.Random      (StdGen)

data GeneralState
  = GeneralState
    { unsetVariables :: IntSet
    , clauses        :: Vector CNF.Clause
    , assignments    :: IntMap (CNF.Positivity, Bool, CNF.Variable, Int)
    , variableLevels :: [(Int, IntSet)]
    , stdGen         :: StdGen
    }
