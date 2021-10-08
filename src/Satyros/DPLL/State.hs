module Satyros.DPLL.State where

import           Data.IntSet             (IntSet)
import           Data.Vector             (Vector)
import qualified Satyros.CNF             as CNF
import           Satyros.DPLL.Assignment (Assignment)
import           System.Random           (StdGen)

data GeneralState
  = GeneralState
    { unsetVariables :: IntSet
    , clauses        :: Vector CNF.Clause
    , assignment     :: Assignment
    , variableLevels :: [(Int, IntSet)]
    , stdGen         :: StdGen
    }
  deriving stock (Show)
