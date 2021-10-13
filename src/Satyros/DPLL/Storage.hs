module Satyros.DPLL.Storage where

import           Data.IntSet             (IntSet)
import           Data.Vector             (Vector)
import qualified Satyros.CNF             as CNF
import           Satyros.DPLL.Assignment (Assignment)
import           System.Random           (StdGen)

data Storage
  = Storage
    { unsetVariables :: IntSet
    , clauses        :: Vector CNF.Clause
    , assignment     :: Assignment
    , variableLevels :: [(Int, IntSet)]
    , stdGen         :: StdGen
    }
  deriving stock (Show)
