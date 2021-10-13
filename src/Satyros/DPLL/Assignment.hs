module Satyros.DPLL.Assignment where

import           Data.IntMap  (IntMap)
import qualified Data.IntMap  as IntMap
import qualified Satyros.CNF  as CNF
import           Satyros.Util (wordToInt)

type Covered = Bool
newtype Assignment = Assignment (IntMap (Bool, Covered, Maybe CNF.Clause))
  deriving newtype (Show)

value :: (Bool, Covered, Maybe CNF.Clause) -> Bool
value (v, _, _) = v

assignValue :: CNF.Literal -> Maybe CNF.Clause -> Assignment -> Assignment
assignValue (CNF.Literal CNF.Positive (CNF.Variable v)) p (Assignment m) = Assignment $ IntMap.insert (wordToInt v) (True, False, p) m
assignValue (CNF.Literal CNF.Negative (CNF.Variable v)) p (Assignment m) = Assignment $ IntMap.insert (wordToInt v) (False, False, p) m

getAssignedValue :: Assignment -> CNF.Literal -> Maybe Bool
getAssignedValue (Assignment asgn) (CNF.Literal p (CNF.Variable v)) = (p ==) . CNF.boolToPositivity . value <$> IntMap.lookup (wordToInt v) asgn
