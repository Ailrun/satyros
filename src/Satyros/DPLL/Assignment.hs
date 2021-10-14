module Satyros.DPLL.Assignment where

import           Control.Lens (At (at), Getter, Index, IxValue, Ixed,
                               Wrapped (Unwrapped, _Wrapped'), _1, _3, _Just,
                               iso, pre, to)
import           Data.Map     (Map)
import qualified Data.Map     as Map
import qualified Satyros.CNF  as CNF

type Covered = Bool
newtype Assignment = Assignment { getAssignment :: Map CNF.Variable (Bool, Covered, Maybe CNF.Clause) }
  deriving newtype (Show)

instance Wrapped Assignment where
  type Unwrapped Assignment = Map CNF.Variable (Bool, Covered, Maybe CNF.Clause)
  _Wrapped' = iso getAssignment Assignment

type instance Index Assignment = CNF.Variable
type instance IxValue Assignment = (Bool, Covered, Maybe CNF.Clause)

instance Ixed Assignment
instance At Assignment where
  at i = _Wrapped' . at i

assignValue :: CNF.Literal -> Maybe CNF.Clause -> Assignment -> Assignment
assignValue (CNF.Literal CNF.Positive x) p (Assignment m) = Assignment $ Map.insert x (True, False, p) m
assignValue (CNF.Literal CNF.Negative x) p (Assignment m) = Assignment $ Map.insert x (False, False, p) m

valueOfLiteral :: CNF.Literal -> Getter Assignment (Maybe Bool)
valueOfLiteral (CNF.Literal v x) = at x . pre (_Just . _1 . to ((== v) . CNF.boolToPositivity))

parentsOfLiteral :: CNF.Literal -> Getter Assignment (Maybe (Maybe CNF.Clause))
parentsOfLiteral (CNF.Literal _ x) = at x . pre (_Just . _3)

-- getCovered :: Assignment -> CNF.Literal -> Maybe Covered
-- getCovered (Assignment asgn) (CNF.Literal _ x) = covered <$> Map.lookup x asgn

-- getParents :: Assignment -> CNF.Literal -> Maybe (Maybe CNF.Clause)
-- getParents (Assignment asgn) (CNF.Literal _ x) = parents <$> Map.lookup x asgn
