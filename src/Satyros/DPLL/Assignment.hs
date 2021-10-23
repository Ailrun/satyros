module Satyros.DPLL.Assignment where

import           Control.Lens (At (at), Index, IxValue, Ixed, Traversal',
                               Wrapped (Unwrapped, _Wrapped'), _1, _2, _Just,
                               from, iso, ix, (?~))
import           Data.Bool    (bool)
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Set     (Set)
import qualified Satyros.CNF  as CNF

newtype Assignment = Assignment { getAssignment :: Map CNF.Variable (Bool, Maybe CNF.Clause) }
  deriving newtype (Show)

instance Wrapped Assignment where
  type Unwrapped Assignment = Map CNF.Variable (Bool, Maybe CNF.Clause)
  _Wrapped' = iso getAssignment Assignment

type instance Index Assignment = CNF.Variable
type instance IxValue Assignment = (Bool, Maybe CNF.Clause)

instance Ixed Assignment
instance At Assignment where
  at i = _Wrapped' . at i

assignVariable :: CNF.Literal -> Maybe CNF.Clause -> Assignment -> Assignment
assignVariable (CNF.Literal CNF.Positive x) p = at x ?~ (True, p)
assignVariable (CNF.Literal CNF.Negative x) p = at x ?~ (False, p)

eraseVariables :: Set CNF.Variable -> Assignment -> Assignment
eraseVariables xs = Assignment . flip Map.withoutKeys xs . getAssignment

valueOfVariable :: CNF.Variable -> Traversal' Assignment Bool
valueOfVariable x = ix x . _1

valueOfLiteral :: CNF.Literal -> Traversal' Assignment Bool
valueOfLiteral (CNF.Literal v x) = valueOfVariable x . from CNF.isPositive . iso (== v) (bool (CNF.negatePositivity v) v)

parentsOfLiteral :: CNF.Literal -> Traversal' Assignment (Maybe CNF.Clause)
parentsOfLiteral (CNF.Literal _ x) = at x . _Just . _2
