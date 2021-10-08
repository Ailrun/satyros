module Satyros.CNF.Formula where

import           Satyros.CNF.Clause   (Clause, maxVariableInClause)
import           Satyros.CNF.Variable (Variable)

newtype Formula = Formula { unFormula :: [Clause] }
  deriving newtype (Show, Semigroup, Monoid)

maxVariableInFormula :: Formula -> Variable
maxVariableInFormula = maximum . fmap maxVariableInClause . unFormula
{-# INLINE maxVariableInFormula #-}
