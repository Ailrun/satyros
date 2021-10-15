{-# LANGUAGE TemplateHaskell #-}
module Satyros.CNF.Formula
  ( Formula(Formula)
  , clausesOfFormula
  , maxVariableInFormula
  ) where

import           Control.Lens         (Iso', _Wrapped, makeWrapped)
import           Satyros.CNF.Clause   (Clause, maxVariableInClause)
import           Satyros.CNF.Variable (Variable)

newtype Formula = Formula { _clausesOfFormula :: [Clause] }
  deriving newtype (Show, Semigroup, Monoid)

makeWrapped ''Formula

clausesOfFormula :: Iso' Formula [Clause]
clausesOfFormula = _Wrapped
{-# INLINE clausesOfFormula #-}

maxVariableInFormula :: Formula -> Variable
maxVariableInFormula = maximum . fmap maxVariableInClause . _clausesOfFormula
{-# INLINE maxVariableInFormula #-}
