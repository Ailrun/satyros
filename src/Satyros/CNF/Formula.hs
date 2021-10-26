{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Satyros.CNF.Formula
  ( FormulaLike(Formula, FormulaLike)
  , Formula
  , clauseLikesOfFormulaLike
  , clausesOfFormula
  , maxVariableInFormula
  ) where

import           Control.Lens         (Iso', _Wrapped, makeWrapped)
import           Satyros.CNF.Clause   (Clause, ClauseLike, maxVariableInClause)
import           Satyros.CNF.Literal  (Literal)
import           Satyros.CNF.Variable (Variable)

newtype FormulaLike a = FormulaLike { _clausesOfFormula :: [ClauseLike a] }
  deriving newtype (Show, Semigroup, Monoid)

makeWrapped ''FormulaLike

type Formula = FormulaLike Literal

pattern Formula :: [Clause] -> Formula
pattern Formula v = FormulaLike v
{-# COMPLETE Formula #-}

clauseLikesOfFormulaLike :: Iso' (FormulaLike a) [ClauseLike a]
clauseLikesOfFormulaLike = _Wrapped
{-# INLINE clauseLikesOfFormulaLike #-}

clausesOfFormula :: Iso' Formula [Clause]
clausesOfFormula = clauseLikesOfFormulaLike
{-# INLINE clausesOfFormula #-}

maxVariableInFormula :: Formula -> Variable
maxVariableInFormula = maximum . fmap maxVariableInClause . _clausesOfFormula
{-# INLINE maxVariableInFormula #-}
