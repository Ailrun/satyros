{-# LANGUAGE TemplateHaskell #-}
module Satyros.CNF.Clause
  ( Clause(Clause)
  , literalsOfClause
  , emptyClause
  , unitClause
  , maxVariableInClause
  ) where

import           Control.Lens         (Iso', _Wrapped, makeWrapped)
import           Satyros.CNF.Literal  (Literal, literalToVariable)
import           Satyros.CNF.Variable (Variable)

newtype Clause = Clause { _literalsOfClause :: [Literal] }
  deriving newtype (Show, Semigroup, Monoid)

makeWrapped ''Clause

literalsOfClause :: Iso' Clause [Literal]
literalsOfClause = _Wrapped
{-# INLINE literalsOfClause #-}

emptyClause :: Clause -> Bool
emptyClause (Clause []) = True
emptyClause _           = False
{-# INLINE emptyClause #-}

unitClause :: Clause -> Bool
unitClause (Clause [_]) = True
unitClause _            = False
{-# INLINE unitClause #-}

maxVariableInClause :: Clause -> Variable
maxVariableInClause = maximum . fmap literalToVariable . _literalsOfClause
{-# INLINE maxVariableInClause #-}
