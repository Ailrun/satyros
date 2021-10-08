module Satyros.CNF.Clause where

import           Satyros.CNF.Literal  (Literal, literalToVariable)
import           Satyros.CNF.Variable (Variable)

newtype Clause = Clause { unClause :: [Literal] }
  deriving newtype (Show, Semigroup, Monoid)

emptyClause :: Clause -> Bool
emptyClause (Clause []) = True
emptyClause _           = False
{-# INLINE emptyClause #-}

unitClause :: Clause -> Bool
unitClause (Clause [_]) = True
unitClause _            = False
{-# INLINE unitClause #-}

maxVariableInClause :: Clause -> Variable
maxVariableInClause = maximum . fmap literalToVariable . unClause
{-# INLINE maxVariableInClause #-}
