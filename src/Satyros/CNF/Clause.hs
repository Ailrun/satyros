{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Satyros.CNF.Clause
  ( ClauseLike(Clause, ClauseLike)
  , Clause
  , entriesOfClauseLike
  , literalsOfClause
  , emptyClause
  , unitClause
  , maxVariableInClause
  ) where

import           Control.Lens         (Iso', _Wrapped, makeWrapped)
import           GHC.Generics         (Generic, Generic1)
import           Satyros.CNF.Literal  (Literal, literalToVariable)
import           Satyros.CNF.Variable (Variable)

newtype ClauseLike a = ClauseLike { _literalsOfClause :: [a] }
  deriving stock (Generic, Generic1)
  deriving newtype (Show, Semigroup, Monoid)

makeWrapped ''ClauseLike

type Clause = ClauseLike Literal

pattern Clause :: [Literal] -> Clause
pattern Clause v = ClauseLike v
{-# COMPLETE Clause #-}

entriesOfClauseLike :: Iso' (ClauseLike a) [a]
entriesOfClauseLike = _Wrapped
{-# INLINE entriesOfClauseLike #-}

literalsOfClause :: Iso' Clause [Literal]
literalsOfClause = entriesOfClauseLike
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
