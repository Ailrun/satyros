{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE TemplateHaskell        #-}
module Satyros.CNF.Positivity
  ( Positivity(Positive, Negative)
  , isPositive
  , negatePositivity
  ) where

import           Control.Lens           (Iso', _Wrapped, makeWrapped)
import           Data.Coerce            (coerce)
import           System.Random.Stateful (Random)

newtype Positivity = Positivity Bool
  deriving stock (Eq)
  deriving newtype (Show, Random)

makeWrapped ''Positivity

isPositive :: Iso' Positivity Bool
isPositive = _Wrapped
{-# INLINE isPositive #-}

pattern Positive :: Positivity
pattern Positive = Positivity True
pattern Negative :: Positivity
pattern Negative = Positivity False
{-# COMPLETE Positive, Negative #-}

negatePositivity :: Positivity -> Positivity
negatePositivity = coerce not
{-# INLINE negatePositivity #-}
