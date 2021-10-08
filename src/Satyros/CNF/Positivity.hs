{-# LANGUAGE PatternSynonyms #-}
module Satyros.CNF.Positivity
  ( Positivity(Positive, Negative)
  , boolToPositivity
  , negatePositivity
  ) where

import           Data.Coerce            (coerce)
import           System.Random.Stateful (Random)

newtype Positivity = Positivity Bool
  deriving stock (Eq)
  deriving newtype (Show, Random)

pattern Positive :: Positivity
pattern Positive = Positivity True
pattern Negative :: Positivity
pattern Negative = Positivity False
{-# COMPLETE Positive, Negative #-}

boolToPositivity :: Bool -> Positivity
boolToPositivity = Positivity
{-# INLINE boolToPositivity #-}

negatePositivity :: Positivity -> Positivity
negatePositivity = coerce not
{-# INLINE negatePositivity #-}
