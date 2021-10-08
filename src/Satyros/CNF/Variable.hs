{-# LANGUAGE PatternSynonyms #-}
module Satyros.CNF.Variable
  ( Variable(Variable)
  ) where

newtype Variable = VariableInternal Word
  deriving stock (Eq, Ord)
  deriving newtype (Show)

pattern Variable :: Word -> Variable
pattern Variable v <- VariableInternal v where
  Variable v
    | v > 0     = VariableInternal v
    | otherwise = error "Zero is not a valid variable"
{-# COMPLETE Variable #-}
