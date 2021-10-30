{-# LANGUAGE PatternSynonyms #-}
module Satyros.QFIDL.Variable
  ( Variable(ZeroVariable, Variable)
  ) where

newtype Variable = VariableInternal Word
  deriving stock (Eq, Ord)
  deriving newtype (Show)

pattern ZeroVariable :: Variable
pattern ZeroVariable = VariableInternal 0

pattern Variable :: Word -> Variable
pattern Variable v <- VariableInternal v where
  Variable v
    | v > 0     = VariableInternal v
    | otherwise = error "Zero is not a valid variable"
{-# COMPLETE ZeroVariable, Variable #-}
