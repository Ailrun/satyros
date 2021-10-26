{-# LANGUAGE PatternSynonyms #-}
module Satyros.QFIDL.Variable
  ( Variable(Variable)
  ) where

newtype Variable = Variable Word
  deriving stock (Eq, Ord)
  deriving newtype (Show)
