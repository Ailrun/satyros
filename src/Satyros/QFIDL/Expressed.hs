module Satyros.QFIDL.Expressed where

import           Satyros.QFIDL.Variable    (Variable)

data Expressed
  = LessThanEqualTo Variable Variable Int
  deriving stock (Eq, Ord, Show)
