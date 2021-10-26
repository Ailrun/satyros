module Satyros.QFIDL.Expressible where

import           Satyros.QFIDL.Variable (Variable)

data Expressible
  = Singleton Variable Operator Double
  | Difference Variable Variable Operator Double
  deriving stock (Eq, Show)

data Operator
  = (::<?)
  | (::<=?)
  | (::>?)
  | (::>=?)
  | (::=?)
  | (::<>?)
  deriving stock (Eq, Show)
