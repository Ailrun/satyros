module Satyros.QFIDL.Expressible where

import           GHC.Generics           (Generic)
import           Satyros.QFIDL.Variable (Variable)

data Expressible
  = Singleton Variable Operator Double
  | Difference Variable Variable Operator Double
  deriving stock (Generic, Eq, Show)

data Operator
  = (::<?)
  | (::<=?)
  | (::>?)
  | (::>=?)
  | (::=?)
  | (::<>?)
  deriving stock (Generic, Eq, Show)
