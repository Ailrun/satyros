module Satyros.QFIDL.Expressed where

import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Satyros.QFIDL.Variable (Variable)

data Expressed
  = LessThanEqualTo Variable Variable Int
  deriving stock (Eq, Ord, Show)

variablesInExpressed :: Expressed -> Set Variable
variablesInExpressed (LessThanEqualTo x1 x2 _) = Set.fromList [x1, x2]
