{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
module Satyros.CNF.Literal
  ( Positivity
  , Literal(Literal)
  , literalToVariable
  , literalToPositivity
  ) where

import           Satyros.CNF.Variable (Variable (Variable))
import           Satyros.Util         (intToWord, wordToInt)

type Positivity = Bool

newtype Literal = LiteralInternal Int
  deriving stock (Eq)
  deriving newtype (Show)

pattern Literal :: Positivity -> Variable -> Literal
pattern Literal a b <- (matchLiteral -> (a, b)) where
  Literal True  (Variable b) = LiteralInternal (wordToInt b)
  Literal False (Variable b) = LiteralInternal (- wordToInt b)
{-# COMPLETE Literal #-}

matchLiteral :: Literal -> (Positivity, Variable)
matchLiteral l = (literalToPositivity l, literalToVariable l)
{-# INLINE matchLiteral #-}

literalToVariable :: Literal -> Variable
literalToVariable (LiteralInternal n) = Variable (intToWord (abs n))
{-# INLINE literalToVariable #-}

literalToPositivity :: Literal -> Positivity
literalToPositivity (LiteralInternal n) = n > 0
{-# INLINE literalToPositivity #-}
