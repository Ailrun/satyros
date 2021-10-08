{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
module Satyros.CNF.Literal
  ( Literal(Literal)
  , literalToVariable
  , literalToPositivity
  ) where

import           Satyros.CNF.Positivity (Positivity (Negative, Positive),
                                         boolToPositivity)
import           Satyros.CNF.Variable   (Variable (Variable))
import           Satyros.Util           (intToWord, wordToInt)

newtype Literal = LiteralInternal Int
  deriving stock (Eq)
  deriving newtype (Show)

pattern Literal :: Positivity -> Variable -> Literal
pattern Literal a b <- (matchLiteral -> (a, b)) where
  Literal Positive (Variable b) = LiteralInternal (wordToInt b)
  Literal Negative (Variable b) = LiteralInternal (- wordToInt b)
{-# COMPLETE Literal #-}

matchLiteral :: Literal -> (Positivity, Variable)
matchLiteral l = (literalToPositivity l, literalToVariable l)
{-# INLINE matchLiteral #-}

literalToVariable :: Literal -> Variable
literalToVariable (LiteralInternal n) = Variable (intToWord (abs n))
{-# INLINE literalToVariable #-}

literalToPositivity :: Literal -> Positivity
literalToPositivity (LiteralInternal n) = boolToPositivity (n > 0)
{-# INLINE literalToPositivity #-}
