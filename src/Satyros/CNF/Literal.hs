{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
module Satyros.CNF.Literal
  ( Literal(Literal)
  , negateLiteral
  , literalToVariable
  , literalToPositivity
  ) where

import           Control.Lens           ((#))
import           GHC.Generics           (Generic)
import           Satyros.CNF.Positivity (Positivity (Negative, Positive),
                                         isPositive)
import           Satyros.CNF.Variable   (Variable (Variable))
import           Satyros.Util           (intToWord, wordToInt)

newtype Literal = LiteralInternal Int
  deriving stock (Generic, Eq, Ord)
  deriving newtype (Show)

pattern Literal :: Positivity -> Variable -> Literal
pattern Literal a b <- (matchLiteral -> (a, b)) where
  Literal Positive (Variable b) = LiteralInternal (wordToInt b)
  Literal Negative (Variable b) = LiteralInternal (- wordToInt b)
{-# COMPLETE Literal #-}

matchLiteral :: Literal -> (Positivity, Variable)
matchLiteral l = (literalToPositivity l, literalToVariable l)
{-# INLINE matchLiteral #-}

negateLiteral :: Literal -> Literal
negateLiteral (LiteralInternal n) = LiteralInternal (- n)
{-# INLINE negateLiteral #-}

literalToVariable :: Literal -> Variable
literalToVariable (LiteralInternal n) = Variable (intToWord (abs n))
{-# INLINE literalToVariable #-}

literalToPositivity :: Literal -> Positivity
literalToPositivity (LiteralInternal n) = isPositive # (n > 0)
{-# INLINE literalToPositivity #-}
