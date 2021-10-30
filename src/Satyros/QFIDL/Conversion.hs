{-# LANGUAGE ViewPatterns #-}
module Satyros.QFIDL.Conversion
  ( ConversionTable
  , toCNF
  , fromAssignment
  ) where

import           Control.Applicative       (liftA2)
import           Control.Lens              (_1, at, over, view, (&), (?~))
import           Data.Coerce               (coerce)
import           Data.List                 (mapAccumL)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Monoid               (Ap (..), getAp)
import           Data.Tuple                (swap)
import qualified Satyros.CNF               as CNF
import           Satyros.QFIDL.Expressed   (Expressed (..))
import           Satyros.QFIDL.Expressible (Expressible (..), Operator (..))
import           Satyros.QFIDL.Variable    (Variable (ZeroVariable))

type ConversionTable = (Map CNF.Variable Expressed, Map Expressed CNF.Literal)

toCNF :: CNF.FormulaLike Expressible -> (CNF.Formula, ConversionTable)
toCNF = coerce $ swap . toCNF'
  where
    toCNF' :: [CNF.ClauseLike Expressible] -> (ConversionTable, [CNF.Clause])
    toCNF' =
      coerce
        ( over _1 (view _1)
          . mapAccumL @[] (mapAccumL @[] go) ((Map.empty, Map.empty), CNF.Variable <$> [1..])
        )
      . concatMap transformClauseLike

    go :: (ConversionTable, [CNF.Variable])
       -> Expressed
       -> ((ConversionTable, [CNF.Variable]), CNF.Literal)
    go s@((v2e, e2l), newVs) e
      | Just l <- Map.lookup e e2l = (s, l)
      | newV:newVs' <- newVs       = (((v2e & at newV ?~ e , e2l & at e ?~ CNF.Literal CNF.Positive newV & at (negateExpressed e) ?~ CNF.Literal CNF.Negative newV), newVs'), CNF.Literal CNF.Positive newV)
      | otherwise                  = error "toCNF: impossible case!"

fromAssignment :: ConversionTable -> [(CNF.Variable, Bool)] -> [Expressed]
fromAssignment = fmap . fromVariable

fromVariable :: ConversionTable -> (CNF.Variable, Bool) -> Expressed
fromVariable (m, _) ((m Map.!) -> e, v)
  | v         = e
  | otherwise = negateExpressed e

negateExpressed :: Expressed -> Expressed
negateExpressed (LessThanEqualTo v1 v2 n) = LessThanEqualTo v2 v1 (- n - 1)

transformClauseLike :: CNF.ClauseLike Expressible -> [CNF.ClauseLike Expressed]
transformClauseLike = coerce go
  where
    go :: [Expressible] -> [[Expressed]]
    go = getAp . mconcat . fmap (Ap . transformExpressible)

transformExpressible :: Expressible -> [[Expressed]]
transformExpressible (Singleton v op n)           = transformExpressible (Difference v ZeroVariable op n)
transformExpressible (Difference v1 v2 (::<?) x)  = [[LessThanEqualTo v1 v2 (ceiling (x - 1))]]
transformExpressible (Difference v1 v2 (::<=?) x) = [[LessThanEqualTo v1 v2 (floor x)]]
transformExpressible (Difference v1 v2 (::>?) x)  = transformExpressible (Difference v2 v1 (::<?) (- x))
transformExpressible (Difference v1 v2 (::>=?) x) = transformExpressible (Difference v2 v1 (::<=?) (- x))
transformExpressible (Difference v1 v2 (::=?) x)  = transformExpressible (Difference v1 v2 (::<=?) x) ++ transformExpressible (Difference v2 v1 (::<=?) (- x))
transformExpressible (Difference v1 v2 (::<>?) x) =
  liftA2 (<>)
  (transformExpressible (Difference v1 v2 (::<?) x))
  (transformExpressible (Difference v2 v1 (::<?) (- x)))
