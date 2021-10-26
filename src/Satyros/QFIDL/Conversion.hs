{-# LANGUAGE ViewPatterns #-}
module Satyros.QFIDL.Conversion
  ( toCNF
  , negateExpressed
  ) where

import           Control.Applicative       (liftA2)
import           Control.Lens              (_1, over, view)
import           Data.Coerce               (coerce)
import           Data.List                 (mapAccumL)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Monoid               (Ap (..), getAp)
import           Data.Tuple                (swap)
import qualified Satyros.CNF               as CNF
import           Satyros.QFIDL.Expressed   (Expressed (..))
import           Satyros.QFIDL.Expressible (Expressible (..), Operator (..))
import           Satyros.QFIDL.Variable    (Variable (Variable))

toCNF :: CNF.FormulaLike Expressible -> (CNF.Formula, Map CNF.Variable Expressed)
toCNF = coerce $ swap . toCNF'
  where
    toCNF' :: [CNF.ClauseLike Expressible] -> (Map CNF.Variable Expressed, [CNF.Clause])
    toCNF' =
      coerce
        ( over _1 (Map.fromList . view _1)
          . mapAccumL @[] (mapAccumL @[] go) ([], Map.empty, CNF.Variable <$> [1..])
        )
      . concatMap transformClauseLike

    go :: ([(CNF.Variable, Expressed)], Map Expressed CNF.Variable, [CNF.Variable])
       -> Expressed
       -> (([(CNF.Variable, Expressed)], Map Expressed CNF.Variable, [CNF.Variable]), CNF.Literal)
    go s@(ve, e2v, newVs) e
      | Just v <- Map.lookup e e2v = (s, CNF.Literal CNF.Positive v)
      | newV:newVs' <- newVs       = (((newV, e):ve, Map.insert e newV e2v, newVs'), CNF.Literal CNF.Positive newV)
      | otherwise                  = error "toCNF: impossible case!"

negateExpressed :: Expressed -> Expressed
negateExpressed (LessThanEqualTo v1 v2 n) = LessThanEqualTo v2 v1 (- n - 1)

transformClauseLike :: CNF.ClauseLike Expressible -> [CNF.ClauseLike Expressed]
transformClauseLike = coerce go
  where
    go :: [Expressible] -> [[Expressed]]
    go = getAp . mconcat . fmap (Ap . transformExpressible)

transformExpressible :: Expressible -> [[Expressed]]
transformExpressible (Singleton v op n)           = transformExpressible (Difference v (Variable 0) op n)
transformExpressible (Difference v1 v2 (::<?) x)  = [[LessThanEqualTo v1 v2 (ceiling (x - 1))]]
transformExpressible (Difference v1 v2 (::<=?) x) = [[LessThanEqualTo v1 v2 (floor x)]]
transformExpressible (Difference v1 v2 (::>?) x)  = transformExpressible (Difference v2 v1 (::<?) (- x))
transformExpressible (Difference v1 v2 (::>=?) x) = transformExpressible (Difference v2 v1 (::<=?) (- x))
transformExpressible (Difference v1 v2 (::=?) x)  = transformExpressible (Difference v1 v2 (::<=?) x) ++ transformExpressible (Difference v2 v1 (::<=?) (- x))
transformExpressible (Difference v1 v2 (::<>?) x) =
  liftA2 (<>)
  (transformExpressible (Difference v1 v2 (::<?) x))
  (transformExpressible (Difference v2 v1 (::<?) (- x)))
