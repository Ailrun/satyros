{-# LANGUAGE ViewPatterns #-}
module DPLLQFIDL where

import           Control.Lens                      (_1, _2, each, from, use,
                                                    uses, view, (%~), (&), (.=),
                                                    (^.), (^..), (^?))
import           Control.Monad                     (forM_, when)
import           Control.Monad.Except              (throwError)
import           Control.Monad.State.Strict        (runState, state)
import           Control.Monad.Trans.Free          (FreeF (Free, Pure),
                                                    hoistFreeT, transFreeT)
import           Data.Coerce                       (coerce)
import           Data.List.Extra                   (notNull, partition)
import qualified Data.Map                          as Map
import           Data.Maybe                        (mapMaybe)
import qualified Data.Set                          as Set
import           Data.Tuple                        (swap)
import qualified Data.Vector                       as Vector
import           Debug.Trace                       (trace)
import           Satyros.BellmanFord.Effect        (BellmanFordF,
                                                    BellmanFordStore)
import qualified Satyros.BellmanFord.Effect        as BellmanFord
import qualified Satyros.BellmanFord.IDLGraph      as BellmanFord
import qualified Satyros.BellmanFord.NegativeCycle as BellmanFord
import           Satyros.BellmanFord.Propagation   as BellmanFord
import qualified Satyros.CNF                       as CNF
import qualified Satyros.DPLL.Assignment           as DPLL
import qualified Satyros.DPLL.BCP                  as DPLL
import qualified Satyros.DPLL.Backtrace            as DPLL
import qualified Satyros.DPLL.Decision             as DPLL
import           Satyros.DPLL.Effect               (DPLL, DPLLF)
import qualified Satyros.DPLL.Effect               as DPLL
import qualified Satyros.DPLL.Storage              as DPLL
import qualified Satyros.QFIDL                     as QFIDL
import           System.Random                     (StdGen, mkStdGen)

data DPLLQFIDLFailure
  = DPLLQFIDLException String
  | DPLLQFIDLUnsatisfiable String
  deriving stock (Show)

example :: CNF.FormulaLike QFIDL.Expressible
example = coerce
  [ [QFIDL.Singleton (QFIDL.Variable 1) (QFIDL.::<?) 5]
  , [QFIDL.Singleton (QFIDL.Variable 1) (QFIDL.::>?) 3]
  , [QFIDL.Singleton (QFIDL.Variable 2) (QFIDL.::>?) 5]
  , [QFIDL.Difference (QFIDL.Variable 2) (QFIDL.Variable 1) (QFIDL.::<?) 2, QFIDL.Difference (QFIDL.Variable 1) (QFIDL.Variable 2) (QFIDL.::>?) (- 3)]
  ]

testDpllqfidl :: CNF.FormulaLike QFIDL.Expressible -> Either DPLLQFIDLFailure [Int]
testDpllqfidl = fst . flip dpllqfidl (mkStdGen 0)

dpllqfidl :: CNF.FormulaLike QFIDL.Expressible
          -> StdGen
          -> (Either DPLLQFIDLFailure [Int], StdGen)
dpllqfidl f stdGen =
  case initialize f stdGen of
    Left e  -> (Left e, stdGen)
    Right s -> loop s & _2 %~ (^. DPLL.stdGen)

initialize :: CNF.FormulaLike QFIDL.Expressible
           -> StdGen
           -> Either DPLLQFIDLFailure (DPLL.Storage (QFIDL.ConversionTable, BellmanFordStore))
initialize f stdGen = do
  when (notNull emptyCs) $
    throwError $ DPLLQFIDLUnsatisfiable "Empty clauses are detected. Is this really intended?"
  forM_ initialAssignmentPairs $ \(x, not . fst -> b) ->
    when (_assignment ^? DPLL.valueOfVariable x == Just b) $
      throwError $ DPLLQFIDLUnsatisfiable "The initial constraint derives a conflict"

  pure DPLL.Storage{..}
  where
    _unassignedVariables = allVariables Set.\\ initiallyAssignedVs
    _clauses = Vector.fromList nonunitCs
    _assignment = DPLL.Assignment $ Map.fromList initialAssignmentPairs
    _variableLevels = [(Nothing, initiallyAssignedVs)]
    _stdGen = stdGen
    _theory = (mapping, Map.empty)

    allVariables = Set.fromList $ fmap CNF.Variable [1..mv]
    CNF.Variable mv = CNF.maxVariableInFormula cnf

    initiallyAssignedVs = Set.fromList $ fmap fst initialAssignmentPairs
    initialAssignmentPairs =
      [(v, (pos ^. CNF.isPositive, Nothing)) | CNF.Literal pos v <- cnfLits]
    cnfLits = unitCs ^.. each . CNF.literalsOfClause . each

    (unitCs, nonunitCs) = partition CNF.unitClause nonemptyCs
    (emptyCs, nonemptyCs) = partition CNF.emptyClause cs
    cs = cnf ^. CNF.clausesOfFormula

    (cnf, mapping) = QFIDL.toCNF f

loop :: DPLL.Storage (QFIDL.ConversionTable, BellmanFordStore)
     -> ( Either DPLLQFIDLFailure [Int]
        , DPLL.Storage (QFIDL.ConversionTable, BellmanFordStore)
        )
loop = go (DPLL.bcp >> DPLL.decision >> pure (Left (DPLLQFIDLException "Post decision continuation should not be reachable")))
  where
    go d s =
      case DPLL.stepDPLL d s of
        (Free eff', s')
          | trace (show eff' <> "       " <> show s') False -> undefined
          | otherwise -> go (naiveHandler eff') s'
        (Pure res, s')  -> (res, s')

naiveHandler :: DPLLF BellmanFordF
                  (DPLL
                    (QFIDL.ConversionTable, BellmanFordStore)
                    BellmanFordF
                    (Either DPLLQFIDLFailure [Int]))
             -> DPLL
                  (QFIDL.ConversionTable, BellmanFordStore)
                  BellmanFordF
                  (Either DPLLQFIDLFailure [Int])
naiveHandler (DPLL.BCPUnitClause c l r) = DPLL.bcpUnitClauseHandler c l >> r
naiveHandler (DPLL.BCPConflict c r) = DPLL.bcpConflictRelSATHandler c >> r
naiveHandler (DPLL.BCPConflictDrivenClause c r) = DPLL.backtrace c >> r
naiveHandler (DPLL.DecisionResult l) = DPLL.decisionResultHandler l >> DPLL.bcp >> DPLL.decision >> pure (Left (DPLLQFIDLException "Post decision continuation should not be reachable"))
naiveHandler DPLL.DecisionComplete = do
  m <- use (DPLL.theory . _1 . _1)
  (g, w) <- uses DPLL.assignment $
    BellmanFord.initializeIDL . fmap (QFIDL.fromAssignment m . uncurry CNF.Literal . swap . (_2 %~ view (_1 . from CNF.isPositive))) . Map.toAscList . DPLL.getAssignment
  DPLL.theory . _2 .= w
  DPLL.DPLL . transFreeT DPLL.InsideDPLL . hoistFreeT (state . (DPLL.theory . _2) . runState) . BellmanFord.runBellmanFord $ BellmanFord.propagation g
  pure (Left (DPLLQFIDLException "Post Bellman-Ford propagation continuation should not be reachable"))
naiveHandler DPLL.BacktraceExhaustion = pure . Left $ DPLLQFIDLUnsatisfiable "Possibilities are exhausted"
naiveHandler (DPLL.BacktraceComplete c l) = DPLL.backtraceCompleteHandler c l >> DPLL.decision >> pure (Left (DPLLQFIDLException "Post decision continuation should not be reachable"))
naiveHandler (DPLL.InsideDPLL (BellmanFord.PropagationCheck _ r)) = r
naiveHandler (DPLL.InsideDPLL (BellmanFord.PropagationFindShorter _ _ r)) = r
naiveHandler (DPLL.InsideDPLL (BellmanFord.PropagationNth _ r)) = r
naiveHandler (DPLL.InsideDPLL BellmanFord.PropagationEnd) = do
  m <- use (DPLL.theory . _1 . _1)
  (g, _) <- uses DPLL.assignment $
    BellmanFord.initializeIDL . fmap (QFIDL.fromAssignment m . uncurry CNF.Literal . swap . (_2 %~ view (_1 . from CNF.isPositive))) . Map.toAscList . DPLL.getAssignment
  DPLL.DPLL . transFreeT DPLL.InsideDPLL . hoistFreeT (state . (DPLL.theory . _2) . runState) . BellmanFord.runBellmanFord $ BellmanFord.negativeCycle g
  pure (Left (DPLLQFIDLException "Post Bellman-Ford negative cycle continuation should not be reachable"))
naiveHandler (DPLL.InsideDPLL (BellmanFord.NegativeCycleCheck _ r)) = r
naiveHandler (DPLL.InsideDPLL (BellmanFord.NegativeCycleFind c)) = do
  m <- use (DPLL.theory . _1 . _2)
  DPLL.bcpConflictRelSATHandler $ CNF.Clause (fmap (m Map.!) c)
  pure (Left (DPLLQFIDLException "Post-BCP conflict handle continuation should not be reachable"))
naiveHandler (DPLL.InsideDPLL BellmanFord.NegativeCyclePass) = uses (DPLL.theory . _2) (Right . mapMaybe (\x -> fst x >> negate <$> BellmanFord.toInt (snd (snd x))) . Map.toAscList)
