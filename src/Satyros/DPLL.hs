{-# LANGUAGE ViewPatterns #-}
module Satyros.DPLL where

import           Control.Lens             (_2, each, uses, (%~), (&), (^.),
                                           (^..), (^?))
import           Control.Monad.Except     (throwError)
import           Control.Monad.Extra      (forM_, when)
import           Control.Monad.Trans.Free (FreeF (Free, Pure))
import           Data.List.Extra          (notNull, partition)
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Vector              as Vector
import qualified Satyros.CNF              as CNF
import qualified Satyros.DPLL.Assignment  as DPLL
import qualified Satyros.DPLL.BCP         as DPLL
import qualified Satyros.DPLL.Backtrace   as DPLL
import qualified Satyros.DPLL.Decision    as DPLL
import           Satyros.DPLL.Effect      (DPLL, DPLLF)
import qualified Satyros.DPLL.Effect      as DPLL
import qualified Satyros.DPLL.Storage     as DPLL
import           System.Random.Stateful   (StdGen, getStdRandom)

data DPLLFailure
  = DPLLException String
  | DPLLUnsatisfiable String

dpll :: CNF.Formula -> IO (Either DPLLFailure [Bool])
dpll = getStdRandom . dpll'

dpll' :: CNF.Formula -> StdGen -> (Either DPLLFailure [Bool], StdGen)
dpll' f stdGen =
  case initialize f stdGen of
    Left e  -> (Left e, stdGen)
    Right s -> loop s & _2 %~ (^. DPLL.stdGen)

loop :: DPLL.Storage -> (Either DPLLFailure [Bool], DPLL.Storage)
loop = go (DPLL.bcp >> pure (Left (DPLLException "Post-BCP continuation should not be reachable")))
  where
    go d s =
      case DPLL.stepDPLL d s of
        (Free eff', s') -> go (naiveHandler eff') s'
        (Pure res, s')  -> (res, s')

-- |
-- Initialize DPLL database and resolve trivial error cases / unit clauses.
initialize :: CNF.Formula -> StdGen -> Either DPLLFailure DPLL.Storage
initialize f stdGen = do
  when (notNull emptyCs) $
    throwError $ DPLLUnsatisfiable "Empty clauses are detected. Is this really intended?"
  forM_ initialAssignmentPairs $ \(x, fst -> b) ->
    when (_assignment ^? DPLL.valueOfVariable x == Just b) $
      throwError $ DPLLUnsatisfiable "The initial constraint derives a conflict"

  pure DPLL.Storage{..}
  where
    _unassignedVariables = allVariables Set.\\ initiallyAssignedVs
    _clauses = Vector.fromList nonunitCs
    _assignment = DPLL.Assignment $ Map.fromList initialAssignmentPairs
    _variableLevels = [(Nothing, initiallyAssignedVs)]
    _stdGen = stdGen

    allVariables = Set.fromList $ fmap CNF.Variable [1..mv]
    CNF.Variable mv = CNF.maxVariableInFormula f

    initiallyAssignedVs = Set.fromList $ fmap fst initialAssignmentPairs
    initialAssignmentPairs =
      [(v, (pos ^. CNF.isPositive, Nothing)) | CNF.Literal pos v <- cnfLits]
    cnfLits = unitCs ^.. each . CNF.literalsOfClause . each

    (unitCs, nonunitCs) = partition CNF.unitClause nonemptyCs
    (emptyCs, nonemptyCs) = partition CNF.emptyClause cs
    cs = f ^. CNF.clausesOfFormula

naiveHandler :: DPLLF (DPLL (Either DPLLFailure [Bool])) -> DPLL (Either DPLLFailure [Bool])
naiveHandler (DPLL.BCPUnitClause c l r) = DPLL.bcpUnitClauseHandler c l >> r
naiveHandler (DPLL.BCPConflict c r) = DPLL.bcpConflictRelSATHandler c >> r
naiveHandler (DPLL.BCPConflictDrivenClause c r) = DPLL.backtrace c >> r
naiveHandler (DPLL.DecisionResult l) = DPLL.decisionResultHandler l >> DPLL.bcp >> pure (Left (DPLLException "Post-BCP continuation should not be reachable"))
naiveHandler DPLL.DecisionComplete = uses DPLL.assignment $ Right . fmap fst . Map.elems . DPLL.getAssignment
naiveHandler DPLL.BacktraceExhaustion = pure . Left $ DPLLUnsatisfiable "Possibilities are exhausted"
naiveHandler (DPLL.BacktraceComplete c l) = DPLL.backtraceCompleteHandler c l >> DPLL.decision >> pure (Left (DPLLException "Post-decision continuation should not be reachable"))
