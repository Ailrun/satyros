module DPLL where

import           Control.Lens             (_2, uses, (%~), (&), (^.))
import           Control.Monad.Trans.Free (FreeF (Free, Pure))
import           Data.Bifunctor           (first)
import           Data.Functor.Const       (Const (Const))
import qualified Data.Map                 as Map
import           Data.Void                (Void, absurd)
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

loop :: DPLL.Storage s -> (Either DPLLFailure [Bool], DPLL.Storage s)
loop = go (DPLL.bcp >> pure (Left (DPLLException "Post-BCP continuation should not be reachable")))
  where
    go d s =
      case DPLL.stepDPLL d s of
        (Free eff', s') -> go (naiveHandler eff') s'
        (Pure res, s')  -> (res, s')

-- |
-- Initialize DPLL database and resolve trivial error cases / unit clauses.
initialize :: CNF.Formula -> StdGen -> Either DPLLFailure (DPLL.Storage ())
initialize f stdGen = first convertFailure $ DPLL.initializeStorage f stdGen ()
  where
    convertFailure :: DPLL.StorageInitializationFailure -> DPLLFailure
    convertFailure DPLL.EmptyClause = DPLLUnsatisfiable "Empty clauses are detected. Is this really intended?"
    convertFailure DPLL.InitialConflict = DPLLUnsatisfiable "The initial constraint derives a conflict"

naiveHandler :: DPLLF (Const Void) (DPLL s (Const Void) (Either DPLLFailure [Bool])) -> DPLL s (Const Void) (Either DPLLFailure [Bool])
naiveHandler (DPLL.BCPUnitClause c l r) = DPLL.bcpUnitClauseHandler c l >> r
naiveHandler (DPLL.BCPConflict c r) = DPLL.bcpConflictRelSATHandler c >> r
naiveHandler (DPLL.BCPConflictDrivenClause c r) = DPLL.backtrace c >> r
naiveHandler (DPLL.DecisionResult l) = DPLL.decisionResultHandler l >> DPLL.bcp >> pure (Left (DPLLException "Post-BCP continuation should not be reachable"))
naiveHandler DPLL.DecisionComplete = uses DPLL.assignment $ Right . fmap fst . Map.elems . DPLL.getAssignment
naiveHandler DPLL.BacktraceExhaustion = pure . Left $ DPLLUnsatisfiable "Possibilities are exhausted"
naiveHandler (DPLL.BacktraceComplete c l) = DPLL.backtraceCompleteHandler c l >> DPLL.decision >> pure (Left (DPLLException "Post-decision continuation should not be reachable"))
naiveHandler (DPLL.InsideDPLL (Const x)) = absurd x
