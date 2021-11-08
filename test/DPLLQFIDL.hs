module DPLLQFIDL where

import           Control.Lens               (Lens', _1, _2, use, uses, view,
                                             (%~), (&), (.=), (^.))
import           Control.Monad.State.Strict (runState, state)
import           Control.Monad.Trans.Free   (FreeF (Free, Pure), hoistFreeT,
                                             transFreeT)
import           Data.Bifunctor             (first)
import           Data.Coerce                (coerce)
import qualified Data.Map                   as Map
import           Satyros.BellmanFord        (BellmanFord, BellmanFordF)
import qualified Satyros.BellmanFord        as BellmanFord
import qualified Satyros.CNF                as CNF
import           Satyros.DPLL               (DPLL, DPLLF)
import qualified Satyros.DPLL               as DPLL
import qualified Satyros.QFIDL              as QFIDL
import           System.Random              (StdGen, mkStdGen)

data DPLLQFIDLFailure
  = DPLLQFIDLException String
  | DPLLQFIDLUnsatisfiable String
  deriving stock (Show)

example :: CNF.FormulaLike QFIDL.Expressible
example = coerce
  [ [QFIDL.Difference (QFIDL.Variable 1) (QFIDL.Variable 2) (QFIDL.::<?) 0, QFIDL.Singleton (QFIDL.Variable 3) (QFIDL.::<>?) 1]
  , [QFIDL.Singleton (QFIDL.Variable 3) (QFIDL.::=?) 1, QFIDL.Difference (QFIDL.Variable 1) (QFIDL.Variable 2) (QFIDL.::<=?) 0]
  ]

testDpllqfidl :: CNF.FormulaLike QFIDL.Expressible -> Either DPLLQFIDLFailure [Int]
testDpllqfidl = fst . flip dpllqfidl (mkStdGen 2)

dpllqfidl :: CNF.FormulaLike QFIDL.Expressible
          -> StdGen
          -> (Either DPLLQFIDLFailure [Int], StdGen)
dpllqfidl f stdGen =
  case initialize f stdGen of
    Left e  -> (Left e, stdGen)
    Right s -> loop s & _2 %~ (^. DPLL.stdGen)

-- |
-- Initialize DPLL database and resolve trivial error cases / unit clauses.
initialize :: CNF.FormulaLike QFIDL.Expressible -> StdGen -> Either DPLLQFIDLFailure (DPLL.Storage (QFIDL.ConversionTable, BellmanFord.Storage))
initialize f stdGen = first convertFailure $ DPLL.initializeStorage cnf stdGen (mapping, Map.empty)
  where
    convertFailure :: DPLL.StorageInitializationFailure -> DPLLQFIDLFailure
    convertFailure DPLL.EmptyClause = DPLLQFIDLUnsatisfiable "Empty clauses are detected. Is this really intended?"
    convertFailure DPLL.InitialConflict = DPLLQFIDLUnsatisfiable "The initial constraint derives a conflict"

    (cnf, mapping) = QFIDL.toCNF f

loop :: DPLL.Storage (QFIDL.ConversionTable, BellmanFord.Storage)
     -> ( Either DPLLQFIDLFailure [Int]
        , DPLL.Storage (QFIDL.ConversionTable, BellmanFord.Storage)
        )
loop = go (DPLL.bcp >> pure (Left (DPLLQFIDLException "Post BCP continuation should not be reachable")))
  where
    go d s =
      case DPLL.stepDPLL d s of
        (Free eff', s') -> go (naiveHandler eff') s'
        (Pure res, s')  -> (res, s')

naiveHandler :: DPLLF BellmanFordF
                  (DPLL
                    (QFIDL.ConversionTable, BellmanFord.Storage)
                    BellmanFordF
                    (Either DPLLQFIDLFailure [Int]))
             -> DPLL
                  (QFIDL.ConversionTable, BellmanFord.Storage)
                  BellmanFordF
                  (Either DPLLQFIDLFailure [Int])
naiveHandler (DPLL.BCPUnitClause c l r) = DPLL.bcpUnitClauseHandler c l >> r
naiveHandler DPLL.BCPComplete = DPLL.decision >> pure (Left (DPLLQFIDLException "Post decision continuation should not be reachable"))
naiveHandler (DPLL.BCPConflict c r) = DPLL.bcpConflictRelSATHandler c >> r
naiveHandler (DPLL.BCPConflictDrivenClause c r) = DPLL.backtrace c >> r
naiveHandler (DPLL.DecisionResult l) = DPLL.decisionResultHandler l >> DPLL.bcp >> pure (Left (DPLLQFIDLException "Post BCP continuation should not be reachable"))
naiveHandler DPLL.DecisionComplete = do
  m <- use (DPLL.theory . _1)
  (g, w) <- uses DPLL.assignment $
    BellmanFord.initializeStorage . QFIDL.fromAssignment m . fmap (_2 %~ view _1) . Map.toAscList . DPLL.getAssignment
  DPLL.theory . _2 .= w
  liftBellmanFord _2 $ BellmanFord.propagation g
  pure (Left (DPLLQFIDLException "Post Bellman-Ford propagation continuation should not be reachable"))
naiveHandler DPLL.BacktraceExhaustion = pure . Left $ DPLLQFIDLUnsatisfiable "Possibilities are exhausted"
naiveHandler (DPLL.BacktraceComplete c l) = DPLL.backtraceCompleteHandler c l >> DPLL.decision >> pure (Left (DPLLQFIDLException "Post decision continuation should not be reachable"))
naiveHandler (DPLL.InsideDPLL (BellmanFord.PropagationCheck _ r)) = r
naiveHandler (DPLL.InsideDPLL (BellmanFord.PropagationFindShorter _ _ r)) = r
naiveHandler (DPLL.InsideDPLL (BellmanFord.PropagationNth _ r)) = r
naiveHandler (DPLL.InsideDPLL BellmanFord.PropagationEnd) = do
  m <- use (DPLL.theory . _1)
  (g, _) <- uses DPLL.assignment $
    BellmanFord.initializeStorage . QFIDL.fromAssignment m . fmap (_2 %~ view _1) . Map.toAscList . DPLL.getAssignment
  liftBellmanFord _2 $ BellmanFord.negativeCycle g
  pure (Left (DPLLQFIDLException "Post Bellman-Ford negative cycle continuation should not be reachable"))
naiveHandler (DPLL.InsideDPLL (BellmanFord.NegativeCycleCheck _ r)) = r
naiveHandler (DPLL.InsideDPLL (BellmanFord.NegativeCycleFind c)) = do
  m <- use (DPLL.theory . _1 . _2)
  DPLL.bcpConflictRelSATHandler $ CNF.Clause (fmap (m Map.!) c)
  pure (Left (DPLLQFIDLException "Post-BCP conflict handle continuation should not be reachable"))
naiveHandler (DPLL.InsideDPLL BellmanFord.NegativeCyclePass) = uses (DPLL.theory . _2) (Right . BellmanFord.storageToValues)

liftBellmanFord :: Lens' s BellmanFord.Storage -> BellmanFord a -> DPLL s BellmanFordF a
liftBellmanFord l =
  DPLL.DPLL
  . transFreeT DPLL.InsideDPLL
  . hoistFreeT (state . (DPLL.theory . l) . runState)
  . BellmanFord.runBellmanFord
