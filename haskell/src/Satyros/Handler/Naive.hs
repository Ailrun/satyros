module Satyros.Handler.Naive where

import           Control.Lens               (Lens', _1, _2, _3, use, uses, view,
                                             (%~), (.=))
import           Control.Monad.State.Strict (runState, state)
import           Control.Monad.Trans.Free   (hoistFreeT, transFreeT)
import qualified Data.Map                   as Map
import           Satyros.BellmanFord        (BellmanFord, BellmanFordF)
import qualified Satyros.BellmanFord        as BellmanFord
import qualified Satyros.CNF                as CNF
import           Satyros.DPLL               (DPLL, DPLLF)
import qualified Satyros.DPLL               as DPLL
import qualified Satyros.QFIDL              as QFIDL

type InternalStorage = (QFIDL.ConversionTable, BellmanFord.IDLGraph, BellmanFord.Storage)
type Storage = DPLL.Storage InternalStorage

naiveHandler :: DPLLF BellmanFordF
                (DPLL InternalStorage BellmanFordF Bool)
             -> DPLL InternalStorage BellmanFordF Bool
naiveHandler (DPLL.BCPUnitClause c l r) = DPLL.bcpUnitClauseHandler c l >> r
naiveHandler DPLL.BCPComplete = DPLL.decision >> pure False
naiveHandler (DPLL.BCPConflict c r) = DPLL.bcpConflictRelSATHandler c >> r
naiveHandler (DPLL.BCPConflictDrivenClause c r) = DPLL.backtrace c >> r
naiveHandler (DPLL.DecisionResult l) = DPLL.decisionResultHandler l >> DPLL.bcp >> pure False
naiveHandler DPLL.DecisionComplete = do
  m <- use (DPLL.theory . _1)
  (g, w) <- uses DPLL.assignment $
    BellmanFord.initializeStorage . QFIDL.fromAssignment m . fmap (_2 %~ view _1) . Map.toAscList . DPLL.getAssignment
  DPLL.theory . _3 .= w
  liftBellmanFord _3 $ BellmanFord.propagation g
  pure False
naiveHandler DPLL.BacktraceExhaustion = pure False
naiveHandler (DPLL.BacktraceComplete c l) = DPLL.backtraceCompleteHandler c l >> DPLL.decision >> pure False
naiveHandler (DPLL.InsideDPLL (BellmanFord.PropagationCheck _ r)) = r
naiveHandler (DPLL.InsideDPLL (BellmanFord.PropagationFindShorter _ _ r)) = r
naiveHandler (DPLL.InsideDPLL (BellmanFord.PropagationNth _ r)) = r
naiveHandler (DPLL.InsideDPLL BellmanFord.PropagationEnd) = do
  m <- use (DPLL.theory . _1)
  (g, _) <- uses DPLL.assignment $
    BellmanFord.initializeStorage . QFIDL.fromAssignment m . fmap (_2 %~ view _1) . Map.toAscList . DPLL.getAssignment
  liftBellmanFord _3 $ BellmanFord.negativeCycle g
  pure False
naiveHandler (DPLL.InsideDPLL (BellmanFord.NegativeCycleCheck _ r)) = r
naiveHandler (DPLL.InsideDPLL (BellmanFord.NegativeCycleFind c)) = do
  m <- use (DPLL.theory . _1 . _2)
  DPLL.bcpConflictRelSATHandler $ CNF.Clause (fmap (m Map.!) c)
  pure False
naiveHandler (DPLL.InsideDPLL BellmanFord.NegativeCyclePass) = pure True

liftBellmanFord :: Lens' s BellmanFord.Storage -> BellmanFord a -> DPLL s BellmanFordF a
liftBellmanFord l =
  DPLL.DPLL
  . transFreeT DPLL.InsideDPLL
  . hoistFreeT (state . (DPLL.theory . l) . runState)
  . BellmanFord.runBellmanFord
