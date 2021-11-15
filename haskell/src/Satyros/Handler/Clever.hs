module Satyros.Handler.Clever where

import           Control.Lens               (Lens', _1, _2, _3, _4, use, uses,
                                             view, (%~), (.=))
import           Control.Monad.State.Strict (runState, state)
import           Control.Monad.Trans.Free   (hoistFreeT, transFreeT)
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Satyros.BellmanFord        (BellmanFord, BellmanFordF)
import qualified Satyros.BellmanFord        as BellmanFord
import qualified Satyros.CNF                as CNF
import           Satyros.DPLL               (DPLL, DPLLF)
import qualified Satyros.DPLL               as DPLL
import           Satyros.Handler.Type       (InternalStorage)
import qualified Satyros.QFIDL              as QFIDL

cleverHandler :: DPLLF BellmanFordF
                (DPLL InternalStorage BellmanFordF Bool)
             -> DPLL InternalStorage BellmanFordF Bool
cleverHandler (DPLL.BCPUnitClause c l r) = DPLL.bcpUnitClauseHandler c l >> r
cleverHandler DPLL.BCPComplete = do
  m <- use (DPLL.theory . _1)
  (g, w) <- uses DPLL.assignment $
    BellmanFord.initializeStorage . QFIDL.fromAssignment m . fmap (_2 %~ view _1) . Map.toAscList . DPLL.getAssignment
  DPLL.theory . _2 .= g
  DPLL.theory . _3 .= w
  liftBellmanFord _3 $ BellmanFord.propagation g
  pure False
cleverHandler (DPLL.BCPConflict c r) = DPLL.bcpConflictRelSATHandler c >> r
cleverHandler (DPLL.BCPConflictDrivenClause c r) = DPLL.backtrace c >> r
cleverHandler (DPLL.DecisionResult l) = DPLL.decisionResultHandler l >> DPLL.bcp >> pure False
cleverHandler DPLL.DecisionComplete = pure True
cleverHandler DPLL.BacktraceExhaustion = pure False
cleverHandler (DPLL.BacktraceComplete c l) = do
  DPLL.backtraceCompleteHandler c l
  m <- use (DPLL.theory . _1)
  (g, w) <- uses DPLL.assignment $
    BellmanFord.initializeStorage . QFIDL.fromAssignment m . fmap (_2 %~ view _1) . Map.toAscList . DPLL.getAssignment
  DPLL.theory . _2 .= g
  DPLL.theory . _3 .= w
  DPLL.bcp
  pure False
cleverHandler (DPLL.InsideDPLL (BellmanFord.PropagationCheck e r)) = do
  DPLL.theory . _4 .= Set.singleton e
  r
cleverHandler (DPLL.InsideDPLL (BellmanFord.PropagationFindShorter _ _ r)) = r
cleverHandler (DPLL.InsideDPLL (BellmanFord.PropagationNth _ r)) = do
  DPLL.theory . _4 .= Set.empty
  r
cleverHandler (DPLL.InsideDPLL BellmanFord.PropagationEnd) = do
  DPLL.theory . _4 .= Set.empty
  g <- use (DPLL.theory . _2)
  liftBellmanFord _3 $ BellmanFord.negativeCycle g
  pure False
cleverHandler (DPLL.InsideDPLL (BellmanFord.NegativeCycleCheck e r)) = do
  DPLL.theory . _4 .= Set.singleton e
  r
cleverHandler (DPLL.InsideDPLL (BellmanFord.NegativeCycleFind c)) = do
  DPLL.theory . _4 .= Set.fromList (fmap (\(QFIDL.LessThanEqualTo v1 v2 _) -> (Just v1, Just v2)) c)
  m <- use (DPLL.theory . _1 . _2)
  DPLL.bcpConflictRelSATHandler $ CNF.Clause (fmap (m Map.!) c)
  pure False
cleverHandler (DPLL.InsideDPLL BellmanFord.NegativeCyclePass) = do
  DPLL.theory . _4 .= Set.empty
  DPLL.decision
  pure False

liftBellmanFord :: Lens' s BellmanFord.Storage -> BellmanFord a -> DPLL s BellmanFordF a
liftBellmanFord l =
  DPLL.DPLL
  . transFreeT DPLL.InsideDPLL
  . hoistFreeT (state . (DPLL.theory . l) . runState)
  . BellmanFord.runBellmanFord
