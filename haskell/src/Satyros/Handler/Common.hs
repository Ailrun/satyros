module Satyros.Handler.Common where

import           Control.Lens               (Lens', _1, _2, _3, _4, use, uses,
                                             view, (%~), (.=))
import           Control.Monad.State.Strict (runState, state)
import           Control.Monad.Trans.Free   (hoistFreeT, transFreeT)
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Satyros.BellmanFord        (BellmanFord, BellmanFordF,
                                             IDLGraph)
import qualified Satyros.BellmanFord        as BellmanFord
import qualified Satyros.CNF                as CNF
import           Satyros.DPLL               (DPLL, DPLLF)
import qualified Satyros.DPLL               as DPLL
import           Satyros.Handler.Type       (InternalStorage)
import qualified Satyros.QFIDL              as QFIDL

commonHandler :: DPLLF BellmanFordF
                (DPLL InternalStorage BellmanFordF Bool)
             -> DPLL InternalStorage BellmanFordF Bool
commonHandler (DPLL.BCPUnitClause c l r) = DPLL.bcpUnitClauseHandler c l >> r
commonHandler (DPLL.BCPConflict c r) = DPLL.bcpConflictRelSATHandler c >> r
commonHandler (DPLL.BCPConflictDrivenClause c r) = DPLL.backtrace c >> r
commonHandler (DPLL.DecisionResult l) = DPLL.decisionResultHandler l >> DPLL.bcp >> pure False
commonHandler DPLL.BacktraceExhaustion = pure False
commonHandler (DPLL.InsideDPLL (BellmanFord.PropagationCheck e r)) = do
  DPLL.theory . _4 .= Set.singleton e
  r
commonHandler (DPLL.InsideDPLL (BellmanFord.PropagationFindShorter _ _ r)) = r
commonHandler (DPLL.InsideDPLL (BellmanFord.PropagationNth _ r)) = do
  DPLL.theory . _4 .= Set.empty
  r
commonHandler (DPLL.InsideDPLL BellmanFord.PropagationEnd) = do
  DPLL.theory . _4 .= Set.empty
  g <- use (DPLL.theory . _2)
  liftBellmanFord _3 $ BellmanFord.negativeCycle g
  pure False
commonHandler (DPLL.InsideDPLL (BellmanFord.NegativeCycleCheck e r)) = do
  DPLL.theory . _4 .= Set.singleton e
  r
commonHandler (DPLL.InsideDPLL (BellmanFord.NegativeCycleFind c)) = do
  DPLL.theory . _4 .= Set.fromList (fmap (\(QFIDL.LessThanEqualTo v1 v2 _) -> (Just v1, Just v2)) c)
  m <- use (DPLL.theory . _1 . _2)
  DPLL.bcpConflictRelSATHandler $ CNF.Clause (fmap (m Map.!) c)
  pure False
commonHandler DPLL.BCPComplete = pure False
commonHandler DPLL.DecisionComplete = pure False
commonHandler (DPLL.BacktraceComplete _ _) = pure False
commonHandler (DPLL.InsideDPLL BellmanFord.NegativeCyclePass) = pure False

setBellmanFord :: DPLL InternalStorage BellmanFordF IDLGraph
setBellmanFord = do
  m <- use (DPLL.theory . _1)
  (g, w) <- uses DPLL.assignment $ BellmanFord.initializeStorage . QFIDL.fromAssignment m . fmap (_2 %~ view _1) . Map.toAscList . DPLL.getAssignment
  DPLL.theory . _2 .= g
  DPLL.theory . _3 .= w
  pure g

liftBellmanFord :: Lens' s BellmanFord.Storage -> BellmanFord a -> DPLL s BellmanFordF a
liftBellmanFord l =
  DPLL.DPLL
  . transFreeT DPLL.InsideDPLL
  . hoistFreeT (state . (DPLL.theory . l) . runState)
  . BellmanFord.runBellmanFord
