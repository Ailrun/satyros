module Satyros.Handler.Clever where

import           Control.Lens           (_1, _2, _3, _4, use, uses, view, (%~),
                                         (.=))
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import           Satyros.BellmanFord    (BellmanFordF)
import qualified Satyros.BellmanFord    as BellmanFord
import           Satyros.DPLL           (DPLL, DPLLF)
import qualified Satyros.DPLL           as DPLL
import           Satyros.Handler.Common (commonHandler, liftBellmanFord)
import           Satyros.Handler.Type   (InternalStorage)
import qualified Satyros.QFIDL          as QFIDL

cleverHandler :: DPLLF BellmanFordF
                 (DPLL InternalStorage BellmanFordF Bool)
              -> DPLL InternalStorage BellmanFordF Bool
cleverHandler DPLL.BCPComplete = do
  m <- use (DPLL.theory . _1)
  (g, w) <- uses DPLL.assignment $
    BellmanFord.initializeStorage . QFIDL.fromAssignment m . fmap (_2 %~ view _1) . Map.toAscList . DPLL.getAssignment
  DPLL.theory . _2 .= g
  DPLL.theory . _3 .= w
  liftBellmanFord _3 $ BellmanFord.propagation g
  pure False
cleverHandler DPLL.DecisionComplete = pure True
cleverHandler (DPLL.BacktraceComplete c l) = do
  DPLL.backtraceCompleteHandler c l
  m <- use (DPLL.theory . _1)
  (g, w) <- uses DPLL.assignment $
    BellmanFord.initializeStorage . QFIDL.fromAssignment m . fmap (_2 %~ view _1) . Map.toAscList . DPLL.getAssignment
  DPLL.theory . _2 .= g
  DPLL.theory . _3 .= w
  DPLL.bcp
  pure False
cleverHandler (DPLL.InsideDPLL BellmanFord.NegativeCyclePass) = do
  DPLL.theory . _4 .= Set.empty
  DPLL.decision
  pure False
cleverHandler eff = commonHandler eff
