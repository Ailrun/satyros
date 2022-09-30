module Satyros.Handler.Naive where

import           Control.Lens           (_2, _3, _4, (.=))
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import           Satyros.BellmanFord    (BellmanFordF)
import qualified Satyros.BellmanFord    as BellmanFord
import           Satyros.DPLL           (DPLL, DPLLF)
import qualified Satyros.DPLL           as DPLL
import           Satyros.Handler.Common (commonHandler, liftBellmanFord,
                                         setBellmanFord)
import           Satyros.Handler.Type   (InternalStorage)

naiveHandler :: DPLLF BellmanFordF
                (DPLL InternalStorage BellmanFordF Bool)
             -> DPLL InternalStorage BellmanFordF Bool
naiveHandler DPLL.BCPComplete = DPLL.decision >> pure False
naiveHandler DPLL.DecisionComplete = do
  g <- setBellmanFord
  liftBellmanFord _3 $ BellmanFord.propagation g
  pure False
naiveHandler (DPLL.BacktraceComplete c l) = do
  DPLL.theory . _2 .= Map.empty
  DPLL.theory . _3 .= Map.empty
  DPLL.backtraceCompleteHandler c l
  DPLL.bcp
  pure False
naiveHandler (DPLL.InsideDPLL BellmanFord.NegativeCyclePass) = do
  DPLL.theory . _4 .= Set.empty
  pure True
naiveHandler eff = commonHandler eff
