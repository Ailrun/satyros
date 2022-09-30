module Satyros.Handler.Clever where

import           Control.Lens           (_3, _4, (.=))
import qualified Data.Set               as Set
import           Satyros.BellmanFord    (BellmanFordF)
import qualified Satyros.BellmanFord    as BellmanFord
import           Satyros.DPLL           (DPLL, DPLLF)
import qualified Satyros.DPLL           as DPLL
import           Satyros.Handler.Common (commonHandler, liftBellmanFord,
                                         setBellmanFord)
import           Satyros.Handler.Type   (InternalStorage)


cleverHandler :: DPLLF BellmanFordF
                 (DPLL InternalStorage BellmanFordF Bool)
              -> DPLL InternalStorage BellmanFordF Bool
cleverHandler DPLL.BCPComplete = do
  g <- setBellmanFord
  liftBellmanFord _3 $ BellmanFord.propagation g
  pure False
cleverHandler DPLL.DecisionComplete = pure True
cleverHandler (DPLL.BacktraceComplete c l) = do
  DPLL.backtraceCompleteHandler c l
  _ <- setBellmanFord
  DPLL.bcp
  pure False
cleverHandler (DPLL.InsideDPLL BellmanFord.NegativeCyclePass) = do
  DPLL.theory . _4 .= Set.empty
  DPLL.decision
  pure False
cleverHandler eff = commonHandler eff
