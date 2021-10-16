module Satyros.DPLL.Decision where

import           Control.Lens             (use)
import qualified Data.Set                 as Set
import qualified Satyros.CNF              as CNF
import           Satyros.DPLL.Effect      (DPLL, decisionComplete,
                                           decisionResult)
import           Satyros.DPLL.Storage     (unassignedVariables)
import           Satyros.DPLL.StorageUtil (assignDecisionVariable)
import           System.Random.Stateful   (StateGenM (StateGenM), randomM,
                                           randomRM)

decision :: DPLL ()
decision = do
  xs <- use unassignedVariables
  if Set.null xs
    then decisionComplete
    else do
      i <- randomRM (0, Set.size xs - 1) StateGenM
      s <- randomM StateGenM
      decisionResult
        . CNF.Literal s
        $ Set.toList xs !! i

decisionResultHandler :: CNF.Literal -> DPLL ()
decisionResultHandler = assignDecisionVariable
