module Satyros.DPLL.Decision where

import           Control.Lens           (use, (.=))
import qualified Data.IntSet            as IntSet
import qualified Satyros.CNF            as CNF
import           Satyros.DPLL.Effect    (DPLL, decisionResult, decisionComplete)
import           Satyros.DPLL.Storage   (unsetVariables)
import           Satyros.Util           (intToWord)
import           System.Random.Stateful (StateGenM (StateGenM), randomM,
                                         randomRM)

decision :: DPLL ()
decision = do
  uvs <- use unsetVariables
  if IntSet.null uvs
    then decisionComplete
    else do
      let
        lenUvs = IntSet.size uvs
      i <- randomRM (0, lenUvs - 1) StateGenM
      let
        v = IntSet.toList uvs !! i
        uvs' = IntSet.delete v uvs
      unsetVariables .= uvs'
      s <- randomM StateGenM
      decisionResult (CNF.Literal s (CNF.Variable (intToWord v)))
