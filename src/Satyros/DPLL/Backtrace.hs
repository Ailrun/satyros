{-# LANGUAGE ViewPatterns #-}
module Satyros.DPLL.Backtrace where

import           Control.Lens             (each, to, (&), (^.), (^..))
import           Data.List                (find)
import           Data.Maybe               (fromJust)
import qualified Data.Set                 as Set
import qualified Satyros.CNF              as CNF
import           Satyros.DPLL.Effect      (DPLL, backtraceComplete,
                                           backtraceExhaustion)
import           Satyros.DPLL.StorageUtil (assignFailureDrivenVariable,
                                           dropIrrelevantLevels, dropLevel,
                                           learnClause, levelToSet)
import           System.Random.Stateful   (StateGenM (StateGenM), randomRM)

backtrace :: CNF.Clause -> DPLL s ()
backtrace cdc = do
  learnClause cdc
  dropIrrelevantLevels cdc
  mayLv <- dropLevel
  case mayLv of
    Just (levelToSet -> lvSet) -> do
      let
        revertibleXs =
          cdc
          ^.. CNF.literalsOfClause
          . each
          . to CNF.literalToVariable
          & Set.intersection lvSet
          . Set.fromList
      i <- randomRM (0, Set.size revertibleXs - 1) StateGenM
      let
        x = Set.elemAt i revertibleXs
        l = cdc ^. CNF.literalsOfClause & fromJust . find ((== x) . CNF.literalToVariable)
      backtraceComplete cdc l
    Nothing -> backtraceExhaustion

backtraceCompleteHandler :: CNF.Clause -> CNF.Literal -> DPLL s ()
backtraceCompleteHandler c l = assignFailureDrivenVariable l c
