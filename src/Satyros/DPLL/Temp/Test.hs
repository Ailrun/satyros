{- |
TODO!!!!
Convert this into a test case
-}
module Satyros.DPLL.Temp.Test where

import           Control.Monad.Trans.Free (FreeF (Free, Pure))
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Vector              as Vector
import qualified Satyros.CNF              as CNF
import           Satyros.DPLL.Assignment  (Assignment (Assignment))
import           Satyros.DPLL.BCP         (bcp, bcpConflictRelSATHandler,
                                           bcpUnitClauseHandler)
import           Satyros.DPLL.Backtrace   (backtrace, backtraceCompleteHandler)
import           Satyros.DPLL.Decision    (decision, decisionResultHandler)
import           Satyros.DPLL.Effect      (DPLL, DPLLF (..), stepDPLL)
import           Satyros.DPLL.Storage     (Storage (..))
import           System.Random            (mkStdGen)

test :: IO ()
test = go (bcp, testStorage)
  where
    go p = do
      mayP <- testLoop p
      case mayP of
        Just p' -> do
          putStr "DPLL:    "
          print p'
          putStrLn ""
          go p'
        Nothing -> putStrLn "finished"

testLoop :: (DPLL () (), Storage ()) -> IO (Maybe (DPLL () (), Storage ()))
testLoop (d0, s0) =
  case stepDPLL d0 s0 of
    (Free eff1, s1) -> do
      putStr "Effect:  "
      print eff1
      pure $ Just (testHandler eff1, s1)
    (Pure _, _)     -> pure Nothing

testHandler :: DPLLF (DPLL () ()) -> DPLL () ()
testHandler (BCPUnitClause c l r) = bcpUnitClauseHandler c l >> r
testHandler (BCPConflict c r) = bcpConflictRelSATHandler c >> r
testHandler (BCPConflictDrivenClause c r) = backtrace c >> r
testHandler (DecisionResult l) = decisionResultHandler l >> bcp
testHandler DecisionComplete = pure ()
testHandler BacktraceExhaustion = pure ()
testHandler (BacktraceComplete c l) = backtraceCompleteHandler c l >> decision

testStorage :: Storage ()
testStorage =
  Storage
  { _unassignedVariables = Set.fromList [CNF.Variable 4]
  , _clauses = Vector.fromList
               [ CNF.Clause
                 [CNF.Literal CNF.Positive (CNF.Variable 1), CNF.Literal CNF.Positive (CNF.Variable 2)]
               , CNF.Clause
                 [CNF.Literal CNF.Positive (CNF.Variable 1), CNF.Literal CNF.Positive (CNF.Variable 3), CNF.Literal CNF.Positive (CNF.Variable 4)]
               , CNF.Clause
                 [CNF.Literal CNF.Negative (CNF.Variable 2), CNF.Literal CNF.Negative (CNF.Variable 3), CNF.Literal CNF.Positive (CNF.Variable 4)]
               , CNF.Clause
                 [CNF.Literal CNF.Negative (CNF.Variable 2), CNF.Literal CNF.Negative (CNF.Variable 4)]
               ]
  , _assignment = Assignment (Map.fromList [(CNF.Variable 3, (True, Nothing)), (CNF.Variable 2, (True, Nothing)), (CNF.Variable 1, (False, Nothing))])
  , _variableLevels = [(Just (CNF.Variable 3), Set.empty), (Just (CNF.Variable 1), Set.singleton (CNF.Variable 2))]
  , _stdGen = mkStdGen 0
  , _theory = ()
  }
