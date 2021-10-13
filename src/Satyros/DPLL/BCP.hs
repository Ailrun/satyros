module Satyros.DPLL.BCP where

import           Control.Lens            (use)
import qualified Satyros.CNF             as CNF
import           Satyros.DPLL.Assignment (getAssignedValue)
import           Satyros.DPLL.Effect     (DPLL, bcpConflict, bcpUnitClause)
import           Satyros.DPLL.Storage    (assignment, clauses)

bcp :: DPLL ()
bcp = use clauses >>= mapM_ bcpOfClause

bcpOfClause :: CNF.Clause -> DPLL ()
bcpOfClause c@(CNF.Clause ls') = do
  asgn <- use assignment
  case go asgn ls' of
    Nothing -> bcpConflict c
    Just Nothing -> pure ()
    Just (Just l) -> do
      bcpUnitClause c l
      bcp
  where
    go _    [] = Nothing
    go asgn (l:ls)
      | Just True <- v = Just Nothing
      | Just False <- v = fromLs
      | Nothing <- fromLs = Just (Just l)
      | otherwise = Just Nothing
      where
        v = getAssignedValue asgn l
        fromLs = go asgn ls
