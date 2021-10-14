module Satyros.DPLL.BCP
  ( bcp
  , bcpUnitClauseHandler
  , bcpConflictRelSATHandler
  ) where

import           Control.Lens             (use, (^.))
import           Control.Monad            (forM_)
import qualified Satyros.CNF              as CNF
import           Satyros.DPLL.Assignment  (valueOfLiteral)
import           Satyros.DPLL.Effect      (DPLL, bcpConflict,
                                           bcpConflictDrivenClause,
                                           bcpUnitClause)
import           Satyros.DPLL.Storage     (assignment, clauses)
import           Satyros.DPLL.StorageUtil (assignImplicationVariable,
                                           deriveConflictClauseRelSAT)

bcp :: DPLL ()
bcp = do
  cls <- use clauses
  forM_ cls $ \c@(CNF.Clause ls') -> do
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
      | Just False <- v = lsRes
      | Nothing <- lsRes = Just (Just l)
      | otherwise = Just Nothing
      where
        v = asgn ^. valueOfLiteral l
        lsRes = go asgn ls

bcpUnitClauseHandler :: CNF.Clause -> CNF.Literal -> DPLL ()
bcpUnitClauseHandler c l = assignImplicationVariable l c

bcpConflictRelSATHandler :: CNF.Clause -> DPLL ()
bcpConflictRelSATHandler c = do
  cdc <- deriveConflictClauseRelSAT c
  bcpConflictDrivenClause cdc
