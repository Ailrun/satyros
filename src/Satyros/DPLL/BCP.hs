module Satyros.DPLL.BCP
  ( bcp
  , bcpUnitClauseHandler
  , bcpConflictRelSATHandler
  ) where

import           Control.Lens             (use, (^?))
import           Control.Monad.Except     (MonadError (throwError), runExceptT)
import           Control.Monad.Extra      (eitherM, forM_)
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
  eitherM id pure . runExceptT $
    forM_ cls $ \c@(CNF.Clause ls') -> do
      asgn <- use assignment
      case foldr (go asgn) Nothing ls' of
        Nothing -> throwError $ bcpConflict c
        Just Nothing -> pure ()
        Just (Just l) -> throwError $ do
          bcpUnitClause c l
          bcp
  where
    go asgn l rest
      | Just True <- v = Just Nothing
      | Just False <- v = rest
      | Nothing <- rest = Just (Just l)
      | otherwise = Just Nothing
      where
        v = asgn ^? valueOfLiteral l

bcpUnitClauseHandler :: CNF.Clause -> CNF.Literal -> DPLL ()
bcpUnitClauseHandler c l = assignImplicationVariable l c

bcpConflictRelSATHandler :: CNF.Clause -> DPLL ()
bcpConflictRelSATHandler c = do
  cdc <- deriveConflictClauseRelSAT c
  bcpConflictDrivenClause cdc
