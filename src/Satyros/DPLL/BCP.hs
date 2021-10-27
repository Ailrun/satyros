module Satyros.DPLL.BCP
  ( bcp
  , bcpUnitClauseHandler
  , bcpConflictRelSATHandler
  ) where

import           Control.Lens             (use, (^?))
import           Control.Monad.Except     (runExceptT, throwError)
import           Control.Monad.Extra      (eitherM, forM_)
import qualified Satyros.CNF              as CNF
import           Satyros.DPLL.Assignment  (valueOfLiteral)
import           Satyros.DPLL.Effect      (DPLL, bcpConflict,
                                           bcpConflictDrivenClause,
                                           bcpUnitClause)
import           Satyros.DPLL.Storage     (assignment, clauses)
import           Satyros.DPLL.StorageUtil (assignImplicationVariable,
                                           deriveConflictClauseRelSAT)

bcp :: (Functor f) => DPLL s f ()
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

bcpUnitClauseHandler :: (Functor f) => CNF.Clause -> CNF.Literal -> DPLL s f ()
bcpUnitClauseHandler c l = assignImplicationVariable l c

bcpConflictRelSATHandler :: (Functor f) => CNF.Clause -> DPLL s f ()
bcpConflictRelSATHandler c = do
  cdc <- deriveConflictClauseRelSAT c
  bcpConflictDrivenClause cdc
