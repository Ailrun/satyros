module Satyros.DPLL.BCP where

import           Control.Monad.State.Strict (MonadState, State, gets)
import           Control.Monad.Trans.Free   (FreeT, MonadFree (wrap))
import qualified Satyros.CNF                as CNF
import           Satyros.DPLL.Assignment    (getAssignedValue)
import           Satyros.DPLL.Storage       (Storage (assignment, clauses))

data BCPF r
  = BCPUnitClause CNF.Clause CNF.Literal r
  | BCPConflict CNF.Clause
  deriving stock (Show, Functor)

newtype BCP a = BCP{ runBCP :: FreeT BCPF (State Storage) a }
  deriving newtype (Functor, Applicative, Monad, MonadFree BCPF, MonadState Storage)

instance Show (BCP a) where
  showsPrec p _ = showParen (p > 10) $ showString "BCP <leftover>"

bcp :: BCP ()
bcp = gets clauses >>= mapM_ bcpOfClause

bcpOfClause :: CNF.Clause -> BCP ()
bcpOfClause c@(CNF.Clause ls') = do
  asgn <- gets assignment
  case go asgn ls' of
    Nothing -> wrap (BCPConflict c)
    Just Nothing -> pure ()
    Just (Just l) -> do
      wrap (BCPUnitClause c l bcp)
  where
    go _    [] = Nothing
    go asgn (l:ls)
      | Just True <- v = Just Nothing
      | Just False <- v = leftover
      | Nothing <- leftover = Just (Just l)
      | otherwise = Just Nothing
      where
        v = getAssignedValue asgn l
        leftover = go asgn ls
