module Satyros.DPLL.Effect where

import           Control.Monad.State.Strict (MonadState, State, runState)
import           Control.Monad.Trans.Free   (FreeF, FreeT (runFreeT),
                                             MonadFree (wrap), hoistFreeT)
import           Data.Bifunctor             (first)
import           Data.Functor.Classes       (Show1 (liftShowsPrec),
                                             showsBinaryWith, showsPrec1,
                                             showsUnaryWith)
import           Data.Functor.Const         (Const (Const))
import qualified Satyros.CNF                as CNF
import           Satyros.DPLL.Storage       (Storage)
import           Satyros.Util               (showsTernaryWith)

newtype DPLL a = DPLL{ runDPLL :: FreeT DPLLF (State Storage) a }
  deriving newtype (Functor, Applicative, Monad, MonadFree DPLLF, MonadState Storage)

instance Show1 DPLL where
  liftShowsPrec sp slp p =
    liftShowsPrec sp slp p
    . hoistFreeT (const $ Const "<stateful computation>")
    . runDPLL

instance (Show a) => Show (DPLL a) where
  showsPrec = showsPrec1

stepDPLL :: DPLL a -> Storage -> (FreeF DPLLF a (DPLL a), Storage)
stepDPLL b s = first (fmap DPLL) $ runState (runFreeT (runDPLL b)) s
{-# INLINE stepDPLL #-}

data DPLLF r
  = BCPUnitClause CNF.Clause CNF.Literal r
  | BCPConflict CNF.Clause r
  | DecisionResult CNF.Literal r
  | DecisionComplete r
  deriving stock (Show, Functor)

instance Show1 DPLLF where
  liftShowsPrec sp _ d (BCPUnitClause c l r) = showsTernaryWith showsPrec showsPrec sp "BCPUnitClause" d c l r
  liftShowsPrec sp _ d (BCPConflict c r) = showsBinaryWith showsPrec sp "BCPUnitClause" d c r
  liftShowsPrec sp _ d (DecisionResult l r) = showsBinaryWith showsPrec sp "DecisionResult" d l r
  liftShowsPrec sp _ d (DecisionComplete r) = showsUnaryWith sp "DecisionComplete" d r

bcpUnitClause :: CNF.Clause -> CNF.Literal -> DPLL ()
bcpUnitClause c l = wrap . BCPUnitClause c l $ pure ()
{-# INLINE bcpUnitClause #-}

bcpConflict :: CNF.Clause -> DPLL ()
bcpConflict c = wrap . BCPConflict c $ pure ()
{-# INLINE bcpConflict #-}

decisionResult :: CNF.Literal -> DPLL ()
decisionResult l = wrap . DecisionResult l $ pure ()
{-# INLINE decisionResult #-}

decisionComplete :: DPLL ()
decisionComplete = wrap . DecisionComplete $ pure ()
{-# INLINE decisionComplete #-}
