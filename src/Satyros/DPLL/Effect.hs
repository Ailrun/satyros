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
  liftShowsPrec sp slp d =
    showsUnaryWith (liftShowsPrec sp slp) "DPLL" d
    . hoistFreeT (const $ Const "<stateful computation>")
    . runDPLL

instance (Show a) => Show (DPLL a) where
  showsPrec = showsPrec1

stepDPLL :: DPLL a -> Storage -> (FreeF DPLLF a (DPLL a), Storage)
stepDPLL d s = first (fmap DPLL) $ runState (runFreeT (runDPLL d)) s
{-# INLINE stepDPLL #-}

data DPLLF r
  = BCPUnitClause CNF.Clause CNF.Literal r
  | BCPConflict CNF.Clause r
  | BCPConflictDrivenClause CNF.Clause r
  | DecisionResult CNF.Literal
  | DecisionComplete
  | BacktraceExhaustion
  | BacktraceComplete CNF.Clause CNF.Literal
  deriving stock (Show, Functor)

instance Show1 DPLLF where
  liftShowsPrec sp _ d (BCPUnitClause c l r) = showsTernaryWith showsPrec showsPrec sp "BCPUnitClause" d c l r
  liftShowsPrec sp _ d (BCPConflict c r) = showsBinaryWith showsPrec sp "BCPConflict" d c r
  liftShowsPrec sp _ d (BCPConflictDrivenClause c r) = showsBinaryWith showsPrec sp "BCPConflictDrivenClause" d c r
  liftShowsPrec _  _ d (DecisionResult l) = showsUnaryWith showsPrec "DecisionResult" d l
  liftShowsPrec _  _ _ DecisionComplete = showString "DecisionComplete"
  liftShowsPrec _  _ _ BacktraceExhaustion = showString "BacktraceExhaustion"
  liftShowsPrec _  _ d (BacktraceComplete c l) = showsBinaryWith showsPrec showsPrec "BacktraceComplete" d c l

bcpUnitClause :: CNF.Clause -> CNF.Literal -> DPLL ()
bcpUnitClause c l = wrap . BCPUnitClause c l $ pure ()
{-# INLINE bcpUnitClause #-}

bcpConflict :: CNF.Clause -> DPLL ()
bcpConflict c = wrap . BCPConflict c $ pure ()
{-# INLINE bcpConflict #-}

bcpConflictDrivenClause :: CNF.Clause -> DPLL ()
bcpConflictDrivenClause c = wrap . BCPConflictDrivenClause c $ pure ()
{-# INLINE bcpConflictDrivenClause #-}

decisionResult :: CNF.Literal -> DPLL ()
decisionResult = wrap . DecisionResult
{-# INLINE decisionResult #-}

decisionComplete :: DPLL ()
decisionComplete = wrap DecisionComplete
{-# INLINE decisionComplete #-}

backtraceExhaustion :: DPLL ()
backtraceExhaustion = wrap BacktraceExhaustion
{-# INLINE backtraceExhaustion #-}

backtraceComplete :: CNF.Clause -> CNF.Literal -> DPLL ()
backtraceComplete c = wrap . BacktraceComplete c
{-# INLINE backtraceComplete #-}
