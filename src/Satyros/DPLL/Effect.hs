{-# LANGUAGE QuantifiedConstraints #-}
module Satyros.DPLL.Effect where

import           Control.Monad.State.Strict (MonadState, State, runState)
import           Control.Monad.Trans.Free   (FreeF, FreeT (FreeT, runFreeT),
                                             MonadFree (wrap), hoistFreeT)
import           Data.Bifunctor             (first)
import           Data.Functor.Classes       (Show1 (liftShowsPrec),
                                             showsBinaryWith, showsPrec1,
                                             showsUnaryWith)
import           Data.Functor.Const         (Const (Const))
import qualified Satyros.CNF                as CNF
import           Satyros.DPLL.Storage       (Storage)
import           Satyros.Util               (showsTernaryWith)

newtype DPLL s f a = DPLL{ runDPLL :: FreeT (DPLLF f) (State (Storage s)) a }
  deriving newtype (Functor, Applicative, Monad, MonadState (Storage s))

instance (Functor f) => MonadFree (DPLLF f) (DPLL s f) where
  wrap (BCPUnitClause c l r)         = DPLL . wrap $ BCPUnitClause c l $ runDPLL r
  wrap (BCPConflict c r)             = DPLL . wrap $ BCPConflict c $ runDPLL r
  wrap (BCPConflictDrivenClause c r) = DPLL . wrap $ BCPConflictDrivenClause c $ runDPLL r
  wrap BCPComplete                   = DPLL . wrap $ BCPComplete
  wrap (DecisionResult l)            = DPLL . wrap $ DecisionResult l
  wrap DecisionComplete              = DPLL . wrap $ DecisionComplete
  wrap BacktraceExhaustion           = DPLL . wrap $ BacktraceExhaustion
  wrap (BacktraceComplete c l)       = DPLL . wrap $ BacktraceComplete c l
  wrap (InsideDPLL inner)            = DPLL . wrap $ InsideDPLL $ fmap runDPLL inner
  {-# INLINE wrap #-}

instance (Show1 f, Functor f) => Show1 (DPLL s f) where
  liftShowsPrec sp slp d =
    showsUnaryWith (liftShowsPrec sp slp) "DPLL" d
    . hoistFreeT (const $ Const "<stateful computation>")
    . runDPLL
  {-# INLINE liftShowsPrec #-}

instance (Show1 f, Functor f, Show a) => Show (DPLL s f a) where
  showsPrec = showsPrec1
  {-# INLINE showsPrec #-}

stepDPLL :: (Functor f) => DPLL s f a -> Storage s -> (FreeF (DPLLF f) a (DPLL s f a), Storage s)
stepDPLL d s = first (fmap DPLL) $ runState (runFreeT (runDPLL d)) s
{-# INLINE stepDPLL #-}

data DPLLF f r
  = BCPUnitClause CNF.Clause CNF.Literal r
  | BCPConflict CNF.Clause r
  | BCPConflictDrivenClause CNF.Clause r
  | BCPComplete
  | DecisionResult CNF.Literal
  | DecisionComplete
  | BacktraceExhaustion
  | BacktraceComplete CNF.Clause CNF.Literal
  | InsideDPLL (f r)
  deriving stock (Show, Functor)

instance (Show1 f, Functor f) => Show1 (DPLLF f) where
  liftShowsPrec sp _   d (BCPUnitClause c l r) = showsTernaryWith showsPrec showsPrec sp "BCPUnitClause" d c l r
  liftShowsPrec sp _   d (BCPConflict c r) = showsBinaryWith showsPrec sp "BCPConflict" d c r
  liftShowsPrec sp _   d (BCPConflictDrivenClause c r) = showsBinaryWith showsPrec sp "BCPConflictDrivenClause" d c r
  liftShowsPrec _  _   _ BCPComplete = showString "BCPComplete"
  liftShowsPrec _  _   d (DecisionResult l) = showsUnaryWith showsPrec "DecisionResult" d l
  liftShowsPrec _  _   _ DecisionComplete = showString "DecisionComplete"
  liftShowsPrec _  _   _ BacktraceExhaustion = showString "BacktraceExhaustion"
  liftShowsPrec _  _   d (BacktraceComplete c l) = showsBinaryWith showsPrec showsPrec "BacktraceComplete" d c l
  liftShowsPrec sp lsp d (InsideDPLL inner) = showsUnaryWith (liftShowsPrec sp lsp) "InsideDPLL" d inner
  {-# INLINE liftShowsPrec #-}

bcpUnitClause :: (Functor f) => CNF.Clause -> CNF.Literal -> DPLL s f ()
bcpUnitClause c l = wrap . BCPUnitClause c l $ pure ()
{-# INLINE bcpUnitClause #-}

bcpConflict :: (Functor f) => CNF.Clause -> DPLL s f ()
bcpConflict c = wrap . BCPConflict c $ pure ()
{-# INLINE bcpConflict #-}

bcpConflictDrivenClause :: (Functor f) => CNF.Clause -> DPLL s f ()
bcpConflictDrivenClause c = wrap . BCPConflictDrivenClause c $ pure ()
{-# INLINE bcpConflictDrivenClause #-}

bcpComplete :: (Functor f) => DPLL s f ()
bcpComplete = wrap BCPComplete
{-# INLINE bcpComplete #-}

decisionResult :: (Functor f) => CNF.Literal -> DPLL s f ()
decisionResult = wrap . DecisionResult
{-# INLINE decisionResult #-}

decisionComplete :: (Functor f) => DPLL s f ()
decisionComplete = wrap DecisionComplete
{-# INLINE decisionComplete #-}

backtraceExhaustion :: (Functor f) => DPLL s f ()
backtraceExhaustion = wrap BacktraceExhaustion
{-# INLINE backtraceExhaustion #-}

backtraceComplete :: (Functor f) => CNF.Clause -> CNF.Literal -> DPLL s f ()
backtraceComplete c = wrap . BacktraceComplete c
{-# INLINE backtraceComplete #-}
