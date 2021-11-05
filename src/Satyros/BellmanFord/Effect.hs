module Satyros.BellmanFord.Effect where

import           Control.Monad.State.Strict  (MonadState, State, runState)
import           Control.Monad.Trans.Free    (FreeF, FreeT (runFreeT),
                                              MonadFree (wrap), hoistFreeT)
import           Data.Bifunctor              (first)
import           Data.Functor.Classes        (Show1 (liftShowsPrec),
                                              showsBinaryWith, showsPrec1,
                                              showsUnaryWith)
import           Data.Functor.Const          (Const (Const))
import           GHC.Generics                (Generic, Generic1)
import           Satyros.BellmanFord.Storage (IDLGraphVertex,
                                              PositiveInfiniteInt, Storage)
import qualified Satyros.QFIDL               as QFIDL
import           Satyros.Util                (showsTernaryWith)

newtype BellmanFord a = BellmanFord{ runBellmanFord :: FreeT BellmanFordF (State Storage) a }
  deriving stock (Generic, Generic1)
  deriving newtype (Functor, Applicative, Monad, MonadFree BellmanFordF, MonadState Storage)

instance Show1 BellmanFord where
  liftShowsPrec sp slp d =
    showsUnaryWith (liftShowsPrec sp slp) "BellmanFord" d
    . hoistFreeT (const $ Const "<stateful computation>")
    . runBellmanFord

instance (Show a) => Show (BellmanFord a) where
  showsPrec = showsPrec1

stepBellmanFord :: BellmanFord a -> Storage -> (FreeF BellmanFordF a (BellmanFord a), Storage)
stepBellmanFord d s = first (fmap BellmanFord) $ runState (runFreeT (runBellmanFord d)) s
{-# INLINE stepBellmanFord #-}

data BellmanFordF r
  = PropagationCheck (IDLGraphVertex, IDLGraphVertex) r
  | PropagationFindShorter IDLGraphVertex (IDLGraphVertex, PositiveInfiniteInt) r
  | PropagationNth Int r
  | PropagationEnd
  | NegativeCycleCheck (IDLGraphVertex, IDLGraphVertex) r
  | NegativeCycleFind [QFIDL.Expressed]
  | NegativeCyclePass
  deriving stock (Generic, Generic1, Show, Functor)

instance Show1 BellmanFordF where
  liftShowsPrec sp _ d (PropagationCheck vs r) = showsBinaryWith showsPrec sp "PropagationCheck" d vs r
  liftShowsPrec sp _ d (PropagationFindShorter v p r) = showsTernaryWith showsPrec showsPrec sp "PropagationFindShorter" d v p r
  liftShowsPrec sp _ d (PropagationNth n r) = showsBinaryWith showsPrec sp "PropagationNth" d n r
  liftShowsPrec _  _ _ PropagationEnd = showString "PropagationEnd"
  liftShowsPrec sp _ d (NegativeCycleCheck vs r) = showsBinaryWith showsPrec sp "NegativeCycleCheck" d vs r
  liftShowsPrec _  _ d (NegativeCycleFind path) = showsUnaryWith showsPrec "NegativeCycleFind" d path
  liftShowsPrec _  _ _ NegativeCyclePass = showString "NegativeCyclePass"

propagationCheck :: (IDLGraphVertex, IDLGraphVertex) -> BellmanFord ()
propagationCheck vs = wrap . PropagationCheck vs $ pure ()
{-# INLINE propagationCheck #-}

propagationFindShorter :: IDLGraphVertex -> (IDLGraphVertex, PositiveInfiniteInt) -> BellmanFord ()
propagationFindShorter v p = wrap . PropagationFindShorter v p $ pure ()
{-# INLINE propagationFindShorter #-}

propagationNth :: Int -> BellmanFord ()
propagationNth n = wrap . PropagationNth n $ pure ()
{-# INLINE propagationNth #-}

propagationEnd :: BellmanFord ()
propagationEnd = wrap PropagationEnd
{-# INLINE propagationEnd #-}

negativeCycleCheck :: (IDLGraphVertex, IDLGraphVertex) -> BellmanFord ()
negativeCycleCheck vs = wrap . NegativeCycleCheck vs $ pure ()
{-# INLINE negativeCycleCheck #-}

negativeCycleFind :: [QFIDL.Expressed] -> BellmanFord ()
negativeCycleFind = wrap . NegativeCycleFind
{-# INLINE negativeCycleFind #-}

negativeCyclePass :: BellmanFord ()
negativeCyclePass = wrap NegativeCyclePass
{-# INLINE negativeCyclePass #-}
