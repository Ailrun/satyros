module Satyros.BellmanFord.Effect where

import           Control.Monad.State          (MonadState, State, runState)
import           Control.Monad.Trans.Free     (FreeF, FreeT (runFreeT),
                                               MonadFree (wrap), hoistFreeT)
import           Data.Bifunctor               (first)
import           Data.Functor.Classes         (Show1 (liftShowsPrec),
                                               showsBinaryWith, showsPrec1,
                                               showsUnaryWith)
import           Data.Functor.Const           (Const (Const))
import           Satyros.BellmanFord.IDLGraph (IDLGraphVertex,
                                               IDLWeightMap,
                                               PositiveInfiniteInt)
import           Satyros.Util                 (showsTernaryWith)

type BellmanFordStore = IDLWeightMap
newtype BellmanFord a = BellmanFord{ runBellmanFord :: FreeT BellmanFordF (State BellmanFordStore) a }
  deriving newtype (Functor, Applicative, Monad, MonadFree BellmanFordF, MonadState BellmanFordStore)

instance Show1 BellmanFord where
  liftShowsPrec sp slp d =
    showsUnaryWith (liftShowsPrec sp slp) "BellmanFord" d
    . hoistFreeT (const $ Const "<stateful computation>")
    . runBellmanFord

instance (Show a) => Show (BellmanFord a) where
  showsPrec = showsPrec1

stepBellmanFord :: BellmanFord a -> BellmanFordStore -> (FreeF BellmanFordF a (BellmanFord a), BellmanFordStore)
stepBellmanFord d s = first (fmap BellmanFord) $ runState (runFreeT (runBellmanFord d)) s
{-# INLINE stepBellmanFord #-}

data BellmanFordF r
  = LoopCheck (IDLGraphVertex, IDLGraphVertex) r
  | LoopFindShorter IDLGraphVertex (IDLGraphVertex, PositiveInfiniteInt) r
  | LoopNth Int r
  | LoopEnd
  | NegativeCycleCheck (IDLGraphVertex, IDLGraphVertex) r
  | NegativeCycleFind [(IDLGraphVertex, IDLGraphVertex, Int)]
  | NegativeCyclePass
  deriving stock (Show, Functor)

instance Show1 BellmanFordF where
  liftShowsPrec sp _ d (LoopCheck vs r) = showsBinaryWith showsPrec sp "LoopCheck" d vs r
  liftShowsPrec sp _ d (LoopFindShorter v p r) = showsTernaryWith showsPrec showsPrec sp "LoopFindShorter" d v p r
  liftShowsPrec sp _ d (LoopNth n r) = showsBinaryWith showsPrec sp "LoopNth" d n r
  liftShowsPrec _  _ _ LoopEnd = showString "LoopEnd"
  liftShowsPrec sp _ d (NegativeCycleCheck vs r) = showsBinaryWith showsPrec sp "NegativeCycleCheck" d vs r
  liftShowsPrec _  _ d (NegativeCycleFind path) = showsUnaryWith showsPrec "NegativeCycleFind" d path
  liftShowsPrec _  _ _ NegativeCyclePass = showString "NegativeCyclePass"

loopCheck :: (IDLGraphVertex, IDLGraphVertex) -> BellmanFord ()
loopCheck vs = wrap . LoopCheck vs $ pure ()
{-# INLINE loopCheck #-}

loopFindShorter :: IDLGraphVertex -> (IDLGraphVertex, PositiveInfiniteInt) -> BellmanFord ()
loopFindShorter v p = wrap . LoopFindShorter v p $ pure ()
{-# INLINE loopFindShorter #-}

loopNth :: Int -> BellmanFord ()
loopNth n = wrap . LoopNth n $ pure ()
{-# INLINE loopNth #-}

loopEnd :: BellmanFord ()
loopEnd = wrap LoopEnd
{-# INLINE loopEnd #-}

negativeCycleCheck :: (IDLGraphVertex, IDLGraphVertex) -> BellmanFord ()
negativeCycleCheck vs = wrap . NegativeCycleCheck vs $ pure ()
{-# INLINE negativeCycleCheck #-}

negativeCycleFind :: [(IDLGraphVertex, IDLGraphVertex, Int)] -> BellmanFord ()
negativeCycleFind = wrap . NegativeCycleFind
{-# INLINE negativeCycleFind #-}

negativeCyclePass :: BellmanFord ()
negativeCyclePass = wrap NegativeCyclePass
{-# INLINE negativeCyclePass #-}
