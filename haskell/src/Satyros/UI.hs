{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass    #-}
module Satyros.UI where

import           Prelude                     hiding ((!!))

import           Control.Lens                (_1, _2, _Just, at, head1, ix,
                                              (%~), (&), (.~), (^.))
import           Control.Monad               (void)
import           Control.Monad.Reader        (liftIO)
import           Control.Monad.Trans.Free    (FreeF (Free, Pure))
import           Data.Either.Extra           (fromRight')
import           Data.Functor.Compose        (Compose (Compose, getCompose))
import           Data.IORef.Extra            (IORef, modifyIORef', newIORef,
                                              readIORef)
import           Data.List.NonEmpty          (NonEmpty ((:|)))
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle (FromJSVal (fromJSVal), Function,
                                              JSM, JSString, JSVal, ToJSVal,
                                              call, global, js, js1, toJSVal,
                                              (!!), (!))
import qualified Satyros.BellmanFord         as BellmanFord
import           Satyros.BellmanFord.Effect  (BellmanFordF)
import qualified Satyros.CNF                 as CNF
import           Satyros.DPLL                (DPLL)
import qualified Satyros.DPLL                as DPLL
import           Satyros.Handler.Naive       (naiveHandler)
import qualified Satyros.QFIDL               as QFIDL
import           Satyros.UI.Util             (function0, function1, function2)
import           System.Random.Stateful      (getStdGen)

type InternalStorage = (QFIDL.ConversionTable, BellmanFord.IDLGraph, BellmanFord.Storage)
type Storage = DPLL.Storage InternalStorage

data SatyrosInterface
  = SatyrosInterface
    { myfun      :: Function
    , assignment :: SatyrosAssignment
    , step       :: Function
    }
  deriving stock (Generic)
  deriving anyclass (ToJSVal)

data SatyrosAssignment
  = SatyrosAssignment
    { getValue :: Function
    , setValue :: Function
    }
  deriving stock (Generic)
  deriving anyclass (ToJSVal)

makeSatyrosInterface :: JSM (JSVal -> JSM JSVal)
makeSatyrosInterface = pure $ \jf -> do
  f <- fromJust <$> fromJSVal jf
  stdGen <- getStdGen
  let
    (cnf, t) = QFIDL.toCNF f
    s = fromRight' $ DPLL.initializeStorage cnf stdGen (t, Map.empty, Map.empty)
  tsRef <- liftIO $ newIORef ((s, DPLL.bcp >> pure False) :| [])
  myfun <- function0 (\_ _ -> global ^. js ("console" :: JSString) . js1 ("log" :: JSString) f >> pure ())
  step <- function1 $ makeStep tsRef
  assignment <- makeAssignment tsRef
  toJSVal SatyrosInterface{..}

makeStep :: IORef (NonEmpty (Storage, DPLL InternalStorage BellmanFordF Bool)) -> JSVal -> JSVal -> JSVal -> JSM ()
makeStep tsRef _ this cb = do
  (s, next) :| _ <- liftIO $ readIORef tsRef
  let
    (feff, s') = DPLL.stepDPLL next s
  case feff of
    Pure b -> void $ call cb this [b]
    Free eff -> liftIO $ modifyIORef' tsRef ((s', naiveHandler eff) NE.<|)

makeAssignment :: IORef (NonEmpty (Storage, DPLL InternalStorage BellmanFordF Bool)) -> JSM SatyrosAssignment
makeAssignment tsRef = do
  getValue <- function2 makeGetValue
  setValue <- function2 makeSetValue
  pure SatyrosAssignment{..}
  where
    makeGetValue _ this jx c = do
      x <- fromJust <$> fromJSVal jx
      ts <- liftIO $ readIORef tsRef
      void $ call c this [ts ^. head1 . _1 . DPLL.assignment . at x & _Just . _2 . _Just %~ (^. CNF.literalsOfClause)]
    makeSetValue _ _ jx jv = do
      x <- fromJust <$> fromJSVal jx
      v <- fromJust <$> fromJSVal jv
      liftIO $ modifyIORef' tsRef $ \ts ->
        ts & head1 . _1 . DPLL.assignment . ix x .~ v

instance FromJSVal CNF.Variable where
  fromJSVal = fmap (fmap CNF.Variable) . fromJSVal
instance FromJSVal CNF.Literal
instance FromJSVal a => FromJSVal (CNF.ClauseLike a)
instance FromJSVal a => FromJSVal (CNF.FormulaLike a)

instance FromJSVal QFIDL.Variable

instance FromJSVal QFIDL.Operator where
  fromJSVal jv = fmap read <$> fromJSVal jv

instance FromJSVal QFIDL.Expressible where
  fromJSVal jv = do
    pl <- jv ! ("length" :: JSString)
    i <- fromJSVal pl
    case i of
      Just (3 :: Int) -> singletonFromJSVal
      Just (4 :: Int) -> differenceFromJSVal
      _               -> pure Nothing
    where
      singletonFromJSVal = getCompose $ QFIDL.Singleton <$> Compose (jv !! 0 >>= fromJSVal) <*> Compose (jv !! 1 >>= fromJSVal) <*> Compose (jv !! 2 >>= fromJSVal)
      differenceFromJSVal = getCompose $ QFIDL.Difference <$> Compose (jv !! 0 >>= fromJSVal) <*> Compose (jv !! 1 >>= fromJSVal) <*> Compose (jv !! 2 >>= fromJSVal) <*> Compose (jv !! 3 >>= fromJSVal)

instance ToJSVal CNF.Variable where
  toJSVal (CNF.Variable v) = toJSVal v
instance ToJSVal CNF.Literal
instance ToJSVal a => ToJSVal (CNF.ClauseLike a)
instance ToJSVal a => ToJSVal (CNF.FormulaLike a)

instance ToJSVal QFIDL.Variable

instance ToJSVal QFIDL.Operator where
  toJSVal x = toJSVal (show x)
instance ToJSVal QFIDL.Expressible where
  toJSVal (QFIDL.Singleton x o v) = sequenceA [toJSVal x, toJSVal o, toJSVal v] >>= toJSVal
  toJSVal (QFIDL.Difference x y o v) = sequenceA [toJSVal x, toJSVal y, toJSVal o, toJSVal v] >>= toJSVal
