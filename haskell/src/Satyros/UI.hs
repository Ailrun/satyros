{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Satyros.UI
  ( makeSatyrosAPI
  , makeSatyrosAPI1
  ) where

import           Prelude                     hiding ((!!))

import           Control.Lens                (_1, _Just, _Wrapped', at, each,
                                              filtered, from, head1, ix,
                                              partsOf, singular, to, (^.),
                                              (^..), (^?))
import           Control.Monad               (void)
import           Control.Monad.Reader        (liftIO)
import           Control.Monad.Trans.Free    (FreeF (Free, Pure))
import           Data.Bifunctor              (bimap, first)
import           Data.Either.Extra           (fromRight')
import           Data.Functor.Compose        (Compose (Compose, getCompose))
import           Data.IORef.Extra            (IORef, modifyIORef', newIORef,
                                              readIORef, writeIORef)
import           Data.List.NonEmpty          (NonEmpty ((:|)))
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust, maybeToList)
import qualified Data.Set                    as Set
import qualified Data.Vector                 as Vector
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle (FromJSVal (fromJSVal), Function,
                                              JSM, JSString, JSVal, ToJSVal,
                                              call, js0, js1, toJSVal, (!!),
                                              (!))
import qualified Satyros.BellmanFord         as BellmanFord
import           Satyros.BellmanFord.Effect  (BellmanFordF)
import qualified Satyros.CNF                 as CNF
import qualified Satyros.CNF                 as DPLL
import           Satyros.DPLL                (DPLL, DPLLF)
import qualified Satyros.DPLL                as DPLL
import           Satyros.Handler.Clever      (cleverHandler)
import           Satyros.Handler.Naive       (naiveHandler)
import           Satyros.Handler.Type        (InternalStorage, Storage)
import qualified Satyros.QFIDL               as QFIDL
import           Satyros.UI.Util             (function0, function1, function2)
import           System.Random.Stateful      (mkStdGen)

data SatyrosAPI
  = SatyrosAPI
    { initialFormula      :: CNF.Formula
    , expressedFormula    :: CNF.FormulaLike QFIDL.Expressed
    , getFormula          :: Function
    , conversionTable     :: SatyrosConversionTable
    , assignment          :: SatyrosAssignmentAPI
    , getImplicationGraph :: Function
    , getBellmanFordGraph :: Function
    , step                :: Function
    , undo                :: Function
    , reset               :: Function
    }
  deriving stock (Generic)
  deriving anyclass (ToJSVal)

data SatyrosConversionTable
  = SatyrosConversionTable
    { variableToExpressed :: [(CNF.Variable, QFIDL.Expressed)]
    , expressedToLiteral  :: [(QFIDL.Expressed, CNF.Literal)]
    }
  deriving stock (Generic)
  deriving anyclass (ToJSVal)

data SatyrosAssignmentAPI
  = SatyrosAssignmentAPI
    { getValueMapList         :: Function
    , getValue                :: Function
    , getValueOfClauseMapList :: Function
    , getValueOfClause        :: Function
    , getValueOfFormula       :: Function
    }
  deriving stock (Generic)
  deriving anyclass (ToJSVal)

data SatyrosImplicationVertex
  = SatyrosImplicationVertex
    { variable   :: CNF.Variable
    , value      :: Bool
    , isDecision :: Bool
    , level      :: Int
    }
  deriving stock (Generic)
  deriving anyclass (ToJSVal)

data SatyrosImplicationEdge
  = SatyrosImplicationEdge
    { startVertex :: CNF.Variable
    , endVertex   :: CNF.Variable
    , levelDiff   :: Int
    , clauseIndex :: Int
    }
  deriving stock (Generic)
  deriving anyclass (ToJSVal)

data SatyrosBellmanFordVertex
  = SatyrosBellmanFordVertex
    { variable :: BellmanFord.IDLGraphVertex
    , distance :: Int
    }
  deriving stock (Generic)
  deriving anyclass (ToJSVal)

data SatyrosBellmanFordEdge
  = SatyrosBellmanFordEdge
    { startVertex :: BellmanFord.IDLGraphVertex
    , endVertex   :: BellmanFord.IDLGraphVertex
    , weight      :: Int
    , lastActive  :: Bool
    }
  deriving stock (Generic)
  deriving anyclass (ToJSVal)

type SatyrosAPIHistory = NonEmpty (Storage, Maybe (DPLLF BellmanFordF (DPLL InternalStorage BellmanFordF Bool)), DPLL InternalStorage BellmanFordF Bool)

makeSatyrosAPI :: JSM (JSVal -> JSM JSVal)
makeSatyrosAPI = pure $ \jf -> do
  f <- fromJust <$> fromJSVal jf
  let
    stdGen = mkStdGen 0
    (cnf, t) = QFIDL.toCNF f
    s = fromRight' $ DPLL.initializeStorage cnf stdGen (t, Map.empty, Map.empty, Set.empty)
  tsRef <- liftIO $ newIORef ((s, Nothing, DPLL.bcp >> pure False) :| [])
  step <- function1 $ makeStep tsRef
  undo <- function1 $ makeUndo tsRef
  reset <- function0 $ makeReset tsRef
  let
    initialFormula = cnf
    expressedFormula =
      cnf
      ^. CNF.clausesOfFormula
      . partsOf
        ( each
          . CNF.literalsOfClause
          . partsOf
            ( each
              . to (\(CNF.Literal p x) -> (x, p ^. CNF.isPositive))
            )
          . to (QFIDL.fromAssignment t)
          . from CNF.entriesOfClauseLike
        )
      . from CNF.clauseLikesOfFormulaLike
    conversionTable = SatyrosConversionTable (Map.toList $ fst t) (Map.toList $ snd t)
  getFormula <- makeGetFormula tsRef
  assignment <- makeAssignmentAPI tsRef
  getImplicationGraph <- makeGetImplicationGraph tsRef
  getBellmanFordGraph <- makeGetBellmanFordGraph tsRef
  toJSVal SatyrosAPI{..}

makeSatyrosAPI1 :: JSM (JSVal -> JSM JSVal)
makeSatyrosAPI1 = pure $ \jf -> do
  f <- fromJust <$> fromJSVal jf
  let
    stdGen = mkStdGen 0
    (cnf, t) = QFIDL.toCNF f
    s = fromRight' $ DPLL.initializeStorage cnf stdGen (t, Map.empty, Map.empty, Set.empty)
  tsRef <- liftIO $ newIORef ((s, Nothing, DPLL.bcp >> pure False) :| [])
  step <- function1 $ makeStep1 tsRef
  undo <- function1 $ makeUndo tsRef
  reset <- function0 $ makeReset tsRef
  let
    initialFormula = cnf
    expressedFormula =
      cnf
      ^. CNF.clausesOfFormula
      . partsOf
        ( each
          . CNF.literalsOfClause
          . partsOf
            ( each
              . to (\(CNF.Literal p x) -> (x, p ^. CNF.isPositive))
            )
          . to (QFIDL.fromAssignment t)
          . from CNF.entriesOfClauseLike
        )
      . from CNF.clauseLikesOfFormulaLike
    conversionTable = SatyrosConversionTable (Map.toList $ fst t) (Map.toList $ snd t)
  getFormula <- makeGetFormula tsRef
  assignment <- makeAssignmentAPI tsRef
  getImplicationGraph <- makeGetImplicationGraph tsRef
  getBellmanFordGraph <- makeGetBellmanFordGraph tsRef
  toJSVal SatyrosAPI{..}

makeStep :: IORef SatyrosAPIHistory -> JSVal -> JSVal -> JSVal -> JSM ()
makeStep tsRef _ _ cb = do
  (s, _, next) :| _ <- liftIO $ readIORef tsRef
  let
    (feff, s') = DPLL.stepDPLL next s
  case feff of
    Pure b   -> do
      liftIO $ modifyIORef' tsRef ((s', Nothing, pure b) NE.<|)
      void $ cb ^. js1 ("Finish" :: JSString) b
    Free eff -> do
      liftIO $ modifyIORef' tsRef ((s', Just eff, naiveHandler eff) NE.<|)
      invokeEffectCallback cb eff

makeStep1 :: IORef SatyrosAPIHistory -> JSVal -> JSVal -> JSVal -> JSM ()
makeStep1 tsRef _ _ cb = do
  (s, _, next) :| _ <- liftIO $ readIORef tsRef
  let
    (feff, s') = DPLL.stepDPLL next s
  case feff of
    Pure b   -> do
      liftIO $ modifyIORef' tsRef ((s', Nothing, pure b) NE.<|)
      void $ cb ^. js1 ("Finish" :: JSString) b
    Free eff -> do
      liftIO $ modifyIORef' tsRef ((s', Just eff, cleverHandler eff) NE.<|)
      invokeEffectCallback cb eff

makeUndo :: IORef SatyrosAPIHistory -> JSVal -> JSVal -> JSVal -> JSM ()
makeUndo tsRef _ _ cb = do
  _ :| ts <- liftIO $ readIORef tsRef
  case ts of
    [] -> void $ cb ^. js1 ("Start" :: JSString) True
    t'@(_, meff, _) : ts' -> do
      liftIO $ writeIORef tsRef (t' :| ts')
      case meff of
        Just eff -> invokeEffectCallback cb eff
        Nothing  -> void $ cb ^. js1 ("Start" :: JSString) False

makeReset :: IORef SatyrosAPIHistory -> JSVal -> JSVal -> JSM ()
makeReset tsRef _ _ = do
  liftIO $ modifyIORef' tsRef (pure . NE.last)

invokeEffectCallback :: JSVal -> DPLLF BellmanFordF a -> JSM ()
invokeEffectCallback cb DPLL.BCPUnitClause{} = void $ cb ^. js0 ("BCPUnitClause" :: JSString)
invokeEffectCallback cb DPLL.BCPComplete = void $ cb ^. js0 ("BCPComplete" :: JSString)
invokeEffectCallback cb DPLL.BCPConflict{} = void $ cb ^. js0 ("BCPConflict" :: JSString)
invokeEffectCallback cb DPLL.BCPConflictDrivenClause{} = void $ cb ^. js0 ("BCPConflictDrivenClause" :: JSString)
invokeEffectCallback cb DPLL.DecisionResult{} = void $ cb ^. js0 ("DecisionResult" :: JSString)
invokeEffectCallback cb DPLL.DecisionComplete = void $ cb ^. js0 ("DecisionComplete" :: JSString)
invokeEffectCallback cb DPLL.BacktraceExhaustion = void $ cb ^. js0 ("BacktraceExhaustion" :: JSString)
invokeEffectCallback cb DPLL.BacktraceComplete{} = void $ cb ^. js0 ("BacktraceComplete" :: JSString)
invokeEffectCallback cb (DPLL.InsideDPLL BellmanFord.PropagationCheck{}) = void $ cb ^. js0 ("PropagationCheck" :: JSString)
invokeEffectCallback cb (DPLL.InsideDPLL BellmanFord.PropagationFindShorter{}) = void $ cb ^. js0 ("PropagationFindShorter" :: JSString)
invokeEffectCallback cb (DPLL.InsideDPLL BellmanFord.PropagationNth{}) = void $ cb ^. js0 ("PropagationNth" :: JSString)
invokeEffectCallback cb (DPLL.InsideDPLL BellmanFord.PropagationEnd) = void $ cb ^. js0 ("PropagationEnd" :: JSString)
invokeEffectCallback cb (DPLL.InsideDPLL BellmanFord.NegativeCycleCheck{}) = void $ cb ^. js0 ("NegativeCycleCheck" :: JSString)
invokeEffectCallback cb (DPLL.InsideDPLL BellmanFord.NegativeCycleFind{}) = void $ cb ^. js0 ("NegativeCycleFind" :: JSString)
invokeEffectCallback cb (DPLL.InsideDPLL BellmanFord.NegativeCyclePass) = void $ cb ^. js0 ("NegativeCyclePass" :: JSString)

makeAssignmentAPI :: IORef SatyrosAPIHistory -> JSM SatyrosAssignmentAPI
makeAssignmentAPI tsRef = do
  getValueMapList <- function1 makeGetValueMapList
  getValue <- function2 makeGetValue
  getValueOfClause <- function2 makeGetValueOfClause
  getValueOfClauseMapList <- function1 makeGetValueOfClauseMapList
  getValueOfFormula <- function1 makeGetValueOfFormula
  pure SatyrosAssignmentAPI{..}
  where
    makeGetValueMapList _ this cb = do
      ts <- liftIO $ readIORef tsRef
      void $ call cb this [ts ^. head1 . _1 . DPLL.assignment . _Wrapped' . to Map.toList]

    makeGetValue _ this jx cb = do
      x <- fromJust <$> fromJSVal jx
      ts <- liftIO $ readIORef tsRef
      void $ call cb this [ts ^. head1 . _1 . DPLL.assignment . at x]

    makeGetValueOfClauseMapList _ this cb = do
      ts <- liftIO $ readIORef tsRef
      let
        s = ts ^. head1 . _1
        asgn = s ^. DPLL.assignment
        cv = s ^.. DPLL.clauses . each . DPLL.literalsOfClause . to (valueOfLiterals asgn)
      void $ call cb this [zip [(0 :: Int)..] cv]

    makeGetValueOfClause _ this ji cb = do
      i <- fromJust <$> fromJSVal ji
      ts <- liftIO $ readIORef tsRef
      let
        s = ts ^. head1 . _1
        asgn = s ^. DPLL.assignment
        cv = s ^. DPLL.clauses . partsOf (ix i . DPLL.literalsOfClause . each) . to (valueOfLiterals asgn)
      void $ call cb this [cv]

    makeGetValueOfFormula _ this cb = do
      ts <- liftIO $ readIORef tsRef
      let
        s = ts ^. head1 . _1
        asgn = s ^. DPLL.assignment
        c = s ^. DPLL.clauses . to Vector.toList . partsOf (each . DPLL.literalsOfClause . to (fmap not . valueOfLiterals asgn)) . to (fmap not . firstTrue)
      void $ call cb this [c]

    valueOfLiterals asgn ls = firstTrue $ fmap (\l -> asgn ^? DPLL.valueOfLiteral l) ls

    firstTrue [] = Just False
    firstTrue (lv : lvs)
      | Just True <- lv = Just True
      | Just False <- lv = rest
      | Just False <- rest = Nothing
      | otherwise = rest
      where
        rest = firstTrue lvs

makeGetFormula :: IORef SatyrosAPIHistory -> JSM Function
makeGetFormula tsRef = do
  function1 makeGet
  where
    makeGet _ this cb = do
      ts <- liftIO $ readIORef tsRef
      void $ call cb this [ts ^. head1 . _1 . DPLL.clauses . to Vector.toList . from CNF.clausesOfFormula]

makeGetImplicationGraph :: IORef SatyrosAPIHistory -> JSM Function
makeGetImplicationGraph tsRef = do
  function1 makeGetVerticesEdges
  where
    makeGetVerticesEdges _ this cb = do
      ts <- liftIO $ readIORef tsRef
      let
        s = ts ^. head1 . _1
        leveledVs =
          concatMap (\(level, vs) -> fmap (\((variable, value), isDecision) -> SatyrosImplicationVertex{..}) vs)
          . zip [(0 :: Int)..]
          . reverse
          . fmap (fmap (first (\x -> (x, s ^. DPLL.assignment . singular (DPLL.valueOfVariable x)))) . uncurry (<>) . bimap (fmap (,True) . maybeToList) (fmap (,False) . Set.toList))
          $ s ^. DPLL.variableLevels
        varToLevel = Map.fromList $ (\SatyrosImplicationVertex{..} -> (variable, level)) <$> leveledVs
        impls =
          concatMap (\(endVertex, (_, c)) ->
                        c ^.. _Just . CNF.literalsOfClause . each . to CNF.literalToVariable . filtered (/= endVertex) . to (makeEdge varToLevel (s ^. DPLL.clauses . to (fromJust . Vector.elemIndex (fromJust c))) endVertex))
          $ s ^. DPLL.assignment . _Wrapped' . to Map.toList
      void $ call cb this [toJSVal leveledVs, toJSVal impls]

    makeEdge varToLevel clauseIndex endVertex startVertex =
      let
        levelDiff = varToLevel Map.! endVertex - varToLevel Map.! startVertex
      in
        SatyrosImplicationEdge{..}

makeGetBellmanFordGraph :: IORef SatyrosAPIHistory -> JSM Function
makeGetBellmanFordGraph tsRef = do
  function1 makeGetVerticesEdges
  where
    makeGetVerticesEdges _ this cb = do
      ts <- liftIO $ readIORef tsRef
      let
        (_, g, m, lastActiveE) = ts ^. head1 . _1 . DPLL.theory
        vs =
          (\(variable, (_, distance)) -> SatyrosBellmanFordVertex{..}) <$> Map.toList m
        es =
          filter (\SatyrosBellmanFordEdge{..} -> startVertex /= endVertex)
          . fmap (\((startVertex, endVertex), weight) ->
                     let
                       lastActive = (startVertex, endVertex) `Set.member` lastActiveE
                     in
                       SatyrosBellmanFordEdge{..}
                 )
          $ Map.toList g
      void $ call cb this [toJSVal vs, toJSVal es]

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

instance FromJSVal QFIDL.Expressed where
  fromJSVal jv = do
    pl <- jv ! ("length" :: JSString)
    i <- fromJSVal pl
    case i of
      Just (3 :: Int) -> differenceFromJSVal
      _               -> pure Nothing
    where
      differenceFromJSVal = getCompose $ QFIDL.LessThanEqualTo <$> Compose (jv !! 0 >>= fromJSVal) <*> Compose (jv !! 1 >>= fromJSVal) <*> Compose (jv !! 2 >>= fromJSVal)

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

instance ToJSVal QFIDL.Expressed where
  toJSVal (QFIDL.LessThanEqualTo x y v) = sequenceA [toJSVal x, toJSVal y, toJSVal v] >>= toJSVal
