{-# LANGUAGE ViewPatterns #-}
module Satyros.DPLL where

import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.Extra    (forM_, ifM, unless, when)
import           Control.Monad.Reader   (ReaderT (runReaderT), asks, lift)
import           Control.Monad.ST       (ST)
import           Data.Foldable.Extra    (allM)
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap
import           Data.IntSet            (IntSet)
import qualified Data.IntSet            as IntSet
import           Data.List.Extra        (notNull, partition, (\\))
import           Data.STRef             (STRef, modifySTRef', newSTRef,
                                         readSTRef, writeSTRef)
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq
import qualified Satyros.CNF            as CNF
import           Satyros.Util           (intToWord, wordToInt)
import           System.Random          (getStdRandom)
import           System.Random.Stateful (STGenM, StdGen, randomM, randomRM,
                                         runSTGen)

data DPLLFailure
  = DPLLException String
  | DPLLUnsatisfiable String

data DPLLAssignment
  = DPLLAssignment
    { value   :: CNF.Positivity
    , covered :: Bool
    }

data DPLLState s
  = DPLLState
    { unsetVariablesRef :: STRef s IntSet
    , clauses           :: Seq CNF.Clause
    , assignmentsRef    :: STRef s (IntMap DPLLAssignment)
    , variableLevelsRef :: STRef s [STRef s (Int, IntSet)]
    , stGenRef          :: STRef s (STGenM StdGen s)
    }

type DPLLMonad s = ReaderT (DPLLState s) (ExceptT DPLLFailure (ST s))

dpll :: (Int, Int, CNF.Formula) -> IO (Either DPLLFailure [Bool])
dpll = getStdRandom . dpll'

dpll' :: (Int, Int, CNF.Formula) -> StdGen -> (Either DPLLFailure [Bool], StdGen)
dpll' input gen =
  runSTGen gen $ \stGen -> runExceptT $ initialize stGen input >>= runReaderT startMainLoop
  where
    startMainLoop :: DPLLMonad s [Bool]
    startMainLoop =
      ifM (bcp True)
      ( ifM decide
        mainLoop
        getFinalAssignment
      )
      (throwError $ DPLLUnsatisfiable "The initial constraint propagation derives a conflict")

    -- |
    -- Main loop for DPLL search.
    mainLoop :: DPLLMonad s [Bool]
    mainLoop =
      ifM (bcp False)
      ( ifM decide
        mainLoop
        getFinalAssignment
      )
      backtrack

    getFinalAssignment :: DPLLMonad s [Bool]
    getFinalAssignment = do
      fmap value
        . IntMap.elems
        <$> readSTRefDPLLState assignmentsRef

    -- |
    -- (Chronological) backtracking
    backtrack :: DPLLMonad s [Bool]
    backtrack =
      ifM dropCoveredLevels
        mainLoop
        (throwError $ DPLLUnsatisfiable "All possibilities are exhausted")

-- |
-- Initialize DPLL database and resolve trivial error cases / unit clauses.
initialize :: STGenM StdGen s -> (Int, Int, CNF.Formula) -> ExceptT DPLLFailure (ST s) (DPLLState s)
initialize stGen (nv, nc, f) = do
  when (length cs /= nc) $
    throwError $ DPLLException "The number of clauses mismatches. Check your DIMACS file."
  when (notNull emptyCs) $
    throwError $ DPLLUnsatisfiable "Empty clauses are detected. Is this really intended?"
  when (CNF.maxVariableInFormula f > CNF.Variable (fromIntegral nv)) $
    throwError $ DPLLException "The number of variables mistmatches. Check your DIMACS file."
  forM_ initialAssignmentPairs $ \(v, value -> b) ->
    when (fmap value (IntMap.lookup v initialAssignment) == Just (not b)) $
      throwError $ DPLLUnsatisfiable "The initial constraint derives a conflict"

  unsetVariablesRef <- lift . newSTRef $ IntSet.fromList $ [1..nv] \\ initialSetVariables
  assignmentsRef <- lift $ newSTRef initialAssignment
  variableLevelsRef <- lift $ newSTRef []
  stGenRef <- lift $ newSTRef stGen
  pure DPLLState{..}
  where
    cs = CNF.unFormula f

    (emptyCs, nonemptyCs) = partition CNF.emptyClause cs
    (unitCs, nonunitCs) = partition CNF.unitClause nonemptyCs
    clauses = Seq.fromList nonunitCs

    cnfLits = [l | CNF.Clause ls <- unitCs, l <- ls]
    initialAssignmentPairs =
      [(wordToInt v, DPLLAssignment pos False) | CNF.Literal pos (CNF.Variable v) <- cnfLits]
    initialSetVariables = fmap fst initialAssignmentPairs
    initialAssignment = IntMap.fromList initialAssignmentPairs

-- |
-- Variable decision
-- This implementation randomly decide a variable among unset vars.
decide :: DPLLMonad s Bool
decide = do
  uvs <- readSTRefDPLLState unsetVariablesRef
  if IntSet.null uvs
    then pure False
    else do
      let
        lenUvs = IntSet.size uvs
      stGen <- readSTRefDPLLState stGenRef
      i <- lift . lift $ randomRM (0, lenUvs - 1) stGen
      let
        v = IntSet.toList uvs !! i
        uvs' = IntSet.delete v uvs
      writeSTRefDPLLState unsetVariablesRef uvs'
      s <- lift . lift $ randomM stGen
      insertNewLevel v
      insertNewAssignment False (CNF.Variable (intToWord v)) s
      pure True

-- |
-- Boolean constraint propagation
-- Returns @True@ iff propagation successfully finished without a conflict
bcp :: Bool -> DPLLMonad s Bool
bcp isStart = asks clauses >>= allM (bcpOfCla isStart)

-- |
-- Boolean constraint propagation for a single clause
-- Returns @True@ iff propagation successfully finished without a conflict
bcpOfCla :: Bool -> CNF.Clause -> DPLLMonad s Bool
bcpOfCla isStart (CNF.Clause ls') = do
  asgn <- readSTRefDPLLState assignmentsRef
  case go asgn ls' of
    Nothing -> pure False
    Just Nothing -> pure True
    Just (Just (CNF.Literal p v)) -> do
      insertNewAssignment isStart v p
      bcp isStart
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

-- |
-- Drop all covered decision levels
-- Returns @True@ iff there is at least one uncovered decision level left
dropCoveredLevels :: DPLLMonad s Bool
dropCoveredLevels = do
  vlRefs <- readSTRefDPLLState variableLevelsRef
  mayVlRefs' <- go vlRefs
  case mayVlRefs' of
    Nothing -> pure False
    Just vlRef -> do
      (dv, vl) <- lift . lift $ readSTRef vlRef
      modifySTRefDPLLState' assignmentsRef (`IntMap.withoutKeys` IntSet.delete dv vl)
      modifySTRefDPLLState' assignmentsRef
        (IntMap.adjust (\DPLLAssignment{..} -> DPLLAssignment{value = not value, covered = True}) dv)
      modifySTRefDPLLState' unsetVariablesRef (<> IntSet.delete dv vl)
      lift . lift $ writeSTRef vlRef (dv, IntSet.singleton dv)
      pure True
  where
    go :: [STRef s (Int, IntSet)] -> DPLLMonad s (Maybe (STRef s (Int, IntSet)))
    go [] = pure Nothing
    go (vlRef:vlRefs) = do
      (dv, _) <- lift . lift $ readSTRef vlRef
      ifM (covered . (IntMap.! dv) <$> readSTRefDPLLState assignmentsRef)
        (dropLevel >> go vlRefs)
        (pure . Just $ vlRef)

dropLevel :: DPLLMonad s ()
dropLevel = do
  vlRef <- head <$> readSTRefDPLLState variableLevelsRef
  (_, vl) <- lift . lift $ readSTRef vlRef
  modifySTRefDPLLState' assignmentsRef (`IntMap.withoutKeys` vl)
  modifySTRefDPLLState' unsetVariablesRef (<> vl)
  modifySTRefDPLLState' variableLevelsRef tail

insertNewLevel :: Int -> DPLLMonad s ()
insertNewLevel v = do
  newLevel <- lift . lift $ newSTRef (v, IntSet.empty)
  modifySTRefDPLLState' variableLevelsRef (newLevel :)

insertNewAssignment :: CNF.Positivity -> CNF.Variable -> Bool -> DPLLMonad s ()
insertNewAssignment isStart (CNF.Variable v) b = do
  unless isStart $ do
    (head -> vlRef) <- readSTRefDPLLState variableLevelsRef
    lift . lift $ modifySTRef' vlRef $ fmap (IntSet.insert (wordToInt v))
  modifySTRefDPLLState' assignmentsRef (IntMap.insert (wordToInt v) (DPLLAssignment b False))

getAssignedValue :: IntMap DPLLAssignment -> CNF.Literal -> Maybe Bool
getAssignedValue asgn (CNF.Literal p (CNF.Variable v)) = (p ==) . value <$> IntMap.lookup (wordToInt v) asgn

readSTRefDPLLState :: (DPLLState s -> STRef s a) -> DPLLMonad s a
readSTRefDPLLState f = asks f >>= lift . lift . readSTRef

writeSTRefDPLLState :: (DPLLState s -> STRef s a) -> a -> DPLLMonad s ()
writeSTRefDPLLState f a = asks f >>= lift . lift . flip writeSTRef a

modifySTRefDPLLState' :: (DPLLState s -> STRef s a) -> (a -> a) -> DPLLMonad s ()
modifySTRefDPLLState' f g = asks f >>= lift . lift . flip modifySTRef' g

-- updateClRefWith :: STRef s (CNFLit, CNFLit, CNF.Clause) -> CNFLit -> (CNFLit, CNFLit, CNF.Clause) -> DPLLMonad s (Either CNF.Clause (Seq CNFLit))
-- updateClRefWith clRef l (l1, l2, CNF.Clause ls)
--   | -l == l1 = do
--       asgns <- asks assignmentsRef >>= lift . readSTRef
--       case findNonZero asgns of
--         Just l1' -> do
--           lift $ writeSTRef clRef (l1', l2, CNF.Clause (l1 : delete l1' ls))
--           pure $ Right Seq.empty
--         Nothing
--           | isNonZero asgns l2 -> setToOne asgns l2
--           | otherwise          -> pure . Left . CNF.Clause $ l1 : l2 : ls
--   | -l == l2 = do
--       asgns <- asks assignmentsRef >>= lift . readSTRef
--       case findNonZero asgns of
--         Just l2' -> do
--           lift $ writeSTRef clRef (l1, l2', CNF.Clause (l2 : delete l2' ls))
--           pure $ Right Seq.empty
--         Nothing
--           | isNonZero asgns l1 -> setToOne asgns l1
--           | otherwise          -> pure . Left . CNF.Clause $ l1 : l2 : ls
--   | otherwise = pure $ Right Seq.empty
--   where
--     findNonZero :: IntMap DPLLAssignment -> Maybe CNFLit
--     findNonZero asgns = find (isNonZero asgns) ls

--     isNonZero asgns (CNFLit i)
--       | Just asgn <- IntMap.lookup (abs i) asgns
--       = value asgn == (i > 0)
--       | otherwise
--       = True

--     setToOne asgns l'@(CNFLit i)
--       | IntMap.member (abs i) asgns
--       = pure $ Right Seq.empty
--       | otherwise
--       = do
--           dlv <- asks decisionLevelRef >>= lift . readSTRef
--           -- TODO: find parents
--           asks assignmentsRef >>= lift . flip modifySTRef' (IntMap.insert (abs i) (DPLLAssignment dlv (i > 0) False))
--           pure . Right $ Seq.singleton l'
