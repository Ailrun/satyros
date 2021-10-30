{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ViewPatterns           #-}
module Satyros.DPLL.Storage where

import           Control.Lens            (each, makeFieldsNoPrefix, set, to,
                                          (&), (.~), (^.), (^..), (^?))
import           Control.Monad           (forM_, unless, when)
import           Control.Monad.Except    (throwError)
import           Data.List               (partition)
import qualified Data.Map                as Map
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vector
import qualified Satyros.CNF             as CNF
import           Satyros.DPLL.Assignment (Assignment (Assignment),
                                          valueOfVariable)
import           System.Random           (RandomGen (..), StdGen)

data Storage s
  = Storage
    { _unassignedVariables :: Set CNF.Variable
    , _clauses             :: Vector CNF.Clause
    , _assignment          :: Assignment
    , _variableLevels      :: [(Maybe CNF.Variable, Set CNF.Variable)]
    , _stdGen              :: StdGen
    , _theory              :: s
    }
  deriving stock (Show)

makeFieldsNoPrefix ''Storage

instance RandomGen (Storage s) where
  split s =
    let
      (stdGen0, stdGen1) = s ^. stdGen . to split
    in
    (s & stdGen .~ stdGen0, s & stdGen .~ stdGen1)

  genWord8 s = flip (set stdGen) s <$> genWord8 (s ^. stdGen)
  genWord16 s = flip (set stdGen) s <$> genWord16 (s ^. stdGen)
  genWord32 s = flip (set stdGen) s <$> genWord32 (s ^. stdGen)
  genWord64 s = flip (set stdGen) s <$> genWord64 (s ^. stdGen)

  genWord32R u s = flip (set stdGen) s <$> genWord32R u (s ^. stdGen)
  genWord64R u s = flip (set stdGen) s <$> genWord64R u (s ^. stdGen)

  genShortByteString n s = flip (set stdGen) s <$> genShortByteString n (s ^. stdGen)

data StorageInitializationFailure
  = EmptyClause
  | InitialConflict

initializeStorage :: CNF.Formula -> StdGen -> s -> Either StorageInitializationFailure (Storage s)
initializeStorage f _stdGen _theory = do
  unless (null emptyCs) $
    throwError EmptyClause
  forM_ initialAssignmentPairs $ \(x, fst -> b) ->
    when (_assignment ^? valueOfVariable x == Just (not b)) $
      throwError InitialConflict

  pure Storage{..}
  where
    _unassignedVariables = allVariables Set.\\ initiallyAssignedVs
    _clauses = Vector.fromList nonunitCs
    _assignment = Assignment $ Map.fromList initialAssignmentPairs
    _variableLevels = [(Nothing, initiallyAssignedVs)]

    allVariables = Set.fromList $ fmap CNF.Variable [1..mv]
    CNF.Variable mv = CNF.maxVariableInFormula f

    initiallyAssignedVs = Set.fromList $ fmap fst initialAssignmentPairs
    initialAssignmentPairs =
      [(v, (pos ^. CNF.isPositive, Nothing)) | CNF.Literal pos v <- cnfLits]
    cnfLits = unitCs ^.. each . CNF.literalsOfClause . each

    (unitCs, nonunitCs) = partition CNF.unitClause nonemptyCs
    (emptyCs, nonemptyCs) = partition CNF.emptyClause cs
    cs = f ^. CNF.clausesOfFormula
