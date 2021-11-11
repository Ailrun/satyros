{-# LANGUAGE ViewPatterns #-}
module Satyros.DPLL.StorageUtil where

import           Control.Lens            (_2, _Just, _Nothing, _Right, each,
                                          failing, filtered, ix, like, re,
                                          takingWhile, to, use, uses, (%=), (&),
                                          (.=), (<|), (^.), (^..), (|>))
import           Data.Either             (partitionEithers)
import           Data.List.Extra         (nubOrd, partition)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import qualified Satyros.CNF             as CNF
import           Satyros.DPLL.Assignment (assignVariable, eraseVariables,
                                          parentsOfLiteral)
import           Satyros.DPLL.Effect     (DPLL)
import           Satyros.DPLL.Storage    (assignment, clauses,
                                          unassignedVariables, variableLevels)

assignDecisionVariable :: (Functor f) => CNF.Literal -> DPLL s f ()
assignDecisionVariable l@(CNF.Literal _ x) = do
  variableLevels %= ((Just x, Set.empty) <|)
  unassignedVariables %= Set.delete x
  assignment %= assignVariable l Nothing

assignImplicationVariable :: (Functor f) => CNF.Literal -> CNF.Clause -> DPLL s f ()
assignImplicationVariable l@(CNF.Literal _ x) c = do
  variableLevels . ix 0 . _2 %= Set.insert x
  unassignedVariables %= Set.delete x
  assignment %= assignVariable l (Just c)

assignFailureDrivenVariable :: (Functor f) => CNF.Literal -> CNF.Clause -> DPLL s f ()
assignFailureDrivenVariable l@(CNF.Literal _ x) c = do
  variableLevels %= ((Nothing, Set.singleton x) <|)
  unassignedVariables %= Set.delete x
  assignment %= assignVariable l (Just c)

eraseCurrentImplicationVariables :: (Functor f) => DPLL s f ()
eraseCurrentImplicationVariables = do
  xs <- use (variableLevels . to head . _2)
  variableLevels . ix 0 . _2 .= Set.empty
  unassignedVariables %= Set.union xs
  assignment %= eraseVariables xs

learnClause :: (Functor f) => CNF.Clause -> DPLL s f ()
learnClause c = do
  clauses %= (|> c)

dropLevel :: (Functor f) => DPLL s f (Maybe (Maybe CNF.Variable, Set CNF.Variable))
dropLevel = do
  lvs <- use variableLevels
  case lvs of
    [] -> pure Nothing
    [_] -> pure Nothing
    h@(levelToSet -> xs) : t -> do
      variableLevels .= t
      unassignedVariables %= Set.union xs
      assignment %= eraseVariables xs
      pure $ Just h

dropIrrelevantLevels :: (Functor f) => CNF.Clause -> DPLL s f ()
dropIrrelevantLevels c = do
  let
    cxs = c ^.. CNF.literalsOfClause . each . to CNF.literalToVariable & Set.fromList
  lvs <- uses variableLevels (^.. takingWhile (Set.disjoint cxs) (each . to levelToSet))
  variableLevels %= drop (length lvs)
  let
    xs = Set.unions lvs
  unassignedVariables %= Set.union xs
  assignment %= eraseVariables xs

deriveConflictClauseRelSAT :: (Functor f) => CNF.Clause -> DPLL s f CNF.Clause
deriveConflictClauseRelSAT c = do
  asgn <- use assignment
  (_, vl) <- uses variableLevels head
  pure . CNF.Clause . Set.toList . go asgn vl $ c ^. CNF.literalsOfClause
  where
    go asgn vl ls
      | null psAtCurrent = nonRecursiveConflictSet
      | otherwise = nonRecursiveConflictSet <> go asgn vl psAtCurrent
      where
        nonRecursiveConflictSet = Set.fromList lsWithoutP <> Set.fromList psAtLower
        (psAtCurrent, psAtLower) = partition ((`Set.member` vl) . CNF.literalToVariable) ps

        (lsWithoutP, nubOrd -> ps) = partitionEithers . flip concatMap ls $ \l ->
          asgn ^.. parentsOfLiteral l . failing (_Just . literalsInParentsOf l . re _Right) (_Nothing . like (Left (CNF.negateLiteral l)))

        literalsInParentsOf l = CNF.literalsOfClause . each . filtered ((&&) <$> (/= l) <*> (/= CNF.negateLiteral l))

levelToSet :: (Maybe CNF.Variable, Set CNF.Variable) -> Set CNF.Variable
levelToSet (dx, xs) = maybe Set.empty Set.singleton dx <> xs
