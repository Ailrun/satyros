{-# LANGUAGE ViewPatterns #-}
module Satyros.DPLL.StorageUtil where

import           Control.Lens            (_2, _Just, _Nothing, _Right, each,
                                          failing, filtered, ix, like, re,
                                          takingWhile, to, use, uses, (%=), (&),
                                          (.=), (<|), (^.), (^..), (|>))
import           Data.Either             (partitionEithers)
import           Data.List               (partition)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import qualified Satyros.CNF             as CNF
import           Satyros.DPLL.Assignment (assignVariable, eraseVariables,
                                          parentsOfLiteral)
import           Satyros.DPLL.Effect     (DPLL)
import           Satyros.DPLL.Storage    (assignment, clauses,
                                          unassignedVariables, variableLevels)

assignDecisionVariable :: CNF.Literal -> DPLL s ()
assignDecisionVariable l@(CNF.Literal _ x) = do
  variableLevels %= ((Just x, Set.empty) <|)
  unassignedVariables %= Set.delete x
  assignment %= assignVariable l Nothing

assignImplicationVariable :: CNF.Literal -> CNF.Clause -> DPLL s ()
assignImplicationVariable l@(CNF.Literal _ x) c = do
  variableLevels . ix 0 . _2 %= Set.insert x
  unassignedVariables %= Set.delete x
  assignment %= assignVariable l (Just c)

assignFailureDrivenVariable :: CNF.Literal -> CNF.Clause -> DPLL s ()
assignFailureDrivenVariable l@(CNF.Literal _ x) c = do
  variableLevels %= ((Nothing, Set.singleton x) <|)
  unassignedVariables %= Set.delete x
  assignment %= assignVariable l (Just c)

eraseCurrentImplicationVariables :: DPLL s ()
eraseCurrentImplicationVariables = do
  xs <- use (variableLevels . to head . _2)
  variableLevels . ix 0 . _2 .= Set.empty
  unassignedVariables %= Set.union xs
  assignment %= eraseVariables xs

learnClause :: CNF.Clause -> DPLL s ()
learnClause c = do
  clauses %= (|> c)

dropLevel :: DPLL s (Maybe (Maybe CNF.Variable, Set CNF.Variable))
dropLevel = do
  lvs <- use variableLevels
  case lvs of
    [] -> pure Nothing
    h@(levelToSet -> xs) : t -> do
      variableLevels .= t
      unassignedVariables %= Set.union xs
      assignment %= eraseVariables xs
      pure $ Just h

dropIrrelevantLevels :: CNF.Clause -> DPLL s ()
dropIrrelevantLevels c = do
  let
    cxs = c ^.. CNF.literalsOfClause . each . to CNF.literalToVariable & Set.fromList
  lvs <- uses variableLevels (^.. takingWhile (Set.disjoint cxs) (each . to levelToSet))
  variableLevels %= drop (length lvs)
  let
    xs = Set.unions lvs
  unassignedVariables %= Set.union xs
  assignment %= eraseVariables xs

deriveConflictClauseRelSAT :: CNF.Clause -> DPLL s CNF.Clause
deriveConflictClauseRelSAT c = do
  asgn <- use assignment
  (_, vl) <- uses variableLevels head
  pure . CNF.Clause . Set.toList . go asgn vl $ c ^. CNF.literalsOfClause
  where
    go asgn vl ls
      | null psAtCurrent = Set.fromList lsWithoutP <> Set.fromList psAtLower
      | otherwise = Set.fromList lsWithoutP <> Set.fromList psAtLower <> go asgn vl psAtCurrent
      where
        (psAtCurrent, psAtLower) = partition ((`Set.member` vl) . CNF.literalToVariable) ps

        (lsWithoutP, ps) = partitionEithers . flip concatMap ls $ \l ->
          asgn
          ^.. parentsOfLiteral l
          . failing (_Just . literalsInParentsOf l . re _Right) (_Nothing . like (Left l))

        literalsInParentsOf l = CNF.literalsOfClause . each . filtered (/= CNF.negateLiteral l)

levelToSet :: (Maybe CNF.Variable, Set CNF.Variable) -> Set CNF.Variable
levelToSet (dx, xs) = maybe Set.empty Set.singleton dx <> xs
