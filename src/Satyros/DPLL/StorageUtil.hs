module Satyros.DPLL.StorageUtil where

import           Control.Lens            (_2, _Just, _Nothing, _Right, each,
                                          failing, filtered, ix, like, re, use,
                                          uses, (%=), (^.), (^..))
import           Data.Either             (partitionEithers)
import           Data.List               (partition)
import qualified Data.Set                as Set
import qualified Satyros.CNF             as CNF
import           Satyros.DPLL.Assignment (assignValue, parentsOfLiteral)
import           Satyros.DPLL.Effect     (DPLL)
import           Satyros.DPLL.Storage    (assignment, unassignedVariables,
                                          variableLevels)

assignDecisionVariable :: CNF.Literal -> DPLL ()
assignDecisionVariable l@(CNF.Literal _ x) = do
  unassignedVariables %= Set.delete x
  assignment %= assignValue l Nothing
  variableLevels %= ((x, Set.empty) :)

assignImplicationVariable :: CNF.Literal -> CNF.Clause -> DPLL ()
assignImplicationVariable l@(CNF.Literal _ x) c = do
  unassignedVariables %= Set.delete x
  assignment %= assignValue l (Just c)
  variableLevels . ix 0 . _2 %= Set.insert x

deriveConflictClauseRelSAT :: CNF.Clause -> DPLL CNF.Clause
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
