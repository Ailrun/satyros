module Satyros.DPLL.StorageUtil where

import           Control.Lens            (_2, _Just, filtered, folded, ix, to,
                                          use, uses, (%=), (^.), (^..))
import           Data.Maybe              (isNothing)
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
  pure . CNF.Clause . Set.toList . go asgn vl $ CNF.unClause c
  where
    go asgn vl ls
      | null psAtCurrent = Set.fromList lsWithoutP <> Set.fromList psAtLower
      | otherwise = Set.fromList lsWithoutP <> Set.fromList psAtLower <> go asgn vl psAtCurrent
      where
        psAtCurrent = filter ((`Set.member` vl) . CNF.literalToVariable) ps
        psAtLower = filter ((`Set.notMember` vl) . CNF.literalToVariable) ps

        ps = map CNF.negateLiteral ls >>= \l ->
          asgn ^.. parentsOfLiteral l . _Just . _Just . to CNF.unClause . folded . filtered (/= l)

        lsWithoutP = filter (maybe False isNothing . (asgn ^.) . parentsOfLiteral) ls
        -- vs = partition (isNothing . snd) . zip ls $ getParents asgn <$> ls
