{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Satyros.DPLL.Storage where

import           Control.Lens            (makeFieldsNoPrefix, set, to, (&),
                                          (.~), (^.))
import           Data.Set                (Set)
import           Data.Vector             (Vector)
import qualified Satyros.CNF             as CNF
import           Satyros.DPLL.Assignment (Assignment)
import           System.Random           (RandomGen (..), StdGen)

data Storage
  = Storage
    { _unassignedVariables :: Set CNF.Variable
    , _clauses             :: Vector CNF.Clause
    , _assignment          :: Assignment
    , _variableLevels      :: [(Maybe CNF.Variable, Set CNF.Variable)]
    , _stdGen              :: StdGen
    }
  deriving stock (Show)

makeFieldsNoPrefix ''Storage

instance RandomGen Storage where
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
