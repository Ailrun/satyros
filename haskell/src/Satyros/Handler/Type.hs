module Satyros.Handler.Type where

import           Data.Set            (Set)
import qualified Satyros.BellmanFord as BellmanFord
import qualified Satyros.DPLL        as DPLL
import qualified Satyros.QFIDL       as QFIDL

type InternalStorage = (QFIDL.ConversionTable, BellmanFord.IDLGraph, BellmanFord.Storage, Set (BellmanFord.IDLGraphVertex , BellmanFord.IDLGraphVertex))
type Storage = DPLL.Storage InternalStorage
