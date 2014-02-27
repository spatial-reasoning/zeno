module Testsuite.Chirotope
    ( Orientation
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Basics
import Helpful.Combinatorics
import Helpful.Math

data Orientation = M | N | P deriving (Ord, Eq, Enum, Bounded)

instance Show Orientation where
    show M = "-"
    show N = "0"
    show P = "+"

instance Read Orientation where
    readsPrec d x = case x of
        "-"  -> [(M,"")]
        "0"  -> [(N,"")]
        "+"  -> [(P,"")]
        _    -> []

--allOrientationNetworks :: Int -> Set.Set (Map.Map [Int] Orientation)
allOrientationNetworks d n =
    (\ (x,y) -> x ++ [head y]) $ break
    (== (replicate (numberOfTuples - eris) N ++ replicate eris M)) $
    kVariations numberOfTuples [minBound..maxBound]
  where
    numberOfTuples = choose n d
    eris = choose (n-1) (d-1)


