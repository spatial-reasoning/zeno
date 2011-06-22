module Helpful where

import Data.Maybe (listToMaybe)
import qualified Data.Set as Set

-- return a list of all subsets of s of size n
subsetsN :: (Ord a) => Set.Set a -> Int -> [Set.Set a]
subsetsN s n
    | n == 0  = [Set.empty]
    | Set.null s  = []
    | otherwise  = (++)
        ( map (Set.insert minElement) (subsetsN s_wo_min (n-1)) )
        ( subsetsN s_wo_min n )
    where (minElement, s_wo_min) = Set.deleteFindMin s

-- return subsets of s, in increasing order of size
subsetsSmallToLarge :: (Ord a) => Set.Set a -> [Set.Set a]
subsetsSmallToLarge s = concatMap (subsetsN s) [0..n]
    where n = Set.size s

-- return subsets of s, in decreasing order of size
subsetsLargeToSmall :: (Ord a) => Set.Set a -> [Set.Set a]
subsetsLargeToSmall s = concatMap (subsetsN s) [n, n-1..0]
    where n = Set.size s

-- return one smallest subset of s satisfying the filter predicate
minFilterSubset :: (Ord a) => ((Set.Set a) -> Bool)
                           -> (Set.Set a)
                           -> Maybe (Set.Set a)
minFilterSubset p s = listToMaybe $ filter p $ subsetsSmallToLarge s

-- return one largest subset of s satisfying the filter predicate
maxFilterSubset :: (Ord a) => ((Set.Set a) -> Bool)
                           -> (Set.Set a)
                           -> Maybe (Set.Set a)
maxFilterSubset p s = listToMaybe $ filter p $ subsetsLargeToSmall s

