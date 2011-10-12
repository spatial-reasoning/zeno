module Helpful where

import Control.Concurrent
import Control.Parallel.Strategies
import Control.Monad
import Data.Maybe (listToMaybe)
import qualified Data.List as List
import qualified Data.Set as Set
import System.Directory
import System.Random

-- Parallel map (n = number of parallel computations)
mapP n f xs = parBuffer n rseq $ map f xs

-- Parallel lists (n = number of parallel computations)
pList n xs = parBuffer n rseq $ xs

setUnion :: (Ord a) => Set.Set (Set.Set a) -> Set.Set a
setUnion s = Set.fold Set.union Set.empty s

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

createTempDir :: String -> IO FilePath
createTempDir dirName = do
    randGen <- newStdGen
    sysTmpDir <- catch (getTemporaryDirectory) (\_ -> return ".")
    let tmpDirPrefix = sysTmpDir ++ "/" ++ dirName ++ "_"
    tmpDir <- nonExistingDirName tmpDirPrefix
    createDirectory tmpDir
    return tmpDir
    where nonExistingDirName prefix = do
            randint <- randomRIO (1, 9999999 :: Int)
            let path = prefix ++ show randint
            fileExists <- doesFileExist path
            dirExists <- doesDirectoryExist path
            if (fileExists || dirExists) then nonExistingDirName prefix
            else return path

_fork1 :: (a -> IO b) -> a -> IO (MVar b)
_fork1 f x =
  do
    cell <- newEmptyMVar
    forkIO (do { result <- f x; putMVar cell result })
    return cell

_fork :: (a -> IO b) -> [a] -> IO [MVar b]
_fork f = mapM (_fork1 f)

_join :: [MVar b] -> IO [b]
_join = mapM takeMVar

parMapM :: (a -> IO b) -> [a] -> IO [b]
parMapM f xs = (_fork f xs) >>= _join

-- The findWithIndex function takes a predicate and a list and returns the pair
-- of the first element in the list satisfying the predicate and its index, or
-- Nothing if there is no such element. 
findWithIndex :: (Num a, Enum a) => (b -> Bool) -> [b] -> Maybe (a, b)
findWithIndex = (. zip [0..]) . List.find . (. snd)

-- interweave x [a, b] = [[x, a, b], [a, x, b], [a, b, x]]
interweave :: t -> [t] -> [[t]]
interweave x [] = [[x]]
interweave x yys@(y:ys) = [x:yys] ++ map (y:) (interweave x ys)

-- returns a list of all k-permutations of a list
kPermutations :: Num a => a -> [a1] -> [[a1]]
kPermutations 0 _ = [[]]
kPermutations _ [] = []
kPermutations k (x:xs) = concatMap (interweave x) (kPermutations (k-1) xs)
                         ++ kPermutations k xs

-- combinations (where the order doesn't matter -- and therefore does...)
kCombinations :: Num a => a -> [a1] -> [[a1]]
kCombinations 0 _ = [[]]
kCombinations _ [] = []
kCombinations n (x:xs) = map (x:) (kCombinations (n-1) xs)
                         ++ kCombinations n xs

-- interweaveWithParity x [a, b] =
--     [ ([x, a, b], 1), ([a, x, b], -1), ([a, b, x], 1) ]
interweaveWithParity :: t -> ([t], Int) -> [ ([t], Int) ]
interweaveWithParity x ([], i) = [ ([x], i) ]
interweaveWithParity x yys@((y:ys), i) =
    (x:(y:ys), i) : ( map
                          ( \(a,b) -> (y:a,b) )
                          ( interweaveWithParity x (ys, (-i)) )
                    )

-- returns a list of all k-permutations of a list
permutationsWithParity :: [a] -> [ ([a], Int) ]
permutationsWithParity [] = [ ([], 1) ]
permutationsWithParity (x:xs) =
    concatMap
        ( interweaveWithParity x )
        ( permutationsWithParity xs )

kPermutationsWithParity :: Int -> [a] -> [ ([a], Int) ]
kPermutationsWithParity k x =
    concatMap permutationsWithParity $ kCombinations k x

-- This is some weired Voodoo stuff!
-- permutations :: [a] -> [[a]]
-- permutations xs0 = xs0 : perms xs0 []
--   where
--     perms []     _  = []
--     perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
--       where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
--             interleave' _ []     r = (ts, r)
--             interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
--                                      in  (y:us, f (t:y:us) : zs)



