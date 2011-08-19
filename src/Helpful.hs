module Helpful where

import Control.Concurrent
import Data.Maybe (listToMaybe)
import qualified Data.List as List
import qualified Data.Set as Set
import System.Directory
import System.Random

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

