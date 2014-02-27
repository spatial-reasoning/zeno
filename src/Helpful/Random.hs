module Helpful.Random where

import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Random

randomRsIO :: (Random a) => (a, a) -> IO [a]
randomRsIO range = do
    gen <- newStdGen
    return $ randomRs range gen

-- Functions to get random values of a bounded, enumerated data type.
beRandomR :: (Enum a, RandomGen g) => (a, a) -> g -> (a, g)
beRandomR (l, u) =
    (\(x,y) -> (toEnum x, y)) . randomR (fromEnum l, fromEnum u)

beRandomRs :: (Enum a, RandomGen g) => (a, a) -> g -> [a]
beRandomRs (l, u) = map toEnum . randomRs (fromEnum l, fromEnum u)

beRandomRIO :: (Enum a) => (a, a) -> IO a
beRandomRIO range = getStdRandom (beRandomR range)

beRandomRsIO :: (Enum a) => (a, a) -> IO [a]
beRandomRsIO range = do
    gen <- newStdGen
    return $ beRandomRs range gen

beRandom :: (Bounded a, Enum a, RandomGen g) => g -> (a, g)
beRandom = beRandomR (minBound, maxBound)

beRandoms :: (Bounded a, Enum a, RandomGen g) => g -> [a]
beRandoms g = (\(x,g') -> x : beRandoms g') (beRandom g)

beRandomIO :: (Bounded a, Enum a) => IO a
beRandomIO = getStdRandom beRandom


-- Functions to pick random values from a given list of values.
oneOf :: (RandomGen g) => [a] -> g -> (a, g)
oneOf domain = (\(x, y) -> (domain!!x, y)) . (randomR (0, (length domain) - 1))

oneOfIO :: [a] -> IO a
oneOfIO = getStdRandom . oneOf

randomsOf :: (RandomGen g) => [a] -> g -> [a]
randomsOf domain = map (domain!!) . (randomRs (0, (length domain) - 1))

randomsOfIO :: [a] -> IO [a]
randomsOfIO domain = do
    gen <- newStdGen
    return $ randomsOf domain gen

oneGeneralOf :: (Eq a, Ord a, RandomGen g) => [a] -> g -> (Set.Set a, g)
oneGeneralOf domain gen = (\(x,y) -> (Set.fromList x, y)) $ foldr
    (\ x (acc, g) ->
      let
        (weWantItIn, g') = randomR (0.0 :: Float, 1.0) g
      in
        if weWantItIn < 0.2 then (x:acc, g') else (acc, g')
    ) ([rel1], gen') domain
  where
    (rel1, gen') = oneOf domain gen

oneGeneralOfIO :: (Eq a, Ord a) => [a] -> IO (Set.Set a)
oneGeneralOfIO = getStdRandom . oneGeneralOf

randomGeneralsOf :: (Eq a, Ord a, RandomGen g) => [a] -> g -> [Set.Set a]
randomGeneralsOf domain gen = x : randomGeneralsOf domain g
  where
    (x, g) = oneGeneralOf domain gen

randomGeneralsOfIO :: (Eq a, Ord a) => [a] -> IO [Set.Set a]
randomGeneralsOfIO domain = do
    gen <- newStdGen
    return $ randomGeneralsOf domain gen

-- | Purely functional O(n log n) random shuffle algorithm.
--   cf. http://www.haskell.org/haskellwiki/Random_shuffle#Purely_functional
--   and http://okmij.org/ftp/Haskell/AlgorithmsH.html#perfect-shuffle
shuffle'' :: RandomGen g => [a] -> g -> ([a], g)
shuffle'' [] gen = ([], gen)
shuffle'' l  gen = 
  toElems $ foldl shuffle''Step (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen = (Map.singleton 0 x, gen)
    shuffle''Step :: RandomGen g
                  => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
    shuffle''Step (m, gen) (i, x) =
        ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
      where
        (j, gen') = randomR (0, i) gen
 
