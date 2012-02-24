module Helpful.Random where

import Control.Monad
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

