module Helpful.Random where

import System.Random


-- Functions to get random values of a bounded, enumerated data type.
beRandomR :: (Bounded a, Enum a, RandomGen g) => (a, a) -> g -> (a, g)
beRandomR (l, u) =
    (\(x,y) -> (toEnum x, y)) . randomR (fromEnum l, fromEnum u)

beRandom :: (Bounded a, Enum a, RandomGen g) => g -> (a, g)
beRandom = beRandomR (minBound, maxBound)

beRandomRs :: (Bounded a, Enum a, RandomGen g) => (a,a) -> g -> [a]
beRandomRs ival g = x : beRandomRs ival g' where (x,g') = beRandomR ival g

beRandoms :: (Bounded a, Enum a, RandomGen g) => g -> [a]
beRandoms g = (\(x,g') -> x : beRandoms g') (beRandom g)

beRandomRIO :: (Bounded a, Enum a) => (a,a) -> IO a
beRandomRIO range = getStdRandom (beRandomR range)

beRandomIO :: (Bounded a, Enum a) => IO a
beRandomIO = getStdRandom beRandom


-- Functions to pick random values from a given list of values.
randomsOf :: (RandomGen g) => [a] -> g -> [a]
randomsOf domain = map (domain!!) . (randomRs (0, (length domain) - 1))

oneOf :: (RandomGen g) => [a] -> g -> (a, g)
oneOf domain = (\(x, y) -> (domain!!x, y)) . (randomR (0, (length domain) - 1))
