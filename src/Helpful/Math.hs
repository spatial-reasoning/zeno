module Helpful.Math where

import Data.Ratio

class (Num a, Ord a) => Exact a where
    myFromThen     :: a -> a -> [a]
    myFromThen n m =
        n `seq` m `seq` (n : myFromThen m (m+m-n))
    range :: a -> a -> a -> [a]
    range a b c = 
        takeWhile predicate (myFromThen a b)
      where
        predicate | a <= b    = (<= c)
                  | otherwise = (>= c)

instance Exact Int
instance Exact Integer
instance (Exact a, Integral a) => Exact (Ratio a) where
    {-# SPECIALIZE instance Exact Rational #-}



-- Useful Stuff for Rational --------------------------------------------------


divR     :: (RealFrac a, Integral b) => a -> a -> b
divR a b = floor $ (/) a b

-- find the numerator x that is closest to y.
closestNumeratorTo     :: (Integral a, Real b) => a -> b -> a
closestNumeratorTo x y = (round $ realToFrac $ y * fromIntegral x)

-- find the ratio with numerator x that is closest to y.
closestRatioTo     :: (Integral a, Real b) => a -> b -> Ratio a
closestRatioTo x y = (closestNumeratorTo x y) % x


-- general math stuff ---------------------------------------------------------

mean    :: (Fractional a) => [a] -> a
mean ls = sum ls / realToFrac (length ls)

choose     :: Integral a => a -> a -> a
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k 

