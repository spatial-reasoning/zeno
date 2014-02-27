module Helpful.Combinatorics
    ( kVariations
    ) where

import Data.List

kVariations k xs
    | k < 0      = []
    | otherwise  = kVariations' k xs

kVariations' 0 _  = [[]]
kVariations' k xs = concatMap (\x -> map (x:) foo) xs
  where
    foo = kVariations' (k-1) xs
