module Testsuite.FlipFlop where

-- standard modules
import qualified Data.Map as Map
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.FlipFlop

import Debug.Trace

-- | consistent networks

stupidAllLeft :: Int -> Network [String] (ARel FlipFlop)
stupidAllLeft n
    | n < 3      = eNetwork
    | otherwise  = Network
        { nDesc = show n ++ " nodes, consistent."
        , nCalc = "flipflop-3"
        , nNumOfNodes = Just n
        , nCons = Map.fromList $ foldl
            (\acc i -> [([show i, show (i+1), show (i+2)], ARel L)] ++ acc
            ) [] [0..n - 3]
        }


allLeft :: Int -> Network [String] (ARel FlipFlop)
allLeft n
    | n < 3      = eNetwork
    | otherwise  = Network
        { nDesc = show n ++ " nodes, consistent, " ++
            " following the rule { p_i p_j p_k (l) | 1 <= i < j < k <= " ++
            show n ++ " }."
        , nCalc = "flipflop-3"
        , nNumOfNodes = Just n
        , nCons = Map.fromList $ foldl
            (\acc i -> (++ acc) $ foldl
                (\acc2 j -> (++ acc2) $ map
                    (\k -> ([show i, show j, show k], ARel L)
                    ) [j + 1..n - 1]
                ) [] [i + 1..n - 2]
            ) [] [0..n - 3]
        }


-- | inconsistent networks

indianTent :: Int -> Network [String] (ARel FlipFlop)
indianTent n
    | n < 3      = eNetwork
    | otherwise  = Network
        { nDesc = show n ++ " nodes, inconsistent, " ++
            " following the rule { p_i p_j p_k (l) | 1 <= i < j < k <= " ++
            show n ++ " } except for p_1 p_2 p_" ++ show n ++ " (r) and\
            \ p_2 p_3 p_" ++ show n ++ " (r)."
        , nCalc = "flipflop-3"
        , nNumOfNodes = Just n
        , nCons = Map.insert ["0", "1", show $ n - 1] (ARel R) $
            Map.insert ["1", "2", show $ n - 1] (ARel R) $ Map.fromList $ foldl
                (\acc i -> (++ acc) $ foldl
                    (\acc2 j -> (++ acc2) $ map
                        (\k -> ([show i, show j, show k], ARel L)
                        ) [j + 1..n - 1]
                    ) [] [i + 1..n - 2]
                ) [] [0..n - 3]
        }

