module Testsuite.Random where

-- standard modules
import Control.Monad
import Control.Monad.Random
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Dipole
import Calculus.FlipFlop
import Helpful
import Helpful.Random

import Debug.Trace


-- | consistent networks

randomAtomicNetwork :: (Calculus a)
                          => Int
                          -> [a]
                          -> Int
                          -> IO (Network [String] a)
randomAtomicNetwork rank domain syze = do
    gen <- newStdGen
    let rels = randomsOf domain gen
    let cons = Map.fromList $
            zip (kCombinations rank $ map show [1..syze]) rels
    let net = eNetwork { nDesc = "Random Network", nCons = cons }
    return net

randomConnectedAtomicNetwork :: (Calculus a)
                             => Int
                             -> [a]
                             -> Int
                             -> Float
                             -> IO (Network [String] a)
randomConnectedAtomicNetwork rank domain syze density = do
    cons <- liftM (Map.fromList . fst) $ foldM
                (\(consAcc, nodesAcc) intNode -> do
                    gen1 <- newStdGen
                    gen2 <- newStdGen
                    gen3 <- newStdGen
                    let node = show intNode
                        combis = kCombinations (rank - 1) nodesAcc
                        rels = randomsOf domain gen1
                        rolls = randomRs (0, 1) gen2 :: [Float]
                        randomCombi = fst $ oneOf combis gen3
                        newCons = mapMaybe
                            (\(combi, rel, roll) ->
                                if roll < density then
                                    Just (combi ++ [node], rel)
                                else
                                    Nothing
                            ) $ zip3 combis rels rolls
                    return ( (consAcc ++) $
                               if null newCons then
                                   [(randomCombi ++ [node], head $ rels)]
                               else
                                   newCons
                           , nodesAcc ++ [node]
                           )
                ) ([], (map show [1..rank - 1])) [rank..syze]
    return $ eNetwork { nDesc = "Random Network", nCons = cons }


-- | inconsistent networks

{-

indianTent :: Int -> Network [String] (Set.Set FlipFlop)
indianTent n
    | n < 3      = eNetwork
    | otherwise  = Network
        { nDesc = show n ++ " nodes, inconsistent, " ++
            " following the rule { p_i p_j p_k (l) | 1 <= i < j < k <= " ++
            show n ++ " } except for p_1 p_2 p_" ++ show n ++ " (r) and\
            \ p_2 p_3 p_" ++ show n ++ " (r)."
        , nCalc = "flipflop-3"
        , nNumOfNodes = Just n
        , nCons = Map.insert ["0", "1", show $ n - 1] (Set.singleton R) $
            Map.insert ["1", "2", show $ n - 1] (Set.singleton R) $ Map.fromList $ foldl
                (\acc i -> (++ acc) $ foldl
                    (\acc2 j -> (++ acc2) $ map
                        (\k -> ([show i, show j, show k], Set.singleton L)
                        ) [j + 1..n - 1]
                    ) [] [i + 1..n - 2]
                ) [] [0..n - 3]
        }

-}
