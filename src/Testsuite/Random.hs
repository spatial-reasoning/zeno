module Testsuite.Random where

-- standard modules
import Control.Monad
import Data.List
import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Data.Maybe
import Data.Random hiding (shuffle)
import Data.Random.Distribution.Binomial
import Data.Random.Extras
import Data.Ratio
import Data.RVar
import qualified Data.Set as Set

-- local modules
import Basics
import Helpful.General
import Helpful.Math
import Helpful.Random

import Debug.Trace


-- | consistent networks

randomScenario :: (Calculus a)
               => Int
               -> [a]
               -> Int
               -> IO (Network [String] a)
randomScenario rank domain syze = do
    rels <- randomsOfIO domain
    let cons = Map.fromList $
            zip (kCombinations rank $ map show [1..syze]) rels
    let net = eNetwork { nDesc = "Random_Network", nCons = cons }
    return net

randomAtomicNetworkWithDensity :: (Calculus a)
                               => Int
                               -> [a]
                               -> Int
                               -> Ratio Int
                               -> IO (Network [String] a)
randomAtomicNetworkWithDensity rank domain syze density = do
    combis <- sampleRVar $ shuffle $ kCombinations rank $ map show [1..syze]
    rels <- randomsOfIO domain
    let denom = choose syze rank
    let (factor, rest) = divMod denom (denominator density)
    let numer = (numerator density) * factor
    let cons = fromJust $ Fold.foldrM (uncurry insertConAtomic)
                                      Map.empty
                                      (take numer $ zip combis rels)
    if rest /= 0 then
        error $ "Cannot create a network of size " ++ show syze
             ++ " and density " ++ show density
    else
        return $ eNetwork { nDesc = "Random_Network", nCons = cons }

randomConnectedAtomicNetworkWithDensity :: (Calculus a)
                                        => Int
                                        -> [a]
                                        -> Int
                                        -> Ratio Int
                                        -> IO (Network [String] a)
randomConnectedAtomicNetworkWithDensity rank domain syze density = do
    let combis = [ kCombinations (rank - 1) $ map show [1..n]
                 | n <- [rank - 1..] ]
    skel <- foldM (\consAcc intNode -> do
                      let node = show intNode
                      combi <- oneOfIO $ combis!!(intNode - rank)
                      rel <- oneOfIO domain
                                     --     v--------- the order of these two
                                     --                is very important
                                     --                because the nodes of the
                                     --                networks are assumed to
                                     --                be sorted throughout the
                                     --                code.
                                     --                Fixme: this should
                                     --                be handled better by
                                     --                internal generating
                                     --                functions. We could need
                                     --                a function that
                                     --                generalises bcInsert and
                                     --                tcInsert.
                                     --v--------v--
                      let newCon = [(combi ++ [node], rel)]
                      return $ consAcc ++ newCon
                  ) [] [rank..syze]
    let combisLeft =
            (kCombinations rank $ map show [1..syze]) \\ (fst $ unzip skel)
    fleshCombis <- sampleRVar $ shuffle combisLeft
    fleshRels <- randomsOfIO domain
    let denom = choose syze rank
    let (factor, rest) = divMod denom (denominator density)
    let numer = (numerator density) * factor
    let flesh = take (numer - syze + rank - 1) $ zip fleshCombis fleshRels
    let cons = fromJust $ Fold.foldrM (uncurry insertConAtomic)
                                      Map.empty
                                      (skel ++ flesh)
    if rest /= 0 || numer < (syze - rank + 1) then
        error $ "Cannot create a connected network of size " ++ show syze
             ++ " and density " ++ show density
    else
        return $ eNetwork { nDesc = "Random_Network", nCons = cons }

-- fixme: these need to be adjusted to the new functions above:
--randomAtomicNetworkAroundDensity :: (Calculus a)
--                                 => Int
--                                 -> [a]
--                                 -> Int
--                                 -> Ratio Int
--                                 -> IO (Network [String] a, Int)
--randomAtomicNetworkAroundDensity rank domain syze density = do
--    let numer' = fixNumeratorAtEdge density
--    let denom = denominator density
--    numer <- sampleBinomial numer' denom
--    net <- randomAtomicNetworkWithDensity rank domain syze (numer%denom)
--    return (net, numer)
--
--randomConnectedAtomicNetworkAroundDensity :: (Calculus a)
--                                          => Int
--                                          -> [a]
--                                          -> Int
--                                          -> Ratio Int
--                                          -> IO (Network [String] a, Int)
--randomConnectedAtomicNetworkAroundDensity rank domain syze density =
--  do
--    let numer' = fixNumeratorAtEdge density
--    let denom = denominator density
--    numer <- sampleBinomial numer' denom
--    net <- randomConnectedAtomicNetworkWithDensity rank domain syze (numer%denom)
--    return (net, numer)
--
--fixNumeratorAtEdge density
--    | numer == 0      = 0.5
--    | numer == denom  = fromIntegral numer - 0.5
--    | otherwise       = fromIntegral numer
--  where
--    numer = numerator density
--    denom = denominator density
--
--sampleBinomial numerator denomin = sampleRVar $
--    binomial (denomin :: Int) (numerator / fromIntegral denomin :: Float)

