module Testsuite.Random where

-- standard modules
import Control.Monad
import Data.List
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
                               -> Int
                               -> IO (Network [String] a)
randomAtomicNetworkWithDensity rank domain syze numerator = do
    combis <- sampleRVar $ shuffle $ kCombinations rank $ map show [1..syze]
    rels <- randomsOfIO domain
    let cons = Map.fromList $ take numerator $ zip combis rels
    return $ eNetwork { nDesc = "Random_Network", nCons = cons }

randomConnectedAtomicNetworkWithDensity :: (Calculus a)
                                        => Int
                                        -> [a]
                                        -> Int
                                        -> Int
                                        -> IO (Network [String] a)
randomConnectedAtomicNetworkWithDensity rank domain syze numerator = do
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
                                     --                code. Fixme: this should
                                     --                be handled better by
                                     --                internal generating
                                     --                functions.
                                     --v--------v--
                      let newCon = [(combi ++ [node], rel)]
                      return $ consAcc ++ newCon
                  ) [] [rank..syze]
    let combisLeft =
            (kCombinations rank $ map show [1..syze]) \\ (fst $ unzip skel)
    fleshCombis <- sampleRVar $ shuffle combisLeft
    fleshRels <- randomsOfIO domain
    let flesh = take (numerator - syze + rank - 1) $ zip fleshCombis fleshRels
    let cons = Map.fromList $ skel ++ flesh
    return $ eNetwork { nDesc = "Random_Network", nCons = cons }

randomAtomicNetworkAroundDensity :: (Calculus a)
                                 => Int
                                 -> [a]
                                 -> Int
                                 -> Int
                                 -> Int
                                 -> IO (Network [String] a, Int)
randomAtomicNetworkAroundDensity rank domain syze numerator' denomin = do
    let numerator = fixNumeratorAtEdge numerator' denomin
    numer <- sampleBinomial numerator denomin
    net <- randomAtomicNetworkWithDensity rank domain syze numer
    return (net, numer)

randomConnectedAtomicNetworkAroundDensity :: (Calculus a)
                                          => Int
                                          -> [a]
                                          -> Int
                                          -> Int
                                          -> Int
                                          -> IO (Network [String] a, Int)
randomConnectedAtomicNetworkAroundDensity rank domain syze numerator' denomin =
  do
    let numerator = fixNumeratorAtEdge numerator' denomin
    numer <- sampleBinomial numerator denomin
    net <- randomConnectedAtomicNetworkWithDensity rank domain syze numer
    return (net, numer)

fixNumeratorAtEdge numerator denomin
    | numerator == 0        = 0.5
    | numerator == denomin  = fromIntegral numerator - 0.5
    | otherwise             = fromIntegral numerator

sampleBinomial numerator denomin = sampleRVar $
    binomial (denomin :: Int) (numerator / fromIntegral denomin :: Float)

