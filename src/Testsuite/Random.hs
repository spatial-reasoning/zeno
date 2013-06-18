module Testsuite.Random where

-- standard modules
import Control.Monad
import Data.List
import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Data.Maybe
import Data.Random ()
import qualified Data.Random.Extras as R
import Data.Ratio
import Data.RVar
import System.Random

-- local modules
import Basics
import Interface.Sparq
import Helpful.General
import Helpful.Math
import Helpful.Random


randomScenario :: (Calculus a)
               => Int
               -> [a]
               -> Int
               -> IO (Network [String] (ARel a))
randomScenario rank domain syze = do
    rels <- randomsOfIO domain
    let cons = fromJust $ consFromList $
            zip (kCombinations rank $ map show [1..syze])
                (map ARel rels)
    let net = eNetwork { nDesc = "Random_Network", nCons = cons }
    return net

randomAClosureConsistentScenario :: (Calculus a)
                                 => Int
                                 -> [a]
                                 -> Int
                                 -> IO (Network [String] (ARel a))
randomAClosureConsistentScenario rank domain syze = do
    let t1:t2:tuples = kCombinations rank $ map show [1..syze]
    rel1 <- oneOfIO atomicDomain
    rel2 <- oneOfIO atomicDomain
    gen <- newStdGen
    let maybeCons = buildScenario gen [ (t2,rel2), (t1,rel1) ] tuples
    if isNothing maybeCons then
        error $ "I could not find an algebraically closed "
                ++ cName (head domain) ++ " scenario of size "
                ++ show syze
    else do
        let net = eNetwork { nDesc = "Random_Network"
                           , nCons = fromJust $ consFromList $
                                     fromJust maybeCons }
        return net
  where
    atomicDomain = map ARel domain
    aClosureInconsistent cons = (Just False ==) $ ( \(x,_,_) -> x) $
        algebraicClosure $ makeNonAtomic $
        eNetwork{ nCons = fromJust $ consFromList cons }
    buildScenario gen cons tuples =
        if aClosureInconsistent cons then
            Nothing
        else if null tuples then
            Just cons
        else
            listToMaybe $ catMaybes scenarios
      where
        (rels, gen') = shuffle atomicDomain gen
        (tuple:tuples') = tuples
        scenarios = map
            (\ rel -> buildScenario gen'
                ((tuple, rel):cons)
                tuples'
            ) rels

randomAtomicNetworkWithDensity :: (Calculus a)
                               => Int
                               -> [a]
                               -> Int
                               -> Ratio Int
                               -> IO (Network [String] (ARel a))
randomAtomicNetworkWithDensity rank domain syze density = do
    combis <- sampleRVar $ R.shuffle $ kCombinations rank $ map show [1..syze]
    rels <- randomsOfIO $ map ARel domain
    let denom = choose syze rank
    let (factor, rest) = divMod denom (denominator density)
    let numer = (numerator density) * factor
    let cons = fromJust $ Fold.foldrM (uncurry insertCon)
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
                                    -> IO (Network [String] (ARel a))
randomConnectedAtomicNetworkWithDensity rank domain syze density = do
    let atomicDomain = map ARel domain
    let combis = [ kCombinations (rank - 1) $ map show [1..n]
                 | n <- [rank - 1..] ]
    skel <- foldM (\consAcc intNode -> do
                      let node = show intNode
                      combi <- oneOfIO $ combis!!(intNode - rank)
                      rel <- oneOfIO atomicDomain
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
    fleshCombis <- sampleRVar $ R.shuffle combisLeft
    fleshRels <- randomsOfIO atomicDomain
    let denom = choose syze rank
    let (factor, rest) = divMod denom (denominator density)
    let numer = (numerator density) * factor
    let flesh = take (numer - syze + rank - 1) $ zip fleshCombis fleshRels
    let cons = fromJust $ Fold.foldrM (uncurry insertCon)
                                      Map.empty
                                      (skel ++ flesh)
    if rest /= 0 || numer < (syze - rank + 1) then
        error $ "Cannot create a connected atomic network of size "
             ++ show syze ++ " and density " ++ show density
    else
        return $ eNetwork { nDesc = "Random_Network", nCons = cons }

randomConnectedNetworkWithDensity :: (Calculus a)
                                  => Int
                                  -> [a]
                                  -> Int
                                  -> Ratio Int
                                  -> IO (Network [String] (GRel a))
randomConnectedNetworkWithDensity rank domain syze density = do
    let combis = [ kCombinations (rank - 1) $ map show [1..n]
                 | n <- [rank - 1..] ]
    skel <- foldM (\consAcc intNode -> do
                      let node = show intNode
                      combi <- oneOfIO $ combis!!(intNode - rank)
                      rel <- oneGeneralOfIO domain
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
                      let newCon = [(combi ++ [node], GRel rel)]
                      return $ consAcc ++ newCon
                  ) [] [rank..syze]
    let combisLeft =
            (kCombinations rank $ map show [1..syze]) \\ (fst $ unzip skel)
    fleshCombis <- sampleRVar $ R.shuffle combisLeft
    fleshRels <- liftM (map GRel) $ randomGeneralsOfIO domain
    let denom = choose syze rank
    let (factor, rest) = divMod denom (denominator density)
    let numer = (numerator density) * factor
    let flesh = take (numer - syze + rank - 1) $ zip fleshCombis fleshRels
    let cons = fromJust $ Fold.foldrM (uncurry insertCon)
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

