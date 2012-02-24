{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Benchmark where

-- standard modules
import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Ratio
import System.IO
import System.IO.Unsafe
import Text.Printf

-- local modules
import Basics
import Export
import Parsing.Qstrlib
import Testsuite.Random
import Helpful

import Debug.Trace



type Benchmark = Map.Map Int   -- maps number of nodes to following attributes
    ( Int                      -- number of networks tested so far
    , Ratio Int                -- last used density
    , Map.Map (Ratio Int)      -- maps a density to the following attributes
        ( Int                  -- # of no
        , Int                  -- # of yes
        , Int                  -- # of undecided
        , Int                  -- # of timeouts
        , Map.Map String       -- maps a name of an algorithm to the following
                               -- attributes
            ( Int              -- # of no
            , Int              -- # of yes
            , Int              -- # of undecided
            , Int              -- # of timeouts
            , Map.Map (Maybe Bool, Int) Int
                               -- maps the rank of fastness (fst, snd, ...) to
                               -- the number of times it was achieved, first
                               -- key is the correct answer given. Only correct
                               -- answers are counted.
            , Double           -- average time (in seconds) needed to check one
                               -- network, regardless of success
            )
        )
    )



markTheBench
    minsize maxsize testThisManyNets funs tymeout rank relations bench = do
    let numOfNodes' = filter
            (\a -> maybe
                True
                (\(a,_,_) -> a < testThisManyNets) $
                Map.lookup a bench
            ) [minsize..maxsize]
    myExit <- timeout 500 getLine
    if maybe False (isInfixOf "q") myExit then do
        putStrLn "Quit requested by user."
    else if null numOfNodes' then do
        putStrLn "Finished."
    else do
        let numOfNodes = head numOfNodes'
        let denomin = choose numOfNodes rank
        let (nOfTestedNet, targetNumer) = maybe
                ( maybe
                    (1, closestNumeratorTo denomin 0.5)
                    (\(_,a,_) -> (1, closestNumeratorTo denomin a)
                    ) $ Map.lookup (numOfNodes - 1) bench
                )
                (\(a,b,c) ->
                    (\(d,e,f,g,_) ->
                      let
                        -- if all methods time out try to generate more
                        -- inconsistent networks.
                        sicnum = signum $ e + f + g - d
                      in
                        ( a + 1
                        , min denomin $ max 0 $
                              round (b * fromIntegral denomin) + sicnum
                        )
                    ) $ (Map.!) c b
                ) $ Map.lookup numOfNodes bench
        (net, results) <-
            checkNetwork rank relations funs tymeout numOfNodes targetNumer
        let targetDens = targetNumer%denomin
        let actualDens = targetDens
        saveSpecialNet
            net results numOfNodes nOfTestedNet targetDens actualDens
        when (Map.null bench) (putStrLn $ showProcedures results)
        putStrLn $ showResults results ++
            (showInfo numOfNodes nOfTestedNet targetDens actualDens )
        let newBench = addToBench
                bench numOfNodes targetDens actualDens net results
        writeFile "RESULTS.BENCHMARK" $ show newBench
        appendFile "RESULTS.NETS" $ show net ++ "\n"
        appendFile "RESULTS.RESULTS" $ show results ++ "\n"
        markTheBench
          minsize maxsize testThisManyNets funs tymeout rank relations newBench


addToBench :: (Calculus a)
           => Benchmark
           -> Int -> (Ratio Int) -> (Ratio Int)
           -> (Network [String] (Set.Set a))
           -> [(String, (Double, Maybe (Maybe Bool)))]
           -> Benchmark
addToBench bench numOfNodes targetDens actualDens net results =
    case Map.lookup numOfNodes bench of
        Nothing -> Map.insert numOfNodes
            ( 1
            , targetDens
            , Map.singleton actualDens newDensList
            ) bench
        Just (bN, bD, bM) -> Map.insert numOfNodes
            ( bN + 1
            , targetDens
            , case Map.lookup actualDens bM of
                  Nothing -> Map.insert actualDens newDensList bM
                  Just (dn, dy, du, dt, dm) -> Map.insert
                      actualDens
                      ( dn + a, dy + b, du + c, dt + d
                      , foldl
                          (\acc (desc, (tyme, answer)) ->
                            let
                              (e, f, g, h, m, oldTyme) =
                                  (Map.!) dm desc
                              descIndex =
                                  (1 +) $ fromJust $ elemIndex
                                      desc sortedDescs
                              newM = Map.insertWith (+)
                                  (fromJust answer, descIndex) 1 m
                              (nE, nF, nG, nH, nM) = case answer of
                                  Just (Just False) ->
                                      (e + 1, f, g, h, newM)
                                  Just (Just True) ->
                                      (e, f + 1, g, h, newM)
                                  Just Nothing ->
                                      ( e, f, g + 1, h
                                      , if (==) totalResult
                                                (Just Nothing)
                                        then
                                            newM
                                        else
                                            m
                                      )
                                  otherwise ->
                                      (e, f, g, h + 1, m)
                            in
                              Map.insert
                                  desc
                                  ( nE, nF, nG, nH, nM
                                  , let
                                      efgh = fromIntegral $
                                                 e + f + g + h
                                    in
                                      (oldTyme * efgh + tyme) /
                                      (efgh + 1)
                                  ) acc
                          ) dm results
                      ) bM
            ) bench
  where
    plainResults = map (snd . snd) results
    false = elem (Just (Just False)) plainResults
    true  = elem (Just (Just True)) plainResults
    nothing = elem (Just Nothing) plainResults
    (a, b, c, d, totalResult) = case (false, true, nothing) of
        (True, True, _)      -> saveContradictingResults
            numOfNodes actualDens net results
        (True, False, _)     -> (1, 0, 0, 0, Just (Just False))
        (False, True, _)     -> (0, 1, 0, 0, Just (Just True))
        (False, False, True) -> (0, 0, 1, 0, Just Nothing)
        otherwise            -> (0, 0, 0, 1, Nothing)
    sortedDescs = map fst $ sortBy
        (\(_,(x,_)) (_,(y,_)) -> compare x y) $
        filter ((totalResult ==) . snd . snd) results
    newDensList =
        ( a, b, c, d
        , Map.fromList $ map
            (\(desc, (tyme, answer)) ->
                let
                    descIndex = (1 +) $ fromJust $ elemIndex
                        desc sortedDescs
                    (e, f, g, h, m) = case answer of
                        Just (Just False) ->
                            ( 1, 0, 0, 0
                            , Map.singleton
                                (Just False, descIndex) 1
                            )
                        Just (Just True) ->
                            ( 0, 1, 0, 0
                            , Map.singleton
                                (Just True, descIndex) 1
                            )
                        Just Nothing ->
                            ( 0, 0, 1, 0
                            , if totalResult == Just Nothing then
                                Map.singleton
                                  (Nothing, descIndex) 1
                              else
                                Map.empty
                            )
                        Nothing -> (0, 0, 0, 1, Map.empty)
                in
                ( desc, (e, f, g, h, m, tyme) )
            ) results
        )

checkNetwork rank relations funs tymeout size numerator = do
    net <- liftM makeNonAtomic $
        randomConnectedAtomicNetworkWithDensity rank relations size numerator
    results <- sequence $ map
                  (\(desc, fun) -> do
                      res <- timeIt $ timeoutP (tymeout * 1000000) $ fun net
                      return $ (desc, res)
                  ) funs
    return (net, results)


-- find phase transition ------------------------------------------------------

-- d = denominator of resulting ratio (== choose numOfNodes rank),
-- n = numOfNodes, m = map
probablePhasetransition d n m =
    densityWithRatioOfFoundNetworksClosestTo 0.1 m $
    intervalWithMeanOfRatiosOfFoundNetworksClosestTo
        0.5 (standardDeviation n) d m

-- r = target ratio (inconsistent / consistent et.al.),
-- l = length of interval, d = denominator, m = Map
intervalWithMeanOfRatiosOfFoundNetworksClosestTo r l d m =
    minimumBy comphare ivals
  where
--  ivals = [ [x, x + 1%d .. x + eris * 1%d] | x <- [0, 1%d .. (d - eris)%d ] ]
    ivals = [ [x, x + 1%d .. x + eris * 1%d]
            | let theMiddle = closestRatioTo d 0.5
            , x <- [theMiddle, theMiddle - 1%d .. 0] ++
                   [theMiddle + 1%d, theMiddle + 2%d .. (d - eris)%d]
            ]
    eris = fromIntegral $ round $ l / (1%d)
    comphare xs ys = compare
                         (abs $ aver (ratios xs) - r)
                         (abs $ aver (ratios ys) - r)
    aver [] = 0   -- maybe this should be something else?
    aver xs = mean xs
    ratios = map ratioFoo . mapMaybe (flip Map.lookup m)

-- r = target ratio, m = map, ls = list
densityWithRatioOfFoundNetworksClosestTo r m ls =
    minimumBy comphare ls
  where
    comphare x y = compare (abs $ ratioAt x - r) (abs $ ratioAt y - r)
    ratioAt x = maybe 0 ratioFoo $ Map.lookup x m

ratioFoo (n,y,u,t,_) = n % (n + y + u + t)

-- n =  numOfNodes rank
standardDeviation n = 1%20     -- FIXME


-- load and save --------------------------------------------------------------

loadBench fileName = do
    benchString <- catch
        (readFile fileName)
        ((\e -> fail $ "File \"" ++ fileName ++ "\" not found."
         ) :: SomeException -> IO String
        )
    bench <- catch
        (readIO benchString)
        ((\e -> fail $ "The file \"" ++ fileName ++
                       "\" does not seem to contain a benchmark."
         ) :: SomeException -> IO String
        )
    return bench

saveContradictingResults numOfNodes dens net results = unsafePerformIO $ do
    appendFile "RESULTS.ERROR" $
        " Number of Nodes: " ++ show numOfNodes ++
        " | Density: " ++ show dens ++ "\n\n" ++
        showNonAtomicNet net ++ "\n" ++
        showProcedures results ++ "\n" ++
        showResults results ++ "\n\n\n"
    error $ "Results contradict each other. Results saved to " ++
            "RESULTS.ERROR"

saveSpecialNet net results numOfNodes nOfTestedNet targetDens actualDens = do
    if    ( elem (length $ filter
                               (\(_, (_, answer)) ->
                                   answer == Just (Just False)
                               ) results
                ) [1,2] )
       || (    ( length $ filter
                              (\(_, (_, answer)) ->
                                  answer == Just (Just False)
                              ) results
               ) > 0
            &&
               elem (length $ filter
                                  (\(_, (_, answer)) ->
                                      answer == Just Nothing
                                  ) results
                    ) [1,2] )
    then
        appendFile "RESULTS.SPECIAL" $
        showInfo numOfNodes nOfTestedNet targetDens actualDens ++ "\n" ++
        showNonAtomicNet net ++ "\n" ++
        showProcedures results ++ "\n\n" ++
        showResults results ++ "\n\n"
    else
        return ()



-- Show things ----------------------------------------------------------------

showInfo numOfNodes nOfTestedNet targetDens actualDens =
    (printf "%8d" (numOfNodes :: Int)) ++ " │ " ++
    (printf "%8d" (nOfTestedNet :: Int)) ++ " │ " ++
    (printf "%4d" (numerator targetDens :: Int)) ++ " / " ++
    (printf "%4d" (denominator targetDens :: Int)) ++ " │ " ++
    (printf "%4d" (numerator actualDens :: Int)) ++ " / " ++
    (printf "%4d" (denominator actualDens :: Int)) ++ " │ "

showProcedures results = " │ " ++
    foldl (\acc (desc, _) -> acc ++ align desc ++ " │ ") "" results ++
    foldl (\acc str -> acc ++ align str ++ " │ ") ""
        [ "# nodes"
        , "# nets"
        , "Target Dens"
        , "Actual Dens" ] ++
    "\n" ++ " ├──────────" ++
    concat (replicate (length results + 1) "┼──────────") ++
    concat (replicate 2 "┼─────────────") ++ "┤"
  where
    align str
        | diff < 1  = str
        | otherwise = replicate (floor   $ fromIntegral diff / 2) ' ' ++ str ++
                      replicate (ceiling $ fromIntegral diff / 2) ' '
      where
        diff = 8 - length str

showResults results = " │" ++
    foldl
        (\acc (_, (elapsed, answer)) -> acc ++
            showAnswer answer ++ ": " ++
            (printf "%3d" ((round elapsed) :: Int)) ++ "s │"
        ) "" results ++ " "

showAnswer :: Maybe (Maybe Bool) -> String
showAnswer (Just (Just False)) = " - "
showAnswer (Just (Just True))  = " + "
showAnswer (Just Nothing)      = " o "
showAnswer Nothing             = " x "

