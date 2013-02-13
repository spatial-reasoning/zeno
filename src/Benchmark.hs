{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Benchmark where

-- standard modules
import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import qualified Data.Foldable as Fold
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Ratio
import Numeric
import System.IO
import System.IO.Unsafe
import Text.Printf

-- local modules
import Basics
import DecisionProcedure
import Export
import Parsing.Qstrlib
import Testsuite.Random

import Helpful
import Debug.Trace

-- improve: we should use records instead of tuples!
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
                               -- network, only in case of correct answers
            , Double           -- average time (in seconds) needed to check one
                               -- network, regardless of success
            )
        )
    )



markTheBench scenario batch minsize maxsize testThisManyNets
             funs tymeout rank relations dens bench = do
    let numOfNodes' = filter
            (\a -> maybe
                True
                (\(a,_,_) -> a < testThisManyNets) $
                Map.lookup a bench
            ) $ if maxsize >= 0 then [minsize..maxsize] else [minsize..]
    myExit <- if batch then
                  return False
              else do
                  hSetBuffering stdin NoBuffering
                  keyPressed 'q'
    if myExit then do
        putStrLn "Quit requested by user."
        return bench
    else if null numOfNodes' then do
        putStrLn "Finished."
        return bench
    else do
        let numOfNodes = head numOfNodes'
        let denomin = choose numOfNodes rank
        let minNumer = numOfNodes - rank + 1
        let (nOfTestedNet, targetNumer) = maybe
                ( maybe
                    (1, closestNumeratorTo denomin dens)
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
                        , min denomin $ max (numOfNodes - rank + 1) $
                              round (b * fromIntegral denomin) + sicnum
                        )
                    ) $ (Map.!) c b
                ) $ Map.lookup numOfNodes bench
        let targetDens =
                if scenario then
                    (1%1)
                else
                    targetNumer%denomin
        (net, results) <-
            checkNetwork scenario rank relations funs tymeout numOfNodes targetDens
        let actualDens = targetDens
        saveSpecialNet
            net results numOfNodes nOfTestedNet targetDens actualDens denomin minNumer
        when (Map.null bench) (putStrLn $ showProcedures results)
        putStrLn $ showResults results ++
            (showInfo numOfNodes nOfTestedNet targetDens actualDens denomin minNumer)
        let newBench = addToBench
                bench numOfNodes targetDens actualDens net results
        writeFile "BENCHMARK.COLLECTION" $ show newBench
        appendFile "BENCHMARK.ANSWERS" $ show (results, numOfNodes, actualDens) ++ "\n"
        markTheBench scenario batch minsize maxsize testThisManyNets
                     funs tymeout rank relations dens newBench


addToBench :: (Relation (a b) b, Calculus b)
           => Benchmark
           -> Int -> (Ratio Int) -> (Ratio Int)
           -> (Network [String] (a b))
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
                              (e, f, g, h, m, oldTyme, oldTyme2) =
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
                                      ef = fromIntegral $ e + f
                                    in
                                      if elem answer
                                              [ Nothing
                                              , Just Nothing ]
                                      then
                                        oldTyme
                                      else
                                        (oldTyme * ef + tyme) / (ef + 1)
                                  , let
                                      efgh = fromIntegral $ e + f + g + h
                                    in
                                      (oldTyme2 * efgh + tyme) / (efgh + 1)
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
                ( desc, (e, f, g, h, m, if elem answer [Nothing, Just Nothing] then 0 else tyme, tyme) )
            ) results
        )

checkNetwork scenario rank relations funs tymeout size dens = do
    net <- if scenario then
               randomScenario rank relations size
           else
               randomConnectedAtomicNetworkWithDensity rank relations size dens
    appendFile "BENCHMARK.NETS" $ show net ++ "\n"
    results <- sequence $ map
              (\DecisionProcedure{ decProName = desc, decProProc = fun } -> do
                  res <- timeIt $ timeoutP (tymeout * 1000000) $ fun net
                  return $ (desc, res)
              ) funs
    -- Delete this after the Opra testrun:
    let (resultTC'time, resultTC'answer)   = fromJust $ lookup "TC WitUn"  results
    let (resultBAC'time, resultBAC'answer) = fromJust $ lookup "BAC" results
    let mergedTcBac =
          ( "BAC + TC-WU"
          , case (resultTC'answer, resultBAC'answer) of
                (Just (Just False), Just (Just False)) ->
                    (min resultTC'time resultBAC'time, Just (Just False))
                (Just (Just False), _) -> (resultTC'time, Just (Just False))
                (_, Just (Just False)) -> (resultBAC'time, Just (Just False))
                (Just (Just True), Just (Just True)) ->
                    (min resultTC'time resultBAC'time, Just (Just True))
                (Just (Just True), _) -> (resultTC'time, Just (Just True))
                (_, Just (Just True)) -> (resultBAC'time, Just (Just True))
                (Nothing, _) -> (max resultTC'time resultBAC'time, Nothing)
                (_, Nothing) -> (max resultTC'time resultBAC'time, Nothing)
                otherwise -> (max resultTC'time resultBAC'time, Just Nothing)
          )
    let results' = results ++ [mergedTcBac]
    return (net, results')     -- delete
--    return (net, results)   -- restore


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
    appendFile "BENCHMARK.ERROR" $
        " Number of Nodes: " ++ show numOfNodes ++
        " | Density: " ++ show dens ++ "\n\n" ++
        showNetwork net ++ "\n" ++
        showProcedures results ++ "\n" ++
        showResults results ++ "\n\n\n"
    error $ "Results contradict each other. Results saved to " ++
            "BENCHMARK.ERROR"

saveSpecialNet net results numOfNodes nOfTestedNet targetDens actualDens denomin minNumer = do
    if    ( elem (length $ filter
                               (\(_, (_, answer)) ->
                                   answer == Just (Just False)
                               ) results
                ) [2] )
    then do
        appendFile "BENCHMARK.SPECIAL" $
            showNetwork net ++ "\n" ++
            showProcedures results ++ "\n" ++
            showResults results ++
            showInfo numOfNodes nOfTestedNet targetDens actualDens denomin minNumer ++
            "\n\n\n" ++ "-------------------------------------------------" ++
            "\n\n\n"
    else
        return ()



-- Show things ----------------------------------------------------------------

showInfo numOfNodes nOfTestedNet targetDens actualDens denomin minNumer =
    (printf "%8d" (numOfNodes :: Int)) ++ " │ "
    ++ (printf "%8d" (nOfTestedNet :: Int)) ++ " │ "
    ++ (printf "%4d" (numerator targetDens :: Int)) ++ " / "
    ++ (printf "%4d" (denominator targetDens :: Int)) ++ " │ "
    ++ (printf "%4d" (numerator minDens :: Int)) ++ " / "
    ++ (printf "%4d" (denominator minDens :: Int)) ++ " │ "
--    ++ (printf "%4d" (numerator actualDens :: Int)) ++ " / "
--    ++ (printf "%4d" (denominator actualDens :: Int)) ++ " │ "
  where
    minDens = minNumer % denomin

showProcedures results = " │ " ++
    foldl (\acc (desc, _) -> acc ++ align desc ++ " │ ") "" results ++
    foldl (\acc str -> acc ++ align str ++ " │ ") ""
        [ "# nodes"
        , "# nets"
        , "  Density  "
        , "Min Density"
--        , "Target Dens"
--        , "Actual Dens"
        ] ++
    "\n" ++ " ├──────────" ++
    concat (replicate (length results + 1) "┼──────────") ++
    concat (replicate 2 "┼─────────────") ++
    "┤"
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

-- Analyze the benchmark ------------------------------------------------------

-- here we plot for each network size a graph mapping the density to the
-- percentage of inconsistent networks found for that density.
plotPercentageOfInconsistentNetworksPerDensity bench = do
    writeFile "plotPercentageOfInconsistentNetworksPerDensity.plt" $
        plotScript ++ plotData
    safeReadProcess "gnuplot"
        ["plotPercentageOfInconsistentNetworksPerDensity.plt"] ""
  where
    (densSet, plotData) = Map.foldlWithKey
        (\ (acc1, acc2) numOfNodes (_, _, densMap) ->
            ( Set.union acc1 (Map.keysSet densMap)
            , (acc2 ++) $
                "\n" ++ "# # = " ++ show numOfNodes ++ Map.foldlWithKey
                    (\ acc3 dens (numN, numY, numU, numT, _) -> (acc3 ++)
                        "\n" ++
                        (show $ fromRational $ toRational dens) ++ " " ++
                        ( show $ 100 * fromIntegral numN /
                                       fromIntegral (numN + numY + numU + numT)
                        )
                    ) "" densMap
                ++ "\nend\n"
            )
        ) (Set.empty, "# \"Density\"  \"Percentage\"") bench
    plotScript =
        "set output 'plotPercentageOfInconsistentNetworksPerDensity.pdf'\n" ++
        "set terminal pdf font \"Times, 20\" monochrome size 5,3\n" ++
        "set lmargin at screen 0.09\n" ++
        "#set bmargin at screen 0.32\n" ++
        "#set bmargin at screen 0.1\n" ++
        "#set rmargin at screen 0.2\n" ++
        "#set tmargin at screen 0.2\n\n" ++
        "# Line style for grid\n" ++
        "#set style line 81 lt 0              # dashed\n" ++
        "#set style line 81 lt rgb \"#e0e0e0\"  # grey\n\n" ++
        "#set grid back linestyle 81\n" ++
        "set border 3 back # Remove border on top and right.\n\n"   ++
        "set xtics 0,0.1,0.6 nomirror\n" ++
        "set ytics 0,20,100  nomirror\n" ++
        "#set xtics 0,0.1,0.6\n" ++
        "#set ytics 0,0.1,0.6\n\n" ++
        "set xrange [" ++
        showFFloat (Just 2) (maybe 0.0 (realToFrac . fst) $ Set.minView densSet) "" ++ ":" ++
        showFFloat (Just 2) (maybe 1.0 (realToFrac . fst) $ Set.maxView densSet) "" ++ "]\n" ++
        "set yrange [0:100]\n\n" ++
        "#set xlabel \"x axis label\"\n" ++
        "#set ylabel \"y axis label\"\n\n" ++
        "#set log x\n" ++
        "#set mxtics 10    # Makes logscale look good.\n\n" ++
        "#set key font \",8\"\n" ++
        "set key bottom center outside horizontal\n\n" ++
        "plot " ++
        ( drop 5 $ intercalate ",\\\n" $ map
            (\ n ->
                "     '-' using 1:2 title \"# = " ++
                (show $ fst $ Map.elemAt n bench) ++ "\" with linespoints"
            ) [0..Map.size bench - 1]
        ) ++ "\n\n"

plotInconsistenciesPerSizeAndMethodInPercent :: Benchmark -> IO ((String, String))
plotInconsistenciesPerSizeAndMethodInPercent bench = do
    writeFile "plotPercentageOfInconsistenciesPerSizeAndMethod.dat" plotData
    writeFile "plotPercentageOfInconsistenciesPerSizeAndMethod.plt" plotScript
    safeReadProcess "gnuplot"
        ["plotPercentageOfInconsistenciesPerSizeAndMethod.plt"] ""
  where
    sizesSet = Map.keysSet bench
    refinedBench = answersPerSizeAndMethod bench
    allMethods = Map.foldr
        (\ (_, _, _, _, v) acc ->
            Set.union acc $ Map.keysSet v
        ) Set.empty refinedBench
    plotData = Set.foldl
            (\ acc x -> acc ++ "  \"" ++ x ++ "\""
            ) "\"#Nodes\"" allMethods ++
        "\n" ++ Map.foldrWithKey
            (\ k (totalInconsistent, _, _, _, v) acc -> (++ "\n" ++ acc) $
                show k ++ "    " ++ Set.foldr
                    (\ method acc2 -> (++ acc2) $
--                        -- absolute:
--                        maybe "-" (\ (x, _, _, _, _, _) -> show x)
--                      -- relative in percent:
                        maybe "-" (\ (inconsistent, _, _, _, _, _, _) -> show $
                                      100 * fromIntegral inconsistent /
                                            fromIntegral totalInconsistent
                                  )
                                  (Map.lookup method v)
                        ++ "    "
                    ) "" allMethods
            ) "" refinedBench
    plotScript =
        "set output 'plotPercentageOfInconsistenciesPerSizeAndMethod.pdf'\n" ++
        "set terminal pdf font \"Times, 20\" monochrome size 5,3\n" ++
        "set lmargin at screen 0.09\n" ++
        "#set bmargin at screen 0.32\n" ++
        "#set rmargin at screen 0.2\n" ++
        "#set tmargin at screen 0.2\n\n" ++
        "# Line style for grid\n" ++
        "#set style line 81 lt 0              # dashed\n" ++
        "#set style line 81 lt rgb \"#e0e0e0\"  # grey\n\n" ++
        "#set grid back linestyle 81\n" ++
        "set border 3 back # Remove border on top and right.\n\n"   ++
        "set xtics nomirror\n" ++
        "#set xtics 0,0.1,0.6 nomirror\n" ++
        "set ytics 0,20,100  nomirror\n" ++
        "#set xtics 0,0.1,0.6\n" ++
        "#set ytics 0,0.1,0.6\n\n" ++
        "set xrange [" ++
        show (maybe 0  ((+ (-1)) . fst) $ Set.minView sizesSet) ++ ":" ++
        show (maybe 47 ((+1) . fst) $ Set.maxView sizesSet) ++ "]\n" ++
        "set yrange [0:100]\n\n" ++
        "#set xlabel \"x axis label\"\n" ++
        "#set ylabel \"y axis label\"\n\n" ++
        "#set log x\n" ++
        "#set mxtics 10    # Makes logscale look good.\n\n" ++
        "set key bottom center outside horizontal\n\n" ++
        "plot " ++ (drop 5 $ intercalate ",\\\n" $ map
            (\ n ->
                "     'plotPercentageOfInconsistenciesPerSizeAndMethod.dat' using 1:" ++ show n ++
                " title column with linespoints"
            ) [2..Set.size allMethods + 1]
        ) ++ "\n\n"

plotSpeedPerSizeAndMethodSuccessOnly :: Benchmark -> IO ((String, String))
plotSpeedPerSizeAndMethodSuccessOnly bench = do
    writeFile "plotSpeedPerSizeAndMethodSuccessOnly.dat" plotData
    writeFile "plotSpeedPerSizeAndMethodSuccessOnly.plt" plotScript
    safeReadProcess "gnuplot"
        ["plotSpeedPerSizeAndMethodSuccessOnly.plt"] ""
  where
    sizesSet = Map.keysSet bench
    refinedBench = answersPerSizeAndMethod bench
    allMethods = Map.foldr
        (\ (_, _, _, _, v) acc ->
            Set.union acc $ Map.keysSet v
        ) Set.empty refinedBench
    plotData = Set.foldl
            (\ acc x -> acc ++ "  \"" ++ x ++ "\""
            ) "\"#Nodes\"" allMethods ++
        "\n" ++ Map.foldrWithKey
            (\ k ( _, _, _, _, v) acc -> (++ "\n" ++ acc) $
                show k ++ "    " ++ Set.foldr
                    (\ method acc2 -> (++ acc2) $
                        maybe "-" (\ (_, _, _, _, _, speed, _) -> show $ speed
                                  )
                                  (Map.lookup method v)
                        ++ "    "
                    ) "" allMethods
            ) "" refinedBench
    plotScript =
        "set output 'plotSpeedPerSizeAndMethodSuccessOnly.pdf'\n" ++
        "set terminal pdf font \"Times, 20\" monochrome size 5,3\n" ++
        "set lmargin at screen 0.09\n" ++
        "#set bmargin at screen 0.32\n" ++
        "#set bmargin at screen 0.1\n" ++
        "#set rmargin at screen 0.2\n" ++
        "#set tmargin at screen 0.2\n\n" ++
        "# Line style for grid\n" ++
        "#set style line 81 lt 0              # dashed\n" ++
        "#set style line 81 lt rgb \"#e0e0e0\"  # grey\n\n" ++
        "#set grid back linestyle 81\n" ++
        "set border 3 back # Remove border on top and right.\n\n"   ++
        "set xtics nomirror\n" ++
        "#set xtics 0,0.1,0.6 nomirror\n" ++
        "set ytics 0,20,100  nomirror\n" ++
        "#set xtics 0,0.1,0.6\n" ++
        "#set ytics 0,0.1,0.6\n\n" ++
        "set xrange [" ++
        show (maybe 0  ((+ (-1)) . fst) $ Set.minView sizesSet) ++ ":" ++
        show (maybe 47 ((+1) . fst) $ Set.maxView sizesSet) ++ "]\n" ++
        "set yrange [0:100]\n\n" ++
        "#set xlabel \"x axis label\"\n" ++
        "#set ylabel \"y axis label\"\n\n" ++
        "#set log x\n" ++
        "#set mxtics 10    # Makes logscale look good.\n\n" ++
        "set key bottom center outside horizontal\n\n" ++
        "plot " ++ (drop 5 $ intercalate ",\\\n" $ map
            (\ n ->
                "     'plotSpeedPerSizeAndMethodSuccessOnly.dat' using 1:" ++ show n ++
                " title column with linespoints"
            ) [2..Set.size allMethods + 1]
        ) ++ "\n\n"

plotSpeedPerSizeAndMethod :: Benchmark -> IO ((String, String))
plotSpeedPerSizeAndMethod bench = do
    writeFile "plotSpeedPerSizeAndMethod.dat" plotData
    writeFile "plotSpeedPerSizeAndMethod.plt" plotScript
    safeReadProcess "gnuplot"
        ["plotSpeedPerSizeAndMethod.plt"] ""
  where
    sizesSet = Map.keysSet bench
    refinedBench = answersPerSizeAndMethod bench
    allMethods = Map.foldr
        (\ (_, _, _, _, v) acc ->
            Set.union acc $ Map.keysSet v
        ) Set.empty refinedBench
    plotData = Set.foldl
            (\ acc x -> acc ++ "  \"" ++ x ++ "\""
            ) "\"#Nodes\"" allMethods ++
        "\n" ++ Map.foldrWithKey
            (\ k ( _, _, _, _, v) acc -> (++ "\n" ++ acc) $
                show k ++ "    " ++ Set.foldr
                    (\ method acc2 -> (++ acc2) $
                        maybe "-" (\ (_, _, _, _, _, _, speed) -> show $ speed
                                  )
                                  (Map.lookup method v)
                        ++ "    "
                    ) "" allMethods
            ) "" refinedBench
    plotScript =
        "set output 'plotSpeedPerSizeAndMethod.pdf'\n" ++
        "set terminal pdf font \"Times, 20\" monochrome size 5,3\n" ++
        "set lmargin at screen 0.09\n" ++
        "#set bmargin at screen 0.32\n" ++
        "#set bmargin at screen 0.1\n" ++
        "#set rmargin at screen 0.2\n" ++
        "#set tmargin at screen 0.2\n\n" ++
        "# Line style for grid\n" ++
        "#set style line 81 lt 0              # dashed\n" ++
        "#set style line 81 lt rgb \"#e0e0e0\"  # grey\n\n" ++
        "#set grid back linestyle 81\n" ++
        "set border 3 back # Remove border on top and right.\n\n"   ++
        "set xtics nomirror\n" ++
        "#set xtics 0,0.1,0.6 nomirror\n" ++
        "set ytics 0,20,100  nomirror\n" ++
        "#set xtics 0,0.1,0.6\n" ++
        "#set ytics 0,0.1,0.6\n\n" ++
        "set xrange [" ++
        show (maybe 0  ((+ (-1)) . fst) $ Set.minView sizesSet) ++ ":" ++
        show (maybe 47 ((+1) . fst) $ Set.maxView sizesSet) ++ "]\n" ++
        "set yrange [0:100]\n\n" ++
        "#set xlabel \"x axis label\"\n" ++
        "#set ylabel \"y axis label\"\n\n" ++
        "#set log x\n" ++
        "#set mxtics 10    # Makes logscale look good.\n\n" ++
        "set key bottom center outside horizontal\n\n" ++
        "plot " ++ (drop 5 $ intercalate ",\\\n" $ map
            (\ n ->
                "     'plotSpeedPerSizeAndMethod.dat' using 1:" ++ show n ++
                " title column with linespoints"
            ) [2..Set.size allMethods + 1]
        ) ++ "\n\n"

answersPerSizeAndMethod bench = Map.map
    (\ (_, _, m) -> collectOverAllDensities m)
    bench

collectOverAllDensities m = Map.fold getFromOneDens (0, 0, 0, 0, Map.empty) m

getFromOneDens (v,w,x,y,m) (acc, acc2, acc3, acc4, acc5) =
    ( acc + v, acc2 + w, acc3 + x, acc4 + y
    , Map.unionWith joinTwoMethodMaps m acc5 )

joinTwoMethodMaps (a1,b1,c1,d1,e1,f1,g1) (a2,b2,c2,d2,e2,f2,g2) =
    ( a1 + a2, b1 + b2, c1 + c2, d1 + d2
    , Map.unionWith (+) e1 e2
    , (fromIntegral(a1 + b1) * f1 + fromIntegral(a2 + b2) * f2) / (fromIntegral $ max 1 (a1 + b1 + a2 + b2))
    , (fromIntegral(a1 + b1 + c1 + d1) * g1 + fromIntegral(a2 + b2 + c2 + d2) * g2) / (fromIntegral $ max 1 ((a1 + b1 + c1 + d1 + a2 + b2 + c2 + d2))) )

-- Analyze the benchmark (old version) ----------------------------------------

analyze bench = do
    let (b,v,w,x,y,summaryMap, str') = Map.foldrWithKey
            analyze' (0,0,0,0,0,Map.empty, "") bench
    let str = "\nSUMMARY:\n\n"
            ++ "Networks total = " ++ show b ++ "\n\n"
            ++ "All methods together:\n #No, #Yes, #Undecided, #Timeout =\n "
            ++ (intercalate ", " $ map show [v,w,x,y]) ++ "\n\n"
            ++ Map.foldrWithKey showMethod "" summaryMap ++ "\n"
            ++ replicate 70 '-' ++ "\n" ++ str'
    writeFile "BENCHMARK.RESULTS" str
    putStrLn "\nResults saved to 'BENCHMARK.RESULTS'\n"

analyze' a (b,c,d) (accB, accV, accW, accX, accY, acc, acc2) =
    ( accB + b, accV + v, accW + w, accX + x, accY + y
    , Map.unionWith joinTwoMethodMaps z acc
    , "\n#Nodes = " ++ show a ++ "\n"
        ++ "#Networks = " ++ show b ++ "\n"
        ++ "Last Density = " ++ show c ++ "\n\n"
        ++ "All methods together:\n #No, #Yes, #Undecided, #Timeout =\n "
        ++ (intercalate ", " $ map show [v,w,x,y]) ++ "\n\n"
        ++ Map.foldrWithKey showMethod "" z ++ "\n"
        ++ replicate 70 '-' ++ "\n" ++ acc2
    )
  where
    (v,w,x,y,z) = collectOverAllDensities d

showMethod k (v,w,x,y,_,z,z') acc = k
    ++ ":\n #No, #Yes, #Undecided, #Timeout, Average Time (Success only, seconds), Average Time (seconds) =\n "
    ++ (intercalate ", " $ map show [v,w,x,y]) ++ ", "
    ++ showFFloat (Just 3) z "" ++ ", "
    ++ showFFloat (Just 3) z "" ++ "\n\n" ++ acc
