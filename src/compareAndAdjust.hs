{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

-- standard modules
import qualified Control.Exception as CE
import Control.Monad
import Control.Parallel.Strategies
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Console.CmdArgs
import System.IO
import System.IO.Unsafe
import Text.Printf

-- local modules
import Basics
import Calculus.Dipole
import Calculus.FlipFlop
import Export
import qualified Interface.Gqr as G
import qualified Interface.Sparq as S
import qualified Interface.Triangle as T
import OrientedMatroid
import Parsing.Qstrlib
import Testsuite
import Testsuite.Random
import Helpful

-- Debugging and Timing
import Debug.Trace
import Criterion.Measurement
--import Data.Time.Clock


-- begin commandline option handling ------------------------------------------

data Options = Options { optNumOfNets  :: Int
                       , optTimeout    :: Int
--                       , optNumOfNodes :: Int
--                       , optDensity    :: Float
                       } deriving (Show, Data, Typeable)

defaultOptions = Options
    { optNumOfNets = def
        &= opt (100 :: Int)
        &= explicit
        &= name "n"
        &= name "networks"
        &= typ "NUMBER OF NETWORKS"
        &= help "Generate at most this NUMBER OF NETWORKS. (0 = infinity)"
    , optTimeout = def
        &= opt (20 :: Int)
        &= explicit
        &= name "t"
        &= name "timeout"
        &= typ "TIMEOUT"
        &= help "Stop generating new constraint networks after TIMEOUT hours. (0 = infinity)"
--    , optNumOfNodes = def
--        &= opt (6 :: Int)
--        &= explicit
--        &= name "k"
--        &= name "nodes"
--        &= typ "NUMBER OF NODES"
--        &= help "Generate networks with this NUMBER OF NODES. (default = 6)"
--    , optDensity = def
--        &= opt (0.5 :: Float)
--        &= explicit
--        &= name "d"
--        &= name "density"
--        &= typ "DENSITY"
--        &= help "Generate networks with this DENSITY. (default = 0.5)"
    } &=
--    verbosity &=
--    help "Compares the results of semi-decision procedures for consistency of\
--         \ FlipFlop constraint networks." &=
    helpArg [explicit, name "h", name "help"] &=
--    versionArg [ignore] &=
    program "compareAndAdjust" &=
    summary "compare version 11.12.22, (K) André Scholz" &=
    details ( lines "\
\ This progam compares several semi-decision procedures for the consistency of\
\ FlipFlop constraint networks.\n"
--   ++ "\n\
-- \ To compare the procedures on 13 networks of density 0.3 with 5 nodes type:\n\
-- \    compare 13 5 0.3\n"
            )


-- end commandline option handling --------------------------------------------

type Benchmark = Map.Map Int   -- maps number of nodes to the following attributes
    ( Int                      -- number of networks tested so far
    , Float                    -- last used density
    , Map.Map Float            -- maps a density to the following attributes
        ( Int                  -- # of no
        , Int                  -- # of yes
        , Int                  -- # of undecided
        , Int                  -- # of timeouts
        , Map.Map String       -- maps a name of an algorithm to the following attributes
            ( Int              -- # of no
            , Int              -- # of yes
            , Int              -- # of undecided
            , Int              -- # of timeouts
            , Map.Map (Maybe Bool, Int) Int   -- maps the rank of fastness (fst, snd, ...) to the number of times it was achieved, first key is the correct answer given. Only correct answers are counted.
            , Double            -- average time (in seconds) needed to check one network, regardless of success
            )
        )
    )

main = do
    hSetBuffering stdout NoBuffering
    Options{..} <- cmdArgs defaultOptions
    startBenchString <- catch
        (readFile "RESULTS.BENCHMARK")
        (\e -> do
            putStrLn "Starting a new Benchmarking...\n"
            return "fromList []"
        )
    let startBenchRead = reads startBenchString
    startBench <- if startBenchRead == [] || (snd $ head startBenchRead) /= "" then do
                          putStrLn "Starting a new Benchmarking...\n"
                          return Map.empty
                      else
                          return $ fst $ head startBenchRead
    markTheBench optNumOfNets optTimeout startBench

markTheBench n t bench = do
    let numOfNodes' = filter
            (\a -> maybe True (\(a,_,_) -> a < n) $ Map.lookup a bench
            ) [5..20]
    myExit <- timeout 100 getLine
    if maybe False (isInfixOf "q") myExit then do
        putStrLn "Quit requested by user."
        return ()
    else if null numOfNodes' then do
        putStrLn "Finished."
        return ()
    else do
        let numOfNodes = head numOfNodes'
        let (nOfTestedNets, dens) = maybe
                ( maybe (0, 0.2) (\(_,a,_) -> (0, a)) $ Map.lookup (numOfNodes - 1) bench )
                (\(x,a,b) ->
                    (\(c,d,e,_,_) ->
                      let
                        -- if all methods time out try to generate more inconsistent networks.
                        sicnum = if c + d + e == 0 then
                                     1
                                 else
                                     fromIntegral $ signum $ d + e - c
                      in
--                        (x, min 1 $ max 0 $ a + sicnum * 0.00002)    -- adjust density
                        (x, min 1 $ max 0 $ a + sicnum * 0.001)    -- adjust density
                    ) $ (Map.!) b a
                ) $ Map.lookup numOfNodes bench
        (nets, results) <- checkNetworks 10 t numOfNodes dens
        saveSpecialNets nets results numOfNodes dens
        putStrLn $ "\n                               === NEW TEST ===\n\n" ++
            " Number of Nodes: " ++ show numOfNodes ++
            " | Number of tested networks: " ++ show (nOfTestedNets + 10) ++
            " | Density: " ++ show dens ++ "\n\n" ++ showResults results
        let newBench = foldl
              (\ben result ->
                let
                  plainResult = map (snd . snd) result
                  false = elem (Just (Just False)) plainResult
                  true  = elem (Just (Just True)) plainResult
                  nothing = elem (Just Nothing) plainResult
                  (a, b, c, d, totalResult) = case (false, true, nothing) of
                      (True, True, _)      -> saveIncorrectResults nets results
                      (True, False, _)     -> (1, 0, 0, 0, Just (Just False))
                      (False, True, _)     -> (0, 1, 0, 0, Just (Just True))
                      (False, False, True) -> (0, 0, 1, 0, Just Nothing)
                      otherwise            -> (0, 0, 0, 1, Nothing)
                  sortedDescs = map fst $ sortBy
                      (\(_,(x,_)) (_,(y,_)) -> compare x y) $
                      filter ((totalResult ==) . snd . snd) result
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
                          ) result
                      )
                in
                  case Map.lookup numOfNodes ben of
                      Nothing -> Map.insert numOfNodes
                          ( 1
                          , dens
                          , Map.singleton dens newDensList
                          ) ben
                      Just (bN, bD, bM) -> Map.insert numOfNodes
                          ( bN + 1
                          , dens
                          , case Map.lookup dens bM of
                                Nothing -> Map.insert dens newDensList bM
                                Just (dn, dy, du, dt, dm) -> Map.insert dens
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
                                                    , if totalResult == Just Nothing then
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
                                                    efgh = fromIntegral $ e + f + g + h
                                                  in
                                                    (oldTyme * efgh + tyme) /
                                                    (efgh + 1)
                                                ) dm
                                        ) dm result
                                    ) bM
                          ) ben
              ) bench results
        writeFile "RESULTS.BENCHMARK" $ show newBench
        appendFile "RESULTS.NETS" $ show nets ++ "\n"
        appendFile "RESULTS.RESULTS" $ show results ++ "\n"
        markTheBench n t newBench

saveIncorrectResults nets answers = unsafePerformIO $ do
    appendFile "RESULTS.ERROR" $ showNets nets ++ showResults answers
--    writeFile "RESULTS.ERROR" $ showResults answers
    error $ "Results contradict each other. Results saved to " ++
            "RESULTS.ERROR"

saveSpecialNets nets results numOfNodes dens = do
    let (sNets, sResults) = unzip $ filter
            (\(_, result) ->
                 (elem (length $ filter (\(_, (_, answer)) -> answer == Just (Just False)) result) [1,2])
              || ( (length $ filter (\(_, (_, answer)) -> answer == Just (Just False)) result) > 0 &&
                   elem (length $ filter (\(_, (_, answer)) -> answer == Just Nothing) result) [1,2]
                 )
            ) $ zip nets results
    if not $ null sNets then
        appendFile "RESULTS.SPECIAL" $ showNets sNets ++
            "\n Number of Nodes: " ++ show numOfNodes ++
            " | Density: " ++ show dens ++ "\n\n" ++
            showResults sResults
    else
        return ()


checkNetworks n t k d = do
    nets <- replicateM n $ liftM makeNonAtomic $ randomConnectedAtomicNetwork 3 [L, R, F, B, I] k d
    let funs = 
            [ ("Algebraic Closure", (\(x,_,_) -> x) . S.algebraicClosure "ff")
            , ("Ternary Algebraic Closure", (\(x,_,_) -> x) . S.ternaryAlgebraicClosure "ff")
            , ("Algebraic Reasoning", S.algebraicReasoning "ff")
            , ("Triangle Consistenty", T.checkConsistency . makeAtomic)
            , ("Chirotope Sloppy", isAcyclicChirotopeFlipFlop True . makeAtomic)
            , ("BFP Sloppy", isAcyclicChirotopeWithoutBPFlipFlop True . makeAtomic)
            , ("Chirotope", isAcyclicChirotopeFlipFlop False . makeAtomic)
            , ("BFP", isAcyclicChirotopeWithoutBPFlipFlop False . makeAtomic)
            ]
    results <- sequence $ map
            (\net -> sequence $ map
                (\(desc, fun) -> do
                    result <- time $ timeoutP (t * 1000000) $ fun net
                    return $ (desc, result)
                ) funs
            ) nets
    return (nets, results)

showNets nets = 
    "\n                               === NEW TEST ===\n\n" ++
    "Networks tested:\n\n" ++ showNetworks nets 1

showResults answers =
    "                               === RESULTS ===\n\n" ++
    "    #   │" ++
    "    AC    │" ++
    "   T-AC   │" ++
    "    AR    │" ++
    "    TC    │" ++
    "   OM s   │" ++
    "   BFPs   │" ++
    "   OM e   │" ++
    "   BFPe   │" ++
    "\n  ──────" ++ (concat $ replicate 8 "┼──────────") ++ "┤\n" ++
    ( fst $ foldl
        (\(acc, number) answersToOneNet ->
            ( (++ "\n") $ acc ++
              "   " ++ printf "%2d" (number :: Int) ++ "   │" ++ foldl
                (\acc2 (_, (elapsed, answer)) -> acc2 ++
                   showAnswer answer ++ ": " ++
                   (printf "%3d" ((round elapsed) :: Int)) ++ "s │"
                ) "" answersToOneNet
            , number + 1 )
        ) ("", 1) answers
    )

showAnswer :: Maybe (Maybe Bool) -> String
showAnswer (Just (Just False)) = " - "
showAnswer (Just (Just True))  = " + "
showAnswer (Just Nothing)      = " o "
showAnswer Nothing             = " x "

showNetworks nets startNumber = foldl
    (\acc (net, k) -> acc ++ " === NETWORK No. " ++ show k ++
                             " ===\n\n" ++ showNonAtomicNet net ++ "\n\n"
    ) "" $ zip nets [startNumber..]

