{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

-- standard modules
import Control.Monad
import Control.Parallel.Strategies
import Data.List
import qualified Data.Set as Set
import System.Console.CmdArgs
import System.IO

-- standard modules
import Basics
import Calculus.Dipole72
import Calculus.FlipFlop
import Export
import qualified Interface.Gqr as G
import qualified Interface.Sparq as S
import qualified Interface.Triangle as T
import DecisionProcedure.FlipFlop.OrientedMatroid
import DecisionProcedure.Dipole72.OrientedMatroid
import Parsing.Qstrlib
import Testsuite.FlipFlop
import Testsuite.Dipole72
import Testsuite.Random
import Helpful

-- Debugging and Timing
import Debug.Trace
import Data.Time.Clock (diffUTCTime, getCurrentTime)


-- begin commandline option handling ------------------------------------------

data Options = Options { m :: Int
                       , n :: Int
                       , d :: Float
                       } deriving (Show, Data, Typeable)

defaultOptions = Options
    { m = 1
        &= typ "NumberOfNetworks"
        &= argPos 0
    , n = 5
        &= typ "NumberOfNodes"
        &= argPos 1
    , d = 1.0
        &= typ "Density"
        &= argPos 2
    } &=
--    verbosity &=
--    help "Compares the results of semi-decision procedures for consistency of\
--         \ FlipFlop constraint networks." &=
    helpArg [explicit, name "h", name "help"] &=
--    versionArg [ignore] &=
    program "compare" &=
    summary "compare version 11.11.29, (K) André Scholz" &=
    details ( lines "\
\ This progam compares several semi-decision procedures for the consistency of\
\ FlipFlop constraint networks.\n\
\ \n\
\ To compare the procedures on 13 networks of density 0.4 with 5 nodes type:\n\
\    compare 13 5 0.4\n" )


-- end commandline option handling --------------------------------------------

main = do
    hSetBuffering stdout NoBuffering
--    Options{..} <- cmdArgs defaultOptions
    checkNetworks


checkNetworks = do
--    dpNet0 <- loadBinaryNetwork ("bla2.net") :: IO (Network [String] (Set.Set Dipole72))
--    dpNet1 <- loadBinaryNetwork ("../testsuite/dipole/24/alleq.net") :: IO (Network [String] (Set.Set Dipole72))
--    dpNet2 <- loadBinaryNetwork ("../testsuite/dipole/24/consistent_01.net") :: IO (Network [String] (Set.Set Dipole72))
--    dpNet3 <- loadBinaryNetwork ("../testsuite/dipole/24/inconsistent_01.net") :: IO (Network [String] (Set.Set Dipole72))
--    let dpNet4 = forwardCircle 8
--    -- At a sizo of 12 this network overwhelms the backtracking of the
--    -- oriented matroid method.
--    let dpNet5 = circleWithTwoCollinearDipolesInside 12
--    let dpNet5 = circleWithTwoCollinearDipolesInside 11
--    let dpNets = map circleWithTwoCollinearDipolesInside [6..40]
    let dpNets = []

    let ffNet1 = allLeft 8
    ffNet2 <- loadNetwork ("../testsuite/flipflop/inconsistent/inconsistent_01.net") :: IO (Network [String] (Set.Set FlipFlop))
    let ffNet3 = indianTent 8
    ffNet4 <- loadNetwork ("../testsuite/flipflop/inconsistent/nomatroid.net") :: IO (Network [String] (Set.Set FlipFlop))
    ffNet5 <- loadNetwork ("../testsuite/flipflop/inconsistent/triskilde.net") :: IO (Network [String] (Set.Set FlipFlop))
    ffNet6 <- loadNetwork ("../testsuite/flipflop/inconsistent/triskilde_less.net") :: IO (Network [String] (Set.Set FlipFlop))
    ffNet7 <- loadNetwork ("../testsuite/flipflop/inconsistent/desargues.net") :: IO (Network [String] (Set.Set FlipFlop))
    ffNet8 <- loadNetwork ("../testsuite/flipflop/inconsistent/minimalStar.net") :: IO (Network [String] (Set.Set FlipFlop))
    ffNet9 <- loadNetwork ("../testsuite/flipflop/inconsistent/pappos.net") :: IO (Network [String] (Set.Set FlipFlop))
    ffNet10 <- loadNetwork ("../testsuite/flipflop/inconsistent/pappos_uniform.net") :: IO (Network [String] (Set.Set FlipFlop))
    ffNet11 <- loadNetwork ("../testsuite/flipflop/inconsistent/tenA.net") :: IO (Network [String] (Set.Set FlipFlop))
    ffNet12 <- loadNetwork ("../testsuite/flipflop/consistent/triskilde.net") :: IO (Network [String] (Set.Set FlipFlop))
    ffNet13 <- loadNetwork ("../testsuite/flipflop/consistent/desargues.net") :: IO (Network [String] (Set.Set FlipFlop))
    ffNet14 <- loadNetwork ("../testsuite/flipflop/consistent/minimalStar.net") :: IO (Network [String] (Set.Set FlipFlop))
    ffNet15 <- loadNetwork ("../testsuite/flipflop/consistent/minimalStar.net") :: IO (Network [String] (Set.Set FlipFlop))
    ffNet15 <- loadNetwork ("../testsuite/flipflop/consistent/pappos.net") :: IO (Network [String] (Set.Set FlipFlop))
--    ffNet16 <- loadNetwork ("../testsuite/flipflop/consistent/pappos_uniform.net") :: IO (Network [String] (Set.Set FlipFlop))

--    ffNet1 <- loadNetwork ("./test.net") :: IO (Network [String] (Set.Set FlipFlop))
--    let ffNets = [ffNet1]

    let ffNets = [ ffNet1
                 , ffNet2
                 , ffNet3
                 , ffNet4
                 , ffNet5
                 , ffNet6
                 , ffNet7
                 , ffNet8
                 , ffNet9
                 , ffNet10
                 , ffNet11
                 , ffNet12
                 , ffNet13
                 , ffNet14
                 , ffNet15
                 ]

    let dpAnswers = makeReadable 0 $ dpCheckConsistency dpNets
    let ffAnswers = makeReadable (length dpNets) $ ffCheckConsistency ffNets

    let showNetworks nets startNumber = foldl
            (\acc (net, k) -> acc ++ " === NETWORK No. " ++ show k ++
                              " ===\n\n" ++ showNonAtomicNet net ++ "\n\n"
            ) "" $ zip nets [startNumber..]

    putStrLn $ "\n                               === NEW TEST ===\n\n" ++
--        "Networks tested:\n\n" ++
--        showNetworks dpNets 1 ++ showNetworks ffNets (length dpNets + 1) ++
--        " === RESULTS ===\n\n" ++
        "Number of Network:              " ++
        (intercalate "  " $ map show [1..length dpNets + length ffNets]) ++
        "\n"

    start <- getCurrentTime
    putStrLn $ "Algebraic Closure:             " ++ dpAnswers!!0 ++ ffAnswers!!0
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed.\n"
    start <- getCurrentTime
    putStrLn $ "Algebraic Reasoning:           " ++ dpAnswers!!1 ++ ffAnswers!!1
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed.\n"
    start <- getCurrentTime
    putStrLn $ "Triangle Consistency:          " ++ dpAnswers!!2 ++ ffAnswers!!2
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed.\n"
    start <- getCurrentTime
    putStrLn $ "Oriented Matroid:              " ++ dpAnswers!!3 ++ ffAnswers!!3
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed.\n"
    start <- getCurrentTime
    putStrLn $ "Biquadratic Polynomial:        " ++ dpAnswers!!4 ++ ffAnswers!!4
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed.\n\n"

{-
    start <- getCurrentTime
    putStrLn $ "Algebraic Closure:             " ++ ffAnswers!!0
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed.\n"
    start <- getCurrentTime
    putStrLn $ "Algebraic Reasoning:           " ++ ffAnswers!!1
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed.\n"
    start <- getCurrentTime
    putStrLn $ "Triangle Consistency:          " ++ ffAnswers!!2
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed.\n"
    start <- getCurrentTime
    putStrLn $ "Oriented Matroid:              " ++ ffAnswers!!3
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed.\n"
    start <- getCurrentTime
    putStrLn $ "Biquadratic Polynomial:        " ++ ffAnswers!!4
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed.\n\n"
-}

    return ()


dpCheckConsistency :: [Network [String] (Set.Set Dipole72)]
                   -> [[Maybe Bool]]
dpCheckConsistency nets = answers
  where
--    answers = [sparqAnswers, gqrAnswers, triangleAnswers, chirotopeAnswers] `using` parList rdeepseq
--    answers = [sparqAnswers, gqrAnswers, triangleAnswers, chirotopeAnswers] `using` parList rseq
--    answers = [sparqAnswers, gqrAnswers, triangleAnswers, chirotopeAnswers]
--    answers = [sparqAnswersPar, gqrAnswers, triangleAnswers, chirotopeAnswers]
    answers = [ aClosureAnswers
              , aReasoningAnswers
              , triangleAnswers
              , chirotopeAnswers
              , biquadraticPolynomialAnswers
              ]
    aClosureAnswers = map ((\(x,_,_) -> x) . S.algebraicClosure "dra-72") nets
    aReasoningAnswers = map (S.algebraicReasoning "dra-72") nets
    triangleAnswers = map (T.checkConsistencyDipole72 . makeAtomic) nets
    chirotopeAnswers = map (isAcyclicChirotopeDipole72 . makeAtomic) nets
    biquadraticPolynomialAnswers = map (isAcyclicChirotopeWithoutBPDipole72 . makeAtomic) nets
--    chirotopeAndBiquadraticPolynomialAnswers = map (isAcyclicChirotopePlainAndWithoutBPDipole72 . makeAtomic) nets
--    [chirotopeAnswers, biquadraticPolynomialAnswers] = transpose chirotopeAndBiquadraticPolynomialAnswers


--    aClosureAnswers = parMap rseq ((\(x,_,_) -> x) . S.algebraicClosure "dra-72") nets
--    aReasoningAnswers = parMap rseq (S.algebraicReasoning "dra-72") nets
--    triangleAnswers = parMap rseq (T.checkConsistencyDipole72 . makeAtomic) nets
--    chirotopeAnswers = parMap rseq (isAcyclicChirotopeDipole72 . makeAtomic) nets
--    biquadraticPolynomialAnswers = parMap rseq (isAcyclicChirotopeWithoutBPDipole72 . makeAtomic) nets



ffCheckConsistency :: [Network [String] (Set.Set FlipFlop)]
                   -> [[Maybe Bool]]
ffCheckConsistency nets = answers
  where
--    answers = [sparqAnswers, gqrAnswers, triangleAnswers, chirotopeAnswers] `using` parList rdeepseq
--    answers = [sparqAnswers, gqrAnswers, triangleAnswers, chirotopeAnswers] `using` parList rseq
--    answers = [sparqAnswers, gqrAnswers, triangleAnswers, chirotopeAnswers]
--    answers = [sparqAnswersPar, gqrAnswers, triangleAnswers, chirotopeAnswers]
    answers = [ aClosureAnswers
              , aReasoningAnswers
              , triangleAnswers
              , chirotopeAnswers
              , biquadraticPolynomialAnswers
              ]
    aClosureAnswers = map ((\(x,_,_) -> x) . S.algebraicClosure "ff") nets
    aReasoningAnswers = map (S.algebraicReasoning "ff") nets
    triangleAnswers = map (T.checkConsistency . makeAtomic) nets
    chirotopeAnswers = map (isAcyclicChirotopeFlipFlop . makeAtomic) nets
    biquadraticPolynomialAnswers = map (isAcyclicChirotopeWithoutBPFlipFlop . makeAtomic) nets
--    chirotopeAndBiquadraticPolynomialAnswers = map (isAcyclicChirotopePlainAndWithoutBPFlipFlop . makeAtomic) nets
--    [chirotopeAnswers, biquadraticPolynomialAnswers] = transpose chirotopeAndBiquadraticPolynomialAnswers


--    aClosureAnswers = parMap rseq ((\(x,_,_) -> x) . S.algebraicClosure "ff") nets
--    aReasoningAnswers = map (S.algebraicReasoning "ff") nets
----    aReasoningAnswers = parMap rseq (S.algebraicReasoning "ff") nets
----    aReasoningAnswers = mapP 4 (S.algebraicReasoning "ff") nets
--    triangleAnswers = parMap rseq (T.checkConsistency . makeAtomic) nets
--    chirotopeAnswers = parMap rseq (isAcyclicChirotopeFlipFlop . makeAtomic) nets
--    biquadraticPolynomialAnswers = parMap rseq (isAcyclicChirotopeWithoutBPFlipFlop . makeAtomic) nets


-- dirty:
makeReadable :: Int -> [[Maybe Bool]] -> [String]
makeReadable n eris = map (concat . map foo . zip [n + 1..]) eris
  where
    foo (k, x) = (replicate (truncate (logBase 10 (fromIntegral k) :: Float)) ' ') ++
        case x of
            Just True  -> " + "
            Just False -> " - "
            Nothing    -> " o "


