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
    Options{..} <- cmdArgs defaultOptions
    checkNetworks m n d


checkNetworks m n d = do
--    ffNets <- replicateM m $ liftM makeNonAtomic $ randomAtomicNetwork 3 [L, R, F, B, I] n
--    ffNets <- replicateM m $ liftM makeNonAtomic $ randomAtomicNetwork 3 [L, R] n
    ffNets <- replicateM m $ liftM makeNonAtomic $ randomConnectedAtomicNetwork 3 [L, R, F, B, I] n d
--    ffNets <- replicateM m $ liftM makeNonAtomic $ randomConnectedAtomicNetwork 3 [L, R] n d
    let dipoleNet = circleWithTwoCollinearDipolesInside 8
--    let dpNets = [dipoleNet]
    let dpNets = []

    let dpAnswers = makeReadable 0 $ dpCheckConsistency dpNets
    let ffAnswers = makeReadable (length dpNets) $ ffCheckConsistency ffNets

    if (length $ filter (== '-') $ ffAnswers!!0) > (length $ filter (== '-') $ ffAnswers!!2) then do
        putStrLn $ "\n                               === NEW TEST ===\n\n" ++
            "Networks tested:\n\n" ++
            (foldl (\acc (net, k) -> acc ++ " === NETWORK No. " ++ show k ++
                                     " ===\n\n" ++ showNonAtomicNet net ++ "\n\n"
                   ) "" $ zip (dpNets ++ ffNets) [1..]
            ) ++ " === RESULTS ===\n\n" ++
            "Number of Network:       " ++
            (intercalate "  " $ map show [1..length dpNets + length ffNets]) ++
            "\n"
        putStrLn $ "Algebraic Closure:      " ++ dpAnswers!!0 ++ ffAnswers!!0
--        putStrLn $ "Algebraic Reasoning:    " ++ dpAnswers!!1 ++ ffAnswers!!1
        putStrLn $ "Triangle Consistency:   " ++ dpAnswers!!2 ++ ffAnswers!!2
        putStrLn $ "Oriented Matroid:       " ++ dpAnswers!!3 ++ ffAnswers!!3
        putStrLn $ "Biquadratic Polynomial: " ++ dpAnswers!!4 ++ ffAnswers!!4
        return ()
    else
        checkNetworks m n d
        

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
--    aClosureAnswers = map ((\(x,_,_) -> x) . S.algebraicClosure "dra-72") nets
--    aReasoningAnswers = map (S.algebraicReasoning "dra-72") nets
--    triangleAnswers = map (T.checkConsistencyDipole72 . makeAtomic) nets
--    chirotopeAnswers = map (isAcyclicChirotopeDipole72 . makeAtomic) nets
--    biquadraticPolynomialAnswers = map (isAcyclicChirotopeWithoutBPDipole72 . makeAtomic) nets
    aClosureAnswers = parMap rseq ((\(x,_,_) -> x) . S.algebraicClosure "dra-72") nets
    aReasoningAnswers = parMap rseq (S.algebraicReasoning "dra-72") nets
    triangleAnswers = parMap rseq (T.checkConsistencyDipole72 . makeAtomic) nets
    chirotopeAnswers = parMap rseq (isAcyclicChirotopeDipole72 . makeAtomic) nets
    biquadraticPolynomialAnswers = parMap rseq (isAcyclicChirotopeWithoutBPDipole72 . makeAtomic) nets


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
--    aClosureAnswers = map ((\(x,_,_) -> x) . S.algebraicClosure "ff") nets
--    aReasoningAnswers = map (S.algebraicReasoning "ff") nets
--    triangleAnswers = map (T.checkConsistency . makeAtomic) nets
--    chirotopeAnswers = map (isAcyclicChirotopeFlipFlop . makeAtomic) nets
--    biquadraticPolynomialAnswers = map (isAcyclicChirotopeWithoutBPFlipFlop . makeAtomic) nets
    aClosureAnswers = parMap rseq ((\(x,_,_) -> x) . S.algebraicClosure "ff") nets
    aReasoningAnswers = parMap rseq (S.algebraicReasoning "ff") nets
--    aReasoningAnswers = map (S.algebraicReasoning "ff") nets
    triangleAnswers = parMap rseq (T.checkConsistency . makeAtomic) nets
    chirotopeAnswers = parMap rseq (isAcyclicChirotopeFlipFlop . makeAtomic) nets
    biquadraticPolynomialAnswers = parMap rseq (isAcyclicChirotopeWithoutBPFlipFlop . makeAtomic) nets

-- dirty:
makeReadable :: Int -> [[Maybe Bool]] -> [String]
makeReadable n eris = map (concat . map foo . zip [n + 1..]) eris
  where
    foo (k, x) = (replicate (truncate (logBase 10 (fromIntegral k) :: Float)) ' ') ++
        case x of
            Just True  -> " + "
            Just False -> " - "
            Nothing    -> " o "


