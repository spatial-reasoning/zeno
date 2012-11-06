{-# LANGUAGE DeriveDataTypeable, RecordWildCards, ExistentialQuantification #-}
module Main where

-- standard modules
import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Control.Parallel.Strategies
import qualified Data.Char as Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Ratio
import System.Console.CmdArgs
import System.Environment
import System.IO
import System.IO.Unsafe
import Text.Printf

-- local modules
import Basics
import Benchmark
import Calculus.All
import DecisionProcedure
import DecisionProcedure.All

import Debug.Trace


-- begin commandline option handling ------------------------------------------

data Options = Options { optMinRange   :: Int
                       , optMaxRange   :: Int
--                       , optSampleSize :: Int
                       , optNumOfNets  :: Int
                       , optTimeout    :: Int
                       , optRelations  :: String
                       , optCalculus   :: String
--                       , optNumOfNodes :: Int
                       , optDensity    :: Float
                       , optAreal      :: Int
                       , optBatch      :: Bool
                       } deriving (Show, Data, Typeable)

defaultOptions = Options
    { optMinRange = 5
        &= opt (5 :: Int)
        &= explicit
        &= name "m"
        &= name "minsize"
        &= typ "MINSIZE"
        &= help "Start with networks of size MINSIZE. (default = 5)"
    , optMaxRange = (-1)
        &= opt (-1 :: Int)
        &= explicit
        &= name "M"
        &= name "maxsize"
        &= typ "MAXSIZE"
        &= help "Stop after networks of size MAXSIZE. (default = negative value = infinity)"
--    , optSampleSize = 1
--        &= opt (1 :: Int)
--        &= explicit
--        &= name "s"
--        &= name "samplesize"
--        &= typ "SAMPLESIZE"
--        &= help "Create SAMPLESIZE number of networks per adjustment step. (default = 1)"
    , optNumOfNets = 5000
        &= opt (5000 :: Int)
        &= explicit
        &= name "n"
        &= name "networks"
        &= typ "NUMBER OF NETWORKS"
        &= help "Generate NUMBER OF NETWORKS networks per density. (default = 5000)"
    , optTimeout = (-1)
        &= opt ((-1) :: Int)
        &= explicit
        &= name "t"
        &= name "timeout"
        &= typ "TIMEOUT"
        &= help "Timeout each decision procedure on each network after TIMEOUT seconds.    (default = negative value = infinity)"
    , optRelations = ""
        &= opt ""
        &= explicit
        &= name "r"
        &= name "relations"
        &= typ "Relation(s)"
        &= help "Only use these relations.        (default = \"\")"
    , optCalculus = ""
        &= opt ""
        &= explicit
        &= name "c"
        &= name "calculus"
        &= typ "Calculus"
        &= help "Use this calculus.              Supported Calculi: Dipole-72, FlipFlop, OPRA-1, OPRA-4, OPRA-8, OPRA-10.     (default = \"\")"
    , optDensity = 0.5
        &= opt (0.5 :: Float)
        &= explicit
        &= name "d"
        &= name "density"
        &= typ "INITIAL DENSITY"
        &= help "Start with the density closest to the given value. (default = 0.5)"
    , optAreal = 0
        &= opt (0 :: Int)
        &= explicit
        &= name "a"
        &= name "areal"
        &= typ "NUMBER"
        &= help "1 = Only use areal relations.          2 = Only use non-areal relations.      Any other number = No restriction. (Default = 0)"
    , optBatch = def
        &= explicit
        &= name "b"
        &= name "batch"
        &= help "Start in batch mode and don't wait for input."
    } &=
--    verbosity &=
--    help "Compares the results of semi-decision procedures for consistency of\
    help ("This progam compares several semi-decision procedures for the " ++
          "consistency of constraint networks using the given relations.") &=
    helpArg [explicit, name "h", name "help", help "Show this message."] &=
--    versionArg [ignore] &=
    versionArg [help "Show version information."] &=
    program "compareAndAdjust" &=
    summary "compare version 12.02.23, (K) André Scholz" &=
    details [ ""
--              "This progam compares several semi-decision procedures for the consistency of constraint networks using the given relations."
--            , "To compare the procedures on 13 networks of density 0.3 with 5 nodes type:"
--            , "compare 13 5 0.3"
            ]


-- end commandline option handling --------------------------------------------

data Calc = forall a. (Calculus a, HasDecisionProcedure a) => Calc a

instance Show Calc where
--    show (Calc a) = "Calc " ++ show a
    show (Calc a) = show a

-- improve: put this list somewhere else and just use it here.
-- maybe make it more generic by having a list of known calculi and mapping the
-- appropriate function over it.
allBaseRels = concat
    [ map Calc (Set.toList cBaserelations :: [FlipFlop])
--    , map Calc (Set.toList cBaserelations :: [Dipole24])
    , map Calc (Set.toList cBaserelations :: [Dipole72])
--    , map Calc (Set.toList cBaserelations :: [Dipole80])
    , map Calc (Set.toList cBaserelations :: [Opra1])
    , map Calc (Set.toList cBaserelations :: [Opra4])
    , map Calc (Set.toList cBaserelations :: [Opra8])
    , map Calc (Set.toList cBaserelations :: [Opra10])
    ]

helperForCalculus str = case map Char.toUpper str of
    "DIPOLE-72" -> Calc (Set.findMin cBaserelations :: Dipole72)
    "FLIPFLOP"  -> Calc (Set.findMin cBaserelations :: FlipFlop)
    "FF"        -> Calc (Set.findMin cBaserelations :: FlipFlop)
    "OPRA-1"    -> Calc (Set.findMin cBaserelations :: Opra1   )
    "OPRA-4"    -> Calc (Set.findMin cBaserelations :: Opra4   )
    "OPRA-8"    -> Calc (Set.findMin cBaserelations :: Opra8   )
    "OPRA-10"   -> Calc (Set.findMin cBaserelations :: Opra10  )
    "OPRA-16"   -> Calc (Set.findMin cBaserelations :: Opra16  )
    otherwise   -> error $
        "Sorry, i don't know about the calculus \"" ++ str ++ "\""

main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    opts <- (if null args then withArgs ["--help"] else id) $ cmdArgs defaultOptions
    optionHandler opts

optionHandler opts@Options{..} = do
    let wordsOptRelations = words optRelations
    if null optCalculus then do
        when (null optRelations)
             (error $ "Which relations or calculus should i use?")
        let typeHelperStr = head $ wordsOptRelations
        let helperLst = filter
                (\ (Calc a) -> showRel a == map Char.toLower typeHelperStr)
                allBaseRels
        when (null helperLst)
             (error $ "\"" ++ typeHelperStr ++
                      "\" is not a valid relation in any known calculus.")
        let boxedTypeHelper = head helperLst
        unboxAndExec boxedTypeHelper wordsOptRelations opts
    else if null optRelations then
        useWholeCalculusAndExec (helperForCalculus optCalculus) opts
    else
        unboxAndExec (helperForCalculus optCalculus) wordsOptRelations opts

unboxAndExec (Calc typeHelper) wordsOptRelations opts@Options{..} = do
    let rels =
            ( case optAreal of
                  1 -> intersect (tail $ typeHelper:cBaserelationsArealList)
                  2 -> intersect (tail $ typeHelper:cBaserelationsNonArealList)
                  _ -> id
            ) $ tail $ typeHelper:(map readRel wordsOptRelations)
    -- force full evaluation of rels. Is there a better way to do this?
    -- ( `seq` does not help here )
    when (rels == rels) (return ())
    exec rels opts

useWholeCalculusAndExec (Calc typeHelper) opts@Options{..} = do
    let rels = tail $ typeHelper:case optAreal of
                                     1 -> cBaserelationsArealList
                                     2 -> cBaserelationsNonArealList
                                     _ -> cBaserelationsList
    exec rels opts

exec rels opts@Options{..} = do
    let head' = head rels
    let rank' = rank head'
    let procedures = proceduresForAtomicNets head'
    let startStr = "Starting a new Benchmarking"
            ++ ( if optBatch then
                     " (running in batch mode)"
                 else
                     " (press 'q' to quit)" )
            ++ "...\n"
    startBenchString <- catch
        (readFile "BENCHMARK.COLLECTION")
        ((\e -> do
            putStrLn startStr
            return "fromList []"
         ) :: SomeException -> IO String
        )
    let startBenchRead = reads startBenchString
    startBench <- if startBenchRead == [] || (snd $ head startBenchRead) /= "" then do
                          putStrLn startStr
                          return Map.empty
                      else
                          return $ fst $ head startBenchRead
    bench <- markTheBench optBatch optMinRange optMaxRange optNumOfNets procedures optTimeout rank' rels optDensity startBench
    analyze bench
    plotInconsistenciesPerSizeAndMethodInPercent bench
    plotPercentageOfInconsistentNetworksPerDensity bench
    plotSpeedPerSizeAndMethodSuccessOnly bench
    plotSpeedPerSizeAndMethod bench

