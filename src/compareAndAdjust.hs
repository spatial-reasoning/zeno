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
import System.TimeIt
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
--                       , optDensity    :: Float
                       } deriving (Show, Data, Typeable)

defaultOptions = Options
    { optMinRange = 5
        &= opt (5 :: Int)
        &= explicit
        &= name "m"
        &= name "minsize"
        &= typ "MINSIZE"
        &= help "Start with networks of size MINSIZE. (default = 5)"
    , optMaxRange = 20
        &= opt (20 :: Int)
        &= explicit
        &= name "M"
        &= name "maxsize"
        &= typ "MAXSIZE"
        &= help "Stop after networks of size MAXSIZE. (default = 20)"
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
        &= help "Use this calculus.              Supported Calculi: Dipole-72, FlipFlop, OPRA-1. (default = \"\")"
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
    help ("This progam compares several semi-decision procedures for the " ++
          "consistency of constraint networks using the given relations.") &=
    helpArg [explicit, name "h", name "help"] &=
--    versionArg [ignore] &=
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

allBaseRels = concat
    [ map Calc (Set.toList cBaserelations :: [FlipFlop])
--    , map Calc (Set.toList cBaserelations :: [Dipole24])
    , map Calc (Set.toList cBaserelations :: [Dipole72])
--    , map Calc (Set.toList cBaserelations :: [Dipole80])
    , map Calc (Set.toList cBaserelations :: [OPRA1])
    ]

helperFor str = case map Char.toUpper str of
    "DIPOLE-72" -> Calc (Set.findMin cBaserelations :: Dipole72)
    "FLIPFLOP"  -> Calc (Set.findMin cBaserelations :: FlipFlop)
    "OPRA-1"    -> Calc (Set.findMin cBaserelations :: OPRA1   )

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
        useWholeCalculusAndExec (helperFor optCalculus) opts
    else
        unboxAndExec (helperFor optCalculus) wordsOptRelations opts

unboxAndExec (Calc typeHelper) wordsOptRelations opts@Options{..} = do
    let rels = tail $ typeHelper:(map readRel wordsOptRelations)
    -- force full evaluation of rels. Is there a better way to do this?
    -- ( `seq` does not help here )
    when (rels == rels) (return ())
    exec rels opts

useWholeCalculusAndExec (Calc typeHelper) opts@Options{..} = do
    let rels = Set.toList cBaserelations :: [Dipole72]
    exec rels opts

exec rels opts@Options{..} = do
    let head' = head rels
    let rang = rank head'
    let procedures = proceduresForAtomicNets head'
    startBenchString <- catch
        (readFile "RESULTS.BENCHMARK")
        ((\e -> do
            putStrLn "Starting a new Benchmarking...\n"
            return "fromList []"
         ) :: SomeException -> IO String
        )
    let startBenchRead = reads startBenchString
    startBench <- if startBenchRead == [] || (snd $ head startBenchRead) /= "" then do
                          putStrLn "Starting a new Benchmarking...\n"
                          return Map.empty
                      else
                          return $ fst $ head startBenchRead
    markTheBench optMinRange optMaxRange optNumOfNets procedures optTimeout rang rels startBench

