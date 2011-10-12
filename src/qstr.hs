{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

-- standard modules
--import qualified Control.Monad.Parallel as CMP
import Control.Parallel.Strategies
import Data.List
import qualified Data.Set as Set
--import System.Console.CmdArgs
--import System.Environment (getArgs, withArgs)
--import System.Exit

-- local modules
import Basics
import Calculus.Dipole
import qualified Interface.Sparq as Sparq
import qualified Interface.Gqr as Gqr
import qualified Interface.Triangle as Triangle
import OrientedMatroid
import Parsing
import Parsing.Qstrlib


main :: IO ()
main = do
    NetworkFile { nfNetwork = net1 } <- loadNetworkFile ("../testsuite/dipole-24/alleq.net") :: IO (NetworkFile Dipole72)
    NetworkFile { nfNetwork = net2 } <- loadNetworkFile ("../testsuite/dipole-24/consistent_02.net") :: IO (NetworkFile Dipole72)
    NetworkFile { nfNetwork = net3 } <- loadNetworkFile ("../testsuite/dipole-24/inconsistent_02.net") :: IO (NetworkFile Dipole72)
    let nets = [net1, net2, net3]
    let answers = checkConsistency nets
    print answers
    return ()


checkConsistency :: [Network [String] (Set.Set Dipole72)]
                 -> [[Maybe Bool]]
checkConsistency nets = transpose answers
    where
        answers = [sparqAnswers, gqrAnswers, triangleAnswers, chirotopeAnswers] `using` parList rpar
        sparqAnswers = Sparq.algebraicClosures "dra-72" nets
        gqrAnswers = Gqr.algebraicClosure "dra-72" nets
        triangleAnswers = map (Triangle.checkConsistency . makeAtomic) nets
        chirotopeAnswers = map (isChirotopeDipole72 . makeAtomic) nets

