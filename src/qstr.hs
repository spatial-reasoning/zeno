module Main where

-- standard modules
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
import Parsing.Qstrlib

-- Debugging and Timing
import Data.Time.Clock (diffUTCTime, getCurrentTime)

main :: IO ()
main = do
    net1 <- loadNetwork ("../testsuite/dipole-24/alleq.net") :: IO (Network [String] (Set.Set Dipole72))
    net2 <- loadNetwork ("../testsuite/dipole-24/consistent_02.net") :: IO (Network [String] (Set.Set Dipole72))
    net3 <- loadNetwork ("../testsuite/dipole-24/consistent_03.net") :: IO (Network [String] (Set.Set Dipole72))
    net4 <- loadNetwork ("../testsuite/dipole-24/consistent_04.net") :: IO (Network [String] (Set.Set Dipole72))
    net5 <- loadNetwork ("../testsuite/dipole-24/inconsistent_02.net") :: IO (Network [String] (Set.Set Dipole72))
    let nets = [net1, net2, net3, net4, net5]
    let answers = checkConsistency nets
    start <- getCurrentTime
    print answers
    end <- getCurrentTime
    print $ show (end `diffUTCTime` start) ++ " elapsed."
    return ()


checkConsistency :: [Network [String] (Set.Set Dipole72)]
                 -> [[Maybe Bool]]
checkConsistency nets = transpose answers
    where
--        answers = [sparqAnswers, gqrAnswers, triangleAnswers, chirotopeAnswers] `using` parList rdeepseq
--        answers = [sparqAnswers, gqrAnswers, triangleAnswers, chirotopeAnswers] `using` parList rseq
--        answers = [sparqAnswers, gqrAnswers, triangleAnswers, chirotopeAnswers]
        answers = [sparqAnswersPar, gqrAnswers, triangleAnswers, chirotopeAnswers]
        -- This is much slower!
--        sparqAnswers = Sparq.algebraicReasonings "dra-72" nets
        sparqAnswers = map (Sparq.algebraicReasoning "dra-72") nets
        sparqAnswersPar = sparqAnswers `using` parList rseq
        gqrAnswers = Gqr.algebraicClosures "dra-72" nets
        triangleAnswers = map (Triangle.checkConsistency . makeAtomic) nets
        chirotopeAnswers = map (isChirotopeDipole72 . makeAtomic) nets

