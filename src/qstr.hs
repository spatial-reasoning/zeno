{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

-- standard modules
import Control.Monad (when)
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
-- local modules
import Interface
import Parsing
import Calculus.Dipole

version = "                                        ."
authors = "André Scholz (andre.scholz@uni-bremen.de)"

main :: IO ()
main = do
    NetworkFile { nfNetwork = net1 } <- loadNetworkFile ("../testsuite/alleq.net") :: IO (NetworkFile Dipole72)
    NetworkFile { nfNetwork = net2 } <- loadNetworkFile ("../testsuite/dipole24/consistent_02.net") :: IO (NetworkFile Dipole72)
    NetworkFile { nfNetwork = net3 } <- loadNetworkFile ("../testsuite/dipole24/inconsistent_02.net") :: IO (NetworkFile Dipole72)
    let nets = [net1, net2, net3]
    answers <- checkConsistency "dra-72" nets
    print answers
    return ()

