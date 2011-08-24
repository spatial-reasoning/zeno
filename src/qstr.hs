{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

import Control.Monad (when)
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Interface
import Parsing

version = "                                        ."
authors = "André Scholz (andre.scholz@uni-bremen.de)"

main :: IO ()
main = do
    net1 <- loadGqrNetworkFile ("../testsuite/alleq.csp")
    net2 <- loadSparqNetworkFile ("../testsuite/alleq.spa")
--    net2 <- loadSparqNetworkFile ("../testsuite/forwardcircle.spa")
    net3 <- loadSparqNetworkFile ("../testsuite/inconsistent_1.spa")
--    net3 <- loadSparqNetworkFile ("../testsuite/alleq.spa")
    let nets = [net1, net2, net3]
    answers <- checkConsistency "dra-80" nets
    print answers
    return ()

