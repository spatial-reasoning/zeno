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
    net1 <- loadGqrNetworkFile ("alleq.csp")
    net2 <- loadSparqNetworkFile ("alleq.spa")
    net3 <- loadSparqNetworkFile ("alleq.spa")
    let nets = [net1, net2, net3]
    answers <- checkConsistency "dra-80" nets
    print answers
    return ()

