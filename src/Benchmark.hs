module Main where

import Interface
import Parsing

main :: IO ()
main = do
    net1 <- loadGqrNetworkFile ("alleq.csp")
    net2 <- loadSparqNetworkFile ("alleq.spa")
    net3 <- loadSparqNetworkFile ("alleq.spa")
    net4 <- loadGqrNetworkFile ("wrong.csp")
    let nets = [net1, net2, net3, net4]
    answers <- checkConsistency "dra-24" nets
    print answers
    return ()

