module Convert.LRChirotope where

-- standard modules
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
-- local modules
import Basics
import Calculus.FlipFlop
import Helpful.General

--import Debug.Trace

{------------------------------------------------------------------------------
 - FlipFlop to Chirotope
------------------------------------------------------------------------------}

flipflopsToChirotope :: Network [String] (ARel FlipFlop)
                     -> Maybe (Network [Int] Int)
flipflopsToChirotope net
    | isNothing net5 || isNothing net3  = Nothing
    | otherwise  = Just $ (fromJust net3)
        { nCons = fst $ Map.foldlWithKey
                                collectOneCon
                                (Map.empty, Map.empty)
                                cons
        }
    where
        collectOneCon (consAcc, mapAcc) nodes rel =
            let
                (newMap, convertedNodes) = mapAccumL
                    (\ m node -> let mappedNode = Map.lookup node m in
                        case mappedNode of
                            Nothing   -> let n = (Map.size m) + 1 in
                                         (Map.insert node n m, n)
                            otherwise -> (m, fromJust mappedNode)
                    )
                    mapAcc
                    nodes
                newRel = case aRel rel of
                    R -> (-1)
                    I -> 0
                    L -> 1
            in
            ( foldl (flip $ uncurry Map.insert) consAcc $
                [(x, newRel * y)
                | (x,y) <- kPermutationsWithParity 3 convertedNodes
                ]
            , newMap
            )
        net5 = ffsToFF5s net
        net3 = ff5sToFF3s $ fromJust net5
        cons = nCons $ fromJust net3

