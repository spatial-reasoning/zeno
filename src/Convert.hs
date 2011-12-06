module Convert where

-- standard modules
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
-- local modules
import Basics
import Calculus.Dipole
import Calculus.FlipFlop
import qualified TriangleConsistency as T
import Helpful

import Debug.Trace

{------------------------------------------------------------------------------
    Dipoles to LR
------------------------------------------------------------------------------}

-- | Converts a single Dipole constranit into the corresponding list of
-- FlipFlop constraints.
dipoleToFlipFlop :: [String]
                 -> Dipole72
                 -> [([String], FlipFlop)]
dipoleToFlipFlop [a, b] rel =
    [ ([as, ae, bs], readRel [r1])
    , ([as, ae, be], readRel [r2])
    , ([bs, be, as], readRel [r3])
    , ([bs, be, ae], readRel [r4])
    ] where
        as = a ++ "_s"
        ae = a ++ "_e"
        bs = b ++ "_s"
        be = b ++ "_e"
        [r1, r2, r3, r4] = showRel rel

-- | Converts a Network of Dipole constraints into the corresponding Network of
-- FlipFlop constraints.
dipolesToFlipFlops :: Network [String] Dipole72
                   -> Maybe (Network [String] FlipFlop)
dipolesToFlipFlops net@Network { nCons = cons }
    | not $ Map.null $ Map.filterWithKey
        (\[a, b, c] rel -> Set.null rel)
        newCons  = Nothing
    | otherwise  = Just $ net{ nCons = Map.map Set.findMin newCons }
  where
    newCons = Map.foldrWithKey
        (\nodes rel mapAcc -> foldl
            (\acc (nodes2, rel2) -> tcInsert nodes2 (Set.singleton rel2) acc)
            mapAcc $
            dipoleToFlipFlop nodes rel
        )
        Map.empty
        cons

---- | Converts a Network of Dipole constraints into the corresponding Network of
---- FlipFlop constraints.
--dipolesToFlipFlops :: Network [String] Dipole72
--                   -> Network [String] FlipFlop
--dipolesToFlipFlops net@Network { nCons = cons } = net
--    { nCons = Map.foldrWithKey
--        (\nodes rel mapAcc -> foldl (flip $ uncurry Map.insert) mapAcc $
--                                                     dipoleToFlipFlop nodes rel
--        )
--        Map.empty
--        cons
--    }

 
{------------------------------------------------------------------------------
    FlipFlop-5 to Dominik
------------------------------------------------------------------------------}
flipFlop5sToDominik :: Network [String] FlipFlop -> [T.Rel]
flipFlop5sToDominik Network { nCons = cons } = Map.foldrWithKey
    (\ [a, b, c] rel ls -> insert (T.Rel a b (showRel rel) c) ls )
    []
    (enumerate cons)

 
{------------------------------------------------------------------------------
 - FlipFlop to Chirotope
------------------------------------------------------------------------------}

flipflop7ToChirotope :: Network [String] FlipFlop
                     -> Maybe (Network [Int] Int)
flipflop7ToChirotope net7
    | isNothing net5 || isNothing net3  = Nothing
    | otherwise  = Just $ (fromJust net3)
        { nCons = fst $ Map.foldrWithKey
                                collectOneCon
                                (Map.empty, Map.empty)
                                cons
        }
    where
        collectOneCon nodes rel (consAcc, mapAcc) =
            let
                (newMap, convertedNodes) = mapAccumL
                    (\ m node -> let mappedNode = Map.lookup node m in
                        case mappedNode of
                            Nothing   -> let n = Map.size m in
                                         (Map.insert node n m, n)
                            otherwise -> (m, fromJust mappedNode)
                    )
                    mapAcc
                    nodes
                newRel = case rel of
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
        net5 = ffsToFF5s net7
        net3 = ff5sToFF3s $ fromJust net5
        cons = nCons $ fromJust net3

