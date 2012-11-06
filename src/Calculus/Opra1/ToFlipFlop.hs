module Calculus.Opra1.ToFlipFlop where

--fixme: fix the whole module! and move it to Convert/

-- standard modules
import qualified Data.Map as Map
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Opra1
import Calculus.FlipFlop

opra1ToFlipFlopRel :: [String] -> Opra1 -> [([String], FlipFlop)]
opra1ToFlipFlopRel [a,b] c =
    [ ([a, "eris_1", b], rel1)
    , ([b, "eris_2", a], rel2)
    ]
  where
    rel1 = getRel 1
    rel2 = getRel 2
    getRel n = case showC!!(n+5) of
        '0' -> F
        '1' -> L
        '2' -> B
        '3' -> R
        'S' -> S
    showC = showRel c


opra1ToFlipFlopNet :: Network [String] (Set.Set Opra1)
                   -> Network [String] (Set.Set FlipFlop)
opra1ToFlipFlopNet net@Network{ nCons = cons } =
    net{ nCons = Map.foldrWithKey collectOneCon Map.empty cons
       }
  where
    endpoints = Map.fromList $ fst $ Set.foldl
        (\ (pairAcc, inc) node ->
            ( (node, "eris_" ++ show inc):pairAcc
            , inc + 1 )
        ) ([], 1) $ nodesIn $ nCons net
    collectOneCon [a, b] rels consAcc =
        Map.insert [b, (Map.!) endpoints b, a] rels2 $
        Map.insert [a, (Map.!) endpoints a, b] rels1 consAcc
      where
        rels1 = Set.map (convertRel 1) $ rels
        rels2 = Set.map (convertRel 2) $ rels
        convertRel n r = case (showRel r)!!(n+5) of
            '0' -> F
            '1' -> L
            '2' -> B
            '3' -> R
            'S' -> S


-- test
testNet = eNetwork
    { nCons = Map.fromList
        [ (["a","b"], Opra1_2_3)
        , (["a","c"], Opra1_0_2)
        , (["b","c"], Opra1_1_1)
        , (["c","d"], Opra1_3_0)
        ]
    }
