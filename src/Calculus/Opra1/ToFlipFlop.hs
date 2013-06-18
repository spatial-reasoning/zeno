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
    [ ([a, a ++ "_discordia_" ++ b, b], rel1)
    , ([b, b ++ "_discordia_" ++ a, a], rel2)
    ]
  where
    rel1 = getRel 1
    rel2 = getRel 3
    getRel n = case showC!!(n+1) of
        '0' -> F
        '1' -> L
        '2' -> B
        '3' -> R
        's' -> S
    showC = cShowRel c


opra1ToFlipFlopNet :: Network [String] (GRel Opra1)
                   -> Network [String] (GRel FlipFlop)
opra1ToFlipFlopNet net@Network{ nCons = cons } =
    net{ nCons = Map.foldrWithKey collectOneCon Map.empty cons }
  where
    collectOneCon [a, b] (GRel rels) consAcc =
        Map.insert [b, b ++ "_discordia_" ++ a, a] rels2 $
        Map.insert [a, a ++ "_discordia_" ++ b, b] rels1 consAcc
      where
        rels1 = GRel $ Set.map (convertRel 1) $ rels
        rels2 = GRel $ Set.map (convertRel 3) $ rels
        convertRel n r = case (cShowRel r)!!(n+1) of
            '0' -> F
            '1' -> L
            '2' -> B
            '3' -> R
            's' -> S


-- test
testNet = eNetwork
    { nCons = Map.fromList
        [ (["a","b"], Opra1_2_3)
        , (["a","c"], Opra1_0_2)
        , (["b","c"], Opra1_1_1)
        , (["c","d"], Opra1_3_0)
        ]
    }
