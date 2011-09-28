module Convert where

-- standard modules
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
-- local modules
import Basics
import Calculus.Dipole
import Calculus.FlipFlop
import OrientedMatroid
import qualified TriangleConsistency as TC


{------------------------------------------------------------------------------
    Dipoles to LR
------------------------------------------------------------------------------}

-- | Converts a single Dipole constranit into the corresponding list of
-- FlipFlop constraints.
dipoleToFlipFlop :: [String]
                 -> Dipole72
                 -> [([String], FlipFlop7)]
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
                   -> Network [String] FlipFlop7
dipolesToFlipFlops net@Network { nCons = cons } = net
    { nCons = Map.foldrWithKey
        (\nodes rel mapAcc -> foldl (flip $ uncurry Map.insert) mapAcc $
                                                     dipoleToFlipFlop nodes rel
        )
        Map.empty
        cons
    }

 
{------------------------------------------------------------------------------
    FlipFlop-5 to Dominik
------------------------------------------------------------------------------}
flipFlop5sToDominik :: Network [String] FlipFlop5 -> [TC.Rel]
flipFlop5sToDominik Network { nCons = cons } = Map.foldrWithKey
    (\ [a, b, c] rel ls -> List.insert (TC.Rel a b (showRel rel) c) ls )
    []
    (enumerate cons)

 
{------------------------------------------------------------------------------
 - FlipFlop to Chirotope
------------------------------------------------------------------------------}

--convertSingleFF7ToChirotope :: Constraint a FlipFlop7 -> ([Int]
--convertSingleFF7ToChirotope Constraint { conNodes = nodes, conRel = rel } =
    


--
-- convert_single_lr72_to_chirotope :: ([Int], Relation)
--                       -> Int
--                       -> (Map.Map [Int] Int, Int)
-- convert_single_lr72_to_chirotope ([a, b, c], r) maxEnt
--     | r == Set.singleton "l" =
--         ( Map.fromList [ ([a, b, c], 1)
--                        , ([a, c, b], (-1))
--                        , ([b, a, c], (-1))
--                        , ([b, c, a], 1)
--                        , ([c, a, b], 1)
--                        , ([c, b, a], (-1))
--                        ]
--         , maxEnt
--         )
--     | r == Set.singleton "r" =
--         ( Map.fromList [ ([a, b, c], (-1))
--                        , ([a, c, b], 1)
--                        , ([b, a, c], 1)
--                        , ([b, c, a], (-1))
--                        , ([c, a, b], (-1))
--                        , ([c, b, a], 1)
--                        ]
--         , maxEnt
--         )
--     | r == Set.singleton "b" =
--         ( Map.fromList [ ([a, b, c], 0)
--                        , ([a, c, b], 0)
--                        , ([b, a, c], 0)
--                        , ([b, c, a], 0)
--                        , ([c, a, b], 0)
--                        , ([c, b, a], 0)
--                        -- Comment needed
--                        , ([a, b, newPoint], 1)
--                        , ([a, newPoint, b],(-1))
--                        , ([b, a, newPoint],(-1))
--                        , ([b, newPoint, a], 1)
--                        , ([newPoint, a, b], 1)
--                        , ([newPoint, b, a], (-1))
--                        -- Comment needed
--                        , ([newPoint, a, c], (-1))
--                        , ([newPoint, c, a], 1)
--                        , ([a, newPoint, c], 1)
--                        , ([a, c, newPoint], (-1))
--                        , ([c, newPoint, a], (-1))
--                        , ([c, a, newPoint], 1)
--                        ]
--         , newPoint
--         )
--     | r == Set.singleton "f" =
--         ( Map.fromList [ ([a, b, c], 0)
--                        , ([a, c, b], 0)
--                        , ([b, a, c], 0)
--                        , ([b, c, a], 0)
--                        , ([c, a, b], 0)
--                        , ([c, b, a], 0)
--                        -- Comment needed
--                        , ([a, b, newPoint], 1)
--                        , ([a, newPoint, b],(-1))
--                        , ([b, a, newPoint],(-1))
--                        , ([b, newPoint, a], 1)
--                        , ([newPoint, a, b], 1)
--                        , ([newPoint, b, a], (-1))
--                        -- Comment needed
--                        , ([newPoint, b, c], 1)
--                        , ([newPoint, c, b], (-1))
--                        , ([b, newPoint, c], (-1))
--                        , ([b, c, newPoint], 1)
--                        , ([c, newPoint, b], 1)
--                        , ([c, b, newPoint], (-1))
--                        ]
--         , newPoint
--         )
--     | r == Set.singleton "i" =
--         ( Map.fromList [ ([a, b, c], 0)
--                        , ([a, c, b], 0)
--                        , ([b, a, c], 0)
--                        , ([b, c, a], 0)
--                        , ([c, a, b], 0)
--                        , ([c, b, a], 0)
--                        -- Comment needed
--                        , ([a, b, newPoint], 1)
--                        , ([a, newPoint, b],(-1))
--                        , ([b, a, newPoint],(-1))
--                        , ([b, newPoint, a], 1)
--                        , ([newPoint, a, b], 1)
--                        , ([newPoint, b, a], (-1))
--                        -- Comment needed
--                        , ([newPoint, a, c], 1)
--                        , ([newPoint, c, a], (-1))
--                        , ([a, newPoint, c], (-1))
--                        , ([a, c, newPoint], 1)
--                        , ([c, newPoint, a], 1)
--                        , ([c, a, newPoint], (-1))
--                        -- Comment needed
--                        , ([newPoint, b, c], (-1))
--                        , ([newPoint, c, b], 1)
--                        , ([b, newPoint, c], 1)
--                        , ([b, c, newPoint], (-1))
--                        , ([c, newPoint, b], (-1))
--                        , ([c, b, newPoint], 1)
--                        ]
--         , newPoint
--         )
--     where
--         newPoint = maxEnt + 1
-- 
-- convert_lr72s_to_chirotope [] ents = (Map.empty, ents)
-- convert_lr72s_to_chirotope (x:y) ents = (Map.union newX newY, finalEnts)
--     where
--         (newX, newEnts)   = convert_single_lr72_to_chirotope x ents
--         (newY, finalEnts) = convert_lr72s_to_chirotope y newEnts
-- 
-- convertLR72sToChirotope :: [Constraint] -> Map.Map [Int] Int
-- convertLR72sToChirotope cons =
--     fst $ convert_lr72s_to_chirotope numericConstaints maxEntity
--     where
--         numericConstaints = enumerateToInt cons -- TODO: When Entities can be Ints, we wont have to read.
--         maxEntity = Set.findMax $ nodesIn $ numericConstaints
-- 
-- -- convert_single_lr72_to_chirotope :: Constraint
-- --                       -> Set.Set Entity
-- --                       -> (Map.Map [Int] Int, Set.Set Entity)
-- -- convert_single_lr72_to_chirotope ([a, b, c], r) ents
-- --     | r == Set.singleton "l" =
-- --         (Map.singleton (map read [a, b, c] :: [Int]) 1, ents)
-- --     | r == Set.singleton "r" =
-- --         (Map.singleton (map read [a, b, c] :: [Int]) (-1), ents)
-- --     | r == Set.singleton "b" =
-- --         ( Map.fromList [ (map read [a, b, c] :: [Int], 0)
-- --                        , (map read [a, b, newPoint] :: [Int], 1)
-- --                        , (map read [newPoint, a, c] :: [Int], (-1))
-- --                        ]
-- --         , Set.insert newPoint ents
-- --         )
-- --     | r == Set.singleton "f" =
-- --         ( Map.fromList [ (map read [a, b, c] :: [Int], 0)
-- --                        , (map read [a, b, newPoint] :: [Int], 1)
-- --                        , (map read [newPoint, b, c] :: [Int], 1)
-- --                        ]
-- --         , Set.insert newPoint ents
-- --         )
-- --     | r == Set.singleton "i" =
-- --         ( Map.fromList [ (map read [a, b, c] :: [Int], 0)
-- --                        , (map read [a, b, newPoint] :: [Int], 1)
-- --                        , (map read [newPoint, a, c] :: [Int], 1)
-- --                        , (map read [newPoint, b, c] :: [Int], (-1))
-- --                        ]
-- --         , Set.insert newPoint ents
-- --         )
-- --     where
-- --         newPoint = head $ filter
-- --             (flip Set.notMember ents)
-- --             [ show n | n <- [1..] ]
-- -- 
-- -- convert_lr72s_to_chirotope [] ents = (Map.empty, ents)
-- -- convert_lr72s_to_chirotope (x:y) ents = (Map.union newX newY, finalEnts)
-- --     where
-- --         (newX, newEnts)   = convert_single_lr72_to_chirotope x ents
-- --         (newY, finalEnts) = convert_lr72s_to_chirotope y newEnts
-- -- 
-- -- convertLR72sToChirotope :: [Constraint] -> Map.Map [Int] Int
-- -- convertLR72sToChirotope cons = fst $ convert_lr72s_to_chirotope numCons $ fst $ nodesIn numCons
-- --     where
-- --         numCons = enumerate cons

