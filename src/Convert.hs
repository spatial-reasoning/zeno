module Convert where

-- standard modules
import qualified Data.Map as Map
import qualified Data.Set as Set
-- local modules
import Basics
import Dipole
import FlipFlop
import OrientedMatroid
import qualified TriangleConsistency as T

----------------------------
-- Dipoles to LR -- Begin --
----------------------------
dipole72ToLR :: Constraint -> [Constraint]
dipole72ToLR ([a, b], rel) =
    [ ([as, ae, bs], Set.map ( (:[]) . (!! 0) ) $ rel)
    , ([as, ae, be], Set.map ( (:[]) . (!! 1) ) $ rel)
    , ([bs, be, as], Set.map ( (:[]) . (!! 2) ) $ rel)
    , ([bs, be, ae], Set.map ( (:[]) . (!! 3) ) $ rel) ]
    where
        as = a ++ "_s"
        ae = a ++ "_e"
        bs = b ++ "_s"
        be = b ++ "_e"

dipole72sToLRs :: [Constraint] -> [Constraint]
dipole72sToLRs cons = uniteEqualPoints $ concat $ map dipole72ToLR cons
--------------------------
-- Dipoles to LR -- End --
--------------------------

------------------------------
-- LR72 to Dominik -- Begin --
------------------------------
convertLR72sForDominik :: [Constraint] -> [T.Rel]
convertLR72sForDominik cons = map
    (\([a, b, c], rel) -> T.Rel
                              ( read a :: Int )
                              ( read b :: Int )
                              ( Set.findMin rel )
                              ( read c :: Int ) )
    (enumerate cons)
----------------------------
-- LR72 to Dominik -- End --
----------------------------

------------------------------------
-- FlipFlop to Chirotope -- Begin --
------------------------------------
convert_single_lr72_to_chirotope :: ([Int], Relation)
                      -> Int
                      -> (Map.Map [Int] Int, Int)
convert_single_lr72_to_chirotope ([a, b, c], r) maxEnt
    | r == Set.singleton "l" =
        ( Map.fromList [ ([a, b, c], 1)
                       , ([a, c, b], (-1))
                       , ([b, a, c], (-1))
                       , ([b, c, a], 1)
                       , ([c, a, b], 1)
                       , ([c, b, a], (-1))
                       ]
        , maxEnt
        )
    | r == Set.singleton "r" =
        ( Map.fromList [ ([a, b, c], (-1))
                       , ([a, c, b], 1)
                       , ([b, a, c], 1)
                       , ([b, c, a], (-1))
                       , ([c, a, b], (-1))
                       , ([c, b, a], 1)
                       ]
        , maxEnt
        )
    | r == Set.singleton "b" =
        ( Map.fromList [ ([a, b, c], 0)
                       , ([a, c, b], 0)
                       , ([b, a, c], 0)
                       , ([b, c, a], 0)
                       , ([c, a, b], 0)
                       , ([c, b, a], 0)
                       -- Comment needed
                       , ([a, b, newPoint], 1)
                       , ([a, newPoint, b],(-1))
                       , ([b, a, newPoint],(-1))
                       , ([b, newPoint, a], 1)
                       , ([newPoint, a, b], 1)
                       , ([newPoint, b, a], (-1))
                       -- Comment needed
                       , ([newPoint, a, c], (-1))
                       , ([newPoint, c, a], 1)
                       , ([a, newPoint, c], 1)
                       , ([a, c, newPoint], (-1))
                       , ([c, newPoint, a], (-1))
                       , ([c, a, newPoint], 1)
                       ]
        , newPoint
        )
    | r == Set.singleton "f" =
        ( Map.fromList [ ([a, b, c], 0)
                       , ([a, c, b], 0)
                       , ([b, a, c], 0)
                       , ([b, c, a], 0)
                       , ([c, a, b], 0)
                       , ([c, b, a], 0)
                       -- Comment needed
                       , ([a, b, newPoint], 1)
                       , ([a, newPoint, b],(-1))
                       , ([b, a, newPoint],(-1))
                       , ([b, newPoint, a], 1)
                       , ([newPoint, a, b], 1)
                       , ([newPoint, b, a], (-1))
                       -- Comment needed
                       , ([newPoint, b, c], 1)
                       , ([newPoint, c, b], (-1))
                       , ([b, newPoint, c], (-1))
                       , ([b, c, newPoint], 1)
                       , ([c, newPoint, b], 1)
                       , ([c, b, newPoint], (-1))
                       ]
        , newPoint
        )
    | r == Set.singleton "i" =
        ( Map.fromList [ ([a, b, c], 0)
                       , ([a, c, b], 0)
                       , ([b, a, c], 0)
                       , ([b, c, a], 0)
                       , ([c, a, b], 0)
                       , ([c, b, a], 0)
                       -- Comment needed
                       , ([a, b, newPoint], 1)
                       , ([a, newPoint, b],(-1))
                       , ([b, a, newPoint],(-1))
                       , ([b, newPoint, a], 1)
                       , ([newPoint, a, b], 1)
                       , ([newPoint, b, a], (-1))
                       -- Comment needed
                       , ([newPoint, a, c], 1)
                       , ([newPoint, c, a], (-1))
                       , ([a, newPoint, c], (-1))
                       , ([a, c, newPoint], 1)
                       , ([c, newPoint, a], 1)
                       , ([c, a, newPoint], (-1))
                       -- Comment needed
                       , ([newPoint, b, c], (-1))
                       , ([newPoint, c, b], 1)
                       , ([b, newPoint, c], 1)
                       , ([b, c, newPoint], (-1))
                       , ([c, newPoint, b], (-1))
                       , ([c, b, newPoint], 1)
                       ]
        , newPoint
        )
    where
        newPoint = maxEnt + 1

convert_lr72s_to_chirotope [] ents = (Map.empty, ents)
convert_lr72s_to_chirotope (x:y) ents = (Map.union newX newY, finalEnts)
    where
        (newX, newEnts)   = convert_single_lr72_to_chirotope x ents
        (newY, finalEnts) = convert_lr72s_to_chirotope y newEnts

convertLR72sToChirotope :: [Constraint] -> Map.Map [Int] Int
convertLR72sToChirotope cons =
    fst $ convert_lr72s_to_chirotope numericConstaints maxEntity
    where
        numericConstaints = enumerateToInt cons -- TODO: When Entities can be Ints, we wont have to read.
        maxEntity = Set.findMax $ listEntities $ numericConstaints

-- convert_single_lr72_to_chirotope :: Constraint
--                       -> Set.Set Entity
--                       -> (Map.Map [Int] Int, Set.Set Entity)
-- convert_single_lr72_to_chirotope ([a, b, c], r) ents
--     | r == Set.singleton "l" =
--         (Map.singleton (map read [a, b, c] :: [Int]) 1, ents)
--     | r == Set.singleton "r" =
--         (Map.singleton (map read [a, b, c] :: [Int]) (-1), ents)
--     | r == Set.singleton "b" =
--         ( Map.fromList [ (map read [a, b, c] :: [Int], 0)
--                        , (map read [a, b, newPoint] :: [Int], 1)
--                        , (map read [newPoint, a, c] :: [Int], (-1))
--                        ]
--         , Set.insert newPoint ents
--         )
--     | r == Set.singleton "f" =
--         ( Map.fromList [ (map read [a, b, c] :: [Int], 0)
--                        , (map read [a, b, newPoint] :: [Int], 1)
--                        , (map read [newPoint, b, c] :: [Int], 1)
--                        ]
--         , Set.insert newPoint ents
--         )
--     | r == Set.singleton "i" =
--         ( Map.fromList [ (map read [a, b, c] :: [Int], 0)
--                        , (map read [a, b, newPoint] :: [Int], 1)
--                        , (map read [newPoint, a, c] :: [Int], 1)
--                        , (map read [newPoint, b, c] :: [Int], (-1))
--                        ]
--         , Set.insert newPoint ents
--         )
--     where
--         newPoint = head $ filter
--             (flip Set.notMember ents)
--             [ show n | n <- [1..] ]
-- 
-- convert_lr72s_to_chirotope [] ents = (Map.empty, ents)
-- convert_lr72s_to_chirotope (x:y) ents = (Map.union newX newY, finalEnts)
--     where
--         (newX, newEnts)   = convert_single_lr72_to_chirotope x ents
--         (newY, finalEnts) = convert_lr72s_to_chirotope y newEnts
-- 
-- convertLR72sToChirotope :: [Constraint] -> Map.Map [Int] Int
-- convertLR72sToChirotope cons = fst $ convert_lr72s_to_chirotope numCons $ fst $ listEntities numCons
--     where
--         numCons = enumerate cons


----------------------------------
-- FlipFlop to Chirotope -- End --
----------------------------------
