module FlipFlop where

-- standard modules
import qualified Data.Map as Map
import qualified Data.Set as Set
-- local modules
import Basics

uniteEqualPoints :: [Constraint] -> [Constraint]
uniteEqualPoints cons = foldl collectSingleConstraint [] cons
    where
        collectSingleConstraint newCons ([a, b, c], rel)
            | rel == Set.singleton "s" || rel == Set.singleton "e" = newCons
            | otherwise = newCons ++
                               [ ( map (applyMap singlePointMap) [a, b, c]
                                 , rel ) ]
        singlePointMap = foldl checkOneCon Map.empty cons
        checkOneCon conMap ([a, b, c], rel)
            | rel == Set.singleton "s" = let mappedA = applyMap conMap a
                           in
                           Map.insert c mappedA $ Map.map
                               (\x -> if x == c then mappedA else x)
                               conMap
            | rel == Set.singleton "e" = let mappedB = applyMap conMap b
                           in
                           Map.insert c mappedB $ Map.map
                               (\x -> if x == c then mappedB else x)
                               conMap
            | otherwise  = conMap
        applyMap m e = Map.findWithDefault e e m

