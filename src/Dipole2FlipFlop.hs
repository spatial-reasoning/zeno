module Dipole2FlipFlop where

-- standard modules
import qualified Data.Set as Set
import qualified Data.Map as Map
-- local modules
import Basics
import qualified TriangleConsistency as T

-- dipole relations modulo converses.
dipoleRelations =
    [ "bbbb" , "bbff" , "beie" , "bfii" , "biif"
    , "blrr" , "brll" , "bsef" , "ebis" , "eifs"
    , "ells" , "errs" , "eses" , "fbii" , "fefe"
    , "ffff" , "fifi" , "flll" , "frrr" , "ibib"
    , "illr" , "irrl" , "lbll" , "lere" , "lfrr"
    , "lirl" , "llll+", "lllla", "lllr" , "llrf"
    , "llrl" , "llrr+", "llrr-", "llrrp", "lrri"
    , "lrrl" , "lrrr" , "rbrr" , "rlrr" , "rrrr+"
    , "rrrra", "sbsb" , "sese" , "sfsi" , "slsr" ]

sortAtomicDipoleConstraint :: Constraint -> Constraint
sortAtomicDipoleConstraint (ents, rel)
    | rel > swappedRel = (reverse ents, swappedRel)
    | otherwise     = (ents, rel)
    where
        swappedRel = Set.map swapRel rel

swapRel rel = [c, d, a, b] ++ swappedSign
    where
        swappedSign = case s of
                          "+" -> "-"
                          "-" -> "+"
                          _   -> s
        a:b:c:d:s = rel

dipoleSort :: [Constraint] -> [Constraint]
dipoleSort cons = map sortAtomicDipoleConstraint cons

dipoleToFlipFlop :: Constraint -> [Constraint]
dipoleToFlipFlop ([a, b], rel) =
    [ ([as, ae, bs], Set.map ( (:[]) . (!! 0) ) $ rel)
    , ([as, ae, be], Set.map ( (:[]) . (!! 1) ) $ rel)
    , ([bs, be, as], Set.map ( (:[]) . (!! 2) ) $ rel)
    , ([bs, be, ae], Set.map ( (:[]) . (!! 3) ) $ rel) ]
    where
        as = a ++ "_s"
        ae = a ++ "_e"
        bs = b ++ "_s"
        be = b ++ "_e"

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

dipolesToFlipFlops :: [Constraint] -> [Constraint]
dipolesToFlipFlops cons = uniteEqualPoints $ concat $ map dipoleToFlipFlop cons

convertFlipFlopsForDominik :: [Constraint] -> [T.Rel]
convertFlipFlopsForDominik cons = map
    (\([a, b, c], rel) -> T.Rel
                              ( read a :: Int )
                              ( read b :: Int )
                              ( Set.findMin rel )
                              ( read c :: Int ) )
    (enumerate cons)

