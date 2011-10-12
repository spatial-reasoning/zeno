module OrientedMatroid where

-- standard modules
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Maybe

-- local modules
import Basics
import Calculus.Dipole
import Calculus.FlipFlop
import Convert
import qualified Helpful as H

--type Assignment = Map.Map [Int] Int
--data Chirotope = Chirotope (Map.Map [Int] Int)
--
--fromMap :: Map.Map a b -> Chirotope
--fromMap m
--    | isChirotope m = Chirotope m
--    | otherwise = error "huhu"

-- Here the Grassmann-Plücker condition is only checked for triples that are
-- already in the map! Triples not in the map are supposed to fullfil the
-- condition, which is not always true. So this function gives false positives.
isNoChirotope :: Map.Map [Int] Int -> Bool
isNoChirotope m
    | Set.isSubsetOf elems (Set.fromList [(-1), 0, 1])
        && elems /= Set.singleton 0
        && and (map ((== rank) . length) keys)
        && domain == [0..(length domain) - 1]
--        && notElem Nothing ( map (flip Map.lookup m) permuts )
        && ( and $ map
               ( \(x:xs) -> foldl
                   (\z y ->
                       z && ((m Map.! (fst y)) == (snd y) * (m Map.! (fst x)))
                   ) True xs
               ) $
               filter ((flip Map.member m) . fst . head) permutsAndParities
           )
        && and grassmannPluecker
        = False
    | otherwise = True
    where
        keys = Map.keys m
        elems = Set.fromList $ Map.elems m
        rank = length $ head keys
        domain = sort $ nub $ concat keys
        permutsAndParities =
            map (H.kPermutationsWithParity rank) $ H.kCombinations rank domain
--        permutsAndParities = H.kPermutationsWithParity rank domain
--        permuts = map fst permutsAndParities
        grassmannPluecker =
            [ (\x -> (elem (-1) x && elem 1 x) || x == [0,0,0])
                [ (Map.!) m (x ++ [a,b]) * (Map.!) m (x ++ [c,d])
                , (Map.!) m (x ++ [a,c]) * (Map.!) m (x ++ [b,d]) * (-1)
                , (Map.!) m (x ++ [a,d]) * (Map.!) m (x ++ [b,c])
                ]
            | xy <- H.kCombinations (rank + 2) domain
            , let (x,[a,b,c,d]) = splitAt (rank - 2) xy
            , Map.member (x ++ [a,b]) m -- filter the ones in the map.
            , Map.member (x ++ [c,d]) m
            , Map.member (x ++ [a,c]) m
            , Map.member (x ++ [b,d]) m
            , Map.member (x ++ [a,d]) m
            , Map.member (x ++ [c,b]) m
            ]

-- Test by backtracking. First check whether the function isNoChirotope
-- rejects the Map, if not then fix one open relation and all its permutations
-- and start again. Stop when one full assignment is found or when no
-- assignment is possible.
--isChirotope :: Map [Int] Int -> Bool
--isChirotope m
--    | isNoChirotope m  = False
--    | otherwise  =
--
--isChirotope_worker :: Map [Int] Int -> [([Int], Int)] -> [[Int]] -> Bool
--isChirotope_worker m cPermutsAndParities cCombinations = 



-- Assume that the simpler conditions testet in isChirotope are already
-- satisfied (e.g. because of preprocessing). Only check whether
-- Grassmann-Pluecker can be satisfied.
satisfiesGrassmannPluecker :: Map.Map [Int] Int -> Bool
satisfiesGrassmannPluecker m
    | null keys  = True
    | otherwise  =
        satisfiesGrassmannPluecker_worker remainingPermutsWithParities m
    where
        keys = Map.keys m
        rank = length $ head keys
        domain = [0..(length $ nub $ concat keys) - 1]
        remainingPermutsWithParities = map
            (H.kPermutationsWithParity rank)
            (filter (flip Map.notMember m) $ H.kCombinations rank domain)
        combis2 = H.kCombinations (rank + 2) domain
        satisfiesGrassmannPluecker_worker remainingPwPs wM
            | and [ (\x -> (elem (-1) x && elem 1 x) || x == [0,0,0])
                     [ (Map.!) wM (x ++ [a,b]) * (Map.!) wM (x ++ [c,d])
                     , (Map.!) wM (x ++ [a,c]) * (Map.!) wM (x ++ [b,d]) * (-1)
                     , (Map.!) wM (x ++ [a,d]) * (Map.!) wM (x ++ [b,c])
                     ]
                  | xy <- combis2
                  , let (x,[a,b,c,d]) = splitAt (rank - 2) xy
                  -- filter the ones in the map.
                  , Map.member (x ++ [a,b]) wM
                  , Map.member (x ++ [c,d]) wM
                  , Map.member (x ++ [a,c]) wM
                  , Map.member (x ++ [b,d]) wM
                  , Map.member (x ++ [a,d]) wM
                  , Map.member (x ++ [c,b]) wM
                  ]
              = if remainingPwPs == [] then
                    True
                else
                    or $ concat $ map
                        (\x -> map
                            (\newSign -> satisfiesGrassmannPluecker_worker
                                (delete x remainingPwPs)
                                (foldl (flip $ uncurry Map.insert) wM
                                    [ (y, newSign * z) | (y, z) <- x ]
                                )
                            )
                            [(-1), 0, 1]
                        )
                        remainingPwPs
            | otherwise  = False



{------------------------------------------------------------------------------
 - Test for FlipFlop and Dipole Networks
------------------------------------------------------------------------------}

isChirotopeFlipFlop :: Network [String] FlipFlop -> Maybe Bool
isChirotopeFlipFlop net
    | isNothing chiroNet  = Just False
    | satisfiesGrassmannPluecker $ nCons $ fromJust chiroNet  =
        if numberOfNodes net < 9  then
            Just True
        else
            Nothing
    | otherwise  = Just False
    where
        chiroNet = flipflop7ToChirotope net


isChirotopeDipole72 :: Network [String] Dipole72 -> Maybe Bool
isChirotopeDipole72 = isChirotopeFlipFlop . dipolesToFlipFlops


