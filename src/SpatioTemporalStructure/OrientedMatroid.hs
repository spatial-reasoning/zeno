module SpatioTemporalStructure.OrientedMatroid where

-- standard modules
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

-- local modules
import Basics
--import Calculus.Dipole
import qualified Helpful as H
import Interface.LpSolve
--import Interface.Clp

--import Debug.Trace

--type Assignment = Map.Map [Int] Int
--data Chirotope = Chirotope (Map.Map [Int] Int)


-- Assume that the simpler conditions are already satisfied (e.g. because of
-- preprocessing). Only check whether Grassmann-Pluecker can be satisfied.
-- Requires that the number of nodes is at least rank + 2 .
satisfiesGrassmannPluecker :: Map.Map [Int] Int -> Bool
satisfiesGrassmannPluecker m
    | null keys  = True
    | otherwise  =
        satisfiesGrassmannPluecker_worker missingPermutsWithParities m
    where
        keys = Map.keys m
        rank = length $ head keys
        sizeOfDomain = length $ nub $ concat keys
        domain = [0..sizeOfDomain - 1]
        missingPermutsWithParities = map
            (H.kPermutationsWithParity rank)
            (filter (flip Map.notMember m) $ H.kCombinations rank domain)
        -- This could be done wiser, so we don't have to split the list again
        -- below! See at function realizable.
        combis = foldl
            (\acc x -> (acc ++) $ map (x ++) $ H.kCombinations 4 (domain \\ x)
            ) [] (H.kCombinations (rank - 2) domain)
        satisfiesGrassmannPluecker_worker missingPwPs wM
            | and [ (\y -> (elem (-1) y && elem 1 y) || y == [0,0,0])
                     [ (Map.!) wM (x ++ [a,b]) * (Map.!) wM (x ++ [c,d])
                     , (Map.!) wM (x ++ [a,c]) * (Map.!) wM (x ++ [b,d]) * (-1)
                     , (Map.!) wM (x ++ [a,d]) * (Map.!) wM (x ++ [b,c])
                     ]
                  | xy <- combis
                  , let (x,[a,b,c,d]) = splitAt (rank - 2) xy
                  -- filter the ones in the map.
                      -- fixme: Here we could also drop
                      -- those triples, that have been tested in the last
                      -- backtracking step!
                  , Map.member (x ++ [a,b]) wM
                  , Map.member (x ++ [c,d]) wM
                  , Map.member (x ++ [a,c]) wM
                  , Map.member (x ++ [b,d]) wM
                  , Map.member (x ++ [a,d]) wM
                  , Map.member (x ++ [c,b]) wM
                  ]
              = if missingPwPs == [] then
                    True
                else
                    let
                        -- Some optimizations might be helpful at this place.
                        x:pwps = missingPwPs
                    in
                    or $ map
                        (\newSign -> satisfiesGrassmannPluecker_worker
                            pwps
                            (foldl (flip $ uncurry Map.insert) wM
                                [ (y, newSign * z) | (y, z) <- x ]
                            )
                        )
                        [(-1), 0, 1]
            | otherwise  = False


-- Assume that the simpler conditions (e.g. that m is alternating and not zero)
-- are already satisfied. Only check whether the third chirotope axiom can be
-- satisfied and whether the chirotope is acyclic.
-- TODO: Can we save time by using the fact that the map is alternating?
isAcyclicChirotope :: Map.Map [Int] Int 
                   ->(Map.Map [Int] Int
                   -> [[Int]]
                   -> Int
                   -> [Int]
                   -> Bool
                  )-> Bool
isAcyclicChirotope m f
    | null keys  = True
--    | not $ Map.null $ Map.filter (flip notElem [0,1] . abs) m  =
    | (not $ satisfiesThirdAxiom m) ||
                        -- /- improve: we should get this from the initial
                        -- /- network instead of reproducing it here.
                        ------------------------------
      (not $ isAcyclic (filter H.isSorted $ Map.keys m) m)  = False
    | otherwise  =
        isAcyclicChirotope_worker missingPermutsWithParities [] m nodesInM
  where
    keys = Map.keys m
    rank = length $ head keys
    domain = Set.toAscList $ Set.fromList $ concat keys
    nodesInM = nodesIn m
--    combis = H.kCombinations (rank + 1) domain
    missingPermutsWithParities = map
        (H.kPermutationsWithParity rank)
        (filter (flip Map.notMember m) $ H.kCombinations rank domain)
    applyMap k m =
        -- A triple containing the same node twice has orientation zero.
        -- TODO: teste ob das unechte triple nicht doch in der Map ist!
        if H.hasDuplicates k then
            Just 0
        else
            Map.lookup k m
    isAcyclicChirotope_worker missingPwPs newTuples wM nodesInwM
        | satisfiesThirdAxiom wM && isAcyclic (take 1 newTuples) wM =
            if missingPwPs == [] then
                -- here we can add a function to run on all full
                -- chirotopes, e.g. the function "is_realizable" !
--                True
                f wM keys rank domain
            else
              let
                -- Some optimizations might be helpful at this place.
                x:stillMissingPwPs = missingPwPs
              in
                or $ map
                    (\newSign ->
                      let
                        newConstraints = [ (y, newSign * z) | (y, z) <- x ]
                      in
                        isAcyclicChirotope_worker
                            stillMissingPwPs
                            (map fst newConstraints)
                            (foldr (uncurry Map.insert)
                                   wM newConstraints)
                            (foldr Set.insert
                                   nodesInwM
                                   (intercalate [] $ map fst $ take 1 x))

                    ) [(-1), 0, 1]
        | otherwise  = False
    -- here we check whether the chirotope axiom B2' from
    -- "A. Björner et al: Oriented Matroids, 2nd ed., page 128"
    -- is fulfilled. Only checking the Grassmann-Pluecker condition is not
    -- sufficient since we also have to check, that the absolute value of the
    -- mapping is a matroid.
    satisfiesThirdAxiom m =
      let
        -- improve: can we make use of the newly introduced constraints in
        -- order to reduce the number of tuples to test in this step?
        tuplePairs = H.kPermutations 2 $ Map.keys $ Map.filter (/= 0) m
      in
        and [ or
                [ case applyMap (y:tailTuple1) m of
                    Nothing    -> True
                    Just 0     -> False
                    Just rel3  -> case applyMap (lefty ++ x:righty) m of
                        Nothing    -> True
                        Just 0     -> False
                        Just rel4  -> rel1 * rel2 == rel3 * rel4
                | y <- tuple2
                , let (lefty, _:righty) = break (== y) tuple2
                ]
            | [tuple1@(x:tailTuple1), tuple2] <- tuplePairs
            , let rel1 = fromJust $ applyMap tuple1 m
            , let rel2 = fromJust $ applyMap tuple2 m
            ]
    -- See "Ralf Gugish: A Construction of Isomorphism Classes of Oriented
    --                   Matroids, in Algorithmic Algebraic Combinatorics and
    --                   Gröbner Bases, p. 236"
    isAcyclic newTuples m = {-# SCC "isAcyclic" #-}
      let
        positiveCircuitElemsOf z = filter
            (\x -> maybe True
                (\y -> (-1)^(fromJust $ elemIndex x z) * y == (-1)) $
                Map.lookup (delete x z) m
            ) z
        negativeCircuitElemsOf z = filter
            (\x -> maybe True
                (\y -> (-1)^(fromJust $ elemIndex x z) * y == 1) $
                Map.lookup (delete x z) m
            ) z
        combis = [ insert y x | x <- newTuples, y <- domain, not $ elem y x ]
        circuits = [ (positiveCircuitElemsOf c, negativeCircuitElemsOf c)
                   | c <- combis ]
      in
        not $ any (\(x,y) -> ( null y && not (null x) )
                          --fixme: is it right to include the negative circuits
                          --       here? I did it because the indian tent
                          --       configuration gave either a negative or a
                          --       positive circuit depending on the
                          --       orientation of the tent.
                          || ( null x && not (null y) )
                  ) circuits

-- search for a bi-quadratic final polynomial via translation into a
-- Linear Programming Problem. Only works for uniform chirotopes.
-- The chirotope axioms are not checked.
hasNoBiquadraticFinalPolynomial :: Map.Map [Int] Int
                                 -> [[Int]]
                                 -> Int
                                 -> [Int]
                                 -> Bool
hasNoBiquadraticFinalPolynomial m keys rank domain =
    case zeroObjective lpInput of
        Nothing    -> False
        Just True  -> False
        Just False -> True
  where
    lpInput = ("max: s ;\n" ++) $ concatMap
        (\[a,b,c,d] ->    "v_" ++ showBracket a ++
                       " + v_" ++ showBracket b ++
                       " - v_" ++ showBracket c ++
                       " - v_" ++ showBracket d ++ " + s <= 0 ;\n"
        ) absoluteQuadratics
--    lpInput = (++ "END") $ ("MIN\n    -s\nST\n" ++) $ concatMap
--        (\[a,b,c,d] -> "    v_" ++ showBracket a ++
--                        " + v_" ++ showBracket b ++
--                        " - v_" ++ showBracket c ++
--                        " - v_" ++ showBracket d ++ " + s <= 0\n"
--        ) absoluteQuadratics
    showBracket = intercalate "_" . map show
    absoluteQuadratics = map
        (\(x, [a,b,c,d]) -> [ sort $ x ++ [a,b]
                            , sort $ x ++ [c,d]
                            , sort $ x ++ [a,c]
                            , sort $ x ++ [b,d] ]
        ) $ filter
            (\(x, [a,b,c,d]) -> chi (x ++ [a,b]) * chi (x ++ [c,d]) > 0 &&
                                chi (x ++ [a,c]) * chi (x ++ [b,d]) > 0 &&
                                chi (x ++ [a,d]) * chi (x ++ [b,c]) > 0
            ) $ foldl
                (\acc x -> (acc ++) $
                    map (\y -> (x,y)) $ H.kPermutations 4 (domain \\ x)
                ) [] (H.kCombinations (rank - 2) domain)
    chi = (Map.!) m
--    keys = Map.keys m
--    rank = length $ head keys
--    domain = nub $ concat keys

hasBiquadraticFinalPolynomial :: Map.Map [Int] Int
                               -> [[Int]]
                               -> Int
                               -> [Int]
                               -> Bool
hasBiquadraticFinalPolynomial a b c d =
    not $ hasNoBiquadraticFinalPolynomial a b c d



