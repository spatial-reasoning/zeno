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
import Interface.LpSolve
--import Interface.Clp

import Debug.Trace

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
                      -- FIXME: Here we could also drop
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
--                   -> [Bool]             -- TWOINONE
                   -> Bool
isAcyclicChirotope m f onlyTestTheGivenMapForAcyclicity
--    | null keys  = [True, True]          -- TWOINONE
    | null keys  = True
--    | not $ Map.null $ Map.filter (flip notElem [0,1] . abs) m  =
    | not $ isAcyclic (Map.toList m) m  = False
    | otherwise  =
        isAcyclicChirotope_worker missingPermutsWithParities [] m
  where
    keys = Map.keys m
    rank = length $ head keys
    domain = nub $ concat keys
--    combis = H.kCombinations (rank + 1) domain
    missingPermutsWithParities = map
        (H.kPermutationsWithParity rank)
        (filter (flip Map.notMember m) $ H.kCombinations rank domain)
    applyMap k m =
        -- ein unechtes Triple hat Orientierung Null
        -- TODO: teste ob das unechte triple nicht doch in der Map ist!
        if H.hasDuplicates k then
            Just 0
        else
            Map.lookup k m
    isAcyclicChirotope_worker missingPwPs newCons wM
        | satisfiesThirdAxiom newCons wM &&
          ( onlyTestTheGivenMapForAcyclicity || isAcyclic (take 1 newCons) wM )
           =
            if missingPwPs == [] then
                -- here we can add a function to run on all full
                -- chirotopes, e.g. the function "is_realizable" !
--                True
                f wM keys rank domain
--                [True, f wM keys rank domain]          -- TWOINONE
            else
              let
                -- Some optimizations might be helpful at this place.
                x:stillMissingPwPs = missingPwPs
              in
--                map or $ transpose $ map          -- TWOINONE
                or $ map
                    (\newSign ->
                      let
                        newConstraints = [ (y, newSign * z) | (y, z) <- x ]
                      in
--                        trace (
--                                "\nassigning " ++ show x ++ " to " ++ show newSign ++ "\n"
--                                ++
--                                "length of missing constraints: " ++ (show $ length stillMissingPwPs)
--                                "a"
--                              ) $
                        isAcyclicChirotope_worker
                            stillMissingPwPs
                            newConstraints $
                            foldl (flip $ uncurry Map.insert) wM newConstraints
                    ) [(-1), 0, 1]
--        | otherwise  = [False, False]            -- TWOINONE
        | otherwise  = False
    satisfiesThirdAxiom newCons m =
      let
        nonzeroNewCons = filter (\(_,x) -> x /= 0 ) newCons
        nonzeroM = Map.filter (/= 0) m
      in
        ( foldl
            (\acc (nodes@(x:tailNodes), rel) -> (acc &&) $ Map.foldrWithKey
                (\nodes2 rel2 acc2 -> (acc2 &&) $
                    or [ case applyMap (y:tailNodes) m of
                           Nothing -> True
                           Just 0 ->  False
                           Just rel3 -> case applyMap (lefty ++ x:righty) m of
                               Nothing -> True
                               Just 0 -> False
                               Just rel4 -> rel * rel2 == rel3 * rel4
                       | y <- nodes2
                       , let (lefty, _:righty) = break (== y) nodes2
                       ]
                ) True nonzeroM
            ) True nonzeroNewCons
        ) &&
        ( Map.foldrWithKey
            (\nodes@(x:tailNodes) rel acc ->
                (acc &&) $ foldl
                    (\acc2 (nodes2, rel2) -> (acc2 &&) $ or
                        [ case applyMap (y:tailNodes) m of
                            Nothing -> True
                            Just 0 ->  False
                            Just rel3 -> case applyMap (lefty ++ x:righty) m of
                                Nothing -> True
                                Just 0 -> False
                                Just rel4 -> rel * rel2 == rel3 * rel4
                        | y <- nodes2
                        , let (lefty, _:righty) = break (== y) nodes2
                        ]
                    ) True nonzeroNewCons
            ) True nonzeroM
        )
    -- TODO: Can we just generate those quadruples of which a corresponding
    -- triple has been added in the last backtracking step?
-- Complicated repaired version:
--    isAcyclic m = not $ any (\[x,y] -> (null x && (not $ null y))
--                                    || (null y && (not $ null x))
--                            ) $ circuits m
--    circuits m = map fst $ mapMaybe
--        (\combi ->
--          let
--            subCombis = [ ( delete node combi
--                          , ( node, fromJust $ elemIndex node combi )
--                          )
--                        | node <- combi ]
--          in
--            maybe Nothing
--              (Just . foldl
--                    (\([posAcc, negAcc], (subCombi, (node, index)):rSubCombis) rel ->
--                        case (-1)^index * rel of
--                            (-1) -> ([node:posAcc, negAcc], rSubCombis)
--                            1    -> ([posAcc, node:negAcc], rSubCombis)
--                            0    -> ([posAcc, negAcc], rSubCombis)
--                            _    -> error $ "This is not a chirotope!" ++
--                                            show m
--                    ) ([[],[]] , subCombis)
--               ) $ mapM (flip Map.lookup m) $ map fst subCombis
--        ) $ H.kCombinations (rank + 1) domain
---- Simple repaired version:
--    isAcyclic m =
--      let
--        circuits = [ (positiveCircuitOf x m, negativeCircuitOf x m) | x <- combis ] 
--      in
--        not $ any (\(x,y) -> ( null x && not (null y) )
--                          || ( null y && not (null x) )
--                  ) circuits
--    positiveCircuitOf z m = filter
--        (\x -> maybe True
--            (\y -> (-1)^(fromJust $ elemIndex x z) * y == (-1)) $
--            Map.lookup (delete x z) m
--        ) z
--    negativeCircuitOf z m = filter
--        (\x -> maybe True
--            (\y -> (-1)^(fromJust $ elemIndex x z) * y == 1) $
--            Map.lookup (delete x z) m
--        ) z
-- Faster version only testing the newly inserted triples :
    isAcyclic newCons m = {-# SCC "isAcyclic" #-}
      let
        positiveCircuitOf z = filter
            (\x -> maybe True
                (\y -> (-1)^(fromJust $ elemIndex x z) * y == (-1)) $
                Map.lookup (delete x z) m
            ) z
        negativeCircuitOf z = filter
            (\x -> maybe True
                (\y -> (-1)^(fromJust $ elemIndex x z) * y == 1) $
                Map.lookup (delete x z) m
            ) z
        combis = [ insert y x | (x,_) <- newCons, y <- domain, not $ elem y x ]
        circuits = [ (positiveCircuitOf c, negativeCircuitOf c) | c <- combis ]
      in
        not $ any (\(x,y) -> ( null x && not (null y) )
                          || ( null y && not (null x) )
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


{------------------------------------------------------------------------------
 - Test for FlipFlop and Dipole Networks
------------------------------------------------------------------------------}

isAcyclicChirotopeFlipFlop :: Bool -> Network [String] FlipFlop -> Maybe Bool
isAcyclicChirotopeFlipFlop sloppy net
--    | (isJust chiroNet &&) $ head $ isAcyclicChirotope      -- TWOINONE
    | (isJust chiroNet &&) $ isAcyclicChirotope
        (nCons $ fromJust chiroNet) (\_ _ _ _ -> True) sloppy =
        if (numberOfNodes (fromJust chiroNet) < 9) && (not sloppy) then
            Just True
        else
            Nothing
    | otherwise  = Just False
  where
    chiroNet = flipflop7ToChirotope net


isAcyclicChirotopeDipole72 :: Bool -> Network [String] Dipole72 -> Maybe Bool
isAcyclicChirotopeDipole72 sloppy net
    | isNothing ffNet  = Nothing
    | otherwise  = isAcyclicChirotopeFlipFlop sloppy $ fromJust ffNet
  where
    ffNet = dipolesToFlipFlops net


isAcyclicChirotopeWithoutBPFlipFlop :: Bool -> Network [String] FlipFlop -> Maybe Bool
isAcyclicChirotopeWithoutBPFlipFlop sloppy net
--    | (isJust chiroNet &&) $ last $ isAcyclicChirotope          -- TWOINONE
    | (isJust chiroNet &&) $ isAcyclicChirotope
        (nCons $ fromJust chiroNet) hasNoBiquadraticFinalPolynomial sloppy =
        if (numberOfNodes (fromJust chiroNet) < 9) && (not sloppy) then
            Just True
        else
            Nothing
    | otherwise  = Just False
  where
    chiroNet = flipflop7ToChirotope net


isAcyclicChirotopeWithoutBPDipole72 :: Bool -> Network [String] Dipole72 -> Maybe Bool
isAcyclicChirotopeWithoutBPDipole72 sloppy net
    | isNothing ffNet  = Nothing
    | otherwise  = isAcyclicChirotopeWithoutBPFlipFlop sloppy $ fromJust ffNet
  where
    ffNet = dipolesToFlipFlops net

{-       TWOINONE
isAcyclicChirotopePlainAndWithoutBPFlipFlop :: Network [String] FlipFlop -> [Maybe Bool]
isAcyclicChirotopePlainAndWithoutBPFlipFlop net
    | isNothing chiroNet  = [Just False, Just False]
    | numberOfNodes (fromJust chiroNet) < 9  = map Just answers
    | otherwise  = map trueToNothing answers
  where
    chiroNet = flipflop7ToChirotope net
    answers = isAcyclicChirotope
        (nCons $ fromJust chiroNet)
        hasNoBiquadraticFinalPolynomial
    trueToNothing True = Nothing
    trueToNothing False = Just False

isAcyclicChirotopePlainAndWithoutBPDipole72 :: Network [String] Dipole72 -> [Maybe Bool]
isAcyclicChirotopePlainAndWithoutBPDipole72 net
    | isNothing ffNet  = [Nothing, Nothing]
    | otherwise  = isAcyclicChirotopePlainAndWithoutBPFlipFlop $ fromJust ffNet
  where
    ffNet = dipolesToFlipFlops net
-}

