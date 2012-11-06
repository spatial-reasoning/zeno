module Calculus.FlipFlop where

-- standard modules
import qualified Data.Char as Char
import qualified Data.Key as Key
import Data.List
import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

-- local modules
import Basics
import Interface.Sparq
import Helpful

import Debug.Trace

data FlipFlop = L | R | B | S | I | E | F | D | T
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Calculus FlipFlop where
    rank _ = 3
    readRel x = case maybeRead $ catchDouTri $ map Char.toUpper x of
        Just z  -> z
        Nothing -> error $ show x ++ " is not a FlipFlop relation."
      where
        catchDouTri "DOU" = "D"
        catchDouTri "TRI" = "T"
        catchDouTri x = x
    showRel D = "dou"
    showRel T = "tri"
    showRel x = (map Char.toLower) $ show x
    cBaserelationsArealList = [L, R]


    tcInvMap = Map.fromList
        [ (L, Set.singleton R)
        , (R, Set.singleton L)
        , (B, Set.singleton F)
        , (S, Set.singleton E)
        , (I, Set.singleton I)
        , (E, Set.singleton S)
        , (F, Set.singleton B)
        , (D, Set.singleton D)
        , (T, Set.singleton T) ]

    tcScMap = Map.fromList
        [ (L, Set.singleton R)
        , (R, Set.singleton L)
        , (B, Set.singleton B)
        , (S, Set.singleton D)
        , (I, Set.singleton F)
        , (E, Set.singleton E)
        , (F, Set.singleton I)
        , (D, Set.singleton S)
        , (T, Set.singleton T) ]

    tcHomMap = Map.fromList
        [ (L, Set.singleton L)
        , (R, Set.singleton R)
        , (B, Set.singleton I)
        , (S, Set.singleton E)
        , (I, Set.singleton F)
        , (E, Set.singleton D)
        , (F, Set.singleton B)
        , (D, Set.singleton S)
        , (T, Set.singleton T) ]


fflip :: FlipFlop -> FlipFlop
fflip S = E
fflip E = S
fflip L = R
fflip R = L
fflip F = B
fflip B = F
fflip I = I


ffsToFF5s :: Network [String] FlipFlop
          -> Maybe (Network [String] FlipFlop)
ffsToFF5s net@Network { nCons = cons } = do
    consWithSamesIdentified <- consWithSamesIdentifiedMaybe
    acc <- Key.foldlWithKeyM
                   checkOneCon
                   (Map.empty, cons, nodesIn $ nCons net)
                   consWithSamesIdentified
    let firstOfTriple (x, _, _) = x
    return $ net{ nCons = firstOfTriple acc}
  where
    checkOneCon acc@(consAcc, allConsAcc, nodesAcc) nodes@[a, b, c] rel
        -- Ensure the existence of a witness for the unsame nodes.
        | rel == S  = ite (a /= b) (ensureUnsamenessOf a b acc) Nothing
        | rel == E  = ite (a /= b) (ensureUnsamenessOf a b acc) Nothing
        | rel == D  = ite (a /= c) (ensureUnsamenessOf b c acc) Nothing
        | rel == T  = Just acc
        | otherwise = ite ((a /= b) && (a /= c) && (b /= c))
                          (Just ( fromJust $ insertConAtomic nodes rel consAcc
                                , allConsAcc
                                , nodesAcc ))
                          Nothing
    newMaxNode nodes = ("eris_" ++) $ maybe "1" (show . (+ 1) . fst) $
        Set.maxView $ Set.map
            (read . drop 5 :: String -> Int) $
            Set.filter (("eris_" ==) . take 5) nodes
    ensureUnsamenessOf x y acc@(consAccInter, allConsAccInter, nodesAccInter) =
      Just $
        if
            Map.null $ Map.filterWithKey
                (isWitnessForUnsamenessOf x y)
                allConsAccInter  --fixme: this should be allConsAccInter, no?
        then
          let
            newNode = newMaxNode nodesAccInter
          in
            ( fromJust $ insertConAtomic [x, y, newNode] L consAccInter
            , fromJust $ insertConAtomic [x, y, newNode] L allConsAccInter
            , Set.insert newNode nodesAccInter )
        else
            acc
    isWitnessForUnsamenessOf x y k v =
        (null $ [x, y] \\ k) && (elem v [L, R, B, I, F])
    consWithSamesIdentifiedMaybe =
        Key.foldlWithKeyM buildConsWithSamesIdentifiedMaybe Map.empty cons
    buildConsWithSamesIdentifiedMaybe consAcc nodes rel = insertConAtomic
        (map (applyMap nodesMap) nodes)
        rel
        consAcc
    nodesMap = Map.foldlWithKey buildNodesMap Map.empty cons
    buildNodesMap mapAcc nodes@[a, b, c] rel
        | rel == S = insertNodeMap c a mapAcc
        | rel == E = insertNodeMap c b mapAcc
        | rel == D = insertNodeMap b a mapAcc
        | rel == T = let eris = insertNodeMap b a mapAcc in
                     insertNodeMap c a eris
        | otherwise = mapAcc
    insertNodeMap node node2 m = case Map.lookup node m of
        Nothing    -> Map.insert node (applyMap m node2) m
        Just node3 ->
          let
            [n2, n3] = sort [applyMap m node2, node3]
            nodesToBeMapped = (Map.keys $ Map.filter (== n3) m)
                              ++ [node, node3]
          in
            foldl (\ acc x -> Map.insert x n2 acc) m nodesToBeMapped
    applyMap m node = Map.findWithDefault node node m
        


-- Convert Back, Inline and Front to Inline.
bifToI :: FlipFlop -> FlipFlop
bifToI B = I
bifToI F = I
bifToI x = x

-- Returns a network of FlipFlop3 constraints equivalent to the given FlipFlop5
-- network.
ff5sToFF3s :: Network [String] FlipFlop
           -> Maybe (Network [String] FlipFlop)
ff5sToFF3s net@Network{ nCons = cons } = do
    (newCons, _) <- Key.foldrWithKeyM
                        collectOneCon
                        (Map.empty, nodesIn $ nCons net)
                        cons
    Just net{ nCons = newCons }
  where
    -- TODO: How to best handle the problem of conversion from atomic to nonatomic and back?
    collectOneCon [a, b, c] rel (consAcc, nodesAcc)
        | rel == L || rel == R  = do
            maybeConsAcc <- insertConAtomic [a, b, c] rel consAcc
            Just (maybeConsAcc, nodesAcc)
        | rel == B  = do
            maybeConsAcc <- Fold.foldlM (flip $ uncurry insertConAtomic)
                                consAcc $
                                [ ([a, b, c], I)
                                , ([a, b, d], flipper L)
                                , ([d, a, c], flipper R)
                                ] ++
                                if new then
                                    inlineNodes
                                else
                                    []
            Just (maybeConsAcc, newNodesAcc)
        | rel == I  = do
            maybeConsAcc <- Fold.foldlM (flip $ uncurry insertConAtomic)
                                consAcc $
                                [ ([a, b, c], I)
                                , ([a, b, d], flipper L)
                                , ([d, a, c], flipper L)
                                , ([d, b, c], flipper R) ]
                                ++
                                if new then
                                    inlineNodes
                                else
                                    []
            Just (maybeConsAcc, newNodesAcc)
        | rel == F  = do
            maybeConsAcc <- Fold.foldlM (flip $ uncurry insertConAtomic)
                                consAcc $
                                [ ([a, b, c], I)
                                , ([a, b, d], flipper L)
                                , ([d, b, c], flipper L) ]
                                ++
                                if new then
                                    inlineNodes
                                else
                                    []
            Just (maybeConsAcc, newNodesAcc)
      where
        newNodesAcc = Set.insert d nodesAcc
        (d, flipper, new)
            | isJust leftD  = (fst $ fromJust leftD , id   , False)
            | isJust rightD = (fst $ fromJust rightD, fflip, False)
            | otherwise     = (newD                 , id   , True )
        leftD = Set.minView $ Set.filter
            (\node ->
                relOfAtomic (Map.union cons consAcc) [a, b, node] == Just L
            ) nodesAcc
        rightD = Set.minView $ Set.filter
            (\node ->
                relOfAtomic (Map.union cons consAcc) [a, b, node] == Just R
            ) nodesAcc
        -- if we could generalize the generation of newD we wouldn't be
        -- restricted to Strings as the type of the nodes.
--        newD = concat $ Set.toList nodesAcc
        newD = ("eris_" ++) $ maybe "1" (show . (+ 1) . fst) $ Set.maxView $
            Set.map
                (read . drop 5 :: String -> Int) $
                Set.filter (("eris_" ==) . take 5) nodesAcc
        inlineNodes = Set.fold (\x pairAcc -> Set.fold
          (\y pairAcc2 ->
            if
                 (x /= y)
              && (    (    relOfAtomic cons [a, b, x] == Just B
                        && (    (    relOfAtomic cons [a, b, y] == Just B
                                  && relOfAtomic cons [x, y, a] == Just F
                                  && relOfAtomic cons [x, y, b] == Just F )
                             || y == a
                             || relOfAtomic cons [a, b, y] == Just I
                             || y == b
                             || relOfAtomic cons [a, b, y] == Just F ))
                   || (    x == a
                        && (    relOfAtomic cons [a, b, y] == Just I
--                             || y == b
                             || relOfAtomic cons [a, b, y] == Just F ))
                   || (    relOfAtomic cons [a, b, x] == Just I
                        && (    (    relOfAtomic cons [a, b, y] == Just I
                                  && relOfAtomic cons [x, y, a] == Just B
                                  && relOfAtomic cons [x, y, b] == Just F )
                             || y == b
                             || relOfAtomic cons [a, b, y] == Just F ))
                   || (    x == b
                        && relOfAtomic cons [a, b, y] == Just F )
                   || (    relOfAtomic cons [a, b, x] == Just F
                        && relOfAtomic cons [a, b, y] == Just F
                        && relOfAtomic cons [x, y, a] == Just B
                        && relOfAtomic cons [x, y, b] == Just B ))
            then
                ([x, y, d], flipper L):pairAcc2
            else
                pairAcc2
          ) pairAcc nodesAcc ) [] nodesAcc

