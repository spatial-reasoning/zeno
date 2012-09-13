module Calculus.FlipFlop where

-- standard modules
import qualified Data.Char as Char
import qualified Data.Key as Key
import Data.List
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
    newMaxNode nodes = (Set.findMax nodes) ++ "_i_scream"
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
        | rel == S = Map.insert c (applyMap mapAcc a) mapAcc
        | rel == E = Map.insert c (applyMap mapAcc b) mapAcc
        | rel == D = Map.insert b (applyMap mapAcc a) mapAcc
        | rel == T = let
                          eris = Map.insert b (applyMap mapAcc a) mapAcc
                      in
                      Map.insert c (applyMap eris a) $ eris
        | otherwise = mapAcc
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
ff5sToFF3s nnnet
    | consistent == Just False || isNothing nnet
                               || elem Set.empty (Map.elems newCons)  = Nothing
    | otherwise  = Just $ makeAtomic $ net { nCons = newCons }
  where
    newCons = fst $ Map.foldrWithKey
                        collectOneCon
                        (Map.empty, nodesIn $ nCons net)
                        cons
    -- TODO: How to best handle the problem of conversion from atomic to nonatomic and back?
    (consistent, _, aClosedNnnet) = algebraicClosure "ff" $ makeNonAtomic nnnet
    nnet = ffsToFF5s $ makeAtomic $ aClosedNnnet
--        { nCons = Map.map (Set.map bifToI) $ nCons aClosedNnnet }    -- Why the heck did i write this?
    net@Network { nCons = cons } = fromJust nnet
    collectOneCon [a, b, c] rel (consAcc, nodesAcc)
        | rel == L || rel == R  =
            ( insertCon [a, b, c] (Set.singleton rel) consAcc
            , nodesAcc )
        | rel == B  =
            ( foldl (flip $ uncurry insertCon) consAcc
                ( [ ([a, b, c], Set.singleton I)
                  , ([a, b, d], Set.singleton $ flipper L)
                  , ([d, a, c], Set.singleton $ flipper R) ]
                  ++
                  if new then
                      inlineNodes
                  else
                      []
                )
            , newNodesAcc )
        | rel == I  =
            ( foldl (flip $ uncurry insertCon) consAcc
                ( [ ([a, b, c], Set.singleton I)
                  , ([a, b, d], Set.singleton $ flipper L)
                  , ([d, a, c], Set.singleton $ flipper L)
                  , ([d, b, c], Set.singleton $ flipper R) ]
                  ++
                  if new then
                      inlineNodes
                  else
                      []
                )
            , newNodesAcc
            )
        | rel == F  =
            ( foldl (flip $ uncurry insertCon) consAcc
                ( [ ([a, b, c], Set.singleton I)
                  , ([a, b, d], Set.singleton $ flipper L)
                  , ([d, b, c], Set.singleton $flipper L) ]
                  ++
                  if new then
                      inlineNodes
                  else
                      []
                )
            , newNodesAcc
            )
      where
        newNodesAcc = Set.insert d nodesAcc
        (d, flipper, new)
            | isJust leftD  = (fst $ fromJust leftD , id   , False)
            | isJust rightD = (fst $ fromJust rightD, fflip, False)
            | otherwise     = (newD                 , id   , True)
        leftD = Set.minView $ Set.filter
            (\node ->
                relOf
                    (Map.union (Map.map Set.singleton cons) consAcc)
                    [a, b, node]
                == (Just $ Set.singleton L)
            ) nodesAcc
        rightD = Set.minView $ Set.filter
            (\node ->
                relOf
                    (Map.union (Map.map Set.singleton cons) consAcc)
                    [a, b, node]
                == (Just $ Set.singleton R)
            ) nodesAcc
        -- if we could generalize the generation of newD we wouldn't be
        -- restricted to Strings as the type of the nodes.
--        newD = concat $ Set.toList nodesAcc
        newD = ("eris_" ++) $ maybe "1" (show . (+ 1) . fst) $ Set.maxView $
            Set.map
                (read . drop 5 :: String -> Int) $
                Set.filter (("eris_" ==) . take 5) nodesAcc
        inlineNodes = Set.fold (\x pairAcc -> Set.fold (\y pairAcc2 ->
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
                ([x, y, d], Set.singleton $ flipper L):pairAcc2
            else
                pairAcc2
            ) pairAcc nodesAcc ) [] nodesAcc

