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
    acc <- Key.foldrWithKeyM
                   checkOneCon
                   (Map.empty, cons, nodesIn $ nCons net)
                   cons
    let firstOfTriple (x, _, _) = x
    return $ net{ nCons = firstOfTriple acc}
  where
    checkOneCon nodes@[a, b, c] rel acc@(consAcc, allConsAcc, nodesAcc)
        -- Ensure the existence of a witness for the unsame nodes.
        | rel == S  = ensureSamenessOf a c $ ensureUnsamenessOf a b acc
        | rel == E  = ensureSamenessOf b c $ ensureUnsamenessOf a b acc
        | rel == D  = ensureSamenessOf a b $ ensureUnsamenessOf b c acc
        | rel == T  = ensureSamenessOf a b acc >>= ensureSamenessOf a c >>= ensureSamenessOf b c
        | otherwise = Just ( fromJust $ insertConAtomic nodes rel consAcc
                           , allConsAcc
                           , nodesAcc )
    newMaxNode nodes = (Set.findMax nodes) ++ "eris"
    ensureUnsamenessOf x y acc@(consAccInter, allConsAccInter, nodesAccInter) =
        if
            Map.null $ Map.filterWithKey
                (isWitnessForUnsamenessOf x y)
                consAccInter
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
    ensureSamenessOf x y acc@(consAccInter, allConsAccInter, nodesAccInter)
        | Map.size cIx == 1 || Map.size cIy == 1  = Just acc
            -- x or y is only related this one time: do nothing.
        | not $ null $ disjointPairsOfPIR x  =
            -- take the first, check whether their relations to y exist and
            -- comply:
            --     non-existent -> add them,
            --     don't comply -> return Nothing
            --     comply       -> do nothing,
            useDisjointPairOfPairs x y
        | not $ null $ disjointPairsOfPIR y  =
            -- same as above but with y exchanged by x.
            useDisjointPairOfPairs y x
        | not $ null pIRxy  =
            -- take the first one, look for a non-inline node,
            -- add a new node and use the later two.
            oneInlinePairIn pIRxy x y
        | not $ null pIRx  = -- same as above
            oneInlinePairIn pIRx x y
        | not $ null pIRy  = -- same as above
            oneInlinePairIn pIRy y x
        | not $ null pNIRx =
            -- take the first two nodes node1 and node2,
            -- introduce two new ones newNode1 and newNode2
            -- and map [node1, newNode1, x/y] and [node2, newNode2, x/y] to F.
            oneNonInlinePairIn pNIRx y
        | not $ null pNIRy =  -- same as above.
            oneNonInlinePairIn pNIRy x
        | otherwise  = Just $
            -- introduce four new nodes, since we cannot know whether any
            -- nodes are collinear or same or not.
          let
            newNode0 = newMaxNode nodesAccInter
            newNode1 = newNode0 ++ "mygoddess"
            newNode2 = newNode0 ++ "standbyme"
            newNode3 = newNode0 ++ "intimesofboredom"
          in
            ( fromJust $
              insertConAtomic [newNode1, newNode3, y] F consAccInter >>=
              insertConAtomic [newNode1, newNode3, x] F >>=
              insertConAtomic [newNode0, newNode2, y] F >>=
              insertConAtomic [newNode0, newNode2, x] F >>=
              insertConAtomic [newNode0, newNode1, x] L
            , fromJust $
              insertConAtomic [newNode1, newNode3, y] F allConsAccInter >>=
              insertConAtomic [newNode1, newNode3, x] F >>=
              insertConAtomic [newNode0, newNode2, y] F >>=
              insertConAtomic [newNode0, newNode2, x] F >>=
              insertConAtomic [newNode0, newNode1, x] L
            , foldr Set.insert nodesAccInter
                  [newNode0, newNode1, newNode2, newNode3] )
      where
        useDisjointPairOfPairs v w =
          let
            [pair1, pair2] = head $ disjointPairsOfPIR v
            relPair1v = fromJust $ relOfAtomic allConsAccInter (pair1 ++ [v])
            relPair2v = fromJust $ relOfAtomic allConsAccInter (pair2 ++ [v])
          in
            ite (elem w (pair1 ++ pair2)) Nothing $ do
                newConsAccInter <- 
                    insertConAtomic (pair1 ++ [w]) relPair1v consAccInter >>=
                    insertConAtomic (pair2 ++ [w]) relPair2v
                newAllConsAccInter <- 
                    insertConAtomic (pair1 ++ [w]) relPair1v allConsAccInter >>=
                    insertConAtomic (pair2 ++ [w]) relPair2v
                return $
                    ( newConsAccInter
                    , newAllConsAccInter
                    , nodesAccInter )
        oneInlinePairIn pairs v w =
          let
            pair = head pairs -- improve: we should look for a pair for which
                              -- there is a nonInlineNode.
            newNode = newMaxNode nodesAccInter
            newNode2 = newNode ++ "mygoddess"
            nonInlineNode' = filter
                ( \node -> (node /= w) && elem
                      (relOfAtomic allConsAccInter $ pair ++ [node])
                      [Just L, Just R]
                ) (Set.toAscList nodesAccInter)
            nonInlineNode = head nonInlineNode'
            addConForW = insertConAtomic
                (pair ++ [w])
                (fromJust $ relOfAtomic allConsAccInter $ pair ++ [v])
          in
            ite (elem w pair) Nothing $ do
            newAll <- addConForW allConsAccInter
            return $
                if null nonInlineNode' then
                    ( fromJust $
                      addConForW consAccInter >>=
                      insertConAtomic (pair ++ [newNode]) L >>=
                      insertConAtomic [newNode, newNode2, w] F >>=
                      insertConAtomic [newNode, newNode2, v] F
                    , fromJust $
                      insertConAtomic (pair ++ [newNode]) L newAll >>=
                      insertConAtomic [newNode, newNode2, w] F >>=
                      insertConAtomic [newNode, newNode2, v] F
                    , Set.insert newNode2 $
                      Set.insert newNode nodesAccInter )
                else
                    ( fromJust $
                      addConForW consAccInter >>=
                      insertConAtomic [newNode, nonInlineNode, w] F >>=
                      insertConAtomic [newNode, nonInlineNode, v] F
                    , fromJust $
                      insertConAtomic [newNode, nonInlineNode, w] F newAll >>=
                      insertConAtomic [newNode, nonInlineNode, v] F
                    , Set.insert newNode nodesAccInter )
        oneNonInlinePairIn pairs v =
          let
            pair@[node1, node2] = head pairs
            newNode1 = newMaxNode nodesAccInter
            newNode2 = newNode1 ++ "mygoddess"
          in
            ite (elem v pair) Nothing $ Just
            ( fromJust $
              insertConAtomic [node2, newNode2, y] F consAccInter >>=
              insertConAtomic [node1, newNode1, y] F >>=
              insertConAtomic [node2, newNode2, x] F >>=
              insertConAtomic [node1, newNode1, x] F
            , fromJust $
              insertConAtomic [node2, newNode2, y] F allConsAccInter >>=
              insertConAtomic [node1, newNode1, y] F >>=
              insertConAtomic [node2, newNode2, x] F >>=
              insertConAtomic [node1, newNode1, x] F
            , Set.insert newNode2 $ Set.insert newNode1 nodesAccInter )
        disjointPairsOfPIR z = filter
            (\ [pair1, pair2] -> null $ intersect pair1 pair2
            ) $ kCombinations 2 $ fst $ pairsInlineAndNotInlineRelatedTo z
        pIRxy = intersect pIRx pIRy
        (pIRx, pNIRx) = pairsInlineAndNotInlineRelatedTo x
        (pIRy, pNIRy) = pairsInlineAndNotInlineRelatedTo y
        pairsInlineAndNotInlineRelatedTo z = Map.foldrWithKey
            (\ k v acc@(inlineAcc, notInlineAcc) ->
                if elem v [B, I, F] then
                    ((delete z k):inlineAcc, notInlineAcc)
                else if elem v [L, R] then
                    (inlineAcc, (delete z k):notInlineAcc)
                else
                    acc
            ) ([], []) (consIncluding z)
        cIx = consIncluding x
        cIy = consIncluding y
        consIncluding z = Map.filterWithKey (\ k v -> elem z k) allConsAccInter


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

