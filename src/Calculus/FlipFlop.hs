module Calculus.FlipFlop where

-- standard modules
import qualified Data.Char as Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
-- local modules
import Basics
import Interface.Sparq

data FlipFlop = L | R | B | S | I | E | F | D | T
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Calculus FlipFlop where
    readRel = read . catchDouTri . (map Char.toUpper)
        where
            catchDouTri "DOU" = "D"
            catchDouTri "TRI" = "T"
            catchDouTri x = x
    showRel D = "dou"
    showRel T = "tri"
    showRel x = (map Char.toLower) $ show x

instance TernaryCalculus FlipFlop where
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


ffsToFF5s :: (Ord a)
          => Network [a] FlipFlop
          -> Maybe (Network [a] FlipFlop)
ffsToFF5s net@Network { nCons = cons }
    | not $ Map.null $ Map.filterWithKey
          (\ [a, b, c] rel -> a == c || b == c || isNothing rel)
          newCons  = Nothing
    | otherwise = Just $ net{ nCons = Map.map fromJust newCons }
    where
        newCons = fst $ Map.foldrWithKey collectOneCon (Map.empty, Map.empty) cons
        collectOneCon nodes@[a, b, c] rel (newConsCol, mapCol)
            | rel == S  = ( newConsCol
                          , Map.insert c (applyMap mapCol a) mapCol )
            | rel == E  = ( newConsCol
                          , Map.insert c (applyMap mapCol b) mapCol )
            | rel == D  = ( newConsCol
                          , Map.insert b (applyMap mapCol a) mapCol )
            | rel == T  = ( newConsCol
                          , Map.insert c (applyMap mapCol a) $
                            Map.insert b (applyMap mapCol a) mapCol )
            | otherwise  = ( insertCon mappedNodes rel newConsCol 
                           , mapCol )
            where
                mappedNodes = map (applyMap mapCol) nodes
        applyMap m node = Map.findWithDefault node node m

-- Convert Back, Inline and Front to Inline.
bifToI :: FlipFlop -> FlipFlop
bifToI B = I
bifToI F = I
bifToI x = x

-- Returns an algebraically closed network of FlipFlop3 constraints equivalent
-- to the given FlipFlop5 network.
ff5sToFF3s :: Network [String] FlipFlop
           -> Maybe (Network [String] FlipFlop)
ff5sToFF3s nnnet
    | consistent == Just False || isNothing nnet  = Nothing
    | otherwise  = Just $ net
        -- insertCon never returns Nothing since the network is algebraically
        -- closed.
        { nCons = Map.map fromJust $ fst $ Map.foldrWithKey
                                               collectOneCon
                                               (Map.empty, nodesIn net)
                                               (nCons net) }
    where
        -- TODO: How to best handle the problem of conversion from atomic to nonatomic and back?
        (consistent, _, aClosedNnnet) =
            algebraicClosure "ff" $ makeNonAtomic nnnet
        nnet = ffsToFF5s $ makeAtomic $ aClosedNnnet
            { nCons = Map.map (Set.map bifToI) $ nCons aClosedNnnet }
        net@Network { nCons = cons } = fromJust nnet
        collectOneCon [a, b, c] rel (consAcc, nodesAcc)
            | rel == L || rel == R  =
                ( insertCon [a, b, c] rel consAcc
                , nodesAcc )
            | rel == B  =
                ( foldl (flip $ uncurry insertCon) consAcc
                    ( [ ([a, b, d], flipper L)               -- HERE WE HAVE
                      , ([d, a, c], flipper R) ]             -- TO FILTER THE
                      ++                                     -- THE DUPLICATES
                      if new then
                          inlineNodes
                      else
                          []
                    )
                , newNodesAcc )
            | rel == I  =
                ( foldl (flip $ uncurry insertCon) consAcc
                    ( [ ([a, b, d], flipper L)
                      , ([d, a, c], flipper L)
                      , ([d, b, c], flipper R) ]
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
                    ( [ ([a, b, d], flipper L)
                      , ([d, b, c], flipper L) ]
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
                    | otherwise     = (newD           , id   , True)
                leftD = Set.minView $ Set.filter
                    (\node -> tcRelOfAtomic cons [a, b, node] == Just L)
                    nodesAcc
                rightD = Set.minView $ Set.filter
                    (\node -> tcRelOfAtomic cons [a, b, node] == Just R)
                    nodesAcc
                -- if we could generalize the generation of newD we wouldn't be
                -- restricted to Strings as the type of the nodes.
                newD = concat $ Set.toList nodesAcc
                inlineNodes = Set.fold (\x pairAcc -> Set.fold (\y pairAcc2 ->
                    if
                         (x /= y)
                      && (    (    tcRelOfAtomic cons [a, b, x] == Just B
                                && (    (    tcRelOfAtomic cons [a, b, y] == Just B
                                          && tcRelOfAtomic cons [x, y, a] == Just F
                                          && tcRelOfAtomic cons [x, y, b] == Just F )
                                     || y == a
                                     || tcRelOfAtomic cons [a, b, y] == Just I
                                     || y == b
                                     || tcRelOfAtomic cons [a, b, y] == Just F ))
                           || (    x == a
                                && (    tcRelOfAtomic cons [a, b, y] == Just I
                                     || y == b
                                     || tcRelOfAtomic cons [a, b, y] == Just F ))
                           || (    tcRelOfAtomic cons [a, b, x] == Just I
                                && (    (    tcRelOfAtomic cons [a, b, y] == Just I
                                          && tcRelOfAtomic cons [x, y, a] == Just B
                                          && tcRelOfAtomic cons [x, y, b] == Just F )
                                     || y == b
                                     || tcRelOfAtomic cons [a, b, y] == Just F ))
                           || (    x == b
                                && tcRelOfAtomic cons [a, b, y] == Just F )
                           || (    tcRelOfAtomic cons [a, b, x] == Just F
                                && tcRelOfAtomic cons [a, b, y] == Just F
                                && tcRelOfAtomic cons [x, y, a] == Just B
                                && tcRelOfAtomic cons [x, y, b] == Just B ))
                    then
                        insert ([x, y, d], flipper L) pairAcc2
                    else
                        pairAcc2
                    ) pairAcc nodesAcc ) [] nodesAcc

