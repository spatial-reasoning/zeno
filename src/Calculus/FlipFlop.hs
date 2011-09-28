module Calculus.FlipFlop where

-- standard modules
import qualified Data.Char as Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
-- local modules
import Basics

data FlipFlop2 = L2 | R2
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data FlipFlop4 = L4 | R4 | S4 | E4
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data FlipFlop7 = L7 | R7 | S7 | E7 | B7 | I7 | F7
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data FlipFlop5 = L5 | R5 | B5 | I5 | F5
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data FlipFlop3 = R3 | I3 | L3
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

class (TernaryCalculus a) => FlipFlop a where
    fflip :: a -> a
    fflip rel = case showRel rel of
        "s" -> readRel "e"
        "e" -> readRel "s"
        "l" -> readRel "r"
        "r" -> readRel "l"
        "f" -> readRel "b"
        "b" -> readRel "f"
        "i" -> rel

instance FlipFlop FlipFlop2
instance FlipFlop FlipFlop4
instance FlipFlop FlipFlop7
instance FlipFlop FlipFlop5
instance FlipFlop FlipFlop3

instance Calculus FlipFlop2 where
    readRel = read . (++ "2") . (map Char.toUpper)
    showRel = (map Char.toLower) . take 1 . show

instance TernaryCalculus FlipFlop2

instance Calculus FlipFlop4 where
    readRel = read . (++ "4") . (map Char.toUpper)
    showRel = (map Char.toLower) . take 1 . show

instance TernaryCalculus FlipFlop4

instance Calculus FlipFlop7 where
    readRel = read . (++ "7") . (map Char.toUpper)
    showRel = (map Char.toLower) . take 1 . show

instance TernaryCalculus FlipFlop7

instance Calculus FlipFlop5 where
    readRel = read . (++ "5") . (map Char.toUpper)
    showRel = (map Char.toLower) . take 1 . show

instance TernaryCalculus FlipFlop5 where
    tcInvMap = Map.fromList
        [ (B5, Set.singleton F5)
        , (I5, Set.singleton I5)
        , (L5, Set.singleton R5)
        , (R5, Set.singleton L5)
        , (F5, Set.singleton B5) ]

    tcScMap = Map.fromList
        [ (B5, Set.singleton B5)
        , (I5, Set.singleton F5)
        , (L5, Set.singleton R5)
        , (R5, Set.singleton L5)
        , (F5, Set.singleton I5) ]

    tcHomMap = Map.fromList
        [ (B5, Set.singleton I5)
        , (I5, Set.singleton F5)
        , (L5, Set.singleton L5)
        , (R5, Set.singleton R5)
        , (F5, Set.singleton B5) ]

instance Calculus FlipFlop3 where
    readRel = read . (++ "3") . (map Char.toUpper)
    showRel = (map Char.toLower) . take 1 . show

instance TernaryCalculus FlipFlop3

ff7ToFF5 :: FlipFlop7 -> FlipFlop5
ff7ToFF5 = readRel . showRel

ff7sToFF5s :: (Ord a)
           => Network [a] FlipFlop7
           -> Maybe (Network [a] FlipFlop5)
ff7sToFF5s net@Network { nCons = cons }
    | not $ Map.null $ Map.filterWithKey
          (\ [a, b, c] _ -> a == c || b == c)
          newCons  = Nothing
    | otherwise = Just $ net{ nCons = newCons }
    where
        newCons = fst $ Map.foldrWithKey collectOneCon (Map.empty, Map.empty) cons
        collectOneCon nodes@[a, b, c] rel (newConsCol, mapCol)
            | rel == S7  = ( newConsCol
                           , Map.insert c (applyMap mapCol a) mapCol )
            | rel == E7  = ( newConsCol
                           , Map.insert c (applyMap mapCol b) mapCol )
            | otherwise  = ( Map.insert
                                 (map (applyMap mapCol) nodes)
                                 (ff7ToFF5 rel)
                                 newConsCol
                           , mapCol )
        applyMap m node = Map.findWithDefault node node m


ff5ToFF3 :: FlipFlop5 -> FlipFlop3
ff5ToFF3 rel
    | rel == L5 = L3
    | rel == R5 = R3
    | otherwise = I3

ff5sToFF3s :: Network [String] FlipFlop5
           -> Network [String] FlipFlop3
ff5sToFF3s net@Network { nCons = cons } =
--    net { nCons = Set.map ff5ToFF3 $ Set.fold collectOneCon Set.empty $ sortRels $ aClosure cons }
    net { nCons = fst $ Map.foldrWithKey collectOneCon (Map.empty, nodesIn net) cons
        }
    where
        collectOneCon [a, b, c] rel (consAcc, nodesAcc)
            | rel == L5 || rel == R5  =
                ( Map.insert [a, b, c] (ff5ToFF3 rel) consAcc
                , nodesAcc )
--            | rel == B5  = sortRels $ aClosure $ foldl (flip Set.insert) consAcc
            | rel == B5  =
                ( foldl (flip $ uncurry Map.insert) consAcc
                    ( [ ([a, b, d], flipper L3)
                      , ([d, a, c], flipper R3) ]
                      ++
                      if new then
                          inlineNodes
                      else
                          []
                    )

                , newNodesAcc )
--            | rel == I5  = sortRels $ aClosure $ foldl (flip Set.insert) consAcc
            | rel == I5  =
                ( foldl (flip $ uncurry Map.insert) consAcc
                    [ ([a, b, d], flipper L3)
                    , ([d, a, c], flipper L3)
                    , ([d, b, c], flipper R3) ]
                , newNodesAcc
                )
--            | rel == F5 = sortRels $ aClosure $ foldl (flip Set.insert) consAcc
            | rel == F5  =
                ( foldl (flip $ uncurry Map.insert) consAcc
                    [ ([a, b, d], flipper L3)
                    , ([d, b, c], flipper L3) ]
                , newNodesAcc
                )
            where
                newNodesAcc = Set.insert d nodesAcc
                (d, flipper, new)
                    | isJust leftD  = (fst $ fromJust leftD , id   , False)
                    | isJust rightD = (fst $ fromJust rightD, fflip, False)
                    | otherwise     = (newD           , id   , True)
                leftD = Set.minView $ Set.filter
                    (\node -> tcRelOfAtomic net [a, b, node] == Just L5)
                    nodesAcc
                rightD = Set.minView $ Set.filter
                    (\node -> tcRelOfAtomic net [a, b, node] == Just R5)
                    nodesAcc
                -- if we could generalize the generation of newD we wouldn't be
                -- restricted to Strings as the type of the nodes.
                newD = concat $ Set.toList nodesAcc
                inlineNodes = Set.fold (\x pairAcc -> Set.fold (\y pairAcc2 ->
                    if
                         (x /= y)
                      && (    (    tcRelOfAtomic net [a, b, x] == Just B5
                                && (    (    tcRelOfAtomic net [a, b, y] == Just B5
                                          && tcRelOfAtomic net [x, y, a] == Just F5
                                          && tcRelOfAtomic net [x, y, b] == Just F5 )
                                     || y == a
                                     || tcRelOfAtomic net [a, b, y] == Just I5
                                     || y == b
                                     || tcRelOfAtomic net [a, b, y] == Just F5 ))
                           || (    x == a
                                && (    tcRelOfAtomic net [a, b, y] == Just I5
                                     || y == b
                                     || tcRelOfAtomic net [a, b, y] == Just F5 ))
                           || (    tcRelOfAtomic net [a, b, x] == Just I5
                                && (    (    tcRelOfAtomic net [a, b, y] == Just I5
                                          && tcRelOfAtomic net [x, y, a] == Just B5
                                          && tcRelOfAtomic net [x, y, b] == Just F5 )
                                     || y == b
                                     || tcRelOfAtomic net [a, b, y] == Just F5 ))
                           || (    x == b
                                && tcRelOfAtomic net [a, b, y] == Just F5 )
                           || (    tcRelOfAtomic net [a, b, x] == Just F5
                                && tcRelOfAtomic net [a, b, y] == Just F5
                                && tcRelOfAtomic net [x, y, a] == Just B5
                                && tcRelOfAtomic net [x, y, b] == Just B5 ))
                    then
                        insert ([x, y, d], flipper L3) pairAcc2
                    else
                        pairAcc2
                    ) pairAcc nodesAcc ) [] nodesAcc

