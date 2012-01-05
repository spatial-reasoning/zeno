module Basics where

-- standard modules
import Control.Monad
import qualified Data.Char as Char
import List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

-- local modules
import qualified Helpful as H

-- Debugging and Timing
import Debug.Trace
--import Data.Time.Clock (diffUTCTime, getCurrentTime)


data Network a b = Network
    { nCons       :: Map.Map a b        -- Constraints
    , nDesc       :: String             -- Description
    , nCalc       :: String             -- Name of calculus
    , nNumOfNodes :: Maybe Int          -- Number of nodes
    } deriving (Eq, Ord, Read, Show)

class (Ord a, Enum a, Bounded a, Read a, Show a) => Calculus a where
    cBaserelations :: Set.Set a
    cBaserelations = Set.fromList [minBound..maxBound]

    readRel       :: String -> a
    showRel       :: a -> String

    readRel = read . (map Char.toUpper)
    showRel = (map Char.toLower) . show


-- This was a try to implement a general way to handle constraint networks.
--
--    conversions :: [( [b] -> [b] , Map.Map a (Set.Set a) )]
--
--    convert :: Map.Map a (Set.Set a) -> Set.Set a -> Set.Set a
--    convert conv = Set.fold Set.union Set.empty . Set.map ((Map.!) conv)
--
--    relOf :: (Ord b) => Map.Map [b] (Set.Set a) -> [b] -> Maybe (Set.Set a)
--    relOf cons nodes = maybe conved Just $ Map.lookup nodes cons where
--        conved = listToMaybe $ Map.foldrWithKey
--            (\k rel acc -> acc ++ foldl
--                (\acc2 (f, c) -> if f k == nodes then
--                                     acc2 ++ [convert c rel]
--                                 else
--                                     acc2
--                ) [] conversions
--            ) [] cons
--
--    insertCon :: (Ord b)
--              => [b]
--              -> a
--              -> Map.Map [b] (Set.Set a)
--              -> Map.Map [b] (Set.Set a)
--    insertCon nodes rel cons
--        | isNothing relInCons  = Map.insert nodes rel cons
--        | otherwise  = 
--      where
--        relInCons = relOf cons nodes


class (Calculus a) => BinaryCalculus a where
    bcIdentity      :: a

    bcConversion    :: Map.Map a (Set.Set a)
    bcComposition   :: Map.Map (a, a) (Set.Set a)

    bcConvert       :: Set.Set a -> Set.Set a
    bcCompose       :: Set.Set a -> Set.Set a -> Set.Set a

    bcConvert = Set.fold Set.union Set.empty . Set.map ((Map.!) bcConversion)

    bcCompose set1 set2 = Set.fold Set.union Set.empty $ Set.map
        (\x -> Set.fold Set.union Set.empty $ Set.map
            (\y -> bcComposition Map.! (x, y))
            set2
        ) set1

    bcRelOf :: (Ord a, Ord b)
            => Map.Map [b] (Set.Set a) -> [b] -> Maybe (Set.Set a)
    bcRelOf cons pair
        | isNothing sortedRel  = Nothing
        | pair == sortedPair  = sortedRel
        | otherwise  = liftM bcConvert sortedRel
      where
        sortedPair = sort pair
        sortedRel  = Map.lookup sortedPair cons

    bcInsert :: (Ord b)
             => [b] -> Set.Set a -> Map.Map [b] (Set.Set a)
             -> Map.Map [b] (Set.Set a)
    bcInsert pair rel cons
        | isNothing relInCons  = Map.insert sortedPair sortedRel cons
        | otherwise  = Map.insert
            sortedPair (Set.intersection sortedRel $ fromJust relInCons) cons
      where
        relInCons = Map.lookup sortedPair cons
        sortedPair = sort pair
        sortedRel
            | sortedPair == pair  = rel
            | otherwise           = bcConvert rel


class (Calculus a) => TernaryCalculus a where
    tcInvMap :: Map.Map a (Set.Set a)
    tcScMap  :: Map.Map a (Set.Set a)
    tcHomMap :: Map.Map a (Set.Set a)

    tcInv  :: Set.Set a -> Set.Set a
    tcSc   :: Set.Set a -> Set.Set a
    tcSci  :: Set.Set a -> Set.Set a
    tcHom  :: Set.Set a -> Set.Set a
    tcHomi :: Set.Set a -> Set.Set a

    tcInv  = Set.fold Set.union Set.empty . Set.map ((Map.!) tcInvMap)
    tcSc   = Set.fold Set.union Set.empty . Set.map ((Map.!) tcScMap)
    tcHom  = Set.fold Set.union Set.empty . Set.map ((Map.!) tcHomMap)
    tcSci  = tcInv . tcSc
    tcHomi = tcInv . tcHom

    -- TODO: Should this be here?
    -- | Extracts the relation between the given nodes from a given network,
    -- even if only given implicitly by means of Invers, ShortCut and Homing.
    tcRelOf :: (Ord a, Ord b)
            => Map.Map [b] (Set.Set a) -> [b] -> Maybe (Set.Set a)
    tcRelOf cons triple
        | isNothing sortedRel  = Nothing
        | triple == sortedTriple  = sortedRel
        | triple == tcTripleInv  sortedTriple  = liftM tcInv  sortedRel
        | triple == tcTripleSc   sortedTriple  = liftM tcSc   sortedRel
        | triple == tcTripleSci  sortedTriple  = liftM tcSci  sortedRel
        | triple == tcTripleHom  sortedTriple  = liftM tcHom  sortedRel
        | triple == tcTripleHomi sortedTriple  = liftM tcHomi sortedRel
      where
        sortedTriple = sort triple
        sortedRel = Map.lookup sortedTriple cons

    -- TODO: Should this be here?
    -- | Extracts the relation between the given nodes from a given network,
    -- even if only given implicitly by means of Invers, ShortCut and Homing.
    tcRelOfAtomic :: (Ord a, Ord b) => Map.Map [b] a -> [b] -> Maybe a
    tcRelOfAtomic cons triple
        | isNothing sortedRel  = Nothing
        | triple == sortedTriple  = sortedRel
        | triple == tcTripleInv  sortedTriple  = unsortRelWith tcInv
        | triple == tcTripleSc   sortedTriple  = unsortRelWith tcSc
        | triple == tcTripleSci  sortedTriple  = unsortRelWith tcSci
        | triple == tcTripleHom  sortedTriple  = unsortRelWith tcHom
        | triple == tcTripleHomi sortedTriple  = unsortRelWith tcHomi
      where
        sortedTriple = sort triple
        sortedRel  = Map.lookup sortedTriple cons
        unsortRelWith f
            | Set.size unsortedRel > 1  = Nothing
            | otherwise  = Just $ Set.findMin unsortedRel
          where
            unsortedRel = f $ Set.singleton $ fromJust sortedRel

    -- TODO: Should this be here?
    tcInsert :: (Ord b)
             => [b] -> Set.Set a -> Map.Map [b] (Set.Set a)
             -> Map.Map [b] (Set.Set a)
    tcInsert triple rel cons
        | isNothing relInCons  = Map.insert sortedTriple sortedRel cons
        | otherwise  = Map.insert
            sortedTriple (Set.intersection sortedRel $ fromJust relInCons) cons
      where
        relInCons = Map.lookup sortedTriple cons
        sortedTriple = sort triple
        sortedRel
            | sortedTriple == triple  = rel
            | sortedTriple == tcTripleInv  triple  = tcInv  rel
            | sortedTriple == tcTripleSc   triple  = tcSc   rel
            | sortedTriple == tcTripleSci  triple  = tcSci  rel
            | sortedTriple == tcTripleHom  triple  = tcHom  rel
            | sortedTriple == tcTripleHomi triple  = tcHomi rel


tcTripleInv  :: [b] -> [b]
tcTripleSc   :: [b] -> [b]
tcTripleSci  :: [b] -> [b]
tcTripleHom  :: [b] -> [b]
tcTripleHomi :: [b] -> [b]
tcTripleInv  [a, b, c] = [b, a, c]
tcTripleSc   [a, b, c] = [a, c, b]
tcTripleSci  [a, b, c] = [c, a, b]
tcTripleHom  [a, b, c] = [b, c, a]
tcTripleHomi [a, b, c] = [c, b, a]

tcTripleInvInv  :: [b] -> [b]
tcTripleScInv   :: [b] -> [b]
tcTripleSciInv  :: [b] -> [b]
tcTripleHomInv  :: [b] -> [b]
tcTripleHomiInv :: [b] -> [b]
tcTripleInvInv  [b, a, c] = [a, b, c]
tcTripleScInv   [a, c, b] = [a, b, c]
tcTripleSciInv  [c, a, b] = [a, b, c]
tcTripleHomInv  [b, c, a] = [a, b, c]
tcTripleHomiInv [c, b, a] = [a, b, c]


{------------------------------------------------------------------------------
 - Empty constants
------------------------------------------------------------------------------}

eNetwork = Network
    { nCons = Map.empty
    , nDesc = "Empty Network"
    , nCalc = ""
    , nNumOfNodes = Nothing
    }


{------------------------------------------------------------------------------
 - Some useful functions
------------------------------------------------------------------------------}

enumerate :: (Ord a, Ord b)
          => Map.Map [a]   b
          -> Map.Map [Int] b
enumerate cons =
    snd $ Map.foldrWithKey collectOneCon (Map.empty, Map.empty) cons
  where
    collectOneCon nodes rel (mapCol, consCol) =
        let
            (newMap, newNodes) = mapAccumL
                (\ m node -> let mappedNode = Map.lookup node m in
                    case mappedNode of
                        Nothing   -> let n = Map.size m in
                                     (Map.insert node n m, n)
                        otherwise -> (m, fromJust mappedNode)
                )
                mapCol
                nodes
        in
        ( newMap
        , Map.insert newNodes rel consCol
        )

enumerate2 :: (Ord a, Ord b)
           => Map.Map [a]   b
           -> (Map.Map [Int] b, Map.Map Int a)
enumerate2 cons = (newCons, enumeration)
  where
    (_, newCons, enumeration) = Map.foldrWithKey
                                    collectOneCon
                                    (Map.empty, Map.empty, Map.empty)
                                    cons
    collectOneCon nodes rel (mapCol, consCol, enumCol) =
        let
            ((newMap, newEnum), newNodes) = mapAccumL
                (\ (m, e) node -> let mappedNode = Map.lookup node m in
                    case mappedNode of
                        Nothing   -> let n = Map.size m in
                            ((Map.insert node n m, Map.insert n node e), n)
                        otherwise -> ((m, e), fromJust mappedNode)
                )
                (mapCol, enumCol)
                nodes
        in
        ( newMap
        , Map.insert newNodes rel consCol
        , newEnum
        )

unenumerate :: (Ord a)
            => Map.Map Int a
            -> Map.Map [Int] b
            -> Map.Map [a] b
unenumerate enumeration cons = Map.mapKeys (map (enumeration Map.!)) cons

unenumerateFromString :: (Ord a)
                      => Map.Map Int a
                      -> Map.Map [String] b
                      -> Map.Map [a] b
unenumerateFromString enumeration cons =
    Map.mapKeys (map ( (enumeration Map.!) . read )) cons


nodesIn :: (Ord a) => Network [a] b -> Set.Set a
nodesIn = Map.foldrWithKey
    (\nodes _ newSet -> foldl (flip Set.insert) newSet nodes )
    Set.empty . nCons

numberOfNodes :: (Ord a) => Network [a] b -> Int
numberOfNodes = Set.size . nodesIn


--findIdentity :: Naa -> Maybe Relation
--findIdentity a = H.maxFilterSubset
--    (\s -> Set.fold (&&) True
--        (Set.map
--            (\x -> (compose a s x == x) && (compose a x s == x))
--            (bcBaserelations a)))
--    (Set.fold Set.union Set.empty $ bcBaserelations a)

isAtomic :: Network [a] (Set.Set b) -> Bool
isAtomic = Map.fold (\x y -> y && ( (== 1) $ Set.size x)) True . nCons

-- This function only keeps atomic relations and generalizes all other
-- relations to the general relation.
makeAtomic :: (Ord a)
           => Network [a] (Set.Set b)
           -> Network [a] b
makeAtomic net@Network { nCons = cons } =
    net { nCons = Map.foldrWithKey
            (\nodes rel consAcc ->
                if Set.size rel == 1 then
                    Map.insert nodes (Set.findMin rel) consAcc
                else
                    consAcc
            ) Map.empty cons
        }

makeNonAtomic :: Network [a] b -> Network [a] (Set.Set b)
makeNonAtomic net@Network { nCons = cons } =
    net { nCons = Map.map Set.singleton cons }




