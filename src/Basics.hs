module Basics where

-- standard modules
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

-- local modules
import qualified Helpful as H


data Network a b = Network
    { nCons       :: Map.Map a b        -- Constraints
    , nDesc       :: String             -- Description
    , nCalc       :: String             -- Name of calculus
    , nNumOfNodes :: Maybe Int          -- Number of nodes
    } deriving (Eq, Ord, Read, Show)

class (Ord a, Enum a, Bounded a, Read a, Show a) => Calculus a where
    readRel       :: String -> a
    showRel       :: a -> String

    readRel = read . (map Char.toUpper)
    showRel = (map Char.toLower) . show

class (Calculus a) => BinaryCalculus a where
    bcBaserelations :: Set.Set a
    bcIdentity      :: a

    bcConversion    :: Map.Map a (Set.Set a)
    bcComposition   :: Map.Map (a, a) (Set.Set a)

    bcConvert       :: Set.Set a -> Set.Set a
    bcCompose       :: Set.Set a -> Set.Set a -> Set.Set a

    bcBaserelations = Set.fromList [minBound..maxBound]

    bcConvert = Set.fold Set.union Set.empty . Set.map ((Map.!) bcConversion)

    bcCompose set1 set2 = Set.fold Set.union Set.empty $ Set.map
        (\x -> Set.fold Set.union Set.empty $ Set.map
            (\y -> bcComposition Map.! (x, y))
            set2
        ) set1

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
    -- extracts the relation between the given nodes from a given network, even
    -- if only given implicitly by means of Invers, ShortCut and Homing.
    tcRelOfAtomic :: (Ord a, Ord b) => Map.Map [b] a -> [b] -> Maybe a
    tcRelOfAtomic cons nodes
        | isJust inv  = Just $ Set.findMin $ tcInv  $ Set.singleton $ fromJust inv
        | isJust sc   = Just $ Set.findMin $ tcSc   $ Set.singleton $ fromJust sc
        | isJust sci  = Just $ Set.findMin $ tcSci  $ Set.singleton $ fromJust sci
        | isJust hom  = Just $ Set.findMin $ tcHom  $ Set.singleton $ fromJust hom
        | isJust homi = Just $ Set.findMin $ tcHomi $ Set.singleton $ fromJust homi
        | otherwise   = Nothing
        where
            inv  = Map.lookup (tcNodesInv  nodes) cons
            sc   = Map.lookup (tcNodesSc   nodes) cons
            sci  = Map.lookup (tcNodesSci  nodes) cons
            hom  = Map.lookup (tcNodesHom  nodes) cons
            homi = Map.lookup (tcNodesHomi nodes) cons

    -- TODO: Should this be here?
    insertCon :: (Ord b)
              => [b] -> a -> Map.Map [b] (Maybe a) -> Map.Map [b] (Maybe a)
    insertCon nodes rel cons
        | isNothing relInCons    = Map.insert nodes (Just rel) cons
        | relInCons == Just rel  = cons
        | otherwise              = Map.insert nodes Nothing cons
        where
            relInCons = tcRelOfAtomic (Map.map fromJust cons) nodes


tcNodesInv  :: [b] -> [b]
tcNodesSc   :: [b] -> [b]
tcNodesSci  :: [b] -> [b]
tcNodesHom  :: [b] -> [b]
tcNodesHomi :: [b] -> [b]
tcNodesInv  [b, a, c] = [a, b, c]
tcNodesSc   [a, c, b] = [a, b, c]
tcNodesSci  [c, a, b] = [a, b, c]
tcNodesHom  [b, c, a] = [a, b, c]
tcNodesHomi [c, b, a] = [a, b, c]



{------------------------------------------------------------------------------
 - Empty constants
------------------------------------------------------------------------------}

eNetwork = Network
    { nCons = Map.empty
    , nDesc = ""
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
                (newMap, newNodes) = List.mapAccumL
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
                ((newMap, newEnum), newNodes) = List.mapAccumL
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




