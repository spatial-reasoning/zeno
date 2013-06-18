{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Basics where

-- standard modules
import Data.Char
import qualified Data.Foldable as Fold
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ratio
import qualified Data.Set as Set

-- local modules
import qualified Helpful.Math as H

data Network a b = Network
    { nCons       :: Map.Map a b        -- Constraints
    , nDesc       :: String             -- Description
    , nCalc       :: String             -- Name of calculus
    , nNumOfNodes :: Maybe Int          -- Number of nodes
    } deriving (Eq, Ord, Read, Show)

newtype ARel a = ARel { aRel :: a }
                      deriving (Eq, Ord, Read, Show)

newtype GRel a = GRel { gRel :: Set.Set a }
                      deriving (Eq, Ord, Read, Show)

-- fixme: split this into "Non Associative Algebra" and "Relation ..."
class (Ord a, Enum a, Bounded a, Read a, Show a) => Calculus a where
    rank :: a -> Int
    rankSet :: Set.Set a -> Int
    rankSet set = rank $ (undefined :: (Set.Set a) -> a) set

    cName      :: a -> String
    cNameGqr   :: a -> String
    cNameSparq :: a -> String
    cNameGqr   = cName
    cNameSparq = cName

    cBaserelationsList :: [a]
    cBaserelationsList =  [minBound..maxBound]

    cBaserelations :: Set.Set a
    cBaserelations = Set.fromList [minBound..maxBound]

    cBaserelationsArealList :: [a]
    cBaserelationsArealList = cBaserelationsList

    cBaserelationsAreal :: Set.Set a
    cBaserelationsAreal = Set.fromList cBaserelationsArealList

    cBaserelationsNonArealList :: [a]
    cBaserelationsNonArealList = cBaserelationsList \\ cBaserelationsArealList

    cBaserelationsNonAreal :: Set.Set a
    cBaserelationsNonAreal = Set.fromList cBaserelationsNonArealList

    cBaserelationsSameList :: [a]
    cBaserelationsSameList = cBaserelationsList \\ cBaserelationsNonSameList

    cBaserelationsSame :: Set.Set a
    cBaserelationsSame = Set.fromList cBaserelationsSameList

    cBaserelationsNonSameList :: [a]
    cBaserelationsNonSameList = cBaserelationsList

    cBaserelationsNonSame :: Set.Set a
    cBaserelationsNonSame = Set.fromList cBaserelationsNonSameList

    cReadRel :: String -> a
    cShowRel :: a -> String

    cReadRel = read . (map toUpper)
    cShowRel = (map toLower) . show

    cSparqifyRel :: a -> String
    cSparqifyRel = cShowRel

    cGqrifyRel :: a -> String
    cGqrifyRel = cShowRel

    identity :: a
    identity = undefined


    -- binary calculi:
    bcConversion    :: Map.Map a (Set.Set a)
    bcConversion    = Map.empty

    bcComposition   :: Map.Map (a, a) (Set.Set a)
    bcComposition   = Map.empty

    bcConvert       :: GRel a -> GRel a
    bcCompose       :: GRel a -> GRel a
                    -> GRel a

    bcConvert (GRel set) =
        if rankSet set == 2 then
            GRel $ Set.foldr Set.union Set.empty $ Set.map
                ((Map.!) bcConversion) set
        else
            error $ "bcConvert is not defined for calculi of rank " ++
                    show (rankSet set) ++ "!"

    bcCompose (GRel set1) (GRel set2) =
        if rankSet set1 == 2 then
            GRel $ Set.foldr Set.union Set.empty $ Set.map
                (\x -> Set.fold Set.union Set.empty $ Set.map
                    (\y -> bcComposition Map.! (x, y))
                    set2
                ) set1
        else
            error $ "bcCompose is not defined for calculi of rank " ++
                    show (rankSet set1) ++ "!"


    -- ternary calculi:
    tcInvMap :: Map.Map a (Set.Set a)
    tcInvMap = Map.empty

    tcScMap  :: Map.Map a (Set.Set a)
    tcScMap  = Map.empty

    tcHomMap :: Map.Map a (Set.Set a)
    tcHomMap = Map.empty


    tcInv  :: GRel a -> GRel a
    tcSc   :: GRel a -> GRel a
    tcSci  :: GRel a -> GRel a
    tcHom  :: GRel a -> GRel a
    tcHomi :: GRel a -> GRel a

    tcInv (GRel set) =
        if rankSet set == 3 then
            GRel $ Set.foldr Set.union Set.empty $ Set.map
                ((Map.!) tcInvMap) set
        else
            error $ "tcInv is not defined for calculi of rank " ++
                    show (rankSet set) ++ "!"
    tcSc (GRel set) =
        if rankSet set == 3 then
            GRel $ Set.foldr Set.union Set.empty $ Set.map
                ((Map.!) tcScMap) set
        else
            error $ "tcInv is not defined for calculi of rank " ++
                    show (rankSet set) ++ "!"
    tcHom (GRel set) =
        if rankSet set == 3 then
            GRel $ Set.foldr Set.union Set.empty $ Set.map
                ((Map.!) tcHomMap) set
        else
            error $ "tcInv is not defined for calculi of rank " ++
                    show (rankSet set) ++ "!"
    tcSci (GRel set) =
        if rankSet set == 3 then
            GRel $
                Set.foldr Set.union Set.empty $ Set.map ((Map.!) tcInvMap) $
                Set.foldr Set.union Set.empty $ Set.map ((Map.!) tcScMap) set
        else
            error $ "tcSci is not defined for calculi of rank " ++
                    show (rankSet set) ++ "!"
    tcHomi (GRel set) =
        if rankSet set == 3 then
            GRel $
                Set.foldr Set.union Set.empty $ Set.map ((Map.!) tcInvMap) $
                Set.foldr Set.union Set.empty $ Set.map ((Map.!) tcHomMap) set
        else
            error $ "tcHomi is not defined for calculi of rank " ++
                    show (rankSet set) ++ "!"

tcNodesInv  :: [b] -> [b]
tcNodesSc   :: [b] -> [b]
tcNodesSci  :: [b] -> [b]
tcNodesHom  :: [b] -> [b]
tcNodesHomi :: [b] -> [b]
tcNodesInv  [a, b, c] = [b, a, c]
tcNodesSc   [a, b, c] = [a, c, b]
tcNodesSci  [a, b, c] = [c, a, b]
tcNodesHom  [a, b, c] = [b, c, a]
tcNodesHomi [a, b, c] = [c, b, a]

tcNodesInvInv  :: [b] -> [b]
tcNodesScInv   :: [b] -> [b]
tcNodesSciInv  :: [b] -> [b]
tcNodesHomInv  :: [b] -> [b]
tcNodesHomiInv :: [b] -> [b]
tcNodesInvInv  [b, a, c] = [a, b, c]
tcNodesScInv   [a, c, b] = [a, b, c]
tcNodesSciInv  [c, a, b] = [a, b, c]
tcNodesHomInv  [b, c, a] = [a, b, c]
tcNodesHomiInv [c, b, a] = [a, b, c]


class Relation a b | a -> b where
    readRel :: String -> a
    showRel :: a -> String

    insertCon :: (Ord c) => [c] -> a -> Map.Map [c] a -> Maybe (Map.Map [c] a)
    consFromList :: (Ord c) => [([c], a)] -> Maybe (Map.Map [c] a)
    relOf :: (Ord c) => Map.Map [c] a -> [c] -> Maybe a
    
    toARel :: a -> Maybe (Maybe (ARel b))
    toGRel :: a -> GRel b

instance (Calculus a) => Relation (ARel a) a where
    readRel str = ARel $ cReadRel str
    showRel (ARel rel) = cShowRel rel

    insertCon nodes rel cons = case relInCons of
        Nothing  -> case Set.size sortedRel of
            1 -> Just $ Map.insert sortedNodes
                                   (ARel $ Set.findMin sortedRel)
                                   cons
            0 -> Nothing
            _ -> Just cons
        Just (ARel relInCons')  ->
            if Set.member relInCons' sortedRel then
                Just cons
            else
                Nothing
      where
        relInCons = Map.lookup sortedNodes cons
        sortedNodes = sort nodes
        GRel sortedRel = sortRel (length nodes)
                                            (toGRel rel)
        sortRel 2 | sortedNodes == nodes      = id
                  | otherwise                 = bcConvert
        sortRel 3 | sortedNodes == nodes              = id
                  | sortedNodes == tcNodesInv  nodes  = tcInv
                  | sortedNodes == tcNodesSc   nodes  = tcSc
                  | sortedNodes == tcNodesSci  nodes  = tcSci
                  | sortedNodes == tcNodesHom  nodes  = tcHom
                  | sortedNodes == tcNodesHomi nodes  = tcHomi

    consFromList = Fold.foldrM (\ (nodes, rel) acc -> insertCon nodes rel acc
                               ) Map.empty

    relOf cons nodes = do
        let sortedNodes = sort nodes
        sortedRel <- Map.lookup sortedNodes cons
        let unsortRel 2 | nodes == sortedNodes  = id
                        | otherwise             = bcConvert
        let unsortRel 3 | nodes == sortedNodes              = id
                        | nodes == tcNodesInv  sortedNodes  = tcInv
                        | nodes == tcNodesSc   sortedNodes  = tcSc
                        | nodes == tcNodesSci  sortedNodes  = tcSci
                        | nodes == tcNodesHom  sortedNodes  = tcHom
                        | nodes == tcNodesHomi sortedNodes  = tcHomi
        let unsortedRel =
                unsortRel (length nodes) (toGRel sortedRel)
        let unsortedRelAtomic = toARel unsortedRel
        case unsortedRelAtomic of
            Just (Just x)  -> Just x
            Just Nothing   -> Nothing
            Nothing        -> error $ "relOf found an empty relation in an " ++
                "atomic network. We should fix something about how we " ++
                "handle permutations of relations!"

    toARel rel = Just (Just rel)
    toGRel = GRel . Set.singleton . aRel


instance (Calculus a) => Relation (GRel a) a where
    readRel str = GRel $ Set.fromList $
        map cReadRel $ words $ str
    showRel (GRel rels) = " " ++ Set.foldl
        (\acc rel -> cShowRel rel ++ " " ++ acc
        ) "" rels

    insertCon nodes rel cons
        | isNothing relInCons  = Just $
            Map.insert sortedNodes (GRel sortedRel) cons
        | Set.null newRel  = Nothing
        | otherwise  = Just $
            Map.insert sortedNodes (GRel newRel) cons
      where
        newRel = Set.intersection sortedRel $ gRel $ fromJust relInCons
        relInCons = Map.lookup sortedNodes cons
        sortedNodes = sort nodes
        GRel sortedRel = sortRel (length nodes) rel
        sortRel 2 | sortedNodes == nodes      = id
                  | otherwise                 = bcConvert
        sortRel 3 | sortedNodes == nodes              = id
                  | sortedNodes == tcNodesInv  nodes  = tcInv
                  | sortedNodes == tcNodesSc   nodes  = tcSc
                  | sortedNodes == tcNodesSci  nodes  = tcSci
                  | sortedNodes == tcNodesHom  nodes  = tcHom
                  | sortedNodes == tcNodesHomi nodes  = tcHomi

    consFromList = Fold.foldrM (\ (nodes, rel) acc -> insertCon nodes rel acc
                               ) Map.empty

    relOf cons nodes = do
        sortedRel <- Map.lookup sortedNodes cons
        let unsortRel 2 | nodes == sortedNodes  = id
                        | otherwise             = bcConvert
        let unsortRel 3 | nodes == sortedNodes              = id
                        | nodes == tcNodesInv  sortedNodes  = tcInv
                        | nodes == tcNodesSc   sortedNodes  = tcSc
                        | nodes == tcNodesSci  sortedNodes  = tcSci
                        | nodes == tcNodesHom  sortedNodes  = tcHom
                        | nodes == tcNodesHomi sortedNodes  = tcHomi
        return $ unsortRel (length nodes) sortedRel
      where
        sortedNodes = sort nodes

    toARel (GRel rel) = case Set.size rel of
        1  -> Just $ Just $ ARel $ Set.findMin rel
        0  -> Nothing
        _  -> Just Nothing

    toGRel = id



isAtomic :: Network [a] (GRel b) -> Bool
isAtomic = Map.fold (\x y -> y && ( (== 1) $ Set.size $ gRel x)) True . nCons

-- This function only keeps atomic relations and generalizes all other
-- relations to the universal relation.
-- fixme: handle empty relations.
makeAtomic :: (Ord a, Relation (b c) c)
           => Network [a] (b c)
           -> Network [a] (ARel c)
makeAtomic net@Network { nCons = cons } =
    net { nCons = Map.foldrWithKey
            (\nodes rel consAcc -> case toARel rel of
                Just (Just rel')  -> Map.insert nodes rel' consAcc
                Just Nothing      -> consAcc
                Nothing           -> error $
                    "\"makeAtomic\" got an empty relation: This casestill " ++
                    "needs to be implemented."
            ) Map.empty cons
        }

makeNonAtomic :: (Relation (b c) c)
              => Network [a] (b c)
              -> Network [a] (GRel c)
makeNonAtomic net@Network { nCons = cons } =
    net { nCons = Map.map toGRel cons }

{------------------------------------------------------------------------------
 - Empty constants
------------------------------------------------------------------------------}

eNetwork = Network
    { nCons = Map.empty
    , nDesc = "NoDescription"
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
    snd $ Map.foldlWithKey collectOneCon (Map.empty, Map.empty) cons
  where
    collectOneCon (mapCol, consCol) nodes rel =
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

enumerateAndEnumeration :: (Ord a, Ord b)
                        => Map.Map [a]   b
                        -> (Map.Map [Int] b, Map.Map Int a)
enumerateAndEnumeration cons = (numericCons, enumeration)
  where
    (_, numericCons, enumeration) = Map.foldlWithKey
                                        collectOneCon
                                        (Map.empty, Map.empty, Map.empty)
                                        cons
    collectOneCon (mapCol, consCol, enumCol) nodes rel =
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


nodesIn :: (Ord a) => Map.Map [a] b -> Set.Set a
nodesIn = Map.foldrWithKey
    (\nodes _ acc -> foldr Set.insert acc nodes ) Set.empty

numberOfNodes :: (Ord a) => Map.Map [a] b -> Int
numberOfNodes = Set.size . nodesIn

-- number of related tuples divided by number of possible tuples.
density :: (Ord a) => Network [a] b -> Ratio Int
density net@Network{nCons = cons}
--    | Map.null cons = error "An empty network has no density."
    | Map.null cons = 0  --fixme: This should be something else.
    | otherwise  = Map.size cons % H.choose size rank
  where
    size = numberOfNodes $ nCons net
    rank = length $ fst $ fst $ fromJust minElem
    minElem = Map.minViewWithKey cons

--findIdentity :: Naa -> Maybe Relation
--findIdentity a = H.maxFilterSubset
--    (\s -> Set.fold (&&) True
--        (Set.map
--            (\x -> (compose a s x == x) && (compose a x s == x))
--            (bcBaserelations a)))
--    (Set.fold Set.union Set.empty $ bcBaserelations a)

