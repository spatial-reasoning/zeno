module Basics where

-- standard modules
import Control.Monad
import qualified Data.Char as Char
import qualified Data.Foldable as Fold
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ratio
import qualified Data.Set as Set

-- local modules
import qualified Helpful.Math as H

-- Debugging and Timing
import Debug.Trace
--import Data.Time.Clock (diffUTCTime, getCurrentTime)


data Network a b = Network
    { nCons       :: Map.Map a b        -- Constraints
    , nDesc       :: String             -- Description
    , nCalc       :: String             -- Name of calculus
    , nNumOfNodes :: Maybe Int          -- Number of nodes
    } deriving (Eq, Ord, Read, Show)

-- fixme: split this into "Non Associative Algebra" and "Relation ..."
class (Ord a, Enum a, Bounded a, Read a, Show a) => Calculus a where
    rank :: a -> Int
    calculus :: a -> String

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

    readRel :: String -> a
    showRel :: a -> String

    readRel = read . (map Char.toUpper)
    showRel = (map Char.toLower) . show

    sparqifyRel :: a -> String
    sparqifyRel = showRel

    gqrifyRel :: a -> String
    gqrifyRel = showRel

    identity :: a
    identity = undefined


    -- binary calculi:
    bcConversion    :: Map.Map a (Set.Set a)
    bcConversion    = Map.empty

    bcComposition   :: Map.Map (a, a) (Set.Set a)
    bcComposition   = Map.empty

    bcConvert       :: Set.Set a -> Set.Set a
    bcCompose       :: Set.Set a -> Set.Set a -> Set.Set a

    bcConvert set =
      let
        theRank = rank $ Set.findMin $ Set.insert minBound set
      in
        if theRank == 2 then
            Set.foldr Set.union Set.empty $ Set.map ((Map.!) bcConversion) set
        else
            error $ "bcConvert is not defined for calculi of rank " ++
                    show theRank ++ "!"

    bcCompose set1 set2 =
      let
        theRank = rank $ Set.findMin $ Set.insert minBound set1
      in
        if theRank == 2 then
            Set.foldr Set.union Set.empty $ Set.map
                (\x -> Set.fold Set.union Set.empty $ Set.map
                    (\y -> bcComposition Map.! (x, y))
                    set2
                ) set1
        else
            error $ "bcCompose is not defined for calculi of rank " ++
                    show theRank ++ "!"


    -- ternary calculi:
    tcInvMap :: Map.Map a (Set.Set a)
    tcInvMap = Map.empty

    tcScMap  :: Map.Map a (Set.Set a)
    tcScMap  = Map.empty

    tcHomMap :: Map.Map a (Set.Set a)
    tcHomMap = Map.empty


    tcInv  :: Set.Set a -> Set.Set a
    tcSc   :: Set.Set a -> Set.Set a
    tcSci  :: Set.Set a -> Set.Set a
    tcHom  :: Set.Set a -> Set.Set a
    tcHomi :: Set.Set a -> Set.Set a

    tcInv set =
      let
        theRank = rank $ Set.findMin $ Set.insert minBound set
      in
        if theRank == 3 then
            Set.foldr Set.union Set.empty $ Set.map ((Map.!) tcInvMap) set
        else
            error $ "tcInv is not defined for calculi of rank " ++
                    show theRank ++ "!"
    tcSc set =
      let
        theRank = rank $ Set.findMin $ Set.insert minBound set
      in
        if theRank == 3 then
            Set.foldr Set.union Set.empty $ Set.map ((Map.!) tcScMap) set
        else
            error $ "tcInv is not defined for calculi of rank " ++
                    show theRank ++ "!"
    tcHom set =
      let
        theRank = rank $ Set.findMin $ Set.insert minBound set
      in
        if theRank == 3 then
            Set.foldr Set.union Set.empty $ Set.map ((Map.!) tcHomMap) set
        else
            error $ "tcInv is not defined for calculi of rank " ++
                    show theRank ++ "!"
    tcSci  = tcInv . tcSc
    tcHomi = tcInv . tcHom


-- fixme: should we return Nothing in case of an empty relation?
insertCon :: (Calculus a, Ord b)
          => [b] -> Set.Set a -> Map.Map [b] (Set.Set a)
          -> Map.Map [b] (Set.Set a)
insertCon nodes rel cons
    | isNothing relInCons  = Map.insert sortedNodes sortedRel cons
    | otherwise  = Map.insert
        sortedNodes (Set.intersection sortedRel $ fromJust relInCons) cons
  where
    relInCons = Map.lookup sortedNodes cons
    sortedNodes = sort nodes
    sortedRel = sortRel (length nodes) rel
    sortRel 2 | sortedNodes == nodes      = id
              | otherwise                 = bcConvert
    sortRel 3 | sortedNodes == nodes              = id
              | sortedNodes == tcNodesInv  nodes  = tcInv
              | sortedNodes == tcNodesSc   nodes  = tcSc
              | sortedNodes == tcNodesSci  nodes  = tcSci
              | sortedNodes == tcNodesHom  nodes  = tcHom
              | sortedNodes == tcNodesHomi nodes  = tcHomi

insertConAtomic :: (Calculus a, Ord b)
                => [b] -> a -> Map.Map [b] a
                -> Maybe (Map.Map [b] a)
insertConAtomic nodes rel cons
    | Set.null sortedRel'      = Nothing
    | Set.size sortedRel' > 1  = error $
        "insertConAtomic: " ++ show sortedRel' ++ " is not atomic!"
    | isNothing relInCons  = Just $ Map.insert sortedNodes sortedRel cons
    | fromJust relInCons /= sortedRel  = Nothing
    | otherwise  = Just cons
  where
    relInCons = Map.lookup sortedNodes cons
    sortedNodes = sort nodes
    sortedRel = Set.findMin sortedRel'
    sortedRel' = sortRel (length nodes) (Set.singleton rel)
    sortRel 2 | sortedNodes == nodes      = id
              | otherwise                 = bcConvert
    sortRel 3 | sortedNodes == nodes              = id
              | sortedNodes == tcNodesInv  nodes  = tcInv
              | sortedNodes == tcNodesSc   nodes  = tcSc
              | sortedNodes == tcNodesSci  nodes  = tcSci
              | sortedNodes == tcNodesHom  nodes  = tcHom
              | sortedNodes == tcNodesHomi nodes  = tcHomi

consFromList :: (Ord a, Calculus b)
             => [([a], Set.Set b)]
             -> Map.Map [a] (Set.Set b)
consFromList = foldr
    (\ (nodes, rel) acc ->
        insertCon nodes rel acc
    ) Map.empty

consFromListAtomic :: (Ord a, Calculus b)
                   => [([a], b)]
                   -> Maybe (Map.Map [a] b)
consFromListAtomic = Fold.foldrM
    (\ (nodes, rel) acc ->
        insertConAtomic nodes rel acc
    ) Map.empty

relOf :: (Ord a, Calculus b)
      => Map.Map [a] (Set.Set b) -> [a]
      -> Maybe (Set.Set b)
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

relOfAtomic :: (Ord a, Calculus b)
            => Map.Map [a] b -> [a]
            -> Maybe b
relOfAtomic cons nodes = do
    sortedRel <- Map.lookup sortedNodes cons
    let unsortRel 2 | nodes == sortedNodes  = id
                    | otherwise             = bcConvert
    let unsortRel 3 | nodes == sortedNodes              = id
                    | nodes == tcNodesInv  sortedNodes  = tcInv
                    | nodes == tcNodesSc   sortedNodes  = tcSc
                    | nodes == tcNodesSci  sortedNodes  = tcSci
                    | nodes == tcNodesHom  sortedNodes  = tcHom
                    | nodes == tcNodesHomi sortedNodes  = tcHomi
    let unsortedRel = unsortRel (length nodes) (Set.singleton sortedRel)
    -- fixme: what about a possiple empty set? Is Nothing the appropriate
    -- answer in this case?
    if Set.size unsortedRel > 1 then
        Nothing
    else
        Just $ Set.findMin unsortedRel
  where
    sortedNodes = sort nodes


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

isAtomic :: Network [a] (Set.Set b) -> Bool
isAtomic = Map.fold (\x y -> y && ( (== 1) $ Set.size x)) True . nCons

-- This function only keeps atomic relations and generalizes all other
-- relations to the universal relation.
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


