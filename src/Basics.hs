module Basics where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Helpful as H

type Entity = String
type Relation = Set.Set String
type Constraint = ([Entity], Relation)

data Naa = Naa  -- NonAssociativeAlgebra
    { baserelations :: Set.Set Relation
    , identity      :: Relation
    , converse      :: Map.Map Relation Relation
    , composition   :: Map.Map (Relation, Relation) Relation
    } deriving (Read, Show, Eq)

data ConstraintNetwork = ConstraintNetwork
    { constraints      :: [Constraint]
    , numberOfEntities :: Maybe Int
    , description      :: Maybe String
    } deriving (Read, Show, Eq)

convert :: Naa -> Relation -> Relation
convert a b = Set.fold Set.union Set.empty $ Set.map
    ((Map.!) (converse a) . Set.singleton) b
---- for the general case (kept for possible later usage)
--    Set.fold Set.union Set.empty $
--    Set.map ((Map.!) (converse a)) $
--    Set.filter (\x -> Set.isSubsetOf x b) (baserelations a)

compose :: Naa -> Relation -> Relation -> Relation
compose a b c = Set.fold Set.union Set.empty $ Set.map
    (\x -> Set.fold Set.union Set.empty $ Set.map
        (\y -> (composition a) Map.! (Set.singleton x, Set.singleton y))
        c)
    b

findIdentity :: Naa -> Maybe Relation
findIdentity a = H.maxFilterSubset
    (\s -> Set.fold (&&) True
        (Set.map
            (\x -> (compose a s x == x) && (compose a x s == x))
            (baserelations a)))
    (Set.fold Set.union Set.empty $ baserelations a)

getNumberOfEntities :: ConstraintNetwork -> Int
getNumberOfEntities net =
    length $ List.nub $ concat [ fst x | x <- constraints net ]

isQuasiAtomic :: ConstraintNetwork -> Bool
isQuasiAtomic net = and [ (Set.size $ snd x) == 1 | x <- constraints net ]

