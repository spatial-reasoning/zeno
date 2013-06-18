module Naa
    ( Relation
    , NonAssociativeAlgebra(..)
    , AlgebraMapping
    , trivialHom
    , findHom
    )
where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Control.Parallel.Strategies
import Debug.Trace

version = "                                        ."
authors = "André Scholz (andre.scholz@uni-bremen.de)"

type Relation = Set.Set String
type AlgebraMapping = Map.Map Relation Relation

data NonAssociativeAlgebra = NonAssociativeAlgebra
    { baserelations :: Set.Set Relation
    , identity      :: Relation
    , converse      :: Map.Map Relation Relation
    , composition   :: Map.Map (Relation, Relation) Relation
    } deriving (Show)

-- begin helper functions -----------------------------------------------------
setCatMaybes a = Set.map Maybe.fromJust . Set.delete Nothing $ a
remove x y = Map.map (Set.delete x) y

-- parallel computing
--mapP = parBuffer 4 rwhnf $ map a b
--mapP = parBuffer 4 rwhnf
mapP = parMap rseq
-- end helper functions -------------------------------------------------------

trivialHom :: NonAssociativeAlgebra
           -> NonAssociativeAlgebra
           -> AlgebraMapping
trivialHom a b = Set.fold
    (\x y -> Map.insert x (biggestPossibleImage x) y)
    Map.empty
    (baserelations a)
    where biggestPossibleImage x
              | Set.isSubsetOf x (identity a)  = identity b
              | otherwise  = Set.fold Set.union Set.empty (baserelations b)

applyMapping :: AlgebraMapping -> Relation -> Relation
applyMapping a b = Set.fold Set.union Set.empty $ Set.map
    (\x ->
--        trace ("applyMapping: " ++ show (a,x)) $
        a Map.! (Set.singleton x))
    b

applyComposition :: Map.Map (Relation, Relation) Relation
                 -> Relation
                 -> Relation
                 -> Relation
applyComposition a b c
    | Set.size b == 1  = Set.fold Set.union Set.empty $
        Set.map (\x ->
--        trace ("applyComposition: " ++ show (b,x)) $
        a Map.! (b, (Set.singleton x)) ) c
    | otherwise  = Set.fold Set.union Set.empty $
        Set.map (\x -> (applyComposition a (Set.singleton x) c)) b

-- find injective atomic homomorphisms only
findHom :: NonAssociativeAlgebra
        -> NonAssociativeAlgebra
        -> AlgebraMapping
        -> Set.Set AlgebraMapping
findHom a b c
    | isAtomic c  = Set.singleton c
    | otherwise   = Set.fold Set.union Set.empty
        (Set.map (findHom a b) (mapOneElement a b c))

isAtomic :: AlgebraMapping -> Bool
isAtomic a = Map.fold (\x y -> (Set.size x == 1) && y) True a

-- this mapping is injective and atomic
mapOneElement :: NonAssociativeAlgebra
              -> NonAssociativeAlgebra
              -> AlgebraMapping
              -> Set.Set AlgebraMapping
--mapOneElement a b c = setCatMaybes $ Set.map    -- sequential
mapOneElement a b c = Set.fromList $ Maybe.catMaybes $ mapP   -- parallel
    (\x ->
        propagate a b $ Map.insert u (Set.singleton x) $ remove x c
--    ) v    -- sequential
    ) (Set.toList v)    -- parallel
    where (u,v) = Map.foldWithKey minByValue (Map.elemAt 0 c) c
          minByValue x y (u,v)
              | (Set.size v) == 1  = (x,y)
              | (Set.size y > 1) && (Set.size y) < (Set.size v)  = (x,y)
              | otherwise  = (u,v)

containsEmptySet :: AlgebraMapping -> Bool
containsEmptySet a = elem Set.empty $ Map.elems a

propagate :: NonAssociativeAlgebra
          -> NonAssociativeAlgebra
          -> AlgebraMapping
          -> Maybe AlgebraMapping
propagate a b c
    | Set.isProperSubsetOf (applyMapping c (identity a)) (identity b) = Nothing
    | containsEmptySet c  = Nothing
    | c ==  oneRoundOfEliminatingImpossibleImages  = Just c
    | otherwise  = propagate a b oneRoundOfEliminatingImpossibleImages
    where oneRoundOfEliminatingImpossibleImages = Map.mapWithKey eris c
          eris x y = Set.filter
              (\u -> Set.notMember False $ Set.map
                  (\v ->
                      let cv = applyMapping c v
                          cCompXV = applyMapping
                              c
                              ( applyComposition (composition a) x v )
                          cCompVX = applyMapping
                              c
                              ( applyComposition (composition a) v x )
                      in Set.member True $ Set.map
                            (\w -> (&&)
                                ( Set.isSubsetOf
                                    (
--                                      trace ("One: " ++ show (u,w)) $
                                      (Map.!)
                                        (composition b)
                                        (Set.singleton u, Set.singleton w) )
                                    cCompXV )
                                ( Set.isSubsetOf
                                    (
--                                      trace ("Two: " ++ show (w,u)) $
                                      (Map.!)
                                        (composition b)
                                        (Set.singleton w, Set.singleton u) )
                                    cCompVX ) )
                            cv )
                  (baserelations a) ) $
              Set.intersection
                  y
                  ( Map.fold Set.intersection
                      ( applyMapping (converse b) $
                          applyMapping c $ applyMapping (converse a) x )
                      ( Map.filterWithKey
                          (\z _ -> Set.isSubsetOf x z)
                          compositions ))
          compositions = Set.fold fnord Map.empty (baserelations a)
          fnord x y = Set.fold
              (\u v -> Map.insertWith Set.intersection
                 (
--                   trace ("fnord: " ++ show (x,u)) $
                   (composition a) Map.! (x,u) )
                 ( applyComposition
                     (composition b)
                     (applyMapping c x)
                     (applyMapping c u) )
                 v
              )
              y (baserelations a)

