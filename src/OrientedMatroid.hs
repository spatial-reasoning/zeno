module OrientedMatroid where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Helpful as H

-- type Assignment = Map.Map [Int] Int
data Chirotope = Chirotope (Map.Map [Int] Int)

isChirotope :: Map.Map [Int] Int -> Bool
isChirotope m
    | Set.isSubsetOf elems (Set.fromList [(-1), 0, 1])
        && elems /= Set.singleton 0
        && and (map ((== rank) . length) keys)
        && domain == [0..(length domain) - 1]
--        && notElem Nothing ( map (flip Map.lookup m) permuts )
        && ( and $
               map ( \(x:xs) -> foldl
                       (\z y ->
                           z && ((m Map.! (fst y)) ==
                                                   (snd y) * (m Map.! (fst x)))
                       ) True xs
                   ) $ filter
                       ((flip Map.member m) . fst . head)
                       permutsAndParities
           )
        && and grassmannPluecker
      = True
    | otherwise = False
    where
        keys = Map.keys m
        elems = Set.fromList $ Map.elems m
        rank = length $ head keys
        domain = List.sort $ List.nub $ concat keys
        permutsAndParities =
            map (H.kPermutationsWithParity rank) $ H.kCombinations rank domain
--        permutsAndParities = H.kPermutationsWithParity rank domain
--        permuts = map fst permutsAndParities
        grassmannPluecker =
            [ (\x -> (elem (-1) x && elem 1 x) || x == [0])
                [ (Map.!) m (x ++ [a,b]) * (Map.!) m (x ++ [c,d])
                , (Map.!) m (x ++ [a,c]) * (Map.!) m (x ++ [b,d]) * (-1)
                , (Map.!) m (x ++ [a,d]) * (Map.!) m (x ++ [b,c])
                ]
            | xy <- H.kCombinations (rank + 2) domain
            , let (x,[a,b,c,d]) = splitAt (rank - 2) xy
            , Map.member (x ++ [a,b]) m -- filter the ones in the map.
            , Map.member (x ++ [c,d]) m
            , Map.member (x ++ [a,c]) m
            , Map.member (x ++ [b,d]) m
            , Map.member (x ++ [a,d]) m
            , Map.member (x ++ [c,b]) m
            ]

--fromMap :: Map.Map a b -> Chirotope
--fromMap m
--    | isChirotope m = Chirotope m
--    | otherwise = error "huhu"

-- let elems = Data.Set.fromList $ Data.Map.elems haha
-- let keys = Data.Map.keys haha
-- let rank = length $ head keys
-- let domain = List.nub $ concat keys  
-- let permutsAndParities = Helpful.kPermutationsWithParity rank domain      
-- let permuts = Prelude.map fst permutsAndParities   
-- let domain = List.nub $ concat keys
--
