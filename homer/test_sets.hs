module Main where

import qualified Data.Set as Set
import qualified Data.List as List

canBeHom a = Set.fold f True a
             where f (x,y) z = (x < y) && z

findHom a b c  | Set.null a && canBeHom c  = c
--               | Set.null c && (Set.size a > Set.size b) = Set.empty
               | canBeHom c  = Set.unions [ findHom (Set.delete x a)
                                                    (Set.delete x b)
                                                    (Set.insert (x,y) c)
                                          | x <- Set.toList a
                                          , y <- Set.toList b
                                          ]
               | otherwise = Set.empty
