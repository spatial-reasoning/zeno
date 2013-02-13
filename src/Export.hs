{-# LANGUAGE FlexibleInstances #-}
module Export where

-- standard modules
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

-- local modules
import Basics

--import Debug.Trace

--qstrify :: (Calculus a) => Network [String] (Set.Set a) -> String
--qstrify net = 


showNetwork :: (Relation (a b) b, Calculus b) => Network [String] (a b) -> String
showNetwork net@Network { nCons = cons
                    , nDesc = desc
                    , nCalc = calc
                    , nNumOfNodes = numOfNodes
                    } = 
    "calculus = " ++ show calc ++ "\ndescription = " ++ show desc ++
    (maybe "" (("\nnumber of nodes = " ++) . show) numOfNodes) ++
    "\nnetwork =\n" ++ 
    ( unlines $ map
        (\(nodes, rel) ->
            "    " ++ (intercalate " " nodes) ++
            " (" ++ showRel rel ++ ")"
        ) $ Map.toList cons
    )


class Sparqifiable a where
    sparqify :: Bool -> a -> String

    exportToSparq :: a -> FilePath -> IO ()
    exportToSparq net filename =
        writeFile filename (sparqify False net ++ "\n")


instance (Calculus a) => Sparqifiable (Network [String] (ARel a)) where
    sparqify = sparqify' (cSparqifyRel . aRel)

instance (Calculus a) => Sparqifiable (Network [String] (GRel a)) where
    sparqify = sparqify'
        ( concat . intersperse " " . Set.toList . Set.map cSparqifyRel . gRel )

sparqify' relFun oneLine net = desc ++ "(" ++ sep1
    ++ intercalate sep2 ["(" ++ (concat $ intersperse " " $ init x)
                             ++ " ("
                             ++ relFun y
                             ++ ") " ++ last x ++ ")"
                        | (x, y) <- Map.toList $ nCons net
                        ]
    ++ sep3 ++ ")"
  where
    (sep1, sep2, sep3) = if oneLine then
                             ("", " ", "")
                         else
                             ("\n ", "\n ", "\n")
    desc = if oneLine then
               ""
           else
               ";; description = " ++ nDesc net ++ "\n"



class Gqrifiable a where
    gqrify :: a -> (String, Map.Map Int String)

    exportToGqr :: a -> FilePath -> IO ()
    exportToGqr net filename =
        writeFile filename (fst $ gqrify net)


instance (Calculus a) => Gqrifiable (Network [String] (ARel a)) where
    gqrify = gqrify' (cGqrifyRel . aRel)

instance (Calculus a) => Gqrifiable (Network [String] (GRel a)) where
    gqrify = gqrify'
        ( concat . intersperse " " . Set.toList . Set.map cGqrifyRel . gRel )

gqrify' relFun net =
    ( show ((Set.size $ nodesIn $ nCons net) - 1)
        ++ " # description = " ++ nDesc net ++ "\n"
        ++ unlines [" " ++ (concat $ intersperse " " $ map show x) ++ " ( "
                        ++ relFun y
                        ++ " )" 
                   | (x, y) <- Map.toList numCons
                   ]
        ++ ".\n"
    , enumeration )
    where
        (numCons, enumeration) = enumerateAndEnumeration $ nCons net

