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

showAtomicNet :: (Calculus b) => Network [String] b -> String
showAtomicNet net@Network { nCons = cons
                    , nDesc = desc
                    , nCalc = calc
                    , nNumOfNodes = numOfNodes
                    } = 
    "calculus = " ++ show calc ++ "\ndescription = " ++ show desc ++
    (maybe "" (("\nnumber of nodes = " ++) . show) numOfNodes) ++
    "\nnetwork =\n" ++ 
    ( unlines $ map
        (\(nodes, rel) ->
            "    " ++ (intercalate " " nodes) ++ " ( " ++ showRel rel ++ " )"
        ) $ Map.toList cons
    )

showNonAtomicNet :: (Calculus b) => Network [String] (Set.Set b) -> String
showNonAtomicNet net@Network { nCons = cons
                    , nDesc = desc
                    , nCalc = calc
                    , nNumOfNodes = numOfNodes
                    } = 
    "calculus = " ++ show calc ++ "\ndescription = " ++ show desc ++
    (maybe "" (("\nnumber of nodes = " ++) . show) numOfNodes) ++
    "\nnetwork =\n" ++ 
    ( unlines $ map
        (\(nodes, rels) ->
            "    " ++ (intercalate " " nodes) ++ " ( " ++
            (concatMap showRel $ Set.toList rels) ++
            " )"
        ) $ Map.toList cons
    )

sparqify :: (Calculus a) => Bool -> Network [String] (Set.Set a) -> String
sparqify oneLine net = desc ++ "(" ++ sep1
    ++ intercalate sep2 ["(" ++ (concat $ intersperse " " $ init x)
                             ++ " ("
                             ++ (concat $ intersperse " " $
                                    Set.toList $ Set.map showRel y)
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

gqrify :: (Calculus a) => Network [String] (Set.Set a) -> (String, Map.Map Int String)
gqrify net =
    ( show ((Set.size $ nodesIn net) - 1)
        ++ " # description = " ++ nDesc net ++ "\n"
        ++ unlines [" " ++ (concat $ intersperse " " $ map show x) ++ " ( "
                        ++ (concat $ intersperse " " $
                               Set.toList $ Set.map showRel y)
                        ++ " )" 
                   | (x, y) <- Map.toList numCons
                   ]
        ++ ".\n"
    , enumeration )
    where
        (numCons, enumeration) = enumerate2 $ nCons net

exportToSparq :: (Calculus a) => Network [String] (Set.Set a) -> FilePath -> IO ()
exportToSparq net filename = do
    writeFile filename (sparqify False net ++ "\n")

exportToGqr :: (Calculus a) => Network [String] (Set.Set a) -> FilePath -> IO ()
exportToGqr net filename = do
    writeFile filename (fst $ gqrify net)

