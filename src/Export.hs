module Export where

-- standard modules
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

-- local modules
import Basics

--import Debug.Trace

sparqify :: (Calculus a) => Network [String] (Set.Set a) -> String
sparqify net = ";; description = " ++ nDesc net ++ "\n(\n"
    ++ unlines [" (" ++ (concat $ List.intersperse " " $ init x)
                     ++ " ("
                     ++ (concat $ List.intersperse " " $
                            Set.toList $ Set.map showRel y)
                     ++ ") " ++ last x ++ ")"
               | (x, y) <- Map.toList $ nCons net
               ]
    ++ ")"

gqrify :: (Calculus a) => Network [String] (Set.Set a) -> String
gqrify net = show ((Set.size $ nodesIn net) - 1)
    ++ " # description = " ++ nDesc net ++ "\n"
    ++ unlines [" " ++ (concat $ List.intersperse " " $ map show x) ++ " ( "
                    ++ (concat $ List.intersperse " " $
                           Set.toList $ Set.map showRel y)
                    ++ " )" 
               | (x, y) <- Map.toList $ enumerate $ nCons net
               ]
    ++ ".\n"

exportToSparQ :: (Calculus a) => Network [String] (Set.Set a) -> FilePath -> IO ()
exportToSparQ net filename = do
    writeFile filename (sparqify net ++ "\n")

exportToGqr :: (Calculus a) => Network [String] (Set.Set a) -> FilePath -> IO ()
exportToGqr net filename = do
    writeFile filename (gqrify net)

