module Export where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Basics
import Debug.Trace

sparqify :: (Calculus a) => Network [String] (Set.Set a) -> String
sparqify net = ";; description = " ++ nDesc net ++ "\n(\n"
    ++ unlines [" (" ++ (concat $ List.intersperse " " $
                            map (("a" ++) . init) x)
                     ++ " ("
                     ++ (concat $ List.intersperse " " $
                            Set.toList $ Set.map showRel y)
                     ++ ") a" ++ last x ++ ")"
               | (x, y) <- Map.toList $ nCons net
               ]
    ++ ")\n"

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
    writeFile filename (sparqify net)

exportToGqr :: (Calculus a) => Network [String] (Set.Set a) -> FilePath -> IO ()
exportToGqr net filename = do
    writeFile filename (gqrify net)

