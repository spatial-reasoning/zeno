module Export where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Basics
import Debug.Trace

sparqify :: ConstraintNetwork -> String
sparqify net = ";; "
    ++ (if Maybe.isJust (numberOfEntities net)
        then show ((Maybe.fromJust (numberOfEntities net)) - 1)
        else show ((getNumberOfEntities net) - 1))
    ++ " # "
    ++ (if Maybe.isJust (description net)
        then Maybe.fromJust (description net)
        else "")
    ++ "\n(\n"
    ++ unlines [" (" ++ (concat (List.intersperse
                                     " " 
                                     (map ("a" ++) . init $ x) ) )
                     ++ " ("
                     ++ (concat (List.intersperse " " (Set.toList y)))
                     ++ ") a" ++ last x ++ ")"
               | (x,y) <- (constraints net)]
    ++ ")\n"

gqrify :: ConstraintNetwork -> String
gqrify net =
    (if Maybe.isJust (numberOfEntities net)
        then show (Maybe.fromJust (numberOfEntities net) - 1)
        else show ((getNumberOfEntities net) - 1))
    ++ " # "
    ++ (if Maybe.isJust (description net)
        then Maybe.fromJust (description net)
        else "")
    ++ "\n"
    ++ unlines [" " ++ x ++ " " ++ y ++ " ( "
                  ++ (concat (List.intersperse " " (Set.toList z))) ++ " )" 
               | ([x,y],z) <- enumerate (constraints net)]
    ++ ".\n"

enumerate :: [Constraint] -> [Constraint]
enumerate cons = zip (map enum xs) ys
    where
        (xs, ys) = unzip cons
        ents = List.nub $ foldl1 List.union [ fst x | x <- cons ]
        enum list = [ show $ Maybe.fromJust $ List.elemIndex x ents
                    | x <- list ]

exportToSparQ :: ConstraintNetwork -> FilePath -> IO ()
exportToSparQ net filename = do
    writeFile filename (sparqify net)

exportToGqr :: ConstraintNetwork -> FilePath -> IO ()
exportToGqr net filename = do
    writeFile filename (gqrify net)

