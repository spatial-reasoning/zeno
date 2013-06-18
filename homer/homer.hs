module Main (main) where

import System.Exit
import System.Environment
import System.IO
import System.Console.GetOpt
import Text.ParserCombinators.Parsec
import Text.Printf
import Data.List (intersect, intercalate)
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Naa
import Debug.Trace

version = "                                        ."
authors = "André Scholz (andre.scholz@uni-bremen.de)"

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    -- Here we thread defaultOptions through all supplied option actions
    opts <- foldl (>>=) (return defaultOptions) actions

    let Options { optVerbose = verbose
                , optInput   = input
                , optOutput  = output  } = opts

    putStrLn ("\nSearching for homomorphisms from "
        ++ (takeWhile (/= '.') (nonOptions !! 0)) ++ " to "
        ++ (takeWhile (/= '.') (nonOptions !! 2)) ++ ".\n")

    -- read files and generate mappings for composition and converse
    convMap1 <- loadConvFile (nonOptions !! 0)
    ((identity1, baserelations1), compMap1) <- loadCompFile (nonOptions !! 1)
    convMap2 <- loadConvFile (nonOptions !! 2)
    ((identity2, baserelations2), compMap2) <- loadCompFile (nonOptions !! 3)

    let naa1 = NonAssociativeAlgebra { baserelations = baserelations1
                                     , identity      = identity1
                                     , converse      = convMap1
                                     , composition   = compMap1
                                     }

    let naa2 = NonAssociativeAlgebra { baserelations = baserelations2
                                     , identity      = identity2
                                     , converse      = convMap2
                                     , composition   = compMap2
                                     }

    let maxLength = Set.findMax $
            Set.map (\x -> length ((Set.toList x) !! 0)) baserelations1
    let foundHoms = (findHom naa1 naa2 (trivialHom naa1 naa2))
    if (Set.size baserelations1) <= (Set.size baserelations2)
        then do
            putStrLn "Searching for identities..."
            putStrLn ("Found:  Id1 = ( "
                ++ intercalate ", " (Set.toList identity1) ++ " )")
            putStrLn ("        Id2 = ( "
                ++ intercalate ", " (Set.toList identity2) ++ " )\n")
        else putStrLn (
                 (takeWhile (/= '.') (nonOptions !! 0))
                 ++ " has more baserelations than "
                 ++ (takeWhile (/= '.') (nonOptions !! 2)) ++ ".")

    if ( (Set.size baserelations1) > (Set.size baserelations2)
      || (foundHoms == Set.empty) )
        then putStrLn ("I found no homomorphism from "
                 ++ (takeWhile (/= '.') (nonOptions !! 0)) ++ " to "
                 ++ (takeWhile (/= '.') (nonOptions !! 2)) ++ ".\n")
        else do
            mapM_
                (\(x,y) -> do
                   printf "======== %d. Homomorphism ========\n\n" (x+1 :: Int)
                   mapM_
                       (\(u,v) -> do
                           printf ("%-" ++ show (maxLength + 1) ++ "s")
                               (intercalate ", " (Set.toList u))
                           putStrLn ("↦ ( " ++ (intercalate ", " (Set.toList v)
                               ++ " )")))
                       (Map.toList y)
                   putStrLn "" )
               (foldl (\x y -> x ++ [(length x, y)]) [] (Set.toList foundHoms))
            putStrLn ("These are all homomorphisms from "
                ++ (takeWhile (/= '.') (nonOptions !! 0)) ++ " to "
                ++ (takeWhile (/= '.') (nonOptions !! 2)) ++ ".\n")


-- begin commandline option handling ------------------------------------------

-- define the type of options
data Options = Options { optInput   :: IO String
                       , optOutput  :: String -> IO ()
                       , optVerbose :: Bool
                       }

-- define default values and actions for options
defaultOptions :: Options
defaultOptions = Options { optInput   = getContents
                         , optOutput  = putStr
                         , optVerbose = False
                         }
-- define the commandline options
options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['h'] ["help"]
        (NoArg showUsage)
        "Print this usage information"
--    , Option ['i'] ["input"]
--        (ReqArg readInput "FILE")
--        "Input file to read"
--    , Option ['o'] ["output"]
--        (ReqArg writeOutput "FILE")
--        "Output file to write"
--    , Option ['v'] ["verbose"]
--        (NoArg verboseTrue)
--        "Enable verbose messages"
    , Option ['V'] ["version"]
        (NoArg showVersion)
        "Show version"
    , Option [] ["authors"]
        (NoArg showAuthors)
        "Who the heck wrote this program?"
    ]

-- functions used in the section above
showUsage _ = do
    prg <- getProgName
    hPutStrLn stderr (
        usageInfo ("\nUsage: " ++ prg ++ " [Option]... ConvFile1 CompFile1 " ++
            "ConvFile2 CompFile2\n") options)
    exitWith ExitSuccess

showVersion _ = do
    putStrLn version
    exitWith ExitSuccess

showAuthors _ = do
    putStrLn authors
    exitWith ExitSuccess

-- verboseTrue opt = return opt { optVerbose = True }
-- readInput arg opt = return opt { optInput = readFile arg }
-- writeOutput arg opt = return opt { optOutput = writeFile arg }

-- end commandline option handling --------------------------------------------

-- begin parsing composition tables -------------------------------------------
parseGqrRelation :: GenParser Char st String
parseGqrRelation = do
    spaces
--    a <- many1 (alphaNum <|> char '_' <|> char '=' <|> char '<' <|> char '>'
--                         <|> char '^' <|> char '~' <|> char '+' <|> char '-')
    a <- many1 (noneOf " :;,()\n\t")
    spaces
    return a

-- parse composition file
parseGqrComp :: GenParser Char st ((Relation, Relation), Relation)
parseGqrComp = do
    a <- parseGqrRelation
    char ':'
    b <- parseGqrRelation
    count 2 (char ':')
    spaces
    char '('
    spaces  -- this is only needed if the composition is not fully definied.
    c <- sepBy parseGqrRelation spaces
    char ')'
    spaces
    return ( ( Set.singleton (map Char.toLower a)
             , Set.singleton (map Char.toLower b) )
           , Set.fromList [map Char.toLower x | x <- c] )

parseGqrCompTable :: GenParser Char st [((Relation, Relation), Relation)]
parseGqrCompTable = many1 parseGqrComp

-- parse converses file
parseGqrConv :: GenParser Char st (Relation, Relation)
parseGqrConv = do
    a <- parseGqrRelation
    count 2 (char ':')
    b <- parseGqrRelation
    return ( Set.singleton (map Char.toLower a)
           , Set.singleton (map Char.toLower b) )

parseGqrConvTable :: GenParser Char st [(Relation, Relation)]
parseGqrConvTable = many1 parseGqrConv

getIdentityAndBaserelations :: [((Relation, Relation), Relation)]
                            -> (Relation, Set.Set Relation)
getIdentityAndBaserelations a =
    ( foldl1 Set.union $ intersect
        ( filter
            ( \u -> and [ y == z | ((x,y),z) <- a, x == u ] )
            (allBaserelations) )
        ( filter
            (\u -> and [ x == z | ((x,y),z) <- a, y == u ] )
            (allBaserelations) )
    , Set.fromList allBaserelations
    )
    where allBaserelations = [ x | ((x,_),_) <- a ]

-- read the input file
loadCompFile :: FilePath -> IO ( (Relation, Set.Set Relation)
                               , Map.Map (Relation,Relation) Relation
                               )
loadCompFile filename = do
    file <- readFile filename
    let table = runParser parseGqrCompTable () "" file
    case table of
        Left error -> do
            fail $ "parse error in " ++ filename ++ " at " ++ show(error)
        Right success -> return ( getIdentityAndBaserelations success
                                , Map.fromList success
                                )

loadConvFile :: FilePath -> IO (Map.Map Relation Relation)
loadConvFile filename = do
    file <- readFile filename
    let table = runParser parseGqrConvTable () "" file
    case table of
        Left error -> do
            fail $ "parse error in " ++ filename ++ " at " ++ show(error)
        Right success -> return (Map.fromList success)

-- end parsing composition tables ---------------------------------------------

