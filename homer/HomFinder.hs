module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import qualified Data.Map as Map
import System.IO.Unsafe

type CompEntry = ((String, String), [String])

parseAtomic :: GenParser Char st Char
parseAtomic =
    do
      x <- oneOf ['F','E','I','S','B','L','R']
      return x

parseDRA :: GenParser Char st String
parseDRA =
    do
      spaces
      a <- parseAtomic
      b <- parseAtomic
      c <- parseAtomic
      d <- parseAtomic
      spaces
      return $ [a, b, c, d]

parseAllen :: GenParser Char st String
parseAllen =
     do
      spaces
      a <- many1 $ alphaNum
      spaces
      return $ a

parseSparQComp :: GenParser Char st String -> GenParser Char st CompEntry
parseSparQComp ps =
    do
      spaces
      char '('
      spaces
      a <- ps
      spaces
      b <- ps
      spaces
      c <-
          do
            char '('
            spaces
            cl <-  many ps
            spaces
            char ')'
            spaces
            return cl
          <|>
          do
            spaces
            cl <-  many ps
            spaces
            return cl
      char ')'
      spaces
      return $ ((a, b), c)

parseSparQTable :: GenParser Char st String -> GenParser Char st [CompEntry]
parseSparQTable ps =
    do
      t <- many1 $ parseSparQComp ps
      return t

loadFile :: GenParser Char () String
         -> FilePath
         -> IO (Either ParseError [CompEntry])
loadFile ps str =
    do
      f <- readFile str
      let t = runParser (parseSparQTable ps) () "" f
      return t

map2Dra :: [String] -> String -> Maybe String
map2Dra mp s =
    case Map.lookup s (Map.fromList $ zip (take (length mp) ["EQ", "M", "MI", "O", "OI", "D", "DI", "S", "SI", "F", "FI", "B", "BI"]) mp) of
      Just x -> Just $ x
      _      -> Nothing

cmps :: [CompEntry]
     -> Set.Set String
     -> Set.Set String
     -> Maybe (Set.Set String)
cmps tbl f s =
    let
        m = Map.fromList $ tbl
    in
    Set.fold (\x y ->
                  Set.fold (\a b ->
                            maybeUnion
                            (
                             case (Map.lookup (x,a) m) of
                               Just p -> Just $ Set.fromList p
                               _      -> Nothing
                            )
                            b) y  s) (Just $ Set.empty) f

maybeUnion :: (Ord a) =>
              Maybe (Set.Set a)
                  -> Maybe (Set.Set a)
                  -> Maybe (Set.Set a)
maybeUnion s1 s2 =
    case (s1,s2) of
      (Just x, Just y) -> Just $ Set.union x y
      (_,_)            -> Nothing

makeComps :: [String] -> [CompEntry] -> [(String,String)]
makeComps arels cs =
    let
        rels = Set.toList $
               foldl (\y ((a, _), _) ->
                      Set.insert a y
                     ) Set.empty cs
        cmps =
            filter (\(x,y) -> x `elem` arels && y `elem` arels) $
                       [(x,y) |
                        x <- rels
                       ,y <- rels]
    in cmps

isCompat :: [Maybe ((String, String), (String, String))]
            -> Bool
isCompat lst = length (filter (\x -> x == Nothing) lst) == 0

embedCheck :: [CompEntry]
           -> [CompEntry]
           -> [String]
           -> (String, String)
           -> Maybe ((String, String), (String, String))
embedCheck allen dra dmap (one, two) =
    let
        allenC = case cmps allen (Set.singleton one) (Set.singleton two) of
                   Just x -> if (Nothing `Set.member` Set.map (map2Dra dmap) x)
                              then Nothing
                              else
                                  Just $ Set.map (\z -> fromJust $ map2Dra dmap z) x
                   _      -> Nothing
        draC   = case cmps allen (Set.singleton one) (Set.singleton two) of
                   Just x -> if (Nothing `Set.member` Set.map (map2Dra dmap) x)
                              then Nothing
                              else
                                  cmps dra (Set.singleton $ fromJust $ map2Dra dmap one) (Set.singleton $ fromJust $ map2Dra dmap two)
                   _      -> Nothing
    in
      case (allenC, draC) of
        (Just ax, Just cx) ->
            case (ax == cx) of
              True         -> Just $ ((one, two), (fromJust $ map2Dra dmap one, fromJust $ map2Dra dmap two))
              False        -> Nothing
        (Nothing, Nothing) -> Just $ ((one, two), (fromJust $ map2Dra dmap one, fromJust $ map2Dra dmap two))
        (_, _)             -> error $ "This should not happen!"

embeds :: [CompEntry]
       -> [CompEntry]
       -> [String]
       -> Maybe [((String, String), (String, String))]
embeds allen dra dmap =
    let
        arels = take (length dmap) ["EQ", "M", "MI", "O", "OI", "D", "DI", "S", "SI", "F", "FI", "B", "BI"]
        pairs = makeComps arels allen
    in
      if (Nothing `elem` map (embedCheck allen dra dmap) pairs)
       then
           Nothing
       else
           Just $ map (\x -> fromJust $ embedCheck allen dra dmap x) pairs

liftList :: [a] -> [[a]]
liftList lst =
    map (\x -> [x]) lst

allWithAll :: [[a]] -> [[a]] -> [[a]] -> [[a]]
allWithAll lst1 lst2 backup =
    case (lst1, lst2) of
      ([], _)          -> []
      ((a:as), [])     -> allWithAll as backup backup
      ((a:as), (b:bs)) -> (a ++ b):(allWithAll (a:as) bs backup)

allWithAllSec :: Ord a => [[a]] -> [[a]] -> [[a]]
allWithAllSec lst1 lst2 =
    let
        out = allWithAll lst1 lst2 lst2
    in
      filter (\x -> (length (Set.toList $ Set.fromList $ x)) == (length x)) out


findHom :: [CompEntry]
        -> [CompEntry]
        -> [String]
        -> Maybe [[String]]
findHom allen dra rels =
    case findHomH allen dra rels $ allWithAllSec (liftList rels) (liftList rels) of
      [] -> Nothing
      xs -> Just $ xs

findHomH :: [CompEntry]
         -> [CompEntry]
         -> [String]
         -> [[String]]
         -> [[String]]
findHomH allen dra rels candidates =
    case (filter (\x -> (embeds allen dra x) /= Nothing) candidates) of
      [] -> []
      as -> if (length (head $ candidates) < (length ["EQ", "M", "MI", "O", "OI", "D", "DI", "S", "SI", "F", "FI", "B", "BI"]) - 1)
             then
                  findHomH allen dra rels (allWithAllSec candidates (liftList rels))
             else
                  lastManStanding allen dra $ allWithAllSec candidates (liftList rels)

lastManStanding :: [CompEntry] -> [CompEntry] -> [[String]] -> [[String]]
lastManStanding allen dra dmap =
    case dmap of
      []   -> []
      x:xs ->
          let
              em = embeds allen dra x
          in
            if em /= Nothing
             then
              [x]
             else
              lastManStanding allen dra xs

main :: IO()
main =
    do
      dra   <- loadFile parseDRA "dra_f.comp"
      allen <- loadFile parseAllen "allen.comp"
      case (dra, allen) of
        (Right d, Right a) -> putStrLn $ show $ findHom a d aback
        _                  -> putStrLn $ errorString


errorString :: String
errorString = "Nope"

afor :: [[Char]]
afor = ["SESE", "EFBS", "BSEF", "IFBI", "BIIF", "BFII", "IIBF", "SFSI", "SISF", "BEIE", "IEBE", "FFBB", "BBFF"]

aback :: [[Char]]
aback = ["ESES", "BBBB", "FEFE", "FIFI", "EBIS", "FBII", "FSEI", "IBIB", "ISEB", "IIFB", "EIFS", "SBSB", "FFFF"]
