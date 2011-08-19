module Parsing where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<*))
import Control.Monad (when)
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Set as Set
import Basics
import Helpful
import Debug.Trace


eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

--------------------------
-- GQR parsing -- Begin --
--------------------------

parseGqrComment :: Parser String
parseGqrComment = do
    string "#"
    skipMany (oneOf " \t")
    comment <- manyTill anyChar eol
    return comment

parseGqrWhiteSpace :: Parser ()
parseGqrWhiteSpace = skipMany ( many1 space <|> parseGqrComment )

parseGqrInfo :: Parser (Maybe Int, Maybe String)
parseGqrInfo = do
    x <- try (skipMany (oneOf " \t") >> many1 digit <* (skipMany (oneOf " \t") >> string "#"))
    y <- skipMany (oneOf " \t") >> manyTill anyChar eol
    return (Just ((read x) + 1), Just y)

parseGqrEntity :: Parser String
parseGqrEntity = do
    parseGqrWhiteSpace
    a <- many1 (noneOf " .,:;()#\t\n\r")
    parseGqrWhiteSpace
    return a

parseGqrConstraint :: Parser Constraint
parseGqrConstraint = do
    a <- parseGqrEntity
    b <- parseGqrEntity
    char '('
    parseGqrWhiteSpace
    c <- sepBy parseGqrEntity parseGqrWhiteSpace
    char ')'
    parseGqrWhiteSpace
    return ( [map Char.toLower a, map Char.toLower b]
           , Set.fromList [map Char.toLower x | x <- c]
           )

parseGqrNetworkFile :: Parser ConstraintNetwork
parseGqrNetworkFile = do
    (numOfEnts, desc) <- option (Nothing, Nothing) parseGqrInfo
    parseGqrWhiteSpace
    constraints <- many1 parseGqrConstraint
    return (ConstraintNetwork constraints numOfEnts desc)

loadGqrNetworkFile :: FilePath -> IO ConstraintNetwork
loadGqrNetworkFile filename = do
    network <- parseFromFile parseGqrNetworkFile filename
    case network of
        Left error -> do
            fail $ "parse error in " ++ filename ++ " at " ++ show(error)
        Right success -> return success


------------------------
-- GQR parsing -- End --
------------------------


----------------------------
-- SparQ parsing -- Begin --
----------------------------

parseSparqComment :: Parser String
parseSparqComment = do
    string ";;"
    many (oneOf " \t")
    comment <- manyTill anyChar eol
    return comment

parseSparqWhiteSpace :: Parser ()
parseSparqWhiteSpace = skipMany ( many1 space <|> parseSparqComment )

parseSparqInfo :: Parser (Maybe Int, Maybe String)
parseSparqInfo = do
    x <- try (string ";;" >> skipMany (oneOf " \t") >>
             many1 digit <* (skipMany (oneOf " \t") >> string "#"))
    y <- skipMany (oneOf " \t") >> manyTill anyChar eol
    return (Just ((read x) + 1), Just y)

parseSparqEntity :: Parser String
parseSparqEntity = do
    a <- many1 (noneOf " .,:;()#\t\n\r")
    parseSparqWhiteSpace
    return a

parseSparqConstraint :: Int -> Parser Constraint
parseSparqConstraint n = do
    char '('
    parseSparqWhiteSpace
    a <- count (n-1) parseSparqEntity
    c <- choice
             [ between
                 (char '(' >> parseSparqWhiteSpace)
                 (char ')')
                 (many parseSparqEntity)
             , count 1 parseSparqEntity ]
    parseSparqWhiteSpace
    b <- parseSparqEntity
    char ')'
    parseSparqWhiteSpace
    return ( map (map Char.toLower) (a ++ [b])
           , Set.fromList [map Char.toLower x | x <- c]
           )

parseSparqNetworkFile :: Parser ConstraintNetwork
parseSparqNetworkFile = do
    (numOfEnts, desc) <- option (Nothing, Nothing) parseSparqInfo
    parseSparqWhiteSpace
    char '('
    parseSparqWhiteSpace
    cons <- choice
                [ try . many1 $ parseSparqConstraint 2
                , try . many1 $ parseSparqConstraint 3 ]
    parseSparqWhiteSpace
    char ')'
    parseSparqWhiteSpace
    return (ConstraintNetwork
                cons
                (numOfEnts)
                (desc))

loadSparqNetworkFile :: FilePath -> IO ConstraintNetwork
loadSparqNetworkFile filename = do
    network <- parseFromFile parseSparqNetworkFile filename
    case network of
        Left error -> do
            fail $ "parse error in " ++ filename ++ " at " ++ show(error)
        Right success ->
            return success


--------------------------
-- SparQ parsing -- End --
--------------------------

