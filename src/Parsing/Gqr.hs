module Parsing.Gqr where

-- standard modules
--import Control.Applicative ((<*))
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
--import qualified Text.Parsec.Language as L
--import Text.Parsec.Perm
import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.Token as P

-- local modules
import Basics
--import qualified Helpful as H

--import Debug.Trace


eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseComment :: Parser String
parseComment = do
    string "#"
    skipMany (oneOf " \t")
    comment <- manyTill anyChar eol
    return comment

parseWhiteSpace :: Parser ()
parseWhiteSpace = skipMany ( many1 space <|> parseComment )

parseInfo :: Parser (Int, String, String)
parseInfo = do
    x <- skipMany (oneOf " \t") >> many1 digit
    y <- optionMaybe
            (try ( skipMany (oneOf " \t") >> char '#' >>
                skipMany (oneOf " \t") >>
                manyTill anyChar (try (lookAhead (choice [string "#", eol]))) )
            )
    z <- optionMaybe
            (try ( char '#' >> skipMany (oneOf " \t") >>
                manyTill anyChar (try (lookAhead eol)) )
            )
    manyTill (oneOf " \t") eol
    return ( (read x) + 1
           , fromMaybe (nDesc eNetwork) y
           , fromMaybe (nCalc eNetwork) z )

parseEntity :: Parser String
parseEntity = do
    parseWhiteSpace
    a <- many1 (noneOf " .,:;()#\t\n\r")
    parseWhiteSpace
    return a

parseConstraint :: Parser ([String], Set.Set String)
parseConstraint = do
    a <- parseEntity
    b <- parseEntity
    char '('
    parseWhiteSpace
    c <- sepBy parseEntity parseWhiteSpace
    char ')'
    parseWhiteSpace
    return ( [map Char.toLower a, map Char.toLower b]
           , Set.fromList [map Char.toLower x | x <- c]
           )

parseNetwork :: (Calculus a) => Parser (Network [String] (Set.Set a))
parseNetwork = do
    (numOfNodes, desc, calc) <- parseInfo
    parseWhiteSpace
    cons <- many1 parseConstraint
    return eNetwork { nCons = Map.map (Set.map readRel) $ Map.fromList cons
                    , nDesc = desc
                    , nCalc = calc
                    , nNumOfNodes = Just numOfNodes }

loadNetwork :: (Calculus a) => FilePath -> IO (Network [String] (Set.Set a))
loadNetwork filename = do
    network <- parseFromFile parseNetwork filename
    case network of
        Left error -> do
            fail $ "parse error in " ++ filename ++ " at " ++ show(error)
        Right success -> return success

