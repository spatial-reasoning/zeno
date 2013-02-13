{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
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

class (Relation a b) => GqrParsable a b | a -> b where
    parseConstraint :: Parser ([String], a)
    parseNetwork :: Parser (Network [String] a)
    parseNetwork = do
        (numOfNodes, desc, calc) <- parseInfo
        parseWhiteSpace
        cons <- many1 parseConstraint
        -- improve: change "fromJust" to catch inconsistent networks
        return eNetwork { nCons = foldl (\ acc (x, y) -> fromJust $
                                            insertCon x y acc
                                        ) Map.empty cons
                        , nDesc = desc
                        , nCalc = calc
                        , nNumOfNodes = Just numOfNodes }

    loadNetwork :: FilePath -> IO (Network [String] a)
    loadNetwork filename = do
        network <- parseFromFile parseNetwork filename
        case network of
            Left error -> do
                fail $ "parse error in " ++ filename ++ " at " ++ show(error)
            Right success ->
                return success

instance (Calculus a) => GqrParsable (GRel a) a where
    parseConstraint = do
        a <- parseEntity
        b <- parseEntity
        char '('
        parseWhiteSpace
        c <- sepBy parseEntity parseWhiteSpace
        char ')'
        parseWhiteSpace
        return ( [map Char.toLower a, map Char.toLower b]
               , GRel $ Set.fromList [cReadRel x | x <- c]
               )

