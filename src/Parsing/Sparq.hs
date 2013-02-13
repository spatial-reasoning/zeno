{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Parsing.Sparq where

-- standard modules
--import Control.Applicative ((<*))
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Text.Parsec.Perm
import Text.ParserCombinators.Parsec

-- local modules
import Basics

--import Debug.Trace


eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseComment :: Parser String
parseComment = do
    string ";;"
    many (oneOf " \t")
    comment <- manyTill anyChar eol
    return comment

parseWhiteSpace :: Parser ()
parseWhiteSpace = skipMany ( many1 space <|> parseComment )

parseInfo :: Parser (Maybe Int, Maybe String, Maybe String)
parseInfo = do
    x <- skipMany (oneOf " \t") >> string ";;" >> skipMany (oneOf " \t") >>
             many1 digit
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
    return ( Just ( (read x) + 1 )
           , y
           , z )

parseEntity :: Parser String
parseEntity = do
    a <- many1 (noneOf " .,:;()#\t\n\r")
    parseWhiteSpace
    return a

class (Relation a b) => SparqParsable a b | a -> b where
    parseConstraint :: Int -> Parser ([String], a)
    parseNetwork :: Parser (Network [String] a)
    parseNetwork = do
        (numOfNodes, desc, calc) <- option (Nothing, Nothing, Nothing) parseInfo
        parseWhiteSpace
        char '('
        parseWhiteSpace
        cons <- choice
                    [ try . many1 $ parseConstraint 2
                    , try . many1 $ parseConstraint 3 ]
        parseWhiteSpace
        char ')'
        parseWhiteSpace
        -- improve: change "fromJust" to catch inconsistent networks
        return eNetwork { nCons = foldl (\ acc (x, y) -> fromJust $
                                            insertCon x y acc
                                        ) Map.empty cons
                        , nDesc = fromMaybe (nDesc eNetwork) desc
                        , nCalc = fromMaybe (nCalc eNetwork) calc
                        , nNumOfNodes = numOfNodes }

    loadNetwork :: (Calculus a) => FilePath -> IO (Network [String] a)
    loadNetwork filename = do
        network <- parseFromFile parseNetwork filename
        case network of
            Left error -> do
                fail $ "parse error in " ++ filename ++ " at " ++ show(error)
            Right success ->
                return success


instance (Calculus a) => SparqParsable (GRel a) a where
    parseConstraint n = do
        char '('
        parseWhiteSpace
        a <- count (n-1) parseEntity
        c <- choice
                 [ between
                     (char '(' >> parseWhiteSpace)
                     (char ')')
                     (many parseEntity)
                 , count 1 parseEntity ]
        parseWhiteSpace
        b <- parseEntity
        char ')'
        parseWhiteSpace
        return ( map (map Char.toLower) (a ++ [b])
               -- fixme: replace "cReadRel" with something sparq-specific.
               , GRel $ Set.fromList [cReadRel x | x <- c]
               )

