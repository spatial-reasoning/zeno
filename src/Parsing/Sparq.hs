module Parsing.Sparq where

-- standard modules
import Control.Applicative ((<*))
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Text.Parsec.Perm
import Text.ParserCombinators.Parsec

-- local modules
import Basics
import Parsing

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

parseInfo :: Parser (Maybe Int, Maybe String)
parseInfo = do
    x <- try (string ";;" >> skipMany (oneOf " \t") >>
             many1 digit <* (skipMany (oneOf " \t") >> string "#"))
    y <- skipMany (oneOf " \t") >> manyTill anyChar eol
    return (Just ((read x) + 1), Just y)

parseEntity :: Parser String
parseEntity = do
    a <- many1 (noneOf " .,:;()#\t\n\r")
    parseWhiteSpace
    return a

parseConstraint :: Int -> Parser ([String], Set.Set String)
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
           , Set.fromList [map Char.toLower x | x <- c]
           )

parseNetwork :: (Calculus a) => Parser (Network [String] (Set.Set a))
parseNetwork = do
    (numOfEnts, desc) <- option (Nothing, Nothing) parseInfo
    parseWhiteSpace
    char '('
    parseWhiteSpace
    cons <- choice
                [ try . many1 $ parseConstraint 2
                , try . many1 $ parseConstraint 3 ]
    parseWhiteSpace
    char ')'
    parseWhiteSpace
    return eNetwork { nCons = Map.map (Set.map readRel) $ Map.fromList cons
                    , nDesc = fromMaybe "" desc }
    
parseNetworkFile :: (Calculus a) => Parser (NetworkFile a)
parseNetworkFile = do
    net <- parseNetwork
    return eNetworkFile { nfNetwork = net }

loadNetworkFile :: (Calculus a) => FilePath -> IO (NetworkFile a)
loadNetworkFile filename = do
    network <- parseFromFile parseNetworkFile filename
    case network of
        Left error -> do
            fail $ "parse error in " ++ filename ++ " at " ++ show(error)
        Right success ->
            return success

