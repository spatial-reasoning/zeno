module Gqr where

{----- This modules needs to be fixed!!



-- standard modules
import Control.Applicative ((<*))
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Text.Parsec.Language as L
import Text.Parsec.Perm
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token as P

-- local modules
import Basics
import Helpful
import qualified Calculus.FlipFlop as FF
import Parsing

--import Debug.Trace


eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

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


--------------------------}
