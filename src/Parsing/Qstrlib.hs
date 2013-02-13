module Parsing.Qstrlib where

-- standard modules
--import Control.Applicative ((<*))
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.String
import qualified Text.Parsec.Language as L
import Text.Parsec.Perm
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token as P

-- local modules
import Basics
import Helpful

--import Debug.Trace


qstrLibLang = L.emptyDef
    { commentLine   = "#"
    , identStart    = alphaNum <|> char '_'
    , identLetter   = alphaNum <|> char '_' <|> char '<'
    , caseSensitive = True
    }

qstrLexer = P.makeTokenParser qstrLibLang

qstrWhiteSpace = P.whiteSpace qstrLexer
qstrIdent      = P.identifier qstrLexer
qstrSymbol     = P.symbol qstrLexer
qstrString     = P.stringLiteral qstrLexer
qstrNatural    = P.natural qstrLexer
qstrParens     = P.parens qstrLexer

qstrConstraint = do
    a <- many1 qstrIdent
    b <- qstrParens (many1 qstrIdent)
    return (a,b)

qstrNetwork = many1 $ try $ qstrConstraint

parseNetworkRaw :: Parser (String, String, [([String], [String])], Maybe Int)
parseNetworkRaw = qstrWhiteSpace >> permute
    ( tuple
        <$$> try (qstrSymbol "calculus =" >> qstrString)
        <||> try (qstrSymbol "description =" >> qstrString)
        <||> try (qstrSymbol "network =" >> qstrNetwork)
        <|?> ( Nothing , (Just . fromIntegral) `liftM` try (qstrSymbol "number of nodes =" >>
                                           qstrNatural) )
    )
    where
        tuple a b c d = (a,b,c,d)

parseNetwork :: (Relation (a b) b, Calculus b) => Parser (Network [String] (a b))
parseNetwork = do
    (calc, desc, net, numOfNodes) <- parseNetworkRaw
    let parsedNet = eNetwork
            { nCons = foldl
                (\acc (nodes, rel) ->
                    -- improve: change "fromJust" to catch inconsistent
                    -- networks
                    fromJust $ insertCon nodes rel acc
                ) Map.empty [(x, readRel $ unwords y) | (x, y) <- net]
            , nDesc = desc
            , nCalc = calc
            , nNumOfNodes = numOfNodes
            }
    return parsedNet

loadNetwork :: (Relation (a b) b, Calculus b)
            => FilePath -> IO (Network [String] (a b))
loadNetwork filename = do
    network <- parseFromFile parseNetwork filename
    case network of
        Left error -> do
            fail $ "parse error in " ++ filename ++ " at " ++ show(error)
        Right success -> return success

