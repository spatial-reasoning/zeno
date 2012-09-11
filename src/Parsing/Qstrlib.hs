module Parsing.Qstrlib where

-- standard modules
--import Control.Applicative ((<*))
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
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
    , identLetter   = alphaNum <|> char '_'
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

parseNetwork :: (Calculus a) => Parser (Network [String] (Set.Set a))
parseNetwork = do
    (calc, desc, net, numOfNodes) <- parseNetworkRaw
    let parsedNet = eNetwork
            { nCons = foldl
                (\acc (nodes, rel) ->
                    insertCon nodes rel acc
                ) Map.empty [(x, Set.fromList (map readRel y)) | (x, y) <- net]
            , nDesc = desc
            , nCalc = calc
            , nNumOfNodes = numOfNodes
            }
    return parsedNet

loadNetwork :: (Calculus a)
            => FilePath -> IO (Network [String] (Set.Set a))
loadNetwork filename = do
    network <- parseFromFile parseNetwork filename
    case network of
        Left error -> do
            fail $ "parse error in " ++ filename ++ " at " ++ show(error)
        Right success -> return success

