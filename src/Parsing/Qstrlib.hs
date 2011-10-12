module Parsing.Qstrlib where

-- standard modules
import Control.Applicative ((<*))
import qualified Data.Map as Map
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
qstrParens     = P.parens qstrLexer

qstrConstraint = do
    a <- many1 qstrIdent
    b <- qstrParens (many1 qstrIdent)
    return (a,b)

qstrNetwork = many1 qstrConstraint

parseNetworkFileRaw :: Parser (String, String, [([String], [String])])
parseNetworkFileRaw = qstrWhiteSpace >> permute ( tuple
    <$$> (qstrSymbol "calculus =" >> qstrString)
    <||> (qstrSymbol "description =" >> qstrString)
    <||> (qstrSymbol "network =" >> qstrNetwork) )
    where
        tuple a b c = (a,b,c)

parseNetworkFile :: (Calculus a) => Parser (NetworkFile a)
parseNetworkFile = do
    (calc, desc, net) <- parseNetworkFileRaw
    let parsedNet = eNetwork
            { nCons = Map.fromList
                          [ (x, Set.fromList (map readRel y))
                          | (x, y) <- net
                          ]
            , nDesc = desc
            }
    return (eNetworkFile { nfCalculus = calc, nfNetwork = parsedNet })

loadNetworkFile :: (Calculus a) => FilePath -> IO (NetworkFile a)
loadNetworkFile filename = do
    network <- parseFromFile parseNetworkFile filename
    case network of
        Left error -> do
            fail $ "parse error in " ++ filename ++ " at " ++ show(error)
        Right success -> return success

