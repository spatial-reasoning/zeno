module Interface.Sparq.Server where

{------------------------------------------------------------------------------
 - Interface to SparQ server
------------------------------------------------------------------------------}

-- standard modules
import Control.Exception
import Control.Monad
import qualified Data.Maybe as Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO
import System.IO.Unsafe
import Network.TCP
import Text.Parsec

-- local modules
import Basics
import Parsing.Sparq
import Export
import Helpful.Process

-- Debugging and Timing
--import Data.Time.Clock (diffUTCTime, getCurrentTime)
--import Debug.Trace

sparq = unsafePerformIO $ do
    sparq' <- openTCPConnection "localhost" 47647
    readToPrompt sparq'
    return sparq'

connectToSparq :: IO (HandleStream String)
connectToSparq = do
    sparq <- openTCPConnection "localhost" 47647
    readToPrompt sparq
    return sparq

readBlocks :: (HStream bufType) => HandleStream bufType -> Int -> IO bufType
readBlocks handle n = do
    x <- readBlock handle n
    case x of
        Left err -> error $ "Connection to SparQ broken: " ++ (show err)
        Right y  -> return y

readOneLine :: HandleStream String -> IO String
readOneLine handle = do
    x <- readLine handle
    case x of
        Left err -> error $ "Connection to SparQ broken: " ++ (show err)
        Right y  -> return $ init y


readToPrompt :: HandleStream String -> IO String
readToPrompt sparq = do
    x <- readBlocks sparq 1
    if x == ">" then do
        y <- readBlocks sparq 1
        return (x ++ y)
    else do
        y <- readToPrompt sparq
        return (x ++ y)


{------------------------------------------------------------------------------
 - Use the SparQ-Server to calculate A-Closure and such things
------------------------------------------------------------------------------}

algebraicClosure :: (Calculus a)
                    => String
                    -> Network [String] (Set.Set a)
                    -> (Maybe Bool, Bool, Network [String] (Set.Set a))
algebraicClosure cal net = unsafePerformIO $ do
        let sparqNet = sparqify True net
        writeBlock sparq ("constraint-reasoning " ++ cal ++ " a-closure "
                           ++ sparqNet ++ "\n")
        sparqModified <- readOneLine sparq
        sparqNewNet <- readToPrompt sparq
        let (consistent, modified) = case sparqModified of
                "** Modified network."   -> (Nothing, True)
                "** Unmodified network." -> (Nothing, False)
                "** Not consistent." -> (Just False, False)
                _ -> error $ "SparQ answered in an unexpected way.\n" ++
                             "On Network:\n" ++ sparqNet ++ "\n" ++
                             "Expected: Modified network.\n" ++
                             "      OR: Unmodified Network.\n" ++
                             "      OR: Not consistent.\n" ++
                             "Actual answer: \"" ++ sparqModified ++ "\""
        let newNet = case parse parseNetwork "" sparqNewNet of
               Left err -> error $ "SparQ answered in an unexpected way.\n" ++
                                   "On Network:\n" ++ sparqNet ++ "\n" ++
                                   "Expected: a SparQ network definition.\n" ++
                                   "Actual answer: " ++ sparqNewNet
               Right success -> success
        if Map.null $ nCons net then
            return (Just True, False, net)
        else case modified of
            False -> return (consistent, modified, net)
            True  -> return (consistent, modified, net {nCons = nCons newNet})
{-# NOINLINE algebraicClosure #-}

ternaryAlgebraicClosure :: (Calculus a)
                        => String
                        -> Network [String] (Set.Set a)
                        -> (Maybe Bool, Bool, Network [String] (Set.Set a))
ternaryAlgebraicClosure cal net = unsafePerformIO $ do
        let sparqNet = sparqify True net
        writeBlock sparq ("constraint-reasoning " ++ cal ++ " ternary-closure "
                           ++ sparqNet ++ "\n")
        sparqModified <- readOneLine sparq
        sparqNewNet <- readToPrompt sparq
        let (consistent, modified) = case sparqModified of
                "** Modified network."   -> (Nothing, True)
                "** Unmodified network." -> (Nothing, False)
                "** Not consistent." -> (Just False, False)
                _ -> error $ "SparQ answered in an unexpected way.\n" ++
                             "On Network:\n" ++ sparqNet ++ "\n" ++
                             "Expected: Modified network.\n" ++
                             "      OR: Unmodified Network.\n" ++
                             "      OR: Not consistent.\n" ++
                             "Actual answer: \"" ++ sparqModified ++ "\""
        let newNet = case parse parseNetwork "" sparqNewNet of
               Left err -> error $ "SparQ answered in an unexpected way.\n" ++
                                   "On Network:\n" ++ sparqNet ++ "\n" ++
                                   "Expected: a SparQ network definition.\n" ++
                                   "Actual answer: " ++ sparqNewNet
               Right success -> success
        if Map.null $ nCons net then
            return (Just True, False, net)
        else case modified of
            False -> return (consistent, modified, net)
            True  -> return (consistent, modified, net {nCons = nCons newNet})
{-# NOINLINE ternaryAlgebraicClosure #-}

algebraicReasoning :: (Calculus a)
                   => String
                   -> Network [String] (Set.Set a)
                   -> Maybe Bool
algebraicReasoning cal net = unsafePerformIO $ do
        let sparqNet = sparqify True net
        writeBlock sparq $ "a-reasoning " ++ cal ++ " consistency " ++ sparqNet ++ "\n"
        -- for some reason we get a second prompt from SparQ. Eat it!
        answer <- readToPrompt sparq
        if isInfixOf "IS SATISFIABLE." answer then do
--            print "sparq answered True.\n"
            return $ Just True
        else if isInfixOf "NOT SATISFIABLE." answer then do
--            print "sparq answered False.\n"
            return $ Just False
        else if isInfixOf "CANNOT DECIDE." answer then do
--            print "sparq can't decided.\n"
            return Nothing
        else do error ("SparQ answered " ++ show answer ++ " on network "
                                                        ++ sparqNet)
{-# NOINLINE algebraicReasoning #-}

