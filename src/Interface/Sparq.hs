{-# LANGUAGE FlexibleContexts #-}
module Interface.Sparq where

-- standard modules
import Control.Exception
import Control.Monad
import qualified Data.Maybe as Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO
import System.IO.Unsafe
import System.Process
--import Network.TCP
import Text.Parsec

-- local modules
import Basics
import Parsing.Sparq
import Export
import Helpful.Process

-- Debugging and Timing
--import Data.Time.Clock (diffUTCTime, getCurrentTime)
--import Debug.Trace
import Helpful.General



{------------------------------------------------------------------------------
 - Interface to SparQ standalone
------------------------------------------------------------------------------}

doInSparq f = unsafePerformIO $ bracket
    (do runInteractiveCommand "sparq -i 2> /dev/null")
    (\ (hIn, _, _, hProc) -> do
        hPutStrLn hIn "quit"
--        terminateProcess hProc
--        waitForProcess hProc
--        getProcessExitCode hProc
    )
    (\ sparq@(hIn, hOut, hErr, hId) -> do
        mapM_ (flip hSetBinaryMode False) [hIn, hOut]
        hSetBuffering hIn LineBuffering
        hSetBuffering hOut NoBuffering
        readToPrompt hOut
        f sparq
    )

readToPrompt :: Handle -> IO String
readToPrompt hOut = do
    sparqResponds <- hWaitForInput hOut (100000)
    if sparqResponds then do
        x <- hGetChar hOut
        if x == 's' then do
            x <- hGetChar hOut
            if x == 'p' then do
                x <- hGetChar hOut
                if x == 'a' then do
                    x <- hGetChar hOut
                    if x == 'r' then do
                        x <- hGetChar hOut
                        if x == 'q' then do
                            x <- hGetChar hOut
                            if x == '>' then do
                                x <- hGetChar hOut
                                if x == ' ' then do
                                    return "sparq> "
                                else do
                                    y <- readToPrompt hOut
                                    return ("sparq>" ++ (x:y))
                            else do
                                y <- readToPrompt hOut
                                return ("sparq" ++ (x:y))
                        else do
                            y <- readToPrompt hOut
                            return ("spar" ++ (x:y))
                    else do
                        y <- readToPrompt hOut
                        return ("spa" ++ (x:y))
                else do
                    y <- readToPrompt hOut
                    return ("sp" ++ (x:y))
            else do
                y <- readToPrompt hOut
                return ("s" ++ (x:y))
        else do
            y <- readToPrompt hOut
            return (x:y)
    else error "\nSparQ doesn't respond anymore!\n"

readOneLine hOut = do
    -- Fixme: make this more useful for us, e.g. as a timeout!
    -- 30 min = 1800000 milliseconds
    sparqResponds <- hWaitForInput hOut (1800000)
    if sparqResponds then do
        x <- hGetLine hOut
        return x
    else error "\nSparQ doesn't respond anymore!\n"

{------------------------------------------------------------------------------
 - Use SparQ standalone to calculate A-Closure and such things
------------------------------------------------------------------------------}

algebraicClosure :: ( Relation (a b) b
                    , Sparqifiable (Network [String] (a b))
                    , Calculus b )
                 => Network [String] (a b)
                 -> (Maybe Bool, Bool, Network [String] (GRel b))
algebraicClosure net =
    if Map.null $ nCons net then
        (Just True, False, makeNonAtomic net)
    else case modified of
        False -> (consistent, modified, makeNonAtomic net)
        True  -> (consistent, modified, net {nCons = nCons newNet})
  where
    sparqNet = sparqify True net
    sparqModified:rest = lines $ unsafeReadProcess "sparq"
        ["constraint-reasoning " ++
         cNameSparq ((undefined :: Network [String] (a b) -> b) net) ++
         " a-closure " ++ sparqNet] ""
    (consistent, modified) = case sparqModified of
        "Modified network."   -> (Nothing, True)
        "Unmodified network." -> (Nothing, False)
        "Not consistent." -> (Just False, False)
        _ -> error $ "SparQ answered in an unexpected way.\n" ++
                     "On Network:\n" ++ sparqNet ++ "\n" ++
                     "Expected: Modified network.\n" ++
                     "      OR: Unmodified Network.\n" ++
                     "      OR: Not consistent.\n" ++
                     "Actual answer: \"" ++ sparqModified
                                         ++ unlines rest ++ "\""
    newNet = case parse parseNetwork "" (unlines rest) of
        Left err -> error $ "SparQ answered in an unexpected way.\n" ++
                            "On Network:\n" ++ sparqNet ++ "\n" ++
                            "Expected: a SparQ network definition.\n" ++
                            "Actual answer: " ++ unlines rest
        Right success -> success

ternaryAlgebraicClosure :: ( Relation (a b) b
                           , Sparqifiable (Network [String] (a b))
                           , Calculus b )
                        => Network [String] (a b)
                        -> (Maybe Bool, Bool, Network [String] (GRel b))
ternaryAlgebraicClosure net =
    if Map.null $ nCons net then
        (Just True, False, makeNonAtomic net)
    else case modified of
        False -> (consistent, modified, makeNonAtomic net)
        True  -> (consistent, modified, net {nCons = nCons newNet})
  where
    sparqNet = sparqify True net
    sparqModified:rest = lines $ unsafeReadProcess "sparq"
        ["constraint-reasoning " ++
         cNameSparq ((undefined :: Network [String] (a b) -> b) net) ++
         " ternary-closure " ++ sparqNet] ""
    (consistent, modified) = case sparqModified of
        "Modified network."   -> (Nothing, True)
        "Unmodified network." -> (Nothing, False)
        "Not consistent." -> (Just False, False)
        _ -> error $ "SparQ answered in an unexpected way.\n" ++
                     "On Network:\n" ++ sparqNet ++ "\n" ++
                     "Expected: Modified network.\n" ++
                     "      OR: Unmodified Network.\n" ++
                     "      OR: Not consistent.\n" ++
                     "Actual answer: \"" ++ sparqModified
                                         ++ unlines rest ++ "\""
    newNet = case parse parseNetwork "" (unlines rest) of
        Left err -> error $ "SparQ answered in an unexpected way.\n" ++
                            "On Network:\n" ++ sparqNet ++ "\n" ++
                            "Expected: a SparQ network definition.\n" ++
                            "Actual answer: " ++ unlines rest
        Right success -> success


algebraicReasoning :: ( Relation (a b) b
                      , Sparqifiable (Network [String] (a b))
                      , Calculus b )
                   => Network [String] (a b)
                   -> Maybe Bool
algebraicReasoning net =
    if Map.null $ nCons net then do
        Just True
    else case answer of
        "IS SATISFIABLE."  -> Just True
        "NOT SATISFIABLE." -> Just False
        "CANNOT DECIDE."   -> Nothing
        otherwise          -> error ( "SparQ answered " ++
                                      show answer ++ " on network " ++
                                      sparqNet )
  where
    sparqNet = sparqify True net
    answer = case answer' of
        []  -> error $ "Sparq gives no answer on \"algebraicReasoning\" over network:\n\n" ++ sparqNet
        x   -> head x
    answer' = lines $ unsafeReadProcess "sparq"
        ["a-reasoning " ++
         cNameSparq ((undefined :: Network [String] (a b) -> b) net) ++
         " consistency " ++ sparqNet] ""

