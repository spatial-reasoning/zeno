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
import Debug.Trace



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

algebraicClosure :: (Calculus a)
                 => String
                 -> Network [String] (Set.Set a)
                 -> (Maybe Bool, Bool, Network [String] (Set.Set a))
algebraicClosure cal net =
    if Map.null $ nCons net then
        (Just True, False, net)
    else case modified of
        False -> (consistent, modified, net)
        True  -> (consistent, modified, net {nCons = nCons newNet})
  where
    sparqNet = sparqify True net
    sparqModified:rest = lines $ unsafeReadProcess "sparq"
        ["constraint-reasoning " ++ cal ++ " a-closure " ++ sparqNet] ""
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

ternaryAlgebraicClosure :: (Calculus a)
                        => String
                        -> Network [String] (Set.Set a)
                        -> (Maybe Bool, Bool, Network [String] (Set.Set a))
ternaryAlgebraicClosure cal net =
    if Map.null $ nCons net then
        (Just True, False, net)
    else case modified of
        False -> (consistent, modified, net)
        True  -> (consistent, modified, net {nCons = nCons newNet})
  where
    sparqNet = sparqify True net
    sparqModified:rest = lines $ unsafeReadProcess "sparq"
        ["constraint-reasoning " ++ cal ++ " ternary-closure " ++ sparqNet] ""
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


algebraicReasoning :: (Calculus a)
                   => String
                   -> Network [String] (Set.Set a)
                   -> Maybe Bool
algebraicReasoning cal net =
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
    answer = head $ lines $ unsafeReadProcess "sparq"
        ["a-reasoning " ++ cal ++ " consistency " ++ sparqNet] ""

{-- This needs to be rewritten using "unsafeReadProcess" ----------------------
algebraicClosures :: (Calculus a)
                  => String
                  -> [Network [String] (Set.Set a)]
                  -> [Maybe Bool]
algebraicClosures cal nets = doInSparq $
    (\ (hIn, hOut, hErr, hId) -> do
        hPutStrLn hIn ("load-calculus " ++ cal)
        readToPrompt hOut
        mapM (\ net ->
            if Map.null $ nCons net then do
                return $ Just True
            else do
                hPutStrLn hIn ("constraint-reasoning * a-closure " ++ sparqify True net)
                sparqModified <- readOneLine hOut
                readToPrompt hOut
                let consistent = case sparqModified of
                        "Modified network."   -> Nothing
                        "Unmodified network." -> Nothing
                        "Not consistent." -> Just False
                        _ -> error $ "SparQ answered in an unexpected way.\n\
                                     \Expected: Modified network.\n\
                                     \      OR: Unmodified Network.\n\
                                     \      OR: Not consistent.\n\
                                     \Actual answer: \"" ++ sparqModified ++ "\""
                return consistent
                ) nets
    )
{-# NOINLINE algebraicClosures #-}


algebraicReasonings :: (Calculus a)
                    => String
                    -> [Network [String] (Set.Set a)]
                    -> [Maybe Bool]
algebraicReasonings cal nets = doInSparq $
    (\ (hIn, hOut, hErr, hId) -> do
        hPutStrLn hIn ("load-calculus " ++ cal)
        readToPrompt hOut
        mapM (\ net ->
            if Map.null $ nCons net then do
                return $ Just True
            else do
                let sparqNet = sparqify True net
--                start <- getCurrentTime
                hPutStrLn hIn ("a-reasoning * consistency " ++ sparqNet)
                answer <- readToPrompt hOut
--                end <- getCurrentTime
--                print $ show (end `diffUTCTime` start) ++ " elapsed."
                if isInfixOf "IS SATISFIABLE." answer then do
                    return $ Just True
                else if isInfixOf "NOT SATISFIABLE." answer then do
                    return $ Just False
                else if isInfixOf "CANNOT DECIDE." answer then do
                    return Nothing
                else do error ("SparQ answered " ++ show answer ++ " on network "
                                                                ++ sparqNet)
                ) nets
    )
{-# NOINLINE algebraicReasonings #-}
------------------------------------------------------------------------------}

{-- Server version (is not well suited for our purpose) -----------------------

{------------------------------------------------------------------------------
 - Interface to SparQ server
------------------------------------------------------------------------------}

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

{-# NOINLINE algebraicClosure #-}
algebraicClosure :: (Calculus a)
                    => String
                    -> Network [String] (Set.Set a)
                    -> (Maybe Bool, Network [String] (Set.Set a))
algebraicClosure cal net = unsafePerformIO $ bracket
    (do connectToSparq)
    (close)
    (\ sparq -> do
        let sparqNet = sparqify True net
        writeBlock sparq ("constraint-reasoning " ++ cal ++ " a-closure "
                                                  ++ sparqNet ++ "\n")
        sparqModified <- readOneLine sparq
        sparqNewNet <- readOneLine sparq
        close sparq
        let modified = case sparqModified of
                "Modified network."   -> Just True
                "Unmodified network." -> Just False
                "Not consistent." -> Nothing
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
        case modified of
            Nothing    -> return (modified, net)
            Just False -> return (modified, net)
            Just True  -> return (modified, net {nCons = nCons newNet})
    )

{-# NOINLINE algebraicReasoning #-}
algebraicReasoning :: (Calculus a)
                   => String
                   -> Network [String] (Set.Set a)
                   -> Maybe Bool
algebraicReasoning cal net = unsafePerformIO $ bracket
    (do connectToSparq)
    (close)
    (\ sparq -> do
        let sparqNet = sparqify True net
        writeBlock sparq $ "a-reasoning " ++ cal ++ " consistency " ++ sparqNet ++ "\n"
        -- for some reason we get a second prompt from SparQ. Eat it!
        answer <- trace "reading from sparq.\n" $ readToPrompt sparq
        if isInfixOf "IS SATISFIABLE." answer then do
            print "sparq answered True.\n"
            return $ Just True
        else if isInfixOf "NOT SATISFIABLE." answer then do
            print "sparq answered False.\n"
            return $ Just False
        else if isInfixOf "CANNOT DECIDE." answer then do
            print "sparq can't decided.\n"
            return Nothing
        else do error ("SparQ answered " ++ show answer ++ " on network "
                                                        ++ sparqNet)
    )

------------------------------------------------------------------------------}
