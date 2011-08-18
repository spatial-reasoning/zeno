module Interface where

import Control.Exception
import qualified Control.Monad.Parallel as CMP
import Data.List
import System.IO
import System.Process
import System.Unix.Directory
import Basics
import Export
import Helpful
import Parsing
import Debug.Trace

checkConsistency :: String
                 -> [ConstraintNetwork]
                 -> IO [(Maybe Bool, Maybe Bool)]
checkConsistency cal nets = bracket
    (do runInteractiveCommand "sparq -i 2> /dev/null")
    (\ (hIn, _, _, _) -> hPutStrLn hIn "quit" )
    (\ sparq -> do
        let (sparqIn, sparqOut, sparqErr, sparqId) = sparq
        mapM_ (flip hSetBinaryMode False) [sparqIn, sparqOut]
        hSetBuffering sparqIn LineBuffering
        hSetBuffering sparqOut NoBuffering
        waitForSparqsPrompt sparqOut
        hPutStrLn sparqIn ("load-calculus " ++ cal)
        waitForSparqsPrompt sparqOut
        forkSparqAnswers <- CMP.forkExec $ checkConsistencyWithSparq sparq nets
        forkGqrAnswers <- CMP.forkExec $ checkConsistencyWithGqr cal nets
        sparqAnswers <- forkSparqAnswers
        gqrAnswers <- forkGqrAnswers
        return $ zip sparqAnswers gqrAnswers)

waitForSparqsPrompt :: Handle -> IO String
waitForSparqsPrompt hOut = do
    sparqResponds <- hWaitForInput hOut (5000)
    if sparqResponds then do
        x <- hGetChar hOut
        if x == '>' then do
            return [x]
        else do
            y <- waitForSparqsPrompt hOut
            return (x:y)
    else error "\nSparQ doesn't respond anymore!\n"

checkConsistencyWithSparq :: (Handle, Handle, Handle, ProcessHandle)
                          -> [ConstraintNetwork]
                          -> IO [Maybe Bool]
checkConsistencyWithSparq (hIn, hOut, hErr, _) nets = mapM (\ net -> do
    let sparqNet = sparqify net
    hPutStrLn hIn ("a-reasoning * consistency" ++ sparqNet)
    waitForSparqsPrompt hOut
    -- for some reason we get a second prompt from SparQ. Eat it!
    answer <- waitForSparqsPrompt hOut
    if isInfixOf "IS SATISFIABLE." answer then
        return $ Just True
    else if isInfixOf "NOT SATISFIABLE." answer then
        return $ Just False
    else if isInfixOf "CANNOT DECIDE." answer then
        return Nothing
    else do error ("SparQ answered " ++ show answer ++ " on network "
                                                    ++ sparqNet)
    ) nets

checkConsistencyWithGqr :: String -> [ConstraintNetwork] -> IO [Maybe Bool]
checkConsistencyWithGqr cal nets = withTemporaryDirectory "Qstrlib-" (\tmpDir -> do
    gqrTempFiles <- mapM (\x -> openTempFile tmpDir "gqrTempFile-.csp") nets
    mapM_ (\ (x,y) -> hPutStr (snd x) (gqrify y)) (zip gqrTempFiles nets)
    mapM_ (hClose . snd) gqrTempFiles
    gqrAnswer <- readProcess "gqr" (["c -C", cal] ++ (map fst gqrTempFiles)) ""
    let answer = map zeroOne [ last x | x <- lines gqrAnswer, head x == '#' ]
    return answer)
    where
        zeroOne x
            | x == '0'  = Just False
            | x == '1'  = Nothing
            | otherwise = error ("GQR failed")

