module Interface.LpSolve where

-- standard modules
import System.Process
import System.IO.Unsafe

-- local modules

--import Data.Time.Clock (diffUTCTime, getCurrentTime)
--import Debug.Trace

zeroObjective :: String -> Maybe Bool
zeroObjective p = unsafePerformIO $ do
--    start <- getCurrentTime
    (_, clpAnswer, _) <- readProcessWithExitCode "lp_solve" [] p
    let answer = lines clpAnswer
--    end <- getCurrentTime
--    print $ (show (end `diffUTCTime` start) ++ " elapsed. ") ++ clpAnswer
    case head answer of
        "This problem is infeasible" -> return Nothing
        "This problem is unbounded" -> return $ Just False
        otherwise -> do
            let (chattering, value) = splitAt 29 (answer!!1)
            if chattering == "Value of objective function: " then
                if (read value :: Float) == 0.0 then
                    return $ Just True
                else
                    return $ Just False
            else
                error $ "lp_solve answered in an unexpected way.\n\
                   \Expected Answer: \"Value of objective function: NUMBER\"\n\
                   \Actual Answer: " ++ clpAnswer
{-# NOINLINE zeroObjective #-}

