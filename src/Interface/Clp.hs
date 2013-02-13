module Interface.Clp where

-- standard modules
import Data.List
import Data.Maybe
import System.IO
import System.IO.Unsafe
--import System.Process

-- local modules
import Helpful.Directory
import Helpful.Process

--import Data.Time.Clock (diffUTCTime, getCurrentTime)
--import Debug.Trace

zeroObjective :: String -> Maybe Bool
zeroObjective p = unsafePerformIO $
  withTempDir "Qstrlib_clp" (\tmpDir -> do
    clpTempFile <- openTempFile tmpDir "clpTempFile.lp"
    hPutStr (snd clpTempFile) p
    hClose $ snd clpTempFile
    (clpAnswer, clpError) <- safeReadProcess "clp" [fst clpTempFile] ""
    let answer = case lines clpAnswer of
        [] -> oops
        x  -> last x
    if "PrimalInfeasible" `isPrefixOf` answer then
        return Nothing
    else if "DualInfeasible" `isPrefixOf` answer then
        return $ Just False
    else do
        let value = stripPrefix "Optimal objective " answer
        if isNothing value then oops
        else if (read $ takeWhile (/= ' ') $ fromJust value :: Float) == 0.0 then
            return $ Just True
        else
            return $ Just False
  )
 where
  oops = error $ "clp answered in an unexpected way.\n\
                 \Expected Answer: \"Value of objective function: NUMBER\"\n\
                 \Actual Answer: " ++ clpAnswer ++ "\n" ++ clpError
{-# NOINLINE zeroObjective #-}

