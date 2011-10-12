module Interface.Gqr where

-- standard modules
import qualified Data.Set as Set
import System.IO
import System.IO.Unsafe
import System.Process
import System.Unix.Directory

-- local modules
import Basics
import Export

--import Debug.Trace


algebraicClosure :: (Calculus a)
                 => String
                 -> [Network [String] (Set.Set a)]
                 -> [Maybe Bool]
algebraicClosure cal nets = unsafePerformIO $
  withTemporaryDirectory "Qstrlib-" (\tmpDir -> do
    gqrTempFiles <- mapM (\x -> openTempFile tmpDir "gqrTempFile-.csp") nets
    mapM_ (\ (x,y) -> hPutStr (snd x) (gqrify y)) (zip gqrTempFiles nets)
    mapM_ (hClose . snd) gqrTempFiles
    gqrAnswer <- readProcess "gqr" (["c -C", cal] ++ (map fst gqrTempFiles)) ""
    let answer = map zeroOne [ last x | x <- lines gqrAnswer, head x == '#' ]
    return answer
  )
  where
      zeroOne x
          | x == '0'  = Just False
          | x == '1'  = Nothing
          | otherwise = error ("GQR failed")

