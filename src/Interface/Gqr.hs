module Interface.Gqr where

-- standard modules
import qualified Data.Set as Set
import System.IO
import System.IO.Unsafe
import System.Process
import System.Unix.Directory
import Text.Parsec

-- local modules
import Basics
import Export
import Parsing.Gqr

--import Debug.Trace


algebraicClosure :: (Calculus a)
                 => String
                 -> Network [String] (Set.Set a)
                 -> (Maybe Bool, Network [String] (Set.Set a))
algebraicClosure cal net = unsafePerformIO $
  withTemporaryDirectory "Qstrlib-" (\tmpDir -> do
    gqrTempFile <- openTempFile tmpDir "gqrTempFile.csp"
    let (gqrNet, enumeration) = gqrify net
    hPutStr (snd gqrTempFile) gqrNet
    hClose $ snd gqrTempFile
    gqrAnswer <- readProcess "gqr" (["c -C", cal, fst gqrTempFile]) ""
    let (fstline, gqrNewNet) = break (== '\n') $ dropWhile (/= '#') gqrAnswer
    let consistent = zeroOne $ last $ init fstline
    let parsedNet = case parse parseNetwork "" gqrNewNet of
            Left err -> error $ "Gqr answered in an unexpected way.\n\
                                \Expected: a Gqr network definition.\n\
                                \Actual answer: " ++ gqrNewNet
            Right success -> success
    let newNet = net { nCons = unenumerateFromString
                                   enumeration $ nCons $ parsedNet }
    if consistent then do
        return (Nothing, newNet)
    else do
        return (Just False, net)
  )
  where
      zeroOne x
          | x == '0'  = False
          | x == '1'  = True
          | otherwise = error ("GQR failed")

algebraicClosures :: (Calculus a)
                  => String
                  -> [Network [String] (Set.Set a)]
                  -> [Maybe Bool]
algebraicClosures cal nets = unsafePerformIO $
  withTemporaryDirectory "Qstrlib-" (\tmpDir -> do
    gqrTempFiles <- mapM (\x -> openTempFile tmpDir "gqrTempFile-.csp") nets
    mapM_ (\ (x,y) -> hPutStr (snd x) (fst $ gqrify y)) (zip gqrTempFiles nets)
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

