{-# LANGUAGE FlexibleContexts #-}
module Interface.Gqr where

-- standard modules
import qualified Data.Set as Set
import System.IO
import System.IO.Unsafe
import Text.Parsec

-- local modules
import Basics
import Export
import Parsing.Gqr
import Helpful.Directory
import Helpful.Process

--import Debug.Trace


algebraicClosure :: ( Relation (a b) b
                    , Gqrifiable (Network [String] (a b))
                    , Calculus b )
                 => Network [String] (a b)
                 -> (Maybe Bool, Network [String] (GRel b))
algebraicClosure net = unsafePerformIO $
  withTempDir "Qstrlib_qgr" (\tmpDir -> do
    gqrTempFile <- openTempFile tmpDir "gqrTempFile.csp"
    let (gqrNet, enumeration) = gqrify net
    hPutStr (snd gqrTempFile) gqrNet
    hClose $ snd gqrTempFile
    gqrAnswer <- safeReadProcess
        "gqr" (["c -C", cNameGqr ((undefined :: Network [String] (a b) -> b) net), "-S", fst gqrTempFile]) ""
    let (fstline, _:gqrNewNet) = break (== '\n') $ dropWhile (/= '#') gqrAnswer
    let consistent = zeroOne $ last fstline
          where
            zeroOne x
                | x == '0'  = False
                | x == '1'  = True
                | otherwise = error ("GQR answered in an unexpected way.\n\
                                     \Expected answer: Gqr information on \
                                     \consistency of a network.\n\
                                     \Actual answer  : " ++ gqrAnswer)
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
        return (Just False, makeNonAtomic net)
  )

algebraicClosures :: ( Relation (a b) b
                     , Gqrifiable (Network [String] (a b))
                     , Calculus b )
                  => [Network [String] (a b)]
                  -> [Maybe Bool]
algebraicClosures nets = unsafePerformIO $
  withTempDir "Qstrlib-" (\tmpDir -> do
    gqrTempFiles <- mapM (\x -> openTempFile tmpDir "gqrTempFile-.csp") nets
    mapM_ (\ (x,y) -> hPutStr (snd x) (fst $ gqrify y)) (zip gqrTempFiles nets)
    mapM_ (hClose . snd) gqrTempFiles
    gqrAnswer <- safeReadProcess
        "gqr" (["c -C", cNameGqr((undefined :: [Network [String] (a b)] -> b) nets)] ++ (map fst gqrTempFiles)) ""
    let answer = map zeroOne [ last x | x <- lines gqrAnswer, head x == '#' ]
          where
            zeroOne x
                | x == '0'  = Just False
                | x == '1'  = Nothing
                | otherwise = error ("GQR answered in an unexpected way.\n\
                                     \Expected answer: Gqr information on \
                                     \consistency of a network.\n\
                                     \Actual answer  : " ++ gqrAnswer)
    return answer
  )

