module Interface.Triangle where

-- standard modules
import Data.Maybe
import System.IO.Unsafe

-- local modules
import Basics
--import Calculus.Dipole
import Calculus.FlipFlop
import Convert
import qualified DecisionProcedure.FlipFlop.TriangleConsistency as T

--import Debug.Trace


checkConsistency :: Network [String] FlipFlop
                 -> Maybe Bool
checkConsistency net7
    | isNothing net5  = Just False
    | otherwise  =  T.runTCpure $ flipFlop5sToDominik $ fromJust net5
    where
        net5 = ffsToFF5s net7

{-
checkConsistencyDipole72 :: Network [String] Dipole72
                         -> Maybe Bool
checkConsistencyDipole72 net
    | isNothing ffNet  = Nothing
    | otherwise  = checkConsistency $ fromJust ffNet
  where
    ffNet = dipolesToFlipFlops net
-}
