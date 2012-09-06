module Interface.Triangle where

-- standard modules
import Data.Maybe
import System.IO.Unsafe

-- local modules
import Basics
--import Calculus.Dipole
import Calculus.Dipole72
import Calculus.FlipFlop
import Convert
import qualified DecisionProcedure.FlipFlop.TriangleConsistency as T

--import Debug.Trace


checkConsistency :: Network [String] FlipFlop
                 -> Maybe Bool
checkConsistency net =
    ffsToFF5s net >>= (T.runTCpure . flipFlopsToDominik)


checkConsistencyDipole72 :: Network [String] Dipole72
                         -> Maybe Bool
checkConsistencyDipole72 net
    | isNothing ffNet  = Nothing
    | otherwise  = checkConsistency $ fromJust ffNet
  where
    ffNet = dipolesToFlipFlops net

