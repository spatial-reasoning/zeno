module DecisionProcedure.Dipole72.TriangleConsistency where

-- standard modules
import Data.Maybe

-- local modules
import Basics
import Calculus.Dipole72
import qualified DecisionProcedure.FlipFlop.TriangleConsistency as FFT
import Convert.LRDipole

checkConsistency :: Network [String] (ARel Dipole72)
                         -> Maybe Bool
checkConsistency net = FFT.checkConsistency $ dipolesToFlipFlops net

