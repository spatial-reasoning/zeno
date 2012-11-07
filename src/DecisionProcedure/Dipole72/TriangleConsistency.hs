module DecisionProcedure.Dipole72.TriangleConsistency where

-- standard modules
import Data.Maybe

-- local modules
import Basics
import Calculus.Dipole72
import DecisionProcedure.FlipFlop.OrientedMatroid
import SpatioTemporalStructure.OrientedMatroid
import Convert.LRDipole

checkConsistencyDipole72 :: Network [String] Dipole72
                         -> Maybe Bool
checkConsistencyDipole72 net
    | isNothing ffNet  = Nothing
    | otherwise  = checkConsistency $ fromJust ffNet
  where
    ffNet = dipolesToFlipFlops net

