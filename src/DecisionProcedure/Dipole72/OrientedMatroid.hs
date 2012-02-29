module DecisionProcedure.Dipole.OrientedMatroid where

-- standard modules
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

-- local modules
import Basics
import Calculus.Dipole
import DecisionProcedure.FlipFlop.OrientedMatroid
import SpatioTemporalStructure.OrientedMatroid
import Convert

import Debug.Trace


isAcyclicChirotopeDipole72 :: Bool -> Network [String] Dipole72 -> Maybe Bool
isAcyclicChirotopeDipole72 sloppy net
    | isNothing ffNet  = Nothing
    | otherwise  = isAcyclicChirotopeFlipFlop sloppy $ fromJust ffNet
  where
    ffNet = dipolesToFlipFlops net


isAcyclicChirotopeWithoutBPDipole72 :: Bool
                                    -> Network [String] Dipole72
                                    -> Maybe Bool
isAcyclicChirotopeWithoutBPDipole72 sloppy net
    | isNothing ffNet  = Nothing
    | otherwise  = isAcyclicChirotopeWithoutBPFlipFlop sloppy $ fromJust ffNet
  where
    ffNet = dipolesToFlipFlops net

