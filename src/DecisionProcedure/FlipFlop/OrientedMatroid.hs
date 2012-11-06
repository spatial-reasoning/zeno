module DecisionProcedure.FlipFlop.OrientedMatroid where

-- standard modules
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

-- local modules
import Basics
import Calculus.FlipFlop
import SpatioTemporalStructure.OrientedMatroid
--fixme: split convert into several modules!
import Convert.LRChirotope

import Helpful

isAcyclicChirotopeFlipFlop :: Network [String] FlipFlop -> Maybe Bool
isAcyclicChirotopeFlipFlop net
    | (isJust chiroNet &&) $ isAcyclicChirotope
        (nCons $ fromJust chiroNet) (\_ _ _ _ -> True) =
        if (numberOfNodes (nCons $ fromJust chiroNet) < 9) then
            Just True
        else
            Nothing
    | otherwise  = Just False
  where
    chiroNet = flipflopsToChirotope net


isAcyclicChirotopeWithoutBPFlipFlop :: Network [String] FlipFlop -> Maybe Bool
isAcyclicChirotopeWithoutBPFlipFlop net
    | (isJust chiroNet &&) $ isAcyclicChirotope
        (nCons $ fromJust chiroNet) hasNoBiquadraticFinalPolynomial =
        if (numberOfNodes (nCons $ fromJust chiroNet) < 9) then
            Just True
        else
            Nothing
    | otherwise  = Just False
  where
    chiroNet = flipflopsToChirotope net

