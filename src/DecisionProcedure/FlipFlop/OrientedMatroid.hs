module DecisionProcedure.FlipFlop.OrientedMatroid where

-- standard modules
import Data.Maybe

-- local modules
import Basics
import Calculus.FlipFlop
import SpatioTemporalStructure.OrientedMatroid
import Convert.LRChirotope


isAcyclicChirotopeFlipFlop :: Network [String] (ARel FlipFlop) -> Maybe Bool
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


isAcyclicChirotopeWithoutBPFlipFlop :: Network [String] (ARel FlipFlop) -> Maybe Bool
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

