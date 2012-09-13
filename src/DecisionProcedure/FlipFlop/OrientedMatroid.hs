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
import Convert


isAcyclicChirotopeFlipFlop :: Bool -> Network [String] FlipFlop -> Maybe Bool
isAcyclicChirotopeFlipFlop sloppy net
    | (isJust chiroNet &&) $ isAcyclicChirotope
        (nCons $ fromJust chiroNet) (\_ _ _ _ -> True) sloppy =
        if (numberOfNodes (nCons $ fromJust chiroNet) < 9) && (not sloppy) then
            Just True
        else
            Nothing
    | otherwise  = Just False
  where
    chiroNet = flipflopsToChirotope net


isAcyclicChirotopeWithoutBPFlipFlop :: Bool -> Network [String] FlipFlop -> Maybe Bool
isAcyclicChirotopeWithoutBPFlipFlop sloppy net
    | (isJust chiroNet &&) $ isAcyclicChirotope
        (nCons $ fromJust chiroNet) hasNoBiquadraticFinalPolynomial sloppy =
        if (numberOfNodes (nCons $ fromJust chiroNet) < 9) && (not sloppy) then
            Just True
        else
            Nothing
    | otherwise  = Just False
  where
    chiroNet = flipflopsToChirotope net

