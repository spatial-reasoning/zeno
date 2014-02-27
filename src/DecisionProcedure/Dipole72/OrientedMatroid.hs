module DecisionProcedure.Dipole72.OrientedMatroid where

-- standard modules
import Data.Maybe

-- local modules
import Basics
import Calculus.Dipole72
import DecisionProcedure.FlipFlop.OrientedMatroid
import SpatioTemporalStructure.OrientedMatroid
import Convert.LRDipole

--import Debug.Trace


isAcyclicChirotopeDipole72 :: Network [String] (ARel Dipole72)
                           -> Maybe Bool
isAcyclicChirotopeDipole72 net =
    isAcyclicChirotopeFlipFlop $ dipolesToFlipFlops net


isAcyclicChirotopeWithoutBPDipole72 :: Network [String] (ARel Dipole72)
                                    -> Maybe Bool
isAcyclicChirotopeWithoutBPDipole72 net =
    isAcyclicChirotopeWithoutBPFlipFlop $ dipolesToFlipFlops net

{-       TWOINONE
isAcyclicChirotopePlainAndWithoutBPFlipFlop :: Network [String] FlipFlop -> [Maybe Bool]
isAcyclicChirotopePlainAndWithoutBPFlipFlop net
    | isNothing chiroNet  = [Just False, Just False]
    | numberOfNodes (fromJust chiroNet) < 9  = map Just answers
    | otherwise  = map trueToNothing answers
  where
    chiroNet = flipflop7ToChirotope net
    answers = isAcyclicChirotope
        (nCons $ fromJust chiroNet)
        hasNoBiquadraticFinalPolynomial
    trueToNothing True = Nothing
    trueToNothing False = Just False

isAcyclicChirotopePlainAndWithoutBPDipole72 :: Network [String] Dipole72 -> [Maybe Bool]
isAcyclicChirotopePlainAndWithoutBPDipole72 net
    | isNothing ffNet  = [Nothing, Nothing]
    | otherwise  = isAcyclicChirotopePlainAndWithoutBPFlipFlop $ fromJust ffNet
  where
    ffNet = dipolesToFlipFlops net
-}

