module DecisionProcedure.Dipole72.OrientedMatroid where

-- standard modules
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

-- local modules
import Basics
import Calculus.Dipole72
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

