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

import Debug.Trace


isAcyclicChirotopeFlipFlop :: Bool -> Network [String] FlipFlop -> Maybe Bool
isAcyclicChirotopeFlipFlop sloppy net
--    | (isJust chiroNet &&) $ head $ isAcyclicChirotope      -- TWOINONE
    | (isJust chiroNet &&) $ isAcyclicChirotope
        (nCons $ fromJust chiroNet) (\_ _ _ _ -> True) sloppy =
-- fixme: we get some false positives! Why?
--        if (numberOfNodes (fromJust chiroNet) < 9) && (not sloppy) then
        if (numberOfNodes (fromJust chiroNet) < 9) && False then
            Just True
        else
            Nothing
    | otherwise  = Just False
  where
    chiroNet = flipflop7ToChirotope net


isAcyclicChirotopeWithoutBPFlipFlop :: Bool -> Network [String] FlipFlop -> Maybe Bool
isAcyclicChirotopeWithoutBPFlipFlop sloppy net
--    | (isJust chiroNet &&) $ last $ isAcyclicChirotope          -- TWOINONE
    | (isJust chiroNet &&) $ isAcyclicChirotope
        (nCons $ fromJust chiroNet) hasNoBiquadraticFinalPolynomial sloppy =
-- fixme: we get some false positives! Why?
--        if (numberOfNodes (fromJust chiroNet) < 9) && (not sloppy) then
        if (numberOfNodes (fromJust chiroNet) < 9) && False then
            Just True
        else
            Nothing
    | otherwise  = Just False
  where
    chiroNet = flipflop7ToChirotope net

