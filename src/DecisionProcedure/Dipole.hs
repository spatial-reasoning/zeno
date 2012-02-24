module DecisionProcedure.Dipole where

-- standard modules
import Data.Maybe
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Dipole
import Convert
import DecisionProcedure

str72 = "dra-72"

toAtomicFlipFlops (a,fun) = (a, newFun)
  where
    newFun net = case dpNet net of
       Just x  -> fun x
       Nothing -> Just False
    dpNet net = dipolesToFlipFlops $ makeAtomic net

instance HasDecisionProcedure Dipole72 where
    proceduresForAtomicNets _ =
{- fixme: Sparq seems to be broken for Dipoles.
        [ algebraicClosure str72
        , algebraicReasoning str72
        , toAtomicFlipFlops triangleConsistency
        , toAtomicFlipFlops chirotopeSloppy
        , toAtomicFlipFlops biquadraticFinalPolynomialsSloppy
        , toAtomicFlipFlops chirotope
        , toAtomicFlipFlops biquadraticFinalPolynomials
        ]
-}
        [ toAtomicFlipFlops triangleConsistency ]

    proceduresForNonAtomicNets _ =
        [ algebraicClosure str72
        , algebraicReasoning str72
        ]
