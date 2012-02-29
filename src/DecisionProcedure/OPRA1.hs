module DecisionProcedure.OPRA1 () where

-- standard modules
import Data.Maybe
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.OPRA1
import Calculus.OPRA1.ToFlipFlop
import DecisionProcedure

str = "opra-1"

toAtomicFlipFlops (a,fun) =
    ( a
    , fun . makeAtomic . opra1ToFlipFlopNet
    )

instance HasDecisionProcedure OPRA1 where
    proceduresForAtomicNets _ =
        [ algebraicClosure str
        , algebraicReasoning str
        , toAtomicFlipFlops triangleConsistency
        , toAtomicFlipFlops chirotopeSloppy
        , toAtomicFlipFlops biquadraticFinalPolynomialsSloppy
        , toAtomicFlipFlops chirotope
        , toAtomicFlipFlops biquadraticFinalPolynomials
        ]

    proceduresForNonAtomicNets _ =
        [ algebraicClosure str
        , algebraicReasoning str
        ]
