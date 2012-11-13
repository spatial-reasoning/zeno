module DecisionProcedure.FlipFlop () where

-- standard modules
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.FlipFlop
import qualified Interface.Sparq as S
import DecisionProcedure

str = "ff"

instance HasDecisionProcedure FlipFlop where
    proceduresForAtomicNets _ =
        [ after makeNonAtomic (algebraicClosure str)
        , after makeNonAtomic (ternaryAlgebraicClosure str)
--        , after makeNonAtomic (algebraicReasoning str)
        , triangleConsistencyFlipFlop
        , chirotope
        , biquadraticFinalPolynomials
        ]

    proceduresForNonAtomicNets _ =
        [ algebraicClosure str
        , ternaryAlgebraicClosure str
        , algebraicReasoning str
        ]
