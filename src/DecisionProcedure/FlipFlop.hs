module DecisionProcedure.FlipFlop where

-- standard modules
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.FlipFlop
import qualified Interface.Sparq as S
import qualified Interface.Triangle as T
import DecisionProcedure

str = "ff"

toAtomic (x,fun) = (x, fun . makeAtomic)

instance HasDecisionProcedure FlipFlop where
    proceduresForAtomicNets _ =
        [ algebraicClosure str
        , ternaryAlgebraicClosure str
        , algebraicReasoning str
        , toAtomic triangleConsistency
        , toAtomic chirotopeSloppy
        , toAtomic biquadraticFinalPolynomialsSloppy
        , toAtomic chirotope
        , toAtomic biquadraticFinalPolynomials
        ]

    proceduresForNonAtomicNets _ =
        [ algebraicClosure str
        , ternaryAlgebraicClosure str
        , algebraicReasoning str
        ]
