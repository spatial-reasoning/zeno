module DecisionProcedure.Opra1 where

-- standard modules
import Data.Maybe
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Opra1
import Calculus.Opra1.ToFlipFlop
import DecisionProcedure

str = "opra1"

-- fixme: make this nonatomic from the beginning.
toFlipFlopsAtomic (a,fun) =
    ( a
    , fun . makeAtomic . opra1ToFlipFlopNet . makeNonAtomic
    )

toFlipFlopsNonAtomic (a,fun) =
    ( a
    , fun . opra1ToFlipFlopNet . makeNonAtomic
    )

instance HasDecisionProcedure Opra1 where
    proceduresForAtomicNets _ =
        [ after makeNonAtomic (algebraicClosureGQR str)
        , toFlipFlopsNonAtomic (algebraicReasoning "ff")
        , toFlipFlopsAtomic triangleConsistencyFlipFlop
        , toFlipFlopsAtomic chirotope
        , toFlipFlopsAtomic biquadraticFinalPolynomials
        ]

    proceduresForNonAtomicNets _ =
        [ algebraicClosure str
        ]
