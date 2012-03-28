module DecisionProcedure.Opra2 () where

-- standard modules
import Data.Maybe
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Opra2
import DecisionProcedure

str = "opra2"

instance HasDecisionProcedure Opra2 where
    proceduresForAtomicNets _ =
        [ after makeNonAtomic (algebraicClosureGQR str)
--        , after makeNonAtomic (algebraicReasoning str)
        ]

    proceduresForNonAtomicNets _ =
        [ algebraicClosure str
        ]
