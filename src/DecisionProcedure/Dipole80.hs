module DecisionProcedure.Dipole72 () where

-- standard modules
import Data.Maybe
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Dipole80
import DecisionProcedure

str = "dra-80"

instance HasDecisionProcedure Dipole72 where
    proceduresForAtomicNets _ =
        [ after makeNonAtomic (algebraicClosure str)
        , after makeNonAtomic (algebraicReasoning str)
        ]

    proceduresForNonAtomicNets _ =
        [ algebraicClosure str
        , algebraicReasoning str
        ]
