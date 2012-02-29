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
        [ algebraicClosure str
        , algebraicReasoning str
        ]

    proceduresForNonAtomicNets _ =
        [ algebraicClosure str
        , algebraicReasoning str
        ]
