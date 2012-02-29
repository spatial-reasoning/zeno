module DecisionProcedure.OPRA2 () where

-- standard modules
import Data.Maybe
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.OPRA2
import DecisionProcedure

str = "opra-2"

--toAtomicFlipFlops (a,fun) =
--    ( a
--    , fun . makeAtomic . opra1ToFlipFlopNet
--    )

instance HasDecisionProcedure OPRA1 where
    proceduresForAtomicNets _ =
        [ algebraicClosure str
        , algebraicReasoning str
        ]

    proceduresForNonAtomicNets _ =
        [ algebraicClosure str
        , algebraicReasoning str
        ]
