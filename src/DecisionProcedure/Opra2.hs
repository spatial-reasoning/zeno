module DecisionProcedure.Opra2
    ( module DecisionProcedure.Opra
    ) where

-- standard modules
import Data.Maybe
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Opra2
import DecisionProcedure
import DecisionProcedure.Opra

str = "opra2"

instance HasDecisionProcedure Opra2 where
    proceduresForAtomicNets _ =
        [ after makeNonAtomic (algebraicClosureGQR str)
--        , after makeNonAtomic (algebraicReasoning str)
        ] ++ map (after opramNetToOpraNetAtomic)
                 (proceduresForAtomicNets (undefined :: Opra))

    proceduresForNonAtomicNets _ =
        [ algebraicClosure str
        ]
