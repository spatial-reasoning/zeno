module DecisionProcedure.Opra8
    ( module DecisionProcedure.Opra
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra8
import DecisionProcedure
import DecisionProcedure.Opra

str = "opra8"

instance HasDecisionProcedure Opra8 where
    proceduresForAtomicNets _ =
        [
--          after makeNonAtomic (algebraicClosureGQR str)
--          after makeNonAtomic (algebraicClosureSpS str)
--        , after makeNonAtomic (algebraicReasoning str)
        ] ++ map (after opramNetToOpraNetAtomic)
                 (proceduresForAtomicNets (undefined :: Opra))

    proceduresForNonAtomicNets _ =
        [
--          algebraicClosure str
        ]
