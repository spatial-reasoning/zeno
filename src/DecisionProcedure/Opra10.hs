module DecisionProcedure.Opra10
    ( module DecisionProcedure.Opra
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra10
import DecisionProcedure
import DecisionProcedure.Opra

str = "opra10"

instance HasDecisionProcedure Opra10 where
    proceduresForAtomicNets _ =
        [
--          after makeNonAtomic (algebraicClosureGQR str)
--        , after makeNonAtomic (algebraicReasoning str)
        ] ++ map (after opramNetToOpraNetAtomic)
                 (proceduresForAtomicNets (undefined :: Opra))

    proceduresForNonAtomicNets _ =
        [
--          algebraicClosure str
        ]
