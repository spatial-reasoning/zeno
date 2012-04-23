module DecisionProcedure.Opra16
    ( module DecisionProcedure.Opra
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra16
import DecisionProcedure
import DecisionProcedure.Opra

str = "opra8"

instance HasDecisionProcedure Opra16 where
    proceduresForAtomicNets _ =
        [
          after makeNonAtomic (algebraicClosureGQR str)
--          after makeNonAtomic (algebraicClosureSpS str)
--        , after makeNonAtomic (algebraicReasoning str)
        ] ++ map (after opramNetToOpraNetAtomic)
                 (proceduresForAtomicNets (undefined :: Opra))

    proceduresForNonAtomicNets _ =
        [
--          algebraicClosure str
        ]
