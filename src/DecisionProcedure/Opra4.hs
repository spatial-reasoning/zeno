module DecisionProcedure.Opra4
    ( module DecisionProcedure.Opra
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra4
import DecisionProcedure
import DecisionProcedure.Opra

str = "opra-4"

instance HasDecisionProcedure Opra4 where
    proceduresForAtomicNets _ =
        [
          after makeNonAtomic (algebraicClosureGQR "opra4")
        , after makeNonAtomic (algebraicClosure "opra-4")
--        , after makeNonAtomic (algebraicClosureSpS str)
--        , after makeNonAtomic (algebraicReasoning "opra-4")
        ] ++ map (after opramNetToOpraNetAtomic)
                 (proceduresForAtomicNets (undefined :: Opra))

    proceduresForNonAtomicNets _ =
        [ algebraicClosure str
        ]
