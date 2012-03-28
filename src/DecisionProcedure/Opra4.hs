module DecisionProcedure.Opra4
    ( module DecisionProcedure.Opra
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra4
import DecisionProcedure
import DecisionProcedure.Opra

str = "opra4"

instance HasDecisionProcedure Opra4 where
    proceduresForAtomicNets _ =
        [ after makeNonAtomic (algebraicClosureGQR str)
--        , after makeNonAtomic (algebraicReasoning str)
        ] ++ map (after opramNetToOpraNetAtomic)
                 (proceduresForAtomicNets (undefined :: Opra))

    proceduresForNonAtomicNets _ =
        [ algebraicClosure str
        ]
