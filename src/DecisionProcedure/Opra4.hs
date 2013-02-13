{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module DecisionProcedure.Opra4
    ( module DecisionProcedure.Opra
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra4
import DecisionProcedure
import DecisionProcedure.AlgebraicClosure
--import DecisionProcedure.AlgebraicGeometric
import DecisionProcedure.Opra

instance HasBinAClosureGqr   ARel Opra4
instance HasBinAClosureGqr   GRel Opra4
instance HasBinAClosureSparq ARel Opra4
instance HasBinAClosureSparq GRel Opra4
--instance HasAReasoning       ARel Opra4
--instance HasAReasoning       GRel Opra4

instance HasDecisionProcedure (ARel Opra4) where
    procedures _ =
        [ algebraicClosureGQR
        , algebraicClosure
--        , algebraicClosureSpS
--        , algebraicReasoning
        ] ++ map (firstApply opramNetToOpraNetAtomic)
                 (procedures (undefined :: ARel Opra))

instance HasDecisionProcedure (GRel Opra4) where
    procedures _ =
        [ algebraicClosure
--        , algebraicReasoning
        ]
