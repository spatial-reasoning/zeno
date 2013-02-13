{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module DecisionProcedure.Opra3
    ( module DecisionProcedure.Opra
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra3
import DecisionProcedure
import DecisionProcedure.AlgebraicClosure
--import DecisionProcedure.AlgebraicGeometric
import DecisionProcedure.Opra

instance HasBinAClosureGqr   ARel Opra3
instance HasBinAClosureGqr   GRel Opra3
instance HasBinAClosureSparq ARel Opra3
instance HasBinAClosureSparq GRel Opra3
--instance HasAReasoning       ARel Opra3
--instance HasAReasoning       GRel Opra3

instance HasDecisionProcedure (ARel Opra3) where
    procedures _ =
        [ algebraicClosureGQR
        , algebraicClosure
--        , algebraicClosureSpS
--        , algebraicReasoning
        ] ++ map (firstApply opramNetToOpraNetAtomic)
                 (procedures (undefined :: ARel Opra))

instance HasDecisionProcedure (GRel Opra3) where
    procedures _ =
        [ algebraicClosure
        ]
