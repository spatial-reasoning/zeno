{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module DecisionProcedure.Opra8
    ( module DecisionProcedure.Opra
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra8
import DecisionProcedure
import DecisionProcedure.AlgebraicClosure
--import DecisionProcedure.AlgebraicGeometric
import DecisionProcedure.Opra

instance HasBinAClosureGqr   ARel Opra8
instance HasBinAClosureGqr   GRel Opra8
instance HasBinAClosureSparq ARel Opra8
instance HasBinAClosureSparq GRel Opra8
--instance HasAReasoning       ARel Opra8
--instance HasAReasoning       GRel Opra8

instance HasDecisionProcedure (ARel Opra8) where
    procedures _ =
        [ algebraicClosureGQR
--        , algebraicClosure
--        , algebraicClosureSpS
--        , algebraicReasoning
        ] ++ map (firstApply opramNetToOpraNetAtomic)
                 (procedures (undefined :: ARel Opra))

instance HasDecisionProcedure (GRel Opra8) where
    procedures _ =
        [ algebraicClosureGQR
        ] ++ map (firstApply opramNetToOpraNet)
                 (procedures (undefined :: GRel Opra))

