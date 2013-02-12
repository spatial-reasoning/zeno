{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module DecisionProcedure.Opra10
    ( module DecisionProcedure.Opra
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra10
import DecisionProcedure
--import DecisionProcedure.AlgebraicClosure
--import DecisionProcedure.AlgebraicGeometric
import DecisionProcedure.Opra

--instance HasBinAClosureGqr   ARel Opra10
--instance HasBinAClosureGqr   GRel Opra10
--instance HasBinAClosureSparq ARel Opra10
--instance HasBinAClosureSparq GRel Opra10
--instance HasAReasoning       ARel Opra10
--instance HasAReasoning       GRel Opra10

instance HasDecisionProcedure (ARel Opra10) where
    procedures _ =
        [
--          algebraicClosureGQR
--        , algebraicReasoning
        ] ++ map (firstApply opramNetToOpraNetAtomic)
                 (procedures (undefined :: ARel Opra))

instance HasDecisionProcedure (GRel Opra10) where
    procedures _ =
        [
--          algebraicClosure
        ]
