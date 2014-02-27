{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module DecisionProcedure.Opra6
    ( module DecisionProcedure.Opra
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra6
import DecisionProcedure
import DecisionProcedure.AlgebraicClosure
--import DecisionProcedure.AlgebraicGeometric
import DecisionProcedure.Opra

instance HasBinAClosureGqr   ARel Opra6
instance HasBinAClosureGqr   GRel Opra6
instance HasBinAClosureSparq ARel Opra6
instance HasBinAClosureSparq GRel Opra6
--instance HasAReasoning       ARel Opra6
--instance HasAReasoning       GRel Opra6

instance HasDecisionProcedure (ARel Opra6) where
    procedures _ =
        [ algebraicClosureGQR
--        , algebraicClosure
--        , algebraicClosureSpS
--        , algebraicReasoning
        ] ++ map (firstApply opramNetToOpraNetAtomic)
                 (procedures (undefined :: ARel Opra))

instance HasDecisionProcedure (GRel Opra6) where
    procedures _ =
        [ algebraicClosureGQR
--        , algebraicReasoning
        ] ++ map (firstApply opramNetToOpraNet)
                 (procedures (undefined :: GRel Opra))

