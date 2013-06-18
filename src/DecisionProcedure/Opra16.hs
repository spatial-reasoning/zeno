{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module DecisionProcedure.Opra16
    ( module DecisionProcedure.Opra
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra16
import DecisionProcedure
--import DecisionProcedure.AlgebraicClosure
--import DecisionProcedure.AlgebraicGeometric
import DecisionProcedure.Opra

--instance HasBinAClosureGqr   ARel Opra16
--instance HasBinAClosureGqr   GRel Opra16
--instance HasBinAClosureSparq ARel Opra16
--instance HasBinAClosureSparq GRel Opra16
--instance HasAReasoning       ARel Opra16
--instance HasAReasoning       GRel Opra16

instance HasDecisionProcedure (ARel Opra16) where
    procedures _ =
        [
--          after makeNonAtomic (algebraicClosureGQR str)
--          after makeNonAtomic (algebraicClosureSpS str)
--        , after makeNonAtomic (algebraicReasoning str)
        ] ++ map (firstApply opramNetToOpraNetAtomic)
                 (procedures (undefined :: ARel Opra))

instance HasDecisionProcedure (GRel Opra16) where
    procedures _ = [
        ] ++ map (firstApply opramNetToOpraNet)
                 (procedures (undefined :: GRel Opra))

