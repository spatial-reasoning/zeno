{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module DecisionProcedure.Opra2
    ( module DecisionProcedure.Opra
    ) where

-- standard modules
import Data.Maybe
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Opra2
import DecisionProcedure
import DecisionProcedure.AlgebraicClosure
--import DecisionProcedure.AlgebraicGeometric
import DecisionProcedure.Opra

instance HasBinAClosureGqr   ARel Opra2
instance HasBinAClosureGqr   GRel Opra2
instance HasBinAClosureSparq ARel Opra2
instance HasBinAClosureSparq GRel Opra2
--instance HasAReasoning       ARel Opra2
--instance HasAReasoning       GRel Opra2

instance HasDecisionProcedure (ARel Opra2) where
    procedures _ =
        [ algebraicClosureGQR
        , algebraicClosure
--        , algebraicReasoning
        ] ++ map (firstApply opramNetToOpraNetAtomic)
                 (procedures (undefined :: ARel Opra))

instance HasDecisionProcedure (GRel Opra2) where
    procedures _ =
        [ algebraicClosureGQR
        , algebraicClosure
        ]
