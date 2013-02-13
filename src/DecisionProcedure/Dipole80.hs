{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module DecisionProcedure.Dipole80 () where

-- standard modules
import Data.Maybe
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Dipole80
import DecisionProcedure

instance HasBinAClosureGqr   ARel Dipole80
instance HasBinAClosureGqr   GRel Dipole80
instance HasBinAClosureSparq ARel Dipole80
instance HasBinAClosureSparq GRel Dipole80
instance HasAReasoning       ARel Dipole80
instance HasAReasoning       GRel Dipole80

instance HasDecisionProcedure (ARel Dipole80) where
    procedures _ =
        [ algebraicClosureGQR
        , algebraicClosure
        , algebraicReasoning
        ]

instance HasDecisionProcedure (GRel Dipole80) where
    procedures _ =
        [ algebraicClosureGQR
        , algebraicClosure
        , algebraicReasoning
        ]
