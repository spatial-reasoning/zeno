{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module DecisionProcedure.Dipole72 () where

-- standard modules
import Data.Maybe
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Dipole72
import Convert
import DecisionProcedure
import DecisionProcedure.FlipFlop
import DecisionProcedure.AlgebraicClosure
import DecisionProcedure.AlgebraicGeometric

instance HasBinAClosureGqr   ARel Dipole72
instance HasBinAClosureGqr   GRel Dipole72
instance HasBinAClosureSparq ARel Dipole72
instance HasBinAClosureSparq GRel Dipole72
instance HasAReasoning       ARel Dipole72
instance HasAReasoning       GRel Dipole72

instance HasDecisionProcedure (ARel Dipole72) where
    procedures _ =
        [ algebraicClosureGQR
        , algebraicClosure
        , algebraicReasoning
        , firstApply dipolesToFlipFlops $ triangleConsistency
        , firstApply dipolesToFlipFlops $ chirotope
        , firstApply dipolesToFlipFlops $ biquadraticFinalPolynomials
        ]

instance HasDecisionProcedure (GRel Dipole72) where
    procedures _ =
        [ algebraicClosureGQR
        , algebraicClosure
        , algebraicReasoning
        ]
