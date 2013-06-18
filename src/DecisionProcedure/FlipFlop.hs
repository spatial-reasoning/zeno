{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module DecisionProcedure.FlipFlop
    ( triangleConsistency
    , chirotope
    , biquadraticFinalPolynomials
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.FlipFlop
import DecisionProcedure
import DecisionProcedure.AlgebraicClosure
import DecisionProcedure.AlgebraicGeometric
import DecisionProcedure.FlipFlop.TriangleConsistency as T
import DecisionProcedure.FlipFlop.OrientedMatroid

triangleConsistency = DecisionProcedure
    { decProName = "TC"
    , decProProc = T.checkConsistency }

chirotope = DecisionProcedure
    { decProName = "OM"
    , decProProc = isAcyclicChirotopeFlipFlop }

biquadraticFinalPolynomials = DecisionProcedure
    { decProName = "BFP"
    , decProProc = isAcyclicChirotopeWithoutBPFlipFlop }

instance HasBinAClosureSparq ARel FlipFlop
instance HasBinAClosureSparq GRel FlipFlop
instance HasTerAClosureSparq ARel FlipFlop
instance HasTerAClosureSparq GRel FlipFlop
instance HasAReasoning       ARel FlipFlop
instance HasAReasoning       GRel FlipFlop

instance HasDecisionProcedure (ARel FlipFlop) where
    procedures _ =
        [ algebraicClosure
        , ternaryAlgebraicClosure
--        , algebraicReasoning
        , triangleConsistency
        , chirotope
        , biquadraticFinalPolynomials
        ]

instance HasDecisionProcedure (GRel FlipFlop) where
    procedures _ =
        [ algebraicClosure
        , ternaryAlgebraicClosure
        , algebraicReasoning
        ]
