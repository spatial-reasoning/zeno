{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module DecisionProcedure.Opra1 where

-- standard modules

-- local modules
import Basics
import Calculus.Opra
import Calculus.Opra1
import Calculus.Opra1.ToFlipFlop
import DecisionProcedure
import DecisionProcedure.Opra
import DecisionProcedure.FlipFlop
import DecisionProcedure.AlgebraicClosure
import DecisionProcedure.AlgebraicGeometric

-- fixme: make this nonatomic from the beginning.
toFlipFlopsAtomic dp@(DecisionProcedure { decProName = a, decProProc = fun }) =
    dp{ decProProc = fun . makeAtomic . opra1ToFlipFlopNet . makeNonAtomic }

toFlipFlopsNonAtomic dp@(DecisionProcedure { decProName = a
                                           , decProProc = fun }) =
    dp{ decProProc = fun . opra1ToFlipFlopNet . makeNonAtomic }

instance HasBinAClosureGqr   ARel Opra1
instance HasBinAClosureGqr   GRel Opra1
instance HasBinAClosureSparq ARel Opra1
instance HasBinAClosureSparq GRel Opra1
instance HasAReasoning       ARel Opra1
instance HasAReasoning       GRel Opra1

instance HasDecisionProcedure (ARel Opra1) where
    procedures _ =
        [ algebraicClosureGQR
--        , algebraicClosure
        , toFlipFlopsNonAtomic algebraicReasoning
        , toFlipFlopsAtomic triangleConsistency
        , toFlipFlopsAtomic chirotope
        , toFlipFlopsAtomic biquadraticFinalPolynomials
        ] ++ map (firstApply opramNetToOpraNetAtomic)
                 (procedures (undefined :: ARel Opra))

instance HasDecisionProcedure (GRel Opra1) where
    procedures _ =
        [ algebraicClosureGQR
--        , algebraicClosure
        ] ++ map (firstApply opramNetToOpraNet)
                 (procedures (undefined :: GRel Opra))

