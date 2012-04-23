module DecisionProcedure.Dipole72 () where

-- standard modules
import Data.Maybe
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Dipole72
import Convert
import DecisionProcedure

str = "dra-72"

toFlipFlopsAtomic (a,fun) = (a, newFun)
  where
    newFun net = case dipolesToFlipFlops net of
       Just x  -> fun x
       Nothing -> Just False

instance HasDecisionProcedure Dipole72 where
    proceduresForAtomicNets _ =
        [ after makeNonAtomic (algebraicClosure str)
        , after makeNonAtomic (algebraicReasoning str)
        , toFlipFlopsAtomic triangleConsistencyFlipFlop
        , toFlipFlopsAtomic chirotopeSloppy
        , toFlipFlopsAtomic biquadraticFinalPolynomialsSloppy
        , toFlipFlopsAtomic chirotope
        , toFlipFlopsAtomic biquadraticFinalPolynomials
        ]

    proceduresForNonAtomicNets _ =
        [ algebraicClosure str
        , algebraicReasoning str
        ]
