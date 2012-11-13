module DecisionProcedure where
--fixme: split this module!

-- standard modules
import qualified Data.Set as Set

-- local modules
import Basics
import qualified Interface.Gqr as G
import qualified Interface.Sparq as S
--import qualified Interface.Sparq.Server as S
import qualified DecisionProcedure.FlipFlop.TriangleConsistency as T
import DecisionProcedure.FlipFlop.OrientedMatroid

class HasDecisionProcedure a where
    proceduresForAtomicNets :: a ->
        [ ( String
--          , Network [String] (Set.Set a) -> Maybe Bool )
          , Network [String] a -> Maybe Bool )
        ]

    proceduresForNonAtomicNets :: a ->
        [ ( String
          , Network [String] (Set.Set a) -> Maybe Bool )
        ]

after c (a, b) = (a, b . c)


algebraicClosureGQR str =
    ( "BAC-GQR", (\(x,_) -> x) . G.algebraicClosure str )

algebraicClosure str =
    ( "BAC", (\(x,_,_) -> x) . S.algebraicClosure str )

--algebraicClosureSpS str =
--    ( "AC", (\(x,_,_) -> x) . SpS.algebraicClosure str )


ternaryAlgebraicClosure str =
    ( "TAC", (\(x,_,_) -> x) . S.ternaryAlgebraicClosure str )

algebraicReasoning str =
    ( "AR", S.algebraicReasoning str )

triangleConsistencyFlipFlop =
    ( "TC", T.checkConsistency)

chirotope =
    ( "OM", isAcyclicChirotopeFlipFlop)

biquadraticFinalPolynomials =
    ( "BFP", isAcyclicChirotopeWithoutBPFlipFlop)

