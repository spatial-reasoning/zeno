module DecisionProcedure where

-- standard modules
import qualified Data.Set as Set

-- local modules
import Basics
import qualified Interface.Gqr as G
import qualified Interface.Sparq as S
--import qualified Interface.Sparq.Server as SpS
import qualified Interface.Triangle as T
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
    ( "AC-GQR", (\(x,_) -> x) . G.algebraicClosure str )

algebraicClosure str =
    ( "AC-SparQ", (\(x,_,_) -> x) . S.algebraicClosure str )

--algebraicClosureSpS str =
--    ( "AC", (\(x,_,_) -> x) . SpS.algebraicClosure str )


ternaryAlgebraicClosure str =
    ( "TAC", (\(x,_,_) -> x) . S.ternaryAlgebraicClosure str )

algebraicReasoning str =
    ( "AR", S.algebraicReasoning str )

triangleConsistencyFlipFlop =
    ( "TC", T.checkConsistency)

chirotope =
    ( "OM e", isAcyclicChirotopeFlipFlop)

biquadraticFinalPolynomials =
    ( "BFP e", isAcyclicChirotopeWithoutBPFlipFlop)

