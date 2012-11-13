module DecisionProcedure.OrientedPoint
    ( module SpatioTemporalStructure.OrientedPoint
    ) where

-- standard modules

-- local modules
import Basics
import DecisionProcedure
import DecisionProcedure.OrientedPoint.AngleConsistency
import SpatioTemporalStructure.OrientedPoint

instance HasDecisionProcedure Otop where
    proceduresForAtomicNets _ =
        [ ("TC", triangleConsistency)
        , ("TC WitUn", triangleConsistencyWithWitnesses)
        , ("TC WitSa", triangleConsistencyWithWitness)
        , ("TC WitUS", triangleConsistencyWithWitnessesAndWitness)
--        , ("AngleCon", angleConsistency)
        ]

    proceduresForNonAtomicNets _ =
        [
        ]
