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
        , ("TC WitUn", triangleConsistencyWithWitness)
        , ("TC WitSa", triangleConsistencyWithWitnesses)
        , ("TC WitUS", triangleConsistencyWithWitnessAndWitnesses)
--        , ("AngleCon", angleConsistency)
        ]

    proceduresForNonAtomicNets _ =
        [
        ]
