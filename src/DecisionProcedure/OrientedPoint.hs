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
        [ ("AngleCon", angleConsistency)
        ]

    proceduresForNonAtomicNets _ =
        [
        ]
