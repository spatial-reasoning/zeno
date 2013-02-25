{-# LANGUAGE FlexibleInstances #-}
module DecisionProcedure.Opus
    ( module SpatioTemporalStructure.OrientedPoint
    ) where

-- standard modules

-- local modules
import Basics
import DecisionProcedure
import DecisionProcedure.Opus.TriangleConsistency
import SpatioTemporalStructure.OrientedPoint

instance HasDecisionProcedure (Opus Rational) where
    procedures _ =
        [ DecisionProcedure { decProName = "OpusTC"
                            , decProProc = triangleConsistency }
        , DecisionProcedure { decProName = "OpusTC WitUn"
                            , decProProc = triangleConsistencyWithWitness }
--        , DecisionProcedure { decProName = "TC WitSa"
--                            , decProProc = triangleConsistencyWithWitnesses }
--        , DecisionProcedure { decProName = "TC WitUS"
--                            , decProProc = triangleConsistencyWithWitnessAndWitnesses }
        ]

