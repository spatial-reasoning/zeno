{-# LANGUAGE FlexibleInstances #-}
module DecisionProcedure.OrientedPoint
    ( module SpatioTemporalStructure.OrientedPoint
    ) where

-- standard modules

-- local modules
import Basics
import DecisionProcedure
--import DecisionProcedure.Otop.TriangleConsistency
import SpatioTemporalStructure.OrientedPoint

instance HasDecisionProcedure (ARel Otop) where
    procedures _ =
        [
--          DecisionProcedure { decProName = "TC"
--                            , decProProc = triangleConsistency }
--        , DecisionProcedure { decProName = "TC WitUn"
--                            , decProProc = triangleConsistencyWithWitness }
--        , DecisionProcedure { decProName = "TC WitSa"
--                            , decProProc = triangleConsistencyWithWitnesses }
--        , DecisionProcedure { decProName = "TC WitUS"
--                            , decProProc = triangleConsistencyWithWitnessAndWitnesses }
--        , DecisionProcedure { decProName = "AC"
--                            , decProProc = angleConsistency }
--        , DecisionProcedure { decProName = "AC WitUn"
--                            , decProProc = angleConsistencyWithWitness }
--        , DecisionProcedure { decProName = "AC WitSa"
--                            , decProProc = angleConsistencyWithWitnesses }
--        , DecisionProcedure { decProName = "AC WitUS"
--                            , decProProc = angleConsistencyWithWitnessAndWitnesses }
        ]

instance HasDecisionProcedure (GRel Otop) where
    procedures _ = []

