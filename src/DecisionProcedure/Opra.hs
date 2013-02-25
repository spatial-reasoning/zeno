{-# LANGUAGE FlexibleInstances #-}
module DecisionProcedure.Opra
    ( module DecisionProcedure.OrientedPoint
    , module DecisionProcedure.Opus
    , module Calculus.Opra
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra
import Convert.OpraToOtop
import Convert.OpraToOpus
import DecisionProcedure
import DecisionProcedure.OrientedPoint
import DecisionProcedure.Opus

instance HasDecisionProcedure (ARel Opra) where
    procedures _ =
        [
        ]
        ++ map (firstApply opraNetToOtopNetAtomic)
               (procedures (undefined :: ARel Otop))
        ++ map (firstApply opraNetToOpusNetAtomic)
               (procedures (undefined :: Opus Rational))

instance HasDecisionProcedure (GRel Opra) where
    procedures _ = []
