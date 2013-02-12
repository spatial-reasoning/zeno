{-# LANGUAGE FlexibleInstances #-}
module DecisionProcedure.Opra
    ( module DecisionProcedure.OrientedPoint
    , module Calculus.Opra
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra
import Convert.OpraToOtop
import DecisionProcedure
import DecisionProcedure.OrientedPoint

instance HasDecisionProcedure (ARel Opra) where
    procedures _ =
        [
        ] ++ map (firstApply opraNetToOtopNetAtomic)
                 (procedures (undefined :: ARel Otop))

instance HasDecisionProcedure (GRel Opra) where
    procedures _ = []
