{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module DecisionProcedure.AlgebraicGeometric where

import Basics
import DecisionProcedure
import Export
import qualified Interface.Sparq as S
--import qualified Interface.Sparq.Server as S

class ( Relation (a b) b, Calculus b
      , Sparqifiable (Network [String] (a b))
      ) => HasAReasoning a b
  where
    algebraicReasoning :: DecisionProcedure (a b)
    algebraicReasoning = DecisionProcedure
        { decProName = "AR"
        , decProProc = S.algebraicReasoning }

