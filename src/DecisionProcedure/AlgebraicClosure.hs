{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module DecisionProcedure.AlgebraicClosure where

import Basics
import Export
import DecisionProcedure
import qualified Interface.Gqr as G
import qualified Interface.Sparq as S
--import qualified Interface.Sparq.Server as S

class ( Relation (a b) b, Calculus b
      , Gqrifiable (Network [String] (a b))
      ) => HasBinAClosureGqr a b
  where
    algebraicClosureGQR :: DecisionProcedure (a b)
    algebraicClosureGQR = DecisionProcedure
        { decProName = "BAC-GQR"
        , decProProc = (\(x,_) -> x) . G.algebraicClosure }

class ( Relation (a b) b, Calculus b
      , Sparqifiable (Network [String] (a b))
      ) => HasBinAClosureSparq a b
  where
    algebraicClosure :: DecisionProcedure (a b)
    algebraicClosure = DecisionProcedure
        { decProName = "BAC"
        , decProProc = (\(x,_,_) -> x) . S.algebraicClosure }

    --algebraicClosureSpS = DecisionProcedure
    --    { decProName = "AC"
    --    , decProProc = (\(x,_,_) -> x) . SpS.algebraicClosure }

class ( Relation (a b) b, Calculus b
      , Sparqifiable (Network [String] (a b))
      ) => HasTerAClosureSparq a b
  where
    ternaryAlgebraicClosure :: DecisionProcedure (a b)
    ternaryAlgebraicClosure = DecisionProcedure
        { decProName = "TAC"
        , decProProc = (\(x,_,_) -> x) . S.ternaryAlgebraicClosure }

