module Convert.OpraToOpus where

-- standard modules
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Opra
import SpatioTemporalStructure.Interval hiding (null, insert)
import SpatioTemporalStructure.OrientedPoint

granSecToIval :: Int -> Int -> Interval Rational
granSecToIval g s = onEnds toRational $
    if even s then
        Interval C C (angle    2 g s) (angle    2 g s)
    else
        Interval O O (minAngle 2 g s) (maxAngle 2 g s)

opraNetToOpusNetAtomic :: Network [String] (ARel Opra)
                       -> Network [String] (Opus Rational)
opraNetToOpusNetAtomic net@Network{nCons = cons} = net {nCons = newCons}
  where
    newCons = Map.foldrWithKey
      (\ nodes@[node, node2] (ARel (Opra gran sec sec2)) acc ->
        (\(xs, xu, ys, yu) ->
          Map.insert [node2, node] (Opus ys yu) $
          Map.insert nodes         (Opus xs xu) acc
        ) $ let
              xIval = granSecToIval gran sec
              yIval = granSecToIval gran sec2
              ySameIval = invertModulo 2 yIval
            in
              case sec of
                  (-1) -> ([yIval], [empty], ySameIval, [empty])
                  _    -> ([empty], [xIval], [empty], [yIval])
      ) Map.empty cons

opraNetToOpusNet :: Network [String] (GRel Opra)
                 -> Network [String] (Opus Rational)
opraNetToOpusNet net@Network{nCons = cons} = net {nCons = newCons}
  where
    newCons = Map.foldrWithKey
      (\ nodes@[node, node2] (GRel opraSet) acc ->
        (\(xs, xu, ys, yu) ->
          Map.insert [node2, node] (Opus ys yu) $
          Map.insert nodes         (Opus xs xu) acc
        ) $ Set.foldr
          (\ (Opra gran sec sec2) (xSame, xUnsame, ySame, yUnsame) ->
            let
              xIval = granSecToIval gran sec
              yIval = granSecToIval gran sec2
              ySameIval = invertModulo 2 xIval
            in
              case sec of
                  (-1) -> (xIval:xSame, xUnsame, ySameIval ++ ySame, yUnsame)
                  _    -> (xSame, xIval:xUnsame, ySame, yIval:yUnsame)
          ) ([],[],[],[]) opraSet
      ) Map.empty cons
