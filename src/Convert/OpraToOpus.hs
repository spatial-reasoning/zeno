module Convert.OpraToOpus where

-- standard modules
import qualified Data.Map as Map
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Opra
import SpatioTemporalStructure.Interval hiding (null, insert)
import qualified SpatioTemporalStructure.Interval as I
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

-- mnemonic: emptyListToEmptyInterval
elei ls = case ls of
    [] -> [empty]
    x  -> x

-- fixme: This conversion is WRONG, since e.g.
-- "node1" "node2" (Opra1 0 1, Opra1 1 0)
-- is not equivalent to
-- "node1" "node2" (Opus [0, 1)) /\ "node2" "node1" (Opus [0, 1))
-- The intervals cannot simply by connected since the respective half-relation
-- cannot be seperated.
opraNetToOpusNet :: Network [String] (GRel Opra)
                 -> Network [String] (Opus Rational)
opraNetToOpusNet net@Network{nCons = cons} = net {nCons = newCons}
  where
    -- improve: refactor some functions.
    newCons = Map.foldrWithKey
      (\ [node, node2] (GRel opraSet) acc ->
        (\(xs, xu, ys, yu) ->
          Map.insert [node2, node ] (Opus (elei ys) (elei yu)) $
          Map.insert [node , node2] (Opus (elei xs) (elei xu)) acc
        ) $ Set.foldr
          (\ (Opra gran sec sec2) (xSame, xUnsame, ySame, yUnsame) ->
            let
              xIval = granSecToIval gran sec
              yIval = granSecToIval gran sec2
              ySameIval = invertModulo 2 yIval
            in
              case sec of
                  (-1) -> ( I.insert yIval xSame
                          , xUnsame
                          , foldr I.insert ySame ySameIval
                          , yUnsame )
                  _    -> ( xSame
                          , I.insert xIval xUnsame
                          , ySame
                          , I.insert yIval yUnsame )
          ) ([],[],[],[]) opraSet
      ) Map.empty cons

