module Convert.OpraToOtop where

-- standard modules
import Data.List
import qualified Data.Map as Map

-- local modules
import Basics
import Calculus.Opra
import SpatioTemporalStructure.OrientedPoint

opraNetToOtopNetAtomic :: Network [String] Opra -> Network [String] Otop
opraNetToOtopNetAtomic net@Network{nCons = cons} = net {nCons = newCons}
  where
    newCons = Map.foldrWithKey
        (\ nodes (Opra gran sec sec2) acc -> case sec of
            (-1)      -> Map.insert nodes (Otop (-gran) sec2) acc
            otherwise -> Map.insert (reverse nodes) (Otop gran sec2) $
                         Map.insert nodes           (Otop gran sec ) acc
        ) Map.empty cons
