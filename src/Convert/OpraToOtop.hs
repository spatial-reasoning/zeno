module Convert.OpraToOtop where

-- standard modules
import qualified Data.Map as Map

-- local modules
import Basics
import Calculus.Opra
import SpatioTemporalStructure.OrientedPoint

--improve: generalize this to include "GRel"s aswell.
opraNetToOtopNetAtomic :: Network [String] (ARel Opra)
                       -> Network [String] (ARel Otop)
opraNetToOtopNetAtomic net@Network{nCons = cons} = net {nCons = newCons}
  where
    newCons = Map.foldrWithKey
        (\ nodes (ARel (Opra gran sec sec2)) acc -> case sec of
            (-1)      -> Map.insert nodes (ARel (Otop (-gran) sec2)) acc
            otherwise -> Map.insert (reverse nodes) (ARel (Otop gran sec2)) $
                         Map.insert nodes           (ARel (Otop gran sec )) acc
        ) Map.empty cons
