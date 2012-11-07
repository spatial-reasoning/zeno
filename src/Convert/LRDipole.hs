module Convert.LRDipole where

-- standard modules
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
-- local modules
import Basics
import Calculus.Dipole72
import Calculus.FlipFlop
--import Helpful

--import Debug.Trace

{------------------------------------------------------------------------------
    Dipoles to LR
------------------------------------------------------------------------------}

-- | Converts a single Dipole constranit into the corresponding list of
-- FlipFlop constraints.
dipoleToFlipFlop :: [String]
                 -> Dipole72
                 -> [([String], FlipFlop)]
dipoleToFlipFlop [a, b] rel =
    [ ([as, ae, bs], readRel [r1])
    , ([as, ae, be], readRel [r2])
    , ([bs, be, as], readRel [r3])
    , ([bs, be, ae], readRel [r4])
    ] where
        as = a ++ "_s"
        ae = a ++ "_e"
        bs = b ++ "_s"
        be = b ++ "_e"
        [r1, r2, r3, r4] = showRel rel

-- | Converts a Network of Dipole constraints into the corresponding Network of
-- FlipFlop constraints.
dipolesToFlipFlops :: Network [String] Dipole72
                   -> Maybe (Network [String] FlipFlop)
dipolesToFlipFlops net@Network { nCons = cons }
    | not $ Map.null $ Map.filterWithKey
        (\[a, b, c] rel -> Set.null rel)
        newCons  = Nothing
    | otherwise  = Just $ net{ nCons = Map.map Set.findMin newCons }
  where
    newCons = Map.foldrWithKey
        (\nodes rel mapAcc -> foldl
            (\acc (nodes2, rel2) -> insertCon nodes2 (Set.singleton rel2) acc)
            mapAcc $
            dipoleToFlipFlop nodes rel
        )
        Map.empty
        cons

