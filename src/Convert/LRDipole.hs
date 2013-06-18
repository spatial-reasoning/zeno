module Convert.LRDipole where

-- standard modules
import qualified Data.Map as Map
import Data.Maybe

-- local modules
import Basics
import Calculus.Dipole72
import Calculus.FlipFlop


{------------------------------------------------------------------------------
    Dipoles to LR
------------------------------------------------------------------------------}

-- | Converts a single Dipole constranit into the corresponding list of
-- FlipFlop constraints.
dipoleToFlipFlop :: [String]
                 -> ARel Dipole72
                 -> [([String], ARel FlipFlop)]
dipoleToFlipFlop [a, b] (ARel rel) =
    [ ([as, ae, bs], ARel $ cReadRel [r1])
    , ([as, ae, be], ARel $ cReadRel [r2])
    , ([bs, be, as], ARel $ cReadRel [r3])
    , ([bs, be, ae], ARel $ cReadRel [r4])
    ] where
        as = a ++ "_s"
        ae = a ++ "_e"
        bs = b ++ "_s"
        be = b ++ "_e"
        [r1, r2, r3, r4] = cShowRel rel

-- | Converts a Network of Dipole constraints into the corresponding Network of
-- FlipFlop constraints.
dipolesToFlipFlops :: Network [String] (ARel Dipole72)
                   -> Network [String] (ARel FlipFlop)
dipolesToFlipFlops net@Network { nCons = cons } = net{ nCons = newCons }
  where
    newCons = Map.foldrWithKey
        (\nodes rel mapAcc -> foldl
            (\acc (nodes2, rel2) -> fromJust $ insertCon nodes2 rel2 acc)
            mapAcc $
            dipoleToFlipFlop nodes rel
        )
        Map.empty
        cons

