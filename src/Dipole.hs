module Dipole where

-- standard modules
import qualified Data.Set as Set
import qualified Data.Map as Map
-- local modules
import Basics

-- dipole-72 relations modulo converses.
dipole72Relations =
    [ "bbbb" , "bbff" , "beie" , "bfii" , "biif"
    , "blrr" , "brll" , "bsef" , "ebis" , "eifs"
    , "ells" , "errs" , "eses" , "fbii" , "fefe"
    , "ffff" , "fifi" , "flll" , "frrr" , "ibib"
    , "illr" , "irrl" , "lbll" , "lere" , "lfrr"
    , "lirl" , "llll+", "lllla", "lllr" , "llrf"
    , "llrl" , "llrr+", "llrr-", "llrrp", "lrri"
    , "lrrl" , "lrrr" , "rbrr" , "rlrr" , "rrrr+"
    , "rrrra", "sbsb" , "sese" , "sfsi" , "slsr" ]

sortAtomicDipoleConstraint :: Constraint -> Constraint
sortAtomicDipoleConstraint (ents, rel)
    | rel > swappedRel = (reverse ents, swappedRel)
    | otherwise     = (ents, rel)
    where
        swappedRel = Set.map swapRel rel

swapRel (a:b:c:d:s) = [c, d, a, b] ++ swappedSign
    where
        swappedSign = case s of
                          "+" -> "-"
                          "-" -> "+"
                          _   -> s

dipoleSort :: [Constraint] -> [Constraint]
dipoleSort cons = map sortAtomicDipoleConstraint cons

