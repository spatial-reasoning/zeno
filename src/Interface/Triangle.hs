module Interface.Triangle where

-- standard modules
import Data.Maybe
import System.IO.Unsafe

-- local modules
import Basics
import Calculus.Dipole
import Calculus.FlipFlop
import Convert
import qualified TriangleConsistency as TC

--import Debug.Trace


checkConsistency :: Basics.Network [String] Dipole72
                 -> Maybe Bool
checkConsistency net7
    | isNothing net5  = Just False
    | otherwise  = unsafePerformIO $
                       TC.runTC $ flipFlop5sToDominik $ fromJust net5
    where
        net5 = ffsToFF5s $ dipolesToFlipFlops net7

