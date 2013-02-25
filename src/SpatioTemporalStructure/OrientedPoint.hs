module SpatioTemporalStructure.OrientedPoint where

import Data.Ratio
import qualified SpatioTemporalStructure.Interval as I

data Otop = Otop
    { otopGran   :: Int
    , otopSector :: Int
    } deriving (Eq, Ord, Read, Show)

data OpusG = OpusG
    { opusGran       :: Int
    , opusSector     :: Int
    , opusOpenFront  :: Bool
    } deriving (Eq, Ord, Read, Show)

class OPoint a where
    gran   :: a -> Int
    sector :: a -> Int

instance OPoint Otop where
    gran   = otopGran
    sector = otopSector

instance OPoint OpusG where
    gran   = opusGran
    sector = opusSector

-- Oriented Points with Angles
-- The full circle has an angle of "2%1" (think π).
data Opus a = Opus
    { opusSame    :: [I.Interval a]
    , opusNonSame :: [I.Interval a]
    } deriving (Eq, Ord, Read, Show)

onOpa :: ([I.Interval a] -> [I.Interval a]) -> Opus a -> Opus a
onOpa fun opus = opus{ opusSame    = fun $ opusSame    opus
                     , opusNonSame = fun $ opusNonSame opus }

--invertOpus :: Rational -> Opus -> Opus
--invertOpus q = onOpus (I.invertModuloList q)

