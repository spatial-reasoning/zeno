module SpatioTemporalStructure.OrientedPoint where

data Otop = Otop Int Int deriving (Eq, Ord, Read, Show)

gran   (Otop a _) = a
sector (Otop _ b) = b

