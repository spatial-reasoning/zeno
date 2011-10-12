module Parsing where

-- standard modules
import qualified Data.Set as Set

-- local modules
import Basics


data NetworkFile a = NetworkFile
    { nfCalculus :: String
    , nfNetwork  :: Network [String] (Set.Set a)
    } deriving (Eq, Ord, Read, Show)

eNetworkFile = NetworkFile
    { nfCalculus = ""
    , nfNetwork  = eNetwork
    }

