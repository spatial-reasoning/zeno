module Main  where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

data Huhu = Huhu { one :: Integer, composition :: Map.Map (String,String) String } deriving (Show)

hihi :: Huhu
hihi = Huhu { one = 1, composition = Map.fromList [(("eins","zwei"), "drei"), (("vier","fünf"), "sechs")] }

bla = Map.fromList [(5,Set.fromList [3,4,5,6]), (3,Set.fromList [1,2,3,4,5])]
ble = Set.fromList [1,3,4]
blu = Set.fromList [1,3,4]

hey x = [ n | n <- [1..x], mod x n == 0 ]
