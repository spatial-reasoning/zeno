module Calculus.Opra3
    ( module Calculus.Opra
    , Opra3(..)
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra

data Opra3 = Opra3_0_0   | Opra3_0_1   | Opra3_0_2   | Opra3_0_3   | Opra3_0_4
           | Opra3_0_5   | Opra3_0_6   | Opra3_0_7   | Opra3_0_8   | Opra3_0_9
           | Opra3_0_10  | Opra3_0_11  | Opra3_1_0   | Opra3_1_1   | Opra3_1_2
           | Opra3_1_3   | Opra3_1_4   | Opra3_1_5   | Opra3_1_6   | Opra3_1_7
           | Opra3_1_8   | Opra3_1_9   | Opra3_1_10  | Opra3_1_11  | Opra3_2_0
           | Opra3_2_1   | Opra3_2_2   | Opra3_2_3   | Opra3_2_4   | Opra3_2_5
           | Opra3_2_6   | Opra3_2_7   | Opra3_2_8   | Opra3_2_9   | Opra3_2_10
           | Opra3_2_11  | Opra3_3_0   | Opra3_3_1   | Opra3_3_2   | Opra3_3_3
           | Opra3_3_4   | Opra3_3_5   | Opra3_3_6   | Opra3_3_7   | Opra3_3_8
           | Opra3_3_9   | Opra3_3_10  | Opra3_3_11  | Opra3_4_0   | Opra3_4_1
           | Opra3_4_2   | Opra3_4_3   | Opra3_4_4   | Opra3_4_5   | Opra3_4_6
           | Opra3_4_7   | Opra3_4_8   | Opra3_4_9   | Opra3_4_10  | Opra3_4_11
           | Opra3_5_0   | Opra3_5_1   | Opra3_5_2   | Opra3_5_3   | Opra3_5_4
           | Opra3_5_5   | Opra3_5_6   | Opra3_5_7   | Opra3_5_8   | Opra3_5_9
           | Opra3_5_10  | Opra3_5_11  | Opra3_6_0   | Opra3_6_1   | Opra3_6_2
           | Opra3_6_3   | Opra3_6_4   | Opra3_6_5   | Opra3_6_6   | Opra3_6_7
           | Opra3_6_8   | Opra3_6_9   | Opra3_6_10  | Opra3_6_11  | Opra3_7_0
           | Opra3_7_1   | Opra3_7_2   | Opra3_7_3   | Opra3_7_4   | Opra3_7_5
           | Opra3_7_6   | Opra3_7_7   | Opra3_7_8   | Opra3_7_9   | Opra3_7_10
           | Opra3_7_11  | Opra3_8_0   | Opra3_8_1   | Opra3_8_2   | Opra3_8_3
           | Opra3_8_4   | Opra3_8_5   | Opra3_8_6   | Opra3_8_7   | Opra3_8_8
           | Opra3_8_9   | Opra3_8_10  | Opra3_8_11  | Opra3_9_0   | Opra3_9_1
           | Opra3_9_2   | Opra3_9_3   | Opra3_9_4   | Opra3_9_5   | Opra3_9_6
           | Opra3_9_7   | Opra3_9_8   | Opra3_9_9   | Opra3_9_10  | Opra3_9_11
           | Opra3_10_0  | Opra3_10_1  | Opra3_10_2  | Opra3_10_3  | Opra3_10_4
           | Opra3_10_5  | Opra3_10_6  | Opra3_10_7  | Opra3_10_8  | Opra3_10_9
           | Opra3_10_10 | Opra3_10_11 | Opra3_11_0  | Opra3_11_1  | Opra3_11_2
           | Opra3_11_3  | Opra3_11_4  | Opra3_11_5  | Opra3_11_6  | Opra3_11_7
           | Opra3_11_8  | Opra3_11_9  | Opra3_11_10 | Opra3_11_11 | Opra3_s_0
           | Opra3_s_1   | Opra3_s_2   | Opra3_s_3   | Opra3_s_4   | Opra3_s_5
           | Opra3_s_6   | Opra3_s_7   | Opra3_s_8   | Opra3_s_9   | Opra3_s_10
           | Opra3_s_11
           deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Opram Opra3 where
    m _ = 3

instance Calculus Opra3 where
    rank _ = 2
    cName _ = "opra-3"
    cNameGqr _ ="opra3"

    cReadRel = readOpram
    cShowRel = showOpram

    cSparqifyRel = sparqifyOpram
    cGqrifyRel   = sparqifyOpram

    cBaserelationsArealList = areal cBaserelationsList
    cBaserelationsNonArealList = nonAreal cBaserelationsList

    bcConvert = opraConvert 3

