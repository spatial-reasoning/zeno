module Calculus.Opra4
    ( module Calculus.Opra
    , Opra4(..)
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra

data Opra4 = Opra4_0_0   | Opra4_0_1   | Opra4_0_2   | Opra4_0_3
           | Opra4_0_4   | Opra4_0_5   | Opra4_0_6   | Opra4_0_7
           | Opra4_0_8   | Opra4_0_9   | Opra4_0_10  | Opra4_0_11
           | Opra4_0_12  | Opra4_0_13  | Opra4_0_14  | Opra4_0_15
           | Opra4_1_0   | Opra4_1_1   | Opra4_1_2   | Opra4_1_3
           | Opra4_1_4   | Opra4_1_5   | Opra4_1_6   | Opra4_1_7
           | Opra4_1_8   | Opra4_1_9   | Opra4_1_10  | Opra4_1_11
           | Opra4_1_12  | Opra4_1_13  | Opra4_1_14  | Opra4_1_15
           | Opra4_2_0   | Opra4_2_1   | Opra4_2_2   | Opra4_2_3
           | Opra4_2_4   | Opra4_2_5   | Opra4_2_6   | Opra4_2_7
           | Opra4_2_8   | Opra4_2_9   | Opra4_2_10  | Opra4_2_11
           | Opra4_2_12  | Opra4_2_13  | Opra4_2_14  | Opra4_2_15
           | Opra4_3_0   | Opra4_3_1   | Opra4_3_2   | Opra4_3_3
           | Opra4_3_4   | Opra4_3_5   | Opra4_3_6   | Opra4_3_7
           | Opra4_3_8   | Opra4_3_9   | Opra4_3_10  | Opra4_3_11
           | Opra4_3_12  | Opra4_3_13  | Opra4_3_14  | Opra4_3_15
           | Opra4_4_0   | Opra4_4_1   | Opra4_4_2   | Opra4_4_3
           | Opra4_4_4   | Opra4_4_5   | Opra4_4_6   | Opra4_4_7
           | Opra4_4_8   | Opra4_4_9   | Opra4_4_10  | Opra4_4_11
           | Opra4_4_12  | Opra4_4_13  | Opra4_4_14  | Opra4_4_15
           | Opra4_5_0   | Opra4_5_1   | Opra4_5_2   | Opra4_5_3
           | Opra4_5_4   | Opra4_5_5   | Opra4_5_6   | Opra4_5_7
           | Opra4_5_8   | Opra4_5_9   | Opra4_5_10  | Opra4_5_11
           | Opra4_5_12  | Opra4_5_13  | Opra4_5_14  | Opra4_5_15
           | Opra4_6_0   | Opra4_6_1   | Opra4_6_2   | Opra4_6_3
           | Opra4_6_4   | Opra4_6_5   | Opra4_6_6   | Opra4_6_7
           | Opra4_6_8   | Opra4_6_9   | Opra4_6_10  | Opra4_6_11
           | Opra4_6_12  | Opra4_6_13  | Opra4_6_14  | Opra4_6_15
           | Opra4_7_0   | Opra4_7_1   | Opra4_7_2   | Opra4_7_3
           | Opra4_7_4   | Opra4_7_5   | Opra4_7_6   | Opra4_7_7
           | Opra4_7_8   | Opra4_7_9   | Opra4_7_10  | Opra4_7_11
           | Opra4_7_12  | Opra4_7_13  | Opra4_7_14  | Opra4_7_15
           | Opra4_8_0   | Opra4_8_1   | Opra4_8_2   | Opra4_8_3
           | Opra4_8_4   | Opra4_8_5   | Opra4_8_6   | Opra4_8_7
           | Opra4_8_8   | Opra4_8_9   | Opra4_8_10  | Opra4_8_11
           | Opra4_8_12  | Opra4_8_13  | Opra4_8_14  | Opra4_8_15
           | Opra4_9_0   | Opra4_9_1   | Opra4_9_2   | Opra4_9_3
           | Opra4_9_4   | Opra4_9_5   | Opra4_9_6   | Opra4_9_7
           | Opra4_9_8   | Opra4_9_9   | Opra4_9_10  | Opra4_9_11
           | Opra4_9_12  | Opra4_9_13  | Opra4_9_14  | Opra4_9_15
           | Opra4_10_0  | Opra4_10_1  | Opra4_10_2  | Opra4_10_3
           | Opra4_10_4  | Opra4_10_5  | Opra4_10_6  | Opra4_10_7
           | Opra4_10_8  | Opra4_10_9  | Opra4_10_10 | Opra4_10_11
           | Opra4_10_12 | Opra4_10_13 | Opra4_10_14 | Opra4_10_15
           | Opra4_11_0  | Opra4_11_1  | Opra4_11_2  | Opra4_11_3
           | Opra4_11_4  | Opra4_11_5  | Opra4_11_6  | Opra4_11_7
           | Opra4_11_8  | Opra4_11_9  | Opra4_11_10 | Opra4_11_11
           | Opra4_11_12 | Opra4_11_13 | Opra4_11_14 | Opra4_11_15
           | Opra4_12_0  | Opra4_12_1  | Opra4_12_2  | Opra4_12_3
           | Opra4_12_4  | Opra4_12_5  | Opra4_12_6  | Opra4_12_7
           | Opra4_12_8  | Opra4_12_9  | Opra4_12_10 | Opra4_12_11
           | Opra4_12_12 | Opra4_12_13 | Opra4_12_14 | Opra4_12_15
           | Opra4_13_0  | Opra4_13_1  | Opra4_13_2  | Opra4_13_3
           | Opra4_13_4  | Opra4_13_5  | Opra4_13_6  | Opra4_13_7
           | Opra4_13_8  | Opra4_13_9  | Opra4_13_10 | Opra4_13_11
           | Opra4_13_12 | Opra4_13_13 | Opra4_13_14 | Opra4_13_15
           | Opra4_14_0  | Opra4_14_1  | Opra4_14_2  | Opra4_14_3
           | Opra4_14_4  | Opra4_14_5  | Opra4_14_6  | Opra4_14_7
           | Opra4_14_8  | Opra4_14_9  | Opra4_14_10 | Opra4_14_11
           | Opra4_14_12 | Opra4_14_13 | Opra4_14_14 | Opra4_14_15
           | Opra4_15_0  | Opra4_15_1  | Opra4_15_2  | Opra4_15_3
           | Opra4_15_4  | Opra4_15_5  | Opra4_15_6  | Opra4_15_7
           | Opra4_15_8  | Opra4_15_9  | Opra4_15_10 | Opra4_15_11
           | Opra4_15_12 | Opra4_15_13 | Opra4_15_14 | Opra4_15_15
           | Opra4_s_0   | Opra4_s_1   | Opra4_s_2   | Opra4_s_3
           | Opra4_s_4   | Opra4_s_5   | Opra4_s_6   | Opra4_s_7
           | Opra4_s_8   | Opra4_s_9   | Opra4_s_10  | Opra4_s_11
           | Opra4_s_12  | Opra4_s_13  | Opra4_s_14  | Opra4_s_15
           deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Opram Opra4 where
    m _ = 4

instance Calculus Opra4 where
    rank _ = 2
    calculus _ = "opra-4"

    readRel = readOpram
    showRel = showOpram

    sparqifyRel = sparqifyOpram
    gqrifyRel   = sparqifyOpram

    cBaserelationsArealList = areal cBaserelationsList
    cBaserelationsNonArealList = nonAreal cBaserelationsList

    bcConvert = opraConvert 4


