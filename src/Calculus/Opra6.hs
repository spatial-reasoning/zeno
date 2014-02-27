module Calculus.Opra6
    ( module Calculus.Opra
    , Opra6(..)
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra

data Opra6 = Opra6_0_0   | Opra6_0_1   | Opra6_0_2   | Opra6_0_3  
           | Opra6_0_4   | Opra6_0_5   | Opra6_0_6   | Opra6_0_7  
           | Opra6_0_8   | Opra6_0_9   | Opra6_0_10  | Opra6_0_11 
           | Opra6_0_12  | Opra6_0_13  | Opra6_0_14  | Opra6_0_15 
           | Opra6_0_16  | Opra6_0_17  | Opra6_0_18  | Opra6_0_19 
           | Opra6_0_20  | Opra6_0_21  | Opra6_0_22  | Opra6_0_23 
           | Opra6_1_0   | Opra6_1_1   | Opra6_1_2   | Opra6_1_3  
           | Opra6_1_4   | Opra6_1_5   | Opra6_1_6   | Opra6_1_7  
           | Opra6_1_8   | Opra6_1_9   | Opra6_1_10  | Opra6_1_11 
           | Opra6_1_12  | Opra6_1_13  | Opra6_1_14  | Opra6_1_15 
           | Opra6_1_16  | Opra6_1_17  | Opra6_1_18  | Opra6_1_19 
           | Opra6_1_20  | Opra6_1_21  | Opra6_1_22  | Opra6_1_23 
           | Opra6_2_0   | Opra6_2_1   | Opra6_2_2   | Opra6_2_3  
           | Opra6_2_4   | Opra6_2_5   | Opra6_2_6   | Opra6_2_7  
           | Opra6_2_8   | Opra6_2_9   | Opra6_2_10  | Opra6_2_11 
           | Opra6_2_12  | Opra6_2_13  | Opra6_2_14  | Opra6_2_15 
           | Opra6_2_16  | Opra6_2_17  | Opra6_2_18  | Opra6_2_19 
           | Opra6_2_20  | Opra6_2_21  | Opra6_2_22  | Opra6_2_23 
           | Opra6_3_0   | Opra6_3_1   | Opra6_3_2   | Opra6_3_3  
           | Opra6_3_4   | Opra6_3_5   | Opra6_3_6   | Opra6_3_7  
           | Opra6_3_8   | Opra6_3_9   | Opra6_3_10  | Opra6_3_11 
           | Opra6_3_12  | Opra6_3_13  | Opra6_3_14  | Opra6_3_15 
           | Opra6_3_16  | Opra6_3_17  | Opra6_3_18  | Opra6_3_19 
           | Opra6_3_20  | Opra6_3_21  | Opra6_3_22  | Opra6_3_23 
           | Opra6_4_0   | Opra6_4_1   | Opra6_4_2   | Opra6_4_3  
           | Opra6_4_4   | Opra6_4_5   | Opra6_4_6   | Opra6_4_7  
           | Opra6_4_8   | Opra6_4_9   | Opra6_4_10  | Opra6_4_11 
           | Opra6_4_12  | Opra6_4_13  | Opra6_4_14  | Opra6_4_15 
           | Opra6_4_16  | Opra6_4_17  | Opra6_4_18  | Opra6_4_19 
           | Opra6_4_20  | Opra6_4_21  | Opra6_4_22  | Opra6_4_23 
           | Opra6_5_0   | Opra6_5_1   | Opra6_5_2   | Opra6_5_3  
           | Opra6_5_4   | Opra6_5_5   | Opra6_5_6   | Opra6_5_7  
           | Opra6_5_8   | Opra6_5_9   | Opra6_5_10  | Opra6_5_11 
           | Opra6_5_12  | Opra6_5_13  | Opra6_5_14  | Opra6_5_15 
           | Opra6_5_16  | Opra6_5_17  | Opra6_5_18  | Opra6_5_19 
           | Opra6_5_20  | Opra6_5_21  | Opra6_5_22  | Opra6_5_23 
           | Opra6_6_0   | Opra6_6_1   | Opra6_6_2   | Opra6_6_3  
           | Opra6_6_4   | Opra6_6_5   | Opra6_6_6   | Opra6_6_7  
           | Opra6_6_8   | Opra6_6_9   | Opra6_6_10  | Opra6_6_11 
           | Opra6_6_12  | Opra6_6_13  | Opra6_6_14  | Opra6_6_15 
           | Opra6_6_16  | Opra6_6_17  | Opra6_6_18  | Opra6_6_19 
           | Opra6_6_20  | Opra6_6_21  | Opra6_6_22  | Opra6_6_23 
           | Opra6_7_0   | Opra6_7_1   | Opra6_7_2   | Opra6_7_3  
           | Opra6_7_4   | Opra6_7_5   | Opra6_7_6   | Opra6_7_7  
           | Opra6_7_8   | Opra6_7_9   | Opra6_7_10  | Opra6_7_11 
           | Opra6_7_12  | Opra6_7_13  | Opra6_7_14  | Opra6_7_15 
           | Opra6_7_16  | Opra6_7_17  | Opra6_7_18  | Opra6_7_19 
           | Opra6_7_20  | Opra6_7_21  | Opra6_7_22  | Opra6_7_23 
           | Opra6_8_0   | Opra6_8_1   | Opra6_8_2   | Opra6_8_3  
           | Opra6_8_4   | Opra6_8_5   | Opra6_8_6   | Opra6_8_7  
           | Opra6_8_8   | Opra6_8_9   | Opra6_8_10  | Opra6_8_11 
           | Opra6_8_12  | Opra6_8_13  | Opra6_8_14  | Opra6_8_15 
           | Opra6_8_16  | Opra6_8_17  | Opra6_8_18  | Opra6_8_19 
           | Opra6_8_20  | Opra6_8_21  | Opra6_8_22  | Opra6_8_23 
           | Opra6_9_0   | Opra6_9_1   | Opra6_9_2   | Opra6_9_3  
           | Opra6_9_4   | Opra6_9_5   | Opra6_9_6   | Opra6_9_7  
           | Opra6_9_8   | Opra6_9_9   | Opra6_9_10  | Opra6_9_11 
           | Opra6_9_12  | Opra6_9_13  | Opra6_9_14  | Opra6_9_15 
           | Opra6_9_16  | Opra6_9_17  | Opra6_9_18  | Opra6_9_19 
           | Opra6_9_20  | Opra6_9_21  | Opra6_9_22  | Opra6_9_23 
           | Opra6_10_0  | Opra6_10_1  | Opra6_10_2  | Opra6_10_3 
           | Opra6_10_4  | Opra6_10_5  | Opra6_10_6  | Opra6_10_7 
           | Opra6_10_8  | Opra6_10_9  | Opra6_10_10 | Opra6_10_11
           | Opra6_10_12 | Opra6_10_13 | Opra6_10_14 | Opra6_10_15
           | Opra6_10_16 | Opra6_10_17 | Opra6_10_18 | Opra6_10_19
           | Opra6_10_20 | Opra6_10_21 | Opra6_10_22 | Opra6_10_23
           | Opra6_11_0  | Opra6_11_1  | Opra6_11_2  | Opra6_11_3 
           | Opra6_11_4  | Opra6_11_5  | Opra6_11_6  | Opra6_11_7 
           | Opra6_11_8  | Opra6_11_9  | Opra6_11_10 | Opra6_11_11
           | Opra6_11_12 | Opra6_11_13 | Opra6_11_14 | Opra6_11_15
           | Opra6_11_16 | Opra6_11_17 | Opra6_11_18 | Opra6_11_19
           | Opra6_11_20 | Opra6_11_21 | Opra6_11_22 | Opra6_11_23
           | Opra6_12_0  | Opra6_12_1  | Opra6_12_2  | Opra6_12_3 
           | Opra6_12_4  | Opra6_12_5  | Opra6_12_6  | Opra6_12_7 
           | Opra6_12_8  | Opra6_12_9  | Opra6_12_10 | Opra6_12_11
           | Opra6_12_12 | Opra6_12_13 | Opra6_12_14 | Opra6_12_15
           | Opra6_12_16 | Opra6_12_17 | Opra6_12_18 | Opra6_12_19
           | Opra6_12_20 | Opra6_12_21 | Opra6_12_22 | Opra6_12_23
           | Opra6_13_0  | Opra6_13_1  | Opra6_13_2  | Opra6_13_3 
           | Opra6_13_4  | Opra6_13_5  | Opra6_13_6  | Opra6_13_7 
           | Opra6_13_8  | Opra6_13_9  | Opra6_13_10 | Opra6_13_11
           | Opra6_13_12 | Opra6_13_13 | Opra6_13_14 | Opra6_13_15
           | Opra6_13_16 | Opra6_13_17 | Opra6_13_18 | Opra6_13_19
           | Opra6_13_20 | Opra6_13_21 | Opra6_13_22 | Opra6_13_23
           | Opra6_14_0  | Opra6_14_1  | Opra6_14_2  | Opra6_14_3 
           | Opra6_14_4  | Opra6_14_5  | Opra6_14_6  | Opra6_14_7 
           | Opra6_14_8  | Opra6_14_9  | Opra6_14_10 | Opra6_14_11
           | Opra6_14_12 | Opra6_14_13 | Opra6_14_14 | Opra6_14_15
           | Opra6_14_16 | Opra6_14_17 | Opra6_14_18 | Opra6_14_19
           | Opra6_14_20 | Opra6_14_21 | Opra6_14_22 | Opra6_14_23
           | Opra6_15_0  | Opra6_15_1  | Opra6_15_2  | Opra6_15_3 
           | Opra6_15_4  | Opra6_15_5  | Opra6_15_6  | Opra6_15_7 
           | Opra6_15_8  | Opra6_15_9  | Opra6_15_10 | Opra6_15_11
           | Opra6_15_12 | Opra6_15_13 | Opra6_15_14 | Opra6_15_15
           | Opra6_15_16 | Opra6_15_17 | Opra6_15_18 | Opra6_15_19
           | Opra6_15_20 | Opra6_15_21 | Opra6_15_22 | Opra6_15_23
           | Opra6_16_0  | Opra6_16_1  | Opra6_16_2  | Opra6_16_3 
           | Opra6_16_4  | Opra6_16_5  | Opra6_16_6  | Opra6_16_7 
           | Opra6_16_8  | Opra6_16_9  | Opra6_16_10 | Opra6_16_11
           | Opra6_16_12 | Opra6_16_13 | Opra6_16_14 | Opra6_16_15
           | Opra6_16_16 | Opra6_16_17 | Opra6_16_18 | Opra6_16_19
           | Opra6_16_20 | Opra6_16_21 | Opra6_16_22 | Opra6_16_23
           | Opra6_17_0  | Opra6_17_1  | Opra6_17_2  | Opra6_17_3 
           | Opra6_17_4  | Opra6_17_5  | Opra6_17_6  | Opra6_17_7 
           | Opra6_17_8  | Opra6_17_9  | Opra6_17_10 | Opra6_17_11
           | Opra6_17_12 | Opra6_17_13 | Opra6_17_14 | Opra6_17_15
           | Opra6_17_16 | Opra6_17_17 | Opra6_17_18 | Opra6_17_19
           | Opra6_17_20 | Opra6_17_21 | Opra6_17_22 | Opra6_17_23
           | Opra6_18_0  | Opra6_18_1  | Opra6_18_2  | Opra6_18_3 
           | Opra6_18_4  | Opra6_18_5  | Opra6_18_6  | Opra6_18_7 
           | Opra6_18_8  | Opra6_18_9  | Opra6_18_10 | Opra6_18_11
           | Opra6_18_12 | Opra6_18_13 | Opra6_18_14 | Opra6_18_15
           | Opra6_18_16 | Opra6_18_17 | Opra6_18_18 | Opra6_18_19
           | Opra6_18_20 | Opra6_18_21 | Opra6_18_22 | Opra6_18_23
           | Opra6_19_0  | Opra6_19_1  | Opra6_19_2  | Opra6_19_3 
           | Opra6_19_4  | Opra6_19_5  | Opra6_19_6  | Opra6_19_7 
           | Opra6_19_8  | Opra6_19_9  | Opra6_19_10 | Opra6_19_11
           | Opra6_19_12 | Opra6_19_13 | Opra6_19_14 | Opra6_19_15
           | Opra6_19_16 | Opra6_19_17 | Opra6_19_18 | Opra6_19_19
           | Opra6_19_20 | Opra6_19_21 | Opra6_19_22 | Opra6_19_23
           | Opra6_20_0  | Opra6_20_1  | Opra6_20_2  | Opra6_20_3 
           | Opra6_20_4  | Opra6_20_5  | Opra6_20_6  | Opra6_20_7 
           | Opra6_20_8  | Opra6_20_9  | Opra6_20_10 | Opra6_20_11
           | Opra6_20_12 | Opra6_20_13 | Opra6_20_14 | Opra6_20_15
           | Opra6_20_16 | Opra6_20_17 | Opra6_20_18 | Opra6_20_19
           | Opra6_20_20 | Opra6_20_21 | Opra6_20_22 | Opra6_20_23
           | Opra6_21_0  | Opra6_21_1  | Opra6_21_2  | Opra6_21_3 
           | Opra6_21_4  | Opra6_21_5  | Opra6_21_6  | Opra6_21_7 
           | Opra6_21_8  | Opra6_21_9  | Opra6_21_10 | Opra6_21_11
           | Opra6_21_12 | Opra6_21_13 | Opra6_21_14 | Opra6_21_15
           | Opra6_21_16 | Opra6_21_17 | Opra6_21_18 | Opra6_21_19
           | Opra6_21_20 | Opra6_21_21 | Opra6_21_22 | Opra6_21_23
           | Opra6_22_0  | Opra6_22_1  | Opra6_22_2  | Opra6_22_3 
           | Opra6_22_4  | Opra6_22_5  | Opra6_22_6  | Opra6_22_7 
           | Opra6_22_8  | Opra6_22_9  | Opra6_22_10 | Opra6_22_11
           | Opra6_22_12 | Opra6_22_13 | Opra6_22_14 | Opra6_22_15
           | Opra6_22_16 | Opra6_22_17 | Opra6_22_18 | Opra6_22_19
           | Opra6_22_20 | Opra6_22_21 | Opra6_22_22 | Opra6_22_23
           | Opra6_23_0  | Opra6_23_1  | Opra6_23_2  | Opra6_23_3 
           | Opra6_23_4  | Opra6_23_5  | Opra6_23_6  | Opra6_23_7 
           | Opra6_23_8  | Opra6_23_9  | Opra6_23_10 | Opra6_23_11
           | Opra6_23_12 | Opra6_23_13 | Opra6_23_14 | Opra6_23_15
           | Opra6_23_16 | Opra6_23_17 | Opra6_23_18 | Opra6_23_19
           | Opra6_23_20 | Opra6_23_21 | Opra6_23_22 | Opra6_23_23
           | Opra6_s_0   | Opra6_s_1   | Opra6_s_2   | Opra6_s_3  
           | Opra6_s_4   | Opra6_s_5   | Opra6_s_6   | Opra6_s_7  
           | Opra6_s_8   | Opra6_s_9   | Opra6_s_10  | Opra6_s_11 
           | Opra6_s_12  | Opra6_s_13  | Opra6_s_14  | Opra6_s_15 
           | Opra6_s_16  | Opra6_s_17  | Opra6_s_18  | Opra6_s_19 
           | Opra6_s_20  | Opra6_s_21  | Opra6_s_22  | Opra6_s_23 
           deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Opram Opra6 where
    m _ = 6

instance Calculus Opra6 where
    rank _ = 2
    cName _ = "opra-6"
    cNameGqr _ = "opra6"

    cReadRel = readOpram
    cShowRel = showOpram

    cSparqifyRel = sparqifyOpram
    cGqrifyRel   = sparqifyOpram

    cBaserelationsArealList = areal cBaserelationsList
    cBaserelationsNonArealList = nonAreal cBaserelationsList
    cBaserelationsSameList     = same     cBaserelationsList
    cBaserelationsNonSameList  = nonSame  cBaserelationsList

    bcConvert = opraConvert 6


