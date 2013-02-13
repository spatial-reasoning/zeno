module Calculus.Opra8
    ( module Calculus.Opra
    , Opra8(..)
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra

data Opra8 = Opra8_0_0   | Opra8_0_1   | Opra8_0_2   | Opra8_0_3  
           | Opra8_0_4   | Opra8_0_5   | Opra8_0_6   | Opra8_0_7  
           | Opra8_0_8   | Opra8_0_9   | Opra8_0_10  | Opra8_0_11 
           | Opra8_0_12  | Opra8_0_13  | Opra8_0_14  | Opra8_0_15 
           | Opra8_0_16  | Opra8_0_17  | Opra8_0_18  | Opra8_0_19 
           | Opra8_0_20  | Opra8_0_21  | Opra8_0_22  | Opra8_0_23 
           | Opra8_0_24  | Opra8_0_25  | Opra8_0_26  | Opra8_0_27 
           | Opra8_0_28  | Opra8_0_29  | Opra8_0_30  | Opra8_0_31 
           | Opra8_1_0   | Opra8_1_1   | Opra8_1_2   | Opra8_1_3  
           | Opra8_1_4   | Opra8_1_5   | Opra8_1_6   | Opra8_1_7  
           | Opra8_1_8   | Opra8_1_9   | Opra8_1_10  | Opra8_1_11 
           | Opra8_1_12  | Opra8_1_13  | Opra8_1_14  | Opra8_1_15 
           | Opra8_1_16  | Opra8_1_17  | Opra8_1_18  | Opra8_1_19 
           | Opra8_1_20  | Opra8_1_21  | Opra8_1_22  | Opra8_1_23 
           | Opra8_1_24  | Opra8_1_25  | Opra8_1_26  | Opra8_1_27 
           | Opra8_1_28  | Opra8_1_29  | Opra8_1_30  | Opra8_1_31 
           | Opra8_2_0   | Opra8_2_1   | Opra8_2_2   | Opra8_2_3  
           | Opra8_2_4   | Opra8_2_5   | Opra8_2_6   | Opra8_2_7  
           | Opra8_2_8   | Opra8_2_9   | Opra8_2_10  | Opra8_2_11 
           | Opra8_2_12  | Opra8_2_13  | Opra8_2_14  | Opra8_2_15 
           | Opra8_2_16  | Opra8_2_17  | Opra8_2_18  | Opra8_2_19 
           | Opra8_2_20  | Opra8_2_21  | Opra8_2_22  | Opra8_2_23 
           | Opra8_2_24  | Opra8_2_25  | Opra8_2_26  | Opra8_2_27 
           | Opra8_2_28  | Opra8_2_29  | Opra8_2_30  | Opra8_2_31 
           | Opra8_3_0   | Opra8_3_1   | Opra8_3_2   | Opra8_3_3  
           | Opra8_3_4   | Opra8_3_5   | Opra8_3_6   | Opra8_3_7  
           | Opra8_3_8   | Opra8_3_9   | Opra8_3_10  | Opra8_3_11 
           | Opra8_3_12  | Opra8_3_13  | Opra8_3_14  | Opra8_3_15 
           | Opra8_3_16  | Opra8_3_17  | Opra8_3_18  | Opra8_3_19 
           | Opra8_3_20  | Opra8_3_21  | Opra8_3_22  | Opra8_3_23 
           | Opra8_3_24  | Opra8_3_25  | Opra8_3_26  | Opra8_3_27 
           | Opra8_3_28  | Opra8_3_29  | Opra8_3_30  | Opra8_3_31 
           | Opra8_4_0   | Opra8_4_1   | Opra8_4_2   | Opra8_4_3  
           | Opra8_4_4   | Opra8_4_5   | Opra8_4_6   | Opra8_4_7  
           | Opra8_4_8   | Opra8_4_9   | Opra8_4_10  | Opra8_4_11 
           | Opra8_4_12  | Opra8_4_13  | Opra8_4_14  | Opra8_4_15 
           | Opra8_4_16  | Opra8_4_17  | Opra8_4_18  | Opra8_4_19 
           | Opra8_4_20  | Opra8_4_21  | Opra8_4_22  | Opra8_4_23 
           | Opra8_4_24  | Opra8_4_25  | Opra8_4_26  | Opra8_4_27 
           | Opra8_4_28  | Opra8_4_29  | Opra8_4_30  | Opra8_4_31 
           | Opra8_5_0   | Opra8_5_1   | Opra8_5_2   | Opra8_5_3  
           | Opra8_5_4   | Opra8_5_5   | Opra8_5_6   | Opra8_5_7  
           | Opra8_5_8   | Opra8_5_9   | Opra8_5_10  | Opra8_5_11 
           | Opra8_5_12  | Opra8_5_13  | Opra8_5_14  | Opra8_5_15 
           | Opra8_5_16  | Opra8_5_17  | Opra8_5_18  | Opra8_5_19 
           | Opra8_5_20  | Opra8_5_21  | Opra8_5_22  | Opra8_5_23 
           | Opra8_5_24  | Opra8_5_25  | Opra8_5_26  | Opra8_5_27 
           | Opra8_5_28  | Opra8_5_29  | Opra8_5_30  | Opra8_5_31 
           | Opra8_6_0   | Opra8_6_1   | Opra8_6_2   | Opra8_6_3  
           | Opra8_6_4   | Opra8_6_5   | Opra8_6_6   | Opra8_6_7  
           | Opra8_6_8   | Opra8_6_9   | Opra8_6_10  | Opra8_6_11 
           | Opra8_6_12  | Opra8_6_13  | Opra8_6_14  | Opra8_6_15 
           | Opra8_6_16  | Opra8_6_17  | Opra8_6_18  | Opra8_6_19 
           | Opra8_6_20  | Opra8_6_21  | Opra8_6_22  | Opra8_6_23 
           | Opra8_6_24  | Opra8_6_25  | Opra8_6_26  | Opra8_6_27 
           | Opra8_6_28  | Opra8_6_29  | Opra8_6_30  | Opra8_6_31 
           | Opra8_7_0   | Opra8_7_1   | Opra8_7_2   | Opra8_7_3  
           | Opra8_7_4   | Opra8_7_5   | Opra8_7_6   | Opra8_7_7  
           | Opra8_7_8   | Opra8_7_9   | Opra8_7_10  | Opra8_7_11 
           | Opra8_7_12  | Opra8_7_13  | Opra8_7_14  | Opra8_7_15 
           | Opra8_7_16  | Opra8_7_17  | Opra8_7_18  | Opra8_7_19 
           | Opra8_7_20  | Opra8_7_21  | Opra8_7_22  | Opra8_7_23 
           | Opra8_7_24  | Opra8_7_25  | Opra8_7_26  | Opra8_7_27 
           | Opra8_7_28  | Opra8_7_29  | Opra8_7_30  | Opra8_7_31 
           | Opra8_8_0   | Opra8_8_1   | Opra8_8_2   | Opra8_8_3  
           | Opra8_8_4   | Opra8_8_5   | Opra8_8_6   | Opra8_8_7  
           | Opra8_8_8   | Opra8_8_9   | Opra8_8_10  | Opra8_8_11 
           | Opra8_8_12  | Opra8_8_13  | Opra8_8_14  | Opra8_8_15 
           | Opra8_8_16  | Opra8_8_17  | Opra8_8_18  | Opra8_8_19 
           | Opra8_8_20  | Opra8_8_21  | Opra8_8_22  | Opra8_8_23 
           | Opra8_8_24  | Opra8_8_25  | Opra8_8_26  | Opra8_8_27 
           | Opra8_8_28  | Opra8_8_29  | Opra8_8_30  | Opra8_8_31 
           | Opra8_9_0   | Opra8_9_1   | Opra8_9_2   | Opra8_9_3  
           | Opra8_9_4   | Opra8_9_5   | Opra8_9_6   | Opra8_9_7  
           | Opra8_9_8   | Opra8_9_9   | Opra8_9_10  | Opra8_9_11 
           | Opra8_9_12  | Opra8_9_13  | Opra8_9_14  | Opra8_9_15 
           | Opra8_9_16  | Opra8_9_17  | Opra8_9_18  | Opra8_9_19 
           | Opra8_9_20  | Opra8_9_21  | Opra8_9_22  | Opra8_9_23 
           | Opra8_9_24  | Opra8_9_25  | Opra8_9_26  | Opra8_9_27 
           | Opra8_9_28  | Opra8_9_29  | Opra8_9_30  | Opra8_9_31 
           | Opra8_10_0  | Opra8_10_1  | Opra8_10_2  | Opra8_10_3 
           | Opra8_10_4  | Opra8_10_5  | Opra8_10_6  | Opra8_10_7 
           | Opra8_10_8  | Opra8_10_9  | Opra8_10_10 | Opra8_10_11
           | Opra8_10_12 | Opra8_10_13 | Opra8_10_14 | Opra8_10_15
           | Opra8_10_16 | Opra8_10_17 | Opra8_10_18 | Opra8_10_19
           | Opra8_10_20 | Opra8_10_21 | Opra8_10_22 | Opra8_10_23
           | Opra8_10_24 | Opra8_10_25 | Opra8_10_26 | Opra8_10_27
           | Opra8_10_28 | Opra8_10_29 | Opra8_10_30 | Opra8_10_31
           | Opra8_11_0  | Opra8_11_1  | Opra8_11_2  | Opra8_11_3 
           | Opra8_11_4  | Opra8_11_5  | Opra8_11_6  | Opra8_11_7 
           | Opra8_11_8  | Opra8_11_9  | Opra8_11_10 | Opra8_11_11
           | Opra8_11_12 | Opra8_11_13 | Opra8_11_14 | Opra8_11_15
           | Opra8_11_16 | Opra8_11_17 | Opra8_11_18 | Opra8_11_19
           | Opra8_11_20 | Opra8_11_21 | Opra8_11_22 | Opra8_11_23
           | Opra8_11_24 | Opra8_11_25 | Opra8_11_26 | Opra8_11_27
           | Opra8_11_28 | Opra8_11_29 | Opra8_11_30 | Opra8_11_31
           | Opra8_12_0  | Opra8_12_1  | Opra8_12_2  | Opra8_12_3 
           | Opra8_12_4  | Opra8_12_5  | Opra8_12_6  | Opra8_12_7 
           | Opra8_12_8  | Opra8_12_9  | Opra8_12_10 | Opra8_12_11
           | Opra8_12_12 | Opra8_12_13 | Opra8_12_14 | Opra8_12_15
           | Opra8_12_16 | Opra8_12_17 | Opra8_12_18 | Opra8_12_19
           | Opra8_12_20 | Opra8_12_21 | Opra8_12_22 | Opra8_12_23
           | Opra8_12_24 | Opra8_12_25 | Opra8_12_26 | Opra8_12_27
           | Opra8_12_28 | Opra8_12_29 | Opra8_12_30 | Opra8_12_31
           | Opra8_13_0  | Opra8_13_1  | Opra8_13_2  | Opra8_13_3 
           | Opra8_13_4  | Opra8_13_5  | Opra8_13_6  | Opra8_13_7 
           | Opra8_13_8  | Opra8_13_9  | Opra8_13_10 | Opra8_13_11
           | Opra8_13_12 | Opra8_13_13 | Opra8_13_14 | Opra8_13_15
           | Opra8_13_16 | Opra8_13_17 | Opra8_13_18 | Opra8_13_19
           | Opra8_13_20 | Opra8_13_21 | Opra8_13_22 | Opra8_13_23
           | Opra8_13_24 | Opra8_13_25 | Opra8_13_26 | Opra8_13_27
           | Opra8_13_28 | Opra8_13_29 | Opra8_13_30 | Opra8_13_31
           | Opra8_14_0  | Opra8_14_1  | Opra8_14_2  | Opra8_14_3 
           | Opra8_14_4  | Opra8_14_5  | Opra8_14_6  | Opra8_14_7 
           | Opra8_14_8  | Opra8_14_9  | Opra8_14_10 | Opra8_14_11
           | Opra8_14_12 | Opra8_14_13 | Opra8_14_14 | Opra8_14_15
           | Opra8_14_16 | Opra8_14_17 | Opra8_14_18 | Opra8_14_19
           | Opra8_14_20 | Opra8_14_21 | Opra8_14_22 | Opra8_14_23
           | Opra8_14_24 | Opra8_14_25 | Opra8_14_26 | Opra8_14_27
           | Opra8_14_28 | Opra8_14_29 | Opra8_14_30 | Opra8_14_31
           | Opra8_15_0  | Opra8_15_1  | Opra8_15_2  | Opra8_15_3 
           | Opra8_15_4  | Opra8_15_5  | Opra8_15_6  | Opra8_15_7 
           | Opra8_15_8  | Opra8_15_9  | Opra8_15_10 | Opra8_15_11
           | Opra8_15_12 | Opra8_15_13 | Opra8_15_14 | Opra8_15_15
           | Opra8_15_16 | Opra8_15_17 | Opra8_15_18 | Opra8_15_19
           | Opra8_15_20 | Opra8_15_21 | Opra8_15_22 | Opra8_15_23
           | Opra8_15_24 | Opra8_15_25 | Opra8_15_26 | Opra8_15_27
           | Opra8_15_28 | Opra8_15_29 | Opra8_15_30 | Opra8_15_31
           | Opra8_16_0  | Opra8_16_1  | Opra8_16_2  | Opra8_16_3 
           | Opra8_16_4  | Opra8_16_5  | Opra8_16_6  | Opra8_16_7 
           | Opra8_16_8  | Opra8_16_9  | Opra8_16_10 | Opra8_16_11
           | Opra8_16_12 | Opra8_16_13 | Opra8_16_14 | Opra8_16_15
           | Opra8_16_16 | Opra8_16_17 | Opra8_16_18 | Opra8_16_19
           | Opra8_16_20 | Opra8_16_21 | Opra8_16_22 | Opra8_16_23
           | Opra8_16_24 | Opra8_16_25 | Opra8_16_26 | Opra8_16_27
           | Opra8_16_28 | Opra8_16_29 | Opra8_16_30 | Opra8_16_31
           | Opra8_17_0  | Opra8_17_1  | Opra8_17_2  | Opra8_17_3 
           | Opra8_17_4  | Opra8_17_5  | Opra8_17_6  | Opra8_17_7 
           | Opra8_17_8  | Opra8_17_9  | Opra8_17_10 | Opra8_17_11
           | Opra8_17_12 | Opra8_17_13 | Opra8_17_14 | Opra8_17_15
           | Opra8_17_16 | Opra8_17_17 | Opra8_17_18 | Opra8_17_19
           | Opra8_17_20 | Opra8_17_21 | Opra8_17_22 | Opra8_17_23
           | Opra8_17_24 | Opra8_17_25 | Opra8_17_26 | Opra8_17_27
           | Opra8_17_28 | Opra8_17_29 | Opra8_17_30 | Opra8_17_31
           | Opra8_18_0  | Opra8_18_1  | Opra8_18_2  | Opra8_18_3 
           | Opra8_18_4  | Opra8_18_5  | Opra8_18_6  | Opra8_18_7 
           | Opra8_18_8  | Opra8_18_9  | Opra8_18_10 | Opra8_18_11
           | Opra8_18_12 | Opra8_18_13 | Opra8_18_14 | Opra8_18_15
           | Opra8_18_16 | Opra8_18_17 | Opra8_18_18 | Opra8_18_19
           | Opra8_18_20 | Opra8_18_21 | Opra8_18_22 | Opra8_18_23
           | Opra8_18_24 | Opra8_18_25 | Opra8_18_26 | Opra8_18_27
           | Opra8_18_28 | Opra8_18_29 | Opra8_18_30 | Opra8_18_31
           | Opra8_19_0  | Opra8_19_1  | Opra8_19_2  | Opra8_19_3 
           | Opra8_19_4  | Opra8_19_5  | Opra8_19_6  | Opra8_19_7 
           | Opra8_19_8  | Opra8_19_9  | Opra8_19_10 | Opra8_19_11
           | Opra8_19_12 | Opra8_19_13 | Opra8_19_14 | Opra8_19_15
           | Opra8_19_16 | Opra8_19_17 | Opra8_19_18 | Opra8_19_19
           | Opra8_19_20 | Opra8_19_21 | Opra8_19_22 | Opra8_19_23
           | Opra8_19_24 | Opra8_19_25 | Opra8_19_26 | Opra8_19_27
           | Opra8_19_28 | Opra8_19_29 | Opra8_19_30 | Opra8_19_31
           | Opra8_20_0  | Opra8_20_1  | Opra8_20_2  | Opra8_20_3 
           | Opra8_20_4  | Opra8_20_5  | Opra8_20_6  | Opra8_20_7 
           | Opra8_20_8  | Opra8_20_9  | Opra8_20_10 | Opra8_20_11
           | Opra8_20_12 | Opra8_20_13 | Opra8_20_14 | Opra8_20_15
           | Opra8_20_16 | Opra8_20_17 | Opra8_20_18 | Opra8_20_19
           | Opra8_20_20 | Opra8_20_21 | Opra8_20_22 | Opra8_20_23
           | Opra8_20_24 | Opra8_20_25 | Opra8_20_26 | Opra8_20_27
           | Opra8_20_28 | Opra8_20_29 | Opra8_20_30 | Opra8_20_31
           | Opra8_21_0  | Opra8_21_1  | Opra8_21_2  | Opra8_21_3 
           | Opra8_21_4  | Opra8_21_5  | Opra8_21_6  | Opra8_21_7 
           | Opra8_21_8  | Opra8_21_9  | Opra8_21_10 | Opra8_21_11
           | Opra8_21_12 | Opra8_21_13 | Opra8_21_14 | Opra8_21_15
           | Opra8_21_16 | Opra8_21_17 | Opra8_21_18 | Opra8_21_19
           | Opra8_21_20 | Opra8_21_21 | Opra8_21_22 | Opra8_21_23
           | Opra8_21_24 | Opra8_21_25 | Opra8_21_26 | Opra8_21_27
           | Opra8_21_28 | Opra8_21_29 | Opra8_21_30 | Opra8_21_31
           | Opra8_22_0  | Opra8_22_1  | Opra8_22_2  | Opra8_22_3 
           | Opra8_22_4  | Opra8_22_5  | Opra8_22_6  | Opra8_22_7 
           | Opra8_22_8  | Opra8_22_9  | Opra8_22_10 | Opra8_22_11
           | Opra8_22_12 | Opra8_22_13 | Opra8_22_14 | Opra8_22_15
           | Opra8_22_16 | Opra8_22_17 | Opra8_22_18 | Opra8_22_19
           | Opra8_22_20 | Opra8_22_21 | Opra8_22_22 | Opra8_22_23
           | Opra8_22_24 | Opra8_22_25 | Opra8_22_26 | Opra8_22_27
           | Opra8_22_28 | Opra8_22_29 | Opra8_22_30 | Opra8_22_31
           | Opra8_23_0  | Opra8_23_1  | Opra8_23_2  | Opra8_23_3 
           | Opra8_23_4  | Opra8_23_5  | Opra8_23_6  | Opra8_23_7 
           | Opra8_23_8  | Opra8_23_9  | Opra8_23_10 | Opra8_23_11
           | Opra8_23_12 | Opra8_23_13 | Opra8_23_14 | Opra8_23_15
           | Opra8_23_16 | Opra8_23_17 | Opra8_23_18 | Opra8_23_19
           | Opra8_23_20 | Opra8_23_21 | Opra8_23_22 | Opra8_23_23
           | Opra8_23_24 | Opra8_23_25 | Opra8_23_26 | Opra8_23_27
           | Opra8_23_28 | Opra8_23_29 | Opra8_23_30 | Opra8_23_31
           | Opra8_24_0  | Opra8_24_1  | Opra8_24_2  | Opra8_24_3 
           | Opra8_24_4  | Opra8_24_5  | Opra8_24_6  | Opra8_24_7 
           | Opra8_24_8  | Opra8_24_9  | Opra8_24_10 | Opra8_24_11
           | Opra8_24_12 | Opra8_24_13 | Opra8_24_14 | Opra8_24_15
           | Opra8_24_16 | Opra8_24_17 | Opra8_24_18 | Opra8_24_19
           | Opra8_24_20 | Opra8_24_21 | Opra8_24_22 | Opra8_24_23
           | Opra8_24_24 | Opra8_24_25 | Opra8_24_26 | Opra8_24_27
           | Opra8_24_28 | Opra8_24_29 | Opra8_24_30 | Opra8_24_31
           | Opra8_25_0  | Opra8_25_1  | Opra8_25_2  | Opra8_25_3 
           | Opra8_25_4  | Opra8_25_5  | Opra8_25_6  | Opra8_25_7 
           | Opra8_25_8  | Opra8_25_9  | Opra8_25_10 | Opra8_25_11
           | Opra8_25_12 | Opra8_25_13 | Opra8_25_14 | Opra8_25_15
           | Opra8_25_16 | Opra8_25_17 | Opra8_25_18 | Opra8_25_19
           | Opra8_25_20 | Opra8_25_21 | Opra8_25_22 | Opra8_25_23
           | Opra8_25_24 | Opra8_25_25 | Opra8_25_26 | Opra8_25_27
           | Opra8_25_28 | Opra8_25_29 | Opra8_25_30 | Opra8_25_31
           | Opra8_26_0  | Opra8_26_1  | Opra8_26_2  | Opra8_26_3 
           | Opra8_26_4  | Opra8_26_5  | Opra8_26_6  | Opra8_26_7 
           | Opra8_26_8  | Opra8_26_9  | Opra8_26_10 | Opra8_26_11
           | Opra8_26_12 | Opra8_26_13 | Opra8_26_14 | Opra8_26_15
           | Opra8_26_16 | Opra8_26_17 | Opra8_26_18 | Opra8_26_19
           | Opra8_26_20 | Opra8_26_21 | Opra8_26_22 | Opra8_26_23
           | Opra8_26_24 | Opra8_26_25 | Opra8_26_26 | Opra8_26_27
           | Opra8_26_28 | Opra8_26_29 | Opra8_26_30 | Opra8_26_31
           | Opra8_27_0  | Opra8_27_1  | Opra8_27_2  | Opra8_27_3 
           | Opra8_27_4  | Opra8_27_5  | Opra8_27_6  | Opra8_27_7 
           | Opra8_27_8  | Opra8_27_9  | Opra8_27_10 | Opra8_27_11
           | Opra8_27_12 | Opra8_27_13 | Opra8_27_14 | Opra8_27_15
           | Opra8_27_16 | Opra8_27_17 | Opra8_27_18 | Opra8_27_19
           | Opra8_27_20 | Opra8_27_21 | Opra8_27_22 | Opra8_27_23
           | Opra8_27_24 | Opra8_27_25 | Opra8_27_26 | Opra8_27_27
           | Opra8_27_28 | Opra8_27_29 | Opra8_27_30 | Opra8_27_31
           | Opra8_28_0  | Opra8_28_1  | Opra8_28_2  | Opra8_28_3 
           | Opra8_28_4  | Opra8_28_5  | Opra8_28_6  | Opra8_28_7 
           | Opra8_28_8  | Opra8_28_9  | Opra8_28_10 | Opra8_28_11
           | Opra8_28_12 | Opra8_28_13 | Opra8_28_14 | Opra8_28_15
           | Opra8_28_16 | Opra8_28_17 | Opra8_28_18 | Opra8_28_19
           | Opra8_28_20 | Opra8_28_21 | Opra8_28_22 | Opra8_28_23
           | Opra8_28_24 | Opra8_28_25 | Opra8_28_26 | Opra8_28_27
           | Opra8_28_28 | Opra8_28_29 | Opra8_28_30 | Opra8_28_31
           | Opra8_29_0  | Opra8_29_1  | Opra8_29_2  | Opra8_29_3 
           | Opra8_29_4  | Opra8_29_5  | Opra8_29_6  | Opra8_29_7 
           | Opra8_29_8  | Opra8_29_9  | Opra8_29_10 | Opra8_29_11
           | Opra8_29_12 | Opra8_29_13 | Opra8_29_14 | Opra8_29_15
           | Opra8_29_16 | Opra8_29_17 | Opra8_29_18 | Opra8_29_19
           | Opra8_29_20 | Opra8_29_21 | Opra8_29_22 | Opra8_29_23
           | Opra8_29_24 | Opra8_29_25 | Opra8_29_26 | Opra8_29_27
           | Opra8_29_28 | Opra8_29_29 | Opra8_29_30 | Opra8_29_31
           | Opra8_30_0  | Opra8_30_1  | Opra8_30_2  | Opra8_30_3 
           | Opra8_30_4  | Opra8_30_5  | Opra8_30_6  | Opra8_30_7 
           | Opra8_30_8  | Opra8_30_9  | Opra8_30_10 | Opra8_30_11
           | Opra8_30_12 | Opra8_30_13 | Opra8_30_14 | Opra8_30_15
           | Opra8_30_16 | Opra8_30_17 | Opra8_30_18 | Opra8_30_19
           | Opra8_30_20 | Opra8_30_21 | Opra8_30_22 | Opra8_30_23
           | Opra8_30_24 | Opra8_30_25 | Opra8_30_26 | Opra8_30_27
           | Opra8_30_28 | Opra8_30_29 | Opra8_30_30 | Opra8_30_31
           | Opra8_31_0  | Opra8_31_1  | Opra8_31_2  | Opra8_31_3 
           | Opra8_31_4  | Opra8_31_5  | Opra8_31_6  | Opra8_31_7 
           | Opra8_31_8  | Opra8_31_9  | Opra8_31_10 | Opra8_31_11
           | Opra8_31_12 | Opra8_31_13 | Opra8_31_14 | Opra8_31_15
           | Opra8_31_16 | Opra8_31_17 | Opra8_31_18 | Opra8_31_19
           | Opra8_31_20 | Opra8_31_21 | Opra8_31_22 | Opra8_31_23
           | Opra8_31_24 | Opra8_31_25 | Opra8_31_26 | Opra8_31_27
           | Opra8_31_28 | Opra8_31_29 | Opra8_31_30 | Opra8_31_31
           | Opra8_s_0   | Opra8_s_1   | Opra8_s_2   | Opra8_s_3  
           | Opra8_s_4   | Opra8_s_5   | Opra8_s_6   | Opra8_s_7  
           | Opra8_s_8   | Opra8_s_9   | Opra8_s_10  | Opra8_s_11 
           | Opra8_s_12  | Opra8_s_13  | Opra8_s_14  | Opra8_s_15 
           | Opra8_s_16  | Opra8_s_17  | Opra8_s_18  | Opra8_s_19 
           | Opra8_s_20  | Opra8_s_21  | Opra8_s_22  | Opra8_s_23 
           | Opra8_s_24  | Opra8_s_25  | Opra8_s_26  | Opra8_s_27 
           | Opra8_s_28  | Opra8_s_29  | Opra8_s_30  | Opra8_s_31 
           deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Opram Opra8 where
    m _ = 8

instance Calculus Opra8 where
    rank _ = 2
    cName _ = "opra-8"
    cNameGqr _ = "opra8"
    cReadRel = readOpram
    cShowRel = showOpram
    cSparqifyRel = sparqifyOpram
    cGqrifyRel   = sparqifyOpram
    cBaserelationsArealList = areal cBaserelationsList
    cBaserelationsNonArealList = nonAreal cBaserelationsList
    cBaserelationsSameList     = same     cBaserelationsList
    cBaserelationsNonSameList  = nonSame  cBaserelationsList

    bcConvert = opraConvert 8

