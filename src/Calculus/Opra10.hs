module Calculus.Opra10
    ( module Calculus.Opra
    , Opra10(..)
    ) where

-- standard modules

-- local modules
import Basics
import Calculus.Opra

data Opra10 = Opra10_0_0   | Opra10_0_1   | Opra10_0_2   | Opra10_0_3  
            | Opra10_0_4   | Opra10_0_5   | Opra10_0_6   | Opra10_0_7  
            | Opra10_0_8   | Opra10_0_9   | Opra10_0_10  | Opra10_0_11 
            | Opra10_0_12  | Opra10_0_13  | Opra10_0_14  | Opra10_0_15 
            | Opra10_0_16  | Opra10_0_17  | Opra10_0_18  | Opra10_0_19 
            | Opra10_0_20  | Opra10_0_21  | Opra10_0_22  | Opra10_0_23 
            | Opra10_0_24  | Opra10_0_25  | Opra10_0_26  | Opra10_0_27 
            | Opra10_0_28  | Opra10_0_29  | Opra10_0_30  | Opra10_0_31 
            | Opra10_0_32  | Opra10_0_33  | Opra10_0_34  | Opra10_0_35 
            | Opra10_0_36  | Opra10_0_37  | Opra10_0_38  | Opra10_0_39 
            | Opra10_1_0   | Opra10_1_1   | Opra10_1_2   | Opra10_1_3  
            | Opra10_1_4   | Opra10_1_5   | Opra10_1_6   | Opra10_1_7  
            | Opra10_1_8   | Opra10_1_9   | Opra10_1_10  | Opra10_1_11 
            | Opra10_1_12  | Opra10_1_13  | Opra10_1_14  | Opra10_1_15 
            | Opra10_1_16  | Opra10_1_17  | Opra10_1_18  | Opra10_1_19 
            | Opra10_1_20  | Opra10_1_21  | Opra10_1_22  | Opra10_1_23 
            | Opra10_1_24  | Opra10_1_25  | Opra10_1_26  | Opra10_1_27 
            | Opra10_1_28  | Opra10_1_29  | Opra10_1_30  | Opra10_1_31 
            | Opra10_1_32  | Opra10_1_33  | Opra10_1_34  | Opra10_1_35 
            | Opra10_1_36  | Opra10_1_37  | Opra10_1_38  | Opra10_1_39 
            | Opra10_2_0   | Opra10_2_1   | Opra10_2_2   | Opra10_2_3  
            | Opra10_2_4   | Opra10_2_5   | Opra10_2_6   | Opra10_2_7  
            | Opra10_2_8   | Opra10_2_9   | Opra10_2_10  | Opra10_2_11 
            | Opra10_2_12  | Opra10_2_13  | Opra10_2_14  | Opra10_2_15 
            | Opra10_2_16  | Opra10_2_17  | Opra10_2_18  | Opra10_2_19 
            | Opra10_2_20  | Opra10_2_21  | Opra10_2_22  | Opra10_2_23 
            | Opra10_2_24  | Opra10_2_25  | Opra10_2_26  | Opra10_2_27 
            | Opra10_2_28  | Opra10_2_29  | Opra10_2_30  | Opra10_2_31 
            | Opra10_2_32  | Opra10_2_33  | Opra10_2_34  | Opra10_2_35 
            | Opra10_2_36  | Opra10_2_37  | Opra10_2_38  | Opra10_2_39 
            | Opra10_3_0   | Opra10_3_1   | Opra10_3_2   | Opra10_3_3  
            | Opra10_3_4   | Opra10_3_5   | Opra10_3_6   | Opra10_3_7  
            | Opra10_3_8   | Opra10_3_9   | Opra10_3_10  | Opra10_3_11 
            | Opra10_3_12  | Opra10_3_13  | Opra10_3_14  | Opra10_3_15 
            | Opra10_3_16  | Opra10_3_17  | Opra10_3_18  | Opra10_3_19 
            | Opra10_3_20  | Opra10_3_21  | Opra10_3_22  | Opra10_3_23 
            | Opra10_3_24  | Opra10_3_25  | Opra10_3_26  | Opra10_3_27 
            | Opra10_3_28  | Opra10_3_29  | Opra10_3_30  | Opra10_3_31 
            | Opra10_3_32  | Opra10_3_33  | Opra10_3_34  | Opra10_3_35 
            | Opra10_3_36  | Opra10_3_37  | Opra10_3_38  | Opra10_3_39 
            | Opra10_4_0   | Opra10_4_1   | Opra10_4_2   | Opra10_4_3  
            | Opra10_4_4   | Opra10_4_5   | Opra10_4_6   | Opra10_4_7  
            | Opra10_4_8   | Opra10_4_9   | Opra10_4_10  | Opra10_4_11 
            | Opra10_4_12  | Opra10_4_13  | Opra10_4_14  | Opra10_4_15 
            | Opra10_4_16  | Opra10_4_17  | Opra10_4_18  | Opra10_4_19 
            | Opra10_4_20  | Opra10_4_21  | Opra10_4_22  | Opra10_4_23 
            | Opra10_4_24  | Opra10_4_25  | Opra10_4_26  | Opra10_4_27 
            | Opra10_4_28  | Opra10_4_29  | Opra10_4_30  | Opra10_4_31 
            | Opra10_4_32  | Opra10_4_33  | Opra10_4_34  | Opra10_4_35 
            | Opra10_4_36  | Opra10_4_37  | Opra10_4_38  | Opra10_4_39 
            | Opra10_5_0   | Opra10_5_1   | Opra10_5_2   | Opra10_5_3  
            | Opra10_5_4   | Opra10_5_5   | Opra10_5_6   | Opra10_5_7  
            | Opra10_5_8   | Opra10_5_9   | Opra10_5_10  | Opra10_5_11 
            | Opra10_5_12  | Opra10_5_13  | Opra10_5_14  | Opra10_5_15 
            | Opra10_5_16  | Opra10_5_17  | Opra10_5_18  | Opra10_5_19 
            | Opra10_5_20  | Opra10_5_21  | Opra10_5_22  | Opra10_5_23 
            | Opra10_5_24  | Opra10_5_25  | Opra10_5_26  | Opra10_5_27 
            | Opra10_5_28  | Opra10_5_29  | Opra10_5_30  | Opra10_5_31 
            | Opra10_5_32  | Opra10_5_33  | Opra10_5_34  | Opra10_5_35 
            | Opra10_5_36  | Opra10_5_37  | Opra10_5_38  | Opra10_5_39 
            | Opra10_6_0   | Opra10_6_1   | Opra10_6_2   | Opra10_6_3  
            | Opra10_6_4   | Opra10_6_5   | Opra10_6_6   | Opra10_6_7  
            | Opra10_6_8   | Opra10_6_9   | Opra10_6_10  | Opra10_6_11 
            | Opra10_6_12  | Opra10_6_13  | Opra10_6_14  | Opra10_6_15 
            | Opra10_6_16  | Opra10_6_17  | Opra10_6_18  | Opra10_6_19 
            | Opra10_6_20  | Opra10_6_21  | Opra10_6_22  | Opra10_6_23 
            | Opra10_6_24  | Opra10_6_25  | Opra10_6_26  | Opra10_6_27 
            | Opra10_6_28  | Opra10_6_29  | Opra10_6_30  | Opra10_6_31 
            | Opra10_6_32  | Opra10_6_33  | Opra10_6_34  | Opra10_6_35 
            | Opra10_6_36  | Opra10_6_37  | Opra10_6_38  | Opra10_6_39 
            | Opra10_7_0   | Opra10_7_1   | Opra10_7_2   | Opra10_7_3  
            | Opra10_7_4   | Opra10_7_5   | Opra10_7_6   | Opra10_7_7  
            | Opra10_7_8   | Opra10_7_9   | Opra10_7_10  | Opra10_7_11 
            | Opra10_7_12  | Opra10_7_13  | Opra10_7_14  | Opra10_7_15 
            | Opra10_7_16  | Opra10_7_17  | Opra10_7_18  | Opra10_7_19 
            | Opra10_7_20  | Opra10_7_21  | Opra10_7_22  | Opra10_7_23 
            | Opra10_7_24  | Opra10_7_25  | Opra10_7_26  | Opra10_7_27 
            | Opra10_7_28  | Opra10_7_29  | Opra10_7_30  | Opra10_7_31 
            | Opra10_7_32  | Opra10_7_33  | Opra10_7_34  | Opra10_7_35 
            | Opra10_7_36  | Opra10_7_37  | Opra10_7_38  | Opra10_7_39 
            | Opra10_8_0   | Opra10_8_1   | Opra10_8_2   | Opra10_8_3  
            | Opra10_8_4   | Opra10_8_5   | Opra10_8_6   | Opra10_8_7  
            | Opra10_8_8   | Opra10_8_9   | Opra10_8_10  | Opra10_8_11 
            | Opra10_8_12  | Opra10_8_13  | Opra10_8_14  | Opra10_8_15 
            | Opra10_8_16  | Opra10_8_17  | Opra10_8_18  | Opra10_8_19 
            | Opra10_8_20  | Opra10_8_21  | Opra10_8_22  | Opra10_8_23 
            | Opra10_8_24  | Opra10_8_25  | Opra10_8_26  | Opra10_8_27 
            | Opra10_8_28  | Opra10_8_29  | Opra10_8_30  | Opra10_8_31 
            | Opra10_8_32  | Opra10_8_33  | Opra10_8_34  | Opra10_8_35 
            | Opra10_8_36  | Opra10_8_37  | Opra10_8_38  | Opra10_8_39 
            | Opra10_9_0   | Opra10_9_1   | Opra10_9_2   | Opra10_9_3  
            | Opra10_9_4   | Opra10_9_5   | Opra10_9_6   | Opra10_9_7  
            | Opra10_9_8   | Opra10_9_9   | Opra10_9_10  | Opra10_9_11 
            | Opra10_9_12  | Opra10_9_13  | Opra10_9_14  | Opra10_9_15 
            | Opra10_9_16  | Opra10_9_17  | Opra10_9_18  | Opra10_9_19 
            | Opra10_9_20  | Opra10_9_21  | Opra10_9_22  | Opra10_9_23 
            | Opra10_9_24  | Opra10_9_25  | Opra10_9_26  | Opra10_9_27 
            | Opra10_9_28  | Opra10_9_29  | Opra10_9_30  | Opra10_9_31 
            | Opra10_9_32  | Opra10_9_33  | Opra10_9_34  | Opra10_9_35 
            | Opra10_9_36  | Opra10_9_37  | Opra10_9_38  | Opra10_9_39 
            | Opra10_10_00 | Opra10_10_01 | Opra10_10_02 | Opra10_10_03
            | Opra10_10_04 | Opra10_10_05 | Opra10_10_06 | Opra10_10_07
            | Opra10_10_08 | Opra10_10_09 | Opra10_10_10 | Opra10_10_11
            | Opra10_10_12 | Opra10_10_13 | Opra10_10_14 | Opra10_10_15
            | Opra10_10_16 | Opra10_10_17 | Opra10_10_18 | Opra10_10_19
            | Opra10_10_20 | Opra10_10_21 | Opra10_10_22 | Opra10_10_23
            | Opra10_10_24 | Opra10_10_25 | Opra10_10_26 | Opra10_10_27
            | Opra10_10_28 | Opra10_10_29 | Opra10_10_30 | Opra10_10_31
            | Opra10_10_32 | Opra10_10_33 | Opra10_10_34 | Opra10_10_35
            | Opra10_10_36 | Opra10_10_37 | Opra10_10_38 | Opra10_10_39
            | Opra10_11_00 | Opra10_11_01 | Opra10_11_02 | Opra10_11_03
            | Opra10_11_04 | Opra10_11_05 | Opra10_11_06 | Opra10_11_07
            | Opra10_11_08 | Opra10_11_09 | Opra10_11_10 | Opra10_11_11
            | Opra10_11_12 | Opra10_11_13 | Opra10_11_14 | Opra10_11_15
            | Opra10_11_16 | Opra10_11_17 | Opra10_11_18 | Opra10_11_19
            | Opra10_11_20 | Opra10_11_21 | Opra10_11_22 | Opra10_11_23
            | Opra10_11_24 | Opra10_11_25 | Opra10_11_26 | Opra10_11_27
            | Opra10_11_28 | Opra10_11_29 | Opra10_11_30 | Opra10_11_31
            | Opra10_11_32 | Opra10_11_33 | Opra10_11_34 | Opra10_11_35
            | Opra10_11_36 | Opra10_11_37 | Opra10_11_38 | Opra10_11_39
            | Opra10_12_00 | Opra10_12_01 | Opra10_12_02 | Opra10_12_03
            | Opra10_12_04 | Opra10_12_05 | Opra10_12_06 | Opra10_12_07
            | Opra10_12_08 | Opra10_12_09 | Opra10_12_10 | Opra10_12_11
            | Opra10_12_12 | Opra10_12_13 | Opra10_12_14 | Opra10_12_15
            | Opra10_12_16 | Opra10_12_17 | Opra10_12_18 | Opra10_12_19
            | Opra10_12_20 | Opra10_12_21 | Opra10_12_22 | Opra10_12_23
            | Opra10_12_24 | Opra10_12_25 | Opra10_12_26 | Opra10_12_27
            | Opra10_12_28 | Opra10_12_29 | Opra10_12_30 | Opra10_12_31
            | Opra10_12_32 | Opra10_12_33 | Opra10_12_34 | Opra10_12_35
            | Opra10_12_36 | Opra10_12_37 | Opra10_12_38 | Opra10_12_39
            | Opra10_13_00 | Opra10_13_01 | Opra10_13_02 | Opra10_13_03
            | Opra10_13_04 | Opra10_13_05 | Opra10_13_06 | Opra10_13_07
            | Opra10_13_08 | Opra10_13_09 | Opra10_13_10 | Opra10_13_11
            | Opra10_13_12 | Opra10_13_13 | Opra10_13_14 | Opra10_13_15
            | Opra10_13_16 | Opra10_13_17 | Opra10_13_18 | Opra10_13_19
            | Opra10_13_20 | Opra10_13_21 | Opra10_13_22 | Opra10_13_23
            | Opra10_13_24 | Opra10_13_25 | Opra10_13_26 | Opra10_13_27
            | Opra10_13_28 | Opra10_13_29 | Opra10_13_30 | Opra10_13_31
            | Opra10_13_32 | Opra10_13_33 | Opra10_13_34 | Opra10_13_35
            | Opra10_13_36 | Opra10_13_37 | Opra10_13_38 | Opra10_13_39
            | Opra10_14_00 | Opra10_14_01 | Opra10_14_02 | Opra10_14_03
            | Opra10_14_04 | Opra10_14_05 | Opra10_14_06 | Opra10_14_07
            | Opra10_14_08 | Opra10_14_09 | Opra10_14_10 | Opra10_14_11
            | Opra10_14_12 | Opra10_14_13 | Opra10_14_14 | Opra10_14_15
            | Opra10_14_16 | Opra10_14_17 | Opra10_14_18 | Opra10_14_19
            | Opra10_14_20 | Opra10_14_21 | Opra10_14_22 | Opra10_14_23
            | Opra10_14_24 | Opra10_14_25 | Opra10_14_26 | Opra10_14_27
            | Opra10_14_28 | Opra10_14_29 | Opra10_14_30 | Opra10_14_31
            | Opra10_14_32 | Opra10_14_33 | Opra10_14_34 | Opra10_14_35
            | Opra10_14_36 | Opra10_14_37 | Opra10_14_38 | Opra10_14_39
            | Opra10_15_00 | Opra10_15_01 | Opra10_15_02 | Opra10_15_03
            | Opra10_15_04 | Opra10_15_05 | Opra10_15_06 | Opra10_15_07
            | Opra10_15_08 | Opra10_15_09 | Opra10_15_10 | Opra10_15_11
            | Opra10_15_12 | Opra10_15_13 | Opra10_15_14 | Opra10_15_15
            | Opra10_15_16 | Opra10_15_17 | Opra10_15_18 | Opra10_15_19
            | Opra10_15_20 | Opra10_15_21 | Opra10_15_22 | Opra10_15_23
            | Opra10_15_24 | Opra10_15_25 | Opra10_15_26 | Opra10_15_27
            | Opra10_15_28 | Opra10_15_29 | Opra10_15_30 | Opra10_15_31
            | Opra10_15_32 | Opra10_15_33 | Opra10_15_34 | Opra10_15_35
            | Opra10_15_36 | Opra10_15_37 | Opra10_15_38 | Opra10_15_39
            | Opra10_16_00 | Opra10_16_01 | Opra10_16_02 | Opra10_16_03
            | Opra10_16_04 | Opra10_16_05 | Opra10_16_06 | Opra10_16_07
            | Opra10_16_08 | Opra10_16_09 | Opra10_16_10 | Opra10_16_11
            | Opra10_16_12 | Opra10_16_13 | Opra10_16_14 | Opra10_16_15
            | Opra10_16_16 | Opra10_16_17 | Opra10_16_18 | Opra10_16_19
            | Opra10_16_20 | Opra10_16_21 | Opra10_16_22 | Opra10_16_23
            | Opra10_16_24 | Opra10_16_25 | Opra10_16_26 | Opra10_16_27
            | Opra10_16_28 | Opra10_16_29 | Opra10_16_30 | Opra10_16_31
            | Opra10_16_32 | Opra10_16_33 | Opra10_16_34 | Opra10_16_35
            | Opra10_16_36 | Opra10_16_37 | Opra10_16_38 | Opra10_16_39
            | Opra10_17_00 | Opra10_17_01 | Opra10_17_02 | Opra10_17_03
            | Opra10_17_04 | Opra10_17_05 | Opra10_17_06 | Opra10_17_07
            | Opra10_17_08 | Opra10_17_09 | Opra10_17_10 | Opra10_17_11
            | Opra10_17_12 | Opra10_17_13 | Opra10_17_14 | Opra10_17_15
            | Opra10_17_16 | Opra10_17_17 | Opra10_17_18 | Opra10_17_19
            | Opra10_17_20 | Opra10_17_21 | Opra10_17_22 | Opra10_17_23
            | Opra10_17_24 | Opra10_17_25 | Opra10_17_26 | Opra10_17_27
            | Opra10_17_28 | Opra10_17_29 | Opra10_17_30 | Opra10_17_31
            | Opra10_17_32 | Opra10_17_33 | Opra10_17_34 | Opra10_17_35
            | Opra10_17_36 | Opra10_17_37 | Opra10_17_38 | Opra10_17_39
            | Opra10_18_00 | Opra10_18_01 | Opra10_18_02 | Opra10_18_03
            | Opra10_18_04 | Opra10_18_05 | Opra10_18_06 | Opra10_18_07
            | Opra10_18_08 | Opra10_18_09 | Opra10_18_10 | Opra10_18_11
            | Opra10_18_12 | Opra10_18_13 | Opra10_18_14 | Opra10_18_15
            | Opra10_18_16 | Opra10_18_17 | Opra10_18_18 | Opra10_18_19
            | Opra10_18_20 | Opra10_18_21 | Opra10_18_22 | Opra10_18_23
            | Opra10_18_24 | Opra10_18_25 | Opra10_18_26 | Opra10_18_27
            | Opra10_18_28 | Opra10_18_29 | Opra10_18_30 | Opra10_18_31
            | Opra10_18_32 | Opra10_18_33 | Opra10_18_34 | Opra10_18_35
            | Opra10_18_36 | Opra10_18_37 | Opra10_18_38 | Opra10_18_39
            | Opra10_19_00 | Opra10_19_01 | Opra10_19_02 | Opra10_19_03
            | Opra10_19_04 | Opra10_19_05 | Opra10_19_06 | Opra10_19_07
            | Opra10_19_08 | Opra10_19_09 | Opra10_19_10 | Opra10_19_11
            | Opra10_19_12 | Opra10_19_13 | Opra10_19_14 | Opra10_19_15
            | Opra10_19_16 | Opra10_19_17 | Opra10_19_18 | Opra10_19_19
            | Opra10_19_20 | Opra10_19_21 | Opra10_19_22 | Opra10_19_23
            | Opra10_19_24 | Opra10_19_25 | Opra10_19_26 | Opra10_19_27
            | Opra10_19_28 | Opra10_19_29 | Opra10_19_30 | Opra10_19_31
            | Opra10_19_32 | Opra10_19_33 | Opra10_19_34 | Opra10_19_35
            | Opra10_19_36 | Opra10_19_37 | Opra10_19_38 | Opra10_19_39
            | Opra10_20_00 | Opra10_20_01 | Opra10_20_02 | Opra10_20_03
            | Opra10_20_04 | Opra10_20_05 | Opra10_20_06 | Opra10_20_07
            | Opra10_20_08 | Opra10_20_09 | Opra10_20_10 | Opra10_20_11
            | Opra10_20_12 | Opra10_20_13 | Opra10_20_14 | Opra10_20_15
            | Opra10_20_16 | Opra10_20_17 | Opra10_20_18 | Opra10_20_19
            | Opra10_20_20 | Opra10_20_21 | Opra10_20_22 | Opra10_20_23
            | Opra10_20_24 | Opra10_20_25 | Opra10_20_26 | Opra10_20_27
            | Opra10_20_28 | Opra10_20_29 | Opra10_20_30 | Opra10_20_31
            | Opra10_20_32 | Opra10_20_33 | Opra10_20_34 | Opra10_20_35
            | Opra10_20_36 | Opra10_20_37 | Opra10_20_38 | Opra10_20_39
            | Opra10_21_00 | Opra10_21_01 | Opra10_21_02 | Opra10_21_03
            | Opra10_21_04 | Opra10_21_05 | Opra10_21_06 | Opra10_21_07
            | Opra10_21_08 | Opra10_21_09 | Opra10_21_10 | Opra10_21_11
            | Opra10_21_12 | Opra10_21_13 | Opra10_21_14 | Opra10_21_15
            | Opra10_21_16 | Opra10_21_17 | Opra10_21_18 | Opra10_21_19
            | Opra10_21_20 | Opra10_21_21 | Opra10_21_22 | Opra10_21_23
            | Opra10_21_24 | Opra10_21_25 | Opra10_21_26 | Opra10_21_27
            | Opra10_21_28 | Opra10_21_29 | Opra10_21_30 | Opra10_21_31
            | Opra10_21_32 | Opra10_21_33 | Opra10_21_34 | Opra10_21_35
            | Opra10_21_36 | Opra10_21_37 | Opra10_21_38 | Opra10_21_39
            | Opra10_22_00 | Opra10_22_01 | Opra10_22_02 | Opra10_22_03
            | Opra10_22_04 | Opra10_22_05 | Opra10_22_06 | Opra10_22_07
            | Opra10_22_08 | Opra10_22_09 | Opra10_22_10 | Opra10_22_11
            | Opra10_22_12 | Opra10_22_13 | Opra10_22_14 | Opra10_22_15
            | Opra10_22_16 | Opra10_22_17 | Opra10_22_18 | Opra10_22_19
            | Opra10_22_20 | Opra10_22_21 | Opra10_22_22 | Opra10_22_23
            | Opra10_22_24 | Opra10_22_25 | Opra10_22_26 | Opra10_22_27
            | Opra10_22_28 | Opra10_22_29 | Opra10_22_30 | Opra10_22_31
            | Opra10_22_32 | Opra10_22_33 | Opra10_22_34 | Opra10_22_35
            | Opra10_22_36 | Opra10_22_37 | Opra10_22_38 | Opra10_22_39
            | Opra10_23_00 | Opra10_23_01 | Opra10_23_02 | Opra10_23_03
            | Opra10_23_04 | Opra10_23_05 | Opra10_23_06 | Opra10_23_07
            | Opra10_23_08 | Opra10_23_09 | Opra10_23_10 | Opra10_23_11
            | Opra10_23_12 | Opra10_23_13 | Opra10_23_14 | Opra10_23_15
            | Opra10_23_16 | Opra10_23_17 | Opra10_23_18 | Opra10_23_19
            | Opra10_23_20 | Opra10_23_21 | Opra10_23_22 | Opra10_23_23
            | Opra10_23_24 | Opra10_23_25 | Opra10_23_26 | Opra10_23_27
            | Opra10_23_28 | Opra10_23_29 | Opra10_23_30 | Opra10_23_31
            | Opra10_23_32 | Opra10_23_33 | Opra10_23_34 | Opra10_23_35
            | Opra10_23_36 | Opra10_23_37 | Opra10_23_38 | Opra10_23_39
            | Opra10_24_00 | Opra10_24_01 | Opra10_24_02 | Opra10_24_03
            | Opra10_24_04 | Opra10_24_05 | Opra10_24_06 | Opra10_24_07
            | Opra10_24_08 | Opra10_24_09 | Opra10_24_10 | Opra10_24_11
            | Opra10_24_12 | Opra10_24_13 | Opra10_24_14 | Opra10_24_15
            | Opra10_24_16 | Opra10_24_17 | Opra10_24_18 | Opra10_24_19
            | Opra10_24_20 | Opra10_24_21 | Opra10_24_22 | Opra10_24_23
            | Opra10_24_24 | Opra10_24_25 | Opra10_24_26 | Opra10_24_27
            | Opra10_24_28 | Opra10_24_29 | Opra10_24_30 | Opra10_24_31
            | Opra10_24_32 | Opra10_24_33 | Opra10_24_34 | Opra10_24_35
            | Opra10_24_36 | Opra10_24_37 | Opra10_24_38 | Opra10_24_39
            | Opra10_25_00 | Opra10_25_01 | Opra10_25_02 | Opra10_25_03
            | Opra10_25_04 | Opra10_25_05 | Opra10_25_06 | Opra10_25_07
            | Opra10_25_08 | Opra10_25_09 | Opra10_25_10 | Opra10_25_11
            | Opra10_25_12 | Opra10_25_13 | Opra10_25_14 | Opra10_25_15
            | Opra10_25_16 | Opra10_25_17 | Opra10_25_18 | Opra10_25_19
            | Opra10_25_20 | Opra10_25_21 | Opra10_25_22 | Opra10_25_23
            | Opra10_25_24 | Opra10_25_25 | Opra10_25_26 | Opra10_25_27
            | Opra10_25_28 | Opra10_25_29 | Opra10_25_30 | Opra10_25_31
            | Opra10_25_32 | Opra10_25_33 | Opra10_25_34 | Opra10_25_35
            | Opra10_25_36 | Opra10_25_37 | Opra10_25_38 | Opra10_25_39
            | Opra10_26_00 | Opra10_26_01 | Opra10_26_02 | Opra10_26_03
            | Opra10_26_04 | Opra10_26_05 | Opra10_26_06 | Opra10_26_07
            | Opra10_26_08 | Opra10_26_09 | Opra10_26_10 | Opra10_26_11
            | Opra10_26_12 | Opra10_26_13 | Opra10_26_14 | Opra10_26_15
            | Opra10_26_16 | Opra10_26_17 | Opra10_26_18 | Opra10_26_19
            | Opra10_26_20 | Opra10_26_21 | Opra10_26_22 | Opra10_26_23
            | Opra10_26_24 | Opra10_26_25 | Opra10_26_26 | Opra10_26_27
            | Opra10_26_28 | Opra10_26_29 | Opra10_26_30 | Opra10_26_31
            | Opra10_26_32 | Opra10_26_33 | Opra10_26_34 | Opra10_26_35
            | Opra10_26_36 | Opra10_26_37 | Opra10_26_38 | Opra10_26_39
            | Opra10_27_00 | Opra10_27_01 | Opra10_27_02 | Opra10_27_03
            | Opra10_27_04 | Opra10_27_05 | Opra10_27_06 | Opra10_27_07
            | Opra10_27_08 | Opra10_27_09 | Opra10_27_10 | Opra10_27_11
            | Opra10_27_12 | Opra10_27_13 | Opra10_27_14 | Opra10_27_15
            | Opra10_27_16 | Opra10_27_17 | Opra10_27_18 | Opra10_27_19
            | Opra10_27_20 | Opra10_27_21 | Opra10_27_22 | Opra10_27_23
            | Opra10_27_24 | Opra10_27_25 | Opra10_27_26 | Opra10_27_27
            | Opra10_27_28 | Opra10_27_29 | Opra10_27_30 | Opra10_27_31
            | Opra10_27_32 | Opra10_27_33 | Opra10_27_34 | Opra10_27_35
            | Opra10_27_36 | Opra10_27_37 | Opra10_27_38 | Opra10_27_39
            | Opra10_28_00 | Opra10_28_01 | Opra10_28_02 | Opra10_28_03
            | Opra10_28_04 | Opra10_28_05 | Opra10_28_06 | Opra10_28_07
            | Opra10_28_08 | Opra10_28_09 | Opra10_28_10 | Opra10_28_11
            | Opra10_28_12 | Opra10_28_13 | Opra10_28_14 | Opra10_28_15
            | Opra10_28_16 | Opra10_28_17 | Opra10_28_18 | Opra10_28_19
            | Opra10_28_20 | Opra10_28_21 | Opra10_28_22 | Opra10_28_23
            | Opra10_28_24 | Opra10_28_25 | Opra10_28_26 | Opra10_28_27
            | Opra10_28_28 | Opra10_28_29 | Opra10_28_30 | Opra10_28_31
            | Opra10_28_32 | Opra10_28_33 | Opra10_28_34 | Opra10_28_35
            | Opra10_28_36 | Opra10_28_37 | Opra10_28_38 | Opra10_28_39
            | Opra10_29_00 | Opra10_29_01 | Opra10_29_02 | Opra10_29_03
            | Opra10_29_04 | Opra10_29_05 | Opra10_29_06 | Opra10_29_07
            | Opra10_29_08 | Opra10_29_09 | Opra10_29_10 | Opra10_29_11
            | Opra10_29_12 | Opra10_29_13 | Opra10_29_14 | Opra10_29_15
            | Opra10_29_16 | Opra10_29_17 | Opra10_29_18 | Opra10_29_19
            | Opra10_29_20 | Opra10_29_21 | Opra10_29_22 | Opra10_29_23
            | Opra10_29_24 | Opra10_29_25 | Opra10_29_26 | Opra10_29_27
            | Opra10_29_28 | Opra10_29_29 | Opra10_29_30 | Opra10_29_31
            | Opra10_29_32 | Opra10_29_33 | Opra10_29_34 | Opra10_29_35
            | Opra10_29_36 | Opra10_29_37 | Opra10_29_38 | Opra10_29_39
            | Opra10_30_00 | Opra10_30_01 | Opra10_30_02 | Opra10_30_03
            | Opra10_30_04 | Opra10_30_05 | Opra10_30_06 | Opra10_30_07
            | Opra10_30_08 | Opra10_30_09 | Opra10_30_10 | Opra10_30_11
            | Opra10_30_12 | Opra10_30_13 | Opra10_30_14 | Opra10_30_15
            | Opra10_30_16 | Opra10_30_17 | Opra10_30_18 | Opra10_30_19
            | Opra10_30_20 | Opra10_30_21 | Opra10_30_22 | Opra10_30_23
            | Opra10_30_24 | Opra10_30_25 | Opra10_30_26 | Opra10_30_27
            | Opra10_30_28 | Opra10_30_29 | Opra10_30_30 | Opra10_30_31
            | Opra10_30_32 | Opra10_30_33 | Opra10_30_34 | Opra10_30_35
            | Opra10_30_36 | Opra10_30_37 | Opra10_30_38 | Opra10_30_39
            | Opra10_31_00 | Opra10_31_01 | Opra10_31_02 | Opra10_31_03
            | Opra10_31_04 | Opra10_31_05 | Opra10_31_06 | Opra10_31_07
            | Opra10_31_08 | Opra10_31_09 | Opra10_31_10 | Opra10_31_11
            | Opra10_31_12 | Opra10_31_13 | Opra10_31_14 | Opra10_31_15
            | Opra10_31_16 | Opra10_31_17 | Opra10_31_18 | Opra10_31_19
            | Opra10_31_20 | Opra10_31_21 | Opra10_31_22 | Opra10_31_23
            | Opra10_31_24 | Opra10_31_25 | Opra10_31_26 | Opra10_31_27
            | Opra10_31_28 | Opra10_31_29 | Opra10_31_30 | Opra10_31_31
            | Opra10_31_32 | Opra10_31_33 | Opra10_31_34 | Opra10_31_35
            | Opra10_31_36 | Opra10_31_37 | Opra10_31_38 | Opra10_31_39
            | Opra10_32_00 | Opra10_32_01 | Opra10_32_02 | Opra10_32_03
            | Opra10_32_04 | Opra10_32_05 | Opra10_32_06 | Opra10_32_07
            | Opra10_32_08 | Opra10_32_09 | Opra10_32_10 | Opra10_32_11
            | Opra10_32_12 | Opra10_32_13 | Opra10_32_14 | Opra10_32_15
            | Opra10_32_16 | Opra10_32_17 | Opra10_32_18 | Opra10_32_19
            | Opra10_32_20 | Opra10_32_21 | Opra10_32_22 | Opra10_32_23
            | Opra10_32_24 | Opra10_32_25 | Opra10_32_26 | Opra10_32_27
            | Opra10_32_28 | Opra10_32_29 | Opra10_32_30 | Opra10_32_31
            | Opra10_32_32 | Opra10_32_33 | Opra10_32_34 | Opra10_32_35
            | Opra10_32_36 | Opra10_32_37 | Opra10_32_38 | Opra10_32_39
            | Opra10_33_00 | Opra10_33_01 | Opra10_33_02 | Opra10_33_03
            | Opra10_33_04 | Opra10_33_05 | Opra10_33_06 | Opra10_33_07
            | Opra10_33_08 | Opra10_33_09 | Opra10_33_10 | Opra10_33_11
            | Opra10_33_12 | Opra10_33_13 | Opra10_33_14 | Opra10_33_15
            | Opra10_33_16 | Opra10_33_17 | Opra10_33_18 | Opra10_33_19
            | Opra10_33_20 | Opra10_33_21 | Opra10_33_22 | Opra10_33_23
            | Opra10_33_24 | Opra10_33_25 | Opra10_33_26 | Opra10_33_27
            | Opra10_33_28 | Opra10_33_29 | Opra10_33_30 | Opra10_33_31
            | Opra10_33_32 | Opra10_33_33 | Opra10_33_34 | Opra10_33_35
            | Opra10_33_36 | Opra10_33_37 | Opra10_33_38 | Opra10_33_39
            | Opra10_34_00 | Opra10_34_01 | Opra10_34_02 | Opra10_34_03
            | Opra10_34_04 | Opra10_34_05 | Opra10_34_06 | Opra10_34_07
            | Opra10_34_08 | Opra10_34_09 | Opra10_34_10 | Opra10_34_11
            | Opra10_34_12 | Opra10_34_13 | Opra10_34_14 | Opra10_34_15
            | Opra10_34_16 | Opra10_34_17 | Opra10_34_18 | Opra10_34_19
            | Opra10_34_20 | Opra10_34_21 | Opra10_34_22 | Opra10_34_23
            | Opra10_34_24 | Opra10_34_25 | Opra10_34_26 | Opra10_34_27
            | Opra10_34_28 | Opra10_34_29 | Opra10_34_30 | Opra10_34_31
            | Opra10_34_32 | Opra10_34_33 | Opra10_34_34 | Opra10_34_35
            | Opra10_34_36 | Opra10_34_37 | Opra10_34_38 | Opra10_34_39
            | Opra10_35_00 | Opra10_35_01 | Opra10_35_02 | Opra10_35_03
            | Opra10_35_04 | Opra10_35_05 | Opra10_35_06 | Opra10_35_07
            | Opra10_35_08 | Opra10_35_09 | Opra10_35_10 | Opra10_35_11
            | Opra10_35_12 | Opra10_35_13 | Opra10_35_14 | Opra10_35_15
            | Opra10_35_16 | Opra10_35_17 | Opra10_35_18 | Opra10_35_19
            | Opra10_35_20 | Opra10_35_21 | Opra10_35_22 | Opra10_35_23
            | Opra10_35_24 | Opra10_35_25 | Opra10_35_26 | Opra10_35_27
            | Opra10_35_28 | Opra10_35_29 | Opra10_35_30 | Opra10_35_31
            | Opra10_35_32 | Opra10_35_33 | Opra10_35_34 | Opra10_35_35
            | Opra10_35_36 | Opra10_35_37 | Opra10_35_38 | Opra10_35_39
            | Opra10_36_00 | Opra10_36_01 | Opra10_36_02 | Opra10_36_03
            | Opra10_36_04 | Opra10_36_05 | Opra10_36_06 | Opra10_36_07
            | Opra10_36_08 | Opra10_36_09 | Opra10_36_10 | Opra10_36_11
            | Opra10_36_12 | Opra10_36_13 | Opra10_36_14 | Opra10_36_15
            | Opra10_36_16 | Opra10_36_17 | Opra10_36_18 | Opra10_36_19
            | Opra10_36_20 | Opra10_36_21 | Opra10_36_22 | Opra10_36_23
            | Opra10_36_24 | Opra10_36_25 | Opra10_36_26 | Opra10_36_27
            | Opra10_36_28 | Opra10_36_29 | Opra10_36_30 | Opra10_36_31
            | Opra10_36_32 | Opra10_36_33 | Opra10_36_34 | Opra10_36_35
            | Opra10_36_36 | Opra10_36_37 | Opra10_36_38 | Opra10_36_39
            | Opra10_37_00 | Opra10_37_01 | Opra10_37_02 | Opra10_37_03
            | Opra10_37_04 | Opra10_37_05 | Opra10_37_06 | Opra10_37_07
            | Opra10_37_08 | Opra10_37_09 | Opra10_37_10 | Opra10_37_11
            | Opra10_37_12 | Opra10_37_13 | Opra10_37_14 | Opra10_37_15
            | Opra10_37_16 | Opra10_37_17 | Opra10_37_18 | Opra10_37_19
            | Opra10_37_20 | Opra10_37_21 | Opra10_37_22 | Opra10_37_23
            | Opra10_37_24 | Opra10_37_25 | Opra10_37_26 | Opra10_37_27
            | Opra10_37_28 | Opra10_37_29 | Opra10_37_30 | Opra10_37_31
            | Opra10_37_32 | Opra10_37_33 | Opra10_37_34 | Opra10_37_35
            | Opra10_37_36 | Opra10_37_37 | Opra10_37_38 | Opra10_37_39
            | Opra10_38_00 | Opra10_38_01 | Opra10_38_02 | Opra10_38_03
            | Opra10_38_04 | Opra10_38_05 | Opra10_38_06 | Opra10_38_07
            | Opra10_38_08 | Opra10_38_09 | Opra10_38_10 | Opra10_38_11
            | Opra10_38_12 | Opra10_38_13 | Opra10_38_14 | Opra10_38_15
            | Opra10_38_16 | Opra10_38_17 | Opra10_38_18 | Opra10_38_19
            | Opra10_38_20 | Opra10_38_21 | Opra10_38_22 | Opra10_38_23
            | Opra10_38_24 | Opra10_38_25 | Opra10_38_26 | Opra10_38_27
            | Opra10_38_28 | Opra10_38_29 | Opra10_38_30 | Opra10_38_31
            | Opra10_38_32 | Opra10_38_33 | Opra10_38_34 | Opra10_38_35
            | Opra10_38_36 | Opra10_38_37 | Opra10_38_38 | Opra10_38_39
            | Opra10_39_00 | Opra10_39_01 | Opra10_39_02 | Opra10_39_03
            | Opra10_39_04 | Opra10_39_05 | Opra10_39_06 | Opra10_39_07
            | Opra10_39_08 | Opra10_39_09 | Opra10_39_10 | Opra10_39_11
            | Opra10_39_12 | Opra10_39_13 | Opra10_39_14 | Opra10_39_15
            | Opra10_39_16 | Opra10_39_17 | Opra10_39_18 | Opra10_39_19
            | Opra10_39_20 | Opra10_39_21 | Opra10_39_22 | Opra10_39_23
            | Opra10_39_24 | Opra10_39_25 | Opra10_39_26 | Opra10_39_27
            | Opra10_39_28 | Opra10_39_29 | Opra10_39_30 | Opra10_39_31
            | Opra10_39_32 | Opra10_39_33 | Opra10_39_34 | Opra10_39_35
            | Opra10_39_36 | Opra10_39_37 | Opra10_39_38 | Opra10_39_39
            | Opra10_s_0   | Opra10_s_1   | Opra10_s_2   | Opra10_s_3  
            | Opra10_s_4   | Opra10_s_5   | Opra10_s_6   | Opra10_s_7  
            | Opra10_s_8   | Opra10_s_9   | Opra10_s_10  | Opra10_s_11 
            | Opra10_s_12  | Opra10_s_13  | Opra10_s_14  | Opra10_s_15 
            | Opra10_s_16  | Opra10_s_17  | Opra10_s_18  | Opra10_s_19 
            | Opra10_s_20  | Opra10_s_21  | Opra10_s_22  | Opra10_s_23 
            | Opra10_s_24  | Opra10_s_25  | Opra10_s_26  | Opra10_s_27 
            | Opra10_s_28  | Opra10_s_29  | Opra10_s_30  | Opra10_s_31 
            | Opra10_s_32  | Opra10_s_33  | Opra10_s_34  | Opra10_s_35 
            | Opra10_s_36  | Opra10_s_37  | Opra10_s_38  | Opra10_s_39 


           deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Opram Opra10 where
    m _ = 10

instance Calculus Opra10 where
    rank _ = 2
    cName _ = "opra-10"
    cNameGqr _ ="opra10"
    cReadRel = readOpram
    cShowRel = showOpram
    cSparqifyRel = sparqifyOpram
    cGqrifyRel   = sparqifyOpram
    cBaserelationsArealList = areal cBaserelationsList
    cBaserelationsNonArealList = nonAreal cBaserelationsList

    bcConvert = opraConvert 10

