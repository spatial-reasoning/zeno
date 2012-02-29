module Calculus.OPRA2 where

-- standard modules
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Helpful

data OPRA2 =
    OPRA2_00 | OPRA2_01 | OPRA2_02 | OPRA2_03 |
    OPRA2_04 | OPRA2_05 | OPRA2_06 | OPRA2_07 |
    OPRA2_10 | OPRA2_11 | OPRA2_12 | OPRA2_13 |
    OPRA2_14 | OPRA2_15 | OPRA2_16 | OPRA2_17 |
    OPRA2_20 | OPRA2_21 | OPRA2_22 | OPRA2_23 |
    OPRA2_24 | OPRA2_25 | OPRA2_26 | OPRA2_27 |
    OPRA2_30 | OPRA2_31 | OPRA2_32 | OPRA2_33 |
    OPRA2_34 | OPRA2_35 | OPRA2_36 | OPRA2_37 |
    OPRA2_40 | OPRA2_41 | OPRA2_42 | OPRA2_43 |
    OPRA2_44 | OPRA2_45 | OPRA2_46 | OPRA2_47 |
    OPRA2_50 | OPRA2_51 | OPRA2_52 | OPRA2_53 |
    OPRA2_54 | OPRA2_55 | OPRA2_56 | OPRA2_57 |
    OPRA2_60 | OPRA2_61 | OPRA2_62 | OPRA2_63 |
    OPRA2_64 | OPRA2_65 | OPRA2_66 | OPRA2_67 |
    OPRA2_70 | OPRA2_71 | OPRA2_72 | OPRA2_73 |
    OPRA2_74 | OPRA2_75 | OPRA2_76 | OPRA2_77 |
    OPRA2_S0 | OPRA2_S1 | OPRA2_S2 | OPRA2_S3 |
    OPRA2_S4 | OPRA2_S5 | OPRA2_S6 | OPRA2_S7 |
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Calculus OPRA2 where
    rank _ = 2
    readRel x = case maybeRead $ map Char.toUpper x of
        Just z  -> z
        Nothing -> error $ show x ++ " is not an OPRA-2 relation."
    showRel x = (map Char.toLower) $ show x

instance BinaryCalculus OPRA2 where
    bcIdentity = OPRA2_S0

    bcConversion = Map.fromList
        [ ( OPRA2_00 , Set.singleton OPRA2_00 )
        , ( OPRA2_01 , Set.singleton OPRA2_10 )
        , ( OPRA2_02 , Set.singleton OPRA2_20 )
        , ( OPRA2_03 , Set.singleton OPRA2_30 )
        , ( OPRA2_04 , Set.singleton OPRA2_40 )
        , ( OPRA2_05 , Set.singleton OPRA2_50 )
        , ( OPRA2_06 , Set.singleton OPRA2_60 )
        , ( OPRA2_07 , Set.singleton OPRA2_70 )
        , ( OPRA2_10 , Set.singleton OPRA2_01 )
        , ( OPRA2_11 , Set.singleton OPRA2_11 )
        , ( OPRA2_12 , Set.singleton OPRA2_21 )
        , ( OPRA2_13 , Set.singleton OPRA2_31 )
        , ( OPRA2_14 , Set.singleton OPRA2_41 )
        , ( OPRA2_15 , Set.singleton OPRA2_51 )
        , ( OPRA2_16 , Set.singleton OPRA2_61 )
        , ( OPRA2_17 , Set.singleton OPRA2_71 )
        , ( OPRA2_20 , Set.singleton OPRA2_02 )
        , ( OPRA2_21 , Set.singleton OPRA2_12 )
        , ( OPRA2_22 , Set.singleton OPRA2_22 )
        , ( OPRA2_23 , Set.singleton OPRA2_32 )
        , ( OPRA2_24 , Set.singleton OPRA2_42 )
        , ( OPRA2_25 , Set.singleton OPRA2_52 )
        , ( OPRA2_26 , Set.singleton OPRA2_62 )
        , ( OPRA2_27 , Set.singleton OPRA2_72 )
        , ( OPRA2_30 , Set.singleton OPRA2_03 )
        , ( OPRA2_31 , Set.singleton OPRA2_13 )
        , ( OPRA2_32 , Set.singleton OPRA2_23 )
        , ( OPRA2_33 , Set.singleton OPRA2_33 )
        , ( OPRA2_34 , Set.singleton OPRA2_43 )
        , ( OPRA2_35 , Set.singleton OPRA2_53 )
        , ( OPRA2_36 , Set.singleton OPRA2_63 )
        , ( OPRA2_37 , Set.singleton OPRA2_73 )
        , ( OPRA2_40 , Set.singleton OPRA2_04 )
        , ( OPRA2_41 , Set.singleton OPRA2_14 )
        , ( OPRA2_42 , Set.singleton OPRA2_24 )
        , ( OPRA2_43 , Set.singleton OPRA2_34 )
        , ( OPRA2_44 , Set.singleton OPRA2_44 )
        , ( OPRA2_45 , Set.singleton OPRA2_54 )
        , ( OPRA2_46 , Set.singleton OPRA2_64 )
        , ( OPRA2_47 , Set.singleton OPRA2_74 )
        , ( OPRA2_50 , Set.singleton OPRA2_05 )
        , ( OPRA2_51 , Set.singleton OPRA2_15 )
        , ( OPRA2_52 , Set.singleton OPRA2_25 )
        , ( OPRA2_53 , Set.singleton OPRA2_35 )
        , ( OPRA2_54 , Set.singleton OPRA2_45 )
        , ( OPRA2_55 , Set.singleton OPRA2_55 )
        , ( OPRA2_56 , Set.singleton OPRA2_65 )
        , ( OPRA2_57 , Set.singleton OPRA2_75 )
        , ( OPRA2_60 , Set.singleton OPRA2_06 )
        , ( OPRA2_61 , Set.singleton OPRA2_16 )
        , ( OPRA2_62 , Set.singleton OPRA2_26 )
        , ( OPRA2_63 , Set.singleton OPRA2_36 )
        , ( OPRA2_64 , Set.singleton OPRA2_46 )
        , ( OPRA2_65 , Set.singleton OPRA2_56 )
        , ( OPRA2_66 , Set.singleton OPRA2_66 )
        , ( OPRA2_67 , Set.singleton OPRA2_76 )
        , ( OPRA2_70 , Set.singleton OPRA2_07 )
        , ( OPRA2_71 , Set.singleton OPRA2_17 )
        , ( OPRA2_72 , Set.singleton OPRA2_27 )
        , ( OPRA2_73 , Set.singleton OPRA2_37 )
        , ( OPRA2_74 , Set.singleton OPRA2_47 )
        , ( OPRA2_75 , Set.singleton OPRA2_57 )
        , ( OPRA2_76 , Set.singleton OPRA2_67 )
        , ( OPRA2_77 , Set.singleton OPRA2_77 )
        , ( OPRA2_S0 , Set.singleton OPRA2_S0 )
        , ( OPRA2_S1 , Set.singleton OPRA2_S7 )
        , ( OPRA2_S2 , Set.singleton OPRA2_S6 )
        , ( OPRA2_S3 , Set.singleton OPRA2_S5 )
        , ( OPRA2_S4 , Set.singleton OPRA2_S4 )
        , ( OPRA2_S5 , Set.singleton OPRA2_S3 )
        , ( OPRA2_S6 , Set.singleton OPRA2_S2 )
        , ( OPRA2_S7 , Set.singleton OPRA2_S1 ) ]


    bcComposition = Map.fromList
        [ ( ( OPRA2_00 , OPRA2_00 )
          , Set.fromList [ OPRA2_20, OPRA2_02, OPRA2_S0 ] )
        , ( ( OPRA2_S3 , OPRA2_S3 )
          , Set.fromList [ OPRA2_S3, OPRA2_S2, OPRA2_S1 ] )
        ]


        [ ( ( OPRA2_00 , OPRA2_00 )
          , Set.fromList [ OPRA2_40, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_00 , OPRA2_01 )
          , Set.fromList [ OPRA2_S7, OPRA2_41, OPRA2_05 ] )
        , ( ( OPRA2_00 , OPRA2_02 )
          , Set.fromList [ OPRA2_S6, OPRA2_42, OPRA2_06 ] )
        , ( ( OPRA2_00 , OPRA2_03 )
          , Set.fromList [ OPRA2_S5, OPRA2_43, OPRA2_07 ] )
        , ( ( OPRA2_00 , OPRA2_04 )
          , Set.fromList [ OPRA2_44, OPRA2_S4, OPRA2_00 ] )
        , ( ( OPRA2_00 , OPRA2_05 )
          , Set.fromList [ OPRA2_45, OPRA2_S3, OPRA2_01 ] )
        , ( ( OPRA2_00 , OPRA2_06 )
          , Set.fromList [ OPRA2_46, OPRA2_S2, OPRA2_02 ] )
        , ( ( OPRA2_00 , OPRA2_07 )
          , Set.fromList [ OPRA2_47, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_00 , OPRA2_10 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_00 , OPRA2_11 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_00 , OPRA2_12 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_00 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_00 , OPRA2_14 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_00 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55 ] )
        , ( ( OPRA2_00 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_00 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_00 , OPRA2_20 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_00 , OPRA2_21 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_00 , OPRA2_22 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_00 , OPRA2_23 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_00 , OPRA2_24 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_00 , OPRA2_25 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_00 , OPRA2_26 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_00 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_00 , OPRA2_30 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_00 , OPRA2_31 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_00 , OPRA2_32 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_00 , OPRA2_33 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_00 , OPRA2_34 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_00 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_00 , OPRA2_36 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_00 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_00 , OPRA2_40 )
          , Set.fromList [ OPRA2_00 ] )
        , ( ( OPRA2_00 , OPRA2_41 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_00 , OPRA2_42 )
          , Set.fromList [ OPRA2_02 ] )
        , ( ( OPRA2_00 , OPRA2_43 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_00 , OPRA2_44 )
          , Set.fromList [ OPRA2_04 ] )
        , ( ( OPRA2_00 , OPRA2_45 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_00 , OPRA2_46 )
          , Set.fromList [ OPRA2_06 ] )
        , ( ( OPRA2_00 , OPRA2_47 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_00 , OPRA2_50 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_00 , OPRA2_51 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_00 , OPRA2_52 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_00 , OPRA2_53 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_00 , OPRA2_54 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_00 , OPRA2_55 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_00 , OPRA2_56 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_00 , OPRA2_57 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_00 , OPRA2_60 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_00 , OPRA2_61 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_00 , OPRA2_62 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_00 , OPRA2_63 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_00 , OPRA2_64 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_00 , OPRA2_65 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_00 , OPRA2_66 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_00 , OPRA2_67 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_00 , OPRA2_70 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_00 , OPRA2_71 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_00 , OPRA2_72 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_00 , OPRA2_73 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_00 , OPRA2_74 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_00 , OPRA2_75 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_00 , OPRA2_76 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_00 , OPRA2_77 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_00 , OPRA2_S0 )
          , Set.fromList [ OPRA2_00 ] )
        , ( ( OPRA2_00 , OPRA2_S1 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_00 , OPRA2_S2 )
          , Set.fromList [ OPRA2_06 ] )
        , ( ( OPRA2_00 , OPRA2_S3 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_00 , OPRA2_S4 )
          , Set.fromList [ OPRA2_04 ] )
        , ( ( OPRA2_00 , OPRA2_S5 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_00 , OPRA2_S6 )
          , Set.fromList [ OPRA2_02 ] )
        , ( ( OPRA2_00 , OPRA2_S7 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_01 , OPRA2_00 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_01 , OPRA2_01 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_01 , OPRA2_02 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_01 , OPRA2_03 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_01 , OPRA2_04 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_01 , OPRA2_05 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_01 , OPRA2_06 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_01 , OPRA2_07 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_01 , OPRA2_10 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_40, OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_01 , OPRA2_11 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_05 ] )
        , ( ( OPRA2_01 , OPRA2_12 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_S6
                         , OPRA2_53, OPRA2_42, OPRA2_31, OPRA2_21, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_06 ] )
        , ( ( OPRA2_01 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_S5, OPRA2_43, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_01 , OPRA2_14 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_44, OPRA2_S4, OPRA2_33, OPRA2_23, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_00 ] )
        , ( ( OPRA2_01 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_01 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_46, OPRA2_35, OPRA2_25, OPRA2_S2, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_02 ] )
        , ( ( OPRA2_01 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_01 , OPRA2_20 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_01 , OPRA2_21 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_01 , OPRA2_22 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_01 , OPRA2_23 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_01 , OPRA2_24 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_01 , OPRA2_25 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55 ] )
        , ( ( OPRA2_01 , OPRA2_26 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_01 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_01 , OPRA2_30 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_01 , OPRA2_31 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_01 , OPRA2_32 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_01 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_01 , OPRA2_34 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_01 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55 ] )
        , ( ( OPRA2_01 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_01 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_01 , OPRA2_40 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_01 , OPRA2_41 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_01 , OPRA2_42 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_01 , OPRA2_43 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_01 , OPRA2_44 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_01 , OPRA2_45 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_01 , OPRA2_46 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_01 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_01 , OPRA2_50 )
          , Set.fromList [ OPRA2_71, OPRA2_17, OPRA2_00 ] )
        , ( ( OPRA2_01 , OPRA2_51 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_01 ] )
        , ( ( OPRA2_01 , OPRA2_52 )
          , Set.fromList [ OPRA2_73, OPRA2_11, OPRA2_02 ] )
        , ( ( OPRA2_01 , OPRA2_53 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03 ] )
        , ( ( OPRA2_01 , OPRA2_54 )
          , Set.fromList [ OPRA2_75, OPRA2_13, OPRA2_04 ] )
        , ( ( OPRA2_01 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05 ] )
        , ( ( OPRA2_01 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_15, OPRA2_06 ] )
        , ( ( OPRA2_01 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07 ] )
        , ( ( OPRA2_01 , OPRA2_60 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_01 , OPRA2_61 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_01 , OPRA2_62 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_01 , OPRA2_63 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_01 , OPRA2_64 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_01 , OPRA2_65 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_01 , OPRA2_66 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_01 , OPRA2_67 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_01 , OPRA2_70 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_01 , OPRA2_71 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_01 , OPRA2_72 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_01 , OPRA2_73 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_01 , OPRA2_74 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_01 , OPRA2_75 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_01 , OPRA2_76 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_01 , OPRA2_77 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_01 , OPRA2_S0 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_01 , OPRA2_S1 )
          , Set.fromList [ OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_01 , OPRA2_S2 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_01 , OPRA2_S3 )
          , Set.fromList [ OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_01 , OPRA2_S4 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_01 , OPRA2_S5 )
          , Set.fromList [ OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_01 , OPRA2_S6 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_01 , OPRA2_S7 )
          , Set.fromList [ OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_02 , OPRA2_00 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_02 , OPRA2_01 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_02 , OPRA2_02 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_02 , OPRA2_03 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_02 , OPRA2_04 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_02 , OPRA2_05 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_02 , OPRA2_06 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_02 , OPRA2_07 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_02 , OPRA2_10 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_02 , OPRA2_11 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_02 , OPRA2_12 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_02 , OPRA2_13 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_02 , OPRA2_14 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_02 , OPRA2_15 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_02 , OPRA2_16 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_02 , OPRA2_17 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_02 , OPRA2_20 )
          , Set.fromList [ OPRA2_40, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_02 , OPRA2_21 )
          , Set.fromList [ OPRA2_S7, OPRA2_41, OPRA2_05 ] )
        , ( ( OPRA2_02 , OPRA2_22 )
          , Set.fromList [ OPRA2_S6, OPRA2_42, OPRA2_06 ] )
        , ( ( OPRA2_02 , OPRA2_23 )
          , Set.fromList [ OPRA2_S5, OPRA2_43, OPRA2_07 ] )
        , ( ( OPRA2_02 , OPRA2_24 )
          , Set.fromList [ OPRA2_44, OPRA2_S4, OPRA2_00 ] )
        , ( ( OPRA2_02 , OPRA2_25 )
          , Set.fromList [ OPRA2_45, OPRA2_S3, OPRA2_01 ] )
        , ( ( OPRA2_02 , OPRA2_26 )
          , Set.fromList [ OPRA2_46, OPRA2_S2, OPRA2_02 ] )
        , ( ( OPRA2_02 , OPRA2_27 )
          , Set.fromList [ OPRA2_47, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_02 , OPRA2_30 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_02 , OPRA2_31 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_02 , OPRA2_32 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_02 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_02 , OPRA2_34 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_02 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55 ] )
        , ( ( OPRA2_02 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_02 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_02 , OPRA2_40 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_02 , OPRA2_41 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_02 , OPRA2_42 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_02 , OPRA2_43 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_02 , OPRA2_44 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_02 , OPRA2_45 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_02 , OPRA2_46 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_02 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_02 , OPRA2_50 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_02 , OPRA2_51 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_02 , OPRA2_52 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_02 , OPRA2_53 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_02 , OPRA2_54 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_02 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_02 , OPRA2_56 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_02 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_02 , OPRA2_60 )
          , Set.fromList [ OPRA2_00 ] )
        , ( ( OPRA2_02 , OPRA2_61 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_02 , OPRA2_62 )
          , Set.fromList [ OPRA2_02 ] )
        , ( ( OPRA2_02 , OPRA2_63 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_02 , OPRA2_64 )
          , Set.fromList [ OPRA2_04 ] )
        , ( ( OPRA2_02 , OPRA2_65 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_02 , OPRA2_66 )
          , Set.fromList [ OPRA2_06 ] )
        , ( ( OPRA2_02 , OPRA2_67 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_02 , OPRA2_70 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_02 , OPRA2_71 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_02 , OPRA2_72 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_02 , OPRA2_73 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_02 , OPRA2_74 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_02 , OPRA2_75 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_02 , OPRA2_76 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_02 , OPRA2_77 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_02 , OPRA2_S0 )
          , Set.fromList [ OPRA2_02 ] )
        , ( ( OPRA2_02 , OPRA2_S1 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_02 , OPRA2_S2 )
          , Set.fromList [ OPRA2_00 ] )
        , ( ( OPRA2_02 , OPRA2_S3 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_02 , OPRA2_S4 )
          , Set.fromList [ OPRA2_06 ] )
        , ( ( OPRA2_02 , OPRA2_S5 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_02 , OPRA2_S6 )
          , Set.fromList [ OPRA2_04 ] )
        , ( ( OPRA2_02 , OPRA2_S7 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_03 , OPRA2_00 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_03 , OPRA2_01 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_03 , OPRA2_02 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_03 , OPRA2_03 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_03 , OPRA2_04 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_03 , OPRA2_05 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_03 , OPRA2_06 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_03 , OPRA2_07 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_03 , OPRA2_10 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_03 , OPRA2_11 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_03 , OPRA2_12 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_03 , OPRA2_13 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_03 , OPRA2_14 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_03 , OPRA2_15 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_03 , OPRA2_16 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_03 , OPRA2_17 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_03 , OPRA2_20 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_03 , OPRA2_21 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_03 , OPRA2_22 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_03 , OPRA2_23 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_03 , OPRA2_24 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_03 , OPRA2_25 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_03 , OPRA2_26 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_03 , OPRA2_27 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_03 , OPRA2_30 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_40, OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_03 , OPRA2_31 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_05 ] )
        , ( ( OPRA2_03 , OPRA2_32 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_S6
                         , OPRA2_53, OPRA2_42, OPRA2_31, OPRA2_21, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_06 ] )
        , ( ( OPRA2_03 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_S5, OPRA2_43, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_03 , OPRA2_34 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_44, OPRA2_S4, OPRA2_33, OPRA2_23, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_00 ] )
        , ( ( OPRA2_03 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_03 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_46, OPRA2_35, OPRA2_25, OPRA2_S2, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_02 ] )
        , ( ( OPRA2_03 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_03 , OPRA2_40 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_03 , OPRA2_41 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_03 , OPRA2_42 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_03 , OPRA2_43 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_03 , OPRA2_44 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_03 , OPRA2_45 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55 ] )
        , ( ( OPRA2_03 , OPRA2_46 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_03 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_03 , OPRA2_50 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_03 , OPRA2_51 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_03 , OPRA2_52 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_03 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_03 , OPRA2_54 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_03 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55 ] )
        , ( ( OPRA2_03 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_03 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_03 , OPRA2_60 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_03 , OPRA2_61 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_03 , OPRA2_62 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_03 , OPRA2_63 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_03 , OPRA2_64 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_03 , OPRA2_65 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_03 , OPRA2_66 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_03 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_03 , OPRA2_70 )
          , Set.fromList [ OPRA2_71, OPRA2_17, OPRA2_00 ] )
        , ( ( OPRA2_03 , OPRA2_71 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_01 ] )
        , ( ( OPRA2_03 , OPRA2_72 )
          , Set.fromList [ OPRA2_73, OPRA2_11, OPRA2_02 ] )
        , ( ( OPRA2_03 , OPRA2_73 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03 ] )
        , ( ( OPRA2_03 , OPRA2_74 )
          , Set.fromList [ OPRA2_75, OPRA2_13, OPRA2_04 ] )
        , ( ( OPRA2_03 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05 ] )
        , ( ( OPRA2_03 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_15, OPRA2_06 ] )
        , ( ( OPRA2_03 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07 ] )
        , ( ( OPRA2_03 , OPRA2_S0 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_03 , OPRA2_S1 )
          , Set.fromList [ OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_03 , OPRA2_S2 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_03 , OPRA2_S3 )
          , Set.fromList [ OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_03 , OPRA2_S4 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_03 , OPRA2_S5 )
          , Set.fromList [ OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_03 , OPRA2_S6 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_03 , OPRA2_S7 )
          , Set.fromList [ OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_04 , OPRA2_00 )
          , Set.fromList [ OPRA2_00 ] )
        , ( ( OPRA2_04 , OPRA2_01 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_04 , OPRA2_02 )
          , Set.fromList [ OPRA2_02 ] )
        , ( ( OPRA2_04 , OPRA2_03 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_04 , OPRA2_04 )
          , Set.fromList [ OPRA2_04 ] )
        , ( ( OPRA2_04 , OPRA2_05 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_04 , OPRA2_06 )
          , Set.fromList [ OPRA2_06 ] )
        , ( ( OPRA2_04 , OPRA2_07 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_04 , OPRA2_10 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_04 , OPRA2_11 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_04 , OPRA2_12 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_04 , OPRA2_13 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_04 , OPRA2_14 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_04 , OPRA2_15 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_04 , OPRA2_16 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_04 , OPRA2_17 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_04 , OPRA2_20 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_04 , OPRA2_21 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_04 , OPRA2_22 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_04 , OPRA2_23 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_04 , OPRA2_24 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_04 , OPRA2_25 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_04 , OPRA2_26 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_04 , OPRA2_27 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_04 , OPRA2_30 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_04 , OPRA2_31 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_04 , OPRA2_32 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_04 , OPRA2_33 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_04 , OPRA2_34 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_04 , OPRA2_35 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_04 , OPRA2_36 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_04 , OPRA2_37 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_04 , OPRA2_40 )
          , Set.fromList [ OPRA2_40, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_04 , OPRA2_41 )
          , Set.fromList [ OPRA2_S7, OPRA2_41, OPRA2_05 ] )
        , ( ( OPRA2_04 , OPRA2_42 )
          , Set.fromList [ OPRA2_S6, OPRA2_42, OPRA2_06 ] )
        , ( ( OPRA2_04 , OPRA2_43 )
          , Set.fromList [ OPRA2_S5, OPRA2_43, OPRA2_07 ] )
        , ( ( OPRA2_04 , OPRA2_44 )
          , Set.fromList [ OPRA2_44, OPRA2_S4, OPRA2_00 ] )
        , ( ( OPRA2_04 , OPRA2_45 )
          , Set.fromList [ OPRA2_45, OPRA2_S3, OPRA2_01 ] )
        , ( ( OPRA2_04 , OPRA2_46 )
          , Set.fromList [ OPRA2_46, OPRA2_S2, OPRA2_02 ] )
        , ( ( OPRA2_04 , OPRA2_47 )
          , Set.fromList [ OPRA2_47, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_04 , OPRA2_50 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_04 , OPRA2_51 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_04 , OPRA2_52 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_04 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_04 , OPRA2_54 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_04 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55 ] )
        , ( ( OPRA2_04 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_04 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_04 , OPRA2_60 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_04 , OPRA2_61 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_04 , OPRA2_62 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_04 , OPRA2_63 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_04 , OPRA2_64 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_04 , OPRA2_65 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_04 , OPRA2_66 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_04 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_04 , OPRA2_70 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_04 , OPRA2_71 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_04 , OPRA2_72 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_04 , OPRA2_73 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_04 , OPRA2_74 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_04 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_04 , OPRA2_76 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_04 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_04 , OPRA2_S0 )
          , Set.fromList [ OPRA2_04 ] )
        , ( ( OPRA2_04 , OPRA2_S1 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_04 , OPRA2_S2 )
          , Set.fromList [ OPRA2_02 ] )
        , ( ( OPRA2_04 , OPRA2_S3 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_04 , OPRA2_S4 )
          , Set.fromList [ OPRA2_00 ] )
        , ( ( OPRA2_04 , OPRA2_S5 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_04 , OPRA2_S6 )
          , Set.fromList [ OPRA2_06 ] )
        , ( ( OPRA2_04 , OPRA2_S7 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_05 , OPRA2_00 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_05 , OPRA2_01 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_05 , OPRA2_02 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_05 , OPRA2_03 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_05 , OPRA2_04 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_05 , OPRA2_05 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_05 , OPRA2_06 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_05 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_05 , OPRA2_10 )
          , Set.fromList [ OPRA2_71, OPRA2_17, OPRA2_00 ] )
        , ( ( OPRA2_05 , OPRA2_11 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_01 ] )
        , ( ( OPRA2_05 , OPRA2_12 )
          , Set.fromList [ OPRA2_73, OPRA2_11, OPRA2_02 ] )
        , ( ( OPRA2_05 , OPRA2_13 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03 ] )
        , ( ( OPRA2_05 , OPRA2_14 )
          , Set.fromList [ OPRA2_75, OPRA2_13, OPRA2_04 ] )
        , ( ( OPRA2_05 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05 ] )
        , ( ( OPRA2_05 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_15, OPRA2_06 ] )
        , ( ( OPRA2_05 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07 ] )
        , ( ( OPRA2_05 , OPRA2_20 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_05 , OPRA2_21 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_05 , OPRA2_22 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_05 , OPRA2_23 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_05 , OPRA2_24 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_05 , OPRA2_25 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_05 , OPRA2_26 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_05 , OPRA2_27 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_05 , OPRA2_30 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_05 , OPRA2_31 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_05 , OPRA2_32 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_05 , OPRA2_33 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_05 , OPRA2_34 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_05 , OPRA2_35 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_05 , OPRA2_36 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_05 , OPRA2_37 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_05 , OPRA2_40 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_05 , OPRA2_41 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_05 , OPRA2_42 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_05 , OPRA2_43 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_05 , OPRA2_44 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_05 , OPRA2_45 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_05 , OPRA2_46 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_05 , OPRA2_47 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_05 , OPRA2_50 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_40, OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_05 , OPRA2_51 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_05 ] )
        , ( ( OPRA2_05 , OPRA2_52 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_S6
                         , OPRA2_53, OPRA2_42, OPRA2_31, OPRA2_21, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_06 ] )
        , ( ( OPRA2_05 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_S5, OPRA2_43, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_05 , OPRA2_54 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_44, OPRA2_S4, OPRA2_33, OPRA2_23, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_00 ] )
        , ( ( OPRA2_05 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_05 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_46, OPRA2_35, OPRA2_25, OPRA2_S2, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_02 ] )
        , ( ( OPRA2_05 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_05 , OPRA2_60 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_05 , OPRA2_61 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_05 , OPRA2_62 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_05 , OPRA2_63 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_05 , OPRA2_64 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_05 , OPRA2_65 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55 ] )
        , ( ( OPRA2_05 , OPRA2_66 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_05 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_05 , OPRA2_70 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_05 , OPRA2_71 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_05 , OPRA2_72 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_05 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_05 , OPRA2_74 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_05 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55 ] )
        , ( ( OPRA2_05 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_05 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_05 , OPRA2_S0 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_05 , OPRA2_S1 )
          , Set.fromList [ OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_05 , OPRA2_S2 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_05 , OPRA2_S3 )
          , Set.fromList [ OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_05 , OPRA2_S4 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_05 , OPRA2_S5 )
          , Set.fromList [ OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_05 , OPRA2_S6 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_05 , OPRA2_S7 )
          , Set.fromList [ OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_06 , OPRA2_00 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_06 , OPRA2_01 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_06 , OPRA2_02 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_06 , OPRA2_03 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_06 , OPRA2_04 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_06 , OPRA2_05 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_06 , OPRA2_06 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_06 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_06 , OPRA2_10 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_06 , OPRA2_11 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_06 , OPRA2_12 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_06 , OPRA2_13 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_06 , OPRA2_14 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_06 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_06 , OPRA2_16 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_06 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_06 , OPRA2_20 )
          , Set.fromList [ OPRA2_00 ] )
        , ( ( OPRA2_06 , OPRA2_21 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_06 , OPRA2_22 )
          , Set.fromList [ OPRA2_02 ] )
        , ( ( OPRA2_06 , OPRA2_23 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_06 , OPRA2_24 )
          , Set.fromList [ OPRA2_04 ] )
        , ( ( OPRA2_06 , OPRA2_25 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_06 , OPRA2_26 )
          , Set.fromList [ OPRA2_06 ] )
        , ( ( OPRA2_06 , OPRA2_27 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_06 , OPRA2_30 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_06 , OPRA2_31 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_06 , OPRA2_32 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_06 , OPRA2_33 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_06 , OPRA2_34 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_06 , OPRA2_35 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_06 , OPRA2_36 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_06 , OPRA2_37 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_06 , OPRA2_40 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_06 , OPRA2_41 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_06 , OPRA2_42 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_06 , OPRA2_43 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_06 , OPRA2_44 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_06 , OPRA2_45 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_06 , OPRA2_46 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_06 , OPRA2_47 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_06 , OPRA2_50 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_06 , OPRA2_51 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_06 , OPRA2_52 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_06 , OPRA2_53 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_06 , OPRA2_54 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_06 , OPRA2_55 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_06 , OPRA2_56 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_06 , OPRA2_57 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_06 , OPRA2_60 )
          , Set.fromList [ OPRA2_40, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_06 , OPRA2_61 )
          , Set.fromList [ OPRA2_S7, OPRA2_41, OPRA2_05 ] )
        , ( ( OPRA2_06 , OPRA2_62 )
          , Set.fromList [ OPRA2_S6, OPRA2_42, OPRA2_06 ] )
        , ( ( OPRA2_06 , OPRA2_63 )
          , Set.fromList [ OPRA2_S5, OPRA2_43, OPRA2_07 ] )
        , ( ( OPRA2_06 , OPRA2_64 )
          , Set.fromList [ OPRA2_44, OPRA2_S4, OPRA2_00 ] )
        , ( ( OPRA2_06 , OPRA2_65 )
          , Set.fromList [ OPRA2_45, OPRA2_S3, OPRA2_01 ] )
        , ( ( OPRA2_06 , OPRA2_66 )
          , Set.fromList [ OPRA2_46, OPRA2_S2, OPRA2_02 ] )
        , ( ( OPRA2_06 , OPRA2_67 )
          , Set.fromList [ OPRA2_47, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_06 , OPRA2_70 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_06 , OPRA2_71 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_06 , OPRA2_72 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_06 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_06 , OPRA2_74 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_06 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55 ] )
        , ( ( OPRA2_06 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_06 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_06 , OPRA2_S0 )
          , Set.fromList [ OPRA2_06 ] )
        , ( ( OPRA2_06 , OPRA2_S1 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_06 , OPRA2_S2 )
          , Set.fromList [ OPRA2_04 ] )
        , ( ( OPRA2_06 , OPRA2_S3 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_06 , OPRA2_S4 )
          , Set.fromList [ OPRA2_02 ] )
        , ( ( OPRA2_06 , OPRA2_S5 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_06 , OPRA2_S6 )
          , Set.fromList [ OPRA2_00 ] )
        , ( ( OPRA2_06 , OPRA2_S7 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_07 , OPRA2_00 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_07 , OPRA2_01 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_07 , OPRA2_02 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_07 , OPRA2_03 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_07 , OPRA2_04 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_07 , OPRA2_05 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55 ] )
        , ( ( OPRA2_07 , OPRA2_06 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_07 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_07 , OPRA2_10 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_07 , OPRA2_11 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_07 , OPRA2_12 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_07 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_07 , OPRA2_14 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_07 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55 ] )
        , ( ( OPRA2_07 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_07 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_07 , OPRA2_20 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_07 , OPRA2_21 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_07 , OPRA2_22 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_07 , OPRA2_23 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_07 , OPRA2_24 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_07 , OPRA2_25 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_07 , OPRA2_26 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_07 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_07 , OPRA2_30 )
          , Set.fromList [ OPRA2_71, OPRA2_17, OPRA2_00 ] )
        , ( ( OPRA2_07 , OPRA2_31 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_01 ] )
        , ( ( OPRA2_07 , OPRA2_32 )
          , Set.fromList [ OPRA2_73, OPRA2_11, OPRA2_02 ] )
        , ( ( OPRA2_07 , OPRA2_33 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03 ] )
        , ( ( OPRA2_07 , OPRA2_34 )
          , Set.fromList [ OPRA2_75, OPRA2_13, OPRA2_04 ] )
        , ( ( OPRA2_07 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05 ] )
        , ( ( OPRA2_07 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_15, OPRA2_06 ] )
        , ( ( OPRA2_07 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07 ] )
        , ( ( OPRA2_07 , OPRA2_40 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_07 , OPRA2_41 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_07 , OPRA2_42 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_07 , OPRA2_43 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_07 , OPRA2_44 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_07 , OPRA2_45 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_07 , OPRA2_46 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_07 , OPRA2_47 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_07 , OPRA2_50 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_07 , OPRA2_51 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_07 , OPRA2_52 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_07 , OPRA2_53 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_07 , OPRA2_54 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_07 , OPRA2_55 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_07 , OPRA2_56 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_07 , OPRA2_57 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_07 , OPRA2_60 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_07 , OPRA2_61 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_07 , OPRA2_62 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_07 , OPRA2_63 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_07 , OPRA2_64 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_07 , OPRA2_65 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_07 , OPRA2_66 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_07 , OPRA2_67 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_07 , OPRA2_70 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_40, OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_07 , OPRA2_71 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_05 ] )
        , ( ( OPRA2_07 , OPRA2_72 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_S6
                         , OPRA2_53, OPRA2_42, OPRA2_31, OPRA2_21, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_06 ] )
        , ( ( OPRA2_07 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_S5, OPRA2_43, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_07 , OPRA2_74 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_44, OPRA2_S4, OPRA2_33, OPRA2_23, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_00 ] )
        , ( ( OPRA2_07 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_07 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_46, OPRA2_35, OPRA2_25, OPRA2_S2, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_02 ] )
        , ( ( OPRA2_07 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_07 , OPRA2_S0 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_07 , OPRA2_S1 )
          , Set.fromList [ OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_07 , OPRA2_S2 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_07 , OPRA2_S3 )
          , Set.fromList [ OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_07 , OPRA2_S4 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_07 , OPRA2_S5 )
          , Set.fromList [ OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_07 , OPRA2_S6 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_07 , OPRA2_S7 )
          , Set.fromList [ OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_10 , OPRA2_00 )
          , Set.fromList [ OPRA2_50, OPRA2_14, OPRA2_S1 ] )
        , ( ( OPRA2_10 , OPRA2_01 )
          , Set.fromList [ OPRA2_S7, OPRA2_51, OPRA2_15, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_10 , OPRA2_02 )
          , Set.fromList [ OPRA2_S7, OPRA2_52, OPRA2_16 ] )
        , ( ( OPRA2_10 , OPRA2_03 )
          , Set.fromList [ OPRA2_S7, OPRA2_S6, OPRA2_53, OPRA2_S5, OPRA2_17 ] )
        , ( ( OPRA2_10 , OPRA2_04 )
          , Set.fromList [ OPRA2_54, OPRA2_S5, OPRA2_10 ] )
        , ( ( OPRA2_10 , OPRA2_05 )
          , Set.fromList [ OPRA2_55, OPRA2_S5, OPRA2_S4, OPRA2_S3, OPRA2_11 ] )
        , ( ( OPRA2_10 , OPRA2_06 )
          , Set.fromList [ OPRA2_56, OPRA2_S3, OPRA2_12 ] )
        , ( ( OPRA2_10 , OPRA2_07 )
          , Set.fromList [ OPRA2_57, OPRA2_S3, OPRA2_S2, OPRA2_13, OPRA2_S1 ] )
        , ( ( OPRA2_10 , OPRA2_10 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_10 , OPRA2_11 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_10 , OPRA2_12 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_10 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_10 , OPRA2_14 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_10 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_10 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_10 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_10 , OPRA2_20 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_10 , OPRA2_21 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_10 , OPRA2_22 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_10 , OPRA2_23 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_10 , OPRA2_24 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_10 , OPRA2_25 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_10 , OPRA2_26 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_10 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_10 , OPRA2_30 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_10 , OPRA2_31 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_10 , OPRA2_32 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_10 , OPRA2_33 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_10 , OPRA2_34 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_10 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_10 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_10 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_10 , OPRA2_40 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_10 , OPRA2_41 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_10 , OPRA2_42 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_10 , OPRA2_43 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_10 , OPRA2_44 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_10 , OPRA2_45 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_10 , OPRA2_46 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_10 , OPRA2_47 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_10 , OPRA2_50 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_10 , OPRA2_51 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_10 , OPRA2_52 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_10 , OPRA2_53 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_10 , OPRA2_54 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_10 , OPRA2_55 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_10 , OPRA2_56 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_10 , OPRA2_57 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_10 , OPRA2_60 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_10 , OPRA2_61 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_10 , OPRA2_62 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_10 , OPRA2_63 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_10 , OPRA2_64 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_10 , OPRA2_65 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_10 , OPRA2_66 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_10 , OPRA2_67 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_10 , OPRA2_70 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_10 , OPRA2_71 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_10 , OPRA2_72 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_10 , OPRA2_73 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_10 , OPRA2_74 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_10 , OPRA2_75 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_10 , OPRA2_76 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_10 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_10 , OPRA2_S0 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_10 , OPRA2_S1 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_10 , OPRA2_S2 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_10 , OPRA2_S3 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_10 , OPRA2_S4 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_10 , OPRA2_S5 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_10 , OPRA2_S6 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_10 , OPRA2_S7 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_11 , OPRA2_00 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_11 , OPRA2_01 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_11 , OPRA2_02 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_11 , OPRA2_03 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_11 , OPRA2_04 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_11 , OPRA2_05 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_11 , OPRA2_06 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_11 , OPRA2_07 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_11 , OPRA2_10 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_S1, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_11 , OPRA2_11 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_57
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10
                         , OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01, OPRA2_S0 ] )
        , ( ( OPRA2_11 , OPRA2_12 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_63
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_37
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_11, OPRA2_10, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_11 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_S6
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_S5, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_37
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05
                         , OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_11 , OPRA2_14 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_43, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_11 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45
                         , OPRA2_44, OPRA2_43, OPRA2_S4, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_11 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_17, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_11 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_S3, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_S2, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_S1, OPRA2_07, OPRA2_03, OPRA2_02
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_11 , OPRA2_20 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_11 , OPRA2_21 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_11 , OPRA2_22 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_11 , OPRA2_23 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_11 , OPRA2_24 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_11 , OPRA2_25 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_11 , OPRA2_26 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_11 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_11 , OPRA2_30 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_11 , OPRA2_31 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_11 , OPRA2_32 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_11 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_11 , OPRA2_34 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_11 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_11 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_11 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_11 , OPRA2_40 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_11 , OPRA2_41 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_11 , OPRA2_42 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_11 , OPRA2_43 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_11 , OPRA2_44 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_11 , OPRA2_45 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_11 , OPRA2_46 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_11 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_11 , OPRA2_50 )
          , Set.fromList [ OPRA2_71, OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_01 ] )
        , ( ( OPRA2_11 , OPRA2_51 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_37, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_11 , OPRA2_52 )
          , Set.fromList [ OPRA2_73, OPRA2_31, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03 ] )
        , ( ( OPRA2_11 , OPRA2_53 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_05
                         , OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_11 , OPRA2_54 )
          , Set.fromList [ OPRA2_75, OPRA2_33, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05 ] )
        , ( ( OPRA2_11 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_11 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_35, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07 ] )
        , ( ( OPRA2_11 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_11 , OPRA2_60 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_11 , OPRA2_61 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_11 , OPRA2_62 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_11 , OPRA2_63 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_11 , OPRA2_64 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_11 , OPRA2_65 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_11 , OPRA2_66 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_11 , OPRA2_67 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_11 , OPRA2_70 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_11 , OPRA2_71 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_11 , OPRA2_72 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_11 , OPRA2_73 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_11 , OPRA2_74 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_11 , OPRA2_75 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_11 , OPRA2_76 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_11 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_11 , OPRA2_S0 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_11 , OPRA2_S1 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_11 , OPRA2_S2 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_11 , OPRA2_S3 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_11 , OPRA2_S4 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_11 , OPRA2_S5 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_11 , OPRA2_S6 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_11 , OPRA2_S7 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_12 , OPRA2_00 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_12 , OPRA2_01 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_12 , OPRA2_02 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_12 , OPRA2_03 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_12 , OPRA2_04 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_12 , OPRA2_05 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_12 , OPRA2_06 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_12 , OPRA2_07 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_12 , OPRA2_10 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_12 , OPRA2_11 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_12 , OPRA2_12 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_12 , OPRA2_13 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_12 , OPRA2_14 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_12 , OPRA2_15 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_12 , OPRA2_16 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_12 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_12 , OPRA2_20 )
          , Set.fromList [ OPRA2_50, OPRA2_14, OPRA2_S1 ] )
        , ( ( OPRA2_12 , OPRA2_21 )
          , Set.fromList [ OPRA2_S7, OPRA2_51, OPRA2_15, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_12 , OPRA2_22 )
          , Set.fromList [ OPRA2_S7, OPRA2_52, OPRA2_16 ] )
        , ( ( OPRA2_12 , OPRA2_23 )
          , Set.fromList [ OPRA2_S7, OPRA2_S6, OPRA2_53, OPRA2_S5, OPRA2_17 ] )
        , ( ( OPRA2_12 , OPRA2_24 )
          , Set.fromList [ OPRA2_54, OPRA2_S5, OPRA2_10 ] )
        , ( ( OPRA2_12 , OPRA2_25 )
          , Set.fromList [ OPRA2_55, OPRA2_S5, OPRA2_S4, OPRA2_S3, OPRA2_11 ] )
        , ( ( OPRA2_12 , OPRA2_26 )
          , Set.fromList [ OPRA2_56, OPRA2_S3, OPRA2_12 ] )
        , ( ( OPRA2_12 , OPRA2_27 )
          , Set.fromList [ OPRA2_57, OPRA2_S3, OPRA2_S2, OPRA2_13, OPRA2_S1 ] )
        , ( ( OPRA2_12 , OPRA2_30 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_12 , OPRA2_31 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_12 , OPRA2_32 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_12 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_12 , OPRA2_34 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_12 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_12 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_12 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_12 , OPRA2_40 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_12 , OPRA2_41 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_12 , OPRA2_42 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_12 , OPRA2_43 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_12 , OPRA2_44 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_12 , OPRA2_45 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_12 , OPRA2_46 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_12 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_12 , OPRA2_50 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_12 , OPRA2_51 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_12 , OPRA2_52 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_12 , OPRA2_53 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_12 , OPRA2_54 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_12 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_12 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_12 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_12 , OPRA2_60 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_12 , OPRA2_61 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_12 , OPRA2_62 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_12 , OPRA2_63 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_12 , OPRA2_64 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_12 , OPRA2_65 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_12 , OPRA2_66 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_12 , OPRA2_67 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_12 , OPRA2_70 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_12 , OPRA2_71 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_12 , OPRA2_72 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_12 , OPRA2_73 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_12 , OPRA2_74 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_12 , OPRA2_75 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_12 , OPRA2_76 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_12 , OPRA2_77 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_12 , OPRA2_S0 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_12 , OPRA2_S1 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_12 , OPRA2_S2 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_12 , OPRA2_S3 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_12 , OPRA2_S4 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_12 , OPRA2_S5 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_12 , OPRA2_S6 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_12 , OPRA2_S7 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_13 , OPRA2_00 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_13 , OPRA2_01 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_13 , OPRA2_02 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_13 , OPRA2_03 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_13 , OPRA2_04 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_13 , OPRA2_05 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_13 , OPRA2_06 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_13 , OPRA2_07 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_13 , OPRA2_10 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_13 , OPRA2_11 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_13 , OPRA2_12 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_13 , OPRA2_13 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_13 , OPRA2_14 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_13 , OPRA2_15 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_13 , OPRA2_16 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_13 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_13 , OPRA2_20 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_13 , OPRA2_21 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_13 , OPRA2_22 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_13 , OPRA2_23 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_13 , OPRA2_24 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_13 , OPRA2_25 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_13 , OPRA2_26 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_13 , OPRA2_27 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_13 , OPRA2_30 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_S1, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_13 , OPRA2_31 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_57
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10
                         , OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01, OPRA2_S0 ] )
        , ( ( OPRA2_13 , OPRA2_32 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_63
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_37
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_11, OPRA2_10, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_13 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_S6
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_S5, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_37
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05
                         , OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_13 , OPRA2_34 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_43, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_13 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45
                         , OPRA2_44, OPRA2_43, OPRA2_S4, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_13 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_17, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_13 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_S3, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_S2, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_S1, OPRA2_07, OPRA2_03, OPRA2_02
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_13 , OPRA2_40 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_13 , OPRA2_41 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_13 , OPRA2_42 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_13 , OPRA2_43 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_13 , OPRA2_44 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_13 , OPRA2_45 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_13 , OPRA2_46 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_13 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_13 , OPRA2_50 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_13 , OPRA2_51 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_13 , OPRA2_52 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_13 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_13 , OPRA2_54 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_13 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_13 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_13 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_13 , OPRA2_60 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_13 , OPRA2_61 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_13 , OPRA2_62 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_13 , OPRA2_63 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_13 , OPRA2_64 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_13 , OPRA2_65 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_13 , OPRA2_66 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_13 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_13 , OPRA2_70 )
          , Set.fromList [ OPRA2_71, OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_01 ] )
        , ( ( OPRA2_13 , OPRA2_71 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_37, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_13 , OPRA2_72 )
          , Set.fromList [ OPRA2_73, OPRA2_31, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03 ] )
        , ( ( OPRA2_13 , OPRA2_73 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_05
                         , OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_13 , OPRA2_74 )
          , Set.fromList [ OPRA2_75, OPRA2_33, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05 ] )
        , ( ( OPRA2_13 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_13 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_35, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07 ] )
        , ( ( OPRA2_13 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_13 , OPRA2_S0 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_13 , OPRA2_S1 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_13 , OPRA2_S2 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_13 , OPRA2_S3 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_13 , OPRA2_S4 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_13 , OPRA2_S5 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_13 , OPRA2_S6 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_13 , OPRA2_S7 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_14 , OPRA2_00 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_14 , OPRA2_01 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_14 , OPRA2_02 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_14 , OPRA2_03 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_14 , OPRA2_04 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_14 , OPRA2_05 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_14 , OPRA2_06 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_14 , OPRA2_07 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_14 , OPRA2_10 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_14 , OPRA2_11 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_14 , OPRA2_12 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_14 , OPRA2_13 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_14 , OPRA2_14 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_14 , OPRA2_15 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_14 , OPRA2_16 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_14 , OPRA2_17 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_14 , OPRA2_20 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_14 , OPRA2_21 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_14 , OPRA2_22 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_14 , OPRA2_23 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_14 , OPRA2_24 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_14 , OPRA2_25 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_14 , OPRA2_26 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_14 , OPRA2_27 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_14 , OPRA2_30 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_14 , OPRA2_31 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_14 , OPRA2_32 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_14 , OPRA2_33 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_14 , OPRA2_34 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_14 , OPRA2_35 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_14 , OPRA2_36 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_14 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_14 , OPRA2_40 )
          , Set.fromList [ OPRA2_50, OPRA2_14, OPRA2_S1 ] )
        , ( ( OPRA2_14 , OPRA2_41 )
          , Set.fromList [ OPRA2_S7, OPRA2_51, OPRA2_15, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_14 , OPRA2_42 )
          , Set.fromList [ OPRA2_S7, OPRA2_52, OPRA2_16 ] )
        , ( ( OPRA2_14 , OPRA2_43 )
          , Set.fromList [ OPRA2_S7, OPRA2_S6, OPRA2_53, OPRA2_S5, OPRA2_17 ] )
        , ( ( OPRA2_14 , OPRA2_44 )
          , Set.fromList [ OPRA2_54, OPRA2_S5, OPRA2_10 ] )
        , ( ( OPRA2_14 , OPRA2_45 )
          , Set.fromList [ OPRA2_55, OPRA2_S5, OPRA2_S4, OPRA2_S3, OPRA2_11 ] )
        , ( ( OPRA2_14 , OPRA2_46 )
          , Set.fromList [ OPRA2_56, OPRA2_S3, OPRA2_12 ] )
        , ( ( OPRA2_14 , OPRA2_47 )
          , Set.fromList [ OPRA2_57, OPRA2_S3, OPRA2_S2, OPRA2_13, OPRA2_S1 ] )
        , ( ( OPRA2_14 , OPRA2_50 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_14 , OPRA2_51 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_14 , OPRA2_52 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_14 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_14 , OPRA2_54 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_14 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_14 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_14 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_14 , OPRA2_60 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_14 , OPRA2_61 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_14 , OPRA2_62 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_14 , OPRA2_63 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_14 , OPRA2_64 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_14 , OPRA2_65 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_14 , OPRA2_66 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_14 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_14 , OPRA2_70 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_14 , OPRA2_71 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_14 , OPRA2_72 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_14 , OPRA2_73 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_14 , OPRA2_74 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_14 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_14 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_14 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_14 , OPRA2_S0 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_14 , OPRA2_S1 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_14 , OPRA2_S2 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_14 , OPRA2_S3 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_14 , OPRA2_S4 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_14 , OPRA2_S5 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_14 , OPRA2_S6 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_14 , OPRA2_S7 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_15 , OPRA2_00 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_15 , OPRA2_01 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_15 , OPRA2_02 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_15 , OPRA2_03 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_15 , OPRA2_04 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_15 , OPRA2_05 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_15 , OPRA2_06 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_15 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_15 , OPRA2_10 )
          , Set.fromList [ OPRA2_71, OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_01 ] )
        , ( ( OPRA2_15 , OPRA2_11 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_37, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_15 , OPRA2_12 )
          , Set.fromList [ OPRA2_73, OPRA2_31, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03 ] )
        , ( ( OPRA2_15 , OPRA2_13 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_05
                         , OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_15 , OPRA2_14 )
          , Set.fromList [ OPRA2_75, OPRA2_33, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05 ] )
        , ( ( OPRA2_15 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_15 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_35, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07 ] )
        , ( ( OPRA2_15 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_15 , OPRA2_20 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_15 , OPRA2_21 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_15 , OPRA2_22 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_15 , OPRA2_23 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_15 , OPRA2_24 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_15 , OPRA2_25 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_15 , OPRA2_26 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_15 , OPRA2_27 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_15 , OPRA2_30 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_15 , OPRA2_31 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_15 , OPRA2_32 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_15 , OPRA2_33 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_15 , OPRA2_34 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_15 , OPRA2_35 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_15 , OPRA2_36 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_15 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_15 , OPRA2_40 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_15 , OPRA2_41 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_15 , OPRA2_42 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_15 , OPRA2_43 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_15 , OPRA2_44 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_15 , OPRA2_45 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_15 , OPRA2_46 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_15 , OPRA2_47 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_15 , OPRA2_50 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_S1, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_15 , OPRA2_51 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_57
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10
                         , OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01, OPRA2_S0 ] )
        , ( ( OPRA2_15 , OPRA2_52 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_63
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_37
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_11, OPRA2_10, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_15 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_S6
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_S5, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_37
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05
                         , OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_15 , OPRA2_54 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_43, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_15 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45
                         , OPRA2_44, OPRA2_43, OPRA2_S4, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_15 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_17, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_15 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_S3, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_S2, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_S1, OPRA2_07, OPRA2_03, OPRA2_02
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_15 , OPRA2_60 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_15 , OPRA2_61 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_15 , OPRA2_62 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_15 , OPRA2_63 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_15 , OPRA2_64 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_15 , OPRA2_65 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_15 , OPRA2_66 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_15 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_15 , OPRA2_70 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_15 , OPRA2_71 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_15 , OPRA2_72 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_15 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_15 , OPRA2_74 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_15 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_15 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_15 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_15 , OPRA2_S0 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_15 , OPRA2_S1 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_15 , OPRA2_S2 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_15 , OPRA2_S3 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_15 , OPRA2_S4 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_15 , OPRA2_S5 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_15 , OPRA2_S6 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_15 , OPRA2_S7 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_16 , OPRA2_00 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_16 , OPRA2_01 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_16 , OPRA2_02 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_16 , OPRA2_03 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_16 , OPRA2_04 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_16 , OPRA2_05 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_16 , OPRA2_06 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_16 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_16 , OPRA2_10 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_16 , OPRA2_11 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_16 , OPRA2_12 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_16 , OPRA2_13 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_16 , OPRA2_14 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_16 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_16 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_16 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_16 , OPRA2_20 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_16 , OPRA2_21 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_16 , OPRA2_22 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_16 , OPRA2_23 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_16 , OPRA2_24 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_16 , OPRA2_25 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_16 , OPRA2_26 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_16 , OPRA2_27 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_16 , OPRA2_30 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_16 , OPRA2_31 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_16 , OPRA2_32 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_16 , OPRA2_33 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_16 , OPRA2_34 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_16 , OPRA2_35 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_16 , OPRA2_36 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_16 , OPRA2_37 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_16 , OPRA2_40 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_16 , OPRA2_41 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_16 , OPRA2_42 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_16 , OPRA2_43 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_16 , OPRA2_44 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_16 , OPRA2_45 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_16 , OPRA2_46 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_16 , OPRA2_47 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_16 , OPRA2_50 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_16 , OPRA2_51 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_16 , OPRA2_52 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_16 , OPRA2_53 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_16 , OPRA2_54 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_16 , OPRA2_55 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_16 , OPRA2_56 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_16 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_16 , OPRA2_60 )
          , Set.fromList [ OPRA2_50, OPRA2_14, OPRA2_S1 ] )
        , ( ( OPRA2_16 , OPRA2_61 )
          , Set.fromList [ OPRA2_S7, OPRA2_51, OPRA2_15, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_16 , OPRA2_62 )
          , Set.fromList [ OPRA2_S7, OPRA2_52, OPRA2_16 ] )
        , ( ( OPRA2_16 , OPRA2_63 )
          , Set.fromList [ OPRA2_S7, OPRA2_S6, OPRA2_53, OPRA2_S5, OPRA2_17 ] )
        , ( ( OPRA2_16 , OPRA2_64 )
          , Set.fromList [ OPRA2_54, OPRA2_S5, OPRA2_10 ] )
        , ( ( OPRA2_16 , OPRA2_65 )
          , Set.fromList [ OPRA2_55, OPRA2_S5, OPRA2_S4, OPRA2_S3, OPRA2_11 ] )
        , ( ( OPRA2_16 , OPRA2_66 )
          , Set.fromList [ OPRA2_56, OPRA2_S3, OPRA2_12 ] )
        , ( ( OPRA2_16 , OPRA2_67 )
          , Set.fromList [ OPRA2_57, OPRA2_S3, OPRA2_S2, OPRA2_13, OPRA2_S1 ] )
        , ( ( OPRA2_16 , OPRA2_70 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_16 , OPRA2_71 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_16 , OPRA2_72 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_16 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_16 , OPRA2_74 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_16 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_16 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_16 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_16 , OPRA2_S0 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_16 , OPRA2_S1 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_16 , OPRA2_S2 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_16 , OPRA2_S3 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_16 , OPRA2_S4 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_16 , OPRA2_S5 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_16 , OPRA2_S6 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_16 , OPRA2_S7 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_17 , OPRA2_00 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_17 , OPRA2_01 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_17 , OPRA2_02 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_17 , OPRA2_03 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_17 , OPRA2_04 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_17 , OPRA2_05 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_17 , OPRA2_06 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_17 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_17 , OPRA2_10 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_51
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_17 , OPRA2_11 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_17 , OPRA2_12 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_63, OPRA2_53
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_17 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_17 , OPRA2_14 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_17 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_17 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_17 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_17 , OPRA2_20 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_17 , OPRA2_21 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_17 , OPRA2_22 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_17 , OPRA2_23 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_17 , OPRA2_24 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_17 , OPRA2_25 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_17 , OPRA2_26 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_17 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_17 , OPRA2_30 )
          , Set.fromList [ OPRA2_71, OPRA2_37, OPRA2_27, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_01 ] )
        , ( ( OPRA2_17 , OPRA2_31 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_37, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_17 , OPRA2_32 )
          , Set.fromList [ OPRA2_73, OPRA2_31, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03 ] )
        , ( ( OPRA2_17 , OPRA2_33 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_05
                         , OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_17 , OPRA2_34 )
          , Set.fromList [ OPRA2_75, OPRA2_33, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05 ] )
        , ( ( OPRA2_17 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_17 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_35, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07 ] )
        , ( ( OPRA2_17 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_17 , OPRA2_40 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_17 , OPRA2_41 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_17 , OPRA2_42 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_17 , OPRA2_43 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_17 , OPRA2_44 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_17 , OPRA2_45 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_17 , OPRA2_46 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_17 , OPRA2_47 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_17 , OPRA2_50 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_17 , OPRA2_51 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_17 , OPRA2_52 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_17 , OPRA2_53 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_17 , OPRA2_54 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_17 , OPRA2_55 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_17 , OPRA2_56 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_17 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_17 , OPRA2_60 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_17 , OPRA2_61 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_17 , OPRA2_62 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_17 , OPRA2_63 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_17 , OPRA2_64 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_17 , OPRA2_65 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_17 , OPRA2_66 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_17 , OPRA2_67 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_17 , OPRA2_70 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_S1, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_17 , OPRA2_71 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_57
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10
                         , OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01, OPRA2_S0 ] )
        , ( ( OPRA2_17 , OPRA2_72 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_63
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_37
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_11, OPRA2_10, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_17 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_S6
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_S5, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_37
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05
                         , OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_17 , OPRA2_74 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_43, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22, OPRA2_21
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_17 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45
                         , OPRA2_44, OPRA2_43, OPRA2_S4, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_17 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_17, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_17 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_S3, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_S2, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_S1, OPRA2_07, OPRA2_03, OPRA2_02
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_17 , OPRA2_S0 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_17 , OPRA2_S1 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_17 , OPRA2_S2 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_17 , OPRA2_S3 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_17 , OPRA2_S4 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_17 , OPRA2_S5 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_17 , OPRA2_S6 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_17 , OPRA2_S7 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_20 , OPRA2_00 )
          , Set.fromList [ OPRA2_60, OPRA2_24, OPRA2_S2 ] )
        , ( ( OPRA2_20 , OPRA2_01 )
          , Set.fromList [ OPRA2_61, OPRA2_25, OPRA2_S1 ] )
        , ( ( OPRA2_20 , OPRA2_02 )
          , Set.fromList [ OPRA2_62, OPRA2_26, OPRA2_S0 ] )
        , ( ( OPRA2_20 , OPRA2_03 )
          , Set.fromList [ OPRA2_S7, OPRA2_63, OPRA2_27 ] )
        , ( ( OPRA2_20 , OPRA2_04 )
          , Set.fromList [ OPRA2_64, OPRA2_S6, OPRA2_20 ] )
        , ( ( OPRA2_20 , OPRA2_05 )
          , Set.fromList [ OPRA2_65, OPRA2_S5, OPRA2_21 ] )
        , ( ( OPRA2_20 , OPRA2_06 )
          , Set.fromList [ OPRA2_66, OPRA2_S4, OPRA2_22 ] )
        , ( ( OPRA2_20 , OPRA2_07 )
          , Set.fromList [ OPRA2_67, OPRA2_S3, OPRA2_23 ] )
        , ( ( OPRA2_20 , OPRA2_10 )
          , Set.fromList [ OPRA2_71, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_20 , OPRA2_11 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_20 , OPRA2_12 )
          , Set.fromList [ OPRA2_73, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_20 , OPRA2_13 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_20 , OPRA2_14 )
          , Set.fromList [ OPRA2_75, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_20 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_20 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_20 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_20 , OPRA2_20 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_20 , OPRA2_21 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_20 , OPRA2_22 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_20 , OPRA2_23 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_20 , OPRA2_24 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_20 , OPRA2_25 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_20 , OPRA2_26 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_20 , OPRA2_27 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_20 , OPRA2_30 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_20 , OPRA2_31 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_20 , OPRA2_32 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_20 , OPRA2_33 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_20 , OPRA2_34 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_20 , OPRA2_35 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_20 , OPRA2_36 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_20 , OPRA2_37 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_20 , OPRA2_40 )
          , Set.fromList [ OPRA2_20 ] )
        , ( ( OPRA2_20 , OPRA2_41 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_20 , OPRA2_42 )
          , Set.fromList [ OPRA2_22 ] )
        , ( ( OPRA2_20 , OPRA2_43 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_20 , OPRA2_44 )
          , Set.fromList [ OPRA2_24 ] )
        , ( ( OPRA2_20 , OPRA2_45 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_20 , OPRA2_46 )
          , Set.fromList [ OPRA2_26 ] )
        , ( ( OPRA2_20 , OPRA2_47 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_20 , OPRA2_50 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_20 , OPRA2_51 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_20 , OPRA2_52 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_20 , OPRA2_53 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_20 , OPRA2_54 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_20 , OPRA2_55 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_20 , OPRA2_56 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_20 , OPRA2_57 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_20 , OPRA2_60 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_20 , OPRA2_61 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_20 , OPRA2_62 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_20 , OPRA2_63 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_20 , OPRA2_64 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_20 , OPRA2_65 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_20 , OPRA2_66 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_20 , OPRA2_67 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_20 , OPRA2_70 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_20 , OPRA2_71 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_20 , OPRA2_72 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_20 , OPRA2_73 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_20 , OPRA2_74 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_20 , OPRA2_75 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_20 , OPRA2_76 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_20 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_20 , OPRA2_S0 )
          , Set.fromList [ OPRA2_20 ] )
        , ( ( OPRA2_20 , OPRA2_S1 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_20 , OPRA2_S2 )
          , Set.fromList [ OPRA2_26 ] )
        , ( ( OPRA2_20 , OPRA2_S3 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_20 , OPRA2_S4 )
          , Set.fromList [ OPRA2_24 ] )
        , ( ( OPRA2_20 , OPRA2_S5 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_20 , OPRA2_S6 )
          , Set.fromList [ OPRA2_22 ] )
        , ( ( OPRA2_20 , OPRA2_S7 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_21 , OPRA2_00 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_21 , OPRA2_01 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_21 , OPRA2_02 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_21 , OPRA2_03 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_21 , OPRA2_04 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_21 , OPRA2_05 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_21 , OPRA2_06 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_21 , OPRA2_07 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_21 , OPRA2_10 )
          , Set.fromList [ OPRA2_71, OPRA2_60, OPRA2_57, OPRA2_47, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_24, OPRA2_S2, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_21 , OPRA2_11 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_S1, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_21 , OPRA2_12 )
          , Set.fromList [ OPRA2_73, OPRA2_62, OPRA2_51, OPRA2_41, OPRA2_37
                         , OPRA2_31, OPRA2_30, OPRA2_26, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_21 , OPRA2_13 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_63
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_21 , OPRA2_14 )
          , Set.fromList [ OPRA2_75, OPRA2_64, OPRA2_S6, OPRA2_53, OPRA2_43
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_21 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_21 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_66, OPRA2_55, OPRA2_45, OPRA2_S4
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_22, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_21 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46, OPRA2_45
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_S3, OPRA2_23, OPRA2_17, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_21 , OPRA2_20 )
          , Set.fromList [ OPRA2_71, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_21 , OPRA2_21 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_21 , OPRA2_22 )
          , Set.fromList [ OPRA2_73, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_21 , OPRA2_23 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_21 , OPRA2_24 )
          , Set.fromList [ OPRA2_75, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_21 , OPRA2_25 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_21 , OPRA2_26 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_21 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_21 , OPRA2_30 )
          , Set.fromList [ OPRA2_71, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_21 , OPRA2_31 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_21 , OPRA2_32 )
          , Set.fromList [ OPRA2_73, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_21 , OPRA2_33 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_21 , OPRA2_34 )
          , Set.fromList [ OPRA2_75, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_21 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_21 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_21 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_21 , OPRA2_40 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_21 , OPRA2_41 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_21 , OPRA2_42 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_21 , OPRA2_43 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_21 , OPRA2_44 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_21 , OPRA2_45 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_21 , OPRA2_46 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_21 , OPRA2_47 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_21 , OPRA2_50 )
          , Set.fromList [ OPRA2_37, OPRA2_20, OPRA2_11 ] )
        , ( ( OPRA2_21 , OPRA2_51 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_21, OPRA2_13
                         , OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_21 , OPRA2_52 )
          , Set.fromList [ OPRA2_31, OPRA2_22, OPRA2_13 ] )
        , ( ( OPRA2_21 , OPRA2_53 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_15
                         , OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_21 , OPRA2_54 )
          , Set.fromList [ OPRA2_33, OPRA2_24, OPRA2_15 ] )
        , ( ( OPRA2_21 , OPRA2_55 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_21 , OPRA2_56 )
          , Set.fromList [ OPRA2_35, OPRA2_26, OPRA2_17 ] )
        , ( ( OPRA2_21 , OPRA2_57 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_17
                         , OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_21 , OPRA2_60 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_21 , OPRA2_61 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_21 , OPRA2_62 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_21 , OPRA2_63 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_21 , OPRA2_64 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_21 , OPRA2_65 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_21 , OPRA2_66 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_21 , OPRA2_67 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_21 , OPRA2_70 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_21 , OPRA2_71 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_21 , OPRA2_72 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_21 , OPRA2_73 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_21 , OPRA2_74 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_21 , OPRA2_75 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_21 , OPRA2_76 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_21 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_21 , OPRA2_S0 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_21 , OPRA2_S1 )
          , Set.fromList [ OPRA2_27, OPRA2_21, OPRA2_20 ] )
        , ( ( OPRA2_21 , OPRA2_S2 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_21 , OPRA2_S3 )
          , Set.fromList [ OPRA2_27, OPRA2_26, OPRA2_25 ] )
        , ( ( OPRA2_21 , OPRA2_S4 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_21 , OPRA2_S5 )
          , Set.fromList [ OPRA2_25, OPRA2_24, OPRA2_23 ] )
        , ( ( OPRA2_21 , OPRA2_S6 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_21 , OPRA2_S7 )
          , Set.fromList [ OPRA2_23, OPRA2_22, OPRA2_21 ] )
        , ( ( OPRA2_22 , OPRA2_00 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_22 , OPRA2_01 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_22 , OPRA2_02 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_22 , OPRA2_03 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_22 , OPRA2_04 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_22 , OPRA2_05 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_22 , OPRA2_06 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_22 , OPRA2_07 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_22 , OPRA2_10 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_22 , OPRA2_11 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_22 , OPRA2_12 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_22 , OPRA2_13 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_22 , OPRA2_14 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_22 , OPRA2_15 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_22 , OPRA2_16 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_22 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_22 , OPRA2_20 )
          , Set.fromList [ OPRA2_60, OPRA2_24, OPRA2_S2 ] )
        , ( ( OPRA2_22 , OPRA2_21 )
          , Set.fromList [ OPRA2_61, OPRA2_25, OPRA2_S1 ] )
        , ( ( OPRA2_22 , OPRA2_22 )
          , Set.fromList [ OPRA2_62, OPRA2_26, OPRA2_S0 ] )
        , ( ( OPRA2_22 , OPRA2_23 )
          , Set.fromList [ OPRA2_S7, OPRA2_63, OPRA2_27 ] )
        , ( ( OPRA2_22 , OPRA2_24 )
          , Set.fromList [ OPRA2_64, OPRA2_S6, OPRA2_20 ] )
        , ( ( OPRA2_22 , OPRA2_25 )
          , Set.fromList [ OPRA2_65, OPRA2_S5, OPRA2_21 ] )
        , ( ( OPRA2_22 , OPRA2_26 )
          , Set.fromList [ OPRA2_66, OPRA2_S4, OPRA2_22 ] )
        , ( ( OPRA2_22 , OPRA2_27 )
          , Set.fromList [ OPRA2_67, OPRA2_S3, OPRA2_23 ] )
        , ( ( OPRA2_22 , OPRA2_30 )
          , Set.fromList [ OPRA2_71, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_22 , OPRA2_31 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_22 , OPRA2_32 )
          , Set.fromList [ OPRA2_73, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_22 , OPRA2_33 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_22 , OPRA2_34 )
          , Set.fromList [ OPRA2_75, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_22 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_22 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_22 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_22 , OPRA2_40 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_22 , OPRA2_41 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_22 , OPRA2_42 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_22 , OPRA2_43 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_22 , OPRA2_44 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_22 , OPRA2_45 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_22 , OPRA2_46 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_22 , OPRA2_47 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_22 , OPRA2_50 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_22 , OPRA2_51 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_22 , OPRA2_52 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_22 , OPRA2_53 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_22 , OPRA2_54 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_22 , OPRA2_55 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_22 , OPRA2_56 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_22 , OPRA2_57 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_22 , OPRA2_60 )
          , Set.fromList [ OPRA2_20 ] )
        , ( ( OPRA2_22 , OPRA2_61 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_22 , OPRA2_62 )
          , Set.fromList [ OPRA2_22 ] )
        , ( ( OPRA2_22 , OPRA2_63 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_22 , OPRA2_64 )
          , Set.fromList [ OPRA2_24 ] )
        , ( ( OPRA2_22 , OPRA2_65 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_22 , OPRA2_66 )
          , Set.fromList [ OPRA2_26 ] )
        , ( ( OPRA2_22 , OPRA2_67 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_22 , OPRA2_70 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_22 , OPRA2_71 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_22 , OPRA2_72 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_22 , OPRA2_73 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_22 , OPRA2_74 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_22 , OPRA2_75 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_22 , OPRA2_76 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_22 , OPRA2_77 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_22 , OPRA2_S0 )
          , Set.fromList [ OPRA2_22 ] )
        , ( ( OPRA2_22 , OPRA2_S1 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_22 , OPRA2_S2 )
          , Set.fromList [ OPRA2_20 ] )
        , ( ( OPRA2_22 , OPRA2_S3 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_22 , OPRA2_S4 )
          , Set.fromList [ OPRA2_26 ] )
        , ( ( OPRA2_22 , OPRA2_S5 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_22 , OPRA2_S6 )
          , Set.fromList [ OPRA2_24 ] )
        , ( ( OPRA2_22 , OPRA2_S7 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_23 , OPRA2_00 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_23 , OPRA2_01 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_23 , OPRA2_02 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_23 , OPRA2_03 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_23 , OPRA2_04 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_23 , OPRA2_05 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_23 , OPRA2_06 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_23 , OPRA2_07 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_23 , OPRA2_10 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_23 , OPRA2_11 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_23 , OPRA2_12 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_23 , OPRA2_13 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_23 , OPRA2_14 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_23 , OPRA2_15 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_23 , OPRA2_16 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_23 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_23 , OPRA2_20 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_23 , OPRA2_21 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_23 , OPRA2_22 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_23 , OPRA2_23 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_23 , OPRA2_24 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_23 , OPRA2_25 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_23 , OPRA2_26 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_23 , OPRA2_27 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_23 , OPRA2_30 )
          , Set.fromList [ OPRA2_71, OPRA2_60, OPRA2_57, OPRA2_47, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_24, OPRA2_S2, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_23 , OPRA2_31 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_S1, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_23 , OPRA2_32 )
          , Set.fromList [ OPRA2_73, OPRA2_62, OPRA2_51, OPRA2_41, OPRA2_37
                         , OPRA2_31, OPRA2_30, OPRA2_26, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_23 , OPRA2_33 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_63
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_23 , OPRA2_34 )
          , Set.fromList [ OPRA2_75, OPRA2_64, OPRA2_S6, OPRA2_53, OPRA2_43
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_23 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_23 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_66, OPRA2_55, OPRA2_45, OPRA2_S4
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_22, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_23 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46, OPRA2_45
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_S3, OPRA2_23, OPRA2_17, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_23 , OPRA2_40 )
          , Set.fromList [ OPRA2_71, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_23 , OPRA2_41 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_23 , OPRA2_42 )
          , Set.fromList [ OPRA2_73, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_23 , OPRA2_43 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_23 , OPRA2_44 )
          , Set.fromList [ OPRA2_75, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_23 , OPRA2_45 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_23 , OPRA2_46 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_23 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_23 , OPRA2_50 )
          , Set.fromList [ OPRA2_71, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_23 , OPRA2_51 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_23 , OPRA2_52 )
          , Set.fromList [ OPRA2_73, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_23 , OPRA2_53 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_23 , OPRA2_54 )
          , Set.fromList [ OPRA2_75, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_23 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_23 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_23 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_23 , OPRA2_60 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_23 , OPRA2_61 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_23 , OPRA2_62 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_23 , OPRA2_63 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_23 , OPRA2_64 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_23 , OPRA2_65 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_23 , OPRA2_66 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_23 , OPRA2_67 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_23 , OPRA2_70 )
          , Set.fromList [ OPRA2_37, OPRA2_20, OPRA2_11 ] )
        , ( ( OPRA2_23 , OPRA2_71 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_21, OPRA2_13
                         , OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_23 , OPRA2_72 )
          , Set.fromList [ OPRA2_31, OPRA2_22, OPRA2_13 ] )
        , ( ( OPRA2_23 , OPRA2_73 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_15
                         , OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_23 , OPRA2_74 )
          , Set.fromList [ OPRA2_33, OPRA2_24, OPRA2_15 ] )
        , ( ( OPRA2_23 , OPRA2_75 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_23 , OPRA2_76 )
          , Set.fromList [ OPRA2_35, OPRA2_26, OPRA2_17 ] )
        , ( ( OPRA2_23 , OPRA2_77 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_17
                         , OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_23 , OPRA2_S0 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_23 , OPRA2_S1 )
          , Set.fromList [ OPRA2_23, OPRA2_22, OPRA2_21 ] )
        , ( ( OPRA2_23 , OPRA2_S2 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_23 , OPRA2_S3 )
          , Set.fromList [ OPRA2_27, OPRA2_21, OPRA2_20 ] )
        , ( ( OPRA2_23 , OPRA2_S4 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_23 , OPRA2_S5 )
          , Set.fromList [ OPRA2_27, OPRA2_26, OPRA2_25 ] )
        , ( ( OPRA2_23 , OPRA2_S6 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_23 , OPRA2_S7 )
          , Set.fromList [ OPRA2_25, OPRA2_24, OPRA2_23 ] )
        , ( ( OPRA2_24 , OPRA2_00 )
          , Set.fromList [ OPRA2_20 ] )
        , ( ( OPRA2_24 , OPRA2_01 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_24 , OPRA2_02 )
          , Set.fromList [ OPRA2_22 ] )
        , ( ( OPRA2_24 , OPRA2_03 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_24 , OPRA2_04 )
          , Set.fromList [ OPRA2_24 ] )
        , ( ( OPRA2_24 , OPRA2_05 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_24 , OPRA2_06 )
          , Set.fromList [ OPRA2_26 ] )
        , ( ( OPRA2_24 , OPRA2_07 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_24 , OPRA2_10 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_24 , OPRA2_11 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_24 , OPRA2_12 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_24 , OPRA2_13 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_24 , OPRA2_14 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_24 , OPRA2_15 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_24 , OPRA2_16 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_24 , OPRA2_17 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_24 , OPRA2_20 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_24 , OPRA2_21 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_24 , OPRA2_22 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_24 , OPRA2_23 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_24 , OPRA2_24 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_24 , OPRA2_25 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_24 , OPRA2_26 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_24 , OPRA2_27 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_24 , OPRA2_30 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_24 , OPRA2_31 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_24 , OPRA2_32 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_24 , OPRA2_33 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_24 , OPRA2_34 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_24 , OPRA2_35 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_24 , OPRA2_36 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_24 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_24 , OPRA2_40 )
          , Set.fromList [ OPRA2_60, OPRA2_24, OPRA2_S2 ] )
        , ( ( OPRA2_24 , OPRA2_41 )
          , Set.fromList [ OPRA2_61, OPRA2_25, OPRA2_S1 ] )
        , ( ( OPRA2_24 , OPRA2_42 )
          , Set.fromList [ OPRA2_62, OPRA2_26, OPRA2_S0 ] )
        , ( ( OPRA2_24 , OPRA2_43 )
          , Set.fromList [ OPRA2_S7, OPRA2_63, OPRA2_27 ] )
        , ( ( OPRA2_24 , OPRA2_44 )
          , Set.fromList [ OPRA2_64, OPRA2_S6, OPRA2_20 ] )
        , ( ( OPRA2_24 , OPRA2_45 )
          , Set.fromList [ OPRA2_65, OPRA2_S5, OPRA2_21 ] )
        , ( ( OPRA2_24 , OPRA2_46 )
          , Set.fromList [ OPRA2_66, OPRA2_S4, OPRA2_22 ] )
        , ( ( OPRA2_24 , OPRA2_47 )
          , Set.fromList [ OPRA2_67, OPRA2_S3, OPRA2_23 ] )
        , ( ( OPRA2_24 , OPRA2_50 )
          , Set.fromList [ OPRA2_71, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_24 , OPRA2_51 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_24 , OPRA2_52 )
          , Set.fromList [ OPRA2_73, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_24 , OPRA2_53 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_24 , OPRA2_54 )
          , Set.fromList [ OPRA2_75, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_24 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_24 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_24 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_24 , OPRA2_60 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_24 , OPRA2_61 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_24 , OPRA2_62 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_24 , OPRA2_63 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_24 , OPRA2_64 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_24 , OPRA2_65 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_24 , OPRA2_66 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_24 , OPRA2_67 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_24 , OPRA2_70 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_24 , OPRA2_71 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_24 , OPRA2_72 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_24 , OPRA2_73 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_24 , OPRA2_74 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_24 , OPRA2_75 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_24 , OPRA2_76 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_24 , OPRA2_77 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_24 , OPRA2_S0 )
          , Set.fromList [ OPRA2_24 ] )
        , ( ( OPRA2_24 , OPRA2_S1 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_24 , OPRA2_S2 )
          , Set.fromList [ OPRA2_22 ] )
        , ( ( OPRA2_24 , OPRA2_S3 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_24 , OPRA2_S4 )
          , Set.fromList [ OPRA2_20 ] )
        , ( ( OPRA2_24 , OPRA2_S5 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_24 , OPRA2_S6 )
          , Set.fromList [ OPRA2_26 ] )
        , ( ( OPRA2_24 , OPRA2_S7 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_25 , OPRA2_00 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_25 , OPRA2_01 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_25 , OPRA2_02 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_25 , OPRA2_03 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_25 , OPRA2_04 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_25 , OPRA2_05 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_25 , OPRA2_06 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_25 , OPRA2_07 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_25 , OPRA2_10 )
          , Set.fromList [ OPRA2_37, OPRA2_20, OPRA2_11 ] )
        , ( ( OPRA2_25 , OPRA2_11 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_21, OPRA2_13
                         , OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_25 , OPRA2_12 )
          , Set.fromList [ OPRA2_31, OPRA2_22, OPRA2_13 ] )
        , ( ( OPRA2_25 , OPRA2_13 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_15
                         , OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_25 , OPRA2_14 )
          , Set.fromList [ OPRA2_33, OPRA2_24, OPRA2_15 ] )
        , ( ( OPRA2_25 , OPRA2_15 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_25 , OPRA2_16 )
          , Set.fromList [ OPRA2_35, OPRA2_26, OPRA2_17 ] )
        , ( ( OPRA2_25 , OPRA2_17 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_17
                         , OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_25 , OPRA2_20 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_25 , OPRA2_21 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_25 , OPRA2_22 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_25 , OPRA2_23 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_25 , OPRA2_24 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_25 , OPRA2_25 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_25 , OPRA2_26 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_25 , OPRA2_27 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_25 , OPRA2_30 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_25 , OPRA2_31 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_25 , OPRA2_32 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_25 , OPRA2_33 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_25 , OPRA2_34 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_25 , OPRA2_35 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_25 , OPRA2_36 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_25 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_25 , OPRA2_40 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_25 , OPRA2_41 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_25 , OPRA2_42 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_25 , OPRA2_43 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_25 , OPRA2_44 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_25 , OPRA2_45 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_25 , OPRA2_46 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_25 , OPRA2_47 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_25 , OPRA2_50 )
          , Set.fromList [ OPRA2_71, OPRA2_60, OPRA2_57, OPRA2_47, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_24, OPRA2_S2, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_25 , OPRA2_51 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_S1, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_25 , OPRA2_52 )
          , Set.fromList [ OPRA2_73, OPRA2_62, OPRA2_51, OPRA2_41, OPRA2_37
                         , OPRA2_31, OPRA2_30, OPRA2_26, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_25 , OPRA2_53 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_63
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_25 , OPRA2_54 )
          , Set.fromList [ OPRA2_75, OPRA2_64, OPRA2_S6, OPRA2_53, OPRA2_43
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_25 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_25 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_66, OPRA2_55, OPRA2_45, OPRA2_S4
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_22, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_25 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46, OPRA2_45
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_S3, OPRA2_23, OPRA2_17, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_25 , OPRA2_60 )
          , Set.fromList [ OPRA2_71, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_25 , OPRA2_61 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_25 , OPRA2_62 )
          , Set.fromList [ OPRA2_73, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_25 , OPRA2_63 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_25 , OPRA2_64 )
          , Set.fromList [ OPRA2_75, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_25 , OPRA2_65 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_25 , OPRA2_66 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_25 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_25 , OPRA2_70 )
          , Set.fromList [ OPRA2_71, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_25 , OPRA2_71 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_25 , OPRA2_72 )
          , Set.fromList [ OPRA2_73, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_25 , OPRA2_73 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_25 , OPRA2_74 )
          , Set.fromList [ OPRA2_75, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_25 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_25 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_25 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_25 , OPRA2_S0 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_25 , OPRA2_S1 )
          , Set.fromList [ OPRA2_25, OPRA2_24, OPRA2_23 ] )
        , ( ( OPRA2_25 , OPRA2_S2 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_25 , OPRA2_S3 )
          , Set.fromList [ OPRA2_23, OPRA2_22, OPRA2_21 ] )
        , ( ( OPRA2_25 , OPRA2_S4 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_25 , OPRA2_S5 )
          , Set.fromList [ OPRA2_27, OPRA2_21, OPRA2_20 ] )
        , ( ( OPRA2_25 , OPRA2_S6 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_25 , OPRA2_S7 )
          , Set.fromList [ OPRA2_27, OPRA2_26, OPRA2_25 ] )
        , ( ( OPRA2_26 , OPRA2_00 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_26 , OPRA2_01 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_26 , OPRA2_02 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_26 , OPRA2_03 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_26 , OPRA2_04 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_26 , OPRA2_05 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_26 , OPRA2_06 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_26 , OPRA2_07 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_26 , OPRA2_10 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_26 , OPRA2_11 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_26 , OPRA2_12 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_26 , OPRA2_13 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_26 , OPRA2_14 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_26 , OPRA2_15 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_26 , OPRA2_16 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_26 , OPRA2_17 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_26 , OPRA2_20 )
          , Set.fromList [ OPRA2_20 ] )
        , ( ( OPRA2_26 , OPRA2_21 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_26 , OPRA2_22 )
          , Set.fromList [ OPRA2_22 ] )
        , ( ( OPRA2_26 , OPRA2_23 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_26 , OPRA2_24 )
          , Set.fromList [ OPRA2_24 ] )
        , ( ( OPRA2_26 , OPRA2_25 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_26 , OPRA2_26 )
          , Set.fromList [ OPRA2_26 ] )
        , ( ( OPRA2_26 , OPRA2_27 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_26 , OPRA2_30 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_26 , OPRA2_31 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_26 , OPRA2_32 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_26 , OPRA2_33 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_26 , OPRA2_34 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_26 , OPRA2_35 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_26 , OPRA2_36 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_26 , OPRA2_37 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_26 , OPRA2_40 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_26 , OPRA2_41 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_26 , OPRA2_42 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_26 , OPRA2_43 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_26 , OPRA2_44 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_26 , OPRA2_45 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_26 , OPRA2_46 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_26 , OPRA2_47 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_26 , OPRA2_50 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_26 , OPRA2_51 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_26 , OPRA2_52 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_26 , OPRA2_53 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_26 , OPRA2_54 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_26 , OPRA2_55 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_26 , OPRA2_56 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_26 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_26 , OPRA2_60 )
          , Set.fromList [ OPRA2_60, OPRA2_24, OPRA2_S2 ] )
        , ( ( OPRA2_26 , OPRA2_61 )
          , Set.fromList [ OPRA2_61, OPRA2_25, OPRA2_S1 ] )
        , ( ( OPRA2_26 , OPRA2_62 )
          , Set.fromList [ OPRA2_62, OPRA2_26, OPRA2_S0 ] )
        , ( ( OPRA2_26 , OPRA2_63 )
          , Set.fromList [ OPRA2_S7, OPRA2_63, OPRA2_27 ] )
        , ( ( OPRA2_26 , OPRA2_64 )
          , Set.fromList [ OPRA2_64, OPRA2_S6, OPRA2_20 ] )
        , ( ( OPRA2_26 , OPRA2_65 )
          , Set.fromList [ OPRA2_65, OPRA2_S5, OPRA2_21 ] )
        , ( ( OPRA2_26 , OPRA2_66 )
          , Set.fromList [ OPRA2_66, OPRA2_S4, OPRA2_22 ] )
        , ( ( OPRA2_26 , OPRA2_67 )
          , Set.fromList [ OPRA2_67, OPRA2_S3, OPRA2_23 ] )
        , ( ( OPRA2_26 , OPRA2_70 )
          , Set.fromList [ OPRA2_71, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_26 , OPRA2_71 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_26 , OPRA2_72 )
          , Set.fromList [ OPRA2_73, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_26 , OPRA2_73 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_26 , OPRA2_74 )
          , Set.fromList [ OPRA2_75, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_26 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_26 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_26 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_26 , OPRA2_S0 )
          , Set.fromList [ OPRA2_26 ] )
        , ( ( OPRA2_26 , OPRA2_S1 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_26 , OPRA2_S2 )
          , Set.fromList [ OPRA2_24 ] )
        , ( ( OPRA2_26 , OPRA2_S3 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_26 , OPRA2_S4 )
          , Set.fromList [ OPRA2_22 ] )
        , ( ( OPRA2_26 , OPRA2_S5 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_26 , OPRA2_S6 )
          , Set.fromList [ OPRA2_20 ] )
        , ( ( OPRA2_26 , OPRA2_S7 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_27 , OPRA2_00 )
          , Set.fromList [ OPRA2_71, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_27 , OPRA2_01 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_27 , OPRA2_02 )
          , Set.fromList [ OPRA2_73, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_27 , OPRA2_03 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_27 , OPRA2_04 )
          , Set.fromList [ OPRA2_75, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_27 , OPRA2_05 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_27 , OPRA2_06 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_27 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_27 , OPRA2_10 )
          , Set.fromList [ OPRA2_71, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_27 , OPRA2_11 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_27 , OPRA2_12 )
          , Set.fromList [ OPRA2_73, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_27 , OPRA2_13 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_27 , OPRA2_14 )
          , Set.fromList [ OPRA2_75, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_27 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_27 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_27 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_27 , OPRA2_20 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_27 , OPRA2_21 )
          , Set.fromList [ OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_27 , OPRA2_22 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_27 , OPRA2_23 )
          , Set.fromList [ OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_27 , OPRA2_24 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_27 , OPRA2_25 )
          , Set.fromList [ OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_27 , OPRA2_26 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_27 , OPRA2_27 )
          , Set.fromList [ OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_27 , OPRA2_30 )
          , Set.fromList [ OPRA2_37, OPRA2_20, OPRA2_11 ] )
        , ( ( OPRA2_27 , OPRA2_31 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_21, OPRA2_13
                         , OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_27 , OPRA2_32 )
          , Set.fromList [ OPRA2_31, OPRA2_22, OPRA2_13 ] )
        , ( ( OPRA2_27 , OPRA2_33 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_15
                         , OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_27 , OPRA2_34 )
          , Set.fromList [ OPRA2_33, OPRA2_24, OPRA2_15 ] )
        , ( ( OPRA2_27 , OPRA2_35 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_27 , OPRA2_36 )
          , Set.fromList [ OPRA2_35, OPRA2_26, OPRA2_17 ] )
        , ( ( OPRA2_27 , OPRA2_37 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_17
                         , OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_27 , OPRA2_40 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_27 , OPRA2_41 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_27 , OPRA2_42 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_27 , OPRA2_43 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_27 , OPRA2_44 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_27 , OPRA2_45 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_27 , OPRA2_46 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_27 , OPRA2_47 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_27 , OPRA2_50 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_27 , OPRA2_51 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_27 , OPRA2_52 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_27 , OPRA2_53 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_27 , OPRA2_54 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_27 , OPRA2_55 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_27 , OPRA2_56 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_27 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_27 , OPRA2_60 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_27 , OPRA2_61 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_27 , OPRA2_62 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_27 , OPRA2_63 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_27 , OPRA2_64 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_27 , OPRA2_65 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_27 , OPRA2_66 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_27 , OPRA2_67 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_27 , OPRA2_70 )
          , Set.fromList [ OPRA2_71, OPRA2_60, OPRA2_57, OPRA2_47, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_24, OPRA2_S2, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_27 , OPRA2_71 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_25, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_S1, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_27 , OPRA2_72 )
          , Set.fromList [ OPRA2_73, OPRA2_62, OPRA2_51, OPRA2_41, OPRA2_37
                         , OPRA2_31, OPRA2_30, OPRA2_26, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_27 , OPRA2_73 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_63
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_27 , OPRA2_74 )
          , Set.fromList [ OPRA2_75, OPRA2_64, OPRA2_S6, OPRA2_53, OPRA2_43
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_27 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_21, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_27 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_66, OPRA2_55, OPRA2_45, OPRA2_S4
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_22, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_27 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46, OPRA2_45
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_S3, OPRA2_23, OPRA2_17, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_27 , OPRA2_S0 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_27 , OPRA2_S1 )
          , Set.fromList [ OPRA2_27, OPRA2_26, OPRA2_25 ] )
        , ( ( OPRA2_27 , OPRA2_S2 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_27 , OPRA2_S3 )
          , Set.fromList [ OPRA2_25, OPRA2_24, OPRA2_23 ] )
        , ( ( OPRA2_27 , OPRA2_S4 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_27 , OPRA2_S5 )
          , Set.fromList [ OPRA2_23, OPRA2_22, OPRA2_21 ] )
        , ( ( OPRA2_27 , OPRA2_S6 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_27 , OPRA2_S7 )
          , Set.fromList [ OPRA2_27, OPRA2_21, OPRA2_20 ] )
        , ( ( OPRA2_30 , OPRA2_00 )
          , Set.fromList [ OPRA2_70, OPRA2_34, OPRA2_S3 ] )
        , ( ( OPRA2_30 , OPRA2_01 )
          , Set.fromList [ OPRA2_71, OPRA2_35, OPRA2_S3, OPRA2_S2, OPRA2_S1 ] )
        , ( ( OPRA2_30 , OPRA2_02 )
          , Set.fromList [ OPRA2_72, OPRA2_36, OPRA2_S1 ] )
        , ( ( OPRA2_30 , OPRA2_03 )
          , Set.fromList [ OPRA2_73, OPRA2_S7, OPRA2_37, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_30 , OPRA2_04 )
          , Set.fromList [ OPRA2_74, OPRA2_S7, OPRA2_30 ] )
        , ( ( OPRA2_30 , OPRA2_05 )
          , Set.fromList [ OPRA2_75, OPRA2_S7, OPRA2_S6, OPRA2_S5, OPRA2_31 ] )
        , ( ( OPRA2_30 , OPRA2_06 )
          , Set.fromList [ OPRA2_76, OPRA2_S5, OPRA2_32 ] )
        , ( ( OPRA2_30 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_S5, OPRA2_S4, OPRA2_33, OPRA2_S3 ] )
        , ( ( OPRA2_30 , OPRA2_10 )
          , Set.fromList [ OPRA2_71, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_30 , OPRA2_11 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_30 , OPRA2_12 )
          , Set.fromList [ OPRA2_73, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_30 , OPRA2_13 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_30 , OPRA2_14 )
          , Set.fromList [ OPRA2_75, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_30 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_30 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10
                         , OPRA2_07 ] )
        , ( ( OPRA2_30 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_30 , OPRA2_20 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_30 , OPRA2_21 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_30 , OPRA2_22 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_30 , OPRA2_23 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_30 , OPRA2_24 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_30 , OPRA2_25 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_30 , OPRA2_26 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_30 , OPRA2_27 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_30 , OPRA2_30 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_30 , OPRA2_31 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_30 , OPRA2_32 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_30 , OPRA2_33 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_30 , OPRA2_34 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_30 , OPRA2_35 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_30 , OPRA2_36 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_30 , OPRA2_37 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_30 , OPRA2_40 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_30 , OPRA2_41 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_30 , OPRA2_42 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_30 , OPRA2_43 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_30 , OPRA2_44 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_30 , OPRA2_45 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_30 , OPRA2_46 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_30 , OPRA2_47 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_30 , OPRA2_50 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_30 , OPRA2_51 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_30 , OPRA2_52 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_30 , OPRA2_53 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_30 , OPRA2_54 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_30 , OPRA2_55 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_30 , OPRA2_56 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_30 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_30 , OPRA2_60 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_30 , OPRA2_61 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_30 , OPRA2_62 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_30 , OPRA2_63 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_30 , OPRA2_64 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_30 , OPRA2_65 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_30 , OPRA2_66 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_30 , OPRA2_67 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_30 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_30 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_30 , OPRA2_72 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_30 , OPRA2_73 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_30 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_30 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_30 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_30 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_30 , OPRA2_S0 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_30 , OPRA2_S1 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_30 , OPRA2_S2 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_30 , OPRA2_S3 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_30 , OPRA2_S4 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_30 , OPRA2_S5 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_30 , OPRA2_S6 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_30 , OPRA2_S7 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_31 , OPRA2_00 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_31 , OPRA2_01 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_31 , OPRA2_02 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_31 , OPRA2_03 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_31 , OPRA2_04 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_31 , OPRA2_05 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_31 , OPRA2_06 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_31 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_31 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46, OPRA2_45
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_31 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_S2, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_S1, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_31 , OPRA2_12 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_31, OPRA2_30, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_31 , OPRA2_13 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_57
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04
                         , OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_31 , OPRA2_14 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_63
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_31 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_S6
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_S5, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_31 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_31 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_47
                         , OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_S4
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_S3, OPRA2_27
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_31 , OPRA2_20 )
          , Set.fromList [ OPRA2_71, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_31 , OPRA2_21 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_31 , OPRA2_22 )
          , Set.fromList [ OPRA2_73, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_31 , OPRA2_23 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_31 , OPRA2_24 )
          , Set.fromList [ OPRA2_75, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_31 , OPRA2_25 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_31 , OPRA2_26 )
          , Set.fromList [ OPRA2_77, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10
                         , OPRA2_07 ] )
        , ( ( OPRA2_31 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_31 , OPRA2_30 )
          , Set.fromList [ OPRA2_71, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_31 , OPRA2_31 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_31 , OPRA2_32 )
          , Set.fromList [ OPRA2_73, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_31 , OPRA2_33 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_31 , OPRA2_34 )
          , Set.fromList [ OPRA2_75, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_31 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_31 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10
                         , OPRA2_07 ] )
        , ( ( OPRA2_31 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_31 , OPRA2_40 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_31 , OPRA2_41 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_31 , OPRA2_42 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_31 , OPRA2_43 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_31 , OPRA2_44 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_31 , OPRA2_45 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_31 , OPRA2_46 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_31 , OPRA2_47 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_31 , OPRA2_50 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_31 , OPRA2_51 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13
                         , OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_31 , OPRA2_52 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_31 , OPRA2_53 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15
                         , OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_31 , OPRA2_54 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_31 , OPRA2_55 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_31 , OPRA2_56 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_31 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_31 , OPRA2_60 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_31 , OPRA2_61 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_31 , OPRA2_62 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_31 , OPRA2_63 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_31 , OPRA2_64 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_31 , OPRA2_65 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_31 , OPRA2_66 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_31 , OPRA2_67 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_31 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_31 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_31 , OPRA2_72 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_31 , OPRA2_73 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_31 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_31 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_31 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_31 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_31 , OPRA2_S0 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_31 , OPRA2_S1 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_31 , OPRA2_S2 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_31 , OPRA2_S3 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_31 , OPRA2_S4 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_31 , OPRA2_S5 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_31 , OPRA2_S6 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_31 , OPRA2_S7 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_32 , OPRA2_00 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_32 , OPRA2_01 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_32 , OPRA2_02 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_32 , OPRA2_03 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_32 , OPRA2_04 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_32 , OPRA2_05 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_32 , OPRA2_06 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_32 , OPRA2_07 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_32 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_32 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_32 , OPRA2_12 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_32 , OPRA2_13 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_32 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_32 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_32 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_32 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_32 , OPRA2_20 )
          , Set.fromList [ OPRA2_70, OPRA2_34, OPRA2_S3 ] )
        , ( ( OPRA2_32 , OPRA2_21 )
          , Set.fromList [ OPRA2_71, OPRA2_35, OPRA2_S3, OPRA2_S2, OPRA2_S1 ] )
        , ( ( OPRA2_32 , OPRA2_22 )
          , Set.fromList [ OPRA2_72, OPRA2_36, OPRA2_S1 ] )
        , ( ( OPRA2_32 , OPRA2_23 )
          , Set.fromList [ OPRA2_73, OPRA2_S7, OPRA2_37, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_32 , OPRA2_24 )
          , Set.fromList [ OPRA2_74, OPRA2_S7, OPRA2_30 ] )
        , ( ( OPRA2_32 , OPRA2_25 )
          , Set.fromList [ OPRA2_75, OPRA2_S7, OPRA2_S6, OPRA2_S5, OPRA2_31 ] )
        , ( ( OPRA2_32 , OPRA2_26 )
          , Set.fromList [ OPRA2_76, OPRA2_S5, OPRA2_32 ] )
        , ( ( OPRA2_32 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_S5, OPRA2_S4, OPRA2_33, OPRA2_S3 ] )
        , ( ( OPRA2_32 , OPRA2_30 )
          , Set.fromList [ OPRA2_71, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_32 , OPRA2_31 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_32 , OPRA2_32 )
          , Set.fromList [ OPRA2_73, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_32 , OPRA2_33 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_32 , OPRA2_34 )
          , Set.fromList [ OPRA2_75, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_32 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_32 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10
                         , OPRA2_07 ] )
        , ( ( OPRA2_32 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_32 , OPRA2_40 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_32 , OPRA2_41 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_32 , OPRA2_42 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_32 , OPRA2_43 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_32 , OPRA2_44 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_32 , OPRA2_45 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_32 , OPRA2_46 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_32 , OPRA2_47 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_32 , OPRA2_50 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_32 , OPRA2_51 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_32 , OPRA2_52 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_32 , OPRA2_53 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_32 , OPRA2_54 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_32 , OPRA2_55 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_32 , OPRA2_56 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_32 , OPRA2_57 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_32 , OPRA2_60 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_32 , OPRA2_61 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_32 , OPRA2_62 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_32 , OPRA2_63 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_32 , OPRA2_64 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_32 , OPRA2_65 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_32 , OPRA2_66 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_32 , OPRA2_67 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_32 , OPRA2_70 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_32 , OPRA2_71 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_32 , OPRA2_72 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_32 , OPRA2_73 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_32 , OPRA2_74 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_32 , OPRA2_75 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_32 , OPRA2_76 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_32 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_32 , OPRA2_S0 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_32 , OPRA2_S1 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_32 , OPRA2_S2 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_32 , OPRA2_S3 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_32 , OPRA2_S4 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_32 , OPRA2_S5 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_32 , OPRA2_S6 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_32 , OPRA2_S7 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_33 , OPRA2_00 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_33 , OPRA2_01 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_33 , OPRA2_02 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_33 , OPRA2_03 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_33 , OPRA2_04 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_33 , OPRA2_05 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_33 , OPRA2_06 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_33 , OPRA2_07 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_33 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_33 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_33 , OPRA2_12 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_33 , OPRA2_13 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_33 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_33 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_33 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_33 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_33 , OPRA2_20 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_33 , OPRA2_21 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_33 , OPRA2_22 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_33 , OPRA2_23 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_33 , OPRA2_24 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_33 , OPRA2_25 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_33 , OPRA2_26 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_33 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_33 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46, OPRA2_45
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_33 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_S2, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_S1, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_33 , OPRA2_32 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_31, OPRA2_30, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_33 , OPRA2_33 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_57
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04
                         , OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_33 , OPRA2_34 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_63
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_33 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_S6
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_S5, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_33 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_33 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_47
                         , OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_S4
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_S3, OPRA2_27
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_33 , OPRA2_40 )
          , Set.fromList [ OPRA2_71, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_33 , OPRA2_41 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_33 , OPRA2_42 )
          , Set.fromList [ OPRA2_73, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_33 , OPRA2_43 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_33 , OPRA2_44 )
          , Set.fromList [ OPRA2_75, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_33 , OPRA2_45 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_33 , OPRA2_46 )
          , Set.fromList [ OPRA2_77, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10
                         , OPRA2_07 ] )
        , ( ( OPRA2_33 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_33 , OPRA2_50 )
          , Set.fromList [ OPRA2_71, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_33 , OPRA2_51 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_33 , OPRA2_52 )
          , Set.fromList [ OPRA2_73, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_33 , OPRA2_53 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_33 , OPRA2_54 )
          , Set.fromList [ OPRA2_75, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_33 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_33 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10
                         , OPRA2_07 ] )
        , ( ( OPRA2_33 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_33 , OPRA2_60 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_33 , OPRA2_61 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_33 , OPRA2_62 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_33 , OPRA2_63 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_33 , OPRA2_64 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_33 , OPRA2_65 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_33 , OPRA2_66 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_33 , OPRA2_67 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_33 , OPRA2_70 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_33 , OPRA2_71 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13
                         , OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_33 , OPRA2_72 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_33 , OPRA2_73 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15
                         , OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_33 , OPRA2_74 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_33 , OPRA2_75 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_33 , OPRA2_76 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_33 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_33 , OPRA2_S0 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_33 , OPRA2_S1 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_33 , OPRA2_S2 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_33 , OPRA2_S3 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_33 , OPRA2_S4 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_33 , OPRA2_S5 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_33 , OPRA2_S6 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_33 , OPRA2_S7 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_34 , OPRA2_00 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_34 , OPRA2_01 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_34 , OPRA2_02 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_34 , OPRA2_03 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_34 , OPRA2_04 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_34 , OPRA2_05 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_34 , OPRA2_06 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_34 , OPRA2_07 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_34 , OPRA2_10 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_34 , OPRA2_11 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_34 , OPRA2_12 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_34 , OPRA2_13 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_34 , OPRA2_14 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_34 , OPRA2_15 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_34 , OPRA2_16 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_34 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_34 , OPRA2_20 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_34 , OPRA2_21 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_34 , OPRA2_22 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_34 , OPRA2_23 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_34 , OPRA2_24 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_34 , OPRA2_25 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_34 , OPRA2_26 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_34 , OPRA2_27 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_34 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_34 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_34 , OPRA2_32 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_34 , OPRA2_33 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_34 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_34 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_34 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_34 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_34 , OPRA2_40 )
          , Set.fromList [ OPRA2_70, OPRA2_34, OPRA2_S3 ] )
        , ( ( OPRA2_34 , OPRA2_41 )
          , Set.fromList [ OPRA2_71, OPRA2_35, OPRA2_S3, OPRA2_S2, OPRA2_S1 ] )
        , ( ( OPRA2_34 , OPRA2_42 )
          , Set.fromList [ OPRA2_72, OPRA2_36, OPRA2_S1 ] )
        , ( ( OPRA2_34 , OPRA2_43 )
          , Set.fromList [ OPRA2_73, OPRA2_S7, OPRA2_37, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_34 , OPRA2_44 )
          , Set.fromList [ OPRA2_74, OPRA2_S7, OPRA2_30 ] )
        , ( ( OPRA2_34 , OPRA2_45 )
          , Set.fromList [ OPRA2_75, OPRA2_S7, OPRA2_S6, OPRA2_S5, OPRA2_31 ] )
        , ( ( OPRA2_34 , OPRA2_46 )
          , Set.fromList [ OPRA2_76, OPRA2_S5, OPRA2_32 ] )
        , ( ( OPRA2_34 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_S5, OPRA2_S4, OPRA2_33, OPRA2_S3 ] )
        , ( ( OPRA2_34 , OPRA2_50 )
          , Set.fromList [ OPRA2_71, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_34 , OPRA2_51 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_34 , OPRA2_52 )
          , Set.fromList [ OPRA2_73, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_34 , OPRA2_53 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_34 , OPRA2_54 )
          , Set.fromList [ OPRA2_75, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_34 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_34 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10
                         , OPRA2_07 ] )
        , ( ( OPRA2_34 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_34 , OPRA2_60 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_34 , OPRA2_61 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_34 , OPRA2_62 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_34 , OPRA2_63 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_34 , OPRA2_64 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_34 , OPRA2_65 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_34 , OPRA2_66 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_34 , OPRA2_67 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_34 , OPRA2_70 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_34 , OPRA2_71 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_34 , OPRA2_72 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_34 , OPRA2_73 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_34 , OPRA2_74 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_34 , OPRA2_75 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_34 , OPRA2_76 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_34 , OPRA2_77 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_34 , OPRA2_S0 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_34 , OPRA2_S1 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_34 , OPRA2_S2 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_34 , OPRA2_S3 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_34 , OPRA2_S4 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_34 , OPRA2_S5 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_34 , OPRA2_S6 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_34 , OPRA2_S7 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_35 , OPRA2_00 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_35 , OPRA2_01 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_35 , OPRA2_02 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_35 , OPRA2_03 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_35 , OPRA2_04 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_35 , OPRA2_05 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_35 , OPRA2_06 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_35 , OPRA2_07 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_35 , OPRA2_10 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_35 , OPRA2_11 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13
                         , OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_35 , OPRA2_12 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_35 , OPRA2_13 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15
                         , OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_35 , OPRA2_14 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_35 , OPRA2_15 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_35 , OPRA2_16 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_35 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_35 , OPRA2_20 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_35 , OPRA2_21 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_35 , OPRA2_22 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_35 , OPRA2_23 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_35 , OPRA2_24 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_35 , OPRA2_25 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_35 , OPRA2_26 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_35 , OPRA2_27 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_35 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_35 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_35 , OPRA2_32 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_35 , OPRA2_33 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_35 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_35 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_35 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_35 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_35 , OPRA2_40 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_35 , OPRA2_41 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_35 , OPRA2_42 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_35 , OPRA2_43 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_35 , OPRA2_44 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_35 , OPRA2_45 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_35 , OPRA2_46 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_35 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_35 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46, OPRA2_45
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_35 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_S2, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_S1, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_35 , OPRA2_52 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_31, OPRA2_30, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_35 , OPRA2_53 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_57
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04
                         , OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_35 , OPRA2_54 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_63
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_35 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_S6
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_S5, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_35 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_35 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_47
                         , OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_S4
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_S3, OPRA2_27
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_35 , OPRA2_60 )
          , Set.fromList [ OPRA2_71, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_35 , OPRA2_61 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_35 , OPRA2_62 )
          , Set.fromList [ OPRA2_73, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_35 , OPRA2_63 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_35 , OPRA2_64 )
          , Set.fromList [ OPRA2_75, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_35 , OPRA2_65 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_35 , OPRA2_66 )
          , Set.fromList [ OPRA2_77, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10
                         , OPRA2_07 ] )
        , ( ( OPRA2_35 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_35 , OPRA2_70 )
          , Set.fromList [ OPRA2_71, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_35 , OPRA2_71 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_35 , OPRA2_72 )
          , Set.fromList [ OPRA2_73, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_35 , OPRA2_73 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_35 , OPRA2_74 )
          , Set.fromList [ OPRA2_75, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_35 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_35 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10
                         , OPRA2_07 ] )
        , ( ( OPRA2_35 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_35 , OPRA2_S0 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_35 , OPRA2_S1 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_35 , OPRA2_S2 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_35 , OPRA2_S3 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_35 , OPRA2_S4 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_35 , OPRA2_S5 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_35 , OPRA2_S6 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_35 , OPRA2_S7 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_36 , OPRA2_00 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_36 , OPRA2_01 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_36 , OPRA2_02 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_36 , OPRA2_03 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_36 , OPRA2_04 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_36 , OPRA2_05 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_36 , OPRA2_06 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_36 , OPRA2_07 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_36 , OPRA2_10 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_36 , OPRA2_11 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_36 , OPRA2_12 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_36 , OPRA2_13 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_36 , OPRA2_14 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_36 , OPRA2_15 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_36 , OPRA2_16 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_36 , OPRA2_17 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_36 , OPRA2_20 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_36 , OPRA2_21 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_36 , OPRA2_22 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_36 , OPRA2_23 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_36 , OPRA2_24 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_36 , OPRA2_25 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_36 , OPRA2_26 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_36 , OPRA2_27 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_36 , OPRA2_30 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_36 , OPRA2_31 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_36 , OPRA2_32 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_36 , OPRA2_33 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_36 , OPRA2_34 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_36 , OPRA2_35 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_36 , OPRA2_36 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_36 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_36 , OPRA2_40 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_36 , OPRA2_41 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_36 , OPRA2_42 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_36 , OPRA2_43 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_36 , OPRA2_44 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_36 , OPRA2_45 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_36 , OPRA2_46 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_36 , OPRA2_47 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_36 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_36 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_36 , OPRA2_52 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_36 , OPRA2_53 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_36 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_36 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_36 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_36 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_36 , OPRA2_60 )
          , Set.fromList [ OPRA2_70, OPRA2_34, OPRA2_S3 ] )
        , ( ( OPRA2_36 , OPRA2_61 )
          , Set.fromList [ OPRA2_71, OPRA2_35, OPRA2_S3, OPRA2_S2, OPRA2_S1 ] )
        , ( ( OPRA2_36 , OPRA2_62 )
          , Set.fromList [ OPRA2_72, OPRA2_36, OPRA2_S1 ] )
        , ( ( OPRA2_36 , OPRA2_63 )
          , Set.fromList [ OPRA2_73, OPRA2_S7, OPRA2_37, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_36 , OPRA2_64 )
          , Set.fromList [ OPRA2_74, OPRA2_S7, OPRA2_30 ] )
        , ( ( OPRA2_36 , OPRA2_65 )
          , Set.fromList [ OPRA2_75, OPRA2_S7, OPRA2_S6, OPRA2_S5, OPRA2_31 ] )
        , ( ( OPRA2_36 , OPRA2_66 )
          , Set.fromList [ OPRA2_76, OPRA2_S5, OPRA2_32 ] )
        , ( ( OPRA2_36 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_S5, OPRA2_S4, OPRA2_33, OPRA2_S3 ] )
        , ( ( OPRA2_36 , OPRA2_70 )
          , Set.fromList [ OPRA2_71, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_36 , OPRA2_71 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_36 , OPRA2_72 )
          , Set.fromList [ OPRA2_73, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_36 , OPRA2_73 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_36 , OPRA2_74 )
          , Set.fromList [ OPRA2_75, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_36 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_36 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10
                         , OPRA2_07 ] )
        , ( ( OPRA2_36 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_36 , OPRA2_S0 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_36 , OPRA2_S1 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_36 , OPRA2_S2 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_36 , OPRA2_S3 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_36 , OPRA2_S4 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_36 , OPRA2_S5 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_36 , OPRA2_S6 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_36 , OPRA2_S7 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_37 , OPRA2_00 )
          , Set.fromList [ OPRA2_71, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_37 , OPRA2_01 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_37 , OPRA2_02 )
          , Set.fromList [ OPRA2_73, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_37 , OPRA2_03 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_37 , OPRA2_04 )
          , Set.fromList [ OPRA2_75, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_37 , OPRA2_05 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_37 , OPRA2_06 )
          , Set.fromList [ OPRA2_77, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10
                         , OPRA2_07 ] )
        , ( ( OPRA2_37 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_37 , OPRA2_10 )
          , Set.fromList [ OPRA2_71, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_37 , OPRA2_11 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_37 , OPRA2_12 )
          , Set.fromList [ OPRA2_73, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_37 , OPRA2_13 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_37 , OPRA2_14 )
          , Set.fromList [ OPRA2_75, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_37 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_37 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10
                         , OPRA2_07 ] )
        , ( ( OPRA2_37 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_37 , OPRA2_20 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_37 , OPRA2_21 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_37 , OPRA2_22 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_37 , OPRA2_23 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_37 , OPRA2_24 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_37 , OPRA2_25 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_37 , OPRA2_26 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_37 , OPRA2_27 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_37 , OPRA2_30 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_37 , OPRA2_31 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13
                         , OPRA2_12, OPRA2_11 ] )
        , ( ( OPRA2_37 , OPRA2_32 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_37 , OPRA2_33 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15
                         , OPRA2_14, OPRA2_13 ] )
        , ( ( OPRA2_37 , OPRA2_34 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_37 , OPRA2_35 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17
                         , OPRA2_16, OPRA2_15 ] )
        , ( ( OPRA2_37 , OPRA2_36 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_37 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_11, OPRA2_10 ] )
        , ( ( OPRA2_37 , OPRA2_40 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_37 , OPRA2_41 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_37 , OPRA2_42 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_37 , OPRA2_43 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_37 , OPRA2_44 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_37 , OPRA2_45 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_37 , OPRA2_46 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_37 , OPRA2_47 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_37 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_37 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_37 , OPRA2_52 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_37 , OPRA2_53 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_37 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_37 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_37 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_37 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_37 , OPRA2_60 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_37 , OPRA2_61 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_37 , OPRA2_62 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_37 , OPRA2_63 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_37 , OPRA2_64 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_37 , OPRA2_65 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_37 , OPRA2_66 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_37 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_37 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46, OPRA2_45
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_37 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_S2, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_S1, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_37 , OPRA2_72 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_61, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_31, OPRA2_30, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_37 , OPRA2_73 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_57
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04
                         , OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_37 , OPRA2_74 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_63
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_37 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_S6
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_S5, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_37 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_65, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_37, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_37 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_47
                         , OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_S4
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_S3, OPRA2_27
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_37 , OPRA2_S0 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_37 , OPRA2_S1 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_37 , OPRA2_S2 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_37 , OPRA2_S3 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_37 , OPRA2_S4 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_37 , OPRA2_S5 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_37 , OPRA2_S6 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_37 , OPRA2_S7 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_40 , OPRA2_00 )
          , Set.fromList [ OPRA2_44, OPRA2_S4, OPRA2_00 ] )
        , ( ( OPRA2_40 , OPRA2_01 )
          , Set.fromList [ OPRA2_45, OPRA2_S3, OPRA2_01 ] )
        , ( ( OPRA2_40 , OPRA2_02 )
          , Set.fromList [ OPRA2_46, OPRA2_S2, OPRA2_02 ] )
        , ( ( OPRA2_40 , OPRA2_03 )
          , Set.fromList [ OPRA2_47, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_40 , OPRA2_04 )
          , Set.fromList [ OPRA2_40, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_40 , OPRA2_05 )
          , Set.fromList [ OPRA2_S7, OPRA2_41, OPRA2_05 ] )
        , ( ( OPRA2_40 , OPRA2_06 )
          , Set.fromList [ OPRA2_S6, OPRA2_42, OPRA2_06 ] )
        , ( ( OPRA2_40 , OPRA2_07 )
          , Set.fromList [ OPRA2_S5, OPRA2_43, OPRA2_07 ] )
        , ( ( OPRA2_40 , OPRA2_10 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_40 , OPRA2_11 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_40 , OPRA2_12 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_40 , OPRA2_13 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_40 , OPRA2_14 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_40 , OPRA2_15 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_40 , OPRA2_16 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_40 , OPRA2_17 )
          , Set.fromList [ OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_40 , OPRA2_20 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_40 , OPRA2_21 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_40 , OPRA2_22 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_40 , OPRA2_23 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_40 , OPRA2_24 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_40 , OPRA2_25 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_40 , OPRA2_26 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_40 , OPRA2_27 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_40 , OPRA2_30 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_40 , OPRA2_31 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_40 , OPRA2_32 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_40 , OPRA2_33 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_40 , OPRA2_34 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_40 , OPRA2_35 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_40 , OPRA2_36 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_40 , OPRA2_37 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_40 , OPRA2_40 )
          , Set.fromList [ OPRA2_40 ] )
        , ( ( OPRA2_40 , OPRA2_41 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_40 , OPRA2_42 )
          , Set.fromList [ OPRA2_42 ] )
        , ( ( OPRA2_40 , OPRA2_43 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_40 , OPRA2_44 )
          , Set.fromList [ OPRA2_44 ] )
        , ( ( OPRA2_40 , OPRA2_45 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_40 , OPRA2_46 )
          , Set.fromList [ OPRA2_46 ] )
        , ( ( OPRA2_40 , OPRA2_47 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_40 , OPRA2_50 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_40 , OPRA2_51 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_40 , OPRA2_52 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_40 , OPRA2_53 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_40 , OPRA2_54 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_40 , OPRA2_55 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_40 , OPRA2_56 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_40 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_40 , OPRA2_60 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_40 , OPRA2_61 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_40 , OPRA2_62 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_40 , OPRA2_63 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_40 , OPRA2_64 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_40 , OPRA2_65 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_40 , OPRA2_66 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_40 , OPRA2_67 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_40 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_40 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_40 , OPRA2_72 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_40 , OPRA2_73 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_40 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_40 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_40 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_40 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_40 , OPRA2_S0 )
          , Set.fromList [ OPRA2_40 ] )
        , ( ( OPRA2_40 , OPRA2_S1 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_40 , OPRA2_S2 )
          , Set.fromList [ OPRA2_46 ] )
        , ( ( OPRA2_40 , OPRA2_S3 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_40 , OPRA2_S4 )
          , Set.fromList [ OPRA2_44 ] )
        , ( ( OPRA2_40 , OPRA2_S5 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_40 , OPRA2_S6 )
          , Set.fromList [ OPRA2_42 ] )
        , ( ( OPRA2_40 , OPRA2_S7 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_41 , OPRA2_00 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_41 , OPRA2_01 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_41 , OPRA2_02 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_41 , OPRA2_03 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_41 , OPRA2_04 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_41 , OPRA2_05 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_41 , OPRA2_06 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_41 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_41 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_44, OPRA2_S4, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_21, OPRA2_11, OPRA2_00 ] )
        , ( ( OPRA2_41 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_41 , OPRA2_12 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_46, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_S2, OPRA2_13, OPRA2_02 ] )
        , ( ( OPRA2_41 , OPRA2_13 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_41 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_41 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_65
                         , OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_41 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_S6, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_42, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_17, OPRA2_06 ] )
        , ( ( OPRA2_41 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_S5, OPRA2_43, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_41 , OPRA2_20 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_41 , OPRA2_21 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_41 , OPRA2_22 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_41 , OPRA2_23 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_41 , OPRA2_24 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_41 , OPRA2_25 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_41 , OPRA2_26 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_41 , OPRA2_27 )
          , Set.fromList [ OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_41 , OPRA2_30 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_41 , OPRA2_31 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_41 , OPRA2_32 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_41 , OPRA2_33 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_41 , OPRA2_34 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_41 , OPRA2_35 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_41 , OPRA2_36 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_41 , OPRA2_37 )
          , Set.fromList [ OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_41 , OPRA2_40 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_41 , OPRA2_41 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_41 , OPRA2_42 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_41 , OPRA2_43 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_41 , OPRA2_44 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_41 , OPRA2_45 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_41 , OPRA2_46 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_41 , OPRA2_47 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_41 , OPRA2_50 )
          , Set.fromList [ OPRA2_57, OPRA2_40, OPRA2_31 ] )
        , ( ( OPRA2_41 , OPRA2_51 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_41, OPRA2_33
                         , OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_41 , OPRA2_52 )
          , Set.fromList [ OPRA2_51, OPRA2_42, OPRA2_33 ] )
        , ( ( OPRA2_41 , OPRA2_53 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_35
                         , OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_41 , OPRA2_54 )
          , Set.fromList [ OPRA2_53, OPRA2_44, OPRA2_35 ] )
        , ( ( OPRA2_41 , OPRA2_55 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_37
                         , OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_41 , OPRA2_56 )
          , Set.fromList [ OPRA2_55, OPRA2_46, OPRA2_37 ] )
        , ( ( OPRA2_41 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_37
                         , OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_41 , OPRA2_60 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_41 , OPRA2_61 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_41 , OPRA2_62 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_41 , OPRA2_63 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_41 , OPRA2_64 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_41 , OPRA2_65 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_41 , OPRA2_66 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_41 , OPRA2_67 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_41 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_41 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_41 , OPRA2_72 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_41 , OPRA2_73 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_41 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_41 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_41 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_41 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_41 , OPRA2_S0 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_41 , OPRA2_S1 )
          , Set.fromList [ OPRA2_47, OPRA2_41, OPRA2_40 ] )
        , ( ( OPRA2_41 , OPRA2_S2 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_41 , OPRA2_S3 )
          , Set.fromList [ OPRA2_47, OPRA2_46, OPRA2_45 ] )
        , ( ( OPRA2_41 , OPRA2_S4 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_41 , OPRA2_S5 )
          , Set.fromList [ OPRA2_45, OPRA2_44, OPRA2_43 ] )
        , ( ( OPRA2_41 , OPRA2_S6 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_41 , OPRA2_S7 )
          , Set.fromList [ OPRA2_43, OPRA2_42, OPRA2_41 ] )
        , ( ( OPRA2_42 , OPRA2_00 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_42 , OPRA2_01 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_42 , OPRA2_02 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_42 , OPRA2_03 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_42 , OPRA2_04 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_42 , OPRA2_05 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_42 , OPRA2_06 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_42 , OPRA2_07 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_42 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_42 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_42 , OPRA2_12 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_42 , OPRA2_13 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_42 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_42 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_42 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_42 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_42 , OPRA2_20 )
          , Set.fromList [ OPRA2_44, OPRA2_S4, OPRA2_00 ] )
        , ( ( OPRA2_42 , OPRA2_21 )
          , Set.fromList [ OPRA2_45, OPRA2_S3, OPRA2_01 ] )
        , ( ( OPRA2_42 , OPRA2_22 )
          , Set.fromList [ OPRA2_46, OPRA2_S2, OPRA2_02 ] )
        , ( ( OPRA2_42 , OPRA2_23 )
          , Set.fromList [ OPRA2_47, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_42 , OPRA2_24 )
          , Set.fromList [ OPRA2_40, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_42 , OPRA2_25 )
          , Set.fromList [ OPRA2_S7, OPRA2_41, OPRA2_05 ] )
        , ( ( OPRA2_42 , OPRA2_26 )
          , Set.fromList [ OPRA2_S6, OPRA2_42, OPRA2_06 ] )
        , ( ( OPRA2_42 , OPRA2_27 )
          , Set.fromList [ OPRA2_S5, OPRA2_43, OPRA2_07 ] )
        , ( ( OPRA2_42 , OPRA2_30 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_42 , OPRA2_31 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_42 , OPRA2_32 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_42 , OPRA2_33 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_42 , OPRA2_34 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_42 , OPRA2_35 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_42 , OPRA2_36 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_42 , OPRA2_37 )
          , Set.fromList [ OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_42 , OPRA2_40 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_42 , OPRA2_41 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_42 , OPRA2_42 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_42 , OPRA2_43 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_42 , OPRA2_44 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_42 , OPRA2_45 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_42 , OPRA2_46 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_42 , OPRA2_47 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_42 , OPRA2_50 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_42 , OPRA2_51 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_42 , OPRA2_52 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_42 , OPRA2_53 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_42 , OPRA2_54 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_42 , OPRA2_55 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_42 , OPRA2_56 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_42 , OPRA2_57 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_42 , OPRA2_60 )
          , Set.fromList [ OPRA2_40 ] )
        , ( ( OPRA2_42 , OPRA2_61 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_42 , OPRA2_62 )
          , Set.fromList [ OPRA2_42 ] )
        , ( ( OPRA2_42 , OPRA2_63 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_42 , OPRA2_64 )
          , Set.fromList [ OPRA2_44 ] )
        , ( ( OPRA2_42 , OPRA2_65 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_42 , OPRA2_66 )
          , Set.fromList [ OPRA2_46 ] )
        , ( ( OPRA2_42 , OPRA2_67 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_42 , OPRA2_70 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_42 , OPRA2_71 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_42 , OPRA2_72 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_42 , OPRA2_73 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_42 , OPRA2_74 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_42 , OPRA2_75 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_42 , OPRA2_76 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_42 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_42 , OPRA2_S0 )
          , Set.fromList [ OPRA2_42 ] )
        , ( ( OPRA2_42 , OPRA2_S1 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_42 , OPRA2_S2 )
          , Set.fromList [ OPRA2_40 ] )
        , ( ( OPRA2_42 , OPRA2_S3 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_42 , OPRA2_S4 )
          , Set.fromList [ OPRA2_46 ] )
        , ( ( OPRA2_42 , OPRA2_S5 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_42 , OPRA2_S6 )
          , Set.fromList [ OPRA2_44 ] )
        , ( ( OPRA2_42 , OPRA2_S7 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_43 , OPRA2_00 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_43 , OPRA2_01 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_43 , OPRA2_02 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_43 , OPRA2_03 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_43 , OPRA2_04 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_43 , OPRA2_05 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_43 , OPRA2_06 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_43 , OPRA2_07 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_43 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_43 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_43 , OPRA2_12 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_43 , OPRA2_13 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_43 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_43 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_43 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_43 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_43 , OPRA2_20 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_43 , OPRA2_21 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_43 , OPRA2_22 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_43 , OPRA2_23 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_43 , OPRA2_24 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_43 , OPRA2_25 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_43 , OPRA2_26 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_43 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_43 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_44, OPRA2_S4, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_21, OPRA2_11, OPRA2_00 ] )
        , ( ( OPRA2_43 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_43 , OPRA2_32 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_46, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_S2, OPRA2_13, OPRA2_02 ] )
        , ( ( OPRA2_43 , OPRA2_33 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_43 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_43 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_65
                         , OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_43 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_S6, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_42, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_17, OPRA2_06 ] )
        , ( ( OPRA2_43 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_S5, OPRA2_43, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_43 , OPRA2_40 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_43 , OPRA2_41 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_43 , OPRA2_42 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_43 , OPRA2_43 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_43 , OPRA2_44 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_43 , OPRA2_45 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_43 , OPRA2_46 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_43 , OPRA2_47 )
          , Set.fromList [ OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_43 , OPRA2_50 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_43 , OPRA2_51 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_43 , OPRA2_52 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_43 , OPRA2_53 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_43 , OPRA2_54 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_43 , OPRA2_55 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_43 , OPRA2_56 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_43 , OPRA2_57 )
          , Set.fromList [ OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_43 , OPRA2_60 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_43 , OPRA2_61 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_43 , OPRA2_62 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_43 , OPRA2_63 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_43 , OPRA2_64 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_43 , OPRA2_65 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_43 , OPRA2_66 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_43 , OPRA2_67 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_43 , OPRA2_70 )
          , Set.fromList [ OPRA2_57, OPRA2_40, OPRA2_31 ] )
        , ( ( OPRA2_43 , OPRA2_71 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_41, OPRA2_33
                         , OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_43 , OPRA2_72 )
          , Set.fromList [ OPRA2_51, OPRA2_42, OPRA2_33 ] )
        , ( ( OPRA2_43 , OPRA2_73 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_35
                         , OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_43 , OPRA2_74 )
          , Set.fromList [ OPRA2_53, OPRA2_44, OPRA2_35 ] )
        , ( ( OPRA2_43 , OPRA2_75 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_37
                         , OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_43 , OPRA2_76 )
          , Set.fromList [ OPRA2_55, OPRA2_46, OPRA2_37 ] )
        , ( ( OPRA2_43 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_37
                         , OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_43 , OPRA2_S0 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_43 , OPRA2_S1 )
          , Set.fromList [ OPRA2_43, OPRA2_42, OPRA2_41 ] )
        , ( ( OPRA2_43 , OPRA2_S2 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_43 , OPRA2_S3 )
          , Set.fromList [ OPRA2_47, OPRA2_41, OPRA2_40 ] )
        , ( ( OPRA2_43 , OPRA2_S4 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_43 , OPRA2_S5 )
          , Set.fromList [ OPRA2_47, OPRA2_46, OPRA2_45 ] )
        , ( ( OPRA2_43 , OPRA2_S6 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_43 , OPRA2_S7 )
          , Set.fromList [ OPRA2_45, OPRA2_44, OPRA2_43 ] )
        , ( ( OPRA2_44 , OPRA2_00 )
          , Set.fromList [ OPRA2_40 ] )
        , ( ( OPRA2_44 , OPRA2_01 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_44 , OPRA2_02 )
          , Set.fromList [ OPRA2_42 ] )
        , ( ( OPRA2_44 , OPRA2_03 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_44 , OPRA2_04 )
          , Set.fromList [ OPRA2_44 ] )
        , ( ( OPRA2_44 , OPRA2_05 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_44 , OPRA2_06 )
          , Set.fromList [ OPRA2_46 ] )
        , ( ( OPRA2_44 , OPRA2_07 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_44 , OPRA2_10 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_44 , OPRA2_11 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_44 , OPRA2_12 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_44 , OPRA2_13 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_44 , OPRA2_14 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_44 , OPRA2_15 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_44 , OPRA2_16 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_44 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_44 , OPRA2_20 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_44 , OPRA2_21 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_44 , OPRA2_22 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_44 , OPRA2_23 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_44 , OPRA2_24 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_44 , OPRA2_25 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_44 , OPRA2_26 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_44 , OPRA2_27 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_44 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_44 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_44 , OPRA2_32 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_44 , OPRA2_33 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_44 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_44 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_44 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_44 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_44 , OPRA2_40 )
          , Set.fromList [ OPRA2_44, OPRA2_S4, OPRA2_00 ] )
        , ( ( OPRA2_44 , OPRA2_41 )
          , Set.fromList [ OPRA2_45, OPRA2_S3, OPRA2_01 ] )
        , ( ( OPRA2_44 , OPRA2_42 )
          , Set.fromList [ OPRA2_46, OPRA2_S2, OPRA2_02 ] )
        , ( ( OPRA2_44 , OPRA2_43 )
          , Set.fromList [ OPRA2_47, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_44 , OPRA2_44 )
          , Set.fromList [ OPRA2_40, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_44 , OPRA2_45 )
          , Set.fromList [ OPRA2_S7, OPRA2_41, OPRA2_05 ] )
        , ( ( OPRA2_44 , OPRA2_46 )
          , Set.fromList [ OPRA2_S6, OPRA2_42, OPRA2_06 ] )
        , ( ( OPRA2_44 , OPRA2_47 )
          , Set.fromList [ OPRA2_S5, OPRA2_43, OPRA2_07 ] )
        , ( ( OPRA2_44 , OPRA2_50 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_44 , OPRA2_51 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_44 , OPRA2_52 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_44 , OPRA2_53 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_44 , OPRA2_54 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_44 , OPRA2_55 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_44 , OPRA2_56 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_44 , OPRA2_57 )
          , Set.fromList [ OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_44 , OPRA2_60 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_44 , OPRA2_61 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_44 , OPRA2_62 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_44 , OPRA2_63 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_44 , OPRA2_64 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_44 , OPRA2_65 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_44 , OPRA2_66 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_44 , OPRA2_67 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_44 , OPRA2_70 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_44 , OPRA2_71 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_44 , OPRA2_72 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_44 , OPRA2_73 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_44 , OPRA2_74 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_44 , OPRA2_75 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_44 , OPRA2_76 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_44 , OPRA2_77 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_44 , OPRA2_S0 )
          , Set.fromList [ OPRA2_44 ] )
        , ( ( OPRA2_44 , OPRA2_S1 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_44 , OPRA2_S2 )
          , Set.fromList [ OPRA2_42 ] )
        , ( ( OPRA2_44 , OPRA2_S3 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_44 , OPRA2_S4 )
          , Set.fromList [ OPRA2_40 ] )
        , ( ( OPRA2_44 , OPRA2_S5 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_44 , OPRA2_S6 )
          , Set.fromList [ OPRA2_46 ] )
        , ( ( OPRA2_44 , OPRA2_S7 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_45 , OPRA2_00 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_45 , OPRA2_01 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_45 , OPRA2_02 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_45 , OPRA2_03 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_45 , OPRA2_04 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_45 , OPRA2_05 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_45 , OPRA2_06 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_45 , OPRA2_07 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_45 , OPRA2_10 )
          , Set.fromList [ OPRA2_57, OPRA2_40, OPRA2_31 ] )
        , ( ( OPRA2_45 , OPRA2_11 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_41, OPRA2_33
                         , OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_45 , OPRA2_12 )
          , Set.fromList [ OPRA2_51, OPRA2_42, OPRA2_33 ] )
        , ( ( OPRA2_45 , OPRA2_13 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_35
                         , OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_45 , OPRA2_14 )
          , Set.fromList [ OPRA2_53, OPRA2_44, OPRA2_35 ] )
        , ( ( OPRA2_45 , OPRA2_15 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_37
                         , OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_45 , OPRA2_16 )
          , Set.fromList [ OPRA2_55, OPRA2_46, OPRA2_37 ] )
        , ( ( OPRA2_45 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_37
                         , OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_45 , OPRA2_20 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_45 , OPRA2_21 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_45 , OPRA2_22 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_45 , OPRA2_23 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_45 , OPRA2_24 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_45 , OPRA2_25 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_45 , OPRA2_26 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_45 , OPRA2_27 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_45 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_45 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_45 , OPRA2_32 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_45 , OPRA2_33 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_45 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_45 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_45 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_45 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_45 , OPRA2_40 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_45 , OPRA2_41 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_45 , OPRA2_42 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_45 , OPRA2_43 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_45 , OPRA2_44 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_45 , OPRA2_45 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_45 , OPRA2_46 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_45 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_45 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_44, OPRA2_S4, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_21, OPRA2_11, OPRA2_00 ] )
        , ( ( OPRA2_45 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_45 , OPRA2_52 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_46, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_S2, OPRA2_13, OPRA2_02 ] )
        , ( ( OPRA2_45 , OPRA2_53 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_45 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_45 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_65
                         , OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_45 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_S6, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_42, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_17, OPRA2_06 ] )
        , ( ( OPRA2_45 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_S5, OPRA2_43, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_45 , OPRA2_60 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_45 , OPRA2_61 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_45 , OPRA2_62 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_45 , OPRA2_63 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_45 , OPRA2_64 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_45 , OPRA2_65 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_45 , OPRA2_66 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_45 , OPRA2_67 )
          , Set.fromList [ OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_45 , OPRA2_70 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_45 , OPRA2_71 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_45 , OPRA2_72 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_45 , OPRA2_73 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_45 , OPRA2_74 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_45 , OPRA2_75 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_45 , OPRA2_76 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_45 , OPRA2_77 )
          , Set.fromList [ OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_45 , OPRA2_S0 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_45 , OPRA2_S1 )
          , Set.fromList [ OPRA2_45, OPRA2_44, OPRA2_43 ] )
        , ( ( OPRA2_45 , OPRA2_S2 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_45 , OPRA2_S3 )
          , Set.fromList [ OPRA2_43, OPRA2_42, OPRA2_41 ] )
        , ( ( OPRA2_45 , OPRA2_S4 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_45 , OPRA2_S5 )
          , Set.fromList [ OPRA2_47, OPRA2_41, OPRA2_40 ] )
        , ( ( OPRA2_45 , OPRA2_S6 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_45 , OPRA2_S7 )
          , Set.fromList [ OPRA2_47, OPRA2_46, OPRA2_45 ] )
        , ( ( OPRA2_46 , OPRA2_00 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_46 , OPRA2_01 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_46 , OPRA2_02 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_46 , OPRA2_03 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_46 , OPRA2_04 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_46 , OPRA2_05 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_46 , OPRA2_06 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_46 , OPRA2_07 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_46 , OPRA2_10 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_46 , OPRA2_11 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_46 , OPRA2_12 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_46 , OPRA2_13 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_46 , OPRA2_14 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_46 , OPRA2_15 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_46 , OPRA2_16 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_46 , OPRA2_17 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_46 , OPRA2_20 )
          , Set.fromList [ OPRA2_40 ] )
        , ( ( OPRA2_46 , OPRA2_21 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_46 , OPRA2_22 )
          , Set.fromList [ OPRA2_42 ] )
        , ( ( OPRA2_46 , OPRA2_23 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_46 , OPRA2_24 )
          , Set.fromList [ OPRA2_44 ] )
        , ( ( OPRA2_46 , OPRA2_25 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_46 , OPRA2_26 )
          , Set.fromList [ OPRA2_46 ] )
        , ( ( OPRA2_46 , OPRA2_27 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_46 , OPRA2_30 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_46 , OPRA2_31 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_46 , OPRA2_32 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_46 , OPRA2_33 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_46 , OPRA2_34 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_46 , OPRA2_35 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_46 , OPRA2_36 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_46 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_46 , OPRA2_40 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_46 , OPRA2_41 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_46 , OPRA2_42 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_46 , OPRA2_43 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_46 , OPRA2_44 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_46 , OPRA2_45 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_46 , OPRA2_46 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_46 , OPRA2_47 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_46 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_46 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_46 , OPRA2_52 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_46 , OPRA2_53 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_46 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_46 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_46 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_46 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_46 , OPRA2_60 )
          , Set.fromList [ OPRA2_44, OPRA2_S4, OPRA2_00 ] )
        , ( ( OPRA2_46 , OPRA2_61 )
          , Set.fromList [ OPRA2_45, OPRA2_S3, OPRA2_01 ] )
        , ( ( OPRA2_46 , OPRA2_62 )
          , Set.fromList [ OPRA2_46, OPRA2_S2, OPRA2_02 ] )
        , ( ( OPRA2_46 , OPRA2_63 )
          , Set.fromList [ OPRA2_47, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_46 , OPRA2_64 )
          , Set.fromList [ OPRA2_40, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_46 , OPRA2_65 )
          , Set.fromList [ OPRA2_S7, OPRA2_41, OPRA2_05 ] )
        , ( ( OPRA2_46 , OPRA2_66 )
          , Set.fromList [ OPRA2_S6, OPRA2_42, OPRA2_06 ] )
        , ( ( OPRA2_46 , OPRA2_67 )
          , Set.fromList [ OPRA2_S5, OPRA2_43, OPRA2_07 ] )
        , ( ( OPRA2_46 , OPRA2_70 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_46 , OPRA2_71 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_46 , OPRA2_72 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_46 , OPRA2_73 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_46 , OPRA2_74 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_46 , OPRA2_75 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_46 , OPRA2_76 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_46 , OPRA2_77 )
          , Set.fromList [ OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_46 , OPRA2_S0 )
          , Set.fromList [ OPRA2_46 ] )
        , ( ( OPRA2_46 , OPRA2_S1 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_46 , OPRA2_S2 )
          , Set.fromList [ OPRA2_44 ] )
        , ( ( OPRA2_46 , OPRA2_S3 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_46 , OPRA2_S4 )
          , Set.fromList [ OPRA2_42 ] )
        , ( ( OPRA2_46 , OPRA2_S5 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_46 , OPRA2_S6 )
          , Set.fromList [ OPRA2_40 ] )
        , ( ( OPRA2_46 , OPRA2_S7 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_47 , OPRA2_00 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_47 , OPRA2_01 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_47 , OPRA2_02 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_47 , OPRA2_03 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_47 , OPRA2_04 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_47 , OPRA2_05 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_47 , OPRA2_06 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_47 , OPRA2_07 )
          , Set.fromList [ OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_47 , OPRA2_10 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_47 , OPRA2_11 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_47 , OPRA2_12 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_47 , OPRA2_13 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_47 , OPRA2_14 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_47 , OPRA2_15 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_47 , OPRA2_16 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_47 , OPRA2_17 )
          , Set.fromList [ OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_47 , OPRA2_20 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_47 , OPRA2_21 )
          , Set.fromList [ OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_47 , OPRA2_22 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_47 , OPRA2_23 )
          , Set.fromList [ OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_47 , OPRA2_24 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_47 , OPRA2_25 )
          , Set.fromList [ OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_47 , OPRA2_26 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_47 , OPRA2_27 )
          , Set.fromList [ OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_47 , OPRA2_30 )
          , Set.fromList [ OPRA2_57, OPRA2_40, OPRA2_31 ] )
        , ( ( OPRA2_47 , OPRA2_31 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_41, OPRA2_33
                         , OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_47 , OPRA2_32 )
          , Set.fromList [ OPRA2_51, OPRA2_42, OPRA2_33 ] )
        , ( ( OPRA2_47 , OPRA2_33 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_35
                         , OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_47 , OPRA2_34 )
          , Set.fromList [ OPRA2_53, OPRA2_44, OPRA2_35 ] )
        , ( ( OPRA2_47 , OPRA2_35 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_37
                         , OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_47 , OPRA2_36 )
          , Set.fromList [ OPRA2_55, OPRA2_46, OPRA2_37 ] )
        , ( ( OPRA2_47 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_37
                         , OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_47 , OPRA2_40 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_47 , OPRA2_41 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_47 , OPRA2_42 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_47 , OPRA2_43 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_47 , OPRA2_44 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_47 , OPRA2_45 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_47 , OPRA2_46 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_47 , OPRA2_47 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_47 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_47 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_47 , OPRA2_52 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_47 , OPRA2_53 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_47 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_47 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_47 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_47 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_47 , OPRA2_60 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_47 , OPRA2_61 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_47 , OPRA2_62 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_47 , OPRA2_63 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50 ] )
        , ( ( OPRA2_47 , OPRA2_64 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_47 , OPRA2_65 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51 ] )
        , ( ( OPRA2_47 , OPRA2_66 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_47 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53 ] )
        , ( ( OPRA2_47 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_44, OPRA2_S4, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_21, OPRA2_11, OPRA2_00 ] )
        , ( ( OPRA2_47 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_45, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_47 , OPRA2_72 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_46, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_S2, OPRA2_13, OPRA2_02 ] )
        , ( ( OPRA2_47 , OPRA2_73 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_47 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_04, OPRA2_S0 ] )
        , ( ( OPRA2_47 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_65
                         , OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_47 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_S6, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_42, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_17, OPRA2_06 ] )
        , ( ( OPRA2_47 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_S5, OPRA2_43, OPRA2_37, OPRA2_33
                         , OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_47 , OPRA2_S0 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_47 , OPRA2_S1 )
          , Set.fromList [ OPRA2_47, OPRA2_46, OPRA2_45 ] )
        , ( ( OPRA2_47 , OPRA2_S2 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_47 , OPRA2_S3 )
          , Set.fromList [ OPRA2_45, OPRA2_44, OPRA2_43 ] )
        , ( ( OPRA2_47 , OPRA2_S4 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_47 , OPRA2_S5 )
          , Set.fromList [ OPRA2_43, OPRA2_42, OPRA2_41 ] )
        , ( ( OPRA2_47 , OPRA2_S6 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_47 , OPRA2_S7 )
          , Set.fromList [ OPRA2_47, OPRA2_41, OPRA2_40 ] )
        , ( ( OPRA2_50 , OPRA2_00 )
          , Set.fromList [ OPRA2_54, OPRA2_S5, OPRA2_10 ] )
        , ( ( OPRA2_50 , OPRA2_01 )
          , Set.fromList [ OPRA2_55, OPRA2_S5, OPRA2_S4, OPRA2_S3, OPRA2_11 ] )
        , ( ( OPRA2_50 , OPRA2_02 )
          , Set.fromList [ OPRA2_56, OPRA2_S3, OPRA2_12 ] )
        , ( ( OPRA2_50 , OPRA2_03 )
          , Set.fromList [ OPRA2_57, OPRA2_S3, OPRA2_S2, OPRA2_13, OPRA2_S1 ] )
        , ( ( OPRA2_50 , OPRA2_04 )
          , Set.fromList [ OPRA2_50, OPRA2_14, OPRA2_S1 ] )
        , ( ( OPRA2_50 , OPRA2_05 )
          , Set.fromList [ OPRA2_S7, OPRA2_51, OPRA2_15, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_50 , OPRA2_06 )
          , Set.fromList [ OPRA2_S7, OPRA2_52, OPRA2_16 ] )
        , ( ( OPRA2_50 , OPRA2_07 )
          , Set.fromList [ OPRA2_S7, OPRA2_S6, OPRA2_53, OPRA2_S5, OPRA2_17 ] )
        , ( ( OPRA2_50 , OPRA2_10 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21
                         , OPRA2_11 ] )
        , ( ( OPRA2_50 , OPRA2_11 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_50 , OPRA2_12 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_13 ] )
        , ( ( OPRA2_50 , OPRA2_13 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_50 , OPRA2_14 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15 ] )
        , ( ( OPRA2_50 , OPRA2_15 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_50 , OPRA2_16 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_17 ] )
        , ( ( OPRA2_50 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_50 , OPRA2_20 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_50 , OPRA2_21 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_50 , OPRA2_22 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_50 , OPRA2_23 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_50 , OPRA2_24 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_50 , OPRA2_25 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_50 , OPRA2_26 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_50 , OPRA2_27 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_50 , OPRA2_30 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_50 , OPRA2_31 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_50 , OPRA2_32 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_50 , OPRA2_33 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_50 , OPRA2_34 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_50 , OPRA2_35 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_50 , OPRA2_36 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_50 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_50 , OPRA2_40 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_50 , OPRA2_41 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_50 , OPRA2_42 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_50 , OPRA2_43 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_50 , OPRA2_44 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_50 , OPRA2_45 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_50 , OPRA2_46 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_50 , OPRA2_47 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_50 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_50 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_50 , OPRA2_52 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_50 , OPRA2_53 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_50 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_50 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_50 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_50 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_50 , OPRA2_60 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_50 , OPRA2_61 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_50 , OPRA2_62 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_50 , OPRA2_63 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_50 , OPRA2_64 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_50 , OPRA2_65 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_50 , OPRA2_66 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_50 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_50 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_17
                         , OPRA2_07 ] )
        , ( ( OPRA2_50 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_50 , OPRA2_72 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_50 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_50 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_50 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_50 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_50 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_50 , OPRA2_S0 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_50 , OPRA2_S1 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_50 , OPRA2_S2 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_50 , OPRA2_S3 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_50 , OPRA2_S4 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_50 , OPRA2_S5 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_50 , OPRA2_S6 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_50 , OPRA2_S7 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_51 , OPRA2_00 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_17
                         , OPRA2_07 ] )
        , ( ( OPRA2_51 , OPRA2_01 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_51 , OPRA2_02 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_51 , OPRA2_03 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_51 , OPRA2_04 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_51 , OPRA2_05 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_51 , OPRA2_06 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_51 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_51 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_S5, OPRA2_43
                         , OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_51 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_S5, OPRA2_45
                         , OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_S4
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_S3, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_51 , OPRA2_12 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_51, OPRA2_50, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_S3
                         , OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_51 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_S3, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_S2, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_S1, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_51 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_51 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04
                         , OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_51 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_65
                         , OPRA2_64, OPRA2_63, OPRA2_57, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_51 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_S6, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_S5, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_51 , OPRA2_20 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21
                         , OPRA2_11 ] )
        , ( ( OPRA2_51 , OPRA2_21 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_51 , OPRA2_22 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_13 ] )
        , ( ( OPRA2_51 , OPRA2_23 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_51 , OPRA2_24 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15 ] )
        , ( ( OPRA2_51 , OPRA2_25 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_51 , OPRA2_26 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_17 ] )
        , ( ( OPRA2_51 , OPRA2_27 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_51 , OPRA2_30 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21
                         , OPRA2_11 ] )
        , ( ( OPRA2_51 , OPRA2_31 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_51 , OPRA2_32 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_13 ] )
        , ( ( OPRA2_51 , OPRA2_33 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_51 , OPRA2_34 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15 ] )
        , ( ( OPRA2_51 , OPRA2_35 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_51 , OPRA2_36 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_17 ] )
        , ( ( OPRA2_51 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_51 , OPRA2_40 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_51 , OPRA2_41 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_51 , OPRA2_42 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_51 , OPRA2_43 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_51 , OPRA2_44 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_51 , OPRA2_45 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_51 , OPRA2_46 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_51 , OPRA2_47 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_51 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_51 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33
                         , OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_51 , OPRA2_52 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_51 , OPRA2_53 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35
                         , OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_51 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_51 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37
                         , OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_51 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_51 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37
                         , OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_51 , OPRA2_60 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_51 , OPRA2_61 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_51 , OPRA2_62 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_51 , OPRA2_63 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_51 , OPRA2_64 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_51 , OPRA2_65 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_51 , OPRA2_66 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_51 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_51 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_17
                         , OPRA2_07 ] )
        , ( ( OPRA2_51 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_51 , OPRA2_72 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_51 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_51 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_51 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_51 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_51 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_51 , OPRA2_S0 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_51 , OPRA2_S1 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_51 , OPRA2_S2 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_51 , OPRA2_S3 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_51 , OPRA2_S4 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_51 , OPRA2_S5 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_51 , OPRA2_S6 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_51 , OPRA2_S7 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_52 , OPRA2_00 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_52 , OPRA2_01 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_52 , OPRA2_02 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_52 , OPRA2_03 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_52 , OPRA2_04 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_52 , OPRA2_05 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_52 , OPRA2_06 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_52 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_52 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_17
                         , OPRA2_07 ] )
        , ( ( OPRA2_52 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_52 , OPRA2_12 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_52 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_52 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_52 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_52 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_52 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_52 , OPRA2_20 )
          , Set.fromList [ OPRA2_54, OPRA2_S5, OPRA2_10 ] )
        , ( ( OPRA2_52 , OPRA2_21 )
          , Set.fromList [ OPRA2_55, OPRA2_S5, OPRA2_S4, OPRA2_S3, OPRA2_11 ] )
        , ( ( OPRA2_52 , OPRA2_22 )
          , Set.fromList [ OPRA2_56, OPRA2_S3, OPRA2_12 ] )
        , ( ( OPRA2_52 , OPRA2_23 )
          , Set.fromList [ OPRA2_57, OPRA2_S3, OPRA2_S2, OPRA2_13, OPRA2_S1 ] )
        , ( ( OPRA2_52 , OPRA2_24 )
          , Set.fromList [ OPRA2_50, OPRA2_14, OPRA2_S1 ] )
        , ( ( OPRA2_52 , OPRA2_25 )
          , Set.fromList [ OPRA2_S7, OPRA2_51, OPRA2_15, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_52 , OPRA2_26 )
          , Set.fromList [ OPRA2_S7, OPRA2_52, OPRA2_16 ] )
        , ( ( OPRA2_52 , OPRA2_27 )
          , Set.fromList [ OPRA2_S7, OPRA2_S6, OPRA2_53, OPRA2_S5, OPRA2_17 ] )
        , ( ( OPRA2_52 , OPRA2_30 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21
                         , OPRA2_11 ] )
        , ( ( OPRA2_52 , OPRA2_31 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_52 , OPRA2_32 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_13 ] )
        , ( ( OPRA2_52 , OPRA2_33 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_52 , OPRA2_34 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15 ] )
        , ( ( OPRA2_52 , OPRA2_35 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_52 , OPRA2_36 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_17 ] )
        , ( ( OPRA2_52 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_52 , OPRA2_40 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_52 , OPRA2_41 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_52 , OPRA2_42 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_52 , OPRA2_43 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_52 , OPRA2_44 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_52 , OPRA2_45 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_52 , OPRA2_46 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_52 , OPRA2_47 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_52 , OPRA2_50 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_52 , OPRA2_51 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_52 , OPRA2_52 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_52 , OPRA2_53 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_52 , OPRA2_54 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_52 , OPRA2_55 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_52 , OPRA2_56 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_52 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_52 , OPRA2_60 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_52 , OPRA2_61 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_52 , OPRA2_62 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_52 , OPRA2_63 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_52 , OPRA2_64 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_52 , OPRA2_65 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_52 , OPRA2_66 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_52 , OPRA2_67 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_52 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_52 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_52 , OPRA2_72 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_52 , OPRA2_73 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_52 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_52 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_52 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_52 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_52 , OPRA2_S0 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_52 , OPRA2_S1 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_52 , OPRA2_S2 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_52 , OPRA2_S3 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_52 , OPRA2_S4 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_52 , OPRA2_S5 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_52 , OPRA2_S6 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_52 , OPRA2_S7 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_53 , OPRA2_00 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_53 , OPRA2_01 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_53 , OPRA2_02 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_53 , OPRA2_03 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_53 , OPRA2_04 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_53 , OPRA2_05 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_53 , OPRA2_06 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_53 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_53 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_17
                         , OPRA2_07 ] )
        , ( ( OPRA2_53 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_53 , OPRA2_12 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_53 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_53 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_53 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_53 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_53 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_53 , OPRA2_20 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_17
                         , OPRA2_07 ] )
        , ( ( OPRA2_53 , OPRA2_21 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_53 , OPRA2_22 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_53 , OPRA2_23 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_53 , OPRA2_24 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_53 , OPRA2_25 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_53 , OPRA2_26 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_53 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_53 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_S5, OPRA2_43
                         , OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_53 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_S5, OPRA2_45
                         , OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_S4
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_S3, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_53 , OPRA2_32 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_51, OPRA2_50, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_S3
                         , OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_53 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_S3, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_S2, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_S1, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_53 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_53 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04
                         , OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_53 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_65
                         , OPRA2_64, OPRA2_63, OPRA2_57, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_53 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_S6, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_S5, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_53 , OPRA2_40 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21
                         , OPRA2_11 ] )
        , ( ( OPRA2_53 , OPRA2_41 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_53 , OPRA2_42 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_13 ] )
        , ( ( OPRA2_53 , OPRA2_43 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_53 , OPRA2_44 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15 ] )
        , ( ( OPRA2_53 , OPRA2_45 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_53 , OPRA2_46 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_17 ] )
        , ( ( OPRA2_53 , OPRA2_47 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_53 , OPRA2_50 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21
                         , OPRA2_11 ] )
        , ( ( OPRA2_53 , OPRA2_51 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_53 , OPRA2_52 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_13 ] )
        , ( ( OPRA2_53 , OPRA2_53 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_53 , OPRA2_54 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15 ] )
        , ( ( OPRA2_53 , OPRA2_55 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_53 , OPRA2_56 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_17 ] )
        , ( ( OPRA2_53 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_53 , OPRA2_60 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_53 , OPRA2_61 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_53 , OPRA2_62 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_53 , OPRA2_63 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_53 , OPRA2_64 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_53 , OPRA2_65 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_53 , OPRA2_66 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_53 , OPRA2_67 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_53 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_53 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33
                         , OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_53 , OPRA2_72 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_53 , OPRA2_73 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35
                         , OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_53 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_53 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37
                         , OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_53 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_53 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37
                         , OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_53 , OPRA2_S0 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_53 , OPRA2_S1 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_53 , OPRA2_S2 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_53 , OPRA2_S3 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_53 , OPRA2_S4 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_53 , OPRA2_S5 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_53 , OPRA2_S6 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_53 , OPRA2_S7 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_54 , OPRA2_00 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_54 , OPRA2_01 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_54 , OPRA2_02 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_54 , OPRA2_03 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_54 , OPRA2_04 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_54 , OPRA2_05 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_54 , OPRA2_06 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_54 , OPRA2_07 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_54 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_54 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_54 , OPRA2_12 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_54 , OPRA2_13 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_54 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_54 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_54 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_54 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_54 , OPRA2_20 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_54 , OPRA2_21 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_54 , OPRA2_22 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_54 , OPRA2_23 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_54 , OPRA2_24 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_54 , OPRA2_25 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_54 , OPRA2_26 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_54 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_54 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_17
                         , OPRA2_07 ] )
        , ( ( OPRA2_54 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_54 , OPRA2_32 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_54 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_54 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_54 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_54 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_54 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_54 , OPRA2_40 )
          , Set.fromList [ OPRA2_54, OPRA2_S5, OPRA2_10 ] )
        , ( ( OPRA2_54 , OPRA2_41 )
          , Set.fromList [ OPRA2_55, OPRA2_S5, OPRA2_S4, OPRA2_S3, OPRA2_11 ] )
        , ( ( OPRA2_54 , OPRA2_42 )
          , Set.fromList [ OPRA2_56, OPRA2_S3, OPRA2_12 ] )
        , ( ( OPRA2_54 , OPRA2_43 )
          , Set.fromList [ OPRA2_57, OPRA2_S3, OPRA2_S2, OPRA2_13, OPRA2_S1 ] )
        , ( ( OPRA2_54 , OPRA2_44 )
          , Set.fromList [ OPRA2_50, OPRA2_14, OPRA2_S1 ] )
        , ( ( OPRA2_54 , OPRA2_45 )
          , Set.fromList [ OPRA2_S7, OPRA2_51, OPRA2_15, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_54 , OPRA2_46 )
          , Set.fromList [ OPRA2_S7, OPRA2_52, OPRA2_16 ] )
        , ( ( OPRA2_54 , OPRA2_47 )
          , Set.fromList [ OPRA2_S7, OPRA2_S6, OPRA2_53, OPRA2_S5, OPRA2_17 ] )
        , ( ( OPRA2_54 , OPRA2_50 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21
                         , OPRA2_11 ] )
        , ( ( OPRA2_54 , OPRA2_51 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_54 , OPRA2_52 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_13 ] )
        , ( ( OPRA2_54 , OPRA2_53 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_54 , OPRA2_54 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15 ] )
        , ( ( OPRA2_54 , OPRA2_55 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_54 , OPRA2_56 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_17 ] )
        , ( ( OPRA2_54 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_54 , OPRA2_60 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_54 , OPRA2_61 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_54 , OPRA2_62 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_54 , OPRA2_63 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_54 , OPRA2_64 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_54 , OPRA2_65 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_54 , OPRA2_66 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_54 , OPRA2_67 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_54 , OPRA2_70 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_54 , OPRA2_71 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_54 , OPRA2_72 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_54 , OPRA2_73 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_54 , OPRA2_74 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_54 , OPRA2_75 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_54 , OPRA2_76 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_54 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_54 , OPRA2_S0 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_54 , OPRA2_S1 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_54 , OPRA2_S2 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_54 , OPRA2_S3 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_54 , OPRA2_S4 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_54 , OPRA2_S5 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_54 , OPRA2_S6 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_54 , OPRA2_S7 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_55 , OPRA2_00 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_55 , OPRA2_01 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_55 , OPRA2_02 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_55 , OPRA2_03 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_55 , OPRA2_04 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_55 , OPRA2_05 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_55 , OPRA2_06 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_55 , OPRA2_07 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_55 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_55 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33
                         , OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_55 , OPRA2_12 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_55 , OPRA2_13 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35
                         , OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_55 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_55 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37
                         , OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_55 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_55 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37
                         , OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_55 , OPRA2_20 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_55 , OPRA2_21 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_55 , OPRA2_22 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_55 , OPRA2_23 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_55 , OPRA2_24 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_55 , OPRA2_25 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_55 , OPRA2_26 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_55 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_55 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_17
                         , OPRA2_07 ] )
        , ( ( OPRA2_55 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_55 , OPRA2_32 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_55 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_55 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_55 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_55 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_55 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_55 , OPRA2_40 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_17
                         , OPRA2_07 ] )
        , ( ( OPRA2_55 , OPRA2_41 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_55 , OPRA2_42 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_55 , OPRA2_43 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_55 , OPRA2_44 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_55 , OPRA2_45 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_55 , OPRA2_46 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_55 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_55 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_S5, OPRA2_43
                         , OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_55 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_S5, OPRA2_45
                         , OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_S4
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_S3, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_55 , OPRA2_52 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_51, OPRA2_50, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_S3
                         , OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_55 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_S3, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_S2, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_S1, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_55 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_55 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04
                         , OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_55 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_65
                         , OPRA2_64, OPRA2_63, OPRA2_57, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_55 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_S6, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_S5, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_55 , OPRA2_60 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21
                         , OPRA2_11 ] )
        , ( ( OPRA2_55 , OPRA2_61 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_55 , OPRA2_62 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_13 ] )
        , ( ( OPRA2_55 , OPRA2_63 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_55 , OPRA2_64 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15 ] )
        , ( ( OPRA2_55 , OPRA2_65 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_55 , OPRA2_66 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_17 ] )
        , ( ( OPRA2_55 , OPRA2_67 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_55 , OPRA2_70 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21
                         , OPRA2_11 ] )
        , ( ( OPRA2_55 , OPRA2_71 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_55 , OPRA2_72 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_13 ] )
        , ( ( OPRA2_55 , OPRA2_73 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_55 , OPRA2_74 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15 ] )
        , ( ( OPRA2_55 , OPRA2_75 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_55 , OPRA2_76 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_17 ] )
        , ( ( OPRA2_55 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_55 , OPRA2_S0 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_55 , OPRA2_S1 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_55 , OPRA2_S2 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_55 , OPRA2_S3 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_55 , OPRA2_S4 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_55 , OPRA2_S5 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_55 , OPRA2_S6 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_55 , OPRA2_S7 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_56 , OPRA2_00 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_56 , OPRA2_01 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_56 , OPRA2_02 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_56 , OPRA2_03 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_56 , OPRA2_04 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_56 , OPRA2_05 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_56 , OPRA2_06 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_56 , OPRA2_07 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_56 , OPRA2_10 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_56 , OPRA2_11 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_56 , OPRA2_12 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_56 , OPRA2_13 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_56 , OPRA2_14 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_56 , OPRA2_15 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_56 , OPRA2_16 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_56 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_56 , OPRA2_20 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_56 , OPRA2_21 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_56 , OPRA2_22 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_56 , OPRA2_23 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_56 , OPRA2_24 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_56 , OPRA2_25 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_56 , OPRA2_26 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_56 , OPRA2_27 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_56 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_56 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_56 , OPRA2_32 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_56 , OPRA2_33 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_56 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_56 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_56 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_56 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_56 , OPRA2_40 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_56 , OPRA2_41 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_56 , OPRA2_42 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_56 , OPRA2_43 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_56 , OPRA2_44 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_56 , OPRA2_45 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_56 , OPRA2_46 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_56 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_56 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_17
                         , OPRA2_07 ] )
        , ( ( OPRA2_56 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_56 , OPRA2_52 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_56 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_56 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_56 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_56 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_56 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_56 , OPRA2_60 )
          , Set.fromList [ OPRA2_54, OPRA2_S5, OPRA2_10 ] )
        , ( ( OPRA2_56 , OPRA2_61 )
          , Set.fromList [ OPRA2_55, OPRA2_S5, OPRA2_S4, OPRA2_S3, OPRA2_11 ] )
        , ( ( OPRA2_56 , OPRA2_62 )
          , Set.fromList [ OPRA2_56, OPRA2_S3, OPRA2_12 ] )
        , ( ( OPRA2_56 , OPRA2_63 )
          , Set.fromList [ OPRA2_57, OPRA2_S3, OPRA2_S2, OPRA2_13, OPRA2_S1 ] )
        , ( ( OPRA2_56 , OPRA2_64 )
          , Set.fromList [ OPRA2_50, OPRA2_14, OPRA2_S1 ] )
        , ( ( OPRA2_56 , OPRA2_65 )
          , Set.fromList [ OPRA2_S7, OPRA2_51, OPRA2_15, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_56 , OPRA2_66 )
          , Set.fromList [ OPRA2_S7, OPRA2_52, OPRA2_16 ] )
        , ( ( OPRA2_56 , OPRA2_67 )
          , Set.fromList [ OPRA2_S7, OPRA2_S6, OPRA2_53, OPRA2_S5, OPRA2_17 ] )
        , ( ( OPRA2_56 , OPRA2_70 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21
                         , OPRA2_11 ] )
        , ( ( OPRA2_56 , OPRA2_71 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_56 , OPRA2_72 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_13 ] )
        , ( ( OPRA2_56 , OPRA2_73 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_56 , OPRA2_74 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15 ] )
        , ( ( OPRA2_56 , OPRA2_75 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_56 , OPRA2_76 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_17 ] )
        , ( ( OPRA2_56 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_56 , OPRA2_S0 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_56 , OPRA2_S1 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_56 , OPRA2_S2 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_56 , OPRA2_S3 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_56 , OPRA2_S4 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_56 , OPRA2_S5 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_56 , OPRA2_S6 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_56 , OPRA2_S7 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_57 , OPRA2_00 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21
                         , OPRA2_11 ] )
        , ( ( OPRA2_57 , OPRA2_01 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_57 , OPRA2_02 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_13 ] )
        , ( ( OPRA2_57 , OPRA2_03 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_57 , OPRA2_04 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15 ] )
        , ( ( OPRA2_57 , OPRA2_05 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_57 , OPRA2_06 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_17 ] )
        , ( ( OPRA2_57 , OPRA2_07 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_57 , OPRA2_10 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21
                         , OPRA2_11 ] )
        , ( ( OPRA2_57 , OPRA2_11 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_13, OPRA2_12
                         , OPRA2_11 ] )
        , ( ( OPRA2_57 , OPRA2_12 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_23
                         , OPRA2_13 ] )
        , ( ( OPRA2_57 , OPRA2_13 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_44, OPRA2_43
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34, OPRA2_33
                         , OPRA2_25, OPRA2_24, OPRA2_23, OPRA2_15, OPRA2_14
                         , OPRA2_13 ] )
        , ( ( OPRA2_57 , OPRA2_14 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15 ] )
        , ( ( OPRA2_57 , OPRA2_15 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16
                         , OPRA2_15 ] )
        , ( ( OPRA2_57 , OPRA2_16 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_17 ] )
        , ( ( OPRA2_57 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_11
                         , OPRA2_10 ] )
        , ( ( OPRA2_57 , OPRA2_20 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_57 , OPRA2_21 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_43, OPRA2_42
                         , OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_57 , OPRA2_22 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_57 , OPRA2_23 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_57 , OPRA2_24 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_57 , OPRA2_25 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_57 , OPRA2_26 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_57 , OPRA2_27 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_57 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57, OPRA2_51, OPRA2_50
                         , OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_57 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_50, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33
                         , OPRA2_32, OPRA2_31 ] )
        , ( ( OPRA2_57 , OPRA2_32 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_57 , OPRA2_33 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35
                         , OPRA2_34, OPRA2_33 ] )
        , ( ( OPRA2_57 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_57 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37
                         , OPRA2_36, OPRA2_35 ] )
        , ( ( OPRA2_57 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_57 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37
                         , OPRA2_31, OPRA2_30 ] )
        , ( ( OPRA2_57 , OPRA2_40 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_57 , OPRA2_41 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_57 , OPRA2_42 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_57 , OPRA2_43 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_57 , OPRA2_44 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_57 , OPRA2_45 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_57 , OPRA2_46 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_57 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_57 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_17
                         , OPRA2_07 ] )
        , ( ( OPRA2_57 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_57 , OPRA2_52 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_57 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_57 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_57 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_57 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_57 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_57 , OPRA2_60 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_17
                         , OPRA2_07 ] )
        , ( ( OPRA2_57 , OPRA2_61 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_57 , OPRA2_62 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_11
                         , OPRA2_01 ] )
        , ( ( OPRA2_57 , OPRA2_63 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_57 , OPRA2_64 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_13
                         , OPRA2_03 ] )
        , ( ( OPRA2_57 , OPRA2_65 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_57 , OPRA2_66 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_15
                         , OPRA2_05 ] )
        , ( ( OPRA2_57 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_57 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_S5, OPRA2_43
                         , OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_21, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07 ] )
        , ( ( OPRA2_57 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_S5, OPRA2_45
                         , OPRA2_44, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_S4
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_S3, OPRA2_23, OPRA2_22, OPRA2_21, OPRA2_17
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_57 , OPRA2_72 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_51, OPRA2_50, OPRA2_45, OPRA2_44
                         , OPRA2_43, OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_S3
                         , OPRA2_23, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_57 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_37, OPRA2_36
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_S3, OPRA2_25
                         , OPRA2_24, OPRA2_23, OPRA2_S2, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_S1, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_57 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_53
                         , OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_03 ] )
        , ( ( OPRA2_57 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_S7, OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_46, OPRA2_45, OPRA2_41, OPRA2_40, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_26, OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_14, OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04
                         , OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_57 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_S7, OPRA2_65
                         , OPRA2_64, OPRA2_63, OPRA2_57, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31, OPRA2_30
                         , OPRA2_27, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_57 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_S6, OPRA2_57, OPRA2_56, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_S5, OPRA2_47, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_40, OPRA2_37, OPRA2_33, OPRA2_32, OPRA2_31
                         , OPRA2_30, OPRA2_27, OPRA2_21, OPRA2_20, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_57 , OPRA2_S0 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_57 , OPRA2_S1 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_57 , OPRA2_S2 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_57 , OPRA2_S3 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_57 , OPRA2_S4 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_57 , OPRA2_S5 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_57 , OPRA2_S6 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_57 , OPRA2_S7 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_60 , OPRA2_00 )
          , Set.fromList [ OPRA2_64, OPRA2_S6, OPRA2_20 ] )
        , ( ( OPRA2_60 , OPRA2_01 )
          , Set.fromList [ OPRA2_65, OPRA2_S5, OPRA2_21 ] )
        , ( ( OPRA2_60 , OPRA2_02 )
          , Set.fromList [ OPRA2_66, OPRA2_S4, OPRA2_22 ] )
        , ( ( OPRA2_60 , OPRA2_03 )
          , Set.fromList [ OPRA2_67, OPRA2_S3, OPRA2_23 ] )
        , ( ( OPRA2_60 , OPRA2_04 )
          , Set.fromList [ OPRA2_60, OPRA2_24, OPRA2_S2 ] )
        , ( ( OPRA2_60 , OPRA2_05 )
          , Set.fromList [ OPRA2_61, OPRA2_25, OPRA2_S1 ] )
        , ( ( OPRA2_60 , OPRA2_06 )
          , Set.fromList [ OPRA2_62, OPRA2_26, OPRA2_S0 ] )
        , ( ( OPRA2_60 , OPRA2_07 )
          , Set.fromList [ OPRA2_S7, OPRA2_63, OPRA2_27 ] )
        , ( ( OPRA2_60 , OPRA2_10 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_60 , OPRA2_11 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_60 , OPRA2_12 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_60 , OPRA2_13 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_60 , OPRA2_14 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_60 , OPRA2_15 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_60 , OPRA2_16 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_60 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_60 , OPRA2_20 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_60 , OPRA2_21 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_60 , OPRA2_22 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_60 , OPRA2_23 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_60 , OPRA2_24 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_60 , OPRA2_25 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_60 , OPRA2_26 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_60 , OPRA2_27 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_60 , OPRA2_30 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_60 , OPRA2_31 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_60 , OPRA2_32 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_60 , OPRA2_33 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_60 , OPRA2_34 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_60 , OPRA2_35 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_60 , OPRA2_36 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_60 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_60 , OPRA2_40 )
          , Set.fromList [ OPRA2_60 ] )
        , ( ( OPRA2_60 , OPRA2_41 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_60 , OPRA2_42 )
          , Set.fromList [ OPRA2_62 ] )
        , ( ( OPRA2_60 , OPRA2_43 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_60 , OPRA2_44 )
          , Set.fromList [ OPRA2_64 ] )
        , ( ( OPRA2_60 , OPRA2_45 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_60 , OPRA2_46 )
          , Set.fromList [ OPRA2_66 ] )
        , ( ( OPRA2_60 , OPRA2_47 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_60 , OPRA2_50 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_60 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_60 , OPRA2_52 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_60 , OPRA2_53 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_60 , OPRA2_54 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_60 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_60 , OPRA2_56 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_60 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_60 , OPRA2_60 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_60 , OPRA2_61 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_60 , OPRA2_62 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_60 , OPRA2_63 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_60 , OPRA2_64 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_60 , OPRA2_65 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_60 , OPRA2_66 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_60 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_60 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_60 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_60 , OPRA2_72 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_60 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_60 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_60 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_60 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_60 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_60 , OPRA2_S0 )
          , Set.fromList [ OPRA2_60 ] )
        , ( ( OPRA2_60 , OPRA2_S1 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_60 , OPRA2_S2 )
          , Set.fromList [ OPRA2_66 ] )
        , ( ( OPRA2_60 , OPRA2_S3 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_60 , OPRA2_S4 )
          , Set.fromList [ OPRA2_64 ] )
        , ( ( OPRA2_60 , OPRA2_S5 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_60 , OPRA2_S6 )
          , Set.fromList [ OPRA2_62 ] )
        , ( ( OPRA2_60 , OPRA2_S7 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_61 , OPRA2_00 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_61 , OPRA2_01 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_61 , OPRA2_02 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_61 , OPRA2_03 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_61 , OPRA2_04 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_61 , OPRA2_05 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_61 , OPRA2_06 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_61 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_61 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_64, OPRA2_S6
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31
                         , OPRA2_20, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_61 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_S5, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_61 , OPRA2_12 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_66, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_S4, OPRA2_33
                         , OPRA2_22, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_61 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35
                         , OPRA2_34, OPRA2_33, OPRA2_S3, OPRA2_23, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_61 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_60, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_24
                         , OPRA2_S2, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_61 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_61, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_61 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_62, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_26
                         , OPRA2_15, OPRA2_05, OPRA2_S0 ] )
        , ( ( OPRA2_61 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_63, OPRA2_57, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_61 , OPRA2_20 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_61 , OPRA2_21 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_61 , OPRA2_22 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_61 , OPRA2_23 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_61 , OPRA2_24 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_61 , OPRA2_25 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_61 , OPRA2_26 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_61 , OPRA2_27 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_61 , OPRA2_30 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_61 , OPRA2_31 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_61 , OPRA2_32 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_61 , OPRA2_33 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_61 , OPRA2_34 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_61 , OPRA2_35 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_61 , OPRA2_36 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_61 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_61 , OPRA2_40 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_61 , OPRA2_41 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_61 , OPRA2_42 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_61 , OPRA2_43 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_61 , OPRA2_44 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_61 , OPRA2_45 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_61 , OPRA2_46 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_61 , OPRA2_47 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_61 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_60, OPRA2_51 ] )
        , ( ( OPRA2_61 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_61, OPRA2_53
                         , OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_61 , OPRA2_52 )
          , Set.fromList [ OPRA2_71, OPRA2_62, OPRA2_53 ] )
        , ( ( OPRA2_61 , OPRA2_53 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_55
                         , OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_61 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_64, OPRA2_55 ] )
        , ( ( OPRA2_61 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_57
                         , OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_61 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_66, OPRA2_57 ] )
        , ( ( OPRA2_61 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_57
                         , OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_61 , OPRA2_60 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_61 , OPRA2_61 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_61 , OPRA2_62 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_61 , OPRA2_63 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_61 , OPRA2_64 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_61 , OPRA2_65 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_61 , OPRA2_66 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_61 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_61 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_61 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_61 , OPRA2_72 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_61 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_61 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_61 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_61 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_61 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_61 , OPRA2_S0 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_61 , OPRA2_S1 )
          , Set.fromList [ OPRA2_67, OPRA2_61, OPRA2_60 ] )
        , ( ( OPRA2_61 , OPRA2_S2 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_61 , OPRA2_S3 )
          , Set.fromList [ OPRA2_67, OPRA2_66, OPRA2_65 ] )
        , ( ( OPRA2_61 , OPRA2_S4 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_61 , OPRA2_S5 )
          , Set.fromList [ OPRA2_65, OPRA2_64, OPRA2_63 ] )
        , ( ( OPRA2_61 , OPRA2_S6 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_61 , OPRA2_S7 )
          , Set.fromList [ OPRA2_63, OPRA2_62, OPRA2_61 ] )
        , ( ( OPRA2_62 , OPRA2_00 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_62 , OPRA2_01 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_62 , OPRA2_02 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_62 , OPRA2_03 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_62 , OPRA2_04 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_62 , OPRA2_05 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_62 , OPRA2_06 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_62 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_62 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_62 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_62 , OPRA2_12 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_62 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_62 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_62 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_62 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_62 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_62 , OPRA2_20 )
          , Set.fromList [ OPRA2_64, OPRA2_S6, OPRA2_20 ] )
        , ( ( OPRA2_62 , OPRA2_21 )
          , Set.fromList [ OPRA2_65, OPRA2_S5, OPRA2_21 ] )
        , ( ( OPRA2_62 , OPRA2_22 )
          , Set.fromList [ OPRA2_66, OPRA2_S4, OPRA2_22 ] )
        , ( ( OPRA2_62 , OPRA2_23 )
          , Set.fromList [ OPRA2_67, OPRA2_S3, OPRA2_23 ] )
        , ( ( OPRA2_62 , OPRA2_24 )
          , Set.fromList [ OPRA2_60, OPRA2_24, OPRA2_S2 ] )
        , ( ( OPRA2_62 , OPRA2_25 )
          , Set.fromList [ OPRA2_61, OPRA2_25, OPRA2_S1 ] )
        , ( ( OPRA2_62 , OPRA2_26 )
          , Set.fromList [ OPRA2_62, OPRA2_26, OPRA2_S0 ] )
        , ( ( OPRA2_62 , OPRA2_27 )
          , Set.fromList [ OPRA2_S7, OPRA2_63, OPRA2_27 ] )
        , ( ( OPRA2_62 , OPRA2_30 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_62 , OPRA2_31 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_62 , OPRA2_32 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_62 , OPRA2_33 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_62 , OPRA2_34 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_62 , OPRA2_35 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_62 , OPRA2_36 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_62 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_62 , OPRA2_40 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_62 , OPRA2_41 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_62 , OPRA2_42 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_62 , OPRA2_43 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_62 , OPRA2_44 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_62 , OPRA2_45 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_62 , OPRA2_46 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_62 , OPRA2_47 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_62 , OPRA2_50 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_62 , OPRA2_51 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_62 , OPRA2_52 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_62 , OPRA2_53 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_62 , OPRA2_54 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_62 , OPRA2_55 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_62 , OPRA2_56 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_62 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_62 , OPRA2_60 )
          , Set.fromList [ OPRA2_60 ] )
        , ( ( OPRA2_62 , OPRA2_61 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_62 , OPRA2_62 )
          , Set.fromList [ OPRA2_62 ] )
        , ( ( OPRA2_62 , OPRA2_63 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_62 , OPRA2_64 )
          , Set.fromList [ OPRA2_64 ] )
        , ( ( OPRA2_62 , OPRA2_65 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_62 , OPRA2_66 )
          , Set.fromList [ OPRA2_66 ] )
        , ( ( OPRA2_62 , OPRA2_67 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_62 , OPRA2_70 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_62 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_62 , OPRA2_72 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_62 , OPRA2_73 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_62 , OPRA2_74 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_62 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_62 , OPRA2_76 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_62 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_62 , OPRA2_S0 )
          , Set.fromList [ OPRA2_62 ] )
        , ( ( OPRA2_62 , OPRA2_S1 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_62 , OPRA2_S2 )
          , Set.fromList [ OPRA2_60 ] )
        , ( ( OPRA2_62 , OPRA2_S3 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_62 , OPRA2_S4 )
          , Set.fromList [ OPRA2_66 ] )
        , ( ( OPRA2_62 , OPRA2_S5 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_62 , OPRA2_S6 )
          , Set.fromList [ OPRA2_64 ] )
        , ( ( OPRA2_62 , OPRA2_S7 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_63 , OPRA2_00 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_63 , OPRA2_01 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_63 , OPRA2_02 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_63 , OPRA2_03 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_63 , OPRA2_04 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_63 , OPRA2_05 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_63 , OPRA2_06 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_63 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_63 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_63 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_63 , OPRA2_12 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_63 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_63 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_63 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_63 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_63 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_63 , OPRA2_20 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_63 , OPRA2_21 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_63 , OPRA2_22 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_63 , OPRA2_23 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_63 , OPRA2_24 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_63 , OPRA2_25 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_63 , OPRA2_26 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_63 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_63 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_64, OPRA2_S6
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31
                         , OPRA2_20, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_63 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_S5, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_63 , OPRA2_32 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_66, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_S4, OPRA2_33
                         , OPRA2_22, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_63 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35
                         , OPRA2_34, OPRA2_33, OPRA2_S3, OPRA2_23, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_63 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_60, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_24
                         , OPRA2_S2, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_63 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_61, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_63 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_62, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_26
                         , OPRA2_15, OPRA2_05, OPRA2_S0 ] )
        , ( ( OPRA2_63 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_63, OPRA2_57, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_63 , OPRA2_40 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_63 , OPRA2_41 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_63 , OPRA2_42 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_63 , OPRA2_43 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_63 , OPRA2_44 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_63 , OPRA2_45 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_63 , OPRA2_46 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_63 , OPRA2_47 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_63 , OPRA2_50 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_63 , OPRA2_51 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_63 , OPRA2_52 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_63 , OPRA2_53 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_63 , OPRA2_54 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_63 , OPRA2_55 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_63 , OPRA2_56 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_63 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_63 , OPRA2_60 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_63 , OPRA2_61 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_63 , OPRA2_62 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_63 , OPRA2_63 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_63 , OPRA2_64 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_63 , OPRA2_65 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_63 , OPRA2_66 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_63 , OPRA2_67 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_63 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_60, OPRA2_51 ] )
        , ( ( OPRA2_63 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_61, OPRA2_53
                         , OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_63 , OPRA2_72 )
          , Set.fromList [ OPRA2_71, OPRA2_62, OPRA2_53 ] )
        , ( ( OPRA2_63 , OPRA2_73 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_55
                         , OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_63 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_64, OPRA2_55 ] )
        , ( ( OPRA2_63 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_57
                         , OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_63 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_66, OPRA2_57 ] )
        , ( ( OPRA2_63 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_57
                         , OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_63 , OPRA2_S0 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_63 , OPRA2_S1 )
          , Set.fromList [ OPRA2_63, OPRA2_62, OPRA2_61 ] )
        , ( ( OPRA2_63 , OPRA2_S2 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_63 , OPRA2_S3 )
          , Set.fromList [ OPRA2_67, OPRA2_61, OPRA2_60 ] )
        , ( ( OPRA2_63 , OPRA2_S4 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_63 , OPRA2_S5 )
          , Set.fromList [ OPRA2_67, OPRA2_66, OPRA2_65 ] )
        , ( ( OPRA2_63 , OPRA2_S6 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_63 , OPRA2_S7 )
          , Set.fromList [ OPRA2_65, OPRA2_64, OPRA2_63 ] )
        , ( ( OPRA2_64 , OPRA2_00 )
          , Set.fromList [ OPRA2_60 ] )
        , ( ( OPRA2_64 , OPRA2_01 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_64 , OPRA2_02 )
          , Set.fromList [ OPRA2_62 ] )
        , ( ( OPRA2_64 , OPRA2_03 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_64 , OPRA2_04 )
          , Set.fromList [ OPRA2_64 ] )
        , ( ( OPRA2_64 , OPRA2_05 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_64 , OPRA2_06 )
          , Set.fromList [ OPRA2_66 ] )
        , ( ( OPRA2_64 , OPRA2_07 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_64 , OPRA2_10 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_64 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_64 , OPRA2_12 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_64 , OPRA2_13 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_64 , OPRA2_14 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_64 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_64 , OPRA2_16 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_64 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_64 , OPRA2_20 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_64 , OPRA2_21 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_64 , OPRA2_22 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_64 , OPRA2_23 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_64 , OPRA2_24 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_64 , OPRA2_25 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_64 , OPRA2_26 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_64 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_64 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_64 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_64 , OPRA2_32 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_64 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_64 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_64 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_64 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_64 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_64 , OPRA2_40 )
          , Set.fromList [ OPRA2_64, OPRA2_S6, OPRA2_20 ] )
        , ( ( OPRA2_64 , OPRA2_41 )
          , Set.fromList [ OPRA2_65, OPRA2_S5, OPRA2_21 ] )
        , ( ( OPRA2_64 , OPRA2_42 )
          , Set.fromList [ OPRA2_66, OPRA2_S4, OPRA2_22 ] )
        , ( ( OPRA2_64 , OPRA2_43 )
          , Set.fromList [ OPRA2_67, OPRA2_S3, OPRA2_23 ] )
        , ( ( OPRA2_64 , OPRA2_44 )
          , Set.fromList [ OPRA2_60, OPRA2_24, OPRA2_S2 ] )
        , ( ( OPRA2_64 , OPRA2_45 )
          , Set.fromList [ OPRA2_61, OPRA2_25, OPRA2_S1 ] )
        , ( ( OPRA2_64 , OPRA2_46 )
          , Set.fromList [ OPRA2_62, OPRA2_26, OPRA2_S0 ] )
        , ( ( OPRA2_64 , OPRA2_47 )
          , Set.fromList [ OPRA2_S7, OPRA2_63, OPRA2_27 ] )
        , ( ( OPRA2_64 , OPRA2_50 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_64 , OPRA2_51 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_64 , OPRA2_52 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_64 , OPRA2_53 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_64 , OPRA2_54 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_64 , OPRA2_55 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_64 , OPRA2_56 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_64 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_64 , OPRA2_60 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_64 , OPRA2_61 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_64 , OPRA2_62 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_64 , OPRA2_63 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_64 , OPRA2_64 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_64 , OPRA2_65 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_64 , OPRA2_66 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_64 , OPRA2_67 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_64 , OPRA2_70 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_64 , OPRA2_71 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_64 , OPRA2_72 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_64 , OPRA2_73 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_64 , OPRA2_74 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_64 , OPRA2_75 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_64 , OPRA2_76 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_64 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_64 , OPRA2_S0 )
          , Set.fromList [ OPRA2_64 ] )
        , ( ( OPRA2_64 , OPRA2_S1 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_64 , OPRA2_S2 )
          , Set.fromList [ OPRA2_62 ] )
        , ( ( OPRA2_64 , OPRA2_S3 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_64 , OPRA2_S4 )
          , Set.fromList [ OPRA2_60 ] )
        , ( ( OPRA2_64 , OPRA2_S5 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_64 , OPRA2_S6 )
          , Set.fromList [ OPRA2_66 ] )
        , ( ( OPRA2_64 , OPRA2_S7 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_65 , OPRA2_00 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_65 , OPRA2_01 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_65 , OPRA2_02 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_65 , OPRA2_03 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_65 , OPRA2_04 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_65 , OPRA2_05 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_65 , OPRA2_06 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_65 , OPRA2_07 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_65 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_60, OPRA2_51 ] )
        , ( ( OPRA2_65 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_61, OPRA2_53
                         , OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_65 , OPRA2_12 )
          , Set.fromList [ OPRA2_71, OPRA2_62, OPRA2_53 ] )
        , ( ( OPRA2_65 , OPRA2_13 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_55
                         , OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_65 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_64, OPRA2_55 ] )
        , ( ( OPRA2_65 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_57
                         , OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_65 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_66, OPRA2_57 ] )
        , ( ( OPRA2_65 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_57
                         , OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_65 , OPRA2_20 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_65 , OPRA2_21 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_65 , OPRA2_22 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_65 , OPRA2_23 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_65 , OPRA2_24 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_65 , OPRA2_25 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_65 , OPRA2_26 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_65 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_65 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_65 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_65 , OPRA2_32 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_65 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_65 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_65 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_65 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_65 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_65 , OPRA2_40 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_65 , OPRA2_41 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_65 , OPRA2_42 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_65 , OPRA2_43 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_65 , OPRA2_44 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_65 , OPRA2_45 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_65 , OPRA2_46 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_65 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_65 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_64, OPRA2_S6
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31
                         , OPRA2_20, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_65 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_S5, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_65 , OPRA2_52 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_66, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_S4, OPRA2_33
                         , OPRA2_22, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_65 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35
                         , OPRA2_34, OPRA2_33, OPRA2_S3, OPRA2_23, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_65 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_60, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_24
                         , OPRA2_S2, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_65 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_61, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_65 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_62, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_26
                         , OPRA2_15, OPRA2_05, OPRA2_S0 ] )
        , ( ( OPRA2_65 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_63, OPRA2_57, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_65 , OPRA2_60 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_65 , OPRA2_61 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_65 , OPRA2_62 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_65 , OPRA2_63 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_65 , OPRA2_64 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_65 , OPRA2_65 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_65 , OPRA2_66 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_65 , OPRA2_67 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_65 , OPRA2_70 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_65 , OPRA2_71 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_65 , OPRA2_72 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_65 , OPRA2_73 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_65 , OPRA2_74 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_65 , OPRA2_75 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_65 , OPRA2_76 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_65 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_65 , OPRA2_S0 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_65 , OPRA2_S1 )
          , Set.fromList [ OPRA2_65, OPRA2_64, OPRA2_63 ] )
        , ( ( OPRA2_65 , OPRA2_S2 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_65 , OPRA2_S3 )
          , Set.fromList [ OPRA2_63, OPRA2_62, OPRA2_61 ] )
        , ( ( OPRA2_65 , OPRA2_S4 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_65 , OPRA2_S5 )
          , Set.fromList [ OPRA2_67, OPRA2_61, OPRA2_60 ] )
        , ( ( OPRA2_65 , OPRA2_S6 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_65 , OPRA2_S7 )
          , Set.fromList [ OPRA2_67, OPRA2_66, OPRA2_65 ] )
        , ( ( OPRA2_66 , OPRA2_00 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_66 , OPRA2_01 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_66 , OPRA2_02 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_66 , OPRA2_03 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_66 , OPRA2_04 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_66 , OPRA2_05 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_66 , OPRA2_06 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_66 , OPRA2_07 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_66 , OPRA2_10 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_66 , OPRA2_11 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_66 , OPRA2_12 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_66 , OPRA2_13 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_66 , OPRA2_14 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_66 , OPRA2_15 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_66 , OPRA2_16 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_66 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_66 , OPRA2_20 )
          , Set.fromList [ OPRA2_60 ] )
        , ( ( OPRA2_66 , OPRA2_21 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_66 , OPRA2_22 )
          , Set.fromList [ OPRA2_62 ] )
        , ( ( OPRA2_66 , OPRA2_23 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_66 , OPRA2_24 )
          , Set.fromList [ OPRA2_64 ] )
        , ( ( OPRA2_66 , OPRA2_25 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_66 , OPRA2_26 )
          , Set.fromList [ OPRA2_66 ] )
        , ( ( OPRA2_66 , OPRA2_27 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_66 , OPRA2_30 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_66 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_66 , OPRA2_32 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_66 , OPRA2_33 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_66 , OPRA2_34 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_66 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_66 , OPRA2_36 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_66 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_66 , OPRA2_40 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_66 , OPRA2_41 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_66 , OPRA2_42 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_66 , OPRA2_43 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_66 , OPRA2_44 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_66 , OPRA2_45 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_66 , OPRA2_46 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_66 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_66 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_66 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_66 , OPRA2_52 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_66 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_66 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_66 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_66 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_66 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_66 , OPRA2_60 )
          , Set.fromList [ OPRA2_64, OPRA2_S6, OPRA2_20 ] )
        , ( ( OPRA2_66 , OPRA2_61 )
          , Set.fromList [ OPRA2_65, OPRA2_S5, OPRA2_21 ] )
        , ( ( OPRA2_66 , OPRA2_62 )
          , Set.fromList [ OPRA2_66, OPRA2_S4, OPRA2_22 ] )
        , ( ( OPRA2_66 , OPRA2_63 )
          , Set.fromList [ OPRA2_67, OPRA2_S3, OPRA2_23 ] )
        , ( ( OPRA2_66 , OPRA2_64 )
          , Set.fromList [ OPRA2_60, OPRA2_24, OPRA2_S2 ] )
        , ( ( OPRA2_66 , OPRA2_65 )
          , Set.fromList [ OPRA2_61, OPRA2_25, OPRA2_S1 ] )
        , ( ( OPRA2_66 , OPRA2_66 )
          , Set.fromList [ OPRA2_62, OPRA2_26, OPRA2_S0 ] )
        , ( ( OPRA2_66 , OPRA2_67 )
          , Set.fromList [ OPRA2_S7, OPRA2_63, OPRA2_27 ] )
        , ( ( OPRA2_66 , OPRA2_70 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_66 , OPRA2_71 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_66 , OPRA2_72 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_66 , OPRA2_73 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_66 , OPRA2_74 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_66 , OPRA2_75 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_66 , OPRA2_76 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_66 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_66 , OPRA2_S0 )
          , Set.fromList [ OPRA2_66 ] )
        , ( ( OPRA2_66 , OPRA2_S1 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_66 , OPRA2_S2 )
          , Set.fromList [ OPRA2_64 ] )
        , ( ( OPRA2_66 , OPRA2_S3 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_66 , OPRA2_S4 )
          , Set.fromList [ OPRA2_62 ] )
        , ( ( OPRA2_66 , OPRA2_S5 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_66 , OPRA2_S6 )
          , Set.fromList [ OPRA2_60 ] )
        , ( ( OPRA2_66 , OPRA2_S7 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_67 , OPRA2_00 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_67 , OPRA2_01 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_67 , OPRA2_02 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_67 , OPRA2_03 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_67 , OPRA2_04 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_67 , OPRA2_05 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_67 , OPRA2_06 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_67 , OPRA2_07 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_67 , OPRA2_10 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_67 , OPRA2_11 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_67 , OPRA2_12 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_67 , OPRA2_13 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_67 , OPRA2_14 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_67 , OPRA2_15 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_67 , OPRA2_16 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_67 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_67 , OPRA2_20 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_67 , OPRA2_21 )
          , Set.fromList [ OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_67 , OPRA2_22 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_67 , OPRA2_23 )
          , Set.fromList [ OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_67 , OPRA2_24 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_67 , OPRA2_25 )
          , Set.fromList [ OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_67 , OPRA2_26 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_67 , OPRA2_27 )
          , Set.fromList [ OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_67 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_60, OPRA2_51 ] )
        , ( ( OPRA2_67 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_61, OPRA2_53
                         , OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_67 , OPRA2_32 )
          , Set.fromList [ OPRA2_71, OPRA2_62, OPRA2_53 ] )
        , ( ( OPRA2_67 , OPRA2_33 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_55
                         , OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_67 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_64, OPRA2_55 ] )
        , ( ( OPRA2_67 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_57
                         , OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_67 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_66, OPRA2_57 ] )
        , ( ( OPRA2_67 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_57
                         , OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_67 , OPRA2_40 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_67 , OPRA2_41 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_67 , OPRA2_42 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_67 , OPRA2_43 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_67 , OPRA2_44 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_67 , OPRA2_45 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_67 , OPRA2_46 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_67 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_67 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_67 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_67 , OPRA2_52 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_67 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_67 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_67 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_67 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_67 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_67 , OPRA2_60 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_67 , OPRA2_61 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_67 , OPRA2_62 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_67 , OPRA2_63 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_67 , OPRA2_64 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_67 , OPRA2_65 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_67 , OPRA2_66 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_67 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_67 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_64, OPRA2_S6
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41, OPRA2_31
                         , OPRA2_20, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_67 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_65, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_S5, OPRA2_43, OPRA2_42, OPRA2_41
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_67 , OPRA2_72 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_66, OPRA2_55
                         , OPRA2_54, OPRA2_53, OPRA2_43, OPRA2_S4, OPRA2_33
                         , OPRA2_22, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_67 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35
                         , OPRA2_34, OPRA2_33, OPRA2_S3, OPRA2_23, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_67 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_60, OPRA2_57
                         , OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35, OPRA2_24
                         , OPRA2_S2, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_67 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_61, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51
                         , OPRA2_50, OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_67 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_62, OPRA2_57
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37, OPRA2_26
                         , OPRA2_15, OPRA2_05, OPRA2_S0 ] )
        , ( ( OPRA2_67 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_S7, OPRA2_63, OPRA2_57, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_41, OPRA2_40
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_67 , OPRA2_S0 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_67 , OPRA2_S1 )
          , Set.fromList [ OPRA2_67, OPRA2_66, OPRA2_65 ] )
        , ( ( OPRA2_67 , OPRA2_S2 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_67 , OPRA2_S3 )
          , Set.fromList [ OPRA2_65, OPRA2_64, OPRA2_63 ] )
        , ( ( OPRA2_67 , OPRA2_S4 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_67 , OPRA2_S5 )
          , Set.fromList [ OPRA2_63, OPRA2_62, OPRA2_61 ] )
        , ( ( OPRA2_67 , OPRA2_S6 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_67 , OPRA2_S7 )
          , Set.fromList [ OPRA2_67, OPRA2_61, OPRA2_60 ] )
        , ( ( OPRA2_70 , OPRA2_00 )
          , Set.fromList [ OPRA2_74, OPRA2_S7, OPRA2_30 ] )
        , ( ( OPRA2_70 , OPRA2_01 )
          , Set.fromList [ OPRA2_75, OPRA2_S7, OPRA2_S6, OPRA2_S5, OPRA2_31 ] )
        , ( ( OPRA2_70 , OPRA2_02 )
          , Set.fromList [ OPRA2_76, OPRA2_S5, OPRA2_32 ] )
        , ( ( OPRA2_70 , OPRA2_03 )
          , Set.fromList [ OPRA2_77, OPRA2_S5, OPRA2_S4, OPRA2_33, OPRA2_S3 ] )
        , ( ( OPRA2_70 , OPRA2_04 )
          , Set.fromList [ OPRA2_70, OPRA2_34, OPRA2_S3 ] )
        , ( ( OPRA2_70 , OPRA2_05 )
          , Set.fromList [ OPRA2_71, OPRA2_35, OPRA2_S3, OPRA2_S2, OPRA2_S1 ] )
        , ( ( OPRA2_70 , OPRA2_06 )
          , Set.fromList [ OPRA2_72, OPRA2_36, OPRA2_S1 ] )
        , ( ( OPRA2_70 , OPRA2_07 )
          , Set.fromList [ OPRA2_73, OPRA2_S7, OPRA2_37, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_70 , OPRA2_10 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_31 ] )
        , ( ( OPRA2_70 , OPRA2_11 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_70 , OPRA2_12 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43
                         , OPRA2_33 ] )
        , ( ( OPRA2_70 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_70 , OPRA2_14 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45
                         , OPRA2_35 ] )
        , ( ( OPRA2_70 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_70 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_37 ] )
        , ( ( OPRA2_70 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_70 , OPRA2_20 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_70 , OPRA2_21 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_70 , OPRA2_22 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_70 , OPRA2_23 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_70 , OPRA2_24 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_70 , OPRA2_25 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_70 , OPRA2_26 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_70 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_70 , OPRA2_30 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_70 , OPRA2_31 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_70 , OPRA2_32 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_70 , OPRA2_33 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_70 , OPRA2_34 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_70 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_70 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_70 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_70 , OPRA2_40 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_70 , OPRA2_41 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_70 , OPRA2_42 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_70 , OPRA2_43 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_70 , OPRA2_44 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_70 , OPRA2_45 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_70 , OPRA2_46 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_70 , OPRA2_47 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_70 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_70 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_70 , OPRA2_52 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_70 , OPRA2_53 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_70 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_70 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_70 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_70 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_70 , OPRA2_60 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_70 , OPRA2_61 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_70 , OPRA2_62 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_70 , OPRA2_63 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_70 , OPRA2_64 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_70 , OPRA2_65 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_70 , OPRA2_66 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_70 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_70 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_27
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_70 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_70 , OPRA2_72 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_31, OPRA2_21
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_70 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_70 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_33, OPRA2_23
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_70 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_70 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_70 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_70 , OPRA2_S0 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_70 , OPRA2_S1 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_70 , OPRA2_S2 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_70 , OPRA2_S3 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_70 , OPRA2_S4 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_70 , OPRA2_S5 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_70 , OPRA2_S6 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_70 , OPRA2_S7 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_71 , OPRA2_00 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_27
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_71 , OPRA2_01 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_71 , OPRA2_02 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_31, OPRA2_21
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_71 , OPRA2_03 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_71 , OPRA2_04 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_33, OPRA2_23
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_71 , OPRA2_05 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_71 , OPRA2_06 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_71 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_71 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_S7, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_71 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_S7, OPRA2_65
                         , OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_S6
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_S5, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_37
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_71 , OPRA2_12 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_71, OPRA2_70, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_43
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_71 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45
                         , OPRA2_44, OPRA2_43, OPRA2_S4, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_71 , OPRA2_14 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_73, OPRA2_72
                         , OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_66, OPRA2_65
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35
                         , OPRA2_34, OPRA2_33, OPRA2_S3, OPRA2_23, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_71 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_S2, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_71 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72
                         , OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_71 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_S7, OPRA2_67
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60, OPRA2_57
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_S1, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_71 , OPRA2_20 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_31 ] )
        , ( ( OPRA2_71 , OPRA2_21 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_71 , OPRA2_22 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43
                         , OPRA2_33 ] )
        , ( ( OPRA2_71 , OPRA2_23 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_71 , OPRA2_24 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45
                         , OPRA2_35 ] )
        , ( ( OPRA2_71 , OPRA2_25 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_71 , OPRA2_26 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_37 ] )
        , ( ( OPRA2_71 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_71 , OPRA2_30 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_31 ] )
        , ( ( OPRA2_71 , OPRA2_31 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_71 , OPRA2_32 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43
                         , OPRA2_33 ] )
        , ( ( OPRA2_71 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_71 , OPRA2_34 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45
                         , OPRA2_35 ] )
        , ( ( OPRA2_71 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_71 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_37 ] )
        , ( ( OPRA2_71 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_71 , OPRA2_40 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_71 , OPRA2_41 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_71 , OPRA2_42 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_71 , OPRA2_43 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_71 , OPRA2_44 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_71 , OPRA2_45 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_71 , OPRA2_46 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_71 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_71 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_61, OPRA2_51
                         , OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_71 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_71 , OPRA2_52 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_53
                         , OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_71 , OPRA2_53 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_71 , OPRA2_54 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_55
                         , OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_71 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05
                         , OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_71 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_57
                         , OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_71 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_71 , OPRA2_60 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_71 , OPRA2_61 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_71 , OPRA2_62 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_71 , OPRA2_63 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_71 , OPRA2_64 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_71 , OPRA2_65 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_71 , OPRA2_66 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_71 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_71 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_27
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_71 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_71 , OPRA2_72 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_31, OPRA2_21
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_71 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_71 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_33, OPRA2_23
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_71 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_71 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_71 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_71 , OPRA2_S0 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_71 , OPRA2_S1 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_71 , OPRA2_S2 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_71 , OPRA2_S3 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_71 , OPRA2_S4 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_71 , OPRA2_S5 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_71 , OPRA2_S6 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_71 , OPRA2_S7 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_72 , OPRA2_00 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_72 , OPRA2_01 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_72 , OPRA2_02 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_72 , OPRA2_03 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_72 , OPRA2_04 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_72 , OPRA2_05 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_72 , OPRA2_06 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_72 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_72 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_27
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_72 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_72 , OPRA2_12 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_31, OPRA2_21
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_72 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_72 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_33, OPRA2_23
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_72 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_72 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_72 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_72 , OPRA2_20 )
          , Set.fromList [ OPRA2_74, OPRA2_S7, OPRA2_30 ] )
        , ( ( OPRA2_72 , OPRA2_21 )
          , Set.fromList [ OPRA2_75, OPRA2_S7, OPRA2_S6, OPRA2_S5, OPRA2_31 ] )
        , ( ( OPRA2_72 , OPRA2_22 )
          , Set.fromList [ OPRA2_76, OPRA2_S5, OPRA2_32 ] )
        , ( ( OPRA2_72 , OPRA2_23 )
          , Set.fromList [ OPRA2_77, OPRA2_S5, OPRA2_S4, OPRA2_33, OPRA2_S3 ] )
        , ( ( OPRA2_72 , OPRA2_24 )
          , Set.fromList [ OPRA2_70, OPRA2_34, OPRA2_S3 ] )
        , ( ( OPRA2_72 , OPRA2_25 )
          , Set.fromList [ OPRA2_71, OPRA2_35, OPRA2_S3, OPRA2_S2, OPRA2_S1 ] )
        , ( ( OPRA2_72 , OPRA2_26 )
          , Set.fromList [ OPRA2_72, OPRA2_36, OPRA2_S1 ] )
        , ( ( OPRA2_72 , OPRA2_27 )
          , Set.fromList [ OPRA2_73, OPRA2_S7, OPRA2_37, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_72 , OPRA2_30 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_31 ] )
        , ( ( OPRA2_72 , OPRA2_31 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_72 , OPRA2_32 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43
                         , OPRA2_33 ] )
        , ( ( OPRA2_72 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_72 , OPRA2_34 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45
                         , OPRA2_35 ] )
        , ( ( OPRA2_72 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_72 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_37 ] )
        , ( ( OPRA2_72 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_72 , OPRA2_40 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_72 , OPRA2_41 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_72 , OPRA2_42 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_72 , OPRA2_43 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_72 , OPRA2_44 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_72 , OPRA2_45 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_72 , OPRA2_46 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_72 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_72 , OPRA2_50 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_72 , OPRA2_51 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_72 , OPRA2_52 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_72 , OPRA2_53 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_72 , OPRA2_54 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_72 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_72 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_72 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_72 , OPRA2_60 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_72 , OPRA2_61 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_72 , OPRA2_62 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_72 , OPRA2_63 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_72 , OPRA2_64 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_72 , OPRA2_65 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_72 , OPRA2_66 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_72 , OPRA2_67 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_72 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_72 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_72 , OPRA2_72 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_72 , OPRA2_73 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_72 , OPRA2_74 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_72 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_72 , OPRA2_76 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_72 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_72 , OPRA2_S0 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_72 , OPRA2_S1 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_72 , OPRA2_S2 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_72 , OPRA2_S3 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_72 , OPRA2_S4 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_72 , OPRA2_S5 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_72 , OPRA2_S6 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_72 , OPRA2_S7 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_73 , OPRA2_00 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_73 , OPRA2_01 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_73 , OPRA2_02 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_73 , OPRA2_03 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_73 , OPRA2_04 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_73 , OPRA2_05 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_73 , OPRA2_06 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_73 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_73 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_27
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_73 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_73 , OPRA2_12 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_31, OPRA2_21
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_73 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_73 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_33, OPRA2_23
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_73 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_73 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_73 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_73 , OPRA2_20 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_27
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_73 , OPRA2_21 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_73 , OPRA2_22 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_31, OPRA2_21
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_73 , OPRA2_23 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_73 , OPRA2_24 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_33, OPRA2_23
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_73 , OPRA2_25 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_73 , OPRA2_26 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_73 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_73 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_S7, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_73 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_S7, OPRA2_65
                         , OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_S6
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_S5, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_37
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_73 , OPRA2_32 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_71, OPRA2_70, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_43
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_73 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45
                         , OPRA2_44, OPRA2_43, OPRA2_S4, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_73 , OPRA2_34 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_73, OPRA2_72
                         , OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_66, OPRA2_65
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35
                         , OPRA2_34, OPRA2_33, OPRA2_S3, OPRA2_23, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_73 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_S2, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_73 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72
                         , OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_73 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_S7, OPRA2_67
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60, OPRA2_57
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_S1, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_73 , OPRA2_40 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_31 ] )
        , ( ( OPRA2_73 , OPRA2_41 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_73 , OPRA2_42 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43
                         , OPRA2_33 ] )
        , ( ( OPRA2_73 , OPRA2_43 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_73 , OPRA2_44 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45
                         , OPRA2_35 ] )
        , ( ( OPRA2_73 , OPRA2_45 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_73 , OPRA2_46 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_37 ] )
        , ( ( OPRA2_73 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_73 , OPRA2_50 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_31 ] )
        , ( ( OPRA2_73 , OPRA2_51 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_73 , OPRA2_52 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43
                         , OPRA2_33 ] )
        , ( ( OPRA2_73 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_73 , OPRA2_54 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45
                         , OPRA2_35 ] )
        , ( ( OPRA2_73 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_73 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_37 ] )
        , ( ( OPRA2_73 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_73 , OPRA2_60 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_73 , OPRA2_61 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_73 , OPRA2_62 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_73 , OPRA2_63 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_73 , OPRA2_64 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_73 , OPRA2_65 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_73 , OPRA2_66 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_73 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_73 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_61, OPRA2_51
                         , OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_73 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_73 , OPRA2_72 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_53
                         , OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_73 , OPRA2_73 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_73 , OPRA2_74 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_55
                         , OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_73 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05
                         , OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_73 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_57
                         , OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_73 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_73 , OPRA2_S0 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_73 , OPRA2_S1 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_73 , OPRA2_S2 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_73 , OPRA2_S3 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_73 , OPRA2_S4 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_73 , OPRA2_S5 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_73 , OPRA2_S6 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_73 , OPRA2_S7 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_74 , OPRA2_00 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_74 , OPRA2_01 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_74 , OPRA2_02 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_74 , OPRA2_03 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_74 , OPRA2_04 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_74 , OPRA2_05 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_74 , OPRA2_06 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_74 , OPRA2_07 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_74 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_74 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_74 , OPRA2_12 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_74 , OPRA2_13 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_74 , OPRA2_14 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_74 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_74 , OPRA2_16 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_74 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_74 , OPRA2_20 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_74 , OPRA2_21 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_74 , OPRA2_22 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_74 , OPRA2_23 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_74 , OPRA2_24 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_74 , OPRA2_25 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_74 , OPRA2_26 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_74 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_74 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_27
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_74 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_74 , OPRA2_32 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_31, OPRA2_21
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_74 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_74 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_33, OPRA2_23
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_74 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_74 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_74 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_74 , OPRA2_40 )
          , Set.fromList [ OPRA2_74, OPRA2_S7, OPRA2_30 ] )
        , ( ( OPRA2_74 , OPRA2_41 )
          , Set.fromList [ OPRA2_75, OPRA2_S7, OPRA2_S6, OPRA2_S5, OPRA2_31 ] )
        , ( ( OPRA2_74 , OPRA2_42 )
          , Set.fromList [ OPRA2_76, OPRA2_S5, OPRA2_32 ] )
        , ( ( OPRA2_74 , OPRA2_43 )
          , Set.fromList [ OPRA2_77, OPRA2_S5, OPRA2_S4, OPRA2_33, OPRA2_S3 ] )
        , ( ( OPRA2_74 , OPRA2_44 )
          , Set.fromList [ OPRA2_70, OPRA2_34, OPRA2_S3 ] )
        , ( ( OPRA2_74 , OPRA2_45 )
          , Set.fromList [ OPRA2_71, OPRA2_35, OPRA2_S3, OPRA2_S2, OPRA2_S1 ] )
        , ( ( OPRA2_74 , OPRA2_46 )
          , Set.fromList [ OPRA2_72, OPRA2_36, OPRA2_S1 ] )
        , ( ( OPRA2_74 , OPRA2_47 )
          , Set.fromList [ OPRA2_73, OPRA2_S7, OPRA2_37, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_74 , OPRA2_50 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_31 ] )
        , ( ( OPRA2_74 , OPRA2_51 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_74 , OPRA2_52 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43
                         , OPRA2_33 ] )
        , ( ( OPRA2_74 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_74 , OPRA2_54 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45
                         , OPRA2_35 ] )
        , ( ( OPRA2_74 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_74 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_37 ] )
        , ( ( OPRA2_74 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_74 , OPRA2_60 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_74 , OPRA2_61 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_74 , OPRA2_62 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_74 , OPRA2_63 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_74 , OPRA2_64 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_74 , OPRA2_65 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_74 , OPRA2_66 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_74 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_74 , OPRA2_70 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_74 , OPRA2_71 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_74 , OPRA2_72 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_74 , OPRA2_73 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_74 , OPRA2_74 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_74 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_74 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_74 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_74 , OPRA2_S0 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_74 , OPRA2_S1 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_74 , OPRA2_S2 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_74 , OPRA2_S3 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_74 , OPRA2_S4 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_74 , OPRA2_S5 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_74 , OPRA2_S6 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_74 , OPRA2_S7 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_75 , OPRA2_00 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_75 , OPRA2_01 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_75 , OPRA2_02 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_75 , OPRA2_03 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_75 , OPRA2_04 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_75 , OPRA2_05 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_75 , OPRA2_06 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_75 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_75 , OPRA2_10 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_61, OPRA2_51
                         , OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_75 , OPRA2_11 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_75 , OPRA2_12 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_53
                         , OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_75 , OPRA2_13 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_75 , OPRA2_14 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_55
                         , OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_75 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05
                         , OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_75 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_57
                         , OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_75 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_75 , OPRA2_20 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_75 , OPRA2_21 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_75 , OPRA2_22 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_75 , OPRA2_23 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_75 , OPRA2_24 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_75 , OPRA2_25 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_75 , OPRA2_26 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_75 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_75 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_27
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_75 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_75 , OPRA2_32 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_31, OPRA2_21
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_75 , OPRA2_33 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_75 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_33, OPRA2_23
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_75 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_75 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_75 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_75 , OPRA2_40 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_27
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_75 , OPRA2_41 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_75 , OPRA2_42 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_31, OPRA2_21
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_75 , OPRA2_43 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_75 , OPRA2_44 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_33, OPRA2_23
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_75 , OPRA2_45 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_75 , OPRA2_46 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_75 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_75 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_S7, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_75 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_S7, OPRA2_65
                         , OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_S6
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_S5, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_37
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_75 , OPRA2_52 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_71, OPRA2_70, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_43
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_75 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45
                         , OPRA2_44, OPRA2_43, OPRA2_S4, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_75 , OPRA2_54 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_73, OPRA2_72
                         , OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_66, OPRA2_65
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35
                         , OPRA2_34, OPRA2_33, OPRA2_S3, OPRA2_23, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_75 , OPRA2_55 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_S2, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_75 , OPRA2_56 )
          , Set.fromList [ OPRA2_77, OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72
                         , OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_75 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_S7, OPRA2_67
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60, OPRA2_57
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_S1, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_75 , OPRA2_60 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_31 ] )
        , ( ( OPRA2_75 , OPRA2_61 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_75 , OPRA2_62 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43
                         , OPRA2_33 ] )
        , ( ( OPRA2_75 , OPRA2_63 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_75 , OPRA2_64 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45
                         , OPRA2_35 ] )
        , ( ( OPRA2_75 , OPRA2_65 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_75 , OPRA2_66 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_37 ] )
        , ( ( OPRA2_75 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_75 , OPRA2_70 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_31 ] )
        , ( ( OPRA2_75 , OPRA2_71 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_75 , OPRA2_72 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43
                         , OPRA2_33 ] )
        , ( ( OPRA2_75 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_75 , OPRA2_74 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45
                         , OPRA2_35 ] )
        , ( ( OPRA2_75 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_75 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_37 ] )
        , ( ( OPRA2_75 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_75 , OPRA2_S0 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_75 , OPRA2_S1 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_75 , OPRA2_S2 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_75 , OPRA2_S3 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_75 , OPRA2_S4 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_75 , OPRA2_S5 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_75 , OPRA2_S6 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_75 , OPRA2_S7 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_76 , OPRA2_00 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_76 , OPRA2_01 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_76 , OPRA2_02 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_76 , OPRA2_03 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_76 , OPRA2_04 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_76 , OPRA2_05 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_76 , OPRA2_06 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_76 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_76 , OPRA2_10 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_76 , OPRA2_11 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_76 , OPRA2_12 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_76 , OPRA2_13 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_76 , OPRA2_14 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_76 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_76 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_76 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_76 , OPRA2_20 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_76 , OPRA2_21 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_76 , OPRA2_22 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_76 , OPRA2_23 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_76 , OPRA2_24 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_76 , OPRA2_25 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_76 , OPRA2_26 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_76 , OPRA2_27 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_76 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_76 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_76 , OPRA2_32 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_76 , OPRA2_33 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_76 , OPRA2_34 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_76 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_76 , OPRA2_36 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_76 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_76 , OPRA2_40 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_76 , OPRA2_41 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_76 , OPRA2_42 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_76 , OPRA2_43 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_76 , OPRA2_44 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_76 , OPRA2_45 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_76 , OPRA2_46 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_76 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_76 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_27
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_76 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_76 , OPRA2_52 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_31, OPRA2_21
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_76 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_76 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_33, OPRA2_23
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_76 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_76 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_76 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_76 , OPRA2_60 )
          , Set.fromList [ OPRA2_74, OPRA2_S7, OPRA2_30 ] )
        , ( ( OPRA2_76 , OPRA2_61 )
          , Set.fromList [ OPRA2_75, OPRA2_S7, OPRA2_S6, OPRA2_S5, OPRA2_31 ] )
        , ( ( OPRA2_76 , OPRA2_62 )
          , Set.fromList [ OPRA2_76, OPRA2_S5, OPRA2_32 ] )
        , ( ( OPRA2_76 , OPRA2_63 )
          , Set.fromList [ OPRA2_77, OPRA2_S5, OPRA2_S4, OPRA2_33, OPRA2_S3 ] )
        , ( ( OPRA2_76 , OPRA2_64 )
          , Set.fromList [ OPRA2_70, OPRA2_34, OPRA2_S3 ] )
        , ( ( OPRA2_76 , OPRA2_65 )
          , Set.fromList [ OPRA2_71, OPRA2_35, OPRA2_S3, OPRA2_S2, OPRA2_S1 ] )
        , ( ( OPRA2_76 , OPRA2_66 )
          , Set.fromList [ OPRA2_72, OPRA2_36, OPRA2_S1 ] )
        , ( ( OPRA2_76 , OPRA2_67 )
          , Set.fromList [ OPRA2_73, OPRA2_S7, OPRA2_37, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_76 , OPRA2_70 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_31 ] )
        , ( ( OPRA2_76 , OPRA2_71 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_76 , OPRA2_72 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43
                         , OPRA2_33 ] )
        , ( ( OPRA2_76 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_76 , OPRA2_74 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45
                         , OPRA2_35 ] )
        , ( ( OPRA2_76 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_76 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_37 ] )
        , ( ( OPRA2_76 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_76 , OPRA2_S0 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_76 , OPRA2_S1 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_76 , OPRA2_S2 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_76 , OPRA2_S3 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_76 , OPRA2_S4 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_76 , OPRA2_S5 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_76 , OPRA2_S6 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_76 , OPRA2_S7 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_77 , OPRA2_00 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_31 ] )
        , ( ( OPRA2_77 , OPRA2_01 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_77 , OPRA2_02 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43
                         , OPRA2_33 ] )
        , ( ( OPRA2_77 , OPRA2_03 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_77 , OPRA2_04 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45
                         , OPRA2_35 ] )
        , ( ( OPRA2_77 , OPRA2_05 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_77 , OPRA2_06 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_37 ] )
        , ( ( OPRA2_77 , OPRA2_07 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_77 , OPRA2_10 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_31 ] )
        , ( ( OPRA2_77 , OPRA2_11 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_33, OPRA2_32
                         , OPRA2_31 ] )
        , ( ( OPRA2_77 , OPRA2_12 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_43
                         , OPRA2_33 ] )
        , ( ( OPRA2_77 , OPRA2_13 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_54, OPRA2_53
                         , OPRA2_45, OPRA2_44, OPRA2_43, OPRA2_35, OPRA2_34
                         , OPRA2_33 ] )
        , ( ( OPRA2_77 , OPRA2_14 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45
                         , OPRA2_35 ] )
        , ( ( OPRA2_77 , OPRA2_15 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_46, OPRA2_45, OPRA2_37, OPRA2_36
                         , OPRA2_35 ] )
        , ( ( OPRA2_77 , OPRA2_16 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_37 ] )
        , ( ( OPRA2_77 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50
                         , OPRA2_47, OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_31
                         , OPRA2_30 ] )
        , ( ( OPRA2_77 , OPRA2_20 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_77 , OPRA2_21 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51 ] )
        , ( ( OPRA2_77 , OPRA2_22 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_77 , OPRA2_23 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_64
                         , OPRA2_63, OPRA2_55, OPRA2_54, OPRA2_53 ] )
        , ( ( OPRA2_77 , OPRA2_24 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_77 , OPRA2_25 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_57, OPRA2_56, OPRA2_55 ] )
        , ( ( OPRA2_77 , OPRA2_26 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_77 , OPRA2_27 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61
                         , OPRA2_60, OPRA2_57, OPRA2_51, OPRA2_50 ] )
        , ( ( OPRA2_77 , OPRA2_30 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_61, OPRA2_51
                         , OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_77 , OPRA2_31 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_53, OPRA2_52
                         , OPRA2_51, OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_77 , OPRA2_32 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_63, OPRA2_53
                         , OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_77 , OPRA2_33 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_55, OPRA2_54
                         , OPRA2_53, OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_77 , OPRA2_34 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_65, OPRA2_55
                         , OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_77 , OPRA2_35 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_67, OPRA2_66, OPRA2_65, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05
                         , OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_77 , OPRA2_36 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_67, OPRA2_57
                         , OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_77 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_67, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_51
                         , OPRA2_50, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07
                         , OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_77 , OPRA2_40 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_77 , OPRA2_41 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_17, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_77 , OPRA2_42 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_77 , OPRA2_43 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_77 , OPRA2_44 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_77 , OPRA2_45 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_77 , OPRA2_46 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_77 , OPRA2_47 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_17, OPRA2_16
                         , OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_77 , OPRA2_50 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_27
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_77 , OPRA2_51 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_77 , OPRA2_52 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_31, OPRA2_21
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_77 , OPRA2_53 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_77 , OPRA2_54 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_33, OPRA2_23
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_77 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_77 , OPRA2_56 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_77 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_77 , OPRA2_60 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_37, OPRA2_27
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06
                         , OPRA2_05 ] )
        , ( ( OPRA2_77 , OPRA2_61 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_71, OPRA2_70
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_21
                         , OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_77 , OPRA2_62 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70, OPRA2_31, OPRA2_21
                         , OPRA2_17, OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_77 , OPRA2_63 )
          , Set.fromList [ OPRA2_77, OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_70
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_23, OPRA2_22
                         , OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12, OPRA2_11
                         , OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02, OPRA2_01
                         , OPRA2_00 ] )
        , ( ( OPRA2_77 , OPRA2_64 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71, OPRA2_33, OPRA2_23
                         , OPRA2_13, OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_77 , OPRA2_65 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72, OPRA2_71
                         , OPRA2_35, OPRA2_34, OPRA2_33, OPRA2_25, OPRA2_24
                         , OPRA2_23, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_05, OPRA2_04, OPRA2_03, OPRA2_02
                         , OPRA2_01 ] )
        , ( ( OPRA2_77 , OPRA2_66 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_35, OPRA2_25
                         , OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_77 , OPRA2_67 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_27, OPRA2_26
                         , OPRA2_25, OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03 ] )
        , ( ( OPRA2_77 , OPRA2_70 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_S7, OPRA2_63, OPRA2_62
                         , OPRA2_61, OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_41
                         , OPRA2_37, OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_17
                         , OPRA2_16, OPRA2_15, OPRA2_07, OPRA2_06, OPRA2_05 ] )
        , ( ( OPRA2_77 , OPRA2_71 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_S7, OPRA2_65
                         , OPRA2_64, OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_S6
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_52, OPRA2_51
                         , OPRA2_S5, OPRA2_43, OPRA2_42, OPRA2_41, OPRA2_37
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_30, OPRA2_27
                         , OPRA2_21, OPRA2_20, OPRA2_17, OPRA2_16, OPRA2_15
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_06, OPRA2_05
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_77 , OPRA2_72 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_71, OPRA2_70, OPRA2_65, OPRA2_64, OPRA2_63
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_43
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_21, OPRA2_17
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_77 , OPRA2_73 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_64, OPRA2_63, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_54, OPRA2_53, OPRA2_S5, OPRA2_45
                         , OPRA2_44, OPRA2_43, OPRA2_S4, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_32, OPRA2_31, OPRA2_S3, OPRA2_23
                         , OPRA2_22, OPRA2_21, OPRA2_17, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_10, OPRA2_07, OPRA2_03, OPRA2_02
                         , OPRA2_01, OPRA2_00 ] )
        , ( ( OPRA2_77 , OPRA2_74 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_73, OPRA2_72
                         , OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_66, OPRA2_65
                         , OPRA2_57, OPRA2_56, OPRA2_55, OPRA2_45, OPRA2_35
                         , OPRA2_34, OPRA2_33, OPRA2_S3, OPRA2_23, OPRA2_13
                         , OPRA2_12, OPRA2_11, OPRA2_03, OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_77 , OPRA2_75 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_66
                         , OPRA2_65, OPRA2_61, OPRA2_60, OPRA2_57, OPRA2_56
                         , OPRA2_55, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_46
                         , OPRA2_45, OPRA2_37, OPRA2_36, OPRA2_35, OPRA2_34
                         , OPRA2_33, OPRA2_S3, OPRA2_25, OPRA2_24, OPRA2_23
                         , OPRA2_S2, OPRA2_15, OPRA2_14, OPRA2_13, OPRA2_12
                         , OPRA2_11, OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03
                         , OPRA2_02, OPRA2_01 ] )
        , ( ( OPRA2_77 , OPRA2_76 )
          , Set.fromList [ OPRA2_77, OPRA2_75, OPRA2_74, OPRA2_73, OPRA2_72
                         , OPRA2_71, OPRA2_70, OPRA2_67, OPRA2_61, OPRA2_60
                         , OPRA2_57, OPRA2_51, OPRA2_50, OPRA2_47, OPRA2_37
                         , OPRA2_36, OPRA2_35, OPRA2_25, OPRA2_15, OPRA2_14
                         , OPRA2_13, OPRA2_S1, OPRA2_05, OPRA2_04, OPRA2_03 ] )
        , ( ( OPRA2_77 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75, OPRA2_74, OPRA2_73
                         , OPRA2_72, OPRA2_71, OPRA2_70, OPRA2_S7, OPRA2_67
                         , OPRA2_63, OPRA2_62, OPRA2_61, OPRA2_60, OPRA2_57
                         , OPRA2_53, OPRA2_52, OPRA2_51, OPRA2_50, OPRA2_47
                         , OPRA2_41, OPRA2_40, OPRA2_37, OPRA2_36, OPRA2_35
                         , OPRA2_31, OPRA2_30, OPRA2_27, OPRA2_26, OPRA2_25
                         , OPRA2_17, OPRA2_16, OPRA2_15, OPRA2_14, OPRA2_13
                         , OPRA2_S1, OPRA2_07, OPRA2_06, OPRA2_05, OPRA2_04
                         , OPRA2_03, OPRA2_S0 ] )
        , ( ( OPRA2_77 , OPRA2_S0 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_77 , OPRA2_S1 )
          , Set.fromList [ OPRA2_77, OPRA2_76, OPRA2_75 ] )
        , ( ( OPRA2_77 , OPRA2_S2 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_77 , OPRA2_S3 )
          , Set.fromList [ OPRA2_75, OPRA2_74, OPRA2_73 ] )
        , ( ( OPRA2_77 , OPRA2_S4 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_77 , OPRA2_S5 )
          , Set.fromList [ OPRA2_73, OPRA2_72, OPRA2_71 ] )
        , ( ( OPRA2_77 , OPRA2_S6 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_77 , OPRA2_S7 )
          , Set.fromList [ OPRA2_77, OPRA2_71, OPRA2_70 ] )
        , ( ( OPRA2_S0 , OPRA2_00 )
          , Set.fromList [ OPRA2_00 ] )
        , ( ( OPRA2_S0 , OPRA2_01 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_S0 , OPRA2_02 )
          , Set.fromList [ OPRA2_02 ] )
        , ( ( OPRA2_S0 , OPRA2_03 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_S0 , OPRA2_04 )
          , Set.fromList [ OPRA2_04 ] )
        , ( ( OPRA2_S0 , OPRA2_05 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_S0 , OPRA2_06 )
          , Set.fromList [ OPRA2_06 ] )
        , ( ( OPRA2_S0 , OPRA2_07 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_S0 , OPRA2_10 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_S0 , OPRA2_11 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_S0 , OPRA2_12 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_S0 , OPRA2_13 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_S0 , OPRA2_14 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_S0 , OPRA2_15 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_S0 , OPRA2_16 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_S0 , OPRA2_17 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_S0 , OPRA2_20 )
          , Set.fromList [ OPRA2_20 ] )
        , ( ( OPRA2_S0 , OPRA2_21 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_S0 , OPRA2_22 )
          , Set.fromList [ OPRA2_22 ] )
        , ( ( OPRA2_S0 , OPRA2_23 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_S0 , OPRA2_24 )
          , Set.fromList [ OPRA2_24 ] )
        , ( ( OPRA2_S0 , OPRA2_25 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_S0 , OPRA2_26 )
          , Set.fromList [ OPRA2_26 ] )
        , ( ( OPRA2_S0 , OPRA2_27 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_S0 , OPRA2_30 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_S0 , OPRA2_31 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_S0 , OPRA2_32 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_S0 , OPRA2_33 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_S0 , OPRA2_34 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_S0 , OPRA2_35 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_S0 , OPRA2_36 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_S0 , OPRA2_37 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_S0 , OPRA2_40 )
          , Set.fromList [ OPRA2_40 ] )
        , ( ( OPRA2_S0 , OPRA2_41 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_S0 , OPRA2_42 )
          , Set.fromList [ OPRA2_42 ] )
        , ( ( OPRA2_S0 , OPRA2_43 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_S0 , OPRA2_44 )
          , Set.fromList [ OPRA2_44 ] )
        , ( ( OPRA2_S0 , OPRA2_45 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_S0 , OPRA2_46 )
          , Set.fromList [ OPRA2_46 ] )
        , ( ( OPRA2_S0 , OPRA2_47 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_S0 , OPRA2_50 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_S0 , OPRA2_51 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_S0 , OPRA2_52 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_S0 , OPRA2_53 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_S0 , OPRA2_54 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_S0 , OPRA2_55 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_S0 , OPRA2_56 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_S0 , OPRA2_57 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_S0 , OPRA2_60 )
          , Set.fromList [ OPRA2_60 ] )
        , ( ( OPRA2_S0 , OPRA2_61 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_S0 , OPRA2_62 )
          , Set.fromList [ OPRA2_62 ] )
        , ( ( OPRA2_S0 , OPRA2_63 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_S0 , OPRA2_64 )
          , Set.fromList [ OPRA2_64 ] )
        , ( ( OPRA2_S0 , OPRA2_65 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_S0 , OPRA2_66 )
          , Set.fromList [ OPRA2_66 ] )
        , ( ( OPRA2_S0 , OPRA2_67 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_S0 , OPRA2_70 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_S0 , OPRA2_71 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_S0 , OPRA2_72 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_S0 , OPRA2_73 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_S0 , OPRA2_74 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_S0 , OPRA2_75 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_S0 , OPRA2_76 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_S0 , OPRA2_77 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_S0 , OPRA2_S0 )
          , Set.fromList [ OPRA2_S0 ] )
        , ( ( OPRA2_S0 , OPRA2_S1 )
          , Set.fromList [ OPRA2_S1 ] )
        , ( ( OPRA2_S0 , OPRA2_S2 )
          , Set.fromList [ OPRA2_S2 ] )
        , ( ( OPRA2_S0 , OPRA2_S3 )
          , Set.fromList [ OPRA2_S3 ] )
        , ( ( OPRA2_S0 , OPRA2_S4 )
          , Set.fromList [ OPRA2_S4 ] )
        , ( ( OPRA2_S0 , OPRA2_S5 )
          , Set.fromList [ OPRA2_S5 ] )
        , ( ( OPRA2_S0 , OPRA2_S6 )
          , Set.fromList [ OPRA2_S6 ] )
        , ( ( OPRA2_S0 , OPRA2_S7 )
          , Set.fromList [ OPRA2_S7 ] )
        , ( ( OPRA2_S1 , OPRA2_00 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_S1 , OPRA2_01 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_S1 , OPRA2_02 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_S1 , OPRA2_03 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_S1 , OPRA2_04 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_S1 , OPRA2_05 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_S1 , OPRA2_06 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_S1 , OPRA2_07 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_S1 , OPRA2_10 )
          , Set.fromList [ OPRA2_30, OPRA2_20, OPRA2_10 ] )
        , ( ( OPRA2_S1 , OPRA2_11 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_S1 , OPRA2_12 )
          , Set.fromList [ OPRA2_32, OPRA2_22, OPRA2_12 ] )
        , ( ( OPRA2_S1 , OPRA2_13 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_S1 , OPRA2_14 )
          , Set.fromList [ OPRA2_34, OPRA2_24, OPRA2_14 ] )
        , ( ( OPRA2_S1 , OPRA2_15 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_S1 , OPRA2_16 )
          , Set.fromList [ OPRA2_36, OPRA2_26, OPRA2_16 ] )
        , ( ( OPRA2_S1 , OPRA2_17 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_S1 , OPRA2_20 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_S1 , OPRA2_21 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_S1 , OPRA2_22 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_S1 , OPRA2_23 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_S1 , OPRA2_24 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_S1 , OPRA2_25 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_S1 , OPRA2_26 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_S1 , OPRA2_27 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_S1 , OPRA2_30 )
          , Set.fromList [ OPRA2_50, OPRA2_40, OPRA2_30 ] )
        , ( ( OPRA2_S1 , OPRA2_31 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_S1 , OPRA2_32 )
          , Set.fromList [ OPRA2_52, OPRA2_42, OPRA2_32 ] )
        , ( ( OPRA2_S1 , OPRA2_33 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_S1 , OPRA2_34 )
          , Set.fromList [ OPRA2_54, OPRA2_44, OPRA2_34 ] )
        , ( ( OPRA2_S1 , OPRA2_35 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_S1 , OPRA2_36 )
          , Set.fromList [ OPRA2_56, OPRA2_46, OPRA2_36 ] )
        , ( ( OPRA2_S1 , OPRA2_37 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_S1 , OPRA2_40 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_S1 , OPRA2_41 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_S1 , OPRA2_42 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_S1 , OPRA2_43 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_S1 , OPRA2_44 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_S1 , OPRA2_45 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_S1 , OPRA2_46 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_S1 , OPRA2_47 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_S1 , OPRA2_50 )
          , Set.fromList [ OPRA2_70, OPRA2_60, OPRA2_50 ] )
        , ( ( OPRA2_S1 , OPRA2_51 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_S1 , OPRA2_52 )
          , Set.fromList [ OPRA2_72, OPRA2_62, OPRA2_52 ] )
        , ( ( OPRA2_S1 , OPRA2_53 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_S1 , OPRA2_54 )
          , Set.fromList [ OPRA2_74, OPRA2_64, OPRA2_54 ] )
        , ( ( OPRA2_S1 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_S1 , OPRA2_56 )
          , Set.fromList [ OPRA2_76, OPRA2_66, OPRA2_56 ] )
        , ( ( OPRA2_S1 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_S1 , OPRA2_60 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_S1 , OPRA2_61 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_S1 , OPRA2_62 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_S1 , OPRA2_63 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_S1 , OPRA2_64 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_S1 , OPRA2_65 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_S1 , OPRA2_66 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_S1 , OPRA2_67 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_S1 , OPRA2_70 )
          , Set.fromList [ OPRA2_70, OPRA2_10, OPRA2_00 ] )
        , ( ( OPRA2_S1 , OPRA2_71 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_S1 , OPRA2_72 )
          , Set.fromList [ OPRA2_72, OPRA2_12, OPRA2_02 ] )
        , ( ( OPRA2_S1 , OPRA2_73 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_S1 , OPRA2_74 )
          , Set.fromList [ OPRA2_74, OPRA2_14, OPRA2_04 ] )
        , ( ( OPRA2_S1 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_S1 , OPRA2_76 )
          , Set.fromList [ OPRA2_76, OPRA2_16, OPRA2_06 ] )
        , ( ( OPRA2_S1 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_S1 , OPRA2_S0 )
          , Set.fromList [ OPRA2_S1 ] )
        , ( ( OPRA2_S1 , OPRA2_S1 )
          , Set.fromList [ OPRA2_S3, OPRA2_S2, OPRA2_S1 ] )
        , ( ( OPRA2_S1 , OPRA2_S2 )
          , Set.fromList [ OPRA2_S3 ] )
        , ( ( OPRA2_S1 , OPRA2_S3 )
          , Set.fromList [ OPRA2_S5, OPRA2_S4, OPRA2_S3 ] )
        , ( ( OPRA2_S1 , OPRA2_S4 )
          , Set.fromList [ OPRA2_S5 ] )
        , ( ( OPRA2_S1 , OPRA2_S5 )
          , Set.fromList [ OPRA2_S7, OPRA2_S6, OPRA2_S5 ] )
        , ( ( OPRA2_S1 , OPRA2_S6 )
          , Set.fromList [ OPRA2_S7 ] )
        , ( ( OPRA2_S1 , OPRA2_S7 )
          , Set.fromList [ OPRA2_S7, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_S2 , OPRA2_00 )
          , Set.fromList [ OPRA2_20 ] )
        , ( ( OPRA2_S2 , OPRA2_01 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_S2 , OPRA2_02 )
          , Set.fromList [ OPRA2_22 ] )
        , ( ( OPRA2_S2 , OPRA2_03 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_S2 , OPRA2_04 )
          , Set.fromList [ OPRA2_24 ] )
        , ( ( OPRA2_S2 , OPRA2_05 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_S2 , OPRA2_06 )
          , Set.fromList [ OPRA2_26 ] )
        , ( ( OPRA2_S2 , OPRA2_07 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_S2 , OPRA2_10 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_S2 , OPRA2_11 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_S2 , OPRA2_12 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_S2 , OPRA2_13 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_S2 , OPRA2_14 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_S2 , OPRA2_15 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_S2 , OPRA2_16 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_S2 , OPRA2_17 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_S2 , OPRA2_20 )
          , Set.fromList [ OPRA2_40 ] )
        , ( ( OPRA2_S2 , OPRA2_21 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_S2 , OPRA2_22 )
          , Set.fromList [ OPRA2_42 ] )
        , ( ( OPRA2_S2 , OPRA2_23 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_S2 , OPRA2_24 )
          , Set.fromList [ OPRA2_44 ] )
        , ( ( OPRA2_S2 , OPRA2_25 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_S2 , OPRA2_26 )
          , Set.fromList [ OPRA2_46 ] )
        , ( ( OPRA2_S2 , OPRA2_27 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_S2 , OPRA2_30 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_S2 , OPRA2_31 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_S2 , OPRA2_32 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_S2 , OPRA2_33 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_S2 , OPRA2_34 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_S2 , OPRA2_35 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_S2 , OPRA2_36 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_S2 , OPRA2_37 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_S2 , OPRA2_40 )
          , Set.fromList [ OPRA2_60 ] )
        , ( ( OPRA2_S2 , OPRA2_41 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_S2 , OPRA2_42 )
          , Set.fromList [ OPRA2_62 ] )
        , ( ( OPRA2_S2 , OPRA2_43 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_S2 , OPRA2_44 )
          , Set.fromList [ OPRA2_64 ] )
        , ( ( OPRA2_S2 , OPRA2_45 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_S2 , OPRA2_46 )
          , Set.fromList [ OPRA2_66 ] )
        , ( ( OPRA2_S2 , OPRA2_47 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_S2 , OPRA2_50 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_S2 , OPRA2_51 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_S2 , OPRA2_52 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_S2 , OPRA2_53 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_S2 , OPRA2_54 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_S2 , OPRA2_55 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_S2 , OPRA2_56 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_S2 , OPRA2_57 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_S2 , OPRA2_60 )
          , Set.fromList [ OPRA2_00 ] )
        , ( ( OPRA2_S2 , OPRA2_61 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_S2 , OPRA2_62 )
          , Set.fromList [ OPRA2_02 ] )
        , ( ( OPRA2_S2 , OPRA2_63 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_S2 , OPRA2_64 )
          , Set.fromList [ OPRA2_04 ] )
        , ( ( OPRA2_S2 , OPRA2_65 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_S2 , OPRA2_66 )
          , Set.fromList [ OPRA2_06 ] )
        , ( ( OPRA2_S2 , OPRA2_67 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_S2 , OPRA2_70 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_S2 , OPRA2_71 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_S2 , OPRA2_72 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_S2 , OPRA2_73 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_S2 , OPRA2_74 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_S2 , OPRA2_75 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_S2 , OPRA2_76 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_S2 , OPRA2_77 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_S2 , OPRA2_S0 )
          , Set.fromList [ OPRA2_S2 ] )
        , ( ( OPRA2_S2 , OPRA2_S1 )
          , Set.fromList [ OPRA2_S3 ] )
        , ( ( OPRA2_S2 , OPRA2_S2 )
          , Set.fromList [ OPRA2_S4 ] )
        , ( ( OPRA2_S2 , OPRA2_S3 )
          , Set.fromList [ OPRA2_S5 ] )
        , ( ( OPRA2_S2 , OPRA2_S4 )
          , Set.fromList [ OPRA2_S6 ] )
        , ( ( OPRA2_S2 , OPRA2_S5 )
          , Set.fromList [ OPRA2_S7 ] )
        , ( ( OPRA2_S2 , OPRA2_S6 )
          , Set.fromList [ OPRA2_S0 ] )
        , ( ( OPRA2_S2 , OPRA2_S7 )
          , Set.fromList [ OPRA2_S1 ] )
        , ( ( OPRA2_S3 , OPRA2_00 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_S3 , OPRA2_01 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_S3 , OPRA2_02 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_S3 , OPRA2_03 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_S3 , OPRA2_04 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_S3 , OPRA2_05 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_S3 , OPRA2_06 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_S3 , OPRA2_07 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_S3 , OPRA2_10 )
          , Set.fromList [ OPRA2_50, OPRA2_40, OPRA2_30 ] )
        , ( ( OPRA2_S3 , OPRA2_11 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_S3 , OPRA2_12 )
          , Set.fromList [ OPRA2_52, OPRA2_42, OPRA2_32 ] )
        , ( ( OPRA2_S3 , OPRA2_13 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_S3 , OPRA2_14 )
          , Set.fromList [ OPRA2_54, OPRA2_44, OPRA2_34 ] )
        , ( ( OPRA2_S3 , OPRA2_15 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_S3 , OPRA2_16 )
          , Set.fromList [ OPRA2_56, OPRA2_46, OPRA2_36 ] )
        , ( ( OPRA2_S3 , OPRA2_17 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_S3 , OPRA2_20 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_S3 , OPRA2_21 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_S3 , OPRA2_22 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_S3 , OPRA2_23 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_S3 , OPRA2_24 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_S3 , OPRA2_25 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_S3 , OPRA2_26 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_S3 , OPRA2_27 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_S3 , OPRA2_30 )
          , Set.fromList [ OPRA2_70, OPRA2_60, OPRA2_50 ] )
        , ( ( OPRA2_S3 , OPRA2_31 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_S3 , OPRA2_32 )
          , Set.fromList [ OPRA2_72, OPRA2_62, OPRA2_52 ] )
        , ( ( OPRA2_S3 , OPRA2_33 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_S3 , OPRA2_34 )
          , Set.fromList [ OPRA2_74, OPRA2_64, OPRA2_54 ] )
        , ( ( OPRA2_S3 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_S3 , OPRA2_36 )
          , Set.fromList [ OPRA2_76, OPRA2_66, OPRA2_56 ] )
        , ( ( OPRA2_S3 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_S3 , OPRA2_40 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_S3 , OPRA2_41 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_S3 , OPRA2_42 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_S3 , OPRA2_43 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_S3 , OPRA2_44 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_S3 , OPRA2_45 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_S3 , OPRA2_46 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_S3 , OPRA2_47 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_S3 , OPRA2_50 )
          , Set.fromList [ OPRA2_70, OPRA2_10, OPRA2_00 ] )
        , ( ( OPRA2_S3 , OPRA2_51 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_S3 , OPRA2_52 )
          , Set.fromList [ OPRA2_72, OPRA2_12, OPRA2_02 ] )
        , ( ( OPRA2_S3 , OPRA2_53 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_S3 , OPRA2_54 )
          , Set.fromList [ OPRA2_74, OPRA2_14, OPRA2_04 ] )
        , ( ( OPRA2_S3 , OPRA2_55 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_S3 , OPRA2_56 )
          , Set.fromList [ OPRA2_76, OPRA2_16, OPRA2_06 ] )
        , ( ( OPRA2_S3 , OPRA2_57 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_S3 , OPRA2_60 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_S3 , OPRA2_61 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_S3 , OPRA2_62 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_S3 , OPRA2_63 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_S3 , OPRA2_64 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_S3 , OPRA2_65 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_S3 , OPRA2_66 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_S3 , OPRA2_67 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_S3 , OPRA2_70 )
          , Set.fromList [ OPRA2_30, OPRA2_20, OPRA2_10 ] )
        , ( ( OPRA2_S3 , OPRA2_71 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_S3 , OPRA2_72 )
          , Set.fromList [ OPRA2_32, OPRA2_22, OPRA2_12 ] )
        , ( ( OPRA2_S3 , OPRA2_73 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_S3 , OPRA2_74 )
          , Set.fromList [ OPRA2_34, OPRA2_24, OPRA2_14 ] )
        , ( ( OPRA2_S3 , OPRA2_75 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_S3 , OPRA2_76 )
          , Set.fromList [ OPRA2_36, OPRA2_26, OPRA2_16 ] )
        , ( ( OPRA2_S3 , OPRA2_77 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_S3 , OPRA2_S0 )
          , Set.fromList [ OPRA2_S3 ] )
        , ( ( OPRA2_S3 , OPRA2_S1 )
          , Set.fromList [ OPRA2_S5, OPRA2_S4, OPRA2_S3 ] )
        , ( ( OPRA2_S3 , OPRA2_S2 )
          , Set.fromList [ OPRA2_S5 ] )
        , ( ( OPRA2_S3 , OPRA2_S3 )
          , Set.fromList [ OPRA2_S7, OPRA2_S6, OPRA2_S5 ] )
        , ( ( OPRA2_S3 , OPRA2_S4 )
          , Set.fromList [ OPRA2_S7 ] )
        , ( ( OPRA2_S3 , OPRA2_S5 )
          , Set.fromList [ OPRA2_S7, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_S3 , OPRA2_S6 )
          , Set.fromList [ OPRA2_S1 ] )
        , ( ( OPRA2_S3 , OPRA2_S7 )
          , Set.fromList [ OPRA2_S3, OPRA2_S2, OPRA2_S1 ] )
        , ( ( OPRA2_S4 , OPRA2_00 )
          , Set.fromList [ OPRA2_40 ] )
        , ( ( OPRA2_S4 , OPRA2_01 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_S4 , OPRA2_02 )
          , Set.fromList [ OPRA2_42 ] )
        , ( ( OPRA2_S4 , OPRA2_03 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_S4 , OPRA2_04 )
          , Set.fromList [ OPRA2_44 ] )
        , ( ( OPRA2_S4 , OPRA2_05 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_S4 , OPRA2_06 )
          , Set.fromList [ OPRA2_46 ] )
        , ( ( OPRA2_S4 , OPRA2_07 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_S4 , OPRA2_10 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_S4 , OPRA2_11 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_S4 , OPRA2_12 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_S4 , OPRA2_13 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_S4 , OPRA2_14 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_S4 , OPRA2_15 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_S4 , OPRA2_16 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_S4 , OPRA2_17 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_S4 , OPRA2_20 )
          , Set.fromList [ OPRA2_60 ] )
        , ( ( OPRA2_S4 , OPRA2_21 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_S4 , OPRA2_22 )
          , Set.fromList [ OPRA2_62 ] )
        , ( ( OPRA2_S4 , OPRA2_23 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_S4 , OPRA2_24 )
          , Set.fromList [ OPRA2_64 ] )
        , ( ( OPRA2_S4 , OPRA2_25 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_S4 , OPRA2_26 )
          , Set.fromList [ OPRA2_66 ] )
        , ( ( OPRA2_S4 , OPRA2_27 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_S4 , OPRA2_30 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_S4 , OPRA2_31 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_S4 , OPRA2_32 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_S4 , OPRA2_33 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_S4 , OPRA2_34 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_S4 , OPRA2_35 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_S4 , OPRA2_36 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_S4 , OPRA2_37 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_S4 , OPRA2_40 )
          , Set.fromList [ OPRA2_00 ] )
        , ( ( OPRA2_S4 , OPRA2_41 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_S4 , OPRA2_42 )
          , Set.fromList [ OPRA2_02 ] )
        , ( ( OPRA2_S4 , OPRA2_43 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_S4 , OPRA2_44 )
          , Set.fromList [ OPRA2_04 ] )
        , ( ( OPRA2_S4 , OPRA2_45 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_S4 , OPRA2_46 )
          , Set.fromList [ OPRA2_06 ] )
        , ( ( OPRA2_S4 , OPRA2_47 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_S4 , OPRA2_50 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_S4 , OPRA2_51 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_S4 , OPRA2_52 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_S4 , OPRA2_53 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_S4 , OPRA2_54 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_S4 , OPRA2_55 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_S4 , OPRA2_56 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_S4 , OPRA2_57 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_S4 , OPRA2_60 )
          , Set.fromList [ OPRA2_20 ] )
        , ( ( OPRA2_S4 , OPRA2_61 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_S4 , OPRA2_62 )
          , Set.fromList [ OPRA2_22 ] )
        , ( ( OPRA2_S4 , OPRA2_63 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_S4 , OPRA2_64 )
          , Set.fromList [ OPRA2_24 ] )
        , ( ( OPRA2_S4 , OPRA2_65 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_S4 , OPRA2_66 )
          , Set.fromList [ OPRA2_26 ] )
        , ( ( OPRA2_S4 , OPRA2_67 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_S4 , OPRA2_70 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_S4 , OPRA2_71 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_S4 , OPRA2_72 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_S4 , OPRA2_73 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_S4 , OPRA2_74 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_S4 , OPRA2_75 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_S4 , OPRA2_76 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_S4 , OPRA2_77 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_S4 , OPRA2_S0 )
          , Set.fromList [ OPRA2_S4 ] )
        , ( ( OPRA2_S4 , OPRA2_S1 )
          , Set.fromList [ OPRA2_S5 ] )
        , ( ( OPRA2_S4 , OPRA2_S2 )
          , Set.fromList [ OPRA2_S6 ] )
        , ( ( OPRA2_S4 , OPRA2_S3 )
          , Set.fromList [ OPRA2_S7 ] )
        , ( ( OPRA2_S4 , OPRA2_S4 )
          , Set.fromList [ OPRA2_S0 ] )
        , ( ( OPRA2_S4 , OPRA2_S5 )
          , Set.fromList [ OPRA2_S1 ] )
        , ( ( OPRA2_S4 , OPRA2_S6 )
          , Set.fromList [ OPRA2_S2 ] )
        , ( ( OPRA2_S4 , OPRA2_S7 )
          , Set.fromList [ OPRA2_S3 ] )
        , ( ( OPRA2_S5 , OPRA2_00 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_S5 , OPRA2_01 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_S5 , OPRA2_02 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_S5 , OPRA2_03 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_S5 , OPRA2_04 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_S5 , OPRA2_05 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_S5 , OPRA2_06 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_S5 , OPRA2_07 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_S5 , OPRA2_10 )
          , Set.fromList [ OPRA2_70, OPRA2_60, OPRA2_50 ] )
        , ( ( OPRA2_S5 , OPRA2_11 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_S5 , OPRA2_12 )
          , Set.fromList [ OPRA2_72, OPRA2_62, OPRA2_52 ] )
        , ( ( OPRA2_S5 , OPRA2_13 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_S5 , OPRA2_14 )
          , Set.fromList [ OPRA2_74, OPRA2_64, OPRA2_54 ] )
        , ( ( OPRA2_S5 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_S5 , OPRA2_16 )
          , Set.fromList [ OPRA2_76, OPRA2_66, OPRA2_56 ] )
        , ( ( OPRA2_S5 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_S5 , OPRA2_20 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_S5 , OPRA2_21 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_S5 , OPRA2_22 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_S5 , OPRA2_23 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_S5 , OPRA2_24 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_S5 , OPRA2_25 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_S5 , OPRA2_26 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_S5 , OPRA2_27 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_S5 , OPRA2_30 )
          , Set.fromList [ OPRA2_70, OPRA2_10, OPRA2_00 ] )
        , ( ( OPRA2_S5 , OPRA2_31 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_S5 , OPRA2_32 )
          , Set.fromList [ OPRA2_72, OPRA2_12, OPRA2_02 ] )
        , ( ( OPRA2_S5 , OPRA2_33 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_S5 , OPRA2_34 )
          , Set.fromList [ OPRA2_74, OPRA2_14, OPRA2_04 ] )
        , ( ( OPRA2_S5 , OPRA2_35 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_S5 , OPRA2_36 )
          , Set.fromList [ OPRA2_76, OPRA2_16, OPRA2_06 ] )
        , ( ( OPRA2_S5 , OPRA2_37 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_S5 , OPRA2_40 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_S5 , OPRA2_41 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_S5 , OPRA2_42 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_S5 , OPRA2_43 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_S5 , OPRA2_44 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_S5 , OPRA2_45 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_S5 , OPRA2_46 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_S5 , OPRA2_47 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_S5 , OPRA2_50 )
          , Set.fromList [ OPRA2_30, OPRA2_20, OPRA2_10 ] )
        , ( ( OPRA2_S5 , OPRA2_51 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_S5 , OPRA2_52 )
          , Set.fromList [ OPRA2_32, OPRA2_22, OPRA2_12 ] )
        , ( ( OPRA2_S5 , OPRA2_53 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_S5 , OPRA2_54 )
          , Set.fromList [ OPRA2_34, OPRA2_24, OPRA2_14 ] )
        , ( ( OPRA2_S5 , OPRA2_55 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_S5 , OPRA2_56 )
          , Set.fromList [ OPRA2_36, OPRA2_26, OPRA2_16 ] )
        , ( ( OPRA2_S5 , OPRA2_57 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_S5 , OPRA2_60 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_S5 , OPRA2_61 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_S5 , OPRA2_62 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_S5 , OPRA2_63 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_S5 , OPRA2_64 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_S5 , OPRA2_65 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_S5 , OPRA2_66 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_S5 , OPRA2_67 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_S5 , OPRA2_70 )
          , Set.fromList [ OPRA2_50, OPRA2_40, OPRA2_30 ] )
        , ( ( OPRA2_S5 , OPRA2_71 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_S5 , OPRA2_72 )
          , Set.fromList [ OPRA2_52, OPRA2_42, OPRA2_32 ] )
        , ( ( OPRA2_S5 , OPRA2_73 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_S5 , OPRA2_74 )
          , Set.fromList [ OPRA2_54, OPRA2_44, OPRA2_34 ] )
        , ( ( OPRA2_S5 , OPRA2_75 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_S5 , OPRA2_76 )
          , Set.fromList [ OPRA2_56, OPRA2_46, OPRA2_36 ] )
        , ( ( OPRA2_S5 , OPRA2_77 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_S5 , OPRA2_S0 )
          , Set.fromList [ OPRA2_S5 ] )
        , ( ( OPRA2_S5 , OPRA2_S1 )
          , Set.fromList [ OPRA2_S7, OPRA2_S6, OPRA2_S5 ] )
        , ( ( OPRA2_S5 , OPRA2_S2 )
          , Set.fromList [ OPRA2_S7 ] )
        , ( ( OPRA2_S5 , OPRA2_S3 )
          , Set.fromList [ OPRA2_S7, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_S5 , OPRA2_S4 )
          , Set.fromList [ OPRA2_S1 ] )
        , ( ( OPRA2_S5 , OPRA2_S5 )
          , Set.fromList [ OPRA2_S3, OPRA2_S2, OPRA2_S1 ] )
        , ( ( OPRA2_S5 , OPRA2_S6 )
          , Set.fromList [ OPRA2_S3 ] )
        , ( ( OPRA2_S5 , OPRA2_S7 )
          , Set.fromList [ OPRA2_S5, OPRA2_S4, OPRA2_S3 ] )
        , ( ( OPRA2_S6 , OPRA2_00 )
          , Set.fromList [ OPRA2_60 ] )
        , ( ( OPRA2_S6 , OPRA2_01 )
          , Set.fromList [ OPRA2_61 ] )
        , ( ( OPRA2_S6 , OPRA2_02 )
          , Set.fromList [ OPRA2_62 ] )
        , ( ( OPRA2_S6 , OPRA2_03 )
          , Set.fromList [ OPRA2_63 ] )
        , ( ( OPRA2_S6 , OPRA2_04 )
          , Set.fromList [ OPRA2_64 ] )
        , ( ( OPRA2_S6 , OPRA2_05 )
          , Set.fromList [ OPRA2_65 ] )
        , ( ( OPRA2_S6 , OPRA2_06 )
          , Set.fromList [ OPRA2_66 ] )
        , ( ( OPRA2_S6 , OPRA2_07 )
          , Set.fromList [ OPRA2_67 ] )
        , ( ( OPRA2_S6 , OPRA2_10 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_S6 , OPRA2_11 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_S6 , OPRA2_12 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_S6 , OPRA2_13 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_S6 , OPRA2_14 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_S6 , OPRA2_15 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_S6 , OPRA2_16 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_S6 , OPRA2_17 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_S6 , OPRA2_20 )
          , Set.fromList [ OPRA2_00 ] )
        , ( ( OPRA2_S6 , OPRA2_21 )
          , Set.fromList [ OPRA2_01 ] )
        , ( ( OPRA2_S6 , OPRA2_22 )
          , Set.fromList [ OPRA2_02 ] )
        , ( ( OPRA2_S6 , OPRA2_23 )
          , Set.fromList [ OPRA2_03 ] )
        , ( ( OPRA2_S6 , OPRA2_24 )
          , Set.fromList [ OPRA2_04 ] )
        , ( ( OPRA2_S6 , OPRA2_25 )
          , Set.fromList [ OPRA2_05 ] )
        , ( ( OPRA2_S6 , OPRA2_26 )
          , Set.fromList [ OPRA2_06 ] )
        , ( ( OPRA2_S6 , OPRA2_27 )
          , Set.fromList [ OPRA2_07 ] )
        , ( ( OPRA2_S6 , OPRA2_30 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_S6 , OPRA2_31 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_S6 , OPRA2_32 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_S6 , OPRA2_33 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_S6 , OPRA2_34 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_S6 , OPRA2_35 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_S6 , OPRA2_36 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_S6 , OPRA2_37 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_S6 , OPRA2_40 )
          , Set.fromList [ OPRA2_20 ] )
        , ( ( OPRA2_S6 , OPRA2_41 )
          , Set.fromList [ OPRA2_21 ] )
        , ( ( OPRA2_S6 , OPRA2_42 )
          , Set.fromList [ OPRA2_22 ] )
        , ( ( OPRA2_S6 , OPRA2_43 )
          , Set.fromList [ OPRA2_23 ] )
        , ( ( OPRA2_S6 , OPRA2_44 )
          , Set.fromList [ OPRA2_24 ] )
        , ( ( OPRA2_S6 , OPRA2_45 )
          , Set.fromList [ OPRA2_25 ] )
        , ( ( OPRA2_S6 , OPRA2_46 )
          , Set.fromList [ OPRA2_26 ] )
        , ( ( OPRA2_S6 , OPRA2_47 )
          , Set.fromList [ OPRA2_27 ] )
        , ( ( OPRA2_S6 , OPRA2_50 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_S6 , OPRA2_51 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_S6 , OPRA2_52 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_S6 , OPRA2_53 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_S6 , OPRA2_54 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_S6 , OPRA2_55 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_S6 , OPRA2_56 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_S6 , OPRA2_57 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_S6 , OPRA2_60 )
          , Set.fromList [ OPRA2_40 ] )
        , ( ( OPRA2_S6 , OPRA2_61 )
          , Set.fromList [ OPRA2_41 ] )
        , ( ( OPRA2_S6 , OPRA2_62 )
          , Set.fromList [ OPRA2_42 ] )
        , ( ( OPRA2_S6 , OPRA2_63 )
          , Set.fromList [ OPRA2_43 ] )
        , ( ( OPRA2_S6 , OPRA2_64 )
          , Set.fromList [ OPRA2_44 ] )
        , ( ( OPRA2_S6 , OPRA2_65 )
          , Set.fromList [ OPRA2_45 ] )
        , ( ( OPRA2_S6 , OPRA2_66 )
          , Set.fromList [ OPRA2_46 ] )
        , ( ( OPRA2_S6 , OPRA2_67 )
          , Set.fromList [ OPRA2_47 ] )
        , ( ( OPRA2_S6 , OPRA2_70 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_S6 , OPRA2_71 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_S6 , OPRA2_72 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_S6 , OPRA2_73 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_S6 , OPRA2_74 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_S6 , OPRA2_75 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_S6 , OPRA2_76 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_S6 , OPRA2_77 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_S6 , OPRA2_S0 )
          , Set.fromList [ OPRA2_S6 ] )
        , ( ( OPRA2_S6 , OPRA2_S1 )
          , Set.fromList [ OPRA2_S7 ] )
        , ( ( OPRA2_S6 , OPRA2_S2 )
          , Set.fromList [ OPRA2_S0 ] )
        , ( ( OPRA2_S6 , OPRA2_S3 )
          , Set.fromList [ OPRA2_S1 ] )
        , ( ( OPRA2_S6 , OPRA2_S4 )
          , Set.fromList [ OPRA2_S2 ] )
        , ( ( OPRA2_S6 , OPRA2_S5 )
          , Set.fromList [ OPRA2_S3 ] )
        , ( ( OPRA2_S6 , OPRA2_S6 )
          , Set.fromList [ OPRA2_S4 ] )
        , ( ( OPRA2_S6 , OPRA2_S7 )
          , Set.fromList [ OPRA2_S5 ] )
        , ( ( OPRA2_S7 , OPRA2_00 )
          , Set.fromList [ OPRA2_70 ] )
        , ( ( OPRA2_S7 , OPRA2_01 )
          , Set.fromList [ OPRA2_71 ] )
        , ( ( OPRA2_S7 , OPRA2_02 )
          , Set.fromList [ OPRA2_72 ] )
        , ( ( OPRA2_S7 , OPRA2_03 )
          , Set.fromList [ OPRA2_73 ] )
        , ( ( OPRA2_S7 , OPRA2_04 )
          , Set.fromList [ OPRA2_74 ] )
        , ( ( OPRA2_S7 , OPRA2_05 )
          , Set.fromList [ OPRA2_75 ] )
        , ( ( OPRA2_S7 , OPRA2_06 )
          , Set.fromList [ OPRA2_76 ] )
        , ( ( OPRA2_S7 , OPRA2_07 )
          , Set.fromList [ OPRA2_77 ] )
        , ( ( OPRA2_S7 , OPRA2_10 )
          , Set.fromList [ OPRA2_70, OPRA2_10, OPRA2_00 ] )
        , ( ( OPRA2_S7 , OPRA2_11 )
          , Set.fromList [ OPRA2_71, OPRA2_11, OPRA2_01 ] )
        , ( ( OPRA2_S7 , OPRA2_12 )
          , Set.fromList [ OPRA2_72, OPRA2_12, OPRA2_02 ] )
        , ( ( OPRA2_S7 , OPRA2_13 )
          , Set.fromList [ OPRA2_73, OPRA2_13, OPRA2_03 ] )
        , ( ( OPRA2_S7 , OPRA2_14 )
          , Set.fromList [ OPRA2_74, OPRA2_14, OPRA2_04 ] )
        , ( ( OPRA2_S7 , OPRA2_15 )
          , Set.fromList [ OPRA2_75, OPRA2_15, OPRA2_05 ] )
        , ( ( OPRA2_S7 , OPRA2_16 )
          , Set.fromList [ OPRA2_76, OPRA2_16, OPRA2_06 ] )
        , ( ( OPRA2_S7 , OPRA2_17 )
          , Set.fromList [ OPRA2_77, OPRA2_17, OPRA2_07 ] )
        , ( ( OPRA2_S7 , OPRA2_20 )
          , Set.fromList [ OPRA2_10 ] )
        , ( ( OPRA2_S7 , OPRA2_21 )
          , Set.fromList [ OPRA2_11 ] )
        , ( ( OPRA2_S7 , OPRA2_22 )
          , Set.fromList [ OPRA2_12 ] )
        , ( ( OPRA2_S7 , OPRA2_23 )
          , Set.fromList [ OPRA2_13 ] )
        , ( ( OPRA2_S7 , OPRA2_24 )
          , Set.fromList [ OPRA2_14 ] )
        , ( ( OPRA2_S7 , OPRA2_25 )
          , Set.fromList [ OPRA2_15 ] )
        , ( ( OPRA2_S7 , OPRA2_26 )
          , Set.fromList [ OPRA2_16 ] )
        , ( ( OPRA2_S7 , OPRA2_27 )
          , Set.fromList [ OPRA2_17 ] )
        , ( ( OPRA2_S7 , OPRA2_30 )
          , Set.fromList [ OPRA2_30, OPRA2_20, OPRA2_10 ] )
        , ( ( OPRA2_S7 , OPRA2_31 )
          , Set.fromList [ OPRA2_31, OPRA2_21, OPRA2_11 ] )
        , ( ( OPRA2_S7 , OPRA2_32 )
          , Set.fromList [ OPRA2_32, OPRA2_22, OPRA2_12 ] )
        , ( ( OPRA2_S7 , OPRA2_33 )
          , Set.fromList [ OPRA2_33, OPRA2_23, OPRA2_13 ] )
        , ( ( OPRA2_S7 , OPRA2_34 )
          , Set.fromList [ OPRA2_34, OPRA2_24, OPRA2_14 ] )
        , ( ( OPRA2_S7 , OPRA2_35 )
          , Set.fromList [ OPRA2_35, OPRA2_25, OPRA2_15 ] )
        , ( ( OPRA2_S7 , OPRA2_36 )
          , Set.fromList [ OPRA2_36, OPRA2_26, OPRA2_16 ] )
        , ( ( OPRA2_S7 , OPRA2_37 )
          , Set.fromList [ OPRA2_37, OPRA2_27, OPRA2_17 ] )
        , ( ( OPRA2_S7 , OPRA2_40 )
          , Set.fromList [ OPRA2_30 ] )
        , ( ( OPRA2_S7 , OPRA2_41 )
          , Set.fromList [ OPRA2_31 ] )
        , ( ( OPRA2_S7 , OPRA2_42 )
          , Set.fromList [ OPRA2_32 ] )
        , ( ( OPRA2_S7 , OPRA2_43 )
          , Set.fromList [ OPRA2_33 ] )
        , ( ( OPRA2_S7 , OPRA2_44 )
          , Set.fromList [ OPRA2_34 ] )
        , ( ( OPRA2_S7 , OPRA2_45 )
          , Set.fromList [ OPRA2_35 ] )
        , ( ( OPRA2_S7 , OPRA2_46 )
          , Set.fromList [ OPRA2_36 ] )
        , ( ( OPRA2_S7 , OPRA2_47 )
          , Set.fromList [ OPRA2_37 ] )
        , ( ( OPRA2_S7 , OPRA2_50 )
          , Set.fromList [ OPRA2_50, OPRA2_40, OPRA2_30 ] )
        , ( ( OPRA2_S7 , OPRA2_51 )
          , Set.fromList [ OPRA2_51, OPRA2_41, OPRA2_31 ] )
        , ( ( OPRA2_S7 , OPRA2_52 )
          , Set.fromList [ OPRA2_52, OPRA2_42, OPRA2_32 ] )
        , ( ( OPRA2_S7 , OPRA2_53 )
          , Set.fromList [ OPRA2_53, OPRA2_43, OPRA2_33 ] )
        , ( ( OPRA2_S7 , OPRA2_54 )
          , Set.fromList [ OPRA2_54, OPRA2_44, OPRA2_34 ] )
        , ( ( OPRA2_S7 , OPRA2_55 )
          , Set.fromList [ OPRA2_55, OPRA2_45, OPRA2_35 ] )
        , ( ( OPRA2_S7 , OPRA2_56 )
          , Set.fromList [ OPRA2_56, OPRA2_46, OPRA2_36 ] )
        , ( ( OPRA2_S7 , OPRA2_57 )
          , Set.fromList [ OPRA2_57, OPRA2_47, OPRA2_37 ] )
        , ( ( OPRA2_S7 , OPRA2_60 )
          , Set.fromList [ OPRA2_50 ] )
        , ( ( OPRA2_S7 , OPRA2_61 )
          , Set.fromList [ OPRA2_51 ] )
        , ( ( OPRA2_S7 , OPRA2_62 )
          , Set.fromList [ OPRA2_52 ] )
        , ( ( OPRA2_S7 , OPRA2_63 )
          , Set.fromList [ OPRA2_53 ] )
        , ( ( OPRA2_S7 , OPRA2_64 )
          , Set.fromList [ OPRA2_54 ] )
        , ( ( OPRA2_S7 , OPRA2_65 )
          , Set.fromList [ OPRA2_55 ] )
        , ( ( OPRA2_S7 , OPRA2_66 )
          , Set.fromList [ OPRA2_56 ] )
        , ( ( OPRA2_S7 , OPRA2_67 )
          , Set.fromList [ OPRA2_57 ] )
        , ( ( OPRA2_S7 , OPRA2_70 )
          , Set.fromList [ OPRA2_70, OPRA2_60, OPRA2_50 ] )
        , ( ( OPRA2_S7 , OPRA2_71 )
          , Set.fromList [ OPRA2_71, OPRA2_61, OPRA2_51 ] )
        , ( ( OPRA2_S7 , OPRA2_72 )
          , Set.fromList [ OPRA2_72, OPRA2_62, OPRA2_52 ] )
        , ( ( OPRA2_S7 , OPRA2_73 )
          , Set.fromList [ OPRA2_73, OPRA2_63, OPRA2_53 ] )
        , ( ( OPRA2_S7 , OPRA2_74 )
          , Set.fromList [ OPRA2_74, OPRA2_64, OPRA2_54 ] )
        , ( ( OPRA2_S7 , OPRA2_75 )
          , Set.fromList [ OPRA2_75, OPRA2_65, OPRA2_55 ] )
        , ( ( OPRA2_S7 , OPRA2_76 )
          , Set.fromList [ OPRA2_76, OPRA2_66, OPRA2_56 ] )
        , ( ( OPRA2_S7 , OPRA2_77 )
          , Set.fromList [ OPRA2_77, OPRA2_67, OPRA2_57 ] )
        , ( ( OPRA2_S7 , OPRA2_S0 )
          , Set.fromList [ OPRA2_S7 ] )
        , ( ( OPRA2_S7 , OPRA2_S1 )
          , Set.fromList [ OPRA2_S7, OPRA2_S1, OPRA2_S0 ] )
        , ( ( OPRA2_S7 , OPRA2_S2 )
          , Set.fromList [ OPRA2_S1 ] )
        , ( ( OPRA2_S7 , OPRA2_S3 )
          , Set.fromList [ OPRA2_S3, OPRA2_S2, OPRA2_S1 ] )
        , ( ( OPRA2_S7 , OPRA2_S4 )
          , Set.fromList [ OPRA2_S3 ] )
        , ( ( OPRA2_S7 , OPRA2_S5 )
          , Set.fromList [ OPRA2_S5, OPRA2_S4, OPRA2_S3 ] )
        , ( ( OPRA2_S7 , OPRA2_S6 )
          , Set.fromList [ OPRA2_S5 ] )
        , ( ( OPRA2_S7 , OPRA2_S7 )
          , Set.fromList [ OPRA2_S7, OPRA2_S6, OPRA2_S5 ] )
        ]
