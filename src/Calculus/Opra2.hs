module Calculus.Opra2 where

-- standard modules
import qualified Data.Char as Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Helpful
import Calculus.Opra

data Opra2 = Opra2_0_0 | Opra2_0_1 | Opra2_0_2 | Opra2_0_3
           | Opra2_0_4 | Opra2_0_5 | Opra2_0_6 | Opra2_0_7
           | Opra2_1_0 | Opra2_1_1 | Opra2_1_2 | Opra2_1_3
           | Opra2_1_4 | Opra2_1_5 | Opra2_1_6 | Opra2_1_7
           | Opra2_2_0 | Opra2_2_1 | Opra2_2_2 | Opra2_2_3
           | Opra2_2_4 | Opra2_2_5 | Opra2_2_6 | Opra2_2_7
           | Opra2_3_0 | Opra2_3_1 | Opra2_3_2 | Opra2_3_3
           | Opra2_3_4 | Opra2_3_5 | Opra2_3_6 | Opra2_3_7
           | Opra2_4_0 | Opra2_4_1 | Opra2_4_2 | Opra2_4_3
           | Opra2_4_4 | Opra2_4_5 | Opra2_4_6 | Opra2_4_7
           | Opra2_5_0 | Opra2_5_1 | Opra2_5_2 | Opra2_5_3
           | Opra2_5_4 | Opra2_5_5 | Opra2_5_6 | Opra2_5_7
           | Opra2_6_0 | Opra2_6_1 | Opra2_6_2 | Opra2_6_3
           | Opra2_6_4 | Opra2_6_5 | Opra2_6_6 | Opra2_6_7
           | Opra2_7_0 | Opra2_7_1 | Opra2_7_2 | Opra2_7_3
           | Opra2_7_4 | Opra2_7_5 | Opra2_7_6 | Opra2_7_7
           | Opra2_s_0 | Opra2_s_1 | Opra2_s_2 | Opra2_s_3
           | Opra2_s_4 | Opra2_s_5 | Opra2_s_6 | Opra2_s_7
           deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Opram Opra2 where
    m _ = 2

instance Calculus Opra2 where
    rank _ = 2
    readRel = readOpram
    showRel = showOpram
    sparqifyRel = sparqifyOpram
    gqrifyRel   = sparqifyOpram
    cBaserelationsArealList = filter
        (\ a -> ( \(b, _:c) -> all odd $ map read [b,c]) $ break (== '_') $
            map (\x -> if x == 's' then '1' else x) $ drop 6 $ show a
        ) cBaserelationsList

instance BinaryCalculus Opra2 where
    bcIdentity = Opra2_s_0

    bcConversion = Map.fromList
        [ ( Opra2_0_0 , Set.singleton Opra2_0_0 )
        , ( Opra2_0_1 , Set.singleton Opra2_1_0 )
        , ( Opra2_0_2 , Set.singleton Opra2_2_0 )
        , ( Opra2_0_3 , Set.singleton Opra2_3_0 )
        , ( Opra2_0_4 , Set.singleton Opra2_4_0 )
        , ( Opra2_0_5 , Set.singleton Opra2_5_0 )
        , ( Opra2_0_6 , Set.singleton Opra2_6_0 )
        , ( Opra2_0_7 , Set.singleton Opra2_7_0 )
        , ( Opra2_1_0 , Set.singleton Opra2_0_1 )
        , ( Opra2_1_1 , Set.singleton Opra2_1_1 )
        , ( Opra2_1_2 , Set.singleton Opra2_2_1 )
        , ( Opra2_1_3 , Set.singleton Opra2_3_1 )
        , ( Opra2_1_4 , Set.singleton Opra2_4_1 )
        , ( Opra2_1_5 , Set.singleton Opra2_5_1 )
        , ( Opra2_1_6 , Set.singleton Opra2_6_1 )
        , ( Opra2_1_7 , Set.singleton Opra2_7_1 )
        , ( Opra2_2_0 , Set.singleton Opra2_0_2 )
        , ( Opra2_2_1 , Set.singleton Opra2_1_2 )
        , ( Opra2_2_2 , Set.singleton Opra2_2_2 )
        , ( Opra2_2_3 , Set.singleton Opra2_3_2 )
        , ( Opra2_2_4 , Set.singleton Opra2_4_2 )
        , ( Opra2_2_5 , Set.singleton Opra2_5_2 )
        , ( Opra2_2_6 , Set.singleton Opra2_6_2 )
        , ( Opra2_2_7 , Set.singleton Opra2_7_2 )
        , ( Opra2_3_0 , Set.singleton Opra2_0_3 )
        , ( Opra2_3_1 , Set.singleton Opra2_1_3 )
        , ( Opra2_3_2 , Set.singleton Opra2_2_3 )
        , ( Opra2_3_3 , Set.singleton Opra2_3_3 )
        , ( Opra2_3_4 , Set.singleton Opra2_4_3 )
        , ( Opra2_3_5 , Set.singleton Opra2_5_3 )
        , ( Opra2_3_6 , Set.singleton Opra2_6_3 )
        , ( Opra2_3_7 , Set.singleton Opra2_7_3 )
        , ( Opra2_4_0 , Set.singleton Opra2_0_4 )
        , ( Opra2_4_1 , Set.singleton Opra2_1_4 )
        , ( Opra2_4_2 , Set.singleton Opra2_2_4 )
        , ( Opra2_4_3 , Set.singleton Opra2_3_4 )
        , ( Opra2_4_4 , Set.singleton Opra2_4_4 )
        , ( Opra2_4_5 , Set.singleton Opra2_5_4 )
        , ( Opra2_4_6 , Set.singleton Opra2_6_4 )
        , ( Opra2_4_7 , Set.singleton Opra2_7_4 )
        , ( Opra2_5_0 , Set.singleton Opra2_0_5 )
        , ( Opra2_5_1 , Set.singleton Opra2_1_5 )
        , ( Opra2_5_2 , Set.singleton Opra2_2_5 )
        , ( Opra2_5_3 , Set.singleton Opra2_3_5 )
        , ( Opra2_5_4 , Set.singleton Opra2_4_5 )
        , ( Opra2_5_5 , Set.singleton Opra2_5_5 )
        , ( Opra2_5_6 , Set.singleton Opra2_6_5 )
        , ( Opra2_5_7 , Set.singleton Opra2_7_5 )
        , ( Opra2_6_0 , Set.singleton Opra2_0_6 )
        , ( Opra2_6_1 , Set.singleton Opra2_1_6 )
        , ( Opra2_6_2 , Set.singleton Opra2_2_6 )
        , ( Opra2_6_3 , Set.singleton Opra2_3_6 )
        , ( Opra2_6_4 , Set.singleton Opra2_4_6 )
        , ( Opra2_6_5 , Set.singleton Opra2_5_6 )
        , ( Opra2_6_6 , Set.singleton Opra2_6_6 )
        , ( Opra2_6_7 , Set.singleton Opra2_7_6 )
        , ( Opra2_7_0 , Set.singleton Opra2_0_7 )
        , ( Opra2_7_1 , Set.singleton Opra2_1_7 )
        , ( Opra2_7_2 , Set.singleton Opra2_2_7 )
        , ( Opra2_7_3 , Set.singleton Opra2_3_7 )
        , ( Opra2_7_4 , Set.singleton Opra2_4_7 )
        , ( Opra2_7_5 , Set.singleton Opra2_5_7 )
        , ( Opra2_7_6 , Set.singleton Opra2_6_7 )
        , ( Opra2_7_7 , Set.singleton Opra2_7_7 )
        , ( Opra2_s_0 , Set.singleton Opra2_s_0 )
        , ( Opra2_s_1 , Set.singleton Opra2_s_7 )
        , ( Opra2_s_2 , Set.singleton Opra2_s_6 )
        , ( Opra2_s_3 , Set.singleton Opra2_s_5 )
        , ( Opra2_s_4 , Set.singleton Opra2_s_4 )
        , ( Opra2_s_5 , Set.singleton Opra2_s_3 )
        , ( Opra2_s_6 , Set.singleton Opra2_s_2 )
        , ( Opra2_s_7 , Set.singleton Opra2_s_1 ) ]


    bcComposition = Map.fromList
        [ ( ( Opra2_0_0 , Opra2_0_0 )
          , Set.fromList [ Opra2_2_0, Opra2_0_2, Opra2_s_0 ] )
        , ( ( Opra2_s_3 , Opra2_s_3 )
          , Set.fromList [ Opra2_s_3, Opra2_s_2, Opra2_s_1 ] )
        ]


        [ ( ( Opra2_0_0 , Opra2_0_0 )
          , Set.fromList [ Opra2_4_0, Opra2_0_4, Opra2_s_0 ] )
        , ( ( Opra2_0_0 , Opra2_0_1 )
          , Set.fromList [ Opra2_s_7, Opra2_4_1, Opra2_0_5 ] )
        , ( ( Opra2_0_0 , Opra2_0_2 )
          , Set.fromList [ Opra2_s_6, Opra2_4_2, Opra2_0_6 ] )
        , ( ( Opra2_0_0 , Opra2_0_3 )
          , Set.fromList [ Opra2_s_5, Opra2_4_3, Opra2_0_7 ] )
        , ( ( Opra2_0_0 , Opra2_0_4 )
          , Set.fromList [ Opra2_4_4, Opra2_s_4, Opra2_0_0 ] )
        , ( ( Opra2_0_0 , Opra2_0_5 )
          , Set.fromList [ Opra2_4_5, Opra2_s_3, Opra2_0_1 ] )
        , ( ( Opra2_0_0 , Opra2_0_6 )
          , Set.fromList [ Opra2_4_6, Opra2_s_2, Opra2_0_2 ] )
        , ( ( Opra2_0_0 , Opra2_0_7 )
          , Set.fromList [ Opra2_4_7, Opra2_s_1, Opra2_0_3 ] )
        , ( ( Opra2_0_0 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1 ] )
        , ( ( Opra2_0_0 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_0_0 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3 ] )
        , ( ( Opra2_0_0 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_0_0 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5 ] )
        , ( ( Opra2_0_0 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_0_0 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7 ] )
        , ( ( Opra2_0_0 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_0_0 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_0_0 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_0_0 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_0_0 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_0_0 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_0_0 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_0_0 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_0_0 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_0_0 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_0_0 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_0_0 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_0_0 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_0_0 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_0_0 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_0_0 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_0_0 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_0_0 , Opra2_4_0 )
          , Set.fromList [ Opra2_0_0 ] )
        , ( ( Opra2_0_0 , Opra2_4_1 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_0_0 , Opra2_4_2 )
          , Set.fromList [ Opra2_0_2 ] )
        , ( ( Opra2_0_0 , Opra2_4_3 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_0_0 , Opra2_4_4 )
          , Set.fromList [ Opra2_0_4 ] )
        , ( ( Opra2_0_0 , Opra2_4_5 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_0_0 , Opra2_4_6 )
          , Set.fromList [ Opra2_0_6 ] )
        , ( ( Opra2_0_0 , Opra2_4_7 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_0_0 , Opra2_5_0 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_0_0 , Opra2_5_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_0 , Opra2_5_2 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_0_0 , Opra2_5_3 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_0 , Opra2_5_4 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_0_0 , Opra2_5_5 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_0 , Opra2_5_6 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_0_0 , Opra2_5_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_0_0 , Opra2_6_0 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_0_0 , Opra2_6_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_0 , Opra2_6_2 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_0_0 , Opra2_6_3 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_0 , Opra2_6_4 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_0_0 , Opra2_6_5 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_0 , Opra2_6_6 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_0_0 , Opra2_6_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_0_0 , Opra2_7_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_0_0 , Opra2_7_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_0 , Opra2_7_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_0_0 , Opra2_7_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_0 , Opra2_7_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_0_0 , Opra2_7_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_0 , Opra2_7_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_0_0 , Opra2_7_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_0 , Opra2_s_0 )
          , Set.fromList [ Opra2_0_0 ] )
        , ( ( Opra2_0_0 , Opra2_s_1 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_0_0 , Opra2_s_2 )
          , Set.fromList [ Opra2_0_6 ] )
        , ( ( Opra2_0_0 , Opra2_s_3 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_0_0 , Opra2_s_4 )
          , Set.fromList [ Opra2_0_4 ] )
        , ( ( Opra2_0_0 , Opra2_s_5 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_0_0 , Opra2_s_6 )
          , Set.fromList [ Opra2_0_2 ] )
        , ( ( Opra2_0_0 , Opra2_s_7 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_0_1 , Opra2_0_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_0_1 , Opra2_0_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_1 , Opra2_0_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_0_1 , Opra2_0_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_1 , Opra2_0_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_0_1 , Opra2_0_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_1 , Opra2_0_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_0_1 , Opra2_0_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_1 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_4_0, Opra2_3_7, Opra2_2_7
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_0_4
                         , Opra2_s_0 ] )
        , ( ( Opra2_0_1 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_5 ] )
        , ( ( Opra2_0_1 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_s_6, Opra2_5_3, Opra2_4_2, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_6 ] )
        , ( ( Opra2_0_1 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_3, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_0_1 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_4_4, Opra2_s_4, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_0_1 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_0_1 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_4_6, Opra2_3_5, Opra2_2_5
                         , Opra2_s_2, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_2 ] )
        , ( ( Opra2_0_1 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_0_1 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1 ] )
        , ( ( Opra2_0_1 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_0_1 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3 ] )
        , ( ( Opra2_0_1 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_0_1 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5 ] )
        , ( ( Opra2_0_1 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_0_1 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7 ] )
        , ( ( Opra2_0_1 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_0_1 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1 ] )
        , ( ( Opra2_0_1 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_0_1 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3 ] )
        , ( ( Opra2_0_1 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_0_1 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5 ] )
        , ( ( Opra2_0_1 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_0_1 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7 ] )
        , ( ( Opra2_0_1 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_0_1 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_0_1 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_0_1 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_0_1 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_0_1 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_0_1 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_0_1 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_0_1 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_0_1 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_7, Opra2_0_0 ] )
        , ( ( Opra2_0_1 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_1 ] )
        , ( ( Opra2_0_1 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_1, Opra2_0_2 ] )
        , ( ( Opra2_0_1 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3 ] )
        , ( ( Opra2_0_1 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_3, Opra2_0_4 ] )
        , ( ( Opra2_0_1 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5 ] )
        , ( ( Opra2_0_1 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_5, Opra2_0_6 ] )
        , ( ( Opra2_0_1 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7 ] )
        , ( ( Opra2_0_1 , Opra2_6_0 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_0_1 , Opra2_6_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_1 , Opra2_6_2 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_0_1 , Opra2_6_3 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_1 , Opra2_6_4 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_0_1 , Opra2_6_5 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_1 , Opra2_6_6 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_0_1 , Opra2_6_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_0_1 , Opra2_7_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_0_1 , Opra2_7_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_1 , Opra2_7_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_0_1 , Opra2_7_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_1 , Opra2_7_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_0_1 , Opra2_7_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_1 , Opra2_7_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_0_1 , Opra2_7_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_1 , Opra2_s_0 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_0_1 , Opra2_s_1 )
          , Set.fromList [ Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_0_1 , Opra2_s_2 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_0_1 , Opra2_s_3 )
          , Set.fromList [ Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_0_1 , Opra2_s_4 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_0_1 , Opra2_s_5 )
          , Set.fromList [ Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_0_1 , Opra2_s_6 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_0_1 , Opra2_s_7 )
          , Set.fromList [ Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_0_2 , Opra2_0_0 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_0_2 , Opra2_0_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_2 , Opra2_0_2 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_0_2 , Opra2_0_3 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_2 , Opra2_0_4 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_0_2 , Opra2_0_5 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_2 , Opra2_0_6 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_0_2 , Opra2_0_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_0_2 , Opra2_1_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_0_2 , Opra2_1_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_2 , Opra2_1_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_0_2 , Opra2_1_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_2 , Opra2_1_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_0_2 , Opra2_1_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_2 , Opra2_1_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_0_2 , Opra2_1_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_2 , Opra2_2_0 )
          , Set.fromList [ Opra2_4_0, Opra2_0_4, Opra2_s_0 ] )
        , ( ( Opra2_0_2 , Opra2_2_1 )
          , Set.fromList [ Opra2_s_7, Opra2_4_1, Opra2_0_5 ] )
        , ( ( Opra2_0_2 , Opra2_2_2 )
          , Set.fromList [ Opra2_s_6, Opra2_4_2, Opra2_0_6 ] )
        , ( ( Opra2_0_2 , Opra2_2_3 )
          , Set.fromList [ Opra2_s_5, Opra2_4_3, Opra2_0_7 ] )
        , ( ( Opra2_0_2 , Opra2_2_4 )
          , Set.fromList [ Opra2_4_4, Opra2_s_4, Opra2_0_0 ] )
        , ( ( Opra2_0_2 , Opra2_2_5 )
          , Set.fromList [ Opra2_4_5, Opra2_s_3, Opra2_0_1 ] )
        , ( ( Opra2_0_2 , Opra2_2_6 )
          , Set.fromList [ Opra2_4_6, Opra2_s_2, Opra2_0_2 ] )
        , ( ( Opra2_0_2 , Opra2_2_7 )
          , Set.fromList [ Opra2_4_7, Opra2_s_1, Opra2_0_3 ] )
        , ( ( Opra2_0_2 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1 ] )
        , ( ( Opra2_0_2 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_0_2 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3 ] )
        , ( ( Opra2_0_2 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_0_2 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5 ] )
        , ( ( Opra2_0_2 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_0_2 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7 ] )
        , ( ( Opra2_0_2 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_0_2 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_0_2 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_0_2 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_0_2 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_0_2 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_0_2 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_0_2 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_0_2 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_0_2 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_0_2 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_0_2 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_0_2 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_0_2 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_0_2 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_0_2 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_0_2 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_0_2 , Opra2_6_0 )
          , Set.fromList [ Opra2_0_0 ] )
        , ( ( Opra2_0_2 , Opra2_6_1 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_0_2 , Opra2_6_2 )
          , Set.fromList [ Opra2_0_2 ] )
        , ( ( Opra2_0_2 , Opra2_6_3 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_0_2 , Opra2_6_4 )
          , Set.fromList [ Opra2_0_4 ] )
        , ( ( Opra2_0_2 , Opra2_6_5 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_0_2 , Opra2_6_6 )
          , Set.fromList [ Opra2_0_6 ] )
        , ( ( Opra2_0_2 , Opra2_6_7 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_0_2 , Opra2_7_0 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_0_2 , Opra2_7_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_2 , Opra2_7_2 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_0_2 , Opra2_7_3 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_2 , Opra2_7_4 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_0_2 , Opra2_7_5 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_2 , Opra2_7_6 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_0_2 , Opra2_7_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_0_2 , Opra2_s_0 )
          , Set.fromList [ Opra2_0_2 ] )
        , ( ( Opra2_0_2 , Opra2_s_1 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_0_2 , Opra2_s_2 )
          , Set.fromList [ Opra2_0_0 ] )
        , ( ( Opra2_0_2 , Opra2_s_3 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_0_2 , Opra2_s_4 )
          , Set.fromList [ Opra2_0_6 ] )
        , ( ( Opra2_0_2 , Opra2_s_5 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_0_2 , Opra2_s_6 )
          , Set.fromList [ Opra2_0_4 ] )
        , ( ( Opra2_0_2 , Opra2_s_7 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_0_3 , Opra2_0_0 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_0_3 , Opra2_0_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_3 , Opra2_0_2 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_0_3 , Opra2_0_3 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_3 , Opra2_0_4 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_0_3 , Opra2_0_5 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_3 , Opra2_0_6 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_0_3 , Opra2_0_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_0_3 , Opra2_1_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_0_3 , Opra2_1_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_3 , Opra2_1_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_0_3 , Opra2_1_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_3 , Opra2_1_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_0_3 , Opra2_1_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_3 , Opra2_1_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_0_3 , Opra2_1_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_3 , Opra2_2_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_0_3 , Opra2_2_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_3 , Opra2_2_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_0_3 , Opra2_2_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_3 , Opra2_2_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_0_3 , Opra2_2_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_3 , Opra2_2_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_0_3 , Opra2_2_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_3 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_4_0, Opra2_3_7, Opra2_2_7
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_0_4
                         , Opra2_s_0 ] )
        , ( ( Opra2_0_3 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_5 ] )
        , ( ( Opra2_0_3 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_s_6, Opra2_5_3, Opra2_4_2, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_6 ] )
        , ( ( Opra2_0_3 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_3, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_0_3 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_4_4, Opra2_s_4, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_0_3 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_0_3 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_4_6, Opra2_3_5, Opra2_2_5
                         , Opra2_s_2, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_2 ] )
        , ( ( Opra2_0_3 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_0_3 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1 ] )
        , ( ( Opra2_0_3 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_0_3 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3 ] )
        , ( ( Opra2_0_3 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_0_3 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5 ] )
        , ( ( Opra2_0_3 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_0_3 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7 ] )
        , ( ( Opra2_0_3 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_0_3 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1 ] )
        , ( ( Opra2_0_3 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_0_3 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3 ] )
        , ( ( Opra2_0_3 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_0_3 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5 ] )
        , ( ( Opra2_0_3 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_0_3 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7 ] )
        , ( ( Opra2_0_3 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_0_3 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_0_3 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_0_3 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_0_3 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_0_3 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_0_3 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_0_3 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_0_3 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_0_3 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_7, Opra2_0_0 ] )
        , ( ( Opra2_0_3 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_1 ] )
        , ( ( Opra2_0_3 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_1, Opra2_0_2 ] )
        , ( ( Opra2_0_3 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3 ] )
        , ( ( Opra2_0_3 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_3, Opra2_0_4 ] )
        , ( ( Opra2_0_3 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5 ] )
        , ( ( Opra2_0_3 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_5, Opra2_0_6 ] )
        , ( ( Opra2_0_3 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7 ] )
        , ( ( Opra2_0_3 , Opra2_s_0 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_0_3 , Opra2_s_1 )
          , Set.fromList [ Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_0_3 , Opra2_s_2 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_0_3 , Opra2_s_3 )
          , Set.fromList [ Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_0_3 , Opra2_s_4 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_0_3 , Opra2_s_5 )
          , Set.fromList [ Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_0_3 , Opra2_s_6 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_0_3 , Opra2_s_7 )
          , Set.fromList [ Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_0_4 , Opra2_0_0 )
          , Set.fromList [ Opra2_0_0 ] )
        , ( ( Opra2_0_4 , Opra2_0_1 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_0_4 , Opra2_0_2 )
          , Set.fromList [ Opra2_0_2 ] )
        , ( ( Opra2_0_4 , Opra2_0_3 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_0_4 , Opra2_0_4 )
          , Set.fromList [ Opra2_0_4 ] )
        , ( ( Opra2_0_4 , Opra2_0_5 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_0_4 , Opra2_0_6 )
          , Set.fromList [ Opra2_0_6 ] )
        , ( ( Opra2_0_4 , Opra2_0_7 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_0_4 , Opra2_1_0 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_0_4 , Opra2_1_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_4 , Opra2_1_2 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_0_4 , Opra2_1_3 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_4 , Opra2_1_4 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_0_4 , Opra2_1_5 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_4 , Opra2_1_6 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_0_4 , Opra2_1_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_0_4 , Opra2_2_0 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_0_4 , Opra2_2_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_4 , Opra2_2_2 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_0_4 , Opra2_2_3 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_4 , Opra2_2_4 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_0_4 , Opra2_2_5 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_4 , Opra2_2_6 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_0_4 , Opra2_2_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_0_4 , Opra2_3_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_0_4 , Opra2_3_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_4 , Opra2_3_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_0_4 , Opra2_3_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_4 , Opra2_3_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_0_4 , Opra2_3_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_4 , Opra2_3_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_0_4 , Opra2_3_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_4 , Opra2_4_0 )
          , Set.fromList [ Opra2_4_0, Opra2_0_4, Opra2_s_0 ] )
        , ( ( Opra2_0_4 , Opra2_4_1 )
          , Set.fromList [ Opra2_s_7, Opra2_4_1, Opra2_0_5 ] )
        , ( ( Opra2_0_4 , Opra2_4_2 )
          , Set.fromList [ Opra2_s_6, Opra2_4_2, Opra2_0_6 ] )
        , ( ( Opra2_0_4 , Opra2_4_3 )
          , Set.fromList [ Opra2_s_5, Opra2_4_3, Opra2_0_7 ] )
        , ( ( Opra2_0_4 , Opra2_4_4 )
          , Set.fromList [ Opra2_4_4, Opra2_s_4, Opra2_0_0 ] )
        , ( ( Opra2_0_4 , Opra2_4_5 )
          , Set.fromList [ Opra2_4_5, Opra2_s_3, Opra2_0_1 ] )
        , ( ( Opra2_0_4 , Opra2_4_6 )
          , Set.fromList [ Opra2_4_6, Opra2_s_2, Opra2_0_2 ] )
        , ( ( Opra2_0_4 , Opra2_4_7 )
          , Set.fromList [ Opra2_4_7, Opra2_s_1, Opra2_0_3 ] )
        , ( ( Opra2_0_4 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1 ] )
        , ( ( Opra2_0_4 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_0_4 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3 ] )
        , ( ( Opra2_0_4 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_0_4 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5 ] )
        , ( ( Opra2_0_4 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_0_4 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7 ] )
        , ( ( Opra2_0_4 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_0_4 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_0_4 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_0_4 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_0_4 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_0_4 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_0_4 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_0_4 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_0_4 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_0_4 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_0_4 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_0_4 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_0_4 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_0_4 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_0_4 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_0_4 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_0_4 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_0_4 , Opra2_s_0 )
          , Set.fromList [ Opra2_0_4 ] )
        , ( ( Opra2_0_4 , Opra2_s_1 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_0_4 , Opra2_s_2 )
          , Set.fromList [ Opra2_0_2 ] )
        , ( ( Opra2_0_4 , Opra2_s_3 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_0_4 , Opra2_s_4 )
          , Set.fromList [ Opra2_0_0 ] )
        , ( ( Opra2_0_4 , Opra2_s_5 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_0_4 , Opra2_s_6 )
          , Set.fromList [ Opra2_0_6 ] )
        , ( ( Opra2_0_4 , Opra2_s_7 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_0_5 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_0_5 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_0_5 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_0_5 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_0_5 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_0_5 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_0_5 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_0_5 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_0_5 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_7, Opra2_0_0 ] )
        , ( ( Opra2_0_5 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_1 ] )
        , ( ( Opra2_0_5 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_1, Opra2_0_2 ] )
        , ( ( Opra2_0_5 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3 ] )
        , ( ( Opra2_0_5 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_3, Opra2_0_4 ] )
        , ( ( Opra2_0_5 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5 ] )
        , ( ( Opra2_0_5 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_5, Opra2_0_6 ] )
        , ( ( Opra2_0_5 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7 ] )
        , ( ( Opra2_0_5 , Opra2_2_0 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_0_5 , Opra2_2_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_5 , Opra2_2_2 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_0_5 , Opra2_2_3 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_5 , Opra2_2_4 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_0_5 , Opra2_2_5 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_5 , Opra2_2_6 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_0_5 , Opra2_2_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_0_5 , Opra2_3_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_0_5 , Opra2_3_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_5 , Opra2_3_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_0_5 , Opra2_3_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_5 , Opra2_3_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_0_5 , Opra2_3_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_5 , Opra2_3_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_0_5 , Opra2_3_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_5 , Opra2_4_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_0_5 , Opra2_4_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_5 , Opra2_4_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_0_5 , Opra2_4_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_5 , Opra2_4_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_0_5 , Opra2_4_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_5 , Opra2_4_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_0_5 , Opra2_4_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_5 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_4_0, Opra2_3_7, Opra2_2_7
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_0_4
                         , Opra2_s_0 ] )
        , ( ( Opra2_0_5 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_5 ] )
        , ( ( Opra2_0_5 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_s_6, Opra2_5_3, Opra2_4_2, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_6 ] )
        , ( ( Opra2_0_5 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_3, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_0_5 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_4_4, Opra2_s_4, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_0_5 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_0_5 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_4_6, Opra2_3_5, Opra2_2_5
                         , Opra2_s_2, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_2 ] )
        , ( ( Opra2_0_5 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_0_5 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1 ] )
        , ( ( Opra2_0_5 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_0_5 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3 ] )
        , ( ( Opra2_0_5 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_0_5 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5 ] )
        , ( ( Opra2_0_5 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_0_5 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7 ] )
        , ( ( Opra2_0_5 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_0_5 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1 ] )
        , ( ( Opra2_0_5 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_0_5 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3 ] )
        , ( ( Opra2_0_5 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_0_5 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5 ] )
        , ( ( Opra2_0_5 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_0_5 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7 ] )
        , ( ( Opra2_0_5 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_0_5 , Opra2_s_0 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_0_5 , Opra2_s_1 )
          , Set.fromList [ Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_0_5 , Opra2_s_2 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_0_5 , Opra2_s_3 )
          , Set.fromList [ Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_0_5 , Opra2_s_4 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_0_5 , Opra2_s_5 )
          , Set.fromList [ Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_0_5 , Opra2_s_6 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_0_5 , Opra2_s_7 )
          , Set.fromList [ Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_0_6 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_0_6 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_0_6 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_0_6 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_0_6 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_0_6 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_0_6 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_0_6 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_0_6 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_0_6 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_0_6 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_0_6 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_0_6 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_0_6 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_0_6 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_0_6 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_0_6 , Opra2_2_0 )
          , Set.fromList [ Opra2_0_0 ] )
        , ( ( Opra2_0_6 , Opra2_2_1 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_0_6 , Opra2_2_2 )
          , Set.fromList [ Opra2_0_2 ] )
        , ( ( Opra2_0_6 , Opra2_2_3 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_0_6 , Opra2_2_4 )
          , Set.fromList [ Opra2_0_4 ] )
        , ( ( Opra2_0_6 , Opra2_2_5 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_0_6 , Opra2_2_6 )
          , Set.fromList [ Opra2_0_6 ] )
        , ( ( Opra2_0_6 , Opra2_2_7 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_0_6 , Opra2_3_0 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_0_6 , Opra2_3_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_6 , Opra2_3_2 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_0_6 , Opra2_3_3 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_6 , Opra2_3_4 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_0_6 , Opra2_3_5 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_6 , Opra2_3_6 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_0_6 , Opra2_3_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_0_6 , Opra2_4_0 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_0_6 , Opra2_4_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_6 , Opra2_4_2 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_0_6 , Opra2_4_3 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_6 , Opra2_4_4 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_0_6 , Opra2_4_5 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_6 , Opra2_4_6 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_0_6 , Opra2_4_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_0_6 , Opra2_5_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_0_6 , Opra2_5_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_6 , Opra2_5_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_0_6 , Opra2_5_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_6 , Opra2_5_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_0_6 , Opra2_5_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_6 , Opra2_5_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_0_6 , Opra2_5_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_6 , Opra2_6_0 )
          , Set.fromList [ Opra2_4_0, Opra2_0_4, Opra2_s_0 ] )
        , ( ( Opra2_0_6 , Opra2_6_1 )
          , Set.fromList [ Opra2_s_7, Opra2_4_1, Opra2_0_5 ] )
        , ( ( Opra2_0_6 , Opra2_6_2 )
          , Set.fromList [ Opra2_s_6, Opra2_4_2, Opra2_0_6 ] )
        , ( ( Opra2_0_6 , Opra2_6_3 )
          , Set.fromList [ Opra2_s_5, Opra2_4_3, Opra2_0_7 ] )
        , ( ( Opra2_0_6 , Opra2_6_4 )
          , Set.fromList [ Opra2_4_4, Opra2_s_4, Opra2_0_0 ] )
        , ( ( Opra2_0_6 , Opra2_6_5 )
          , Set.fromList [ Opra2_4_5, Opra2_s_3, Opra2_0_1 ] )
        , ( ( Opra2_0_6 , Opra2_6_6 )
          , Set.fromList [ Opra2_4_6, Opra2_s_2, Opra2_0_2 ] )
        , ( ( Opra2_0_6 , Opra2_6_7 )
          , Set.fromList [ Opra2_4_7, Opra2_s_1, Opra2_0_3 ] )
        , ( ( Opra2_0_6 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1 ] )
        , ( ( Opra2_0_6 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_0_6 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3 ] )
        , ( ( Opra2_0_6 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_0_6 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5 ] )
        , ( ( Opra2_0_6 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_0_6 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7 ] )
        , ( ( Opra2_0_6 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_0_6 , Opra2_s_0 )
          , Set.fromList [ Opra2_0_6 ] )
        , ( ( Opra2_0_6 , Opra2_s_1 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_0_6 , Opra2_s_2 )
          , Set.fromList [ Opra2_0_4 ] )
        , ( ( Opra2_0_6 , Opra2_s_3 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_0_6 , Opra2_s_4 )
          , Set.fromList [ Opra2_0_2 ] )
        , ( ( Opra2_0_6 , Opra2_s_5 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_0_6 , Opra2_s_6 )
          , Set.fromList [ Opra2_0_0 ] )
        , ( ( Opra2_0_6 , Opra2_s_7 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_0_7 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1 ] )
        , ( ( Opra2_0_7 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_0_7 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3 ] )
        , ( ( Opra2_0_7 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_0_7 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5 ] )
        , ( ( Opra2_0_7 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_0_7 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7 ] )
        , ( ( Opra2_0_7 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_0_7 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1 ] )
        , ( ( Opra2_0_7 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_0_7 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3 ] )
        , ( ( Opra2_0_7 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_0_7 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5 ] )
        , ( ( Opra2_0_7 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_0_7 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7 ] )
        , ( ( Opra2_0_7 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_0_7 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_0_7 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_0_7 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_0_7 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_0_7 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_0_7 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_0_7 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_0_7 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_0_7 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_7, Opra2_0_0 ] )
        , ( ( Opra2_0_7 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_1 ] )
        , ( ( Opra2_0_7 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_1, Opra2_0_2 ] )
        , ( ( Opra2_0_7 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3 ] )
        , ( ( Opra2_0_7 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_3, Opra2_0_4 ] )
        , ( ( Opra2_0_7 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5 ] )
        , ( ( Opra2_0_7 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_5, Opra2_0_6 ] )
        , ( ( Opra2_0_7 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7 ] )
        , ( ( Opra2_0_7 , Opra2_4_0 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_0_7 , Opra2_4_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_7 , Opra2_4_2 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_0_7 , Opra2_4_3 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_7 , Opra2_4_4 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_0_7 , Opra2_4_5 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_7 , Opra2_4_6 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_0_7 , Opra2_4_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_0_7 , Opra2_5_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_0_7 , Opra2_5_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_7 , Opra2_5_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_0_7 , Opra2_5_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_7 , Opra2_5_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_0_7 , Opra2_5_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_7 , Opra2_5_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_0_7 , Opra2_5_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_7 , Opra2_6_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_0_7 , Opra2_6_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_7 , Opra2_6_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_0_7 , Opra2_6_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_0_7 , Opra2_6_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_0_7 , Opra2_6_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_0_7 , Opra2_6_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_0_7 , Opra2_6_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_0_7 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_4_0, Opra2_3_7, Opra2_2_7
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_0_4
                         , Opra2_s_0 ] )
        , ( ( Opra2_0_7 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_5 ] )
        , ( ( Opra2_0_7 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_s_6, Opra2_5_3, Opra2_4_2, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_6 ] )
        , ( ( Opra2_0_7 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_3, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_0_7 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_4_4, Opra2_s_4, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_0_7 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_0_7 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_4_6, Opra2_3_5, Opra2_2_5
                         , Opra2_s_2, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_2 ] )
        , ( ( Opra2_0_7 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_0_7 , Opra2_s_0 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_0_7 , Opra2_s_1 )
          , Set.fromList [ Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_0_7 , Opra2_s_2 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_0_7 , Opra2_s_3 )
          , Set.fromList [ Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_0_7 , Opra2_s_4 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_0_7 , Opra2_s_5 )
          , Set.fromList [ Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_0_7 , Opra2_s_6 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_0_7 , Opra2_s_7 )
          , Set.fromList [ Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_0 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_0, Opra2_1_4, Opra2_s_1 ] )
        , ( ( Opra2_1_0 , Opra2_0_1 )
          , Set.fromList [ Opra2_s_7, Opra2_5_1, Opra2_1_5, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_1_0 , Opra2_0_2 )
          , Set.fromList [ Opra2_s_7, Opra2_5_2, Opra2_1_6 ] )
        , ( ( Opra2_1_0 , Opra2_0_3 )
          , Set.fromList [ Opra2_s_7, Opra2_s_6, Opra2_5_3, Opra2_s_5
                         , Opra2_1_7 ] )
        , ( ( Opra2_1_0 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_4, Opra2_s_5, Opra2_1_0 ] )
        , ( ( Opra2_1_0 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_5, Opra2_s_5, Opra2_s_4, Opra2_s_3
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_0 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_6, Opra2_s_3, Opra2_1_2 ] )
        , ( ( Opra2_1_0 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7, Opra2_s_3, Opra2_s_2, Opra2_1_3
                         , Opra2_s_1 ] )
        , ( ( Opra2_1_0 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_1_0 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_0 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_0 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_0 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_1_0 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_0 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_0 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_0 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_1_0 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_0 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_1_0 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_0 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_1_0 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_0 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_1_0 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_0 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_1_0 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_0 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_1_0 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_0 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_1_0 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_0 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_1_0 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_0 , Opra2_4_0 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_1_0 , Opra2_4_1 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_1_0 , Opra2_4_2 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_1_0 , Opra2_4_3 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_1_0 , Opra2_4_4 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_1_0 , Opra2_4_5 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_1_0 , Opra2_4_6 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_1_0 , Opra2_4_7 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_1_0 , Opra2_5_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_1_0 , Opra2_5_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_0 , Opra2_5_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_1_0 , Opra2_5_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_0 , Opra2_5_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_1_0 , Opra2_5_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_0 , Opra2_5_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_1_0 , Opra2_5_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_1_0 , Opra2_6_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_1_0 , Opra2_6_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_0 , Opra2_6_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_1_0 , Opra2_6_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_0 , Opra2_6_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_1_0 , Opra2_6_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_0 , Opra2_6_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_1_0 , Opra2_6_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_1_0 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_0 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_0 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_0 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_0 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_0 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_0 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_0 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_0 , Opra2_s_0 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_1_0 , Opra2_s_1 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_1_0 , Opra2_s_2 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_1_0 , Opra2_s_3 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_1_0 , Opra2_s_4 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_1_0 , Opra2_s_5 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_1_0 , Opra2_s_6 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_1_0 , Opra2_s_7 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_1_1 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_1 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_1 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_1 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_1 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_1 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_1 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_1 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_1 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_s_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_1 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_5_7, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_1
                         , Opra2_4_0, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0, Opra2_s_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1, Opra2_s_0 ] )
        , ( ( Opra2_1_1 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_3, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_1, Opra2_1_0, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_1 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_s_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_s_5
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_6, Opra2_0_5
                         , Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_1 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_3, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_1 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_s_5, Opra2_4_5, Opra2_4_4
                         , Opra2_4_3, Opra2_s_4, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_s_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_6, Opra2_0_5
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_1 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_7
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_1 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_s_3, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_s_2
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_s_1, Opra2_0_7, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_1 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_1_1 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_1 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_1 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_1 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_1_1 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_1 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_1 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_1 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_1_1 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_1 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_1 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_1 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_1_1 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_1 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_1 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_1 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_1_1 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_1 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_1_1 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_1 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_1_1 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_1 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_1_1 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_1 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_7, Opra2_2_7, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_1 ] )
        , ( ( Opra2_1_1 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_1 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_1, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3 ] )
        , ( ( Opra2_1_1 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_1 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_3, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5 ] )
        , ( ( Opra2_1_1 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_1 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_5, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7 ] )
        , ( ( Opra2_1_1 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_1 , Opra2_6_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_1_1 , Opra2_6_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_1 , Opra2_6_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_1_1 , Opra2_6_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_1 , Opra2_6_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_1_1 , Opra2_6_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_1 , Opra2_6_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_1_1 , Opra2_6_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_1_1 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_1 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_1 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_1 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_1 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_1 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_1 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_1 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_1 , Opra2_s_0 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_1_1 , Opra2_s_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_1 , Opra2_s_2 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_1_1 , Opra2_s_3 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_1 , Opra2_s_4 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_1_1 , Opra2_s_5 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_1 , Opra2_s_6 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_1_1 , Opra2_s_7 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_2 , Opra2_0_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_1_2 , Opra2_0_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_2 , Opra2_0_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_1_2 , Opra2_0_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_2 , Opra2_0_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_1_2 , Opra2_0_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_2 , Opra2_0_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_1_2 , Opra2_0_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_1_2 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_2 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_2 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_2 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_2 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_2 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_2 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_2 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_2 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_0, Opra2_1_4, Opra2_s_1 ] )
        , ( ( Opra2_1_2 , Opra2_2_1 )
          , Set.fromList [ Opra2_s_7, Opra2_5_1, Opra2_1_5, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_1_2 , Opra2_2_2 )
          , Set.fromList [ Opra2_s_7, Opra2_5_2, Opra2_1_6 ] )
        , ( ( Opra2_1_2 , Opra2_2_3 )
          , Set.fromList [ Opra2_s_7, Opra2_s_6, Opra2_5_3, Opra2_s_5
                         , Opra2_1_7 ] )
        , ( ( Opra2_1_2 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_4, Opra2_s_5, Opra2_1_0 ] )
        , ( ( Opra2_1_2 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_5, Opra2_s_5, Opra2_s_4, Opra2_s_3
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_2 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_6, Opra2_s_3, Opra2_1_2 ] )
        , ( ( Opra2_1_2 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7, Opra2_s_3, Opra2_s_2, Opra2_1_3
                         , Opra2_s_1 ] )
        , ( ( Opra2_1_2 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_1_2 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_2 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_2 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_2 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_1_2 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_2 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_2 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_2 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_1_2 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_2 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_1_2 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_2 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_1_2 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_2 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_1_2 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_2 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_1_2 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_2 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_1_2 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_2 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_1_2 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_2 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_1_2 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_2 , Opra2_6_0 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_1_2 , Opra2_6_1 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_1_2 , Opra2_6_2 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_1_2 , Opra2_6_3 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_1_2 , Opra2_6_4 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_1_2 , Opra2_6_5 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_1_2 , Opra2_6_6 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_1_2 , Opra2_6_7 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_1_2 , Opra2_7_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_1_2 , Opra2_7_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_2 , Opra2_7_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_1_2 , Opra2_7_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_2 , Opra2_7_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_1_2 , Opra2_7_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_2 , Opra2_7_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_1_2 , Opra2_7_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_1_2 , Opra2_s_0 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_1_2 , Opra2_s_1 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_1_2 , Opra2_s_2 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_1_2 , Opra2_s_3 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_1_2 , Opra2_s_4 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_1_2 , Opra2_s_5 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_1_2 , Opra2_s_6 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_1_2 , Opra2_s_7 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_1_3 , Opra2_0_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_1_3 , Opra2_0_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_3 , Opra2_0_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_1_3 , Opra2_0_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_3 , Opra2_0_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_1_3 , Opra2_0_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_3 , Opra2_0_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_1_3 , Opra2_0_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_1_3 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_3 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_3 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_3 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_3 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_3 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_3 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_3 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_3 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_3 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_3 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_3 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_3 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_3 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_3 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_3 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_3 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_s_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_3 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_5_7, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_1
                         , Opra2_4_0, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0, Opra2_s_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1, Opra2_s_0 ] )
        , ( ( Opra2_1_3 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_3, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_1, Opra2_1_0, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_3 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_s_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_s_5
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_6, Opra2_0_5
                         , Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_3 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_3, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_3 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_s_5, Opra2_4_5, Opra2_4_4
                         , Opra2_4_3, Opra2_s_4, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_s_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_6, Opra2_0_5
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_3 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_7
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_3 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_s_3, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_s_2
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_s_1, Opra2_0_7, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_3 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_1_3 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_3 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_3 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_3 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_1_3 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_3 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_3 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_3 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_1_3 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_3 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_3 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_3 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_1_3 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_3 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_3 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_3 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_1_3 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_3 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_1_3 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_3 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_1_3 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_3 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_1_3 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_3 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_7, Opra2_2_7, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_1 ] )
        , ( ( Opra2_1_3 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_3 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_1, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3 ] )
        , ( ( Opra2_1_3 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_3 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_3, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5 ] )
        , ( ( Opra2_1_3 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_3 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_5, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7 ] )
        , ( ( Opra2_1_3 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_3 , Opra2_s_0 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_1_3 , Opra2_s_1 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_3 , Opra2_s_2 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_1_3 , Opra2_s_3 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_3 , Opra2_s_4 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_1_3 , Opra2_s_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_3 , Opra2_s_6 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_1_3 , Opra2_s_7 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_4 , Opra2_0_0 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_1_4 , Opra2_0_1 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_1_4 , Opra2_0_2 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_1_4 , Opra2_0_3 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_1_4 , Opra2_0_4 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_1_4 , Opra2_0_5 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_1_4 , Opra2_0_6 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_1_4 , Opra2_0_7 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_1_4 , Opra2_1_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_1_4 , Opra2_1_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_4 , Opra2_1_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_1_4 , Opra2_1_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_4 , Opra2_1_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_1_4 , Opra2_1_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_4 , Opra2_1_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_1_4 , Opra2_1_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_1_4 , Opra2_2_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_1_4 , Opra2_2_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_4 , Opra2_2_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_1_4 , Opra2_2_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_4 , Opra2_2_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_1_4 , Opra2_2_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_4 , Opra2_2_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_1_4 , Opra2_2_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_1_4 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_4 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_4 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_4 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_4 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_4 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_4 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_4 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_4 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_0, Opra2_1_4, Opra2_s_1 ] )
        , ( ( Opra2_1_4 , Opra2_4_1 )
          , Set.fromList [ Opra2_s_7, Opra2_5_1, Opra2_1_5, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_1_4 , Opra2_4_2 )
          , Set.fromList [ Opra2_s_7, Opra2_5_2, Opra2_1_6 ] )
        , ( ( Opra2_1_4 , Opra2_4_3 )
          , Set.fromList [ Opra2_s_7, Opra2_s_6, Opra2_5_3, Opra2_s_5
                         , Opra2_1_7 ] )
        , ( ( Opra2_1_4 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_4, Opra2_s_5, Opra2_1_0 ] )
        , ( ( Opra2_1_4 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_5, Opra2_s_5, Opra2_s_4, Opra2_s_3
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_4 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_6, Opra2_s_3, Opra2_1_2 ] )
        , ( ( Opra2_1_4 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7, Opra2_s_3, Opra2_s_2, Opra2_1_3
                         , Opra2_s_1 ] )
        , ( ( Opra2_1_4 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_1_4 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_4 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_4 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_4 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_1_4 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_4 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_4 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_4 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_1_4 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_4 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_1_4 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_4 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_1_4 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_4 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_1_4 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_4 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_1_4 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_4 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_1_4 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_4 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_1_4 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_4 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_1_4 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_4 , Opra2_s_0 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_1_4 , Opra2_s_1 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_1_4 , Opra2_s_2 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_1_4 , Opra2_s_3 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_1_4 , Opra2_s_4 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_1_4 , Opra2_s_5 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_1_4 , Opra2_s_6 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_1_4 , Opra2_s_7 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_1_5 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_1_5 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_5 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_1_5 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_5 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_1_5 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_5 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_1_5 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_5 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_7, Opra2_2_7, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_1 ] )
        , ( ( Opra2_1_5 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_5 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_1, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3 ] )
        , ( ( Opra2_1_5 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_5 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_3, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5 ] )
        , ( ( Opra2_1_5 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_5 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_5, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7 ] )
        , ( ( Opra2_1_5 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_5 , Opra2_2_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_1_5 , Opra2_2_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_5 , Opra2_2_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_1_5 , Opra2_2_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_5 , Opra2_2_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_1_5 , Opra2_2_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_5 , Opra2_2_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_1_5 , Opra2_2_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_1_5 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_5 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_5 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_5 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_5 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_5 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_5 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_5 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_5 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_5 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_5 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_5 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_5 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_5 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_5 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_5 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_5 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_s_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_5 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_5_7, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_1
                         , Opra2_4_0, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0, Opra2_s_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1, Opra2_s_0 ] )
        , ( ( Opra2_1_5 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_3, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_1, Opra2_1_0, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_5 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_s_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_s_5
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_6, Opra2_0_5
                         , Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_5 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_3, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_5 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_s_5, Opra2_4_5, Opra2_4_4
                         , Opra2_4_3, Opra2_s_4, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_s_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_6, Opra2_0_5
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_5 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_7
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_5 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_s_3, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_s_2
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_s_1, Opra2_0_7, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_5 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_1_5 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_5 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_5 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_5 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_1_5 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_5 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_5 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_5 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_1_5 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_5 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_5 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_5 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_1_5 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_5 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_5 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_5 , Opra2_s_0 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_1_5 , Opra2_s_1 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_5 , Opra2_s_2 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_1_5 , Opra2_s_3 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_5 , Opra2_s_4 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_1_5 , Opra2_s_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_5 , Opra2_s_6 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_1_5 , Opra2_s_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_6 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_1_6 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_6 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_1_6 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_6 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_1_6 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_6 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_1_6 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_6 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_1_6 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_6 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_1_6 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_6 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_1_6 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_6 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_1_6 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_6 , Opra2_2_0 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_1_6 , Opra2_2_1 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_1_6 , Opra2_2_2 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_1_6 , Opra2_2_3 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_1_6 , Opra2_2_4 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_1_6 , Opra2_2_5 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_1_6 , Opra2_2_6 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_1_6 , Opra2_2_7 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_1_6 , Opra2_3_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_1_6 , Opra2_3_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_6 , Opra2_3_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_1_6 , Opra2_3_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_6 , Opra2_3_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_1_6 , Opra2_3_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_6 , Opra2_3_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_1_6 , Opra2_3_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_1_6 , Opra2_4_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_1_6 , Opra2_4_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_6 , Opra2_4_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_1_6 , Opra2_4_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_6 , Opra2_4_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_1_6 , Opra2_4_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_6 , Opra2_4_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_1_6 , Opra2_4_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_1_6 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_6 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_6 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_6 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_6 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_6 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_6 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_6 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_6 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_0, Opra2_1_4, Opra2_s_1 ] )
        , ( ( Opra2_1_6 , Opra2_6_1 )
          , Set.fromList [ Opra2_s_7, Opra2_5_1, Opra2_1_5, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_1_6 , Opra2_6_2 )
          , Set.fromList [ Opra2_s_7, Opra2_5_2, Opra2_1_6 ] )
        , ( ( Opra2_1_6 , Opra2_6_3 )
          , Set.fromList [ Opra2_s_7, Opra2_s_6, Opra2_5_3, Opra2_s_5
                         , Opra2_1_7 ] )
        , ( ( Opra2_1_6 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_4, Opra2_s_5, Opra2_1_0 ] )
        , ( ( Opra2_1_6 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_5, Opra2_s_5, Opra2_s_4, Opra2_s_3
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_6 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_6, Opra2_s_3, Opra2_1_2 ] )
        , ( ( Opra2_1_6 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7, Opra2_s_3, Opra2_s_2, Opra2_1_3
                         , Opra2_s_1 ] )
        , ( ( Opra2_1_6 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_1_6 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_6 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_6 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_6 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_1_6 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_6 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_6 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_6 , Opra2_s_0 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_1_6 , Opra2_s_1 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_1_6 , Opra2_s_2 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_1_6 , Opra2_s_3 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_1_6 , Opra2_s_4 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_1_6 , Opra2_s_5 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_1_6 , Opra2_s_6 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_1_6 , Opra2_s_7 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_1_7 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_1_7 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_7 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_7 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_7 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_1_7 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_7 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_7 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_7 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_1_7 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_7 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_3
                         , Opra2_5_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_7 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_7 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_1_7 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_7 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_7 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_7 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_1_7 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_7 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_1_7 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_7 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_1_7 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_7 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_1_7 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_7 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_7, Opra2_2_7, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_1 ] )
        , ( ( Opra2_1_7 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_7 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_1, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3 ] )
        , ( ( Opra2_1_7 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_7 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_3, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5 ] )
        , ( ( Opra2_1_7 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_7 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_5, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7 ] )
        , ( ( Opra2_1_7 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_7 , Opra2_4_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_1_7 , Opra2_4_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_7 , Opra2_4_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_1_7 , Opra2_4_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_7 , Opra2_4_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_1_7 , Opra2_4_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_7 , Opra2_4_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_1_7 , Opra2_4_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_1_7 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_7 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_7 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_7 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_7 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_7 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_7 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_7 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_7 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_7 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_7 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_1_7 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_1_7 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_7 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_1_7 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_7 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_1_7 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_s_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_1_7 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_5_7, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_1
                         , Opra2_4_0, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0, Opra2_s_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1, Opra2_s_0 ] )
        , ( ( Opra2_1_7 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_3, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_1, Opra2_1_0, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_1_7 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_s_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_s_5
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_6, Opra2_0_5
                         , Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_1_7 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_3, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_1_7 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_s_5, Opra2_4_5, Opra2_4_4
                         , Opra2_4_3, Opra2_s_4, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_s_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_6, Opra2_0_5
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_7 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_7
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_1_7 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_s_3, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_s_2
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_s_1, Opra2_0_7, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_1_7 , Opra2_s_0 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_1_7 , Opra2_s_1 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_1_7 , Opra2_s_2 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_1_7 , Opra2_s_3 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_1_7 , Opra2_s_4 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_1_7 , Opra2_s_5 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_1_7 , Opra2_s_6 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_1_7 , Opra2_s_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_0 , Opra2_0_0 )
          , Set.fromList [ Opra2_6_0, Opra2_2_4, Opra2_s_2 ] )
        , ( ( Opra2_2_0 , Opra2_0_1 )
          , Set.fromList [ Opra2_6_1, Opra2_2_5, Opra2_s_1 ] )
        , ( ( Opra2_2_0 , Opra2_0_2 )
          , Set.fromList [ Opra2_6_2, Opra2_2_6, Opra2_s_0 ] )
        , ( ( Opra2_2_0 , Opra2_0_3 )
          , Set.fromList [ Opra2_s_7, Opra2_6_3, Opra2_2_7 ] )
        , ( ( Opra2_2_0 , Opra2_0_4 )
          , Set.fromList [ Opra2_6_4, Opra2_s_6, Opra2_2_0 ] )
        , ( ( Opra2_2_0 , Opra2_0_5 )
          , Set.fromList [ Opra2_6_5, Opra2_s_5, Opra2_2_1 ] )
        , ( ( Opra2_2_0 , Opra2_0_6 )
          , Set.fromList [ Opra2_6_6, Opra2_s_4, Opra2_2_2 ] )
        , ( ( Opra2_2_0 , Opra2_0_7 )
          , Set.fromList [ Opra2_6_7, Opra2_s_3, Opra2_2_3 ] )
        , ( ( Opra2_2_0 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_0 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_2_0 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_0 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_2_0 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_0 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_2_0 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_0 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_2_0 , Opra2_2_0 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_2_0 , Opra2_2_1 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_0 , Opra2_2_2 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_2_0 , Opra2_2_3 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_0 , Opra2_2_4 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_2_0 , Opra2_2_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_0 , Opra2_2_6 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_2_0 , Opra2_2_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_0 , Opra2_3_0 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_2_0 , Opra2_3_1 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_0 , Opra2_3_2 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_2_0 , Opra2_3_3 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_0 , Opra2_3_4 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_2_0 , Opra2_3_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_0 , Opra2_3_6 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_2_0 , Opra2_3_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_0 , Opra2_4_0 )
          , Set.fromList [ Opra2_2_0 ] )
        , ( ( Opra2_2_0 , Opra2_4_1 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_2_0 , Opra2_4_2 )
          , Set.fromList [ Opra2_2_2 ] )
        , ( ( Opra2_2_0 , Opra2_4_3 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_2_0 , Opra2_4_4 )
          , Set.fromList [ Opra2_2_4 ] )
        , ( ( Opra2_2_0 , Opra2_4_5 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_2_0 , Opra2_4_6 )
          , Set.fromList [ Opra2_2_6 ] )
        , ( ( Opra2_2_0 , Opra2_4_7 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_2_0 , Opra2_5_0 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_2_0 , Opra2_5_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_0 , Opra2_5_2 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_2_0 , Opra2_5_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_0 , Opra2_5_4 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_2_0 , Opra2_5_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_0 , Opra2_5_6 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_2_0 , Opra2_5_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_2_0 , Opra2_6_0 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_2_0 , Opra2_6_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_0 , Opra2_6_2 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_2_0 , Opra2_6_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_0 , Opra2_6_4 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_2_0 , Opra2_6_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_0 , Opra2_6_6 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_2_0 , Opra2_6_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_2_0 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_2_0 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_0 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_2_0 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_0 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_2_0 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_0 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_2_0 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_0 , Opra2_s_0 )
          , Set.fromList [ Opra2_2_0 ] )
        , ( ( Opra2_2_0 , Opra2_s_1 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_2_0 , Opra2_s_2 )
          , Set.fromList [ Opra2_2_6 ] )
        , ( ( Opra2_2_0 , Opra2_s_3 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_2_0 , Opra2_s_4 )
          , Set.fromList [ Opra2_2_4 ] )
        , ( ( Opra2_2_0 , Opra2_s_5 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_2_0 , Opra2_s_6 )
          , Set.fromList [ Opra2_2_2 ] )
        , ( ( Opra2_2_0 , Opra2_s_7 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_2_1 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_2_1 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_1 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_2_1 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_1 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_2_1 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_1 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_2_1 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_1 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_0, Opra2_5_7, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_4
                         , Opra2_s_2, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_1 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_5
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_s_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_1 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_0_3
                         , Opra2_s_0 ] )
        , ( ( Opra2_2_1 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_3, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_1 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_4, Opra2_s_6, Opra2_5_3
                         , Opra2_4_3, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_1 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_1 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_6, Opra2_5_5, Opra2_4_5
                         , Opra2_s_4, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_2, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_1 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_3, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_2_1 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_1 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_2_1 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_1 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_2_1 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_1 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_2_1 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_1 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_2_1 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_1 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_2_1 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_1 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_2_1 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_1 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_2_1 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_1 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_2_1 , Opra2_4_0 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_2_1 , Opra2_4_1 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_1 , Opra2_4_2 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_2_1 , Opra2_4_3 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_1 , Opra2_4_4 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_2_1 , Opra2_4_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_1 , Opra2_4_6 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_2_1 , Opra2_4_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_1 , Opra2_5_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_0, Opra2_1_1 ] )
        , ( ( Opra2_2_1 , Opra2_5_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_1 , Opra2_5_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_2, Opra2_1_3 ] )
        , ( ( Opra2_2_1 , Opra2_5_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_1 , Opra2_5_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_4, Opra2_1_5 ] )
        , ( ( Opra2_2_1 , Opra2_5_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_1 , Opra2_5_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_6, Opra2_1_7 ] )
        , ( ( Opra2_2_1 , Opra2_5_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_1 , Opra2_6_0 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_2_1 , Opra2_6_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_1 , Opra2_6_2 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_2_1 , Opra2_6_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_1 , Opra2_6_4 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_2_1 , Opra2_6_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_1 , Opra2_6_6 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_2_1 , Opra2_6_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_2_1 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_2_1 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_1 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_2_1 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_1 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_2_1 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_1 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_2_1 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_1 , Opra2_s_0 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_2_1 , Opra2_s_1 )
          , Set.fromList [ Opra2_2_7, Opra2_2_1, Opra2_2_0 ] )
        , ( ( Opra2_2_1 , Opra2_s_2 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_2_1 , Opra2_s_3 )
          , Set.fromList [ Opra2_2_7, Opra2_2_6, Opra2_2_5 ] )
        , ( ( Opra2_2_1 , Opra2_s_4 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_2_1 , Opra2_s_5 )
          , Set.fromList [ Opra2_2_5, Opra2_2_4, Opra2_2_3 ] )
        , ( ( Opra2_2_1 , Opra2_s_6 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_2_1 , Opra2_s_7 )
          , Set.fromList [ Opra2_2_3, Opra2_2_2, Opra2_2_1 ] )
        , ( ( Opra2_2_2 , Opra2_0_0 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_2_2 , Opra2_0_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_2 , Opra2_0_2 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_2_2 , Opra2_0_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_2 , Opra2_0_4 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_2_2 , Opra2_0_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_2 , Opra2_0_6 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_2_2 , Opra2_0_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_2_2 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_2_2 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_2 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_2_2 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_2 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_2_2 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_2 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_2_2 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_2 , Opra2_2_0 )
          , Set.fromList [ Opra2_6_0, Opra2_2_4, Opra2_s_2 ] )
        , ( ( Opra2_2_2 , Opra2_2_1 )
          , Set.fromList [ Opra2_6_1, Opra2_2_5, Opra2_s_1 ] )
        , ( ( Opra2_2_2 , Opra2_2_2 )
          , Set.fromList [ Opra2_6_2, Opra2_2_6, Opra2_s_0 ] )
        , ( ( Opra2_2_2 , Opra2_2_3 )
          , Set.fromList [ Opra2_s_7, Opra2_6_3, Opra2_2_7 ] )
        , ( ( Opra2_2_2 , Opra2_2_4 )
          , Set.fromList [ Opra2_6_4, Opra2_s_6, Opra2_2_0 ] )
        , ( ( Opra2_2_2 , Opra2_2_5 )
          , Set.fromList [ Opra2_6_5, Opra2_s_5, Opra2_2_1 ] )
        , ( ( Opra2_2_2 , Opra2_2_6 )
          , Set.fromList [ Opra2_6_6, Opra2_s_4, Opra2_2_2 ] )
        , ( ( Opra2_2_2 , Opra2_2_7 )
          , Set.fromList [ Opra2_6_7, Opra2_s_3, Opra2_2_3 ] )
        , ( ( Opra2_2_2 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_2 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_2_2 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_2 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_2_2 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_2 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_2_2 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_2 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_2_2 , Opra2_4_0 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_2_2 , Opra2_4_1 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_2 , Opra2_4_2 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_2_2 , Opra2_4_3 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_2 , Opra2_4_4 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_2_2 , Opra2_4_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_2 , Opra2_4_6 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_2_2 , Opra2_4_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_2 , Opra2_5_0 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_2_2 , Opra2_5_1 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_2 , Opra2_5_2 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_2_2 , Opra2_5_3 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_2 , Opra2_5_4 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_2_2 , Opra2_5_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_2 , Opra2_5_6 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_2_2 , Opra2_5_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_2 , Opra2_6_0 )
          , Set.fromList [ Opra2_2_0 ] )
        , ( ( Opra2_2_2 , Opra2_6_1 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_2_2 , Opra2_6_2 )
          , Set.fromList [ Opra2_2_2 ] )
        , ( ( Opra2_2_2 , Opra2_6_3 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_2_2 , Opra2_6_4 )
          , Set.fromList [ Opra2_2_4 ] )
        , ( ( Opra2_2_2 , Opra2_6_5 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_2_2 , Opra2_6_6 )
          , Set.fromList [ Opra2_2_6 ] )
        , ( ( Opra2_2_2 , Opra2_6_7 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_2_2 , Opra2_7_0 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_2_2 , Opra2_7_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_2 , Opra2_7_2 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_2_2 , Opra2_7_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_2 , Opra2_7_4 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_2_2 , Opra2_7_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_2 , Opra2_7_6 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_2_2 , Opra2_7_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_2_2 , Opra2_s_0 )
          , Set.fromList [ Opra2_2_2 ] )
        , ( ( Opra2_2_2 , Opra2_s_1 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_2_2 , Opra2_s_2 )
          , Set.fromList [ Opra2_2_0 ] )
        , ( ( Opra2_2_2 , Opra2_s_3 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_2_2 , Opra2_s_4 )
          , Set.fromList [ Opra2_2_6 ] )
        , ( ( Opra2_2_2 , Opra2_s_5 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_2_2 , Opra2_s_6 )
          , Set.fromList [ Opra2_2_4 ] )
        , ( ( Opra2_2_2 , Opra2_s_7 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_2_3 , Opra2_0_0 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_2_3 , Opra2_0_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_3 , Opra2_0_2 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_2_3 , Opra2_0_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_3 , Opra2_0_4 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_2_3 , Opra2_0_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_3 , Opra2_0_6 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_2_3 , Opra2_0_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_2_3 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_2_3 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_3 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_2_3 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_3 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_2_3 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_3 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_2_3 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_3 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_2_3 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_3 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_2_3 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_3 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_2_3 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_3 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_2_3 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_3 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_0, Opra2_5_7, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_4
                         , Opra2_s_2, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_3 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_5
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_s_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_3 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_0_3
                         , Opra2_s_0 ] )
        , ( ( Opra2_2_3 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_3, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_3 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_4, Opra2_s_6, Opra2_5_3
                         , Opra2_4_3, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_3 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_3 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_6, Opra2_5_5, Opra2_4_5
                         , Opra2_s_4, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_2, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_3 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_3, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_2_3 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_3 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_2_3 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_3 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_2_3 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_3 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_2_3 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_3 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_2_3 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_3 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_2_3 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_3 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_2_3 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_3 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_2_3 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_3 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_2_3 , Opra2_6_0 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_2_3 , Opra2_6_1 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_3 , Opra2_6_2 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_2_3 , Opra2_6_3 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_3 , Opra2_6_4 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_2_3 , Opra2_6_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_3 , Opra2_6_6 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_2_3 , Opra2_6_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_3 , Opra2_7_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_0, Opra2_1_1 ] )
        , ( ( Opra2_2_3 , Opra2_7_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_3 , Opra2_7_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_2, Opra2_1_3 ] )
        , ( ( Opra2_2_3 , Opra2_7_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_3 , Opra2_7_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_4, Opra2_1_5 ] )
        , ( ( Opra2_2_3 , Opra2_7_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_3 , Opra2_7_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_6, Opra2_1_7 ] )
        , ( ( Opra2_2_3 , Opra2_7_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_3 , Opra2_s_0 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_2_3 , Opra2_s_1 )
          , Set.fromList [ Opra2_2_3, Opra2_2_2, Opra2_2_1 ] )
        , ( ( Opra2_2_3 , Opra2_s_2 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_2_3 , Opra2_s_3 )
          , Set.fromList [ Opra2_2_7, Opra2_2_1, Opra2_2_0 ] )
        , ( ( Opra2_2_3 , Opra2_s_4 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_2_3 , Opra2_s_5 )
          , Set.fromList [ Opra2_2_7, Opra2_2_6, Opra2_2_5 ] )
        , ( ( Opra2_2_3 , Opra2_s_6 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_2_3 , Opra2_s_7 )
          , Set.fromList [ Opra2_2_5, Opra2_2_4, Opra2_2_3 ] )
        , ( ( Opra2_2_4 , Opra2_0_0 )
          , Set.fromList [ Opra2_2_0 ] )
        , ( ( Opra2_2_4 , Opra2_0_1 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_2_4 , Opra2_0_2 )
          , Set.fromList [ Opra2_2_2 ] )
        , ( ( Opra2_2_4 , Opra2_0_3 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_2_4 , Opra2_0_4 )
          , Set.fromList [ Opra2_2_4 ] )
        , ( ( Opra2_2_4 , Opra2_0_5 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_2_4 , Opra2_0_6 )
          , Set.fromList [ Opra2_2_6 ] )
        , ( ( Opra2_2_4 , Opra2_0_7 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_2_4 , Opra2_1_0 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_2_4 , Opra2_1_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_4 , Opra2_1_2 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_2_4 , Opra2_1_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_4 , Opra2_1_4 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_2_4 , Opra2_1_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_4 , Opra2_1_6 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_2_4 , Opra2_1_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_2_4 , Opra2_2_0 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_2_4 , Opra2_2_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_4 , Opra2_2_2 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_2_4 , Opra2_2_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_4 , Opra2_2_4 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_2_4 , Opra2_2_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_4 , Opra2_2_6 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_2_4 , Opra2_2_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_2_4 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_2_4 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_4 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_2_4 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_4 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_2_4 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_4 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_2_4 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_4 , Opra2_4_0 )
          , Set.fromList [ Opra2_6_0, Opra2_2_4, Opra2_s_2 ] )
        , ( ( Opra2_2_4 , Opra2_4_1 )
          , Set.fromList [ Opra2_6_1, Opra2_2_5, Opra2_s_1 ] )
        , ( ( Opra2_2_4 , Opra2_4_2 )
          , Set.fromList [ Opra2_6_2, Opra2_2_6, Opra2_s_0 ] )
        , ( ( Opra2_2_4 , Opra2_4_3 )
          , Set.fromList [ Opra2_s_7, Opra2_6_3, Opra2_2_7 ] )
        , ( ( Opra2_2_4 , Opra2_4_4 )
          , Set.fromList [ Opra2_6_4, Opra2_s_6, Opra2_2_0 ] )
        , ( ( Opra2_2_4 , Opra2_4_5 )
          , Set.fromList [ Opra2_6_5, Opra2_s_5, Opra2_2_1 ] )
        , ( ( Opra2_2_4 , Opra2_4_6 )
          , Set.fromList [ Opra2_6_6, Opra2_s_4, Opra2_2_2 ] )
        , ( ( Opra2_2_4 , Opra2_4_7 )
          , Set.fromList [ Opra2_6_7, Opra2_s_3, Opra2_2_3 ] )
        , ( ( Opra2_2_4 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_4 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_2_4 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_4 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_2_4 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_4 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_2_4 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_4 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_2_4 , Opra2_6_0 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_2_4 , Opra2_6_1 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_4 , Opra2_6_2 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_2_4 , Opra2_6_3 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_4 , Opra2_6_4 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_2_4 , Opra2_6_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_4 , Opra2_6_6 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_2_4 , Opra2_6_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_4 , Opra2_7_0 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_2_4 , Opra2_7_1 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_4 , Opra2_7_2 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_2_4 , Opra2_7_3 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_4 , Opra2_7_4 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_2_4 , Opra2_7_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_4 , Opra2_7_6 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_2_4 , Opra2_7_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_4 , Opra2_s_0 )
          , Set.fromList [ Opra2_2_4 ] )
        , ( ( Opra2_2_4 , Opra2_s_1 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_2_4 , Opra2_s_2 )
          , Set.fromList [ Opra2_2_2 ] )
        , ( ( Opra2_2_4 , Opra2_s_3 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_2_4 , Opra2_s_4 )
          , Set.fromList [ Opra2_2_0 ] )
        , ( ( Opra2_2_4 , Opra2_s_5 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_2_4 , Opra2_s_6 )
          , Set.fromList [ Opra2_2_6 ] )
        , ( ( Opra2_2_4 , Opra2_s_7 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_2_5 , Opra2_0_0 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_2_5 , Opra2_0_1 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_5 , Opra2_0_2 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_2_5 , Opra2_0_3 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_5 , Opra2_0_4 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_2_5 , Opra2_0_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_5 , Opra2_0_6 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_2_5 , Opra2_0_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_5 , Opra2_1_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_0, Opra2_1_1 ] )
        , ( ( Opra2_2_5 , Opra2_1_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_5 , Opra2_1_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_2, Opra2_1_3 ] )
        , ( ( Opra2_2_5 , Opra2_1_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_5 , Opra2_1_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_4, Opra2_1_5 ] )
        , ( ( Opra2_2_5 , Opra2_1_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_5 , Opra2_1_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_6, Opra2_1_7 ] )
        , ( ( Opra2_2_5 , Opra2_1_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_5 , Opra2_2_0 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_2_5 , Opra2_2_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_5 , Opra2_2_2 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_2_5 , Opra2_2_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_5 , Opra2_2_4 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_2_5 , Opra2_2_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_5 , Opra2_2_6 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_2_5 , Opra2_2_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_2_5 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_2_5 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_5 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_2_5 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_5 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_2_5 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_5 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_2_5 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_5 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_2_5 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_5 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_2_5 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_5 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_2_5 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_5 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_2_5 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_5 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_0, Opra2_5_7, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_4
                         , Opra2_s_2, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_5 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_5
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_s_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_5 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_0_3
                         , Opra2_s_0 ] )
        , ( ( Opra2_2_5 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_3, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_5 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_4, Opra2_s_6, Opra2_5_3
                         , Opra2_4_3, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_5 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_5 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_6, Opra2_5_5, Opra2_4_5
                         , Opra2_s_4, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_2, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_5 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_3, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_2_5 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_5 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_2_5 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_5 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_2_5 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_5 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_2_5 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_5 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_2_5 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_5 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_2_5 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_5 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_2_5 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_5 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_2_5 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_5 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_2_5 , Opra2_s_0 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_2_5 , Opra2_s_1 )
          , Set.fromList [ Opra2_2_5, Opra2_2_4, Opra2_2_3 ] )
        , ( ( Opra2_2_5 , Opra2_s_2 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_2_5 , Opra2_s_3 )
          , Set.fromList [ Opra2_2_3, Opra2_2_2, Opra2_2_1 ] )
        , ( ( Opra2_2_5 , Opra2_s_4 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_2_5 , Opra2_s_5 )
          , Set.fromList [ Opra2_2_7, Opra2_2_1, Opra2_2_0 ] )
        , ( ( Opra2_2_5 , Opra2_s_6 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_2_5 , Opra2_s_7 )
          , Set.fromList [ Opra2_2_7, Opra2_2_6, Opra2_2_5 ] )
        , ( ( Opra2_2_6 , Opra2_0_0 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_2_6 , Opra2_0_1 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_6 , Opra2_0_2 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_2_6 , Opra2_0_3 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_6 , Opra2_0_4 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_2_6 , Opra2_0_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_6 , Opra2_0_6 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_2_6 , Opra2_0_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_6 , Opra2_1_0 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_2_6 , Opra2_1_1 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_6 , Opra2_1_2 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_2_6 , Opra2_1_3 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_6 , Opra2_1_4 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_2_6 , Opra2_1_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_6 , Opra2_1_6 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_2_6 , Opra2_1_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_6 , Opra2_2_0 )
          , Set.fromList [ Opra2_2_0 ] )
        , ( ( Opra2_2_6 , Opra2_2_1 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_2_6 , Opra2_2_2 )
          , Set.fromList [ Opra2_2_2 ] )
        , ( ( Opra2_2_6 , Opra2_2_3 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_2_6 , Opra2_2_4 )
          , Set.fromList [ Opra2_2_4 ] )
        , ( ( Opra2_2_6 , Opra2_2_5 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_2_6 , Opra2_2_6 )
          , Set.fromList [ Opra2_2_6 ] )
        , ( ( Opra2_2_6 , Opra2_2_7 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_2_6 , Opra2_3_0 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_2_6 , Opra2_3_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_6 , Opra2_3_2 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_2_6 , Opra2_3_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_6 , Opra2_3_4 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_2_6 , Opra2_3_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_6 , Opra2_3_6 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_2_6 , Opra2_3_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_2_6 , Opra2_4_0 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_2_6 , Opra2_4_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_6 , Opra2_4_2 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_2_6 , Opra2_4_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_6 , Opra2_4_4 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_2_6 , Opra2_4_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_6 , Opra2_4_6 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_2_6 , Opra2_4_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_2_6 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_2_6 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_6 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_2_6 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_6 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_2_6 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_6 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_2_6 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_6 , Opra2_6_0 )
          , Set.fromList [ Opra2_6_0, Opra2_2_4, Opra2_s_2 ] )
        , ( ( Opra2_2_6 , Opra2_6_1 )
          , Set.fromList [ Opra2_6_1, Opra2_2_5, Opra2_s_1 ] )
        , ( ( Opra2_2_6 , Opra2_6_2 )
          , Set.fromList [ Opra2_6_2, Opra2_2_6, Opra2_s_0 ] )
        , ( ( Opra2_2_6 , Opra2_6_3 )
          , Set.fromList [ Opra2_s_7, Opra2_6_3, Opra2_2_7 ] )
        , ( ( Opra2_2_6 , Opra2_6_4 )
          , Set.fromList [ Opra2_6_4, Opra2_s_6, Opra2_2_0 ] )
        , ( ( Opra2_2_6 , Opra2_6_5 )
          , Set.fromList [ Opra2_6_5, Opra2_s_5, Opra2_2_1 ] )
        , ( ( Opra2_2_6 , Opra2_6_6 )
          , Set.fromList [ Opra2_6_6, Opra2_s_4, Opra2_2_2 ] )
        , ( ( Opra2_2_6 , Opra2_6_7 )
          , Set.fromList [ Opra2_6_7, Opra2_s_3, Opra2_2_3 ] )
        , ( ( Opra2_2_6 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_6 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_2_6 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_6 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_2_6 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_6 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_2_6 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_6 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_2_6 , Opra2_s_0 )
          , Set.fromList [ Opra2_2_6 ] )
        , ( ( Opra2_2_6 , Opra2_s_1 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_2_6 , Opra2_s_2 )
          , Set.fromList [ Opra2_2_4 ] )
        , ( ( Opra2_2_6 , Opra2_s_3 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_2_6 , Opra2_s_4 )
          , Set.fromList [ Opra2_2_2 ] )
        , ( ( Opra2_2_6 , Opra2_s_5 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_2_6 , Opra2_s_6 )
          , Set.fromList [ Opra2_2_0 ] )
        , ( ( Opra2_2_6 , Opra2_s_7 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_2_7 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_7 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_2_7 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_7 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_2_7 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_7 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_2_7 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_7 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_2_7 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_7 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_2_7 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_3, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_7 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_2_7 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_7 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_2_7 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_7 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_2_7 , Opra2_2_0 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_2_7 , Opra2_2_1 )
          , Set.fromList [ Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_7 , Opra2_2_2 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_2_7 , Opra2_2_3 )
          , Set.fromList [ Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_7 , Opra2_2_4 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_2_7 , Opra2_2_5 )
          , Set.fromList [ Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_7 , Opra2_2_6 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_2_7 , Opra2_2_7 )
          , Set.fromList [ Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_7 , Opra2_3_0 )
          , Set.fromList [ Opra2_3_7, Opra2_2_0, Opra2_1_1 ] )
        , ( ( Opra2_2_7 , Opra2_3_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_2_7 , Opra2_3_2 )
          , Set.fromList [ Opra2_3_1, Opra2_2_2, Opra2_1_3 ] )
        , ( ( Opra2_2_7 , Opra2_3_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_2_7 , Opra2_3_4 )
          , Set.fromList [ Opra2_3_3, Opra2_2_4, Opra2_1_5 ] )
        , ( ( Opra2_2_7 , Opra2_3_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_2_7 , Opra2_3_6 )
          , Set.fromList [ Opra2_3_5, Opra2_2_6, Opra2_1_7 ] )
        , ( ( Opra2_2_7 , Opra2_3_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_2_7 , Opra2_4_0 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_2_7 , Opra2_4_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_7 , Opra2_4_2 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_2_7 , Opra2_4_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_7 , Opra2_4_4 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_2_7 , Opra2_4_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_7 , Opra2_4_6 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_2_7 , Opra2_4_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_2_7 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_2_7 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_7 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_2_7 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_7 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_2_7 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_7 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_2_7 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_7 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_2_7 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_7 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_2_7 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_2_7 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_2_7 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_2_7 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_2_7 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_2_7 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_0, Opra2_5_7, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_4
                         , Opra2_s_2, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_7 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_5
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_s_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_2_7 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_0_3
                         , Opra2_s_0 ] )
        , ( ( Opra2_2_7 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_3, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_2_7 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_4, Opra2_s_6, Opra2_5_3
                         , Opra2_4_3, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_7 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_2_7 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_6, Opra2_5_5, Opra2_4_5
                         , Opra2_s_4, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_2, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_2_7 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_3, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_2_7 , Opra2_s_0 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_2_7 , Opra2_s_1 )
          , Set.fromList [ Opra2_2_7, Opra2_2_6, Opra2_2_5 ] )
        , ( ( Opra2_2_7 , Opra2_s_2 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_2_7 , Opra2_s_3 )
          , Set.fromList [ Opra2_2_5, Opra2_2_4, Opra2_2_3 ] )
        , ( ( Opra2_2_7 , Opra2_s_4 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_2_7 , Opra2_s_5 )
          , Set.fromList [ Opra2_2_3, Opra2_2_2, Opra2_2_1 ] )
        , ( ( Opra2_2_7 , Opra2_s_6 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_2_7 , Opra2_s_7 )
          , Set.fromList [ Opra2_2_7, Opra2_2_1, Opra2_2_0 ] )
        , ( ( Opra2_3_0 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_0, Opra2_3_4, Opra2_s_3 ] )
        , ( ( Opra2_3_0 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_1, Opra2_3_5, Opra2_s_3, Opra2_s_2
                         , Opra2_s_1 ] )
        , ( ( Opra2_3_0 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_2, Opra2_3_6, Opra2_s_1 ] )
        , ( ( Opra2_3_0 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_3, Opra2_s_7, Opra2_3_7, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_3_0 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_4, Opra2_s_7, Opra2_3_0 ] )
        , ( ( Opra2_3_0 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5, Opra2_s_7, Opra2_s_6, Opra2_s_5
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_0 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_6, Opra2_s_5, Opra2_3_2 ] )
        , ( ( Opra2_3_0 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_s_5, Opra2_s_4, Opra2_3_3
                         , Opra2_s_3 ] )
        , ( ( Opra2_3_0 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_3_0 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_0 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_3_0 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_0 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_3_0 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_0 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7 ] )
        , ( ( Opra2_3_0 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_3_0 , Opra2_2_0 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_0 , Opra2_2_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_0 , Opra2_2_2 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_0 , Opra2_2_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_0 , Opra2_2_4 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_0 , Opra2_2_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_0 , Opra2_2_6 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_0 , Opra2_2_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_0 , Opra2_3_0 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_0 , Opra2_3_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_0 , Opra2_3_2 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_0 , Opra2_3_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_0 , Opra2_3_4 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_0 , Opra2_3_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_0 , Opra2_3_6 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_0 , Opra2_3_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_0 , Opra2_4_0 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_3_0 , Opra2_4_1 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_3_0 , Opra2_4_2 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_3_0 , Opra2_4_3 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_3_0 , Opra2_4_4 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_3_0 , Opra2_4_5 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_3_0 , Opra2_4_6 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_3_0 , Opra2_4_7 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_3_0 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_3_0 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_0 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_3_0 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_0 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_3_0 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_0 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_3_0 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_3_0 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_3_0 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_0 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_3_0 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_0 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_3_0 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_0 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_3_0 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_3_0 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_0 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_0 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_0 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_0 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_0 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_0 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_0 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_0 , Opra2_s_0 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_3_0 , Opra2_s_1 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_3_0 , Opra2_s_2 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_3_0 , Opra2_s_3 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_3_0 , Opra2_s_4 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_3_0 , Opra2_s_5 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_3_0 , Opra2_s_6 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_3_0 , Opra2_s_7 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_3_1 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_1 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_1 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_1 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_1 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_1 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_1 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_1 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_1 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_s_3, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_1 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_s_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_s_2, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_s_1, Opra2_0_3
                         , Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_3_1 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_1
                         , Opra2_3_0, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_1 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_5_7, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_4_0, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3, Opra2_s_0 ] )
        , ( ( Opra2_3_1 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_3, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_1 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_s_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_s_5
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_3_1 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_3_7
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_3_1 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_s_5, Opra2_4_7, Opra2_4_6
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_s_4
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_s_3, Opra2_2_7, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_3_1 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_3_1 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_1 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_3_1 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_1 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_3_1 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_1 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7 ] )
        , ( ( Opra2_3_1 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_3_1 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_3_1 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_1 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_3_1 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_1 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_3_1 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_1 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7 ] )
        , ( ( Opra2_3_1 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_3_1 , Opra2_4_0 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_1 , Opra2_4_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_1 , Opra2_4_2 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_1 , Opra2_4_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_1 , Opra2_4_4 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_1 , Opra2_4_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_1 , Opra2_4_6 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_1 , Opra2_4_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_1 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_1 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_1 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_1 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_1 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_1 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_1 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_1 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_1 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_3_1 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_1 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_3_1 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_1 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_3_1 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_1 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_3_1 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_3_1 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_1 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_1 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_1 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_1 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_1 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_1 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_1 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_1 , Opra2_s_0 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_3_1 , Opra2_s_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_1 , Opra2_s_2 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_3_1 , Opra2_s_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_1 , Opra2_s_4 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_3_1 , Opra2_s_5 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_1 , Opra2_s_6 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_3_1 , Opra2_s_7 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_2 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_3_2 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_2 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_3_2 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_2 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_3_2 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_2 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_3_2 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_3_2 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_2 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_2 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_2 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_2 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_2 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_2 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_2 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_2 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_0, Opra2_3_4, Opra2_s_3 ] )
        , ( ( Opra2_3_2 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_1, Opra2_3_5, Opra2_s_3, Opra2_s_2
                         , Opra2_s_1 ] )
        , ( ( Opra2_3_2 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_2, Opra2_3_6, Opra2_s_1 ] )
        , ( ( Opra2_3_2 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_3, Opra2_s_7, Opra2_3_7, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_3_2 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_4, Opra2_s_7, Opra2_3_0 ] )
        , ( ( Opra2_3_2 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5, Opra2_s_7, Opra2_s_6, Opra2_s_5
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_2 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_6, Opra2_s_5, Opra2_3_2 ] )
        , ( ( Opra2_3_2 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_s_5, Opra2_s_4, Opra2_3_3
                         , Opra2_s_3 ] )
        , ( ( Opra2_3_2 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_3_2 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_2 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_3_2 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_2 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_3_2 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_2 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7 ] )
        , ( ( Opra2_3_2 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_3_2 , Opra2_4_0 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_2 , Opra2_4_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_2 , Opra2_4_2 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_2 , Opra2_4_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_2 , Opra2_4_4 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_2 , Opra2_4_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_2 , Opra2_4_6 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_2 , Opra2_4_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_2 , Opra2_5_0 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_2 , Opra2_5_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_2 , Opra2_5_2 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_2 , Opra2_5_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_2 , Opra2_5_4 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_2 , Opra2_5_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_2 , Opra2_5_6 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_2 , Opra2_5_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_2 , Opra2_6_0 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_3_2 , Opra2_6_1 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_3_2 , Opra2_6_2 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_3_2 , Opra2_6_3 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_3_2 , Opra2_6_4 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_3_2 , Opra2_6_5 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_3_2 , Opra2_6_6 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_3_2 , Opra2_6_7 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_3_2 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_3_2 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_2 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_3_2 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_2 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_3_2 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_2 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_3_2 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_3_2 , Opra2_s_0 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_3_2 , Opra2_s_1 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_3_2 , Opra2_s_2 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_3_2 , Opra2_s_3 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_3_2 , Opra2_s_4 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_3_2 , Opra2_s_5 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_3_2 , Opra2_s_6 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_3_2 , Opra2_s_7 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_3_3 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_3_3 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_3 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_3_3 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_3 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_3_3 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_3 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_3_3 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_3_3 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_3 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_3 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_3 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_3 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_3 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_3 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_3 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_3 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_3 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_3 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_3 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_3 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_3 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_3 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_3 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_3 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_s_3, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_3 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_s_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_s_2, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_s_1, Opra2_0_3
                         , Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_3_3 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_1
                         , Opra2_3_0, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_3 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_5_7, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_4_0, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3, Opra2_s_0 ] )
        , ( ( Opra2_3_3 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_3, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_3 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_s_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_s_5
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_3_3 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_3_7
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_3_3 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_s_5, Opra2_4_7, Opra2_4_6
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_s_4
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_s_3, Opra2_2_7, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_3_3 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_3_3 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_3 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_3_3 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_3 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_3_3 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_3 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7 ] )
        , ( ( Opra2_3_3 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_3_3 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_3_3 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_3 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_3_3 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_3 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_3_3 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_3 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7 ] )
        , ( ( Opra2_3_3 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_3_3 , Opra2_6_0 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_3 , Opra2_6_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_3 , Opra2_6_2 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_3 , Opra2_6_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_3 , Opra2_6_4 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_3 , Opra2_6_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_3 , Opra2_6_6 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_3 , Opra2_6_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_3 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_3 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_3 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_3 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_3 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_3 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_3 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_3 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_3 , Opra2_s_0 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_3_3 , Opra2_s_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_3 , Opra2_s_2 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_3_3 , Opra2_s_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_3 , Opra2_s_4 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_3_3 , Opra2_s_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_3 , Opra2_s_6 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_3_3 , Opra2_s_7 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_4 , Opra2_0_0 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_3_4 , Opra2_0_1 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_3_4 , Opra2_0_2 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_3_4 , Opra2_0_3 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_3_4 , Opra2_0_4 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_3_4 , Opra2_0_5 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_3_4 , Opra2_0_6 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_3_4 , Opra2_0_7 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_3_4 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_3_4 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_4 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_3_4 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_4 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_3_4 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_4 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_3_4 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_3_4 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_3_4 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_4 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_3_4 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_4 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_3_4 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_4 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_3_4 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_3_4 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_4 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_4 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_4 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_4 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_4 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_4 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_4 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_4 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_0, Opra2_3_4, Opra2_s_3 ] )
        , ( ( Opra2_3_4 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_1, Opra2_3_5, Opra2_s_3, Opra2_s_2
                         , Opra2_s_1 ] )
        , ( ( Opra2_3_4 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_2, Opra2_3_6, Opra2_s_1 ] )
        , ( ( Opra2_3_4 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_3, Opra2_s_7, Opra2_3_7, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_3_4 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_4, Opra2_s_7, Opra2_3_0 ] )
        , ( ( Opra2_3_4 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5, Opra2_s_7, Opra2_s_6, Opra2_s_5
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_4 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_6, Opra2_s_5, Opra2_3_2 ] )
        , ( ( Opra2_3_4 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_s_5, Opra2_s_4, Opra2_3_3
                         , Opra2_s_3 ] )
        , ( ( Opra2_3_4 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_3_4 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_4 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_3_4 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_4 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_3_4 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_4 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7 ] )
        , ( ( Opra2_3_4 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_3_4 , Opra2_6_0 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_4 , Opra2_6_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_4 , Opra2_6_2 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_4 , Opra2_6_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_4 , Opra2_6_4 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_4 , Opra2_6_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_4 , Opra2_6_6 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_4 , Opra2_6_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_4 , Opra2_7_0 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_4 , Opra2_7_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_4 , Opra2_7_2 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_4 , Opra2_7_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_4 , Opra2_7_4 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_4 , Opra2_7_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_4 , Opra2_7_6 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_4 , Opra2_7_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_4 , Opra2_s_0 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_3_4 , Opra2_s_1 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_3_4 , Opra2_s_2 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_3_4 , Opra2_s_3 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_3_4 , Opra2_s_4 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_3_4 , Opra2_s_5 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_3_4 , Opra2_s_6 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_3_4 , Opra2_s_7 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_3_5 , Opra2_0_0 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_5 , Opra2_0_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_5 , Opra2_0_2 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_5 , Opra2_0_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_5 , Opra2_0_4 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_5 , Opra2_0_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_5 , Opra2_0_6 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_5 , Opra2_0_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_5 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_5 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_5 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_5 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_5 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_5 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_5 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_5 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_5 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_3_5 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_5 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_3_5 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_5 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_3_5 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_5 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_3_5 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_3_5 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_5 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_5 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_5 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_5 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_5 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_5 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_5 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_5 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_5 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_5 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_5 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_5 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_5 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_5 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_5 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_5 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_s_3, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_5 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_s_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_s_2, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_s_1, Opra2_0_3
                         , Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_3_5 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_1
                         , Opra2_3_0, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_5 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_5_7, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_4_0, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3, Opra2_s_0 ] )
        , ( ( Opra2_3_5 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_3, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_5 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_s_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_s_5
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_3_5 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_3_7
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_3_5 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_s_5, Opra2_4_7, Opra2_4_6
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_s_4
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_s_3, Opra2_2_7, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_3_5 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_3_5 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_5 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_3_5 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_5 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_3_5 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_5 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7 ] )
        , ( ( Opra2_3_5 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_3_5 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_3_5 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_5 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_3_5 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_5 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_3_5 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_5 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7 ] )
        , ( ( Opra2_3_5 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_3_5 , Opra2_s_0 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_3_5 , Opra2_s_1 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_5 , Opra2_s_2 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_3_5 , Opra2_s_3 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_5 , Opra2_s_4 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_3_5 , Opra2_s_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_5 , Opra2_s_6 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_3_5 , Opra2_s_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_6 , Opra2_0_0 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_6 , Opra2_0_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_6 , Opra2_0_2 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_6 , Opra2_0_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_6 , Opra2_0_4 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_6 , Opra2_0_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_6 , Opra2_0_6 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_6 , Opra2_0_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_6 , Opra2_1_0 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_6 , Opra2_1_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_6 , Opra2_1_2 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_6 , Opra2_1_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_6 , Opra2_1_4 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_6 , Opra2_1_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_6 , Opra2_1_6 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_6 , Opra2_1_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_6 , Opra2_2_0 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_3_6 , Opra2_2_1 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_3_6 , Opra2_2_2 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_3_6 , Opra2_2_3 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_3_6 , Opra2_2_4 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_3_6 , Opra2_2_5 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_3_6 , Opra2_2_6 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_3_6 , Opra2_2_7 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_3_6 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_3_6 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_6 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_3_6 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_6 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_3_6 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_6 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_3_6 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_3_6 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_3_6 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_6 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_3_6 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_6 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_3_6 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_6 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_3_6 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_3_6 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_6 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_6 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_6 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_6 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_6 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_6 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_6 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_6 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_0, Opra2_3_4, Opra2_s_3 ] )
        , ( ( Opra2_3_6 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_1, Opra2_3_5, Opra2_s_3, Opra2_s_2
                         , Opra2_s_1 ] )
        , ( ( Opra2_3_6 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_2, Opra2_3_6, Opra2_s_1 ] )
        , ( ( Opra2_3_6 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_3, Opra2_s_7, Opra2_3_7, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_3_6 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_4, Opra2_s_7, Opra2_3_0 ] )
        , ( ( Opra2_3_6 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5, Opra2_s_7, Opra2_s_6, Opra2_s_5
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_6 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_6, Opra2_s_5, Opra2_3_2 ] )
        , ( ( Opra2_3_6 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_s_5, Opra2_s_4, Opra2_3_3
                         , Opra2_s_3 ] )
        , ( ( Opra2_3_6 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_3_6 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_6 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_3_6 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_6 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_3_6 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_6 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7 ] )
        , ( ( Opra2_3_6 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_3_6 , Opra2_s_0 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_3_6 , Opra2_s_1 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_3_6 , Opra2_s_2 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_3_6 , Opra2_s_3 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_3_6 , Opra2_s_4 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_3_6 , Opra2_s_5 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_3_6 , Opra2_s_6 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_3_6 , Opra2_s_7 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_3_7 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_3_7 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_7 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_3_7 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_7 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_3_7 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_7 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7 ] )
        , ( ( Opra2_3_7 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_3_7 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_3_7 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_7 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_3, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_3_7 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_4
                         , Opra2_2_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_7 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_3_7 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_7 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7 ] )
        , ( ( Opra2_3_7 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_7
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_3_7 , Opra2_2_0 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_7 , Opra2_2_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_7 , Opra2_2_2 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_7 , Opra2_2_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_7 , Opra2_2_4 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_7 , Opra2_2_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_7 , Opra2_2_6 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_7 , Opra2_2_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_7 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_3_7 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_3_7 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_3_7 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_3_7 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_3_7 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_3_7 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_3_7 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_3_7 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_3_7 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_7 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_3_7 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_7 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_3_7 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_7 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_3_7 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_3_7 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_7 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_7 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_7 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_7 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_7 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_7 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_7 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_7 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_7 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_7 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_3_7 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_3_7 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_7 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_3_7 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_7 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_3_7 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_s_3, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_3_7 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_s_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_s_2, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_s_1, Opra2_0_3
                         , Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_3_7 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_1
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_1
                         , Opra2_3_0, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_3_7 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_5_7, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_4_0, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3, Opra2_s_0 ] )
        , ( ( Opra2_3_7 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_3, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_3_7 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_s_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_s_5
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_3_7 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_5
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_3_7
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_3_7 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_s_5, Opra2_4_7, Opra2_4_6
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_s_4
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_3_0
                         , Opra2_s_3, Opra2_2_7, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_3_7 , Opra2_s_0 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_3_7 , Opra2_s_1 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_3_7 , Opra2_s_2 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_3_7 , Opra2_s_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_3_7 , Opra2_s_4 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_3_7 , Opra2_s_5 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_3_7 , Opra2_s_6 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_3_7 , Opra2_s_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_0 , Opra2_0_0 )
          , Set.fromList [ Opra2_4_4, Opra2_s_4, Opra2_0_0 ] )
        , ( ( Opra2_4_0 , Opra2_0_1 )
          , Set.fromList [ Opra2_4_5, Opra2_s_3, Opra2_0_1 ] )
        , ( ( Opra2_4_0 , Opra2_0_2 )
          , Set.fromList [ Opra2_4_6, Opra2_s_2, Opra2_0_2 ] )
        , ( ( Opra2_4_0 , Opra2_0_3 )
          , Set.fromList [ Opra2_4_7, Opra2_s_1, Opra2_0_3 ] )
        , ( ( Opra2_4_0 , Opra2_0_4 )
          , Set.fromList [ Opra2_4_0, Opra2_0_4, Opra2_s_0 ] )
        , ( ( Opra2_4_0 , Opra2_0_5 )
          , Set.fromList [ Opra2_s_7, Opra2_4_1, Opra2_0_5 ] )
        , ( ( Opra2_4_0 , Opra2_0_6 )
          , Set.fromList [ Opra2_s_6, Opra2_4_2, Opra2_0_6 ] )
        , ( ( Opra2_4_0 , Opra2_0_7 )
          , Set.fromList [ Opra2_s_5, Opra2_4_3, Opra2_0_7 ] )
        , ( ( Opra2_4_0 , Opra2_1_0 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_1
                         , Opra2_1_1 ] )
        , ( ( Opra2_4_0 , Opra2_1_1 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_4_0 , Opra2_1_2 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_3
                         , Opra2_1_3 ] )
        , ( ( Opra2_4_0 , Opra2_1_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_4_0 , Opra2_1_4 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5 ] )
        , ( ( Opra2_4_0 , Opra2_1_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_4_0 , Opra2_1_6 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_1_7 ] )
        , ( ( Opra2_4_0 , Opra2_1_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_4_0 , Opra2_2_0 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_4_0 , Opra2_2_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_0 , Opra2_2_2 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_4_0 , Opra2_2_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_0 , Opra2_2_4 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_4_0 , Opra2_2_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_0 , Opra2_2_6 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_4_0 , Opra2_2_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_0 , Opra2_3_0 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_4_0 , Opra2_3_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_0 , Opra2_3_2 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_4_0 , Opra2_3_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_0 , Opra2_3_4 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_4_0 , Opra2_3_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_0 , Opra2_3_6 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_4_0 , Opra2_3_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_0 , Opra2_4_0 )
          , Set.fromList [ Opra2_4_0 ] )
        , ( ( Opra2_4_0 , Opra2_4_1 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_4_0 , Opra2_4_2 )
          , Set.fromList [ Opra2_4_2 ] )
        , ( ( Opra2_4_0 , Opra2_4_3 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_4_0 , Opra2_4_4 )
          , Set.fromList [ Opra2_4_4 ] )
        , ( ( Opra2_4_0 , Opra2_4_5 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_4_0 , Opra2_4_6 )
          , Set.fromList [ Opra2_4_6 ] )
        , ( ( Opra2_4_0 , Opra2_4_7 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_4_0 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_4_0 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_0 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_4_0 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_0 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_4_0 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_0 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_4_0 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_4_0 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_4_0 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_0 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_4_0 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_0 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_4_0 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_0 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_4_0 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_4_0 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_4_0 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_0 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_4_0 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_0 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_4_0 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_0 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_4_0 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_0 , Opra2_s_0 )
          , Set.fromList [ Opra2_4_0 ] )
        , ( ( Opra2_4_0 , Opra2_s_1 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_4_0 , Opra2_s_2 )
          , Set.fromList [ Opra2_4_6 ] )
        , ( ( Opra2_4_0 , Opra2_s_3 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_4_0 , Opra2_s_4 )
          , Set.fromList [ Opra2_4_4 ] )
        , ( ( Opra2_4_0 , Opra2_s_5 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_4_0 , Opra2_s_6 )
          , Set.fromList [ Opra2_4_2 ] )
        , ( ( Opra2_4_0 , Opra2_s_7 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_4_1 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_4_1 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_1 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_4_1 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_1 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_4_1 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_1 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_4_1 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_1 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_4, Opra2_s_4, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_1, Opra2_1_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_4_1 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_5
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_s_3, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_4_1 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_s_2, Opra2_1_3
                         , Opra2_0_2 ] )
        , ( ( Opra2_4_1 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_4_1 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_0_4
                         , Opra2_s_0 ] )
        , ( ( Opra2_4_1 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_5, Opra2_6_4, Opra2_6_3, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_4_1 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_s_6, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_4_2, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_1_7
                         , Opra2_0_6 ] )
        , ( ( Opra2_4_1 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_3, Opra2_3_7, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_4_1 , Opra2_2_0 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_1
                         , Opra2_1_1 ] )
        , ( ( Opra2_4_1 , Opra2_2_1 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_4_1 , Opra2_2_2 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_3
                         , Opra2_1_3 ] )
        , ( ( Opra2_4_1 , Opra2_2_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_4_1 , Opra2_2_4 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5 ] )
        , ( ( Opra2_4_1 , Opra2_2_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_4_1 , Opra2_2_6 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_1_7 ] )
        , ( ( Opra2_4_1 , Opra2_2_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_4_1 , Opra2_3_0 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_1
                         , Opra2_1_1 ] )
        , ( ( Opra2_4_1 , Opra2_3_1 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_4_1 , Opra2_3_2 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_3
                         , Opra2_1_3 ] )
        , ( ( Opra2_4_1 , Opra2_3_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_4_1 , Opra2_3_4 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5 ] )
        , ( ( Opra2_4_1 , Opra2_3_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_4_1 , Opra2_3_6 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_1_7 ] )
        , ( ( Opra2_4_1 , Opra2_3_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_4_1 , Opra2_4_0 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_4_1 , Opra2_4_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_1 , Opra2_4_2 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_4_1 , Opra2_4_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_1 , Opra2_4_4 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_4_1 , Opra2_4_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_1 , Opra2_4_6 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_4_1 , Opra2_4_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_1 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_0, Opra2_3_1 ] )
        , ( ( Opra2_4_1 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_1 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_2, Opra2_3_3 ] )
        , ( ( Opra2_4_1 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_1 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_4, Opra2_3_5 ] )
        , ( ( Opra2_4_1 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_1 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_6, Opra2_3_7 ] )
        , ( ( Opra2_4_1 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_1 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_4_1 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_1 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_4_1 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_1 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_4_1 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_1 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_4_1 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_4_1 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_4_1 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_1 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_4_1 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_1 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_4_1 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_1 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_4_1 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_1 , Opra2_s_0 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_4_1 , Opra2_s_1 )
          , Set.fromList [ Opra2_4_7, Opra2_4_1, Opra2_4_0 ] )
        , ( ( Opra2_4_1 , Opra2_s_2 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_4_1 , Opra2_s_3 )
          , Set.fromList [ Opra2_4_7, Opra2_4_6, Opra2_4_5 ] )
        , ( ( Opra2_4_1 , Opra2_s_4 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_4_1 , Opra2_s_5 )
          , Set.fromList [ Opra2_4_5, Opra2_4_4, Opra2_4_3 ] )
        , ( ( Opra2_4_1 , Opra2_s_6 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_4_1 , Opra2_s_7 )
          , Set.fromList [ Opra2_4_3, Opra2_4_2, Opra2_4_1 ] )
        , ( ( Opra2_4_2 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_4_2 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_2 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_4_2 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_2 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_4_2 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_2 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_4_2 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_4_2 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_4_2 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_2 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_4_2 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_2 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_4_2 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_2 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_4_2 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_2 , Opra2_2_0 )
          , Set.fromList [ Opra2_4_4, Opra2_s_4, Opra2_0_0 ] )
        , ( ( Opra2_4_2 , Opra2_2_1 )
          , Set.fromList [ Opra2_4_5, Opra2_s_3, Opra2_0_1 ] )
        , ( ( Opra2_4_2 , Opra2_2_2 )
          , Set.fromList [ Opra2_4_6, Opra2_s_2, Opra2_0_2 ] )
        , ( ( Opra2_4_2 , Opra2_2_3 )
          , Set.fromList [ Opra2_4_7, Opra2_s_1, Opra2_0_3 ] )
        , ( ( Opra2_4_2 , Opra2_2_4 )
          , Set.fromList [ Opra2_4_0, Opra2_0_4, Opra2_s_0 ] )
        , ( ( Opra2_4_2 , Opra2_2_5 )
          , Set.fromList [ Opra2_s_7, Opra2_4_1, Opra2_0_5 ] )
        , ( ( Opra2_4_2 , Opra2_2_6 )
          , Set.fromList [ Opra2_s_6, Opra2_4_2, Opra2_0_6 ] )
        , ( ( Opra2_4_2 , Opra2_2_7 )
          , Set.fromList [ Opra2_s_5, Opra2_4_3, Opra2_0_7 ] )
        , ( ( Opra2_4_2 , Opra2_3_0 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_1
                         , Opra2_1_1 ] )
        , ( ( Opra2_4_2 , Opra2_3_1 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_4_2 , Opra2_3_2 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_3
                         , Opra2_1_3 ] )
        , ( ( Opra2_4_2 , Opra2_3_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_4_2 , Opra2_3_4 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5 ] )
        , ( ( Opra2_4_2 , Opra2_3_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_4_2 , Opra2_3_6 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_1_7 ] )
        , ( ( Opra2_4_2 , Opra2_3_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_4_2 , Opra2_4_0 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_4_2 , Opra2_4_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_2 , Opra2_4_2 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_4_2 , Opra2_4_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_2 , Opra2_4_4 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_4_2 , Opra2_4_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_2 , Opra2_4_6 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_4_2 , Opra2_4_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_2 , Opra2_5_0 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_4_2 , Opra2_5_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_2 , Opra2_5_2 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_4_2 , Opra2_5_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_2 , Opra2_5_4 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_4_2 , Opra2_5_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_2 , Opra2_5_6 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_4_2 , Opra2_5_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_2 , Opra2_6_0 )
          , Set.fromList [ Opra2_4_0 ] )
        , ( ( Opra2_4_2 , Opra2_6_1 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_4_2 , Opra2_6_2 )
          , Set.fromList [ Opra2_4_2 ] )
        , ( ( Opra2_4_2 , Opra2_6_3 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_4_2 , Opra2_6_4 )
          , Set.fromList [ Opra2_4_4 ] )
        , ( ( Opra2_4_2 , Opra2_6_5 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_4_2 , Opra2_6_6 )
          , Set.fromList [ Opra2_4_6 ] )
        , ( ( Opra2_4_2 , Opra2_6_7 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_4_2 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_4_2 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_2 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_4_2 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_2 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_4_2 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_2 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_4_2 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_4_2 , Opra2_s_0 )
          , Set.fromList [ Opra2_4_2 ] )
        , ( ( Opra2_4_2 , Opra2_s_1 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_4_2 , Opra2_s_2 )
          , Set.fromList [ Opra2_4_0 ] )
        , ( ( Opra2_4_2 , Opra2_s_3 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_4_2 , Opra2_s_4 )
          , Set.fromList [ Opra2_4_6 ] )
        , ( ( Opra2_4_2 , Opra2_s_5 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_4_2 , Opra2_s_6 )
          , Set.fromList [ Opra2_4_4 ] )
        , ( ( Opra2_4_2 , Opra2_s_7 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_4_3 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_4_3 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_3 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_4_3 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_3 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_4_3 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_3 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_4_3 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_4_3 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_4_3 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_3 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_4_3 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_3 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_4_3 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_3 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_4_3 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_3 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_4_3 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_3 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_4_3 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_3 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_4_3 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_3 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_4_3 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_3 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_4, Opra2_s_4, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_1, Opra2_1_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_4_3 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_5
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_s_3, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_4_3 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_s_2, Opra2_1_3
                         , Opra2_0_2 ] )
        , ( ( Opra2_4_3 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_4_3 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_0_4
                         , Opra2_s_0 ] )
        , ( ( Opra2_4_3 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_5, Opra2_6_4, Opra2_6_3, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_4_3 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_s_6, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_4_2, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_1_7
                         , Opra2_0_6 ] )
        , ( ( Opra2_4_3 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_3, Opra2_3_7, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_4_3 , Opra2_4_0 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_1
                         , Opra2_1_1 ] )
        , ( ( Opra2_4_3 , Opra2_4_1 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_4_3 , Opra2_4_2 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_3
                         , Opra2_1_3 ] )
        , ( ( Opra2_4_3 , Opra2_4_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_4_3 , Opra2_4_4 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5 ] )
        , ( ( Opra2_4_3 , Opra2_4_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_4_3 , Opra2_4_6 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_1_7 ] )
        , ( ( Opra2_4_3 , Opra2_4_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_4_3 , Opra2_5_0 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_1
                         , Opra2_1_1 ] )
        , ( ( Opra2_4_3 , Opra2_5_1 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_4_3 , Opra2_5_2 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_3
                         , Opra2_1_3 ] )
        , ( ( Opra2_4_3 , Opra2_5_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_4_3 , Opra2_5_4 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5 ] )
        , ( ( Opra2_4_3 , Opra2_5_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_4_3 , Opra2_5_6 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_1_7 ] )
        , ( ( Opra2_4_3 , Opra2_5_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_4_3 , Opra2_6_0 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_4_3 , Opra2_6_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_3 , Opra2_6_2 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_4_3 , Opra2_6_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_3 , Opra2_6_4 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_4_3 , Opra2_6_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_3 , Opra2_6_6 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_4_3 , Opra2_6_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_3 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_0, Opra2_3_1 ] )
        , ( ( Opra2_4_3 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_3 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_2, Opra2_3_3 ] )
        , ( ( Opra2_4_3 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_3 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_4, Opra2_3_5 ] )
        , ( ( Opra2_4_3 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_3 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_6, Opra2_3_7 ] )
        , ( ( Opra2_4_3 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_3 , Opra2_s_0 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_4_3 , Opra2_s_1 )
          , Set.fromList [ Opra2_4_3, Opra2_4_2, Opra2_4_1 ] )
        , ( ( Opra2_4_3 , Opra2_s_2 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_4_3 , Opra2_s_3 )
          , Set.fromList [ Opra2_4_7, Opra2_4_1, Opra2_4_0 ] )
        , ( ( Opra2_4_3 , Opra2_s_4 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_4_3 , Opra2_s_5 )
          , Set.fromList [ Opra2_4_7, Opra2_4_6, Opra2_4_5 ] )
        , ( ( Opra2_4_3 , Opra2_s_6 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_4_3 , Opra2_s_7 )
          , Set.fromList [ Opra2_4_5, Opra2_4_4, Opra2_4_3 ] )
        , ( ( Opra2_4_4 , Opra2_0_0 )
          , Set.fromList [ Opra2_4_0 ] )
        , ( ( Opra2_4_4 , Opra2_0_1 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_4_4 , Opra2_0_2 )
          , Set.fromList [ Opra2_4_2 ] )
        , ( ( Opra2_4_4 , Opra2_0_3 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_4_4 , Opra2_0_4 )
          , Set.fromList [ Opra2_4_4 ] )
        , ( ( Opra2_4_4 , Opra2_0_5 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_4_4 , Opra2_0_6 )
          , Set.fromList [ Opra2_4_6 ] )
        , ( ( Opra2_4_4 , Opra2_0_7 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_4_4 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_4_4 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_4 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_4_4 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_4 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_4_4 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_4 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_4_4 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_4_4 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_4_4 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_4 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_4_4 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_4 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_4_4 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_4 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_4_4 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_4_4 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_4_4 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_4 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_4_4 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_4 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_4_4 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_4 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_4_4 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_4 , Opra2_4_0 )
          , Set.fromList [ Opra2_4_4, Opra2_s_4, Opra2_0_0 ] )
        , ( ( Opra2_4_4 , Opra2_4_1 )
          , Set.fromList [ Opra2_4_5, Opra2_s_3, Opra2_0_1 ] )
        , ( ( Opra2_4_4 , Opra2_4_2 )
          , Set.fromList [ Opra2_4_6, Opra2_s_2, Opra2_0_2 ] )
        , ( ( Opra2_4_4 , Opra2_4_3 )
          , Set.fromList [ Opra2_4_7, Opra2_s_1, Opra2_0_3 ] )
        , ( ( Opra2_4_4 , Opra2_4_4 )
          , Set.fromList [ Opra2_4_0, Opra2_0_4, Opra2_s_0 ] )
        , ( ( Opra2_4_4 , Opra2_4_5 )
          , Set.fromList [ Opra2_s_7, Opra2_4_1, Opra2_0_5 ] )
        , ( ( Opra2_4_4 , Opra2_4_6 )
          , Set.fromList [ Opra2_s_6, Opra2_4_2, Opra2_0_6 ] )
        , ( ( Opra2_4_4 , Opra2_4_7 )
          , Set.fromList [ Opra2_s_5, Opra2_4_3, Opra2_0_7 ] )
        , ( ( Opra2_4_4 , Opra2_5_0 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_1
                         , Opra2_1_1 ] )
        , ( ( Opra2_4_4 , Opra2_5_1 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_4_4 , Opra2_5_2 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_3
                         , Opra2_1_3 ] )
        , ( ( Opra2_4_4 , Opra2_5_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_4_4 , Opra2_5_4 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5 ] )
        , ( ( Opra2_4_4 , Opra2_5_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_4_4 , Opra2_5_6 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_1_7 ] )
        , ( ( Opra2_4_4 , Opra2_5_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_4_4 , Opra2_6_0 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_4_4 , Opra2_6_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_4 , Opra2_6_2 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_4_4 , Opra2_6_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_4 , Opra2_6_4 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_4_4 , Opra2_6_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_4 , Opra2_6_6 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_4_4 , Opra2_6_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_4 , Opra2_7_0 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_4_4 , Opra2_7_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_4 , Opra2_7_2 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_4_4 , Opra2_7_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_4 , Opra2_7_4 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_4_4 , Opra2_7_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_4 , Opra2_7_6 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_4_4 , Opra2_7_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_4 , Opra2_s_0 )
          , Set.fromList [ Opra2_4_4 ] )
        , ( ( Opra2_4_4 , Opra2_s_1 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_4_4 , Opra2_s_2 )
          , Set.fromList [ Opra2_4_2 ] )
        , ( ( Opra2_4_4 , Opra2_s_3 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_4_4 , Opra2_s_4 )
          , Set.fromList [ Opra2_4_0 ] )
        , ( ( Opra2_4_4 , Opra2_s_5 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_4_4 , Opra2_s_6 )
          , Set.fromList [ Opra2_4_6 ] )
        , ( ( Opra2_4_4 , Opra2_s_7 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_4_5 , Opra2_0_0 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_4_5 , Opra2_0_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_5 , Opra2_0_2 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_4_5 , Opra2_0_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_5 , Opra2_0_4 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_4_5 , Opra2_0_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_5 , Opra2_0_6 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_4_5 , Opra2_0_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_5 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_0, Opra2_3_1 ] )
        , ( ( Opra2_4_5 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_5 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_2, Opra2_3_3 ] )
        , ( ( Opra2_4_5 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_5 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_4, Opra2_3_5 ] )
        , ( ( Opra2_4_5 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_5 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_6, Opra2_3_7 ] )
        , ( ( Opra2_4_5 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_5 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_4_5 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_5 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_4_5 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_5 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_4_5 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_5 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_4_5 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_4_5 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_4_5 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_5 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_4_5 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_5 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_4_5 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_5 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_4_5 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_5 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_4_5 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_5 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_4_5 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_5 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_4_5 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_5 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_4_5 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_5 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_4, Opra2_s_4, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_1, Opra2_1_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_4_5 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_5
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_s_3, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_4_5 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_s_2, Opra2_1_3
                         , Opra2_0_2 ] )
        , ( ( Opra2_4_5 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_4_5 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_0_4
                         , Opra2_s_0 ] )
        , ( ( Opra2_4_5 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_5, Opra2_6_4, Opra2_6_3, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_4_5 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_s_6, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_4_2, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_1_7
                         , Opra2_0_6 ] )
        , ( ( Opra2_4_5 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_3, Opra2_3_7, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_4_5 , Opra2_6_0 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_1
                         , Opra2_1_1 ] )
        , ( ( Opra2_4_5 , Opra2_6_1 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_4_5 , Opra2_6_2 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_3
                         , Opra2_1_3 ] )
        , ( ( Opra2_4_5 , Opra2_6_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_4_5 , Opra2_6_4 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5 ] )
        , ( ( Opra2_4_5 , Opra2_6_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_4_5 , Opra2_6_6 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_1_7 ] )
        , ( ( Opra2_4_5 , Opra2_6_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_4_5 , Opra2_7_0 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_1
                         , Opra2_1_1 ] )
        , ( ( Opra2_4_5 , Opra2_7_1 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_4_5 , Opra2_7_2 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_3
                         , Opra2_1_3 ] )
        , ( ( Opra2_4_5 , Opra2_7_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_4_5 , Opra2_7_4 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5 ] )
        , ( ( Opra2_4_5 , Opra2_7_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_4_5 , Opra2_7_6 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_1_7 ] )
        , ( ( Opra2_4_5 , Opra2_7_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_4_5 , Opra2_s_0 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_4_5 , Opra2_s_1 )
          , Set.fromList [ Opra2_4_5, Opra2_4_4, Opra2_4_3 ] )
        , ( ( Opra2_4_5 , Opra2_s_2 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_4_5 , Opra2_s_3 )
          , Set.fromList [ Opra2_4_3, Opra2_4_2, Opra2_4_1 ] )
        , ( ( Opra2_4_5 , Opra2_s_4 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_4_5 , Opra2_s_5 )
          , Set.fromList [ Opra2_4_7, Opra2_4_1, Opra2_4_0 ] )
        , ( ( Opra2_4_5 , Opra2_s_6 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_4_5 , Opra2_s_7 )
          , Set.fromList [ Opra2_4_7, Opra2_4_6, Opra2_4_5 ] )
        , ( ( Opra2_4_6 , Opra2_0_0 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_4_6 , Opra2_0_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_6 , Opra2_0_2 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_4_6 , Opra2_0_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_6 , Opra2_0_4 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_4_6 , Opra2_0_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_6 , Opra2_0_6 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_4_6 , Opra2_0_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_6 , Opra2_1_0 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_4_6 , Opra2_1_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_6 , Opra2_1_2 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_4_6 , Opra2_1_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_6 , Opra2_1_4 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_4_6 , Opra2_1_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_6 , Opra2_1_6 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_4_6 , Opra2_1_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_6 , Opra2_2_0 )
          , Set.fromList [ Opra2_4_0 ] )
        , ( ( Opra2_4_6 , Opra2_2_1 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_4_6 , Opra2_2_2 )
          , Set.fromList [ Opra2_4_2 ] )
        , ( ( Opra2_4_6 , Opra2_2_3 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_4_6 , Opra2_2_4 )
          , Set.fromList [ Opra2_4_4 ] )
        , ( ( Opra2_4_6 , Opra2_2_5 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_4_6 , Opra2_2_6 )
          , Set.fromList [ Opra2_4_6 ] )
        , ( ( Opra2_4_6 , Opra2_2_7 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_4_6 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_4_6 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_6 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_4_6 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_6 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_4_6 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_6 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_4_6 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_4_6 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_4_6 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_6 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_4_6 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_6 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_4_6 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_6 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_4_6 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_4_6 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_4_6 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_6 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_4_6 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_6 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_4_6 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_6 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_4_6 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_6 , Opra2_6_0 )
          , Set.fromList [ Opra2_4_4, Opra2_s_4, Opra2_0_0 ] )
        , ( ( Opra2_4_6 , Opra2_6_1 )
          , Set.fromList [ Opra2_4_5, Opra2_s_3, Opra2_0_1 ] )
        , ( ( Opra2_4_6 , Opra2_6_2 )
          , Set.fromList [ Opra2_4_6, Opra2_s_2, Opra2_0_2 ] )
        , ( ( Opra2_4_6 , Opra2_6_3 )
          , Set.fromList [ Opra2_4_7, Opra2_s_1, Opra2_0_3 ] )
        , ( ( Opra2_4_6 , Opra2_6_4 )
          , Set.fromList [ Opra2_4_0, Opra2_0_4, Opra2_s_0 ] )
        , ( ( Opra2_4_6 , Opra2_6_5 )
          , Set.fromList [ Opra2_s_7, Opra2_4_1, Opra2_0_5 ] )
        , ( ( Opra2_4_6 , Opra2_6_6 )
          , Set.fromList [ Opra2_s_6, Opra2_4_2, Opra2_0_6 ] )
        , ( ( Opra2_4_6 , Opra2_6_7 )
          , Set.fromList [ Opra2_s_5, Opra2_4_3, Opra2_0_7 ] )
        , ( ( Opra2_4_6 , Opra2_7_0 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_1
                         , Opra2_1_1 ] )
        , ( ( Opra2_4_6 , Opra2_7_1 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_4_6 , Opra2_7_2 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_3
                         , Opra2_1_3 ] )
        , ( ( Opra2_4_6 , Opra2_7_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_4_6 , Opra2_7_4 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5 ] )
        , ( ( Opra2_4_6 , Opra2_7_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_4_6 , Opra2_7_6 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_1_7 ] )
        , ( ( Opra2_4_6 , Opra2_7_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_4_6 , Opra2_s_0 )
          , Set.fromList [ Opra2_4_6 ] )
        , ( ( Opra2_4_6 , Opra2_s_1 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_4_6 , Opra2_s_2 )
          , Set.fromList [ Opra2_4_4 ] )
        , ( ( Opra2_4_6 , Opra2_s_3 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_4_6 , Opra2_s_4 )
          , Set.fromList [ Opra2_4_2 ] )
        , ( ( Opra2_4_6 , Opra2_s_5 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_4_6 , Opra2_s_6 )
          , Set.fromList [ Opra2_4_0 ] )
        , ( ( Opra2_4_6 , Opra2_s_7 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_4_7 , Opra2_0_0 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_1
                         , Opra2_1_1 ] )
        , ( ( Opra2_4_7 , Opra2_0_1 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_4_7 , Opra2_0_2 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_3
                         , Opra2_1_3 ] )
        , ( ( Opra2_4_7 , Opra2_0_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_4_7 , Opra2_0_4 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5 ] )
        , ( ( Opra2_4_7 , Opra2_0_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_4_7 , Opra2_0_6 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_1_7 ] )
        , ( ( Opra2_4_7 , Opra2_0_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_4_7 , Opra2_1_0 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_1
                         , Opra2_1_1 ] )
        , ( ( Opra2_4_7 , Opra2_1_1 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1 ] )
        , ( ( Opra2_4_7 , Opra2_1_2 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_3
                         , Opra2_1_3 ] )
        , ( ( Opra2_4_7 , Opra2_1_3 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3 ] )
        , ( ( Opra2_4_7 , Opra2_1_4 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5 ] )
        , ( ( Opra2_4_7 , Opra2_1_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5 ] )
        , ( ( Opra2_4_7 , Opra2_1_6 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_1_7 ] )
        , ( ( Opra2_4_7 , Opra2_1_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_1, Opra2_1_0 ] )
        , ( ( Opra2_4_7 , Opra2_2_0 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_4_7 , Opra2_2_1 )
          , Set.fromList [ Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_7 , Opra2_2_2 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_4_7 , Opra2_2_3 )
          , Set.fromList [ Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_7 , Opra2_2_4 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_4_7 , Opra2_2_5 )
          , Set.fromList [ Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_7 , Opra2_2_6 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_4_7 , Opra2_2_7 )
          , Set.fromList [ Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_7 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_7, Opra2_4_0, Opra2_3_1 ] )
        , ( ( Opra2_4_7 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_4_7 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_1, Opra2_4_2, Opra2_3_3 ] )
        , ( ( Opra2_4_7 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_4_7 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_3, Opra2_4_4, Opra2_3_5 ] )
        , ( ( Opra2_4_7 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_4_7 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_5, Opra2_4_6, Opra2_3_7 ] )
        , ( ( Opra2_4_7 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_4_7 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_4_7 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_7 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_4_7 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_7 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_4_7 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_7 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_4_7 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_4_7 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_4_7 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_7 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_4_7 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_7 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_4_7 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_7 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_4_7 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_7 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_4_7 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_7 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_4_7 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_4_7 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_4_7 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_4_7 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_4_7 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_4_7 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_4, Opra2_s_4, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_1, Opra2_1_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_4_7 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_5
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_s_3, Opra2_2_3, Opra2_2_2
                         , Opra2_2_1, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_4_7 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_s_2, Opra2_1_3
                         , Opra2_0_2 ] )
        , ( ( Opra2_4_7 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_4_7 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_0_4
                         , Opra2_s_0 ] )
        , ( ( Opra2_4_7 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_5, Opra2_6_4, Opra2_6_3, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_6
                         , Opra2_2_5, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_4_7 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_s_6, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_4_2, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_1_7
                         , Opra2_0_6 ] )
        , ( ( Opra2_4_7 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_s_5
                         , Opra2_4_3, Opra2_3_7, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_2_1
                         , Opra2_2_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_4_7 , Opra2_s_0 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_4_7 , Opra2_s_1 )
          , Set.fromList [ Opra2_4_7, Opra2_4_6, Opra2_4_5 ] )
        , ( ( Opra2_4_7 , Opra2_s_2 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_4_7 , Opra2_s_3 )
          , Set.fromList [ Opra2_4_5, Opra2_4_4, Opra2_4_3 ] )
        , ( ( Opra2_4_7 , Opra2_s_4 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_4_7 , Opra2_s_5 )
          , Set.fromList [ Opra2_4_3, Opra2_4_2, Opra2_4_1 ] )
        , ( ( Opra2_4_7 , Opra2_s_6 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_4_7 , Opra2_s_7 )
          , Set.fromList [ Opra2_4_7, Opra2_4_1, Opra2_4_0 ] )
        , ( ( Opra2_5_0 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_4, Opra2_s_5, Opra2_1_0 ] )
        , ( ( Opra2_5_0 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_5, Opra2_s_5, Opra2_s_4, Opra2_s_3
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_0 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_6, Opra2_s_3, Opra2_1_2 ] )
        , ( ( Opra2_5_0 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_7, Opra2_s_3, Opra2_s_2, Opra2_1_3
                         , Opra2_s_1 ] )
        , ( ( Opra2_5_0 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_0, Opra2_1_4, Opra2_s_1 ] )
        , ( ( Opra2_5_0 , Opra2_0_5 )
          , Set.fromList [ Opra2_s_7, Opra2_5_1, Opra2_1_5, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_5_0 , Opra2_0_6 )
          , Set.fromList [ Opra2_s_7, Opra2_5_2, Opra2_1_6 ] )
        , ( ( Opra2_5_0 , Opra2_0_7 )
          , Set.fromList [ Opra2_s_7, Opra2_s_6, Opra2_5_3, Opra2_s_5
                         , Opra2_1_7 ] )
        , ( ( Opra2_5_0 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_5_0 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_0 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_5_0 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_5_0 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_5_0 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_5_0 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_5_0 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_5_0 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_0 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_0 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_0 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_0 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_0 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_0 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_0 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_0 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_0 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_0 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_0 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_0 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_0 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_0 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_0 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_0 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_5_0 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_5_0 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_5_0 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_5_0 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_5_0 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_5_0 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_5_0 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_5_0 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_5_0 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_5_0 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_5_0 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_5_0 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_5_0 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_5_0 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_5_0 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_5_0 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_5_0 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_5_0 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_5_0 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_5_0 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_5_0 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_5_0 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_5_0 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_5_0 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_5_0 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_5_0 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_5_0 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_0 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_5_0 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_0 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_5_0 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_0 , Opra2_s_0 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_5_0 , Opra2_s_1 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_5_0 , Opra2_s_2 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_5_0 , Opra2_s_3 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_5_0 , Opra2_s_4 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_5_0 , Opra2_s_5 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_5_0 , Opra2_s_6 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_5_0 , Opra2_s_7 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_5_1 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_5_1 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_5_1 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_5_1 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_1 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_5_1 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_1 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_5_1 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_1 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_s_5, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_5_1 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_s_5, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_s_4, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_s_3, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_5_1 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_1
                         , Opra2_5_0, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_1 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_6
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_s_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_s_2, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_s_1, Opra2_0_3
                         , Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_5_1 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_1 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_6_2, Opra2_6_1, Opra2_5_7
                         , Opra2_5_6, Opra2_5_5, Opra2_5_4, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3, Opra2_s_0 ] )
        , ( ( Opra2_5_1 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_5, Opra2_6_4, Opra2_6_3, Opra2_5_7
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_1
                         , Opra2_4_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_1 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_7, Opra2_6_6
                         , Opra2_6_5, Opra2_6_4, Opra2_6_3, Opra2_s_6
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_5_0
                         , Opra2_s_5, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_5_1 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_5_1 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_1 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_5_1 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_5_1 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_5_1 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_5_1 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_5_1 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_5_1 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_5_1 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_1 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_5_1 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_5_1 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_5_1 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_5_1 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_5_1 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_5_1 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_1 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_1 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_1 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_1 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_1 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_1 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_1 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_1 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_1 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_1 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_1 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_1 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_1 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_1 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_1 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_1 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_5_1 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_5_1 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_5_1 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_5_1 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_5_1 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_5_1 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_5_1 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_5_1 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_5_1 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_5_1 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_5_1 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_1 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_5_1 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_1 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_5_1 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_1 , Opra2_s_0 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_5_1 , Opra2_s_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_5_1 , Opra2_s_2 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_5_1 , Opra2_s_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_5_1 , Opra2_s_4 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_5_1 , Opra2_s_5 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_5_1 , Opra2_s_6 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_5_1 , Opra2_s_7 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_5_2 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_5_2 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_5_2 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_5_2 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_5_2 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_5_2 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_5_2 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_5_2 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_5_2 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_5_2 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_5_2 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_5_2 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_2 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_5_2 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_2 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_5_2 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_2 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_4, Opra2_s_5, Opra2_1_0 ] )
        , ( ( Opra2_5_2 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_5, Opra2_s_5, Opra2_s_4, Opra2_s_3
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_2 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_6, Opra2_s_3, Opra2_1_2 ] )
        , ( ( Opra2_5_2 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_7, Opra2_s_3, Opra2_s_2, Opra2_1_3
                         , Opra2_s_1 ] )
        , ( ( Opra2_5_2 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_0, Opra2_1_4, Opra2_s_1 ] )
        , ( ( Opra2_5_2 , Opra2_2_5 )
          , Set.fromList [ Opra2_s_7, Opra2_5_1, Opra2_1_5, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_5_2 , Opra2_2_6 )
          , Set.fromList [ Opra2_s_7, Opra2_5_2, Opra2_1_6 ] )
        , ( ( Opra2_5_2 , Opra2_2_7 )
          , Set.fromList [ Opra2_s_7, Opra2_s_6, Opra2_5_3, Opra2_s_5
                         , Opra2_1_7 ] )
        , ( ( Opra2_5_2 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_5_2 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_2 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_5_2 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_5_2 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_5_2 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_5_2 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_5_2 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_5_2 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_2 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_2 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_2 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_2 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_2 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_2 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_2 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_2 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_2 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_2 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_2 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_2 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_2 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_2 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_2 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_2 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_5_2 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_5_2 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_5_2 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_5_2 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_5_2 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_5_2 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_5_2 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_5_2 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_5_2 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_5_2 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_5_2 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_5_2 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_5_2 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_5_2 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_5_2 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_5_2 , Opra2_s_0 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_5_2 , Opra2_s_1 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_5_2 , Opra2_s_2 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_5_2 , Opra2_s_3 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_5_2 , Opra2_s_4 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_5_2 , Opra2_s_5 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_5_2 , Opra2_s_6 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_5_2 , Opra2_s_7 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_5_3 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_5_3 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_5_3 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_5_3 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_5_3 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_5_3 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_5_3 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_5_3 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_5_3 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_5_3 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_5_3 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_5_3 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_3 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_5_3 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_3 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_5_3 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_3 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_5_3 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_5_3 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_5_3 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_3 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_5_3 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_3 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_5_3 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_3 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_s_5, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_5_3 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_s_5, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_s_4, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_s_3, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_5_3 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_1
                         , Opra2_5_0, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_3 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_6
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_s_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_s_2, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_s_1, Opra2_0_3
                         , Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_5_3 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_3 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_6_2, Opra2_6_1, Opra2_5_7
                         , Opra2_5_6, Opra2_5_5, Opra2_5_4, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3, Opra2_s_0 ] )
        , ( ( Opra2_5_3 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_5, Opra2_6_4, Opra2_6_3, Opra2_5_7
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_1
                         , Opra2_4_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_3 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_7, Opra2_6_6
                         , Opra2_6_5, Opra2_6_4, Opra2_6_3, Opra2_s_6
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_5_0
                         , Opra2_s_5, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_5_3 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_5_3 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_3 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_5_3 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_5_3 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_5_3 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_5_3 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_5_3 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_5_3 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_5_3 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_3 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_5_3 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_5_3 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_5_3 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_5_3 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_5_3 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_5_3 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_3 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_3 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_3 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_3 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_3 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_3 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_3 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_3 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_3 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_3 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_3 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_3 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_3 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_3 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_3 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_3 , Opra2_s_0 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_5_3 , Opra2_s_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_5_3 , Opra2_s_2 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_5_3 , Opra2_s_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_5_3 , Opra2_s_4 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_5_3 , Opra2_s_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_5_3 , Opra2_s_6 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_5_3 , Opra2_s_7 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_5_4 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_5_4 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_5_4 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_5_4 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_5_4 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_5_4 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_5_4 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_5_4 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_5_4 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_5_4 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_5_4 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_5_4 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_5_4 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_5_4 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_5_4 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_5_4 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_5_4 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_5_4 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_5_4 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_5_4 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_5_4 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_5_4 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_5_4 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_5_4 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_5_4 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_5_4 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_5_4 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_5_4 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_4 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_5_4 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_4 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_5_4 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_4 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_4, Opra2_s_5, Opra2_1_0 ] )
        , ( ( Opra2_5_4 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_5, Opra2_s_5, Opra2_s_4, Opra2_s_3
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_4 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_6, Opra2_s_3, Opra2_1_2 ] )
        , ( ( Opra2_5_4 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_7, Opra2_s_3, Opra2_s_2, Opra2_1_3
                         , Opra2_s_1 ] )
        , ( ( Opra2_5_4 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_0, Opra2_1_4, Opra2_s_1 ] )
        , ( ( Opra2_5_4 , Opra2_4_5 )
          , Set.fromList [ Opra2_s_7, Opra2_5_1, Opra2_1_5, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_5_4 , Opra2_4_6 )
          , Set.fromList [ Opra2_s_7, Opra2_5_2, Opra2_1_6 ] )
        , ( ( Opra2_5_4 , Opra2_4_7 )
          , Set.fromList [ Opra2_s_7, Opra2_s_6, Opra2_5_3, Opra2_s_5
                         , Opra2_1_7 ] )
        , ( ( Opra2_5_4 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_5_4 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_4 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_5_4 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_5_4 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_5_4 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_5_4 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_5_4 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_5_4 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_4 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_4 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_4 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_4 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_4 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_4 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_4 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_4 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_4 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_4 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_4 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_4 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_4 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_4 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_4 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_4 , Opra2_s_0 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_5_4 , Opra2_s_1 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_5_4 , Opra2_s_2 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_5_4 , Opra2_s_3 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_5_4 , Opra2_s_4 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_5_4 , Opra2_s_5 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_5_4 , Opra2_s_6 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_5_4 , Opra2_s_7 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_5_5 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_5 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_5 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_5 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_5 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_5 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_5 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_5 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_5 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_5 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_5 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_5 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_5 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_5 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_5 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_5 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_5 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_5_5 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_5_5 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_5_5 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_5_5 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_5_5 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_5_5 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_5_5 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_5_5 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_5_5 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_5_5 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_5_5 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_5 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_5_5 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_5 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_5_5 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_5 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_5_5 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_5_5 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_5_5 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_5 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_5_5 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_5 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_5_5 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_5 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_s_5, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_5_5 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_s_5, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_s_4, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_s_3, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_5_5 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_1
                         , Opra2_5_0, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_5 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_6
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_s_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_s_2, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_s_1, Opra2_0_3
                         , Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_5_5 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_5 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_6_2, Opra2_6_1, Opra2_5_7
                         , Opra2_5_6, Opra2_5_5, Opra2_5_4, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3, Opra2_s_0 ] )
        , ( ( Opra2_5_5 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_5, Opra2_6_4, Opra2_6_3, Opra2_5_7
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_1
                         , Opra2_4_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_5 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_7, Opra2_6_6
                         , Opra2_6_5, Opra2_6_4, Opra2_6_3, Opra2_s_6
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_5_0
                         , Opra2_s_5, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_5_5 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_5_5 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_5 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_5_5 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_5_5 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_5_5 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_5_5 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_5_5 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_5_5 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_5_5 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_5 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_5_5 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_5_5 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_5_5 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_5_5 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_5_5 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_5_5 , Opra2_s_0 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_5_5 , Opra2_s_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_5_5 , Opra2_s_2 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_5_5 , Opra2_s_3 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_5_5 , Opra2_s_4 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_5_5 , Opra2_s_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_5_5 , Opra2_s_6 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_5_5 , Opra2_s_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_5_6 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_6 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_6 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_6 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_6 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_6 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_6 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_6 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_6 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_6 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_6 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_6 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_6 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_6 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_6 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_6 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_6 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_5_6 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_5_6 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_5_6 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_5_6 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_5_6 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_5_6 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_5_6 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_5_6 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_5_6 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_5_6 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_5_6 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_5_6 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_5_6 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_5_6 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_5_6 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_5_6 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_5_6 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_5_6 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_5_6 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_5_6 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_5_6 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_5_6 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_5_6 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_5_6 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_5_6 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_5_6 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_5_6 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_6 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_5_6 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_6 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_5_6 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_6 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_4, Opra2_s_5, Opra2_1_0 ] )
        , ( ( Opra2_5_6 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_5, Opra2_s_5, Opra2_s_4, Opra2_s_3
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_6 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_6, Opra2_s_3, Opra2_1_2 ] )
        , ( ( Opra2_5_6 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_7, Opra2_s_3, Opra2_s_2, Opra2_1_3
                         , Opra2_s_1 ] )
        , ( ( Opra2_5_6 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_0, Opra2_1_4, Opra2_s_1 ] )
        , ( ( Opra2_5_6 , Opra2_6_5 )
          , Set.fromList [ Opra2_s_7, Opra2_5_1, Opra2_1_5, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_5_6 , Opra2_6_6 )
          , Set.fromList [ Opra2_s_7, Opra2_5_2, Opra2_1_6 ] )
        , ( ( Opra2_5_6 , Opra2_6_7 )
          , Set.fromList [ Opra2_s_7, Opra2_s_6, Opra2_5_3, Opra2_s_5
                         , Opra2_1_7 ] )
        , ( ( Opra2_5_6 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_5_6 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_6 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_5_6 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_5_6 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_5_6 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_5_6 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_5_6 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_5_6 , Opra2_s_0 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_5_6 , Opra2_s_1 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_5_6 , Opra2_s_2 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_5_6 , Opra2_s_3 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_5_6 , Opra2_s_4 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_5_6 , Opra2_s_5 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_5_6 , Opra2_s_6 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_5_6 , Opra2_s_7 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_5_7 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_5_7 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_7 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_5_7 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_5_7 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_5_7 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_5_7 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_5_7 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_5_7 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_5_7 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_3, Opra2_1_2
                         , Opra2_1_1 ] )
        , ( ( Opra2_5_7 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_5_7 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3 ] )
        , ( ( Opra2_5_7 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_5_7 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5 ] )
        , ( ( Opra2_5_7 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_5_7 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_1
                         , Opra2_1_0 ] )
        , ( ( Opra2_5_7 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_7 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_7 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_7 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_7 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_7 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_7 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_7 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_7 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_5_7 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_5_7 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_5_7 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_5_7 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_5_7 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_5_7 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_5_7 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_5_7 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_5_7 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_5_7 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_5_7 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_5_7 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_5_7 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_5_7 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_5_7 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_5_7 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_5_7 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_5_7 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_5_7 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_7 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_5_7 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_7 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_5_7 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_7 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_5_7 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_5_7 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_5_7 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_7 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_5_7 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_7 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_5_7 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_7 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_s_5, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7 ] )
        , ( ( Opra2_5_7 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_s_5, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_s_4, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_s_3, Opra2_2_3
                         , Opra2_2_2, Opra2_2_1, Opra2_1_7, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_5_7 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_1
                         , Opra2_5_0, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3, Opra2_s_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_5_7 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_6
                         , Opra2_4_5, Opra2_4_4, Opra2_4_3, Opra2_3_7
                         , Opra2_3_6, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_s_3, Opra2_2_5, Opra2_2_4, Opra2_2_3
                         , Opra2_s_2, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_s_1, Opra2_0_3
                         , Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_5_7 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_2_5
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_3 ] )
        , ( ( Opra2_5_7 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_s_7, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_6_2, Opra2_6_1, Opra2_5_7
                         , Opra2_5_6, Opra2_5_5, Opra2_5_4, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_6, Opra2_2_5
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3, Opra2_s_0 ] )
        , ( ( Opra2_5_7 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_s_7
                         , Opra2_6_5, Opra2_6_4, Opra2_6_3, Opra2_5_7
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_1
                         , Opra2_4_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_5_7 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_7, Opra2_6_6
                         , Opra2_6_5, Opra2_6_4, Opra2_6_3, Opra2_s_6
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_5_0
                         , Opra2_s_5, Opra2_4_7, Opra2_4_3, Opra2_4_2
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_1, Opra2_2_0, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_1, Opra2_1_0, Opra2_0_7
                         , Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_5_7 , Opra2_s_0 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_5_7 , Opra2_s_1 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_5_7 , Opra2_s_2 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_5_7 , Opra2_s_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_5_7 , Opra2_s_4 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_5_7 , Opra2_s_5 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_5_7 , Opra2_s_6 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_5_7 , Opra2_s_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_0 , Opra2_0_0 )
          , Set.fromList [ Opra2_6_4, Opra2_s_6, Opra2_2_0 ] )
        , ( ( Opra2_6_0 , Opra2_0_1 )
          , Set.fromList [ Opra2_6_5, Opra2_s_5, Opra2_2_1 ] )
        , ( ( Opra2_6_0 , Opra2_0_2 )
          , Set.fromList [ Opra2_6_6, Opra2_s_4, Opra2_2_2 ] )
        , ( ( Opra2_6_0 , Opra2_0_3 )
          , Set.fromList [ Opra2_6_7, Opra2_s_3, Opra2_2_3 ] )
        , ( ( Opra2_6_0 , Opra2_0_4 )
          , Set.fromList [ Opra2_6_0, Opra2_2_4, Opra2_s_2 ] )
        , ( ( Opra2_6_0 , Opra2_0_5 )
          , Set.fromList [ Opra2_6_1, Opra2_2_5, Opra2_s_1 ] )
        , ( ( Opra2_6_0 , Opra2_0_6 )
          , Set.fromList [ Opra2_6_2, Opra2_2_6, Opra2_s_0 ] )
        , ( ( Opra2_6_0 , Opra2_0_7 )
          , Set.fromList [ Opra2_s_7, Opra2_6_3, Opra2_2_7 ] )
        , ( ( Opra2_6_0 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_1 ] )
        , ( ( Opra2_6_0 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_6_0 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_3_3 ] )
        , ( ( Opra2_6_0 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_6_0 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5 ] )
        , ( ( Opra2_6_0 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_6_0 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7 ] )
        , ( ( Opra2_6_0 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_6_0 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_6_0 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_0 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_6_0 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_0 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_6_0 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_0 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_6_0 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_0 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_6_0 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_0 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_6_0 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_0 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_6_0 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_0 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_6_0 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_0 , Opra2_4_0 )
          , Set.fromList [ Opra2_6_0 ] )
        , ( ( Opra2_6_0 , Opra2_4_1 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_6_0 , Opra2_4_2 )
          , Set.fromList [ Opra2_6_2 ] )
        , ( ( Opra2_6_0 , Opra2_4_3 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_6_0 , Opra2_4_4 )
          , Set.fromList [ Opra2_6_4 ] )
        , ( ( Opra2_6_0 , Opra2_4_5 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_6_0 , Opra2_4_6 )
          , Set.fromList [ Opra2_6_6 ] )
        , ( ( Opra2_6_0 , Opra2_4_7 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_6_0 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_6_0 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_6_0 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_6_0 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_6_0 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_6_0 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_6_0 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_6_0 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_6_0 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_6_0 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_6_0 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_6_0 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_6_0 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_6_0 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_6_0 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_6_0 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_6_0 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_0 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_6_0 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_0 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_6_0 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_0 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_6_0 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_0 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_6_0 , Opra2_s_0 )
          , Set.fromList [ Opra2_6_0 ] )
        , ( ( Opra2_6_0 , Opra2_s_1 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_6_0 , Opra2_s_2 )
          , Set.fromList [ Opra2_6_6 ] )
        , ( ( Opra2_6_0 , Opra2_s_3 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_6_0 , Opra2_s_4 )
          , Set.fromList [ Opra2_6_4 ] )
        , ( ( Opra2_6_0 , Opra2_s_5 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_6_0 , Opra2_s_6 )
          , Set.fromList [ Opra2_6_2 ] )
        , ( ( Opra2_6_0 , Opra2_s_7 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_6_1 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_1 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_6_1 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_1 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_6_1 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_1 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_6_1 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_1 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_6_1 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_4
                         , Opra2_s_6, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_1, Opra2_2_0, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_1 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_s_5
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_6_1 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_s_4, Opra2_3_3, Opra2_2_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_1 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_s_3, Opra2_2_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_1 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5, Opra2_2_4, Opra2_s_2, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_1 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_1 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_2
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_2_6, Opra2_1_5, Opra2_0_5
                         , Opra2_s_0 ] )
        , ( ( Opra2_6_1 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_3, Opra2_5_7
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_5_0
                         , Opra2_4_7, Opra2_4_1, Opra2_4_0, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_1 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_1 ] )
        , ( ( Opra2_6_1 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_6_1 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_3_3 ] )
        , ( ( Opra2_6_1 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_6_1 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5 ] )
        , ( ( Opra2_6_1 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_6_1 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7 ] )
        , ( ( Opra2_6_1 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_6_1 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_1 ] )
        , ( ( Opra2_6_1 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_6_1 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_3_3 ] )
        , ( ( Opra2_6_1 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_6_1 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5 ] )
        , ( ( Opra2_6_1 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_6_1 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7 ] )
        , ( ( Opra2_6_1 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_6_1 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_6_1 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_1 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_6_1 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_1 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_6_1 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_1 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_6_1 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_1 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_0, Opra2_5_1 ] )
        , ( ( Opra2_6_1 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_1 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_2, Opra2_5_3 ] )
        , ( ( Opra2_6_1 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_1 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_4, Opra2_5_5 ] )
        , ( ( Opra2_6_1 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_1 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_6, Opra2_5_7 ] )
        , ( ( Opra2_6_1 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_1 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_6_1 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_6_1 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_6_1 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_6_1 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_6_1 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_6_1 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_6_1 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_6_1 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_1 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_6_1 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_1 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_6_1 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_1 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_6_1 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_1 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_6_1 , Opra2_s_0 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_6_1 , Opra2_s_1 )
          , Set.fromList [ Opra2_6_7, Opra2_6_1, Opra2_6_0 ] )
        , ( ( Opra2_6_1 , Opra2_s_2 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_6_1 , Opra2_s_3 )
          , Set.fromList [ Opra2_6_7, Opra2_6_6, Opra2_6_5 ] )
        , ( ( Opra2_6_1 , Opra2_s_4 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_6_1 , Opra2_s_5 )
          , Set.fromList [ Opra2_6_5, Opra2_6_4, Opra2_6_3 ] )
        , ( ( Opra2_6_1 , Opra2_s_6 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_6_1 , Opra2_s_7 )
          , Set.fromList [ Opra2_6_3, Opra2_6_2, Opra2_6_1 ] )
        , ( ( Opra2_6_2 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_6_2 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_6_2 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_6_2 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_6_2 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_6_2 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_6_2 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_6_2 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_6_2 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_2 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_6_2 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_2 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_6_2 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_2 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_6_2 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_2 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_6_2 , Opra2_2_0 )
          , Set.fromList [ Opra2_6_4, Opra2_s_6, Opra2_2_0 ] )
        , ( ( Opra2_6_2 , Opra2_2_1 )
          , Set.fromList [ Opra2_6_5, Opra2_s_5, Opra2_2_1 ] )
        , ( ( Opra2_6_2 , Opra2_2_2 )
          , Set.fromList [ Opra2_6_6, Opra2_s_4, Opra2_2_2 ] )
        , ( ( Opra2_6_2 , Opra2_2_3 )
          , Set.fromList [ Opra2_6_7, Opra2_s_3, Opra2_2_3 ] )
        , ( ( Opra2_6_2 , Opra2_2_4 )
          , Set.fromList [ Opra2_6_0, Opra2_2_4, Opra2_s_2 ] )
        , ( ( Opra2_6_2 , Opra2_2_5 )
          , Set.fromList [ Opra2_6_1, Opra2_2_5, Opra2_s_1 ] )
        , ( ( Opra2_6_2 , Opra2_2_6 )
          , Set.fromList [ Opra2_6_2, Opra2_2_6, Opra2_s_0 ] )
        , ( ( Opra2_6_2 , Opra2_2_7 )
          , Set.fromList [ Opra2_s_7, Opra2_6_3, Opra2_2_7 ] )
        , ( ( Opra2_6_2 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_1 ] )
        , ( ( Opra2_6_2 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_6_2 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_3_3 ] )
        , ( ( Opra2_6_2 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_6_2 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5 ] )
        , ( ( Opra2_6_2 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_6_2 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7 ] )
        , ( ( Opra2_6_2 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_6_2 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_6_2 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_2 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_6_2 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_2 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_6_2 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_2 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_6_2 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_2 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_6_2 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_2 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_6_2 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_2 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_6_2 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_2 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_6_2 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_2 , Opra2_6_0 )
          , Set.fromList [ Opra2_6_0 ] )
        , ( ( Opra2_6_2 , Opra2_6_1 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_6_2 , Opra2_6_2 )
          , Set.fromList [ Opra2_6_2 ] )
        , ( ( Opra2_6_2 , Opra2_6_3 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_6_2 , Opra2_6_4 )
          , Set.fromList [ Opra2_6_4 ] )
        , ( ( Opra2_6_2 , Opra2_6_5 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_6_2 , Opra2_6_6 )
          , Set.fromList [ Opra2_6_6 ] )
        , ( ( Opra2_6_2 , Opra2_6_7 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_6_2 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_6_2 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_6_2 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_6_2 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_6_2 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_6_2 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_6_2 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_6_2 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_6_2 , Opra2_s_0 )
          , Set.fromList [ Opra2_6_2 ] )
        , ( ( Opra2_6_2 , Opra2_s_1 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_6_2 , Opra2_s_2 )
          , Set.fromList [ Opra2_6_0 ] )
        , ( ( Opra2_6_2 , Opra2_s_3 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_6_2 , Opra2_s_4 )
          , Set.fromList [ Opra2_6_6 ] )
        , ( ( Opra2_6_2 , Opra2_s_5 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_6_2 , Opra2_s_6 )
          , Set.fromList [ Opra2_6_4 ] )
        , ( ( Opra2_6_2 , Opra2_s_7 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_6_3 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_6_3 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_6_3 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_6_3 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_6_3 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_6_3 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_6_3 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_6_3 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_6_3 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_3 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_6_3 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_3 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_6_3 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_3 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_6_3 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_3 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_6_3 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_3 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_6_3 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_3 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_6_3 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_3 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_6_3 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_3 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_6_3 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_4
                         , Opra2_s_6, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_1, Opra2_2_0, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_3 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_s_5
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_6_3 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_s_4, Opra2_3_3, Opra2_2_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_3 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_s_3, Opra2_2_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_3 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5, Opra2_2_4, Opra2_s_2, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_3 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_3 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_2
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_2_6, Opra2_1_5, Opra2_0_5
                         , Opra2_s_0 ] )
        , ( ( Opra2_6_3 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_3, Opra2_5_7
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_5_0
                         , Opra2_4_7, Opra2_4_1, Opra2_4_0, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_3 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_1 ] )
        , ( ( Opra2_6_3 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_6_3 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_3_3 ] )
        , ( ( Opra2_6_3 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_6_3 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5 ] )
        , ( ( Opra2_6_3 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_6_3 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7 ] )
        , ( ( Opra2_6_3 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_6_3 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_1 ] )
        , ( ( Opra2_6_3 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_6_3 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_3_3 ] )
        , ( ( Opra2_6_3 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_6_3 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5 ] )
        , ( ( Opra2_6_3 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_6_3 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7 ] )
        , ( ( Opra2_6_3 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_6_3 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_6_3 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_3 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_6_3 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_3 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_6_3 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_3 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_6_3 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_3 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_0, Opra2_5_1 ] )
        , ( ( Opra2_6_3 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_3 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_2, Opra2_5_3 ] )
        , ( ( Opra2_6_3 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_3 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_4, Opra2_5_5 ] )
        , ( ( Opra2_6_3 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_3 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_6, Opra2_5_7 ] )
        , ( ( Opra2_6_3 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_3 , Opra2_s_0 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_6_3 , Opra2_s_1 )
          , Set.fromList [ Opra2_6_3, Opra2_6_2, Opra2_6_1 ] )
        , ( ( Opra2_6_3 , Opra2_s_2 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_6_3 , Opra2_s_3 )
          , Set.fromList [ Opra2_6_7, Opra2_6_1, Opra2_6_0 ] )
        , ( ( Opra2_6_3 , Opra2_s_4 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_6_3 , Opra2_s_5 )
          , Set.fromList [ Opra2_6_7, Opra2_6_6, Opra2_6_5 ] )
        , ( ( Opra2_6_3 , Opra2_s_6 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_6_3 , Opra2_s_7 )
          , Set.fromList [ Opra2_6_5, Opra2_6_4, Opra2_6_3 ] )
        , ( ( Opra2_6_4 , Opra2_0_0 )
          , Set.fromList [ Opra2_6_0 ] )
        , ( ( Opra2_6_4 , Opra2_0_1 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_6_4 , Opra2_0_2 )
          , Set.fromList [ Opra2_6_2 ] )
        , ( ( Opra2_6_4 , Opra2_0_3 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_6_4 , Opra2_0_4 )
          , Set.fromList [ Opra2_6_4 ] )
        , ( ( Opra2_6_4 , Opra2_0_5 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_6_4 , Opra2_0_6 )
          , Set.fromList [ Opra2_6_6 ] )
        , ( ( Opra2_6_4 , Opra2_0_7 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_6_4 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_6_4 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_6_4 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_6_4 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_6_4 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_6_4 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_6_4 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_6_4 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_6_4 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_6_4 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_6_4 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_6_4 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_6_4 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_6_4 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_6_4 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_6_4 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_6_4 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_4 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_6_4 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_4 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_6_4 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_4 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_6_4 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_4 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_6_4 , Opra2_4_0 )
          , Set.fromList [ Opra2_6_4, Opra2_s_6, Opra2_2_0 ] )
        , ( ( Opra2_6_4 , Opra2_4_1 )
          , Set.fromList [ Opra2_6_5, Opra2_s_5, Opra2_2_1 ] )
        , ( ( Opra2_6_4 , Opra2_4_2 )
          , Set.fromList [ Opra2_6_6, Opra2_s_4, Opra2_2_2 ] )
        , ( ( Opra2_6_4 , Opra2_4_3 )
          , Set.fromList [ Opra2_6_7, Opra2_s_3, Opra2_2_3 ] )
        , ( ( Opra2_6_4 , Opra2_4_4 )
          , Set.fromList [ Opra2_6_0, Opra2_2_4, Opra2_s_2 ] )
        , ( ( Opra2_6_4 , Opra2_4_5 )
          , Set.fromList [ Opra2_6_1, Opra2_2_5, Opra2_s_1 ] )
        , ( ( Opra2_6_4 , Opra2_4_6 )
          , Set.fromList [ Opra2_6_2, Opra2_2_6, Opra2_s_0 ] )
        , ( ( Opra2_6_4 , Opra2_4_7 )
          , Set.fromList [ Opra2_s_7, Opra2_6_3, Opra2_2_7 ] )
        , ( ( Opra2_6_4 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_1 ] )
        , ( ( Opra2_6_4 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_6_4 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_3_3 ] )
        , ( ( Opra2_6_4 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_6_4 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5 ] )
        , ( ( Opra2_6_4 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_6_4 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7 ] )
        , ( ( Opra2_6_4 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_6_4 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_6_4 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_4 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_6_4 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_4 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_6_4 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_4 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_6_4 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_4 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_6_4 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_4 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_6_4 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_4 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_6_4 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_4 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_6_4 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_4 , Opra2_s_0 )
          , Set.fromList [ Opra2_6_4 ] )
        , ( ( Opra2_6_4 , Opra2_s_1 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_6_4 , Opra2_s_2 )
          , Set.fromList [ Opra2_6_2 ] )
        , ( ( Opra2_6_4 , Opra2_s_3 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_6_4 , Opra2_s_4 )
          , Set.fromList [ Opra2_6_0 ] )
        , ( ( Opra2_6_4 , Opra2_s_5 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_6_4 , Opra2_s_6 )
          , Set.fromList [ Opra2_6_6 ] )
        , ( ( Opra2_6_4 , Opra2_s_7 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_6_5 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_6_5 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_5 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_6_5 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_5 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_6_5 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_5 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_6_5 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_5 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_0, Opra2_5_1 ] )
        , ( ( Opra2_6_5 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_5 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_2, Opra2_5_3 ] )
        , ( ( Opra2_6_5 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_5 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_4, Opra2_5_5 ] )
        , ( ( Opra2_6_5 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_5 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_6, Opra2_5_7 ] )
        , ( ( Opra2_6_5 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_5 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_6_5 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_6_5 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_6_5 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_6_5 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_6_5 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_6_5 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_6_5 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_6_5 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_5 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_6_5 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_5 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_6_5 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_5 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_6_5 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_5 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_6_5 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_5 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_6_5 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_5 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_6_5 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_5 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_6_5 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_5 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_6_5 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_4
                         , Opra2_s_6, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_1, Opra2_2_0, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_5 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_s_5
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_6_5 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_s_4, Opra2_3_3, Opra2_2_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_5 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_s_3, Opra2_2_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_5 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5, Opra2_2_4, Opra2_s_2, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_5 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_5 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_2
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_2_6, Opra2_1_5, Opra2_0_5
                         , Opra2_s_0 ] )
        , ( ( Opra2_6_5 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_3, Opra2_5_7
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_5_0
                         , Opra2_4_7, Opra2_4_1, Opra2_4_0, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_5 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_1 ] )
        , ( ( Opra2_6_5 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_6_5 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_3_3 ] )
        , ( ( Opra2_6_5 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_6_5 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5 ] )
        , ( ( Opra2_6_5 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_6_5 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7 ] )
        , ( ( Opra2_6_5 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_6_5 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_1 ] )
        , ( ( Opra2_6_5 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_6_5 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_3_3 ] )
        , ( ( Opra2_6_5 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_6_5 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5 ] )
        , ( ( Opra2_6_5 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_6_5 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7 ] )
        , ( ( Opra2_6_5 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_6_5 , Opra2_s_0 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_6_5 , Opra2_s_1 )
          , Set.fromList [ Opra2_6_5, Opra2_6_4, Opra2_6_3 ] )
        , ( ( Opra2_6_5 , Opra2_s_2 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_6_5 , Opra2_s_3 )
          , Set.fromList [ Opra2_6_3, Opra2_6_2, Opra2_6_1 ] )
        , ( ( Opra2_6_5 , Opra2_s_4 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_6_5 , Opra2_s_5 )
          , Set.fromList [ Opra2_6_7, Opra2_6_1, Opra2_6_0 ] )
        , ( ( Opra2_6_5 , Opra2_s_6 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_6_5 , Opra2_s_7 )
          , Set.fromList [ Opra2_6_7, Opra2_6_6, Opra2_6_5 ] )
        , ( ( Opra2_6_6 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_6_6 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_6 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_6_6 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_6 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_6_6 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_6 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_6_6 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_6 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_6_6 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_6 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_6_6 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_6 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_6_6 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_6 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_6_6 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_6 , Opra2_2_0 )
          , Set.fromList [ Opra2_6_0 ] )
        , ( ( Opra2_6_6 , Opra2_2_1 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_6_6 , Opra2_2_2 )
          , Set.fromList [ Opra2_6_2 ] )
        , ( ( Opra2_6_6 , Opra2_2_3 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_6_6 , Opra2_2_4 )
          , Set.fromList [ Opra2_6_4 ] )
        , ( ( Opra2_6_6 , Opra2_2_5 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_6_6 , Opra2_2_6 )
          , Set.fromList [ Opra2_6_6 ] )
        , ( ( Opra2_6_6 , Opra2_2_7 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_6_6 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_6_6 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_6_6 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_6_6 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_6_6 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_6_6 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_6_6 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_6_6 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_6_6 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_6_6 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_6_6 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_6_6 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_6_6 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_6_6 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_6_6 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_6_6 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_6_6 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_6 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_6_6 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_6 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_6_6 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_6 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_6_6 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_6 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_6_6 , Opra2_6_0 )
          , Set.fromList [ Opra2_6_4, Opra2_s_6, Opra2_2_0 ] )
        , ( ( Opra2_6_6 , Opra2_6_1 )
          , Set.fromList [ Opra2_6_5, Opra2_s_5, Opra2_2_1 ] )
        , ( ( Opra2_6_6 , Opra2_6_2 )
          , Set.fromList [ Opra2_6_6, Opra2_s_4, Opra2_2_2 ] )
        , ( ( Opra2_6_6 , Opra2_6_3 )
          , Set.fromList [ Opra2_6_7, Opra2_s_3, Opra2_2_3 ] )
        , ( ( Opra2_6_6 , Opra2_6_4 )
          , Set.fromList [ Opra2_6_0, Opra2_2_4, Opra2_s_2 ] )
        , ( ( Opra2_6_6 , Opra2_6_5 )
          , Set.fromList [ Opra2_6_1, Opra2_2_5, Opra2_s_1 ] )
        , ( ( Opra2_6_6 , Opra2_6_6 )
          , Set.fromList [ Opra2_6_2, Opra2_2_6, Opra2_s_0 ] )
        , ( ( Opra2_6_6 , Opra2_6_7 )
          , Set.fromList [ Opra2_s_7, Opra2_6_3, Opra2_2_7 ] )
        , ( ( Opra2_6_6 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_1 ] )
        , ( ( Opra2_6_6 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_6_6 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_3_3 ] )
        , ( ( Opra2_6_6 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_6_6 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5 ] )
        , ( ( Opra2_6_6 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_6_6 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7 ] )
        , ( ( Opra2_6_6 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_6_6 , Opra2_s_0 )
          , Set.fromList [ Opra2_6_6 ] )
        , ( ( Opra2_6_6 , Opra2_s_1 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_6_6 , Opra2_s_2 )
          , Set.fromList [ Opra2_6_4 ] )
        , ( ( Opra2_6_6 , Opra2_s_3 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_6_6 , Opra2_s_4 )
          , Set.fromList [ Opra2_6_2 ] )
        , ( ( Opra2_6_6 , Opra2_s_5 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_6_6 , Opra2_s_6 )
          , Set.fromList [ Opra2_6_0 ] )
        , ( ( Opra2_6_6 , Opra2_s_7 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_6_7 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_1 ] )
        , ( ( Opra2_6_7 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_6_7 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_3_3 ] )
        , ( ( Opra2_6_7 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_6_7 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5 ] )
        , ( ( Opra2_6_7 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_6_7 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7 ] )
        , ( ( Opra2_6_7 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_6_7 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_1
                         , Opra2_3_1 ] )
        , ( ( Opra2_6_7 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_3, Opra2_3_2, Opra2_3_1 ] )
        , ( ( Opra2_6_7 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_3_3 ] )
        , ( ( Opra2_6_7 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_5, Opra2_4_4, Opra2_4_3
                         , Opra2_3_5, Opra2_3_4, Opra2_3_3 ] )
        , ( ( Opra2_6_7 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5 ] )
        , ( ( Opra2_6_7 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_6, Opra2_4_5
                         , Opra2_3_7, Opra2_3_6, Opra2_3_5 ] )
        , ( ( Opra2_6_7 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7 ] )
        , ( ( Opra2_6_7 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_4_1, Opra2_4_0
                         , Opra2_3_7, Opra2_3_1, Opra2_3_0 ] )
        , ( ( Opra2_6_7 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_6_7 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_7 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_6_7 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_7 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_6_7 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_7 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_6_7 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_7 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_6_0, Opra2_5_1 ] )
        , ( ( Opra2_6_7 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1 ] )
        , ( ( Opra2_6_7 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_1, Opra2_6_2, Opra2_5_3 ] )
        , ( ( Opra2_6_7 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3 ] )
        , ( ( Opra2_6_7 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_6_4, Opra2_5_5 ] )
        , ( ( Opra2_6_7 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5 ] )
        , ( ( Opra2_6_7 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_6_6, Opra2_5_7 ] )
        , ( ( Opra2_6_7 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0 ] )
        , ( ( Opra2_6_7 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_6_7 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_6_7 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_6_7 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_6_7 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_6_7 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_6_7 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_6_7 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_6_7 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_7 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_6_7 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_7 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_6_7 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_7 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_6_7 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_7 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_6_7 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_7 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_6_7 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_7 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_6_7 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_7 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_6_7 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_7 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_6_7 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_4
                         , Opra2_s_6, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_4_1, Opra2_3_1, Opra2_2_0, Opra2_1_7
                         , Opra2_0_7 ] )
        , ( ( Opra2_6_7 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_5, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_s_5
                         , Opra2_4_3, Opra2_4_2, Opra2_4_1, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_6_7 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_3
                         , Opra2_s_4, Opra2_3_3, Opra2_2_2, Opra2_1_1
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_7 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_s_3, Opra2_2_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_6_7 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_4_5
                         , Opra2_3_5, Opra2_2_4, Opra2_s_2, Opra2_1_3
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_7 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_1, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_6_7 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_2
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_3_7, Opra2_2_6, Opra2_1_5, Opra2_0_5
                         , Opra2_s_0 ] )
        , ( ( Opra2_6_7 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_s_7, Opra2_6_3, Opra2_5_7
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_5_0
                         , Opra2_4_7, Opra2_4_1, Opra2_4_0, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_6_7 , Opra2_s_0 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_6_7 , Opra2_s_1 )
          , Set.fromList [ Opra2_6_7, Opra2_6_6, Opra2_6_5 ] )
        , ( ( Opra2_6_7 , Opra2_s_2 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_6_7 , Opra2_s_3 )
          , Set.fromList [ Opra2_6_5, Opra2_6_4, Opra2_6_3 ] )
        , ( ( Opra2_6_7 , Opra2_s_4 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_6_7 , Opra2_s_5 )
          , Set.fromList [ Opra2_6_3, Opra2_6_2, Opra2_6_1 ] )
        , ( ( Opra2_6_7 , Opra2_s_6 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_6_7 , Opra2_s_7 )
          , Set.fromList [ Opra2_6_7, Opra2_6_1, Opra2_6_0 ] )
        , ( ( Opra2_7_0 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_4, Opra2_s_7, Opra2_3_0 ] )
        , ( ( Opra2_7_0 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_5, Opra2_s_7, Opra2_s_6, Opra2_s_5
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_0 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_6, Opra2_s_5, Opra2_3_2 ] )
        , ( ( Opra2_7_0 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_7, Opra2_s_5, Opra2_s_4, Opra2_3_3
                         , Opra2_s_3 ] )
        , ( ( Opra2_7_0 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_0, Opra2_3_4, Opra2_s_3 ] )
        , ( ( Opra2_7_0 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_1, Opra2_3_5, Opra2_s_3, Opra2_s_2
                         , Opra2_s_1 ] )
        , ( ( Opra2_7_0 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_2, Opra2_3_6, Opra2_s_1 ] )
        , ( ( Opra2_7_0 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_3, Opra2_s_7, Opra2_3_7, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_7_0 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_7_0 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_0 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_7_0 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_7_0 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_7_0 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_7_0 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_7_0 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_7_0 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_7_0 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_7_0 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_7_0 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_7_0 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_7_0 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_7_0 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_7_0 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_7_0 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_7_0 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_7_0 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_7_0 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_7_0 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_7_0 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_7_0 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_7_0 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_7_0 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_7_0 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_7_0 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_7_0 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_7_0 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_7_0 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_7_0 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_7_0 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_7_0 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_0 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_0 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_0 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_0 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_0 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_0 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_0 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_0 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_0 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_0 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_0 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_0 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_0 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_0 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_0 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_0 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_7_0 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_0 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_0 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_0 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_0 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_0 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_5
                         , Opra2_2_5, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_7_0 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_0 , Opra2_s_0 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_7_0 , Opra2_s_1 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_7_0 , Opra2_s_2 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_7_0 , Opra2_s_3 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_7_0 , Opra2_s_4 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_7_0 , Opra2_s_5 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_7_0 , Opra2_s_6 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_7_0 , Opra2_s_7 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_7_1 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_7_1 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_1 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_1 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_1 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_1 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_1 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_5
                         , Opra2_2_5, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_7_1 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_1 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_s_7
                         , Opra2_6_3, Opra2_6_2, Opra2_6_1, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_4_1, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_1 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_s_7, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_s_6, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_s_5, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_6, Opra2_0_5
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_1 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_1, Opra2_7_0, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_s_5, Opra2_4_3, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_1 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_6_7, Opra2_6_6, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_5_7, Opra2_5_6, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_s_5, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_s_4, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_s_3, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_1 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_3
                         , Opra2_7_2, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_s_3, Opra2_2_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_1 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_6_7, Opra2_6_6, Opra2_6_5, Opra2_6_1
                         , Opra2_6_0, Opra2_5_7, Opra2_5_6, Opra2_5_5
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_6
                         , Opra2_4_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_s_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_s_2, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_s_1, Opra2_0_5, Opra2_0_4, Opra2_0_3
                         , Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_1 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_5, Opra2_7_4, Opra2_7_3
                         , Opra2_7_2, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_1 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_s_7, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3, Opra2_s_0 ] )
        , ( ( Opra2_7_1 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_7_1 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_1 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_7_1 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_7_1 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_7_1 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_7_1 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_7_1 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_7_1 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_7_1 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_1 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_7_1 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_7_1 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_7_1 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_7_1 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_7_1 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_7_1 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_7_1 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_7_1 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_7_1 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_7_1 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_7_1 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_7_1 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_7_1 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_7_1 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_1
                         , Opra2_5_1, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_1 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_1 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_5_3, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_1 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_1 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_5_5, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_1 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_1 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_5_7, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_1 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_1 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_1 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_1 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_1 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_1 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_1 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_1 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_1 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_1 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_7_1 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_1 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_1 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_1 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_1 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_1 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_5
                         , Opra2_2_5, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_7_1 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_1 , Opra2_s_0 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_7_1 , Opra2_s_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_7_1 , Opra2_s_2 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_7_1 , Opra2_s_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_7_1 , Opra2_s_4 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_7_1 , Opra2_s_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_7_1 , Opra2_s_6 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_7_1 , Opra2_s_7 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_7_2 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_2 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_2 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_2 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_2 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_2 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_2 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_2 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_2 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_7_2 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_2 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_2 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_2 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_2 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_2 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_5
                         , Opra2_2_5, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_7_2 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_2 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_4, Opra2_s_7, Opra2_3_0 ] )
        , ( ( Opra2_7_2 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_5, Opra2_s_7, Opra2_s_6, Opra2_s_5
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_2 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_6, Opra2_s_5, Opra2_3_2 ] )
        , ( ( Opra2_7_2 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_7, Opra2_s_5, Opra2_s_4, Opra2_3_3
                         , Opra2_s_3 ] )
        , ( ( Opra2_7_2 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_0, Opra2_3_4, Opra2_s_3 ] )
        , ( ( Opra2_7_2 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_1, Opra2_3_5, Opra2_s_3, Opra2_s_2
                         , Opra2_s_1 ] )
        , ( ( Opra2_7_2 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_2, Opra2_3_6, Opra2_s_1 ] )
        , ( ( Opra2_7_2 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_3, Opra2_s_7, Opra2_3_7, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_7_2 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_7_2 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_2 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_7_2 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_7_2 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_7_2 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_7_2 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_7_2 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_7_2 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_7_2 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_7_2 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_7_2 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_7_2 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_7_2 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_7_2 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_7_2 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_7_2 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_7_2 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_7_2 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_7_2 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_7_2 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_7_2 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_7_2 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_7_2 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_7_2 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_7_2 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_7_2 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_7_2 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_7_2 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_7_2 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_7_2 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_7_2 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_7_2 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_2 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_2 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_2 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_2 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_2 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_2 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_2 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_2 , Opra2_s_0 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_7_2 , Opra2_s_1 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_7_2 , Opra2_s_2 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_7_2 , Opra2_s_3 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_7_2 , Opra2_s_4 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_7_2 , Opra2_s_5 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_7_2 , Opra2_s_6 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_7_2 , Opra2_s_7 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_7_3 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_3 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_3 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_3 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_3 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_3 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_3 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_3 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_3 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_7_3 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_3 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_3 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_3 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_3 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_3 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_5
                         , Opra2_2_5, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_7_3 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_3 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_7_3 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_3 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_3 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_3 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_3 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_3 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_5
                         , Opra2_2_5, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_7_3 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_3 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_s_7
                         , Opra2_6_3, Opra2_6_2, Opra2_6_1, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_4_1, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_3 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_s_7, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_s_6, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_s_5, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_6, Opra2_0_5
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_3 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_1, Opra2_7_0, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_s_5, Opra2_4_3, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_3 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_6_7, Opra2_6_6, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_5_7, Opra2_5_6, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_s_5, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_s_4, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_s_3, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_3 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_3
                         , Opra2_7_2, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_s_3, Opra2_2_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_3 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_6_7, Opra2_6_6, Opra2_6_5, Opra2_6_1
                         , Opra2_6_0, Opra2_5_7, Opra2_5_6, Opra2_5_5
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_6
                         , Opra2_4_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_s_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_s_2, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_s_1, Opra2_0_5, Opra2_0_4, Opra2_0_3
                         , Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_3 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_5, Opra2_7_4, Opra2_7_3
                         , Opra2_7_2, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_3 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_s_7, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3, Opra2_s_0 ] )
        , ( ( Opra2_7_3 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_7_3 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_3 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_7_3 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_7_3 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_7_3 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_7_3 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_7_3 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_7_3 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_7_3 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_3 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_7_3 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_7_3 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_7_3 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_7_3 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_7_3 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_7_3 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_7_3 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_7_3 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_7_3 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_7_3 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_7_3 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_7_3 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_7_3 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_7_3 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_1
                         , Opra2_5_1, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_3 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_3 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_5_3, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_3 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_3 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_5_5, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_3 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_3 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_5_7, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_3 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_3 , Opra2_s_0 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_7_3 , Opra2_s_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_7_3 , Opra2_s_2 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_7_3 , Opra2_s_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_7_3 , Opra2_s_4 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_7_3 , Opra2_s_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_7_3 , Opra2_s_6 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_7_3 , Opra2_s_7 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_7_4 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_7_4 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_7_4 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_7_4 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_7_4 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_7_4 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_7_4 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_7_4 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_7_4 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_4 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_4 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_4 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_4 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_4 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_4 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_4 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_4 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_4 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_4 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_4 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_4 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_4 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_4 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_4 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_4 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_7_4 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_4 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_4 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_4 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_4 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_4 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_5
                         , Opra2_2_5, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_7_4 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_4 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_4, Opra2_s_7, Opra2_3_0 ] )
        , ( ( Opra2_7_4 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_5, Opra2_s_7, Opra2_s_6, Opra2_s_5
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_4 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_6, Opra2_s_5, Opra2_3_2 ] )
        , ( ( Opra2_7_4 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_7, Opra2_s_5, Opra2_s_4, Opra2_3_3
                         , Opra2_s_3 ] )
        , ( ( Opra2_7_4 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_0, Opra2_3_4, Opra2_s_3 ] )
        , ( ( Opra2_7_4 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_1, Opra2_3_5, Opra2_s_3, Opra2_s_2
                         , Opra2_s_1 ] )
        , ( ( Opra2_7_4 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_2, Opra2_3_6, Opra2_s_1 ] )
        , ( ( Opra2_7_4 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_3, Opra2_s_7, Opra2_3_7, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_7_4 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_7_4 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_4 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_7_4 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_7_4 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_7_4 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_7_4 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_7_4 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_7_4 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_7_4 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_7_4 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_7_4 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_7_4 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_7_4 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_7_4 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_7_4 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_7_4 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_7_4 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_7_4 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_7_4 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_7_4 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_7_4 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_7_4 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_7_4 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_7_4 , Opra2_s_0 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_7_4 , Opra2_s_1 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_7_4 , Opra2_s_2 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_7_4 , Opra2_s_3 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_7_4 , Opra2_s_4 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_7_4 , Opra2_s_5 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_7_4 , Opra2_s_6 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_7_4 , Opra2_s_7 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_7_5 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_7_5 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_7_5 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_7_5 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_7_5 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_7_5 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_7_5 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_7_5 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_7_5 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_1
                         , Opra2_5_1, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_5 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_5 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_5_3, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_5 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_5 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_5_5, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_5 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_5 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_5_7, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_5 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_5 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_5 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_5 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_5 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_5 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_5 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_5 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_5 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_5 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_7_5 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_5 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_5 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_5 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_5 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_5 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_5
                         , Opra2_2_5, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_7_5 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_5 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_7_5 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_5 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_5 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_5 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_5 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_5 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_5
                         , Opra2_2_5, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_7_5 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_5 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_s_7
                         , Opra2_6_3, Opra2_6_2, Opra2_6_1, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_4_1, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_5 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_s_7, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_s_6, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_s_5, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_6, Opra2_0_5
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_5 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_1, Opra2_7_0, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_s_5, Opra2_4_3, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_5 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_6_7, Opra2_6_6, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_5_7, Opra2_5_6, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_s_5, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_s_4, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_s_3, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_5 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_3
                         , Opra2_7_2, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_s_3, Opra2_2_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_5 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_6_7, Opra2_6_6, Opra2_6_5, Opra2_6_1
                         , Opra2_6_0, Opra2_5_7, Opra2_5_6, Opra2_5_5
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_6
                         , Opra2_4_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_s_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_s_2, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_s_1, Opra2_0_5, Opra2_0_4, Opra2_0_3
                         , Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_5 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_5, Opra2_7_4, Opra2_7_3
                         , Opra2_7_2, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_5 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_s_7, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3, Opra2_s_0 ] )
        , ( ( Opra2_7_5 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_7_5 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_5 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_7_5 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_7_5 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_7_5 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_7_5 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_7_5 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_7_5 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_7_5 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_5 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_7_5 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_7_5 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_7_5 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_7_5 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_7_5 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_7_5 , Opra2_s_0 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_7_5 , Opra2_s_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_7_5 , Opra2_s_2 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_7_5 , Opra2_s_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_7_5 , Opra2_s_4 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_7_5 , Opra2_s_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_7_5 , Opra2_s_6 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_7_5 , Opra2_s_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_7_6 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_7_6 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_7_6 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_7_6 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_7_6 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_7_6 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_7_6 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_7_6 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_7_6 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_7_6 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_7_6 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_7_6 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_7_6 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_7_6 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_7_6 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_7_6 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_7_6 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_7_6 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_7_6 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_7_6 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_7_6 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_7_6 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_7_6 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_7_6 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_7_6 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_6 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_6 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_6 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_6 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_6 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_6 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_6 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_6 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_6 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_6 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_6 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_6 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_6 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_6 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_6 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_6 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_7_6 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_6 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_6 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_6 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_6 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_6 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_5
                         , Opra2_2_5, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_7_6 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_6 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_4, Opra2_s_7, Opra2_3_0 ] )
        , ( ( Opra2_7_6 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_5, Opra2_s_7, Opra2_s_6, Opra2_s_5
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_6 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_6, Opra2_s_5, Opra2_3_2 ] )
        , ( ( Opra2_7_6 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_7, Opra2_s_5, Opra2_s_4, Opra2_3_3
                         , Opra2_s_3 ] )
        , ( ( Opra2_7_6 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_0, Opra2_3_4, Opra2_s_3 ] )
        , ( ( Opra2_7_6 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_1, Opra2_3_5, Opra2_s_3, Opra2_s_2
                         , Opra2_s_1 ] )
        , ( ( Opra2_7_6 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_2, Opra2_3_6, Opra2_s_1 ] )
        , ( ( Opra2_7_6 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_3, Opra2_s_7, Opra2_3_7, Opra2_s_1
                         , Opra2_s_0 ] )
        , ( ( Opra2_7_6 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_7_6 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_6 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_7_6 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_7_6 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_7_6 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_7_6 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_7_6 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_7_6 , Opra2_s_0 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_7_6 , Opra2_s_1 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_7_6 , Opra2_s_2 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_7_6 , Opra2_s_3 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_7_6 , Opra2_s_4 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_7_6 , Opra2_s_5 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_7_6 , Opra2_s_6 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_7_6 , Opra2_s_7 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_7_7 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_7_7 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_7 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_7_7 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_7_7 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_7_7 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_7_7 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_7_7 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_7_7 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_7_7 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_4_3
                         , Opra2_4_2, Opra2_4_1, Opra2_3_3, Opra2_3_2
                         , Opra2_3_1 ] )
        , ( ( Opra2_7_7 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_7_7 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3 ] )
        , ( ( Opra2_7_7 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_7_7 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_6, Opra2_4_5, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5 ] )
        , ( ( Opra2_7_7 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_7_7 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_1
                         , Opra2_3_0 ] )
        , ( ( Opra2_7_7 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_7_7 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_5_3, Opra2_5_2
                         , Opra2_5_1 ] )
        , ( ( Opra2_7_7 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_7_7 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3 ] )
        , ( ( Opra2_7_7 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_7_7 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5 ] )
        , ( ( Opra2_7_7 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_7_7 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0 ] )
        , ( ( Opra2_7_7 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_6_1
                         , Opra2_5_1, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_7 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_6_3, Opra2_6_2, Opra2_6_1
                         , Opra2_5_3, Opra2_5_2, Opra2_5_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_7 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_6_3
                         , Opra2_5_3, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_7 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_5_5, Opra2_5_4, Opra2_5_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_7 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_6_5
                         , Opra2_5_5, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_7 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_6_7, Opra2_6_6, Opra2_6_5
                         , Opra2_5_7, Opra2_5_6, Opra2_5_5, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_7 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_6_7
                         , Opra2_5_7, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_7 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_6_7, Opra2_6_1, Opra2_6_0
                         , Opra2_5_7, Opra2_5_1, Opra2_5_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_7 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_7_7 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_7 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_7_7 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_7 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_7_7 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_7 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_7_7 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_7 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_7_7 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_7 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_7 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_7 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_7 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_7 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_5
                         , Opra2_2_5, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_7_7 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_7 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_3_7
                         , Opra2_2_7, Opra2_1_7, Opra2_1_6, Opra2_1_5
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5 ] )
        , ( ( Opra2_7_7 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_1
                         , Opra2_7_0, Opra2_3_7, Opra2_3_1, Opra2_3_0
                         , Opra2_2_7, Opra2_2_1, Opra2_2_0, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_7 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0, Opra2_3_1
                         , Opra2_2_1, Opra2_1_7, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_7 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_3, Opra2_7_2, Opra2_7_1
                         , Opra2_7_0, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_2_3, Opra2_2_2, Opra2_2_1, Opra2_1_7
                         , Opra2_1_3, Opra2_1_2, Opra2_1_1, Opra2_1_0
                         , Opra2_0_7, Opra2_0_3, Opra2_0_2, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_7 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_3_3
                         , Opra2_2_3, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_3, Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_7 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_7_2
                         , Opra2_7_1, Opra2_3_5, Opra2_3_4, Opra2_3_3
                         , Opra2_2_5, Opra2_2_4, Opra2_2_3, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_7 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3, Opra2_3_5
                         , Opra2_2_5, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_5, Opra2_0_4, Opra2_0_3 ] )
        , ( ( Opra2_7_7 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_2_7, Opra2_2_6, Opra2_2_5, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_1_4, Opra2_1_3
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_7 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_s_7
                         , Opra2_6_3, Opra2_6_2, Opra2_6_1, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_4_1, Opra2_3_7
                         , Opra2_3_1, Opra2_3_0, Opra2_2_7, Opra2_1_7
                         , Opra2_1_6, Opra2_1_5, Opra2_0_7, Opra2_0_6
                         , Opra2_0_5 ] )
        , ( ( Opra2_7_7 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_s_7, Opra2_6_5, Opra2_6_4, Opra2_6_3
                         , Opra2_6_2, Opra2_6_1, Opra2_s_6, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_5_2, Opra2_5_1
                         , Opra2_s_5, Opra2_4_3, Opra2_4_2, Opra2_4_1
                         , Opra2_3_7, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_3_0, Opra2_2_7, Opra2_2_1, Opra2_2_0
                         , Opra2_1_7, Opra2_1_6, Opra2_1_5, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_6, Opra2_0_5
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_7 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_1, Opra2_7_0, Opra2_6_5
                         , Opra2_6_4, Opra2_6_3, Opra2_5_5, Opra2_5_4
                         , Opra2_5_3, Opra2_s_5, Opra2_4_3, Opra2_3_3
                         , Opra2_3_2, Opra2_3_1, Opra2_2_1, Opra2_1_7
                         , Opra2_1_1, Opra2_1_0, Opra2_0_7, Opra2_0_1
                         , Opra2_0_0 ] )
        , ( ( Opra2_7_7 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_6_7, Opra2_6_6, Opra2_6_5, Opra2_6_4
                         , Opra2_6_3, Opra2_5_7, Opra2_5_6, Opra2_5_5
                         , Opra2_5_4, Opra2_5_3, Opra2_s_5, Opra2_4_5
                         , Opra2_4_4, Opra2_4_3, Opra2_s_4, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_3_2, Opra2_3_1
                         , Opra2_s_3, Opra2_2_3, Opra2_2_2, Opra2_2_1
                         , Opra2_1_7, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_1_0, Opra2_0_7, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1, Opra2_0_0 ] )
        , ( ( Opra2_7_7 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_3
                         , Opra2_7_2, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_6, Opra2_6_5, Opra2_5_7, Opra2_5_6
                         , Opra2_5_5, Opra2_4_5, Opra2_3_5, Opra2_3_4
                         , Opra2_3_3, Opra2_s_3, Opra2_2_3, Opra2_1_3
                         , Opra2_1_2, Opra2_1_1, Opra2_0_3, Opra2_0_2
                         , Opra2_0_1 ] )
        , ( ( Opra2_7_7 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_6_7, Opra2_6_6, Opra2_6_5, Opra2_6_1
                         , Opra2_6_0, Opra2_5_7, Opra2_5_6, Opra2_5_5
                         , Opra2_5_1, Opra2_5_0, Opra2_4_7, Opra2_4_6
                         , Opra2_4_5, Opra2_3_7, Opra2_3_6, Opra2_3_5
                         , Opra2_3_4, Opra2_3_3, Opra2_s_3, Opra2_2_5
                         , Opra2_2_4, Opra2_2_3, Opra2_s_2, Opra2_1_5
                         , Opra2_1_4, Opra2_1_3, Opra2_1_2, Opra2_1_1
                         , Opra2_s_1, Opra2_0_5, Opra2_0_4, Opra2_0_3
                         , Opra2_0_2, Opra2_0_1 ] )
        , ( ( Opra2_7_7 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_7, Opra2_7_5, Opra2_7_4, Opra2_7_3
                         , Opra2_7_2, Opra2_7_1, Opra2_7_0, Opra2_6_7
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_1
                         , Opra2_5_0, Opra2_4_7, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_2_5, Opra2_1_5, Opra2_1_4
                         , Opra2_1_3, Opra2_s_1, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3 ] )
        , ( ( Opra2_7_7 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5, Opra2_7_4
                         , Opra2_7_3, Opra2_7_2, Opra2_7_1, Opra2_7_0
                         , Opra2_s_7, Opra2_6_7, Opra2_6_3, Opra2_6_2
                         , Opra2_6_1, Opra2_6_0, Opra2_5_7, Opra2_5_3
                         , Opra2_5_2, Opra2_5_1, Opra2_5_0, Opra2_4_7
                         , Opra2_4_1, Opra2_4_0, Opra2_3_7, Opra2_3_6
                         , Opra2_3_5, Opra2_3_1, Opra2_3_0, Opra2_2_7
                         , Opra2_2_6, Opra2_2_5, Opra2_1_7, Opra2_1_6
                         , Opra2_1_5, Opra2_1_4, Opra2_1_3, Opra2_s_1
                         , Opra2_0_7, Opra2_0_6, Opra2_0_5, Opra2_0_4
                         , Opra2_0_3, Opra2_s_0 ] )
        , ( ( Opra2_7_7 , Opra2_s_0 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_7_7 , Opra2_s_1 )
          , Set.fromList [ Opra2_7_7, Opra2_7_6, Opra2_7_5 ] )
        , ( ( Opra2_7_7 , Opra2_s_2 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_7_7 , Opra2_s_3 )
          , Set.fromList [ Opra2_7_5, Opra2_7_4, Opra2_7_3 ] )
        , ( ( Opra2_7_7 , Opra2_s_4 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_7_7 , Opra2_s_5 )
          , Set.fromList [ Opra2_7_3, Opra2_7_2, Opra2_7_1 ] )
        , ( ( Opra2_7_7 , Opra2_s_6 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_7_7 , Opra2_s_7 )
          , Set.fromList [ Opra2_7_7, Opra2_7_1, Opra2_7_0 ] )
        , ( ( Opra2_s_0 , Opra2_0_0 )
          , Set.fromList [ Opra2_0_0 ] )
        , ( ( Opra2_s_0 , Opra2_0_1 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_s_0 , Opra2_0_2 )
          , Set.fromList [ Opra2_0_2 ] )
        , ( ( Opra2_s_0 , Opra2_0_3 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_s_0 , Opra2_0_4 )
          , Set.fromList [ Opra2_0_4 ] )
        , ( ( Opra2_s_0 , Opra2_0_5 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_s_0 , Opra2_0_6 )
          , Set.fromList [ Opra2_0_6 ] )
        , ( ( Opra2_s_0 , Opra2_0_7 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_s_0 , Opra2_1_0 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_s_0 , Opra2_1_1 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_s_0 , Opra2_1_2 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_s_0 , Opra2_1_3 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_s_0 , Opra2_1_4 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_s_0 , Opra2_1_5 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_s_0 , Opra2_1_6 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_s_0 , Opra2_1_7 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_s_0 , Opra2_2_0 )
          , Set.fromList [ Opra2_2_0 ] )
        , ( ( Opra2_s_0 , Opra2_2_1 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_s_0 , Opra2_2_2 )
          , Set.fromList [ Opra2_2_2 ] )
        , ( ( Opra2_s_0 , Opra2_2_3 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_s_0 , Opra2_2_4 )
          , Set.fromList [ Opra2_2_4 ] )
        , ( ( Opra2_s_0 , Opra2_2_5 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_s_0 , Opra2_2_6 )
          , Set.fromList [ Opra2_2_6 ] )
        , ( ( Opra2_s_0 , Opra2_2_7 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_s_0 , Opra2_3_0 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_s_0 , Opra2_3_1 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_s_0 , Opra2_3_2 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_s_0 , Opra2_3_3 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_s_0 , Opra2_3_4 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_s_0 , Opra2_3_5 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_s_0 , Opra2_3_6 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_s_0 , Opra2_3_7 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_s_0 , Opra2_4_0 )
          , Set.fromList [ Opra2_4_0 ] )
        , ( ( Opra2_s_0 , Opra2_4_1 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_s_0 , Opra2_4_2 )
          , Set.fromList [ Opra2_4_2 ] )
        , ( ( Opra2_s_0 , Opra2_4_3 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_s_0 , Opra2_4_4 )
          , Set.fromList [ Opra2_4_4 ] )
        , ( ( Opra2_s_0 , Opra2_4_5 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_s_0 , Opra2_4_6 )
          , Set.fromList [ Opra2_4_6 ] )
        , ( ( Opra2_s_0 , Opra2_4_7 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_s_0 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_s_0 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_s_0 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_s_0 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_s_0 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_s_0 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_s_0 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_s_0 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_s_0 , Opra2_6_0 )
          , Set.fromList [ Opra2_6_0 ] )
        , ( ( Opra2_s_0 , Opra2_6_1 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_s_0 , Opra2_6_2 )
          , Set.fromList [ Opra2_6_2 ] )
        , ( ( Opra2_s_0 , Opra2_6_3 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_s_0 , Opra2_6_4 )
          , Set.fromList [ Opra2_6_4 ] )
        , ( ( Opra2_s_0 , Opra2_6_5 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_s_0 , Opra2_6_6 )
          , Set.fromList [ Opra2_6_6 ] )
        , ( ( Opra2_s_0 , Opra2_6_7 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_s_0 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_s_0 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_s_0 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_s_0 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_s_0 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_s_0 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_s_0 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_s_0 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_s_0 , Opra2_s_0 )
          , Set.fromList [ Opra2_s_0 ] )
        , ( ( Opra2_s_0 , Opra2_s_1 )
          , Set.fromList [ Opra2_s_1 ] )
        , ( ( Opra2_s_0 , Opra2_s_2 )
          , Set.fromList [ Opra2_s_2 ] )
        , ( ( Opra2_s_0 , Opra2_s_3 )
          , Set.fromList [ Opra2_s_3 ] )
        , ( ( Opra2_s_0 , Opra2_s_4 )
          , Set.fromList [ Opra2_s_4 ] )
        , ( ( Opra2_s_0 , Opra2_s_5 )
          , Set.fromList [ Opra2_s_5 ] )
        , ( ( Opra2_s_0 , Opra2_s_6 )
          , Set.fromList [ Opra2_s_6 ] )
        , ( ( Opra2_s_0 , Opra2_s_7 )
          , Set.fromList [ Opra2_s_7 ] )
        , ( ( Opra2_s_1 , Opra2_0_0 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_s_1 , Opra2_0_1 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_s_1 , Opra2_0_2 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_s_1 , Opra2_0_3 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_s_1 , Opra2_0_4 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_s_1 , Opra2_0_5 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_s_1 , Opra2_0_6 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_s_1 , Opra2_0_7 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_s_1 , Opra2_1_0 )
          , Set.fromList [ Opra2_3_0, Opra2_2_0, Opra2_1_0 ] )
        , ( ( Opra2_s_1 , Opra2_1_1 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_s_1 , Opra2_1_2 )
          , Set.fromList [ Opra2_3_2, Opra2_2_2, Opra2_1_2 ] )
        , ( ( Opra2_s_1 , Opra2_1_3 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_s_1 , Opra2_1_4 )
          , Set.fromList [ Opra2_3_4, Opra2_2_4, Opra2_1_4 ] )
        , ( ( Opra2_s_1 , Opra2_1_5 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_s_1 , Opra2_1_6 )
          , Set.fromList [ Opra2_3_6, Opra2_2_6, Opra2_1_6 ] )
        , ( ( Opra2_s_1 , Opra2_1_7 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_s_1 , Opra2_2_0 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_s_1 , Opra2_2_1 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_s_1 , Opra2_2_2 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_s_1 , Opra2_2_3 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_s_1 , Opra2_2_4 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_s_1 , Opra2_2_5 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_s_1 , Opra2_2_6 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_s_1 , Opra2_2_7 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_s_1 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_0, Opra2_4_0, Opra2_3_0 ] )
        , ( ( Opra2_s_1 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_s_1 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_2, Opra2_4_2, Opra2_3_2 ] )
        , ( ( Opra2_s_1 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_s_1 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_4, Opra2_4_4, Opra2_3_4 ] )
        , ( ( Opra2_s_1 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_s_1 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_6, Opra2_4_6, Opra2_3_6 ] )
        , ( ( Opra2_s_1 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_s_1 , Opra2_4_0 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_s_1 , Opra2_4_1 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_s_1 , Opra2_4_2 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_s_1 , Opra2_4_3 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_s_1 , Opra2_4_4 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_s_1 , Opra2_4_5 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_s_1 , Opra2_4_6 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_s_1 , Opra2_4_7 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_s_1 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_0, Opra2_6_0, Opra2_5_0 ] )
        , ( ( Opra2_s_1 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_s_1 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_2, Opra2_6_2, Opra2_5_2 ] )
        , ( ( Opra2_s_1 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_s_1 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_4, Opra2_6_4, Opra2_5_4 ] )
        , ( ( Opra2_s_1 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_s_1 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_6, Opra2_6_6, Opra2_5_6 ] )
        , ( ( Opra2_s_1 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_s_1 , Opra2_6_0 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_s_1 , Opra2_6_1 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_s_1 , Opra2_6_2 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_s_1 , Opra2_6_3 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_s_1 , Opra2_6_4 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_s_1 , Opra2_6_5 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_s_1 , Opra2_6_6 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_s_1 , Opra2_6_7 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_s_1 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_0, Opra2_1_0, Opra2_0_0 ] )
        , ( ( Opra2_s_1 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_s_1 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_2, Opra2_1_2, Opra2_0_2 ] )
        , ( ( Opra2_s_1 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_s_1 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_4, Opra2_1_4, Opra2_0_4 ] )
        , ( ( Opra2_s_1 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_s_1 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_6, Opra2_1_6, Opra2_0_6 ] )
        , ( ( Opra2_s_1 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_s_1 , Opra2_s_0 )
          , Set.fromList [ Opra2_s_1 ] )
        , ( ( Opra2_s_1 , Opra2_s_1 )
          , Set.fromList [ Opra2_s_3, Opra2_s_2, Opra2_s_1 ] )
        , ( ( Opra2_s_1 , Opra2_s_2 )
          , Set.fromList [ Opra2_s_3 ] )
        , ( ( Opra2_s_1 , Opra2_s_3 )
          , Set.fromList [ Opra2_s_5, Opra2_s_4, Opra2_s_3 ] )
        , ( ( Opra2_s_1 , Opra2_s_4 )
          , Set.fromList [ Opra2_s_5 ] )
        , ( ( Opra2_s_1 , Opra2_s_5 )
          , Set.fromList [ Opra2_s_7, Opra2_s_6, Opra2_s_5 ] )
        , ( ( Opra2_s_1 , Opra2_s_6 )
          , Set.fromList [ Opra2_s_7 ] )
        , ( ( Opra2_s_1 , Opra2_s_7 )
          , Set.fromList [ Opra2_s_7, Opra2_s_1, Opra2_s_0 ] )
        , ( ( Opra2_s_2 , Opra2_0_0 )
          , Set.fromList [ Opra2_2_0 ] )
        , ( ( Opra2_s_2 , Opra2_0_1 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_s_2 , Opra2_0_2 )
          , Set.fromList [ Opra2_2_2 ] )
        , ( ( Opra2_s_2 , Opra2_0_3 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_s_2 , Opra2_0_4 )
          , Set.fromList [ Opra2_2_4 ] )
        , ( ( Opra2_s_2 , Opra2_0_5 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_s_2 , Opra2_0_6 )
          , Set.fromList [ Opra2_2_6 ] )
        , ( ( Opra2_s_2 , Opra2_0_7 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_s_2 , Opra2_1_0 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_s_2 , Opra2_1_1 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_s_2 , Opra2_1_2 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_s_2 , Opra2_1_3 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_s_2 , Opra2_1_4 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_s_2 , Opra2_1_5 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_s_2 , Opra2_1_6 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_s_2 , Opra2_1_7 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_s_2 , Opra2_2_0 )
          , Set.fromList [ Opra2_4_0 ] )
        , ( ( Opra2_s_2 , Opra2_2_1 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_s_2 , Opra2_2_2 )
          , Set.fromList [ Opra2_4_2 ] )
        , ( ( Opra2_s_2 , Opra2_2_3 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_s_2 , Opra2_2_4 )
          , Set.fromList [ Opra2_4_4 ] )
        , ( ( Opra2_s_2 , Opra2_2_5 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_s_2 , Opra2_2_6 )
          , Set.fromList [ Opra2_4_6 ] )
        , ( ( Opra2_s_2 , Opra2_2_7 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_s_2 , Opra2_3_0 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_s_2 , Opra2_3_1 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_s_2 , Opra2_3_2 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_s_2 , Opra2_3_3 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_s_2 , Opra2_3_4 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_s_2 , Opra2_3_5 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_s_2 , Opra2_3_6 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_s_2 , Opra2_3_7 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_s_2 , Opra2_4_0 )
          , Set.fromList [ Opra2_6_0 ] )
        , ( ( Opra2_s_2 , Opra2_4_1 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_s_2 , Opra2_4_2 )
          , Set.fromList [ Opra2_6_2 ] )
        , ( ( Opra2_s_2 , Opra2_4_3 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_s_2 , Opra2_4_4 )
          , Set.fromList [ Opra2_6_4 ] )
        , ( ( Opra2_s_2 , Opra2_4_5 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_s_2 , Opra2_4_6 )
          , Set.fromList [ Opra2_6_6 ] )
        , ( ( Opra2_s_2 , Opra2_4_7 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_s_2 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_s_2 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_s_2 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_s_2 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_s_2 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_s_2 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_s_2 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_s_2 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_s_2 , Opra2_6_0 )
          , Set.fromList [ Opra2_0_0 ] )
        , ( ( Opra2_s_2 , Opra2_6_1 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_s_2 , Opra2_6_2 )
          , Set.fromList [ Opra2_0_2 ] )
        , ( ( Opra2_s_2 , Opra2_6_3 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_s_2 , Opra2_6_4 )
          , Set.fromList [ Opra2_0_4 ] )
        , ( ( Opra2_s_2 , Opra2_6_5 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_s_2 , Opra2_6_6 )
          , Set.fromList [ Opra2_0_6 ] )
        , ( ( Opra2_s_2 , Opra2_6_7 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_s_2 , Opra2_7_0 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_s_2 , Opra2_7_1 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_s_2 , Opra2_7_2 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_s_2 , Opra2_7_3 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_s_2 , Opra2_7_4 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_s_2 , Opra2_7_5 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_s_2 , Opra2_7_6 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_s_2 , Opra2_7_7 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_s_2 , Opra2_s_0 )
          , Set.fromList [ Opra2_s_2 ] )
        , ( ( Opra2_s_2 , Opra2_s_1 )
          , Set.fromList [ Opra2_s_3 ] )
        , ( ( Opra2_s_2 , Opra2_s_2 )
          , Set.fromList [ Opra2_s_4 ] )
        , ( ( Opra2_s_2 , Opra2_s_3 )
          , Set.fromList [ Opra2_s_5 ] )
        , ( ( Opra2_s_2 , Opra2_s_4 )
          , Set.fromList [ Opra2_s_6 ] )
        , ( ( Opra2_s_2 , Opra2_s_5 )
          , Set.fromList [ Opra2_s_7 ] )
        , ( ( Opra2_s_2 , Opra2_s_6 )
          , Set.fromList [ Opra2_s_0 ] )
        , ( ( Opra2_s_2 , Opra2_s_7 )
          , Set.fromList [ Opra2_s_1 ] )
        , ( ( Opra2_s_3 , Opra2_0_0 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_s_3 , Opra2_0_1 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_s_3 , Opra2_0_2 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_s_3 , Opra2_0_3 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_s_3 , Opra2_0_4 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_s_3 , Opra2_0_5 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_s_3 , Opra2_0_6 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_s_3 , Opra2_0_7 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_s_3 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_0, Opra2_4_0, Opra2_3_0 ] )
        , ( ( Opra2_s_3 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_s_3 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_2, Opra2_4_2, Opra2_3_2 ] )
        , ( ( Opra2_s_3 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_s_3 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_4, Opra2_4_4, Opra2_3_4 ] )
        , ( ( Opra2_s_3 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_s_3 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_6, Opra2_4_6, Opra2_3_6 ] )
        , ( ( Opra2_s_3 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_s_3 , Opra2_2_0 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_s_3 , Opra2_2_1 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_s_3 , Opra2_2_2 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_s_3 , Opra2_2_3 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_s_3 , Opra2_2_4 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_s_3 , Opra2_2_5 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_s_3 , Opra2_2_6 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_s_3 , Opra2_2_7 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_s_3 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_0, Opra2_6_0, Opra2_5_0 ] )
        , ( ( Opra2_s_3 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_s_3 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_2, Opra2_6_2, Opra2_5_2 ] )
        , ( ( Opra2_s_3 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_s_3 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_4, Opra2_6_4, Opra2_5_4 ] )
        , ( ( Opra2_s_3 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_s_3 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_6, Opra2_6_6, Opra2_5_6 ] )
        , ( ( Opra2_s_3 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_s_3 , Opra2_4_0 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_s_3 , Opra2_4_1 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_s_3 , Opra2_4_2 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_s_3 , Opra2_4_3 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_s_3 , Opra2_4_4 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_s_3 , Opra2_4_5 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_s_3 , Opra2_4_6 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_s_3 , Opra2_4_7 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_s_3 , Opra2_5_0 )
          , Set.fromList [ Opra2_7_0, Opra2_1_0, Opra2_0_0 ] )
        , ( ( Opra2_s_3 , Opra2_5_1 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_s_3 , Opra2_5_2 )
          , Set.fromList [ Opra2_7_2, Opra2_1_2, Opra2_0_2 ] )
        , ( ( Opra2_s_3 , Opra2_5_3 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_s_3 , Opra2_5_4 )
          , Set.fromList [ Opra2_7_4, Opra2_1_4, Opra2_0_4 ] )
        , ( ( Opra2_s_3 , Opra2_5_5 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_s_3 , Opra2_5_6 )
          , Set.fromList [ Opra2_7_6, Opra2_1_6, Opra2_0_6 ] )
        , ( ( Opra2_s_3 , Opra2_5_7 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_s_3 , Opra2_6_0 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_s_3 , Opra2_6_1 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_s_3 , Opra2_6_2 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_s_3 , Opra2_6_3 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_s_3 , Opra2_6_4 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_s_3 , Opra2_6_5 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_s_3 , Opra2_6_6 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_s_3 , Opra2_6_7 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_s_3 , Opra2_7_0 )
          , Set.fromList [ Opra2_3_0, Opra2_2_0, Opra2_1_0 ] )
        , ( ( Opra2_s_3 , Opra2_7_1 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_s_3 , Opra2_7_2 )
          , Set.fromList [ Opra2_3_2, Opra2_2_2, Opra2_1_2 ] )
        , ( ( Opra2_s_3 , Opra2_7_3 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_s_3 , Opra2_7_4 )
          , Set.fromList [ Opra2_3_4, Opra2_2_4, Opra2_1_4 ] )
        , ( ( Opra2_s_3 , Opra2_7_5 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_s_3 , Opra2_7_6 )
          , Set.fromList [ Opra2_3_6, Opra2_2_6, Opra2_1_6 ] )
        , ( ( Opra2_s_3 , Opra2_7_7 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_s_3 , Opra2_s_0 )
          , Set.fromList [ Opra2_s_3 ] )
        , ( ( Opra2_s_3 , Opra2_s_1 )
          , Set.fromList [ Opra2_s_5, Opra2_s_4, Opra2_s_3 ] )
        , ( ( Opra2_s_3 , Opra2_s_2 )
          , Set.fromList [ Opra2_s_5 ] )
        , ( ( Opra2_s_3 , Opra2_s_3 )
          , Set.fromList [ Opra2_s_7, Opra2_s_6, Opra2_s_5 ] )
        , ( ( Opra2_s_3 , Opra2_s_4 )
          , Set.fromList [ Opra2_s_7 ] )
        , ( ( Opra2_s_3 , Opra2_s_5 )
          , Set.fromList [ Opra2_s_7, Opra2_s_1, Opra2_s_0 ] )
        , ( ( Opra2_s_3 , Opra2_s_6 )
          , Set.fromList [ Opra2_s_1 ] )
        , ( ( Opra2_s_3 , Opra2_s_7 )
          , Set.fromList [ Opra2_s_3, Opra2_s_2, Opra2_s_1 ] )
        , ( ( Opra2_s_4 , Opra2_0_0 )
          , Set.fromList [ Opra2_4_0 ] )
        , ( ( Opra2_s_4 , Opra2_0_1 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_s_4 , Opra2_0_2 )
          , Set.fromList [ Opra2_4_2 ] )
        , ( ( Opra2_s_4 , Opra2_0_3 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_s_4 , Opra2_0_4 )
          , Set.fromList [ Opra2_4_4 ] )
        , ( ( Opra2_s_4 , Opra2_0_5 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_s_4 , Opra2_0_6 )
          , Set.fromList [ Opra2_4_6 ] )
        , ( ( Opra2_s_4 , Opra2_0_7 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_s_4 , Opra2_1_0 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_s_4 , Opra2_1_1 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_s_4 , Opra2_1_2 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_s_4 , Opra2_1_3 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_s_4 , Opra2_1_4 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_s_4 , Opra2_1_5 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_s_4 , Opra2_1_6 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_s_4 , Opra2_1_7 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_s_4 , Opra2_2_0 )
          , Set.fromList [ Opra2_6_0 ] )
        , ( ( Opra2_s_4 , Opra2_2_1 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_s_4 , Opra2_2_2 )
          , Set.fromList [ Opra2_6_2 ] )
        , ( ( Opra2_s_4 , Opra2_2_3 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_s_4 , Opra2_2_4 )
          , Set.fromList [ Opra2_6_4 ] )
        , ( ( Opra2_s_4 , Opra2_2_5 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_s_4 , Opra2_2_6 )
          , Set.fromList [ Opra2_6_6 ] )
        , ( ( Opra2_s_4 , Opra2_2_7 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_s_4 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_s_4 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_s_4 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_s_4 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_s_4 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_s_4 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_s_4 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_s_4 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_s_4 , Opra2_4_0 )
          , Set.fromList [ Opra2_0_0 ] )
        , ( ( Opra2_s_4 , Opra2_4_1 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_s_4 , Opra2_4_2 )
          , Set.fromList [ Opra2_0_2 ] )
        , ( ( Opra2_s_4 , Opra2_4_3 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_s_4 , Opra2_4_4 )
          , Set.fromList [ Opra2_0_4 ] )
        , ( ( Opra2_s_4 , Opra2_4_5 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_s_4 , Opra2_4_6 )
          , Set.fromList [ Opra2_0_6 ] )
        , ( ( Opra2_s_4 , Opra2_4_7 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_s_4 , Opra2_5_0 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_s_4 , Opra2_5_1 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_s_4 , Opra2_5_2 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_s_4 , Opra2_5_3 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_s_4 , Opra2_5_4 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_s_4 , Opra2_5_5 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_s_4 , Opra2_5_6 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_s_4 , Opra2_5_7 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_s_4 , Opra2_6_0 )
          , Set.fromList [ Opra2_2_0 ] )
        , ( ( Opra2_s_4 , Opra2_6_1 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_s_4 , Opra2_6_2 )
          , Set.fromList [ Opra2_2_2 ] )
        , ( ( Opra2_s_4 , Opra2_6_3 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_s_4 , Opra2_6_4 )
          , Set.fromList [ Opra2_2_4 ] )
        , ( ( Opra2_s_4 , Opra2_6_5 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_s_4 , Opra2_6_6 )
          , Set.fromList [ Opra2_2_6 ] )
        , ( ( Opra2_s_4 , Opra2_6_7 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_s_4 , Opra2_7_0 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_s_4 , Opra2_7_1 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_s_4 , Opra2_7_2 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_s_4 , Opra2_7_3 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_s_4 , Opra2_7_4 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_s_4 , Opra2_7_5 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_s_4 , Opra2_7_6 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_s_4 , Opra2_7_7 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_s_4 , Opra2_s_0 )
          , Set.fromList [ Opra2_s_4 ] )
        , ( ( Opra2_s_4 , Opra2_s_1 )
          , Set.fromList [ Opra2_s_5 ] )
        , ( ( Opra2_s_4 , Opra2_s_2 )
          , Set.fromList [ Opra2_s_6 ] )
        , ( ( Opra2_s_4 , Opra2_s_3 )
          , Set.fromList [ Opra2_s_7 ] )
        , ( ( Opra2_s_4 , Opra2_s_4 )
          , Set.fromList [ Opra2_s_0 ] )
        , ( ( Opra2_s_4 , Opra2_s_5 )
          , Set.fromList [ Opra2_s_1 ] )
        , ( ( Opra2_s_4 , Opra2_s_6 )
          , Set.fromList [ Opra2_s_2 ] )
        , ( ( Opra2_s_4 , Opra2_s_7 )
          , Set.fromList [ Opra2_s_3 ] )
        , ( ( Opra2_s_5 , Opra2_0_0 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_s_5 , Opra2_0_1 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_s_5 , Opra2_0_2 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_s_5 , Opra2_0_3 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_s_5 , Opra2_0_4 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_s_5 , Opra2_0_5 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_s_5 , Opra2_0_6 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_s_5 , Opra2_0_7 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_s_5 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_0, Opra2_6_0, Opra2_5_0 ] )
        , ( ( Opra2_s_5 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_s_5 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_2, Opra2_6_2, Opra2_5_2 ] )
        , ( ( Opra2_s_5 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_s_5 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_4, Opra2_6_4, Opra2_5_4 ] )
        , ( ( Opra2_s_5 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_s_5 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_6, Opra2_6_6, Opra2_5_6 ] )
        , ( ( Opra2_s_5 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_s_5 , Opra2_2_0 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_s_5 , Opra2_2_1 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_s_5 , Opra2_2_2 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_s_5 , Opra2_2_3 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_s_5 , Opra2_2_4 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_s_5 , Opra2_2_5 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_s_5 , Opra2_2_6 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_s_5 , Opra2_2_7 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_s_5 , Opra2_3_0 )
          , Set.fromList [ Opra2_7_0, Opra2_1_0, Opra2_0_0 ] )
        , ( ( Opra2_s_5 , Opra2_3_1 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_s_5 , Opra2_3_2 )
          , Set.fromList [ Opra2_7_2, Opra2_1_2, Opra2_0_2 ] )
        , ( ( Opra2_s_5 , Opra2_3_3 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_s_5 , Opra2_3_4 )
          , Set.fromList [ Opra2_7_4, Opra2_1_4, Opra2_0_4 ] )
        , ( ( Opra2_s_5 , Opra2_3_5 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_s_5 , Opra2_3_6 )
          , Set.fromList [ Opra2_7_6, Opra2_1_6, Opra2_0_6 ] )
        , ( ( Opra2_s_5 , Opra2_3_7 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_s_5 , Opra2_4_0 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_s_5 , Opra2_4_1 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_s_5 , Opra2_4_2 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_s_5 , Opra2_4_3 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_s_5 , Opra2_4_4 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_s_5 , Opra2_4_5 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_s_5 , Opra2_4_6 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_s_5 , Opra2_4_7 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_s_5 , Opra2_5_0 )
          , Set.fromList [ Opra2_3_0, Opra2_2_0, Opra2_1_0 ] )
        , ( ( Opra2_s_5 , Opra2_5_1 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_s_5 , Opra2_5_2 )
          , Set.fromList [ Opra2_3_2, Opra2_2_2, Opra2_1_2 ] )
        , ( ( Opra2_s_5 , Opra2_5_3 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_s_5 , Opra2_5_4 )
          , Set.fromList [ Opra2_3_4, Opra2_2_4, Opra2_1_4 ] )
        , ( ( Opra2_s_5 , Opra2_5_5 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_s_5 , Opra2_5_6 )
          , Set.fromList [ Opra2_3_6, Opra2_2_6, Opra2_1_6 ] )
        , ( ( Opra2_s_5 , Opra2_5_7 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_s_5 , Opra2_6_0 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_s_5 , Opra2_6_1 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_s_5 , Opra2_6_2 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_s_5 , Opra2_6_3 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_s_5 , Opra2_6_4 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_s_5 , Opra2_6_5 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_s_5 , Opra2_6_6 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_s_5 , Opra2_6_7 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_s_5 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_0, Opra2_4_0, Opra2_3_0 ] )
        , ( ( Opra2_s_5 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_s_5 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_2, Opra2_4_2, Opra2_3_2 ] )
        , ( ( Opra2_s_5 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_s_5 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_4, Opra2_4_4, Opra2_3_4 ] )
        , ( ( Opra2_s_5 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_s_5 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_6, Opra2_4_6, Opra2_3_6 ] )
        , ( ( Opra2_s_5 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_s_5 , Opra2_s_0 )
          , Set.fromList [ Opra2_s_5 ] )
        , ( ( Opra2_s_5 , Opra2_s_1 )
          , Set.fromList [ Opra2_s_7, Opra2_s_6, Opra2_s_5 ] )
        , ( ( Opra2_s_5 , Opra2_s_2 )
          , Set.fromList [ Opra2_s_7 ] )
        , ( ( Opra2_s_5 , Opra2_s_3 )
          , Set.fromList [ Opra2_s_7, Opra2_s_1, Opra2_s_0 ] )
        , ( ( Opra2_s_5 , Opra2_s_4 )
          , Set.fromList [ Opra2_s_1 ] )
        , ( ( Opra2_s_5 , Opra2_s_5 )
          , Set.fromList [ Opra2_s_3, Opra2_s_2, Opra2_s_1 ] )
        , ( ( Opra2_s_5 , Opra2_s_6 )
          , Set.fromList [ Opra2_s_3 ] )
        , ( ( Opra2_s_5 , Opra2_s_7 )
          , Set.fromList [ Opra2_s_5, Opra2_s_4, Opra2_s_3 ] )
        , ( ( Opra2_s_6 , Opra2_0_0 )
          , Set.fromList [ Opra2_6_0 ] )
        , ( ( Opra2_s_6 , Opra2_0_1 )
          , Set.fromList [ Opra2_6_1 ] )
        , ( ( Opra2_s_6 , Opra2_0_2 )
          , Set.fromList [ Opra2_6_2 ] )
        , ( ( Opra2_s_6 , Opra2_0_3 )
          , Set.fromList [ Opra2_6_3 ] )
        , ( ( Opra2_s_6 , Opra2_0_4 )
          , Set.fromList [ Opra2_6_4 ] )
        , ( ( Opra2_s_6 , Opra2_0_5 )
          , Set.fromList [ Opra2_6_5 ] )
        , ( ( Opra2_s_6 , Opra2_0_6 )
          , Set.fromList [ Opra2_6_6 ] )
        , ( ( Opra2_s_6 , Opra2_0_7 )
          , Set.fromList [ Opra2_6_7 ] )
        , ( ( Opra2_s_6 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_s_6 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_s_6 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_s_6 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_s_6 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_s_6 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_s_6 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_s_6 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_s_6 , Opra2_2_0 )
          , Set.fromList [ Opra2_0_0 ] )
        , ( ( Opra2_s_6 , Opra2_2_1 )
          , Set.fromList [ Opra2_0_1 ] )
        , ( ( Opra2_s_6 , Opra2_2_2 )
          , Set.fromList [ Opra2_0_2 ] )
        , ( ( Opra2_s_6 , Opra2_2_3 )
          , Set.fromList [ Opra2_0_3 ] )
        , ( ( Opra2_s_6 , Opra2_2_4 )
          , Set.fromList [ Opra2_0_4 ] )
        , ( ( Opra2_s_6 , Opra2_2_5 )
          , Set.fromList [ Opra2_0_5 ] )
        , ( ( Opra2_s_6 , Opra2_2_6 )
          , Set.fromList [ Opra2_0_6 ] )
        , ( ( Opra2_s_6 , Opra2_2_7 )
          , Set.fromList [ Opra2_0_7 ] )
        , ( ( Opra2_s_6 , Opra2_3_0 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_s_6 , Opra2_3_1 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_s_6 , Opra2_3_2 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_s_6 , Opra2_3_3 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_s_6 , Opra2_3_4 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_s_6 , Opra2_3_5 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_s_6 , Opra2_3_6 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_s_6 , Opra2_3_7 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_s_6 , Opra2_4_0 )
          , Set.fromList [ Opra2_2_0 ] )
        , ( ( Opra2_s_6 , Opra2_4_1 )
          , Set.fromList [ Opra2_2_1 ] )
        , ( ( Opra2_s_6 , Opra2_4_2 )
          , Set.fromList [ Opra2_2_2 ] )
        , ( ( Opra2_s_6 , Opra2_4_3 )
          , Set.fromList [ Opra2_2_3 ] )
        , ( ( Opra2_s_6 , Opra2_4_4 )
          , Set.fromList [ Opra2_2_4 ] )
        , ( ( Opra2_s_6 , Opra2_4_5 )
          , Set.fromList [ Opra2_2_5 ] )
        , ( ( Opra2_s_6 , Opra2_4_6 )
          , Set.fromList [ Opra2_2_6 ] )
        , ( ( Opra2_s_6 , Opra2_4_7 )
          , Set.fromList [ Opra2_2_7 ] )
        , ( ( Opra2_s_6 , Opra2_5_0 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_s_6 , Opra2_5_1 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_s_6 , Opra2_5_2 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_s_6 , Opra2_5_3 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_s_6 , Opra2_5_4 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_s_6 , Opra2_5_5 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_s_6 , Opra2_5_6 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_s_6 , Opra2_5_7 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_s_6 , Opra2_6_0 )
          , Set.fromList [ Opra2_4_0 ] )
        , ( ( Opra2_s_6 , Opra2_6_1 )
          , Set.fromList [ Opra2_4_1 ] )
        , ( ( Opra2_s_6 , Opra2_6_2 )
          , Set.fromList [ Opra2_4_2 ] )
        , ( ( Opra2_s_6 , Opra2_6_3 )
          , Set.fromList [ Opra2_4_3 ] )
        , ( ( Opra2_s_6 , Opra2_6_4 )
          , Set.fromList [ Opra2_4_4 ] )
        , ( ( Opra2_s_6 , Opra2_6_5 )
          , Set.fromList [ Opra2_4_5 ] )
        , ( ( Opra2_s_6 , Opra2_6_6 )
          , Set.fromList [ Opra2_4_6 ] )
        , ( ( Opra2_s_6 , Opra2_6_7 )
          , Set.fromList [ Opra2_4_7 ] )
        , ( ( Opra2_s_6 , Opra2_7_0 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_s_6 , Opra2_7_1 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_s_6 , Opra2_7_2 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_s_6 , Opra2_7_3 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_s_6 , Opra2_7_4 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_s_6 , Opra2_7_5 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_s_6 , Opra2_7_6 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_s_6 , Opra2_7_7 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_s_6 , Opra2_s_0 )
          , Set.fromList [ Opra2_s_6 ] )
        , ( ( Opra2_s_6 , Opra2_s_1 )
          , Set.fromList [ Opra2_s_7 ] )
        , ( ( Opra2_s_6 , Opra2_s_2 )
          , Set.fromList [ Opra2_s_0 ] )
        , ( ( Opra2_s_6 , Opra2_s_3 )
          , Set.fromList [ Opra2_s_1 ] )
        , ( ( Opra2_s_6 , Opra2_s_4 )
          , Set.fromList [ Opra2_s_2 ] )
        , ( ( Opra2_s_6 , Opra2_s_5 )
          , Set.fromList [ Opra2_s_3 ] )
        , ( ( Opra2_s_6 , Opra2_s_6 )
          , Set.fromList [ Opra2_s_4 ] )
        , ( ( Opra2_s_6 , Opra2_s_7 )
          , Set.fromList [ Opra2_s_5 ] )
        , ( ( Opra2_s_7 , Opra2_0_0 )
          , Set.fromList [ Opra2_7_0 ] )
        , ( ( Opra2_s_7 , Opra2_0_1 )
          , Set.fromList [ Opra2_7_1 ] )
        , ( ( Opra2_s_7 , Opra2_0_2 )
          , Set.fromList [ Opra2_7_2 ] )
        , ( ( Opra2_s_7 , Opra2_0_3 )
          , Set.fromList [ Opra2_7_3 ] )
        , ( ( Opra2_s_7 , Opra2_0_4 )
          , Set.fromList [ Opra2_7_4 ] )
        , ( ( Opra2_s_7 , Opra2_0_5 )
          , Set.fromList [ Opra2_7_5 ] )
        , ( ( Opra2_s_7 , Opra2_0_6 )
          , Set.fromList [ Opra2_7_6 ] )
        , ( ( Opra2_s_7 , Opra2_0_7 )
          , Set.fromList [ Opra2_7_7 ] )
        , ( ( Opra2_s_7 , Opra2_1_0 )
          , Set.fromList [ Opra2_7_0, Opra2_1_0, Opra2_0_0 ] )
        , ( ( Opra2_s_7 , Opra2_1_1 )
          , Set.fromList [ Opra2_7_1, Opra2_1_1, Opra2_0_1 ] )
        , ( ( Opra2_s_7 , Opra2_1_2 )
          , Set.fromList [ Opra2_7_2, Opra2_1_2, Opra2_0_2 ] )
        , ( ( Opra2_s_7 , Opra2_1_3 )
          , Set.fromList [ Opra2_7_3, Opra2_1_3, Opra2_0_3 ] )
        , ( ( Opra2_s_7 , Opra2_1_4 )
          , Set.fromList [ Opra2_7_4, Opra2_1_4, Opra2_0_4 ] )
        , ( ( Opra2_s_7 , Opra2_1_5 )
          , Set.fromList [ Opra2_7_5, Opra2_1_5, Opra2_0_5 ] )
        , ( ( Opra2_s_7 , Opra2_1_6 )
          , Set.fromList [ Opra2_7_6, Opra2_1_6, Opra2_0_6 ] )
        , ( ( Opra2_s_7 , Opra2_1_7 )
          , Set.fromList [ Opra2_7_7, Opra2_1_7, Opra2_0_7 ] )
        , ( ( Opra2_s_7 , Opra2_2_0 )
          , Set.fromList [ Opra2_1_0 ] )
        , ( ( Opra2_s_7 , Opra2_2_1 )
          , Set.fromList [ Opra2_1_1 ] )
        , ( ( Opra2_s_7 , Opra2_2_2 )
          , Set.fromList [ Opra2_1_2 ] )
        , ( ( Opra2_s_7 , Opra2_2_3 )
          , Set.fromList [ Opra2_1_3 ] )
        , ( ( Opra2_s_7 , Opra2_2_4 )
          , Set.fromList [ Opra2_1_4 ] )
        , ( ( Opra2_s_7 , Opra2_2_5 )
          , Set.fromList [ Opra2_1_5 ] )
        , ( ( Opra2_s_7 , Opra2_2_6 )
          , Set.fromList [ Opra2_1_6 ] )
        , ( ( Opra2_s_7 , Opra2_2_7 )
          , Set.fromList [ Opra2_1_7 ] )
        , ( ( Opra2_s_7 , Opra2_3_0 )
          , Set.fromList [ Opra2_3_0, Opra2_2_0, Opra2_1_0 ] )
        , ( ( Opra2_s_7 , Opra2_3_1 )
          , Set.fromList [ Opra2_3_1, Opra2_2_1, Opra2_1_1 ] )
        , ( ( Opra2_s_7 , Opra2_3_2 )
          , Set.fromList [ Opra2_3_2, Opra2_2_2, Opra2_1_2 ] )
        , ( ( Opra2_s_7 , Opra2_3_3 )
          , Set.fromList [ Opra2_3_3, Opra2_2_3, Opra2_1_3 ] )
        , ( ( Opra2_s_7 , Opra2_3_4 )
          , Set.fromList [ Opra2_3_4, Opra2_2_4, Opra2_1_4 ] )
        , ( ( Opra2_s_7 , Opra2_3_5 )
          , Set.fromList [ Opra2_3_5, Opra2_2_5, Opra2_1_5 ] )
        , ( ( Opra2_s_7 , Opra2_3_6 )
          , Set.fromList [ Opra2_3_6, Opra2_2_6, Opra2_1_6 ] )
        , ( ( Opra2_s_7 , Opra2_3_7 )
          , Set.fromList [ Opra2_3_7, Opra2_2_7, Opra2_1_7 ] )
        , ( ( Opra2_s_7 , Opra2_4_0 )
          , Set.fromList [ Opra2_3_0 ] )
        , ( ( Opra2_s_7 , Opra2_4_1 )
          , Set.fromList [ Opra2_3_1 ] )
        , ( ( Opra2_s_7 , Opra2_4_2 )
          , Set.fromList [ Opra2_3_2 ] )
        , ( ( Opra2_s_7 , Opra2_4_3 )
          , Set.fromList [ Opra2_3_3 ] )
        , ( ( Opra2_s_7 , Opra2_4_4 )
          , Set.fromList [ Opra2_3_4 ] )
        , ( ( Opra2_s_7 , Opra2_4_5 )
          , Set.fromList [ Opra2_3_5 ] )
        , ( ( Opra2_s_7 , Opra2_4_6 )
          , Set.fromList [ Opra2_3_6 ] )
        , ( ( Opra2_s_7 , Opra2_4_7 )
          , Set.fromList [ Opra2_3_7 ] )
        , ( ( Opra2_s_7 , Opra2_5_0 )
          , Set.fromList [ Opra2_5_0, Opra2_4_0, Opra2_3_0 ] )
        , ( ( Opra2_s_7 , Opra2_5_1 )
          , Set.fromList [ Opra2_5_1, Opra2_4_1, Opra2_3_1 ] )
        , ( ( Opra2_s_7 , Opra2_5_2 )
          , Set.fromList [ Opra2_5_2, Opra2_4_2, Opra2_3_2 ] )
        , ( ( Opra2_s_7 , Opra2_5_3 )
          , Set.fromList [ Opra2_5_3, Opra2_4_3, Opra2_3_3 ] )
        , ( ( Opra2_s_7 , Opra2_5_4 )
          , Set.fromList [ Opra2_5_4, Opra2_4_4, Opra2_3_4 ] )
        , ( ( Opra2_s_7 , Opra2_5_5 )
          , Set.fromList [ Opra2_5_5, Opra2_4_5, Opra2_3_5 ] )
        , ( ( Opra2_s_7 , Opra2_5_6 )
          , Set.fromList [ Opra2_5_6, Opra2_4_6, Opra2_3_6 ] )
        , ( ( Opra2_s_7 , Opra2_5_7 )
          , Set.fromList [ Opra2_5_7, Opra2_4_7, Opra2_3_7 ] )
        , ( ( Opra2_s_7 , Opra2_6_0 )
          , Set.fromList [ Opra2_5_0 ] )
        , ( ( Opra2_s_7 , Opra2_6_1 )
          , Set.fromList [ Opra2_5_1 ] )
        , ( ( Opra2_s_7 , Opra2_6_2 )
          , Set.fromList [ Opra2_5_2 ] )
        , ( ( Opra2_s_7 , Opra2_6_3 )
          , Set.fromList [ Opra2_5_3 ] )
        , ( ( Opra2_s_7 , Opra2_6_4 )
          , Set.fromList [ Opra2_5_4 ] )
        , ( ( Opra2_s_7 , Opra2_6_5 )
          , Set.fromList [ Opra2_5_5 ] )
        , ( ( Opra2_s_7 , Opra2_6_6 )
          , Set.fromList [ Opra2_5_6 ] )
        , ( ( Opra2_s_7 , Opra2_6_7 )
          , Set.fromList [ Opra2_5_7 ] )
        , ( ( Opra2_s_7 , Opra2_7_0 )
          , Set.fromList [ Opra2_7_0, Opra2_6_0, Opra2_5_0 ] )
        , ( ( Opra2_s_7 , Opra2_7_1 )
          , Set.fromList [ Opra2_7_1, Opra2_6_1, Opra2_5_1 ] )
        , ( ( Opra2_s_7 , Opra2_7_2 )
          , Set.fromList [ Opra2_7_2, Opra2_6_2, Opra2_5_2 ] )
        , ( ( Opra2_s_7 , Opra2_7_3 )
          , Set.fromList [ Opra2_7_3, Opra2_6_3, Opra2_5_3 ] )
        , ( ( Opra2_s_7 , Opra2_7_4 )
          , Set.fromList [ Opra2_7_4, Opra2_6_4, Opra2_5_4 ] )
        , ( ( Opra2_s_7 , Opra2_7_5 )
          , Set.fromList [ Opra2_7_5, Opra2_6_5, Opra2_5_5 ] )
        , ( ( Opra2_s_7 , Opra2_7_6 )
          , Set.fromList [ Opra2_7_6, Opra2_6_6, Opra2_5_6 ] )
        , ( ( Opra2_s_7 , Opra2_7_7 )
          , Set.fromList [ Opra2_7_7, Opra2_6_7, Opra2_5_7 ] )
        , ( ( Opra2_s_7 , Opra2_s_0 )
          , Set.fromList [ Opra2_s_7 ] )
        , ( ( Opra2_s_7 , Opra2_s_1 )
          , Set.fromList [ Opra2_s_7, Opra2_s_1, Opra2_s_0 ] )
        , ( ( Opra2_s_7 , Opra2_s_2 )
          , Set.fromList [ Opra2_s_1 ] )
        , ( ( Opra2_s_7 , Opra2_s_3 )
          , Set.fromList [ Opra2_s_3, Opra2_s_2, Opra2_s_1 ] )
        , ( ( Opra2_s_7 , Opra2_s_4 )
          , Set.fromList [ Opra2_s_3 ] )
        , ( ( Opra2_s_7 , Opra2_s_5 )
          , Set.fromList [ Opra2_s_5, Opra2_s_4, Opra2_s_3 ] )
        , ( ( Opra2_s_7 , Opra2_s_6 )
          , Set.fromList [ Opra2_s_5 ] )
        , ( ( Opra2_s_7 , Opra2_s_7 )
          , Set.fromList [ Opra2_s_7, Opra2_s_6, Opra2_s_5 ] )
        ]
