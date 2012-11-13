module Calculus.Opra1 where

-- standard modules
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Opra

data Opra1 = Opra1_0_0 | Opra1_0_1 | Opra1_0_2 | Opra1_0_3
           | Opra1_1_0 | Opra1_1_1 | Opra1_1_2 | Opra1_1_3
           | Opra1_2_0 | Opra1_2_1 | Opra1_2_2 | Opra1_2_3
           | Opra1_3_0 | Opra1_3_1 | Opra1_3_2 | Opra1_3_3
           | Opra1_s_0 | Opra1_s_1 | Opra1_s_2 | Opra1_s_3
           deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Opram Opra1 where
    m _ = 1

instance Calculus Opra1 where
    rank _ = 2
    calculus _ ="opra-1"
    readRel = readOpram
    showRel = showOpram
    sparqifyRel = sparqifyOpram
    gqrifyRel   = sparqifyOpram
    cBaserelationsArealList = filter
        (\ a -> ( \(b, _:c) -> all odd $ map read [b,c]) $ break (== '_') $
            map (\x -> if x == 's' then '1' else x) $ drop 6 $ show a
        ) cBaserelationsList

    identity = Opra1_s_0

    bcConvert = opraConvert 1

    bcConversion = Map.fromList
        [ ( Opra1_0_0 , Set.singleton Opra1_0_0 )
        , ( Opra1_0_1 , Set.singleton Opra1_1_0 )
        , ( Opra1_0_2 , Set.singleton Opra1_2_0 )
        , ( Opra1_0_3 , Set.singleton Opra1_3_0 )
        , ( Opra1_1_0 , Set.singleton Opra1_0_1 )
        , ( Opra1_1_1 , Set.singleton Opra1_1_1 )
        , ( Opra1_1_2 , Set.singleton Opra1_2_1 )
        , ( Opra1_1_3 , Set.singleton Opra1_3_1 )
        , ( Opra1_2_0 , Set.singleton Opra1_0_2 )
        , ( Opra1_2_1 , Set.singleton Opra1_1_2 )
        , ( Opra1_2_2 , Set.singleton Opra1_2_2 )
        , ( Opra1_2_3 , Set.singleton Opra1_3_2 )
        , ( Opra1_3_0 , Set.singleton Opra1_0_3 )
        , ( Opra1_3_1 , Set.singleton Opra1_1_3 )
        , ( Opra1_3_2 , Set.singleton Opra1_2_3 )
        , ( Opra1_3_3 , Set.singleton Opra1_3_3 )
        , ( Opra1_s_0 , Set.singleton Opra1_s_0 )
        , ( Opra1_s_1 , Set.singleton Opra1_s_3 )
        , ( Opra1_s_2 , Set.singleton Opra1_s_2 )
        , ( Opra1_s_3 , Set.singleton Opra1_s_1 ) ]

    bcComposition = Map.fromList
        [ ( ( Opra1_0_0 , Opra1_0_0 )
          , Set.fromList [ Opra1_2_0, Opra1_0_2, Opra1_s_0 ] )
        , ( ( Opra1_0_0 , Opra1_0_1 )
          , Set.fromList [ Opra1_s_3, Opra1_2_1, Opra1_0_3 ] )
        , ( ( Opra1_0_0 , Opra1_0_2 )
          , Set.fromList [ Opra1_2_2, Opra1_s_2, Opra1_0_0 ] )
        , ( ( Opra1_0_0 , Opra1_0_3 )
          , Set.fromList [ Opra1_2_3, Opra1_s_1, Opra1_0_1 ] )
        , ( ( Opra1_0_0 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_0_0 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1 ] )
        , ( ( Opra1_0_0 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_0_0 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0 ] )
        , ( ( Opra1_0_0 , Opra1_2_0 )
          , Set.fromList [ Opra1_0_0 ] )
        , ( ( Opra1_0_0 , Opra1_2_1 )
          , Set.fromList [ Opra1_0_1 ] )
        , ( ( Opra1_0_0 , Opra1_2_2 )
          , Set.fromList [ Opra1_0_2 ] )
        , ( ( Opra1_0_0 , Opra1_2_3 )
          , Set.fromList [ Opra1_0_3 ] )
        , ( ( Opra1_0_0 , Opra1_3_0 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_0_0 , Opra1_3_1 )
          , Set.fromList [ Opra1_1_3, Opra1_1_1, Opra1_1_0 ] )
        , ( ( Opra1_0_0 , Opra1_3_2 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_0_0 , Opra1_3_3 )
          , Set.fromList [ Opra1_1_3, Opra1_1_2, Opra1_1_1 ] )
        , ( ( Opra1_0_0 , Opra1_s_0 )
          , Set.fromList [ Opra1_0_0 ] )
        , ( ( Opra1_0_0 , Opra1_s_1 )
          , Set.fromList [ Opra1_0_3 ] )
        , ( ( Opra1_0_0 , Opra1_s_2 )
          , Set.fromList [ Opra1_0_2 ] )
        , ( ( Opra1_0_0 , Opra1_s_3 )
          , Set.fromList [ Opra1_0_1 ] )
        , ( ( Opra1_0_1 , Opra1_0_0 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_0_1 , Opra1_0_1 )
          , Set.fromList [ Opra1_1_3, Opra1_1_1, Opra1_1_0 ] )
        , ( ( Opra1_0_1 , Opra1_0_2 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_0_1 , Opra1_0_3 )
          , Set.fromList [ Opra1_1_3, Opra1_1_2, Opra1_1_1 ] )
        , ( ( Opra1_0_1 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_1, Opra1_2_0, Opra1_1_3, Opra1_0_2
                         , Opra1_s_0 ] )
        , ( ( Opra1_0_1 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_s_3
                         , Opra1_2_1, Opra1_1_3, Opra1_1_1, Opra1_1_0
                         , Opra1_0_3 ] )
        , ( ( Opra1_0_1 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_3, Opra1_2_2, Opra1_s_2, Opra1_1_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_0_1 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1, Opra1_s_1
                         , Opra1_0_1 ] )
        , ( ( Opra1_0_1 , Opra1_2_0 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_0_1 , Opra1_2_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1 ] )
        , ( ( Opra1_0_1 , Opra1_2_2 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_0_1 , Opra1_2_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0 ] )
        , ( ( Opra1_0_1 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_1, Opra1_1_3, Opra1_0_0 ] )
        , ( ( Opra1_0_1 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_1_3
                         , Opra1_1_1, Opra1_1_0, Opra1_0_1 ] )
        , ( ( Opra1_0_1 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_3, Opra1_1_1, Opra1_0_2 ] )
        , ( ( Opra1_0_1 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_0_3 ] )
        , ( ( Opra1_0_1 , Opra1_s_0 )
          , Set.fromList [ Opra1_0_1 ] )
        , ( ( Opra1_0_1 , Opra1_s_1 )
          , Set.fromList [ Opra1_0_3, Opra1_0_1, Opra1_0_0 ] )
        , ( ( Opra1_0_1 , Opra1_s_2 )
          , Set.fromList [ Opra1_0_3 ] )
        , ( ( Opra1_0_1 , Opra1_s_3 )
          , Set.fromList [ Opra1_0_3, Opra1_0_2, Opra1_0_1 ] )
        , ( ( Opra1_0_2 , Opra1_0_0 )
          , Set.fromList [ Opra1_0_0 ] )
        , ( ( Opra1_0_2 , Opra1_0_1 )
          , Set.fromList [ Opra1_0_1 ] )
        , ( ( Opra1_0_2 , Opra1_0_2 )
          , Set.fromList [ Opra1_0_2 ] )
        , ( ( Opra1_0_2 , Opra1_0_3 )
          , Set.fromList [ Opra1_0_3 ] )
        , ( ( Opra1_0_2 , Opra1_1_0 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_0_2 , Opra1_1_1 )
          , Set.fromList [ Opra1_1_3, Opra1_1_1, Opra1_1_0 ] )
        , ( ( Opra1_0_2 , Opra1_1_2 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_0_2 , Opra1_1_3 )
          , Set.fromList [ Opra1_1_3, Opra1_1_2, Opra1_1_1 ] )
        , ( ( Opra1_0_2 , Opra1_2_0 )
          , Set.fromList [ Opra1_2_0, Opra1_0_2, Opra1_s_0 ] )
        , ( ( Opra1_0_2 , Opra1_2_1 )
          , Set.fromList [ Opra1_s_3, Opra1_2_1, Opra1_0_3 ] )
        , ( ( Opra1_0_2 , Opra1_2_2 )
          , Set.fromList [ Opra1_2_2, Opra1_s_2, Opra1_0_0 ] )
        , ( ( Opra1_0_2 , Opra1_2_3 )
          , Set.fromList [ Opra1_2_3, Opra1_s_1, Opra1_0_1 ] )
        , ( ( Opra1_0_2 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_0_2 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1 ] )
        , ( ( Opra1_0_2 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_0_2 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0 ] )
        , ( ( Opra1_0_2 , Opra1_s_0 )
          , Set.fromList [ Opra1_0_2 ] )
        , ( ( Opra1_0_2 , Opra1_s_1 )
          , Set.fromList [ Opra1_0_1 ] )
        , ( ( Opra1_0_2 , Opra1_s_2 )
          , Set.fromList [ Opra1_0_0 ] )
        , ( ( Opra1_0_2 , Opra1_s_3 )
          , Set.fromList [ Opra1_0_3 ] )
        , ( ( Opra1_0_3 , Opra1_0_0 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_0_3 , Opra1_0_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1 ] )
        , ( ( Opra1_0_3 , Opra1_0_2 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_0_3 , Opra1_0_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0 ] )
        , ( ( Opra1_0_3 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_1, Opra1_1_3, Opra1_0_0 ] )
        , ( ( Opra1_0_3 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_1_3
                         , Opra1_1_1, Opra1_1_0, Opra1_0_1 ] )
        , ( ( Opra1_0_3 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_3, Opra1_1_1, Opra1_0_2 ] )
        , ( ( Opra1_0_3 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_0_3 ] )
        , ( ( Opra1_0_3 , Opra1_2_0 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_0_3 , Opra1_2_1 )
          , Set.fromList [ Opra1_1_3, Opra1_1_1, Opra1_1_0 ] )
        , ( ( Opra1_0_3 , Opra1_2_2 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_0_3 , Opra1_2_3 )
          , Set.fromList [ Opra1_1_3, Opra1_1_2, Opra1_1_1 ] )
        , ( ( Opra1_0_3 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_1, Opra1_2_0, Opra1_1_3, Opra1_0_2
                         , Opra1_s_0 ] )
        , ( ( Opra1_0_3 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_s_3
                         , Opra1_2_1, Opra1_1_3, Opra1_1_1, Opra1_1_0
                         , Opra1_0_3 ] )
        , ( ( Opra1_0_3 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_3, Opra1_2_2, Opra1_s_2, Opra1_1_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_0_3 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1, Opra1_s_1
                         , Opra1_0_1 ] )
        , ( ( Opra1_0_3 , Opra1_s_0 )
          , Set.fromList [ Opra1_0_3 ] )
        , ( ( Opra1_0_3 , Opra1_s_1 )
          , Set.fromList [ Opra1_0_3, Opra1_0_2, Opra1_0_1 ] )
        , ( ( Opra1_0_3 , Opra1_s_2 )
          , Set.fromList [ Opra1_0_1 ] )
        , ( ( Opra1_0_3 , Opra1_s_3 )
          , Set.fromList [ Opra1_0_3, Opra1_0_1, Opra1_0_0 ] )
        , ( ( Opra1_1_0 , Opra1_0_0 )
          , Set.fromList [ Opra1_3_0, Opra1_1_2, Opra1_s_1 ] )
        , ( ( Opra1_1_0 , Opra1_0_1 )
          , Set.fromList [ Opra1_3_1, Opra1_s_3, Opra1_1_3, Opra1_s_1
                         , Opra1_s_0 ] )
        , ( ( Opra1_1_0 , Opra1_0_2 )
          , Set.fromList [ Opra1_3_2, Opra1_s_3, Opra1_1_0 ] )
        , ( ( Opra1_1_0 , Opra1_0_3 )
          , Set.fromList [ Opra1_3_3, Opra1_s_3, Opra1_s_2, Opra1_1_1
                         , Opra1_s_1 ] )
        , ( ( Opra1_1_0 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_1, Opra1_1_1, Opra1_0_1 ] )
        , ( ( Opra1_1_0 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_0_3, Opra1_0_2
                         , Opra1_0_1 ] )
        , ( ( Opra1_1_0 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_3, Opra1_1_3, Opra1_0_3 ] )
        , ( ( Opra1_1_0 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_1_3
                         , Opra1_1_1, Opra1_1_0, Opra1_0_3, Opra1_0_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_1_0 , Opra1_2_0 )
          , Set.fromList [ Opra1_1_0 ] )
        , ( ( Opra1_1_0 , Opra1_2_1 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_1_0 , Opra1_2_2 )
          , Set.fromList [ Opra1_1_2 ] )
        , ( ( Opra1_1_0 , Opra1_2_3 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_1_0 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_3, Opra1_2_3, Opra1_1_3 ] )
        , ( ( Opra1_1_0 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_2_1, Opra1_2_0, Opra1_1_3, Opra1_1_1
                         , Opra1_1_0 ] )
        , ( ( Opra1_1_0 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_1, Opra1_2_1, Opra1_1_1 ] )
        , ( ( Opra1_1_0 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_2_3
                         , Opra1_2_2, Opra1_2_1, Opra1_1_3, Opra1_1_2
                         , Opra1_1_1 ] )
        , ( ( Opra1_1_0 , Opra1_s_0 )
          , Set.fromList [ Opra1_1_0 ] )
        , ( ( Opra1_1_0 , Opra1_s_1 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_1_0 , Opra1_s_2 )
          , Set.fromList [ Opra1_1_2 ] )
        , ( ( Opra1_1_0 , Opra1_s_3 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_1_1 , Opra1_0_0 )
          , Set.fromList [ Opra1_3_3, Opra1_2_3, Opra1_1_3 ] )
        , ( ( Opra1_1_1 , Opra1_0_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_2_1, Opra1_2_0, Opra1_1_3, Opra1_1_1
                         , Opra1_1_0 ] )
        , ( ( Opra1_1_1 , Opra1_0_2 )
          , Set.fromList [ Opra1_3_1, Opra1_2_1, Opra1_1_1 ] )
        , ( ( Opra1_1_1 , Opra1_0_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_2_3
                         , Opra1_2_2, Opra1_2_1, Opra1_1_3, Opra1_1_2
                         , Opra1_1_1 ] )
        , ( ( Opra1_1_1 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1, Opra1_s_1
                         , Opra1_0_1 ] )
        , ( ( Opra1_1_1 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_s_3, Opra1_2_3, Opra1_2_1, Opra1_2_0
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1, Opra1_1_0
                         , Opra1_s_1, Opra1_0_3, Opra1_0_2, Opra1_0_1
                         , Opra1_s_0 ] )
        , ( ( Opra1_1_1 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_s_3
                         , Opra1_2_1, Opra1_1_3, Opra1_1_1, Opra1_1_0
                         , Opra1_0_3 ] )
        , ( ( Opra1_1_1 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_s_3, Opra1_2_3, Opra1_2_2, Opra1_2_1
                         , Opra1_s_2, Opra1_1_3, Opra1_1_2, Opra1_1_1
                         , Opra1_1_0, Opra1_s_1, Opra1_0_3, Opra1_0_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_1_1 , Opra1_2_0 )
          , Set.fromList [ Opra1_3_1, Opra1_1_1, Opra1_0_1 ] )
        , ( ( Opra1_1_1 , Opra1_2_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_0_3, Opra1_0_2
                         , Opra1_0_1 ] )
        , ( ( Opra1_1_1 , Opra1_2_2 )
          , Set.fromList [ Opra1_3_3, Opra1_1_3, Opra1_0_3 ] )
        , ( ( Opra1_1_1 , Opra1_2_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_1_3
                         , Opra1_1_1, Opra1_1_0, Opra1_0_3, Opra1_0_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_1_1 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_2_3, Opra1_1_3
                         , Opra1_1_1, Opra1_1_0, Opra1_0_1 ] )
        , ( ( Opra1_1_1 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_2_3, Opra1_2_1, Opra1_2_0, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_1_0, Opra1_0_3
                         , Opra1_0_2, Opra1_0_1 ] )
        , ( ( Opra1_1_1 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_2_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_0_3 ] )
        , ( ( Opra1_1_1 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_2_3, Opra1_2_2, Opra1_2_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_1_0, Opra1_0_3
                         , Opra1_0_1, Opra1_0_0 ] )
        , ( ( Opra1_1_1 , Opra1_s_0 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_1_1 , Opra1_s_1 )
          , Set.fromList [ Opra1_1_3, Opra1_1_1, Opra1_1_0 ] )
        , ( ( Opra1_1_1 , Opra1_s_2 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_1_1 , Opra1_s_3 )
          , Set.fromList [ Opra1_1_3, Opra1_1_2, Opra1_1_1 ] )
        , ( ( Opra1_1_2 , Opra1_0_0 )
          , Set.fromList [ Opra1_1_0 ] )
        , ( ( Opra1_1_2 , Opra1_0_1 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_1_2 , Opra1_0_2 )
          , Set.fromList [ Opra1_1_2 ] )
        , ( ( Opra1_1_2 , Opra1_0_3 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_1_2 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_3, Opra1_2_3, Opra1_1_3 ] )
        , ( ( Opra1_1_2 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_2_1, Opra1_2_0, Opra1_1_3, Opra1_1_1
                         , Opra1_1_0 ] )
        , ( ( Opra1_1_2 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_1, Opra1_2_1, Opra1_1_1 ] )
        , ( ( Opra1_1_2 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_2_3
                         , Opra1_2_2, Opra1_2_1, Opra1_1_3, Opra1_1_2
                         , Opra1_1_1 ] )
        , ( ( Opra1_1_2 , Opra1_2_0 )
          , Set.fromList [ Opra1_3_0, Opra1_1_2, Opra1_s_1 ] )
        , ( ( Opra1_1_2 , Opra1_2_1 )
          , Set.fromList [ Opra1_3_1, Opra1_s_3, Opra1_1_3, Opra1_s_1
                         , Opra1_s_0 ] )
        , ( ( Opra1_1_2 , Opra1_2_2 )
          , Set.fromList [ Opra1_3_2, Opra1_s_3, Opra1_1_0 ] )
        , ( ( Opra1_1_2 , Opra1_2_3 )
          , Set.fromList [ Opra1_3_3, Opra1_s_3, Opra1_s_2, Opra1_1_1
                         , Opra1_s_1 ] )
        , ( ( Opra1_1_2 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_1, Opra1_1_1, Opra1_0_1 ] )
        , ( ( Opra1_1_2 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_0_3, Opra1_0_2
                         , Opra1_0_1 ] )
        , ( ( Opra1_1_2 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_3, Opra1_1_3, Opra1_0_3 ] )
        , ( ( Opra1_1_2 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_1_3
                         , Opra1_1_1, Opra1_1_0, Opra1_0_3, Opra1_0_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_1_2 , Opra1_s_0 )
          , Set.fromList [ Opra1_1_2 ] )
        , ( ( Opra1_1_2 , Opra1_s_1 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_1_2 , Opra1_s_2 )
          , Set.fromList [ Opra1_1_0 ] )
        , ( ( Opra1_1_2 , Opra1_s_3 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_1_3 , Opra1_0_0 )
          , Set.fromList [ Opra1_3_1, Opra1_1_1, Opra1_0_1 ] )
        , ( ( Opra1_1_3 , Opra1_0_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_0_3, Opra1_0_2
                         , Opra1_0_1 ] )
        , ( ( Opra1_1_3 , Opra1_0_2 )
          , Set.fromList [ Opra1_3_3, Opra1_1_3, Opra1_0_3 ] )
        , ( ( Opra1_1_3 , Opra1_0_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_1_3
                         , Opra1_1_1, Opra1_1_0, Opra1_0_3, Opra1_0_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_1_3 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_2_3, Opra1_1_3
                         , Opra1_1_1, Opra1_1_0, Opra1_0_1 ] )
        , ( ( Opra1_1_3 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_2_3, Opra1_2_1, Opra1_2_0, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_1_0, Opra1_0_3
                         , Opra1_0_2, Opra1_0_1 ] )
        , ( ( Opra1_1_3 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_2_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_0_3 ] )
        , ( ( Opra1_1_3 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_2_3, Opra1_2_2, Opra1_2_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_1_0, Opra1_0_3
                         , Opra1_0_1, Opra1_0_0 ] )
        , ( ( Opra1_1_3 , Opra1_2_0 )
          , Set.fromList [ Opra1_3_3, Opra1_2_3, Opra1_1_3 ] )
        , ( ( Opra1_1_3 , Opra1_2_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_2_1, Opra1_2_0, Opra1_1_3, Opra1_1_1
                         , Opra1_1_0 ] )
        , ( ( Opra1_1_3 , Opra1_2_2 )
          , Set.fromList [ Opra1_3_1, Opra1_2_1, Opra1_1_1 ] )
        , ( ( Opra1_1_3 , Opra1_2_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_2_3
                         , Opra1_2_2, Opra1_2_1, Opra1_1_3, Opra1_1_2
                         , Opra1_1_1 ] )
        , ( ( Opra1_1_3 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1, Opra1_s_1
                         , Opra1_0_1 ] )
        , ( ( Opra1_1_3 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_s_3, Opra1_2_3, Opra1_2_1, Opra1_2_0
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1, Opra1_1_0
                         , Opra1_s_1, Opra1_0_3, Opra1_0_2, Opra1_0_1
                         , Opra1_s_0 ] )
        , ( ( Opra1_1_3 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_s_3
                         , Opra1_2_1, Opra1_1_3, Opra1_1_1, Opra1_1_0
                         , Opra1_0_3 ] )
        , ( ( Opra1_1_3 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_s_3, Opra1_2_3, Opra1_2_2, Opra1_2_1
                         , Opra1_s_2, Opra1_1_3, Opra1_1_2, Opra1_1_1
                         , Opra1_1_0, Opra1_s_1, Opra1_0_3, Opra1_0_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_1_3 , Opra1_s_0 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_1_3 , Opra1_s_1 )
          , Set.fromList [ Opra1_1_3, Opra1_1_2, Opra1_1_1 ] )
        , ( ( Opra1_1_3 , Opra1_s_2 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_1_3 , Opra1_s_3 )
          , Set.fromList [ Opra1_1_3, Opra1_1_1, Opra1_1_0 ] )
        , ( ( Opra1_2_0 , Opra1_0_0 )
          , Set.fromList [ Opra1_2_2, Opra1_s_2, Opra1_0_0 ] )
        , ( ( Opra1_2_0 , Opra1_0_1 )
          , Set.fromList [ Opra1_2_3, Opra1_s_1, Opra1_0_1 ] )
        , ( ( Opra1_2_0 , Opra1_0_2 )
          , Set.fromList [ Opra1_2_0, Opra1_0_2, Opra1_s_0 ] )
        , ( ( Opra1_2_0 , Opra1_0_3 )
          , Set.fromList [ Opra1_s_3, Opra1_2_1, Opra1_0_3 ] )
        , ( ( Opra1_2_0 , Opra1_1_0 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_2_0 , Opra1_1_1 )
          , Set.fromList [ Opra1_1_3, Opra1_1_2, Opra1_1_1 ] )
        , ( ( Opra1_2_0 , Opra1_1_2 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_2_0 , Opra1_1_3 )
          , Set.fromList [ Opra1_1_3, Opra1_1_1, Opra1_1_0 ] )
        , ( ( Opra1_2_0 , Opra1_2_0 )
          , Set.fromList [ Opra1_2_0 ] )
        , ( ( Opra1_2_0 , Opra1_2_1 )
          , Set.fromList [ Opra1_2_1 ] )
        , ( ( Opra1_2_0 , Opra1_2_2 )
          , Set.fromList [ Opra1_2_2 ] )
        , ( ( Opra1_2_0 , Opra1_2_3 )
          , Set.fromList [ Opra1_2_3 ] )
        , ( ( Opra1_2_0 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_2_0 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0 ] )
        , ( ( Opra1_2_0 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_2_0 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1 ] )
        , ( ( Opra1_2_0 , Opra1_s_0 )
          , Set.fromList [ Opra1_2_0 ] )
        , ( ( Opra1_2_0 , Opra1_s_1 )
          , Set.fromList [ Opra1_2_3 ] )
        , ( ( Opra1_2_0 , Opra1_s_2 )
          , Set.fromList [ Opra1_2_2 ] )
        , ( ( Opra1_2_0 , Opra1_s_3 )
          , Set.fromList [ Opra1_2_1 ] )
        , ( ( Opra1_2_1 , Opra1_0_0 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_2_1 , Opra1_0_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0 ] )
        , ( ( Opra1_2_1 , Opra1_0_2 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_2_1 , Opra1_0_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1 ] )
        , ( ( Opra1_2_1 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_3, Opra1_2_2, Opra1_s_2, Opra1_1_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_2_1 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1, Opra1_s_1
                         , Opra1_0_1 ] )
        , ( ( Opra1_2_1 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_1, Opra1_2_0, Opra1_1_3, Opra1_0_2
                         , Opra1_s_0 ] )
        , ( ( Opra1_2_1 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_s_3
                         , Opra1_2_1, Opra1_1_3, Opra1_1_1, Opra1_1_0
                         , Opra1_0_3 ] )
        , ( ( Opra1_2_1 , Opra1_2_0 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_2_1 , Opra1_2_1 )
          , Set.fromList [ Opra1_1_3, Opra1_1_2, Opra1_1_1 ] )
        , ( ( Opra1_2_1 , Opra1_2_2 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_2_1 , Opra1_2_3 )
          , Set.fromList [ Opra1_1_3, Opra1_1_1, Opra1_1_0 ] )
        , ( ( Opra1_2_1 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_3, Opra1_2_0, Opra1_1_1 ] )
        , ( ( Opra1_2_1 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_1
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1 ] )
        , ( ( Opra1_2_1 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_1, Opra1_2_2, Opra1_1_3 ] )
        , ( ( Opra1_2_1 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_2_3
                         , Opra1_1_3, Opra1_1_1, Opra1_1_0 ] )
        , ( ( Opra1_2_1 , Opra1_s_0 )
          , Set.fromList [ Opra1_2_1 ] )
        , ( ( Opra1_2_1 , Opra1_s_1 )
          , Set.fromList [ Opra1_2_3, Opra1_2_1, Opra1_2_0 ] )
        , ( ( Opra1_2_1 , Opra1_s_2 )
          , Set.fromList [ Opra1_2_3 ] )
        , ( ( Opra1_2_1 , Opra1_s_3 )
          , Set.fromList [ Opra1_2_3, Opra1_2_2, Opra1_2_1 ] )
        , ( ( Opra1_2_2 , Opra1_0_0 )
          , Set.fromList [ Opra1_2_0 ] )
        , ( ( Opra1_2_2 , Opra1_0_1 )
          , Set.fromList [ Opra1_2_1 ] )
        , ( ( Opra1_2_2 , Opra1_0_2 )
          , Set.fromList [ Opra1_2_2 ] )
        , ( ( Opra1_2_2 , Opra1_0_3 )
          , Set.fromList [ Opra1_2_3 ] )
        , ( ( Opra1_2_2 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_2_2 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0 ] )
        , ( ( Opra1_2_2 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_2_2 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1 ] )
        , ( ( Opra1_2_2 , Opra1_2_0 )
          , Set.fromList [ Opra1_2_2, Opra1_s_2, Opra1_0_0 ] )
        , ( ( Opra1_2_2 , Opra1_2_1 )
          , Set.fromList [ Opra1_2_3, Opra1_s_1, Opra1_0_1 ] )
        , ( ( Opra1_2_2 , Opra1_2_2 )
          , Set.fromList [ Opra1_2_0, Opra1_0_2, Opra1_s_0 ] )
        , ( ( Opra1_2_2 , Opra1_2_3 )
          , Set.fromList [ Opra1_s_3, Opra1_2_1, Opra1_0_3 ] )
        , ( ( Opra1_2_2 , Opra1_3_0 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_2_2 , Opra1_3_1 )
          , Set.fromList [ Opra1_1_3, Opra1_1_2, Opra1_1_1 ] )
        , ( ( Opra1_2_2 , Opra1_3_2 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_2_2 , Opra1_3_3 )
          , Set.fromList [ Opra1_1_3, Opra1_1_1, Opra1_1_0 ] )
        , ( ( Opra1_2_2 , Opra1_s_0 )
          , Set.fromList [ Opra1_2_2 ] )
        , ( ( Opra1_2_2 , Opra1_s_1 )
          , Set.fromList [ Opra1_2_1 ] )
        , ( ( Opra1_2_2 , Opra1_s_2 )
          , Set.fromList [ Opra1_2_0 ] )
        , ( ( Opra1_2_2 , Opra1_s_3 )
          , Set.fromList [ Opra1_2_3 ] )
        , ( ( Opra1_2_3 , Opra1_0_0 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_2_3 , Opra1_0_1 )
          , Set.fromList [ Opra1_1_3, Opra1_1_2, Opra1_1_1 ] )
        , ( ( Opra1_2_3 , Opra1_0_2 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_2_3 , Opra1_0_3 )
          , Set.fromList [ Opra1_1_3, Opra1_1_1, Opra1_1_0 ] )
        , ( ( Opra1_2_3 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_3, Opra1_2_0, Opra1_1_1 ] )
        , ( ( Opra1_2_3 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_1
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1 ] )
        , ( ( Opra1_2_3 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_1, Opra1_2_2, Opra1_1_3 ] )
        , ( ( Opra1_2_3 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_2_3
                         , Opra1_1_3, Opra1_1_1, Opra1_1_0 ] )
        , ( ( Opra1_2_3 , Opra1_2_0 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_2_3 , Opra1_2_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0 ] )
        , ( ( Opra1_2_3 , Opra1_2_2 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_2_3 , Opra1_2_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1 ] )
        , ( ( Opra1_2_3 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_3, Opra1_2_2, Opra1_s_2, Opra1_1_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_2_3 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1, Opra1_s_1
                         , Opra1_0_1 ] )
        , ( ( Opra1_2_3 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_1, Opra1_2_0, Opra1_1_3, Opra1_0_2
                         , Opra1_s_0 ] )
        , ( ( Opra1_2_3 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_s_3
                         , Opra1_2_1, Opra1_1_3, Opra1_1_1, Opra1_1_0
                         , Opra1_0_3 ] )
        , ( ( Opra1_2_3 , Opra1_s_0 )
          , Set.fromList [ Opra1_2_3 ] )
        , ( ( Opra1_2_3 , Opra1_s_1 )
          , Set.fromList [ Opra1_2_3, Opra1_2_2, Opra1_2_1 ] )
        , ( ( Opra1_2_3 , Opra1_s_2 )
          , Set.fromList [ Opra1_2_1 ] )
        , ( ( Opra1_2_3 , Opra1_s_3 )
          , Set.fromList [ Opra1_2_3, Opra1_2_1, Opra1_2_0 ] )
        , ( ( Opra1_3_0 , Opra1_0_0 )
          , Set.fromList [ Opra1_3_2, Opra1_s_3, Opra1_1_0 ] )
        , ( ( Opra1_3_0 , Opra1_0_1 )
          , Set.fromList [ Opra1_3_3, Opra1_s_3, Opra1_s_2, Opra1_1_1
                         , Opra1_s_1 ] )
        , ( ( Opra1_3_0 , Opra1_0_2 )
          , Set.fromList [ Opra1_3_0, Opra1_1_2, Opra1_s_1 ] )
        , ( ( Opra1_3_0 , Opra1_0_3 )
          , Set.fromList [ Opra1_3_1, Opra1_s_3, Opra1_1_3, Opra1_s_1
                         , Opra1_s_0 ] )
        , ( ( Opra1_3_0 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_1, Opra1_2_1, Opra1_1_1 ] )
        , ( ( Opra1_3_0 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_2_3
                         , Opra1_2_2, Opra1_2_1, Opra1_1_3, Opra1_1_2
                         , Opra1_1_1 ] )
        , ( ( Opra1_3_0 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_3, Opra1_2_3, Opra1_1_3 ] )
        , ( ( Opra1_3_0 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_2_1, Opra1_2_0, Opra1_1_3, Opra1_1_1
                         , Opra1_1_0 ] )
        , ( ( Opra1_3_0 , Opra1_2_0 )
          , Set.fromList [ Opra1_3_0 ] )
        , ( ( Opra1_3_0 , Opra1_2_1 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_3_0 , Opra1_2_2 )
          , Set.fromList [ Opra1_3_2 ] )
        , ( ( Opra1_3_0 , Opra1_2_3 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_3_0 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_3, Opra1_1_3, Opra1_0_3 ] )
        , ( ( Opra1_3_0 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_1_3
                         , Opra1_1_1, Opra1_1_0, Opra1_0_3, Opra1_0_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_3_0 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_1, Opra1_1_1, Opra1_0_1 ] )
        , ( ( Opra1_3_0 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_0_3, Opra1_0_2
                         , Opra1_0_1 ] )
        , ( ( Opra1_3_0 , Opra1_s_0 )
          , Set.fromList [ Opra1_3_0 ] )
        , ( ( Opra1_3_0 , Opra1_s_1 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_3_0 , Opra1_s_2 )
          , Set.fromList [ Opra1_3_2 ] )
        , ( ( Opra1_3_0 , Opra1_s_3 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_3_1 , Opra1_0_0 )
          , Set.fromList [ Opra1_3_3, Opra1_1_3, Opra1_0_3 ] )
        , ( ( Opra1_3_1 , Opra1_0_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_1_3
                         , Opra1_1_1, Opra1_1_0, Opra1_0_3, Opra1_0_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_3_1 , Opra1_0_2 )
          , Set.fromList [ Opra1_3_1, Opra1_1_1, Opra1_0_1 ] )
        , ( ( Opra1_3_1 , Opra1_0_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_0_3, Opra1_0_2
                         , Opra1_0_1 ] )
        , ( ( Opra1_3_1 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_s_3
                         , Opra1_2_1, Opra1_1_3, Opra1_1_1, Opra1_1_0
                         , Opra1_0_3 ] )
        , ( ( Opra1_3_1 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_s_3, Opra1_2_3, Opra1_2_2, Opra1_2_1
                         , Opra1_s_2, Opra1_1_3, Opra1_1_2, Opra1_1_1
                         , Opra1_1_0, Opra1_s_1, Opra1_0_3, Opra1_0_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_3_1 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1, Opra1_s_1
                         , Opra1_0_1 ] )
        , ( ( Opra1_3_1 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_s_3, Opra1_2_3, Opra1_2_1, Opra1_2_0
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1, Opra1_1_0
                         , Opra1_s_1, Opra1_0_3, Opra1_0_2, Opra1_0_1
                         , Opra1_s_0 ] )
        , ( ( Opra1_3_1 , Opra1_2_0 )
          , Set.fromList [ Opra1_3_1, Opra1_2_1, Opra1_1_1 ] )
        , ( ( Opra1_3_1 , Opra1_2_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_2_3
                         , Opra1_2_2, Opra1_2_1, Opra1_1_3, Opra1_1_2
                         , Opra1_1_1 ] )
        , ( ( Opra1_3_1 , Opra1_2_2 )
          , Set.fromList [ Opra1_3_3, Opra1_2_3, Opra1_1_3 ] )
        , ( ( Opra1_3_1 , Opra1_2_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_2_1, Opra1_2_0, Opra1_1_3, Opra1_1_1
                         , Opra1_1_0 ] )
        , ( ( Opra1_3_1 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_1
                         , Opra1_1_3, Opra1_1_1, Opra1_0_3 ] )
        , ( ( Opra1_3_1 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_2_3, Opra1_2_2, Opra1_2_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_1_0, Opra1_0_3
                         , Opra1_0_1, Opra1_0_0 ] )
        , ( ( Opra1_3_1 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_2_3
                         , Opra1_1_3, Opra1_1_1, Opra1_0_1 ] )
        , ( ( Opra1_3_1 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_2_3, Opra1_2_1, Opra1_2_0, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_1_0, Opra1_0_3
                         , Opra1_0_2, Opra1_0_1 ] )
        , ( ( Opra1_3_1 , Opra1_s_0 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_3_1 , Opra1_s_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0 ] )
        , ( ( Opra1_3_1 , Opra1_s_2 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_3_1 , Opra1_s_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1 ] )
        , ( ( Opra1_3_2 , Opra1_0_0 )
          , Set.fromList [ Opra1_3_0 ] )
        , ( ( Opra1_3_2 , Opra1_0_1 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_3_2 , Opra1_0_2 )
          , Set.fromList [ Opra1_3_2 ] )
        , ( ( Opra1_3_2 , Opra1_0_3 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_3_2 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_3, Opra1_1_3, Opra1_0_3 ] )
        , ( ( Opra1_3_2 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_1_3
                         , Opra1_1_1, Opra1_1_0, Opra1_0_3, Opra1_0_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_3_2 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_1, Opra1_1_1, Opra1_0_1 ] )
        , ( ( Opra1_3_2 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_0_3, Opra1_0_2
                         , Opra1_0_1 ] )
        , ( ( Opra1_3_2 , Opra1_2_0 )
          , Set.fromList [ Opra1_3_2, Opra1_s_3, Opra1_1_0 ] )
        , ( ( Opra1_3_2 , Opra1_2_1 )
          , Set.fromList [ Opra1_3_3, Opra1_s_3, Opra1_s_2, Opra1_1_1
                         , Opra1_s_1 ] )
        , ( ( Opra1_3_2 , Opra1_2_2 )
          , Set.fromList [ Opra1_3_0, Opra1_1_2, Opra1_s_1 ] )
        , ( ( Opra1_3_2 , Opra1_2_3 )
          , Set.fromList [ Opra1_3_1, Opra1_s_3, Opra1_1_3, Opra1_s_1
                         , Opra1_s_0 ] )
        , ( ( Opra1_3_2 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_1, Opra1_2_1, Opra1_1_1 ] )
        , ( ( Opra1_3_2 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_2_3
                         , Opra1_2_2, Opra1_2_1, Opra1_1_3, Opra1_1_2
                         , Opra1_1_1 ] )
        , ( ( Opra1_3_2 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_3, Opra1_2_3, Opra1_1_3 ] )
        , ( ( Opra1_3_2 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_2_1, Opra1_2_0, Opra1_1_3, Opra1_1_1
                         , Opra1_1_0 ] )
        , ( ( Opra1_3_2 , Opra1_s_0 )
          , Set.fromList [ Opra1_3_2 ] )
        , ( ( Opra1_3_2 , Opra1_s_1 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_3_2 , Opra1_s_2 )
          , Set.fromList [ Opra1_3_0 ] )
        , ( ( Opra1_3_2 , Opra1_s_3 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_3_3 , Opra1_0_0 )
          , Set.fromList [ Opra1_3_1, Opra1_2_1, Opra1_1_1 ] )
        , ( ( Opra1_3_3 , Opra1_0_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_2_3
                         , Opra1_2_2, Opra1_2_1, Opra1_1_3, Opra1_1_2
                         , Opra1_1_1 ] )
        , ( ( Opra1_3_3 , Opra1_0_2 )
          , Set.fromList [ Opra1_3_3, Opra1_2_3, Opra1_1_3 ] )
        , ( ( Opra1_3_3 , Opra1_0_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_2_1, Opra1_2_0, Opra1_1_3, Opra1_1_1
                         , Opra1_1_0 ] )
        , ( ( Opra1_3_3 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_1
                         , Opra1_1_3, Opra1_1_1, Opra1_0_3 ] )
        , ( ( Opra1_3_3 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_2_3, Opra1_2_2, Opra1_2_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_1_0, Opra1_0_3
                         , Opra1_0_1, Opra1_0_0 ] )
        , ( ( Opra1_3_3 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_2_3
                         , Opra1_1_3, Opra1_1_1, Opra1_0_1 ] )
        , ( ( Opra1_3_3 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_2_3, Opra1_2_1, Opra1_2_0, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_1_0, Opra1_0_3
                         , Opra1_0_2, Opra1_0_1 ] )
        , ( ( Opra1_3_3 , Opra1_2_0 )
          , Set.fromList [ Opra1_3_3, Opra1_1_3, Opra1_0_3 ] )
        , ( ( Opra1_3_3 , Opra1_2_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_1_3
                         , Opra1_1_1, Opra1_1_0, Opra1_0_3, Opra1_0_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_3_3 , Opra1_2_2 )
          , Set.fromList [ Opra1_3_1, Opra1_1_1, Opra1_0_1 ] )
        , ( ( Opra1_3_3 , Opra1_2_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_1_3
                         , Opra1_1_2, Opra1_1_1, Opra1_0_3, Opra1_0_2
                         , Opra1_0_1 ] )
        , ( ( Opra1_3_3 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_s_3
                         , Opra1_2_1, Opra1_1_3, Opra1_1_1, Opra1_1_0
                         , Opra1_0_3 ] )
        , ( ( Opra1_3_3 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_s_3, Opra1_2_3, Opra1_2_2, Opra1_2_1
                         , Opra1_s_2, Opra1_1_3, Opra1_1_2, Opra1_1_1
                         , Opra1_1_0, Opra1_s_1, Opra1_0_3, Opra1_0_1
                         , Opra1_0_0 ] )
        , ( ( Opra1_3_3 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0, Opra1_2_3
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1, Opra1_s_1
                         , Opra1_0_1 ] )
        , ( ( Opra1_3_3 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1, Opra1_3_0
                         , Opra1_s_3, Opra1_2_3, Opra1_2_1, Opra1_2_0
                         , Opra1_1_3, Opra1_1_2, Opra1_1_1, Opra1_1_0
                         , Opra1_s_1, Opra1_0_3, Opra1_0_2, Opra1_0_1
                         , Opra1_s_0 ] )
        , ( ( Opra1_3_3 , Opra1_s_0 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_3_3 , Opra1_s_1 )
          , Set.fromList [ Opra1_3_3, Opra1_3_2, Opra1_3_1 ] )
        , ( ( Opra1_3_3 , Opra1_s_2 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_3_3 , Opra1_s_3 )
          , Set.fromList [ Opra1_3_3, Opra1_3_1, Opra1_3_0 ] )
        , ( ( Opra1_s_0 , Opra1_0_0 )
          , Set.fromList [ Opra1_0_0 ] )
        , ( ( Opra1_s_0 , Opra1_0_1 )
          , Set.fromList [ Opra1_0_1 ] )
        , ( ( Opra1_s_0 , Opra1_0_2 )
          , Set.fromList [ Opra1_0_2 ] )
        , ( ( Opra1_s_0 , Opra1_0_3 )
          , Set.fromList [ Opra1_0_3 ] )
        , ( ( Opra1_s_0 , Opra1_1_0 )
          , Set.fromList [ Opra1_1_0 ] )
        , ( ( Opra1_s_0 , Opra1_1_1 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_s_0 , Opra1_1_2 )
          , Set.fromList [ Opra1_1_2 ] )
        , ( ( Opra1_s_0 , Opra1_1_3 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_s_0 , Opra1_2_0 )
          , Set.fromList [ Opra1_2_0 ] )
        , ( ( Opra1_s_0 , Opra1_2_1 )
          , Set.fromList [ Opra1_2_1 ] )
        , ( ( Opra1_s_0 , Opra1_2_2 )
          , Set.fromList [ Opra1_2_2 ] )
        , ( ( Opra1_s_0 , Opra1_2_3 )
          , Set.fromList [ Opra1_2_3 ] )
        , ( ( Opra1_s_0 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_0 ] )
        , ( ( Opra1_s_0 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_s_0 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_2 ] )
        , ( ( Opra1_s_0 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_s_0 , Opra1_s_0 )
          , Set.fromList [ Opra1_s_0 ] )
        , ( ( Opra1_s_0 , Opra1_s_1 )
          , Set.fromList [ Opra1_s_1 ] )
        , ( ( Opra1_s_0 , Opra1_s_2 )
          , Set.fromList [ Opra1_s_2 ] )
        , ( ( Opra1_s_0 , Opra1_s_3 )
          , Set.fromList [ Opra1_s_3 ] )
        , ( ( Opra1_s_1 , Opra1_0_0 )
          , Set.fromList [ Opra1_1_0 ] )
        , ( ( Opra1_s_1 , Opra1_0_1 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_s_1 , Opra1_0_2 )
          , Set.fromList [ Opra1_1_2 ] )
        , ( ( Opra1_s_1 , Opra1_0_3 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_s_1 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_0, Opra1_2_0, Opra1_1_0 ] )
        , ( ( Opra1_s_1 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_1, Opra1_2_1, Opra1_1_1 ] )
        , ( ( Opra1_s_1 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_2, Opra1_2_2, Opra1_1_2 ] )
        , ( ( Opra1_s_1 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_2_3, Opra1_1_3 ] )
        , ( ( Opra1_s_1 , Opra1_2_0 )
          , Set.fromList [ Opra1_3_0 ] )
        , ( ( Opra1_s_1 , Opra1_2_1 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_s_1 , Opra1_2_2 )
          , Set.fromList [ Opra1_3_2 ] )
        , ( ( Opra1_s_1 , Opra1_2_3 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_s_1 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_0, Opra1_1_0, Opra1_0_0 ] )
        , ( ( Opra1_s_1 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_1, Opra1_1_1, Opra1_0_1 ] )
        , ( ( Opra1_s_1 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_2, Opra1_1_2, Opra1_0_2 ] )
        , ( ( Opra1_s_1 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_1_3, Opra1_0_3 ] )
        , ( ( Opra1_s_1 , Opra1_s_0 )
          , Set.fromList [ Opra1_s_1 ] )
        , ( ( Opra1_s_1 , Opra1_s_1 )
          , Set.fromList [ Opra1_s_3, Opra1_s_2, Opra1_s_1 ] )
        , ( ( Opra1_s_1 , Opra1_s_2 )
          , Set.fromList [ Opra1_s_3 ] )
        , ( ( Opra1_s_1 , Opra1_s_3 )
          , Set.fromList [ Opra1_s_3, Opra1_s_1, Opra1_s_0 ] )
        , ( ( Opra1_s_2 , Opra1_0_0 )
          , Set.fromList [ Opra1_2_0 ] )
        , ( ( Opra1_s_2 , Opra1_0_1 )
          , Set.fromList [ Opra1_2_1 ] )
        , ( ( Opra1_s_2 , Opra1_0_2 )
          , Set.fromList [ Opra1_2_2 ] )
        , ( ( Opra1_s_2 , Opra1_0_3 )
          , Set.fromList [ Opra1_2_3 ] )
        , ( ( Opra1_s_2 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_0 ] )
        , ( ( Opra1_s_2 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_s_2 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_2 ] )
        , ( ( Opra1_s_2 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_s_2 , Opra1_2_0 )
          , Set.fromList [ Opra1_0_0 ] )
        , ( ( Opra1_s_2 , Opra1_2_1 )
          , Set.fromList [ Opra1_0_1 ] )
        , ( ( Opra1_s_2 , Opra1_2_2 )
          , Set.fromList [ Opra1_0_2 ] )
        , ( ( Opra1_s_2 , Opra1_2_3 )
          , Set.fromList [ Opra1_0_3 ] )
        , ( ( Opra1_s_2 , Opra1_3_0 )
          , Set.fromList [ Opra1_1_0 ] )
        , ( ( Opra1_s_2 , Opra1_3_1 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_s_2 , Opra1_3_2 )
          , Set.fromList [ Opra1_1_2 ] )
        , ( ( Opra1_s_2 , Opra1_3_3 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_s_2 , Opra1_s_0 )
          , Set.fromList [ Opra1_s_2 ] )
        , ( ( Opra1_s_2 , Opra1_s_1 )
          , Set.fromList [ Opra1_s_3 ] )
        , ( ( Opra1_s_2 , Opra1_s_2 )
          , Set.fromList [ Opra1_s_0 ] )
        , ( ( Opra1_s_2 , Opra1_s_3 )
          , Set.fromList [ Opra1_s_1 ] )
        , ( ( Opra1_s_3 , Opra1_0_0 )
          , Set.fromList [ Opra1_3_0 ] )
        , ( ( Opra1_s_3 , Opra1_0_1 )
          , Set.fromList [ Opra1_3_1 ] )
        , ( ( Opra1_s_3 , Opra1_0_2 )
          , Set.fromList [ Opra1_3_2 ] )
        , ( ( Opra1_s_3 , Opra1_0_3 )
          , Set.fromList [ Opra1_3_3 ] )
        , ( ( Opra1_s_3 , Opra1_1_0 )
          , Set.fromList [ Opra1_3_0, Opra1_1_0, Opra1_0_0 ] )
        , ( ( Opra1_s_3 , Opra1_1_1 )
          , Set.fromList [ Opra1_3_1, Opra1_1_1, Opra1_0_1 ] )
        , ( ( Opra1_s_3 , Opra1_1_2 )
          , Set.fromList [ Opra1_3_2, Opra1_1_2, Opra1_0_2 ] )
        , ( ( Opra1_s_3 , Opra1_1_3 )
          , Set.fromList [ Opra1_3_3, Opra1_1_3, Opra1_0_3 ] )
        , ( ( Opra1_s_3 , Opra1_2_0 )
          , Set.fromList [ Opra1_1_0 ] )
        , ( ( Opra1_s_3 , Opra1_2_1 )
          , Set.fromList [ Opra1_1_1 ] )
        , ( ( Opra1_s_3 , Opra1_2_2 )
          , Set.fromList [ Opra1_1_2 ] )
        , ( ( Opra1_s_3 , Opra1_2_3 )
          , Set.fromList [ Opra1_1_3 ] )
        , ( ( Opra1_s_3 , Opra1_3_0 )
          , Set.fromList [ Opra1_3_0, Opra1_2_0, Opra1_1_0 ] )
        , ( ( Opra1_s_3 , Opra1_3_1 )
          , Set.fromList [ Opra1_3_1, Opra1_2_1, Opra1_1_1 ] )
        , ( ( Opra1_s_3 , Opra1_3_2 )
          , Set.fromList [ Opra1_3_2, Opra1_2_2, Opra1_1_2 ] )
        , ( ( Opra1_s_3 , Opra1_3_3 )
          , Set.fromList [ Opra1_3_3, Opra1_2_3, Opra1_1_3 ] )
        , ( ( Opra1_s_3 , Opra1_s_0 )
          , Set.fromList [ Opra1_s_3 ] )
        , ( ( Opra1_s_3 , Opra1_s_1 )
          , Set.fromList [ Opra1_s_3, Opra1_s_1, Opra1_s_0 ] )
        , ( ( Opra1_s_3 , Opra1_s_2 )
          , Set.fromList [ Opra1_s_1 ] )
        , ( ( Opra1_s_3 , Opra1_s_3 )
          , Set.fromList [ Opra1_s_3, Opra1_s_2, Opra1_s_1 ] )
        ]
