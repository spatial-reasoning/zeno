module Calculus.Dipole24 where

-- standard modules
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

-- local modules
import Basics
import Helpful


data Dipole24 = ELLS24 | ERRS24 | ESES24 | LERE24 | LLLL24 | LLLR24
              | LLRL24 | LLRR24 | LRLL24 | LRRL24 | LRRR24 | LSEL24
              | RELE24 | RLLL24 | RLLR24 | RLRR24 | RRLL24 | RRLR24
              | RRRL24 | RRRR24 | RSER24 | SESE24 | SLSR24 | SRSL24
              deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Calculus Dipole24 where
    rank _ = 2
    readRel x = case y of
        Just z  -> z
        Nothing -> error $ show x ++ " is not a Dipole-24 relation."
      where
        y = maybeRead $ (++ "24") $ map Char.toUpper x
    showRel = (map Char.toLower) . (take 4) . show

    identity = SESE24

    bcConversion = Map.fromList
        [ ( ELLS24 , Set.singleton LSEL24 )
        , ( ERRS24 , Set.singleton RSER24 )
        , ( ESES24 , Set.singleton ESES24 )
        , ( LERE24 , Set.singleton RELE24 )
        , ( LLLL24 , Set.singleton LLLL24 )
        , ( LLLR24 , Set.singleton LRLL24 )
        , ( LLRL24 , Set.singleton RLLL24 )
        , ( LLRR24 , Set.singleton RRLL24 )
        , ( LRLL24 , Set.singleton LLLR24 )
        , ( LRRL24 , Set.singleton RLLR24 )
        , ( LRRR24 , Set.singleton RRLR24 )
        , ( LSEL24 , Set.singleton ELLS24 )
        , ( RELE24 , Set.singleton LERE24 )
        , ( RLLL24 , Set.singleton LLRL24 )
        , ( RLLR24 , Set.singleton LRRL24 )
        , ( RLRR24 , Set.singleton RRRL24 )
        , ( RRLL24 , Set.singleton LLRR24 )
        , ( RRLR24 , Set.singleton LRRR24 )
        , ( RRRL24 , Set.singleton RLRR24 )
        , ( RRRR24 , Set.singleton RRRR24 )
        , ( RSER24 , Set.singleton ERRS24 )
        , ( SESE24 , Set.singleton SESE24 )
        , ( SLSR24 , Set.singleton SRSL24 )
        , ( SRSL24 , Set.singleton SLSR24 ) ]

    bcComposition = Map.fromList
        [ ( ( ELLS24 , ELLS24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24 ] )
        , ( ( ELLS24 , ERRS24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24 ] )
        , ( ( ELLS24 , ESES24 )
          , Set.fromList
                [ LERE24 ] )
        , ( ( ELLS24 , LERE24 )
          , Set.fromList
                [ LLLR24, LLRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( ELLS24 , LLLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24, RRRL24
                , SRSL24 ] )
        , ( ( ELLS24 , LLLR24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24, RRRL24
                , SRSL24 ] )
        , ( ( ELLS24 , LLRL24 )
          , Set.fromList
                [ LLLR24, LLRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24
                , SLSR24 ] )
        , ( ( ELLS24 , LLRR24 )
          , Set.fromList
                [ LLLR24, LLRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24
                , SLSR24 ] )
        , ( ( ELLS24 , LRLL24 )
          , Set.fromList
                [ LRLL24, LRRL24, RLLL24, RRLL24, RRRL24, SRSL24 ] )
        , ( ( ELLS24 , LRRL24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( ELLS24 , LRRR24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( ELLS24 , LSEL24 )
          , Set.fromList
                [ LERE24, RELE24, SESE24 ] )
        , ( ( ELLS24 , RELE24 )
          , Set.fromList
                [ LLLL24, LLRL24, RLLL24 ] )
        , ( ( ELLS24 , RLLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RLLL24 ] )
        , ( ( ELLS24 , RLLR24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RLLL24 ] )
        , ( ( ELLS24 , RLRR24 )
          , Set.fromList
                [ LRRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24 ] )
        , ( ( ELLS24 , RRLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, RLLL24, RRLL24, RRRL24 ] )
        , ( ( ELLS24 , RRLR24 )
          , Set.fromList
                [ LLLL24, LLRL24, RLLL24, RRLL24 ] )
        , ( ( ELLS24 , RRRL24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24, RRRR24 ] )
        , ( ( ELLS24 , RRRR24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24, RRLR24, RRRR24 ] )
        , ( ( ELLS24 , RSER24 )
          , Set.fromList
                [ LERE24, RELE24 ] )
        , ( ( ELLS24 , SESE24 )
          , Set.fromList
                [ ELLS24 ] )
        , ( ( ELLS24 , SLSR24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24 ] )
        , ( ( ELLS24 , SRSL24 )
          , Set.fromList
                [ ELLS24, ERRS24 ] )
        , ( ( ERRS24 , ELLS24 )
          , Set.fromList
                [ RLLL24, RRLL24, RRRL24 ] )
        , ( ( ERRS24 , ERRS24 )
          , Set.fromList
                [ RLLR24, RLRR24, RRLR24, RRRR24, RSER24 ] )
        , ( ( ERRS24 , ESES24 )
          , Set.fromList
                [ RELE24 ] )
        , ( ( ERRS24 , LERE24 )
          , Set.fromList
                [ LRRR24, RRLR24, RRRR24 ] )
        , ( ( ERRS24 , LLLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, RLLL24, RRLL24, RRRL24 ] )
        , ( ( ERRS24 , LLLR24 )
          , Set.fromList
                [ LLLL24, RLLL24, RRLL24, RRRL24 ] )
        , ( ( ERRS24 , LLRL24 )
          , Set.fromList
                [ LLRR24, LRRR24, RRLR24, RRRR24 ] )
        , ( ( ERRS24 , LLRR24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24, RRLR24, RRRR24 ] )
        , ( ( ERRS24 , LRLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RLLL24 ] )
        , ( ( ERRS24 , LRRL24 )
          , Set.fromList
                [ LRRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24 ] )
        , ( ( ERRS24 , LRRR24 )
          , Set.fromList
                [ LRRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24 ] )
        , ( ( ERRS24 , LSEL24 )
          , Set.fromList
                [ LERE24, RELE24 ] )
        , ( ( ERRS24 , RELE24 )
          , Set.fromList
                [ LRLL24, LRRL24, RRLL24, RRRL24, SRSL24 ] )
        , ( ( ERRS24 , RLLL24 )
          , Set.fromList
                [ LRLL24, LRRL24, RLLL24, RRLL24, RRRL24, SRSL24 ] )
        , ( ( ERRS24 , RLLR24 )
          , Set.fromList
                [ LRLL24, LRRL24, RLLL24, RRLL24, RRRL24, SRSL24 ] )
        , ( ( ERRS24 , RLRR24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( ERRS24 , RRLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24, RRRL24
                , SRSL24 ] )
        , ( ( ERRS24 , RRLR24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24, RRRL24
                , SRSL24 ] )
        , ( ( ERRS24 , RRRL24 )
          , Set.fromList
                [ LLLR24, LLRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24
                , SLSR24 ] )
        , ( ( ERRS24 , RRRR24 )
          , Set.fromList
                [ LLLR24, LLRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24
                , SLSR24 ] )
        , ( ( ERRS24 , RSER24 )
          , Set.fromList
                [ LERE24, RELE24, SESE24 ] )
        , ( ( ERRS24 , SESE24 )
          , Set.fromList
                [ ERRS24 ] )
        , ( ( ERRS24 , SLSR24 )
          , Set.fromList
                [ ELLS24, ERRS24 ] )
        , ( ( ERRS24 , SRSL24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24 ] )
        , ( ( ESES24 , ELLS24 )
          , Set.fromList
                [ SRSL24 ] )
        , ( ( ESES24 , ERRS24 )
          , Set.fromList
                [ SLSR24 ] )
        , ( ( ESES24 , ESES24 )
          , Set.fromList
                [ SESE24 ] )
        , ( ( ESES24 , LERE24 )
          , Set.fromList
                [ RSER24 ] )
        , ( ( ESES24 , LLLL24 )
          , Set.fromList
                [ RRLL24 ] )
        , ( ( ESES24 , LLLR24 )
          , Set.fromList
                [ RRRL24 ] )
        , ( ( ESES24 , LLRL24 )
          , Set.fromList
                [ RRLR24 ] )
        , ( ( ESES24 , LLRR24 )
          , Set.fromList
                [ RRRR24 ] )
        , ( ( ESES24 , LRLL24 )
          , Set.fromList
                [ RLLL24 ] )
        , ( ( ESES24 , LRRL24 )
          , Set.fromList
                [ RLLR24 ] )
        , ( ( ESES24 , LRRR24 )
          , Set.fromList
                [ RLRR24 ] )
        , ( ( ESES24 , LSEL24 )
          , Set.fromList
                [ RELE24 ] )
        , ( ( ESES24 , RELE24 )
          , Set.fromList
                [ LSEL24 ] )
        , ( ( ESES24 , RLLL24 )
          , Set.fromList
                [ LRLL24 ] )
        , ( ( ESES24 , RLLR24 )
          , Set.fromList
                [ LRRL24 ] )
        , ( ( ESES24 , RLRR24 )
          , Set.fromList
                [ LRRR24 ] )
        , ( ( ESES24 , RRLL24 )
          , Set.fromList
                [ LLLL24 ] )
        , ( ( ESES24 , RRLR24 )
          , Set.fromList
                [ LLRL24 ] )
        , ( ( ESES24 , RRRL24 )
          , Set.fromList
                [ LLLR24 ] )
        , ( ( ESES24 , RRRR24 )
          , Set.fromList
                [ LLRR24 ] )
        , ( ( ESES24 , RSER24 )
          , Set.fromList
                [ LERE24 ] )
        , ( ( ESES24 , SESE24 )
          , Set.fromList
                [ ESES24 ] )
        , ( ( ESES24 , SLSR24 )
          , Set.fromList
                [ ERRS24 ] )
        , ( ( ESES24 , SRSL24 )
          , Set.fromList
                [ ELLS24 ] )
        , ( ( LERE24 , ELLS24 )
          , Set.fromList
                [ ELLS24, ERRS24 ] )
        , ( ( LERE24 , ERRS24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24 ] )
        , ( ( LERE24 , ESES24 )
          , Set.fromList
                [ ELLS24 ] )
        , ( ( LERE24 , LERE24 )
          , Set.fromList
                [ LERE24, RELE24 ] )
        , ( ( LERE24 , LLLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, RLLL24, RRLL24, RRRL24 ] )
        , ( ( LERE24 , LLLR24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24, RRRR24 ] )
        , ( ( LERE24 , LLRL24 )
          , Set.fromList
                [ LLLL24, LLRL24, RLLL24, RRLL24 ] )
        , ( ( LERE24 , LLRR24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24, RRLR24, RRRR24 ] )
        , ( ( LERE24 , LRLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RLLL24 ] )
        , ( ( LERE24 , LRRL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RLLL24 ] )
        , ( ( LERE24 , LRRR24 )
          , Set.fromList
                [ LRRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24 ] )
        , ( ( LERE24 , LSEL24 )
          , Set.fromList
                [ LLLL24, LLRL24, RLLL24 ] )
        , ( ( LERE24 , RELE24 )
          , Set.fromList
                [ LERE24, RELE24, SESE24 ] )
        , ( ( LERE24 , RLLL24 )
          , Set.fromList
                [ LRLL24, LRRL24, RLLL24, RRLL24, RRRL24, SRSL24 ] )
        , ( ( LERE24 , RLLR24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( LERE24 , RLRR24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( LERE24 , RRLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24, RRRL24
                , SRSL24 ] )
        , ( ( LERE24 , RRLR24 )
          , Set.fromList
                [ LLLR24, LLRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24
                , SLSR24 ] )
        , ( ( LERE24 , RRRL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24, RRRL24
                , SRSL24 ] )
        , ( ( LERE24 , RRRR24 )
          , Set.fromList
                [ LLLR24, LLRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24
                , SLSR24 ] )
        , ( ( LERE24 , RSER24 )
          , Set.fromList
                [ LLLR24, LLRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( LERE24 , SESE24 )
          , Set.fromList
                [ LERE24 ] )
        , ( ( LERE24 , SLSR24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24 ] )
        , ( ( LERE24 , SRSL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24 ] )
        , ( ( LLLL24 , ELLS24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24 ] )
        , ( ( LLLL24 , ERRS24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRLL24, LRRR24 ] )
        , ( ( LLLL24 , ESES24 )
          , Set.fromList
                [ LLRR24 ] )
        , ( ( LLLL24 , LERE24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RLLL24, RLLR24, RLRR24
                , SLSR24 ] )
        , ( ( LLLL24 , LLLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24, SESE24
                , SLSR24, SRSL24 ] )
        , ( ( LLLL24 , LLLR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24
                , LRRR24, LSEL24, RRLL24, RRRL24, RRRR24, SRSL24 ] )
        , ( ( LLLL24 , LLRL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRR24, RSER24, SLSR24 ] )
        , ( ( LLLL24 , LLRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LERE24, LLLL24, LLLR24, LLRL24
                , LLRR24, LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( LLLL24 , LRLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRL24
                , LRRR24, RLLL24, RLLR24, RLRR24, RRLL24, RRRL24, RRRR24
                , SLSR24, SRSL24 ] )
        , ( ( LLLL24 , LRRL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRR24, RLLL24
                , RLLR24, RLRR24, SLSR24 ] )
        , ( ( LLLL24 , LRRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRL24
                , LRRR24, RLLL24, RLLR24, RLRR24, RRLL24, RRRL24, RRRR24
                , SLSR24, SRSL24 ] )
        , ( ( LLLL24 , LSEL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RLLL24, RLLR24, RLRR24
                , SLSR24 ] )
        , ( ( LLLL24 , RELE24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLLL24, RLRR24 ] )
        , ( ( LLLL24 , RLLL24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( LLLL24 , RLLR24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RLLL24, RLRR24 ] )
        , ( ( LLLL24 , RLRR24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( LLLL24 , RRLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRR24, RLLL24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( LLLL24 , RRLR24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLLL24, RLRR24, RRLL24
                , RRRR24 ] )
        , ( ( LLLL24 , RRRL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRLL24, LRRR24, RRLL24
                , RRRR24 ] )
        , ( ( LLLL24 , RRRR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRR24, RLLL24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( LLLL24 , RSER24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLLL24, RLRR24 ] )
        , ( ( LLLL24 , SESE24 )
          , Set.fromList
                [ LLLL24 ] )
        , ( ( LLLL24 , SLSR24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24 ] )
        , ( ( LLLL24 , SRSL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRLL24, LRRR24 ] )
        , ( ( LLLR24 , ELLS24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24 ] )
        , ( ( LLLR24 , ERRS24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24 ] )
        , ( ( LLLR24 , ESES24 )
          , Set.fromList
                [ LLRL24 ] )
        , ( ( LLLR24 , LERE24 )
          , Set.fromList
                [ LLLR24, LLRL24, LLRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( LLLR24 , LLLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LSEL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( LLLR24 , LLLR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24
                , RRRL24, SRSL24 ] )
        , ( ( LLLR24 , LLRL24 )
          , Set.fromList
                [ LLLR24, LLRL24, LLRR24, RLLR24, RLRR24, RRLR24, RRRR24
                , RSER24, SLSR24 ] )
        , ( ( LLLR24 , LLRR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LSEL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( LLLR24 , LRLL24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRL24
                , SESE24, SLSR24, SRSL24 ] )
        , ( ( LLLR24 , LRRL24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24, RLLR24
                , RLRR24, SLSR24 ] )
        , ( ( LLLR24 , LRRR24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24, RLLR24
                , RLRR24, SLSR24 ] )
        , ( ( LLLR24 , LSEL24 )
          , Set.fromList
                [ LLLR24, LLRL24, LLRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( LLLR24 , RELE24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, RLLL24, RLLR24 ] )
        , ( ( LLLR24 , RLLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24
                , RLLL24, RLLR24 ] )
        , ( ( LLLR24 , RLLR24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24
                , RLLL24, RLLR24 ] )
        , ( ( LLLR24 , RLRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LLLL24, LLLR24, LLRL24, LRLL24
                , LRRL24, LRRR24, LSEL24, RLLL24, RLLR24, RLRR24, RRLR24
                , RRRL24, RRRR24, RSER24 ] )
        , ( ( LLLR24 , RRLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRRL24, LRRR24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( LLLR24 , RRLR24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, RELE24, RLLL24, RLLR24
                , RRLL24, RRLR24 ] )
        , ( ( LLLR24 , RRRL24 )
          , Set.fromList
                [ ERRS24, LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24
                , RRRL24, RRRR24 ] )
        , ( ( LLLR24 , RRRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRRL24, LRRR24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( LLLR24 , RSER24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, RLLL24, RLLR24 ] )
        , ( ( LLLR24 , SESE24 )
          , Set.fromList
                [ LLLR24 ] )
        , ( ( LLLR24 , SLSR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24 ] )
        , ( ( LLLR24 , SRSL24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24 ] )
        , ( ( LLRL24 , ELLS24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24 ] )
        , ( ( LLRL24 , ERRS24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24 ] )
        , ( ( LLRL24 , ESES24 )
          , Set.fromList
                [ LLLR24 ] )
        , ( ( LLRL24 , LERE24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, RLLL24, RLLR24 ] )
        , ( ( LLRL24 , LLLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRRL24, LRRR24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( LLRL24 , LLLR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24
                , RRRL24, RRRR24 ] )
        , ( ( LLRL24 , LLRL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, RELE24, RLLL24, RLLR24
                , RRLL24, RRLR24 ] )
        , ( ( LLRL24 , LLRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRRL24, LRRR24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( LLRL24 , LRLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24
                , RLLL24, RLLR24 ] )
        , ( ( LLRL24 , LRRL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24
                , RLLL24, RLLR24 ] )
        , ( ( LLRL24 , LRRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LLLL24, LLLR24, LLRL24, LRLL24
                , LRRL24, LRRR24, LSEL24, RLLL24, RLLR24, RLRR24, RRLR24
                , RRRL24, RRRR24, RSER24 ] )
        , ( ( LLRL24 , LSEL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, RLLL24, RLLR24 ] )
        , ( ( LLRL24 , RELE24 )
          , Set.fromList
                [ LLLR24, LLRL24, LLRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( LLRL24 , RLLL24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRL24
                , SESE24, SLSR24, SRSL24 ] )
        , ( ( LLRL24 , RLLR24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24, RLLR24
                , RLRR24, SLSR24 ] )
        , ( ( LLRL24 , RLRR24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24, RLLR24
                , RLRR24, SLSR24 ] )
        , ( ( LLRL24 , RRLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LSEL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( LLRL24 , RRLR24 )
          , Set.fromList
                [ LLLR24, LLRL24, LLRR24, RLLR24, RLRR24, RRLR24, RRRR24
                , RSER24, SLSR24 ] )
        , ( ( LLRL24 , RRRL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24
                , RRRL24, SRSL24 ] )
        , ( ( LLRL24 , RRRR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LSEL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( LLRL24 , RSER24 )
          , Set.fromList
                [ LLLR24, LLRL24, LLRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( LLRL24 , SESE24 )
          , Set.fromList
                [ LLRL24 ] )
        , ( ( LLRL24 , SLSR24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24 ] )
        , ( ( LLRL24 , SRSL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24 ] )
        , ( ( LLRR24 , ELLS24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRLL24, LRRR24 ] )
        , ( ( LLRR24 , ERRS24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24 ] )
        , ( ( LLRR24 , ESES24 )
          , Set.fromList
                [ LLLL24 ] )
        , ( ( LLRR24 , LERE24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLLL24, RLRR24 ] )
        , ( ( LLRR24 , LLLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRR24, RLLL24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( LLRR24 , LLLR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRLL24, LRRR24, RRLL24
                , RRRR24 ] )
        , ( ( LLRR24 , LLRL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLLL24, RLRR24, RRLL24
                , RRRR24 ] )
        , ( ( LLRR24 , LLRR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRR24, RLLL24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( LLRR24 , LRLL24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( LLRR24 , LRRL24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RLLL24, RLRR24 ] )
        , ( ( LLRR24 , LRRR24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( LLRR24 , LSEL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLLL24, RLRR24 ] )
        , ( ( LLRR24 , RELE24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RLLL24, RLLR24, RLRR24
                , SLSR24 ] )
        , ( ( LLRR24 , RLLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRL24
                , LRRR24, RLLL24, RLLR24, RLRR24, RRLL24, RRRL24, RRRR24
                , SLSR24, SRSL24 ] )
        , ( ( LLRR24 , RLLR24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRR24, RLLL24
                , RLLR24, RLRR24, SLSR24 ] )
        , ( ( LLRR24 , RLRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRL24
                , LRRR24, RLLL24, RLLR24, RLRR24, RRLL24, RRRL24, RRRR24
                , SLSR24, SRSL24 ] )
        , ( ( LLRR24 , RRLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24, SESE24
                , SLSR24, SRSL24 ] )
        , ( ( LLRR24 , RRLR24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRR24, RSER24, SLSR24 ] )
        , ( ( LLRR24 , RRRL24 )
          , Set.fromList
                [ ERRS24, LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24
                , LRRR24, LSEL24, RRLL24, RRRL24, RRRR24, SRSL24 ] )
        , ( ( LLRR24 , RRRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LERE24, LLLL24, LLLR24, LLRL24
                , LLRR24, LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( LLRR24 , RSER24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RLLL24, RLLR24, RLRR24
                , SLSR24 ] )
        , ( ( LLRR24 , SESE24 )
          , Set.fromList
                [ LLRR24 ] )
        , ( ( LLRR24 , SLSR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRLL24, LRRR24 ] )
        , ( ( LLRR24 , SRSL24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24 ] )
        , ( ( LRLL24 , ELLS24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRR24
                , RSER24 ] )
        , ( ( LRLL24 , ERRS24 )
          , Set.fromList
                [ RLRR24, RRLL24, RRRL24, RRRR24 ] )
        , ( ( LRLL24 , ESES24 )
          , Set.fromList
                [ RLRR24 ] )
        , ( ( LRLL24 , LERE24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RRLL24, RRRL24, RRRR24
                , SRSL24 ] )
        , ( ( LRLL24 , LLLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRR24, RSER24, SLSR24 ] )
        , ( ( LRLL24 , LLLR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24, SESE24
                , SLSR24, SRSL24 ] )
        , ( ( LRLL24 , LLRL24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LERE24, LLLL24, LLLR24, LLRL24
                , LLRR24, LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( LRLL24 , LLRR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24
                , LRRR24, LSEL24, RRLL24, RRRL24, RRRR24, SRSL24 ] )
        , ( ( LRLL24 , LRLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, LRLL24, RLLL24, RLLR24
                , RLRR24, SLSR24 ] )
        , ( ( LRLL24 , LRRL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRL24
                , LRRR24, RLLL24, RLLR24, RLRR24, RRLL24, RRRL24, RRRR24
                , SLSR24, SRSL24 ] )
        , ( ( LRLL24 , LRRR24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RLRR24, RRLL24, RRRL24
                , RRRR24, SRSL24 ] )
        , ( ( LRLL24 , LSEL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RLLL24, RLLR24, RLRR24
                , SLSR24 ] )
        , ( ( LRLL24 , RELE24 )
          , Set.fromList
                [ LRLL24, RRLL24, RRLR24, RRRR24 ] )
        , ( ( LRLL24 , RLLL24 )
          , Set.fromList
                [ LRLL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( LRLL24 , RLLR24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( LRLL24 , RLRR24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RLRR24 ] )
        , ( ( LRLL24 , RRLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRLL24, RRLL24, RRLR24
                , RRRR24 ] )
        , ( ( LRLL24 , RRLR24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, LRLL24, RLRR24, RRLL24, RRLR24
                , RRRR24 ] )
        , ( ( LRLL24 , RRRL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRLL24, RLRR24, RRLL24, RRRL24
                , RRRR24 ] )
        , ( ( LRLL24 , RRRR24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLRR24, RRLL24, RRRL24
                , RRRR24 ] )
        , ( ( LRLL24 , RSER24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLRR24 ] )
        , ( ( LRLL24 , SESE24 )
          , Set.fromList
                [ LRLL24 ] )
        , ( ( LRLL24 , SLSR24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24 ] )
        , ( ( LRLL24 , SRSL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRLL24 ] )
        , ( ( LRRL24 , ELLS24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RRLL24, RRLR24, RRRL24 ] )
        , ( ( LRRL24 , ERRS24 )
          , Set.fromList
                [ RLLR24, RLRR24, RRLR24, RRRL24, RRRR24, RSER24 ] )
        , ( ( LRRL24 , ESES24 )
          , Set.fromList
                [ RLLR24 ] )
        , ( ( LRRL24 , LERE24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( LRRL24 , LLLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, RELE24, RLLL24, RLLR24
                , RRLL24, RRLR24, RRRL24 ] )
        , ( ( LRRL24 , LLLR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRRL24, LRRR24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( LRRL24 , LLRL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRRL24, LRRR24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( LRRL24 , LLRR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24
                , RRLR24, RRRL24, RRRR24 ] )
        , ( ( LRRL24 , LRLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24
                , RLLL24, RLLR24 ] )
        , ( ( LRRL24 , LRRL24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LLLL24, LLLR24, LLRL24, LRLL24
                , LRRL24, LRRR24, LSEL24, RLLL24, RLLR24, RLRR24, RRLR24
                , RRRL24, RRRR24, RSER24 ] )
        , ( ( LRRL24 , LRRR24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RLLR24, RLRR24, RRLR24, RRRL24
                , RRRR24, RSER24 ] )
        , ( ( LRRL24 , LSEL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, RLLL24, RLLR24 ] )
        , ( ( LRRL24 , RELE24 )
          , Set.fromList
                [ LRLL24, LRRL24, RRLL24, RRLR24, RRRL24, SRSL24 ] )
        , ( ( LRRL24 , RLLL24 )
          , Set.fromList
                [ LRLL24, LRRL24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, SRSL24 ] )
        , ( ( LRRL24 , RLLR24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRL24
                , SESE24, SLSR24, SRSL24 ] )
        , ( ( LRRL24 , RLRR24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24, RLLR24
                , RLRR24, SLSR24 ] )
        , ( ( LRRL24 , RRLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24
                , RRLR24, RRRL24, SRSL24 ] )
        , ( ( LRRL24 , RRLR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LSEL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( LRRL24 , RRRL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LSEL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( LRRL24 , RRRR24 )
          , Set.fromList
                [ LLLR24, LLRL24, LLRR24, RLLR24, RLRR24, RRLR24, RRRL24
                , RRRR24, RSER24, SLSR24 ] )
        , ( ( LRRL24 , RSER24 )
          , Set.fromList
                [ LLLR24, LLRL24, LLRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( LRRL24 , SESE24 )
          , Set.fromList
                [ LRRL24 ] )
        , ( ( LRRL24 , SLSR24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24 ] )
        , ( ( LRRL24 , SRSL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24 ] )
        , ( ( LRRR24 , ELLS24 )
          , Set.fromList
                [ RLLL24, RRLL24, RRRL24, RRRR24 ] )
        , ( ( LRRR24 , ERRS24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRR24
                , RSER24 ] )
        , ( ( LRRR24 , ESES24 )
          , Set.fromList
                [ RLLL24 ] )
        , ( ( LRRR24 , LERE24 )
          , Set.fromList
                [ LRRR24, RRLL24, RRLR24, RRRR24 ] )
        , ( ( LRRR24 , LLLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLLL24, RRLL24, RRRL24
                , RRRR24 ] )
        , ( ( LRRR24 , LLLR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRRR24, RLLL24, RRLL24, RRRL24
                , RRRR24 ] )
        , ( ( LRRR24 , LLRL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, LRRR24, RLLL24, RRLL24, RRLR24
                , RRRR24 ] )
        , ( ( LRRR24 , LLRR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRRR24, RRLL24, RRLR24
                , RRRR24 ] )
        , ( ( LRRR24 , LRLL24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RLLL24 ] )
        , ( ( LRRR24 , LRRL24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( LRRR24 , LRRR24 )
          , Set.fromList
                [ LRRR24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( LRRR24 , LSEL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLLL24 ] )
        , ( ( LRRR24 , RELE24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RRLL24, RRRL24, RRRR24
                , SRSL24 ] )
        , ( ( LRRR24 , RLLL24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RLLL24, RRLL24, RRRL24
                , RRRR24, SRSL24 ] )
        , ( ( LRRR24 , RLLR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRL24
                , LRRR24, RLLL24, RLLR24, RLRR24, RRLL24, RRRL24, RRRR24
                , SLSR24, SRSL24 ] )
        , ( ( LRRR24 , RLRR24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, LRRR24, RLLL24, RLLR24
                , RLRR24, SLSR24 ] )
        , ( ( LRRR24 , RRLL24 )
          , Set.fromList
                [ ERRS24, LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24
                , LRRR24, LSEL24, RRLL24, RRRL24, RRRR24, SRSL24 ] )
        , ( ( LRRR24 , RRLR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24, SESE24
                , SLSR24, SRSL24 ] )
        , ( ( LRRR24 , RRRL24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LERE24, LLLL24, LLLR24, LLRL24
                , LLRR24, LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( LRRR24 , RRRR24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRR24, RSER24, SLSR24 ] )
        , ( ( LRRR24 , RSER24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RLLL24, RLLR24, RLRR24
                , SLSR24 ] )
        , ( ( LRRR24 , SESE24 )
          , Set.fromList
                [ LRRR24 ] )
        , ( ( LRRR24 , SLSR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRRR24 ] )
        , ( ( LRRR24 , SRSL24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24 ] )
        , ( ( LSEL24 , ELLS24 )
          , Set.fromList
                [ SESE24, SLSR24, SRSL24 ] )
        , ( ( LSEL24 , ERRS24 )
          , Set.fromList
                [ SLSR24, SRSL24 ] )
        , ( ( LSEL24 , ESES24 )
          , Set.fromList
                [ SLSR24 ] )
        , ( ( LSEL24 , LERE24 )
          , Set.fromList
                [ ESES24, LSEL24, RSER24 ] )
        , ( ( LSEL24 , LLLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, RELE24, RLLL24, RLLR24, RRLL24
                , RRLR24 ] )
        , ( ( LSEL24 , LLLR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RRRL24
                , RRRR24 ] )
        , ( ( LSEL24 , LLRL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, RELE24, RLLL24, RLLR24, RRLL24
                , RRLR24 ] )
        , ( ( LSEL24 , LLRR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RRRL24
                , RRRR24 ] )
        , ( ( LSEL24 , LRLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LRLL24, RLLL24, RLLR24 ] )
        , ( ( LSEL24 , LRRL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LRLL24, RLLL24, RLLR24 ] )
        , ( ( LSEL24 , LRRR24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RLRR24, RRRL24, RRRR24 ] )
        , ( ( LSEL24 , LSEL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, RLLL24, RLLR24 ] )
        , ( ( LSEL24 , RELE24 )
          , Set.fromList
                [ LSEL24, RSER24 ] )
        , ( ( LSEL24 , RLLL24 )
          , Set.fromList
                [ LRLL24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24 ] )
        , ( ( LSEL24 , RLLR24 )
          , Set.fromList
                [ LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RLRR24 ] )
        , ( ( LSEL24 , RLRR24 )
          , Set.fromList
                [ LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RLRR24 ] )
        , ( ( LSEL24 , RRLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LRLL24, RRLL24, RRLR24 ] )
        , ( ( LSEL24 , RRLR24 )
          , Set.fromList
                [ LLRL24, LLRR24, RLRR24, RRRR24 ] )
        , ( ( LSEL24 , RRRL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LRLL24, RRLL24 ] )
        , ( ( LSEL24 , RRRR24 )
          , Set.fromList
                [ LLRL24, LLRR24, RLRR24, RRRL24, RRRR24 ] )
        , ( ( LSEL24 , RSER24 )
          , Set.fromList
                [ LLRL24, LLRR24, RLRR24 ] )
        , ( ( LSEL24 , SESE24 )
          , Set.fromList
                [ LSEL24 ] )
        , ( ( LSEL24 , SLSR24 )
          , Set.fromList
                [ LERE24, LLRL24, LLRR24, LRRL24, LRRR24 ] )
        , ( ( LSEL24 , SRSL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LRLL24 ] )
        , ( ( RELE24 , ELLS24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24 ] )
        , ( ( RELE24 , ERRS24 )
          , Set.fromList
                [ ELLS24, ERRS24 ] )
        , ( ( RELE24 , ESES24 )
          , Set.fromList
                [ ERRS24 ] )
        , ( ( RELE24 , LERE24 )
          , Set.fromList
                [ LERE24, RELE24, SESE24 ] )
        , ( ( RELE24 , LLLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24, RRRL24
                , SRSL24 ] )
        , ( ( RELE24 , LLLR24 )
          , Set.fromList
                [ LLLR24, LLRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24
                , SLSR24 ] )
        , ( ( RELE24 , LLRL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24, RRRL24
                , SRSL24 ] )
        , ( ( RELE24 , LLRR24 )
          , Set.fromList
                [ LLLR24, LLRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24
                , SLSR24 ] )
        , ( ( RELE24 , LRLL24 )
          , Set.fromList
                [ LRLL24, LRRL24, RLLL24, RRLL24, RRRL24, SRSL24 ] )
        , ( ( RELE24 , LRRL24 )
          , Set.fromList
                [ LRLL24, LRRL24, RLLL24, RRLL24, RRRL24, SRSL24 ] )
        , ( ( RELE24 , LRRR24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( RELE24 , LSEL24 )
          , Set.fromList
                [ LRLL24, LRRL24, RRLL24, RRRL24, SRSL24 ] )
        , ( ( RELE24 , RELE24 )
          , Set.fromList
                [ LERE24, RELE24 ] )
        , ( ( RELE24 , RLLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RLLL24 ] )
        , ( ( RELE24 , RLLR24 )
          , Set.fromList
                [ LRRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24 ] )
        , ( ( RELE24 , RLRR24 )
          , Set.fromList
                [ LRRR24, RLLR24, RLRR24, RRLR24, RRRR24, RSER24 ] )
        , ( ( RELE24 , RRLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, RLLL24, RRLL24, RRRL24 ] )
        , ( ( RELE24 , RRLR24 )
          , Set.fromList
                [ LLRR24, LRRR24, RRLR24, RRRR24 ] )
        , ( ( RELE24 , RRRL24 )
          , Set.fromList
                [ LLLL24, RLLL24, RRLL24, RRRL24 ] )
        , ( ( RELE24 , RRRR24 )
          , Set.fromList
                [ LLLR24, LLRR24, LRRR24, RRLR24, RRRR24 ] )
        , ( ( RELE24 , RSER24 )
          , Set.fromList
                [ LRRR24, RRLR24, RRRR24 ] )
        , ( ( RELE24 , SESE24 )
          , Set.fromList
                [ RELE24 ] )
        , ( ( RELE24 , SLSR24 )
          , Set.fromList
                [ RLLR24, RLRR24, RRLR24, RRRR24, RSER24 ] )
        , ( ( RELE24 , SRSL24 )
          , Set.fromList
                [ RLLL24, RRLL24, RRRL24 ] )
        , ( ( RLLL24 , ELLS24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24 ] )
        , ( ( RLLL24 , ERRS24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRRR24 ] )
        , ( ( RLLL24 , ESES24 )
          , Set.fromList
                [ LRRR24 ] )
        , ( ( RLLL24 , LERE24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RLLL24, RLLR24, RLRR24
                , SLSR24 ] )
        , ( ( RLLL24 , LLLL24 )
          , Set.fromList
                [ ERRS24, LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24
                , LRRR24, LSEL24, RRLL24, RRRL24, RRRR24, SRSL24 ] )
        , ( ( RLLL24 , LLLR24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LERE24, LLLL24, LLLR24, LLRL24
                , LLRR24, LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( RLLL24 , LLRL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24, SESE24
                , SLSR24, SRSL24 ] )
        , ( ( RLLL24 , LLRR24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRR24, RSER24, SLSR24 ] )
        , ( ( RLLL24 , LRLL24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RLLL24, RRLL24, RRRL24
                , RRRR24, SRSL24 ] )
        , ( ( RLLL24 , LRRL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRL24
                , LRRR24, RLLL24, RLLR24, RLRR24, RRLL24, RRRL24, RRRR24
                , SLSR24, SRSL24 ] )
        , ( ( RLLL24 , LRRR24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, LRRR24, RLLL24, RLLR24
                , RLRR24, SLSR24 ] )
        , ( ( RLLL24 , LSEL24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RRLL24, RRRL24, RRRR24
                , SRSL24 ] )
        , ( ( RLLL24 , RELE24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLLL24 ] )
        , ( ( RLLL24 , RLLL24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RLLL24 ] )
        , ( ( RLLL24 , RLLR24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( RLLL24 , RLRR24 )
          , Set.fromList
                [ LRRR24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( RLLL24 , RRLL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLLL24, RRLL24, RRRL24
                , RRRR24 ] )
        , ( ( RLLL24 , RRLR24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, LRRR24, RLLL24, RRLL24, RRLR24
                , RRRR24 ] )
        , ( ( RLLL24 , RRRL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRRR24, RLLL24, RRLL24, RRRL24
                , RRRR24 ] )
        , ( ( RLLL24 , RRRR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRRR24, RRLL24, RRLR24
                , RRRR24 ] )
        , ( ( RLLL24 , RSER24 )
          , Set.fromList
                [ LRRR24, RRLL24, RRLR24, RRRR24 ] )
        , ( ( RLLL24 , SESE24 )
          , Set.fromList
                [ RLLL24 ] )
        , ( ( RLLL24 , SLSR24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRR24
                , RSER24 ] )
        , ( ( RLLL24 , SRSL24 )
          , Set.fromList
                [ RLLL24, RRLL24, RRRL24, RRRR24 ] )
        , ( ( RLLR24 , ELLS24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24 ] )
        , ( ( RLLR24 , ERRS24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24 ] )
        , ( ( RLLR24 , ESES24 )
          , Set.fromList
                [ LRRL24 ] )
        , ( ( RLLR24 , LERE24 )
          , Set.fromList
                [ LLLR24, LLRL24, LLRR24, RLLR24, RLRR24, SLSR24 ] )
        , ( ( RLLR24 , LLLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24
                , RRLR24, RRRL24, SRSL24 ] )
        , ( ( RLLR24 , LLLR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LSEL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( RLLR24 , LLRL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LSEL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( RLLR24 , LLRR24 )
          , Set.fromList
                [ LLLR24, LLRL24, LLRR24, RLLR24, RLRR24, RRLR24, RRRL24
                , RRRR24, RSER24, SLSR24 ] )
        , ( ( RLLR24 , LRLL24 )
          , Set.fromList
                [ LRLL24, LRRL24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, SRSL24 ] )
        , ( ( RLLR24 , LRRL24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRL24
                , SESE24, SLSR24, SRSL24 ] )
        , ( ( RLLR24 , LRRR24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24, RLLR24
                , RLRR24, SLSR24 ] )
        , ( ( RLLR24 , LSEL24 )
          , Set.fromList
                [ LRLL24, LRRL24, RRLL24, RRLR24, RRRL24, SRSL24 ] )
        , ( ( RLLR24 , RELE24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, RLLL24, RLLR24 ] )
        , ( ( RLLR24 , RLLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, LRLL24, LRRL24, LSEL24
                , RLLL24, RLLR24 ] )
        , ( ( RLLR24 , RLLR24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LLLL24, LLLR24, LLRL24, LRLL24
                , LRRL24, LRRR24, LSEL24, RLLL24, RLLR24, RLRR24, RRLR24
                , RRRL24, RRRR24, RSER24 ] )
        , ( ( RLLR24 , RLRR24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RLLR24, RLRR24, RRLR24, RRRL24
                , RRRR24, RSER24 ] )
        , ( ( RLLR24 , RRLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRL24, RELE24, RLLL24, RLLR24
                , RRLL24, RRLR24, RRRL24 ] )
        , ( ( RLLR24 , RRLR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRRL24, LRRR24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( RLLR24 , RRRL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRRL24, LRRR24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( RLLR24 , RRRR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLLR24, LLRL24, LLRR24, LRRL24, LRRR24
                , RRLR24, RRRL24, RRRR24 ] )
        , ( ( RLLR24 , RSER24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( RLLR24 , SESE24 )
          , Set.fromList
                [ RLLR24 ] )
        , ( ( RLLR24 , SLSR24 )
          , Set.fromList
                [ RLLR24, RLRR24, RRLR24, RRRL24, RRRR24, RSER24 ] )
        , ( ( RLLR24 , SRSL24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RRLL24, RRLR24, RRRL24 ] )
        , ( ( RLRR24 , ELLS24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRLL24 ] )
        , ( ( RLRR24 , ERRS24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24 ] )
        , ( ( RLRR24 , ESES24 )
          , Set.fromList
                [ LRLL24 ] )
        , ( ( RLRR24 , LERE24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLRR24 ] )
        , ( ( RLRR24 , LLLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRLL24, RRLL24, RRLR24
                , RRRR24 ] )
        , ( ( RLRR24 , LLLR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRR24, LRLL24, RLRR24, RRLL24, RRRL24
                , RRRR24 ] )
        , ( ( RLRR24 , LLRL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, LRLL24, RLRR24, RRLL24, RRLR24
                , RRRR24 ] )
        , ( ( RLRR24 , LLRR24 )
          , Set.fromList
                [ LLLL24, LLRL24, LLRR24, RLRR24, RRLL24, RRRL24
                , RRRR24 ] )
        , ( ( RLRR24 , LRLL24 )
          , Set.fromList
                [ LRLL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( RLRR24 , LRRL24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( RLRR24 , LRRR24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RLRR24 ] )
        , ( ( RLRR24 , LSEL24 )
          , Set.fromList
                [ LRLL24, RRLL24, RRLR24, RRRR24 ] )
        , ( ( RLRR24 , RELE24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RLLL24, RLLR24, RLRR24
                , SLSR24 ] )
        , ( ( RLRR24 , RLLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, LRLL24, RLLL24, RLLR24
                , RLRR24, SLSR24 ] )
        , ( ( RLRR24 , RLLR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRL24
                , LRRR24, RLLL24, RLLR24, RLRR24, RRLL24, RRRL24, RRRR24
                , SLSR24, SRSL24 ] )
        , ( ( RLRR24 , RLRR24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RLRR24, RRLL24, RRRL24
                , RRRR24, SRSL24 ] )
        , ( ( RLRR24 , RRLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRR24, RSER24, SLSR24 ] )
        , ( ( RLRR24 , RRLR24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LERE24, LLLL24, LLLR24, LLRL24
                , LLRR24, LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( RLRR24 , RRRL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24, SESE24
                , SLSR24, SRSL24 ] )
        , ( ( RLRR24 , RRRR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24
                , LRRR24, LSEL24, RRLL24, RRRL24, RRRR24, SRSL24 ] )
        , ( ( RLRR24 , RSER24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RRLL24, RRRL24, RRRR24
                , SRSL24 ] )
        , ( ( RLRR24 , SESE24 )
          , Set.fromList
                [ RLRR24 ] )
        , ( ( RLRR24 , SLSR24 )
          , Set.fromList
                [ RLRR24, RRLL24, RRRL24, RRRR24 ] )
        , ( ( RLRR24 , SRSL24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRR24
                , RSER24 ] )
        , ( ( RRLL24 , ELLS24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRR24
                , RSER24 ] )
        , ( ( RRLL24 , ERRS24 )
          , Set.fromList
                [ RLLL24, RLRR24, RRLL24, RRRL24, RRRR24 ] )
        , ( ( RRLL24 , ESES24 )
          , Set.fromList
                [ RRRR24 ] )
        , ( ( RRLL24 , LERE24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RRLL24, RRRL24, RRRR24
                , SRSL24 ] )
        , ( ( RRLL24 , LLLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LERE24, LLLL24, LLLR24, LLRL24
                , LLRR24, LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( RRLL24 , LLLR24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRR24, RSER24, SLSR24 ] )
        , ( ( RRLL24 , LLRL24 )
          , Set.fromList
                [ ERRS24, LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24
                , LRRR24, LSEL24, RRLL24, RRRL24, RRRR24, SRSL24 ] )
        , ( ( RRLL24 , LLRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24, SESE24
                , SLSR24, SRSL24 ] )
        , ( ( RRLL24 , LRLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRL24
                , LRRR24, RLLL24, RLLR24, RLRR24, RRLL24, RRRL24, RRRR24
                , SLSR24, SRSL24 ] )
        , ( ( RRLL24 , LRRL24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RLLL24, RLRR24, RRLL24
                , RRRL24, RRRR24, SRSL24 ] )
        , ( ( RRLL24 , LRRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRL24
                , LRRR24, RLLL24, RLLR24, RLRR24, RRLL24, RRRL24, RRRR24
                , SLSR24, SRSL24 ] )
        , ( ( RRLL24 , LSEL24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RRLL24, RRRL24, RRRR24
                , SRSL24 ] )
        , ( ( RRLL24 , RELE24 )
          , Set.fromList
                [ LRLL24, LRRR24, RRLL24, RRLR24, RRRR24 ] )
        , ( ( RRLL24 , RLLL24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( RRLL24 , RLLR24 )
          , Set.fromList
                [ LRLL24, LRRR24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24
                , RRLR24, RRRR24, RSER24 ] )
        , ( ( RRLL24 , RLRR24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( RRLL24 , RRLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRR24, RLLL24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( RRLL24 , RRLR24 )
          , Set.fromList
                [ LLLL24, LLRR24, LRLL24, LRRR24, RRLL24, RRLR24
                , RRRR24 ] )
        , ( ( RRLL24 , RRRL24 )
          , Set.fromList
                [ LLLL24, LLRR24, RLLL24, RLRR24, RRLL24, RRRL24
                , RRRR24 ] )
        , ( ( RRLL24 , RRRR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRR24, RLLL24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( RRLL24 , RSER24 )
          , Set.fromList
                [ LRLL24, LRRR24, RRLL24, RRLR24, RRRR24 ] )
        , ( ( RRLL24 , SESE24 )
          , Set.fromList
                [ RRLL24 ] )
        , ( ( RRLL24 , SLSR24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRR24
                , RSER24 ] )
        , ( ( RRLL24 , SRSL24 )
          , Set.fromList
                [ RLLL24, RLRR24, RRLL24, RRRL24, RRRR24 ] )
        , ( ( RRLR24 , ELLS24 )
          , Set.fromList
                [ RLLR24, RLRR24, RRLR24, RRRL24, RRRR24, RSER24 ] )
        , ( ( RRLR24 , ERRS24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RRLL24, RRLR24, RRRL24 ] )
        , ( ( RRLR24 , ESES24 )
          , Set.fromList
                [ RRRL24 ] )
        , ( ( RRLR24 , LERE24 )
          , Set.fromList
                [ LRLL24, LRRL24, RRLL24, RRLR24, RRRL24, SRSL24 ] )
        , ( ( RRLR24 , LLLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LSEL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( RRLR24 , LLLR24 )
          , Set.fromList
                [ LLLR24, LLRR24, RLLR24, RLRR24, RRLR24, RRRL24, RRRR24
                , RSER24, SLSR24 ] )
        , ( ( RRLR24 , LLRL24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24, RRLR24
                , RRRL24, SRSL24 ] )
        , ( ( RRLR24 , LLRR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LSEL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( RRLR24 , LRLL24 )
          , Set.fromList
                [ LRLL24, LRRL24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, SRSL24 ] )
        , ( ( RRLR24 , LRRL24 )
          , Set.fromList
                [ LRLL24, LRRL24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, SRSL24 ] )
        , ( ( RRLR24 , LRRR24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRL24
                , SESE24, SLSR24, SRSL24 ] )
        , ( ( RRLR24 , LSEL24 )
          , Set.fromList
                [ LRLL24, LRRL24, RRLL24, RRLR24, RRRL24, SRSL24 ] )
        , ( ( RRLR24 , RELE24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( RRLR24 , RLLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LLLL24, LLLR24, LLRL24, LRLL24
                , LRRL24, LRRR24, LSEL24, RLLL24, RLLR24, RLRR24, RRLR24
                , RRRL24, RRRR24, RSER24 ] )
        , ( ( RRLR24 , RLLR24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RLLR24, RLRR24, RRLR24, RRRL24
                , RRRR24, RSER24 ] )
        , ( ( RRLR24 , RLRR24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RLLR24, RLRR24, RRLR24, RRRL24
                , RRRR24, RSER24 ] )
        , ( ( RRLR24 , RRLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRRL24, LRRR24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( RRLR24 , RRLR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( RRLR24 , RRRL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, RELE24, RLLL24, RLLR24, RRLL24
                , RRLR24, RRRL24 ] )
        , ( ( RRLR24 , RRRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRRL24, LRRR24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( RRLR24 , RSER24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( RRLR24 , SESE24 )
          , Set.fromList
                [ RRLR24 ] )
        , ( ( RRLR24 , SLSR24 )
          , Set.fromList
                [ RLLR24, RLRR24, RRLR24, RRRL24, RRRR24, RSER24 ] )
        , ( ( RRLR24 , SRSL24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RRLL24, RRLR24, RRRL24 ] )
        , ( ( RRRL24 , ELLS24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RRLL24, RRLR24, RRRL24 ] )
        , ( ( RRRL24 , ERRS24 )
          , Set.fromList
                [ RLLR24, RLRR24, RRLR24, RRRL24, RRRR24, RSER24 ] )
        , ( ( RRRL24 , ESES24 )
          , Set.fromList
                [ RRLR24 ] )
        , ( ( RRRL24 , LERE24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( RRRL24 , LLLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRRL24, LRRR24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( RRRL24 , LLLR24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, RELE24, RLLL24, RLLR24, RRLL24
                , RRLR24, RRRL24 ] )
        , ( ( RRRL24 , LLRL24 )
          , Set.fromList
                [ ERRS24, LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( RRRL24 , LLRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRRL24, LRRR24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, RRRR24 ] )
        , ( ( RRRL24 , LRLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LLLL24, LLLR24, LLRL24, LRLL24
                , LRRL24, LRRR24, LSEL24, RLLL24, RLLR24, RLRR24, RRLR24
                , RRRL24, RRRR24, RSER24 ] )
        , ( ( RRRL24 , LRRL24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RLLR24, RLRR24, RRLR24, RRRL24
                , RRRR24, RSER24 ] )
        , ( ( RRRL24 , LRRR24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RLLR24, RLRR24, RRLR24, RRRL24
                , RRRR24, RSER24 ] )
        , ( ( RRRL24 , LSEL24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( RRRL24 , RELE24 )
          , Set.fromList
                [ LRLL24, LRRL24, RRLL24, RRLR24, RRRL24, SRSL24 ] )
        , ( ( RRRL24 , RLLL24 )
          , Set.fromList
                [ LRLL24, LRRL24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, SRSL24 ] )
        , ( ( RRRL24 , RLLR24 )
          , Set.fromList
                [ LRLL24, LRRL24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24
                , RRRL24, SRSL24 ] )
        , ( ( RRRL24 , RLRR24 )
          , Set.fromList
                [ LERE24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRL24
                , SESE24, SLSR24, SRSL24 ] )
        , ( ( RRRL24 , RRLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LSEL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( RRRL24 , RRLR24 )
          , Set.fromList
                [ LLLL24, LLRL24, LRLL24, LRRL24, LSEL24, RRLL24, RRLR24
                , RRRL24, SRSL24 ] )
        , ( ( RRRL24 , RRRL24 )
          , Set.fromList
                [ LLLR24, LLRR24, RLLR24, RLRR24, RRLR24, RRRL24, RRRR24
                , RSER24, SLSR24 ] )
        , ( ( RRRL24 , RRRR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRL24, LSEL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( RRRL24 , RSER24 )
          , Set.fromList
                [ LRLL24, LRRL24, RRLL24, RRLR24, RRRL24, SRSL24 ] )
        , ( ( RRRL24 , SESE24 )
          , Set.fromList
                [ RRRL24 ] )
        , ( ( RRRL24 , SLSR24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RRLL24, RRLR24, RRRL24 ] )
        , ( ( RRRL24 , SRSL24 )
          , Set.fromList
                [ RLLR24, RLRR24, RRLR24, RRRL24, RRRR24, RSER24 ] )
        , ( ( RRRR24 , ELLS24 )
          , Set.fromList
                [ RLLL24, RLRR24, RRLL24, RRRL24, RRRR24 ] )
        , ( ( RRRR24 , ERRS24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRR24
                , RSER24 ] )
        , ( ( RRRR24 , ESES24 )
          , Set.fromList
                [ RRLL24 ] )
        , ( ( RRRR24 , LERE24 )
          , Set.fromList
                [ LRLL24, LRRR24, RRLL24, RRLR24, RRRR24 ] )
        , ( ( RRRR24 , LLLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRR24, RLLL24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( RRRR24 , LLLR24 )
          , Set.fromList
                [ LLLL24, LLRR24, RLLL24, RLRR24, RRLL24, RRRL24
                , RRRR24 ] )
        , ( ( RRRR24 , LLRL24 )
          , Set.fromList
                [ LLLL24, LLRR24, LRLL24, LRRR24, RRLL24, RRLR24
                , RRRR24 ] )
        , ( ( RRRR24 , LLRR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LLRL24, LLRR24, LRLL24, LRRR24, RLLL24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24 ] )
        , ( ( RRRR24 , LRLL24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( RRRR24 , LRRL24 )
          , Set.fromList
                [ LRLL24, LRRR24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24
                , RRLR24, RRRR24, RSER24 ] )
        , ( ( RRRR24 , LRRR24 )
          , Set.fromList
                [ LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24, LRRR24
                , LSEL24, RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24
                , RRRR24, RSER24 ] )
        , ( ( RRRR24 , LSEL24 )
          , Set.fromList
                [ LRLL24, LRRR24, RRLL24, RRLR24, RRRR24 ] )
        , ( ( RRRR24 , RELE24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RRLL24, RRRL24, RRRR24
                , SRSL24 ] )
        , ( ( RRRR24 , RLLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRL24
                , LRRR24, RLLL24, RLLR24, RLRR24, RRLL24, RRRL24, RRRR24
                , SLSR24, SRSL24 ] )
        , ( ( RRRR24 , RLLR24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RLLL24, RLRR24, RRLL24
                , RRRL24, RRRR24, SRSL24 ] )
        , ( ( RRRR24 , RLRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LLLL24, LLLR24, LLRR24, LRLL24, LRRL24
                , LRRR24, RLLL24, RLLR24, RLRR24, RRLL24, RRRL24, RRRR24
                , SLSR24, SRSL24 ] )
        , ( ( RRRR24 , RRLL24 )
          , Set.fromList
                [ ELLS24, ERRS24, ESES24, LERE24, LLLL24, LLLR24, LLRL24
                , LLRR24, LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24
                , RLLR24, RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24
                , SLSR24, SRSL24 ] )
        , ( ( RRRR24 , RRLR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLLL24, LLRL24, LLRR24, LRLL24, LRRL24
                , LRRR24, LSEL24, RRLL24, RRRL24, RRRR24, SRSL24 ] )
        , ( ( RRRR24 , RRRL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LLRR24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRR24, RSER24, SLSR24 ] )
        , ( ( RRRR24 , RRRR24 )
          , Set.fromList
                [ ELLS24, ERRS24, LERE24, LLLL24, LLLR24, LLRL24, LLRR24
                , LRLL24, LRRL24, LRRR24, LSEL24, RELE24, RLLL24, RLLR24
                , RLRR24, RRLL24, RRLR24, RRRL24, RRRR24, RSER24, SESE24
                , SLSR24, SRSL24 ] )
        , ( ( RRRR24 , RSER24 )
          , Set.fromList
                [ ERRS24, LRLL24, LRRL24, LRRR24, RRLL24, RRRL24, RRRR24
                , SRSL24 ] )
        , ( ( RRRR24 , SESE24 )
          , Set.fromList
                [ RRRR24 ] )
        , ( ( RRRR24 , SLSR24 )
          , Set.fromList
                [ RLLL24, RLRR24, RRLL24, RRRL24, RRRR24 ] )
        , ( ( RRRR24 , SRSL24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RLRR24, RRLL24, RRLR24, RRRR24
                , RSER24 ] )
        , ( ( RSER24 , ELLS24 )
          , Set.fromList
                [ SLSR24, SRSL24 ] )
        , ( ( RSER24 , ERRS24 )
          , Set.fromList
                [ SESE24, SLSR24, SRSL24 ] )
        , ( ( RSER24 , ESES24 )
          , Set.fromList
                [ SRSL24 ] )
        , ( ( RSER24 , LERE24 )
          , Set.fromList
                [ LSEL24, RSER24 ] )
        , ( ( RSER24 , LLLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LRLL24, RRLL24, RRLR24 ] )
        , ( ( RSER24 , LLLR24 )
          , Set.fromList
                [ LLRR24, RLRR24, RRRL24, RRRR24 ] )
        , ( ( RSER24 , LLRL24 )
          , Set.fromList
                [ LLLL24, LRLL24, RRLL24, RRLR24 ] )
        , ( ( RSER24 , LLRR24 )
          , Set.fromList
                [ LLRL24, LLRR24, RLRR24, RRRL24, RRRR24 ] )
        , ( ( RSER24 , LRLL24 )
          , Set.fromList
                [ LRLL24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24 ] )
        , ( ( RSER24 , LRRL24 )
          , Set.fromList
                [ LRLL24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24 ] )
        , ( ( RSER24 , LRRR24 )
          , Set.fromList
                [ LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RLRR24 ] )
        , ( ( RSER24 , LSEL24 )
          , Set.fromList
                [ LRLL24, RRLL24, RRLR24 ] )
        , ( ( RSER24 , RELE24 )
          , Set.fromList
                [ ESES24, LSEL24, RSER24 ] )
        , ( ( RSER24 , RLLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LRLL24, RLLL24, RLLR24 ] )
        , ( ( RSER24 , RLLR24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RLRR24, RRRL24, RRRR24 ] )
        , ( ( RSER24 , RLRR24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RLRR24, RRRL24, RRRR24 ] )
        , ( ( RSER24 , RRLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, RELE24, RLLL24, RLLR24, RRLL24
                , RRLR24 ] )
        , ( ( RSER24 , RRLR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RRRL24
                , RRRR24 ] )
        , ( ( RSER24 , RRRL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, RELE24, RLLL24, RLLR24, RRLL24
                , RRLR24 ] )
        , ( ( RSER24 , RRRR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RRRL24
                , RRRR24 ] )
        , ( ( RSER24 , RSER24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RRRL24, RRRR24 ] )
        , ( ( RSER24 , SESE24 )
          , Set.fromList
                [ RSER24 ] )
        , ( ( RSER24 , SLSR24 )
          , Set.fromList
                [ RLRR24, RRRL24, RRRR24 ] )
        , ( ( RSER24 , SRSL24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RRLL24, RRLR24 ] )
        , ( ( SESE24 , ELLS24 )
          , Set.fromList
                [ ELLS24 ] )
        , ( ( SESE24 , ERRS24 )
          , Set.fromList
                [ ERRS24 ] )
        , ( ( SESE24 , ESES24 )
          , Set.fromList
                [ ESES24 ] )
        , ( ( SESE24 , LERE24 )
          , Set.fromList
                [ LERE24 ] )
        , ( ( SESE24 , LLLL24 )
          , Set.fromList
                [ LLLL24 ] )
        , ( ( SESE24 , LLLR24 )
          , Set.fromList
                [ LLLR24 ] )
        , ( ( SESE24 , LLRL24 )
          , Set.fromList
                [ LLRL24 ] )
        , ( ( SESE24 , LLRR24 )
          , Set.fromList
                [ LLRR24 ] )
        , ( ( SESE24 , LRLL24 )
          , Set.fromList
                [ LRLL24 ] )
        , ( ( SESE24 , LRRL24 )
          , Set.fromList
                [ LRRL24 ] )
        , ( ( SESE24 , LRRR24 )
          , Set.fromList
                [ LRRR24 ] )
        , ( ( SESE24 , LSEL24 )
          , Set.fromList
                [ LSEL24 ] )
        , ( ( SESE24 , RELE24 )
          , Set.fromList
                [ RELE24 ] )
        , ( ( SESE24 , RLLL24 )
          , Set.fromList
                [ RLLL24 ] )
        , ( ( SESE24 , RLLR24 )
          , Set.fromList
                [ RLLR24 ] )
        , ( ( SESE24 , RLRR24 )
          , Set.fromList
                [ RLRR24 ] )
        , ( ( SESE24 , RRLL24 )
          , Set.fromList
                [ RRLL24 ] )
        , ( ( SESE24 , RRLR24 )
          , Set.fromList
                [ RRLR24 ] )
        , ( ( SESE24 , RRRL24 )
          , Set.fromList
                [ RRRL24 ] )
        , ( ( SESE24 , RRRR24 )
          , Set.fromList
                [ RRRR24 ] )
        , ( ( SESE24 , RSER24 )
          , Set.fromList
                [ RSER24 ] )
        , ( ( SESE24 , SESE24 )
          , Set.fromList
                [ SESE24 ] )
        , ( ( SESE24 , SLSR24 )
          , Set.fromList
                [ SLSR24 ] )
        , ( ( SESE24 , SRSL24 )
          , Set.fromList
                [ SRSL24 ] )
        , ( ( SLSR24 , ELLS24 )
          , Set.fromList
                [ LLLL24, LLLR24, LRLL24 ] )
        , ( ( SLSR24 , ERRS24 )
          , Set.fromList
                [ LERE24, LLRL24, LLRR24, LRRL24, LRRR24 ] )
        , ( ( SLSR24 , ESES24 )
          , Set.fromList
                [ LSEL24 ] )
        , ( ( SLSR24 , LERE24 )
          , Set.fromList
                [ LLRL24, LLRR24, RLRR24 ] )
        , ( ( SLSR24 , LLLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LRLL24, RRLL24, RRLR24 ] )
        , ( ( SLSR24 , LLLR24 )
          , Set.fromList
                [ LLLL24, LLLR24, LRLL24, RRLL24 ] )
        , ( ( SLSR24 , LLRL24 )
          , Set.fromList
                [ LLRL24, LLRR24, RLRR24, RRRR24 ] )
        , ( ( SLSR24 , LLRR24 )
          , Set.fromList
                [ LLRL24, LLRR24, RLRR24, RRRL24, RRRR24 ] )
        , ( ( SLSR24 , LRLL24 )
          , Set.fromList
                [ LRLL24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24 ] )
        , ( ( SLSR24 , LRRL24 )
          , Set.fromList
                [ LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RLRR24 ] )
        , ( ( SLSR24 , LRRR24 )
          , Set.fromList
                [ LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RLRR24 ] )
        , ( ( SLSR24 , LSEL24 )
          , Set.fromList
                [ LSEL24, RSER24 ] )
        , ( ( SLSR24 , RELE24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, RLLL24, RLLR24 ] )
        , ( ( SLSR24 , RLLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LRLL24, RLLL24, RLLR24 ] )
        , ( ( SLSR24 , RLLR24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LRLL24, RLLL24, RLLR24 ] )
        , ( ( SLSR24 , RLRR24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RLRR24, RRRL24, RRRR24 ] )
        , ( ( SLSR24 , RRLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, RELE24, RLLL24, RLLR24, RRLL24
                , RRLR24 ] )
        , ( ( SLSR24 , RRLR24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, RELE24, RLLL24, RLLR24, RRLL24
                , RRLR24 ] )
        , ( ( SLSR24 , RRRL24 )
          , Set.fromList
                [ ERRS24, LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RRRL24
                , RRRR24 ] )
        , ( ( SLSR24 , RRRR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RRRL24
                , RRRR24 ] )
        , ( ( SLSR24 , RSER24 )
          , Set.fromList
                [ ESES24, LSEL24, RSER24 ] )
        , ( ( SLSR24 , SESE24 )
          , Set.fromList
                [ SLSR24 ] )
        , ( ( SLSR24 , SLSR24 )
          , Set.fromList
                [ SLSR24, SRSL24 ] )
        , ( ( SLSR24 , SRSL24 )
          , Set.fromList
                [ SESE24, SLSR24, SRSL24 ] )
        , ( ( SRSL24 , ELLS24 )
          , Set.fromList
                [ RELE24, RLLL24, RLLR24, RRLL24, RRLR24 ] )
        , ( ( SRSL24 , ERRS24 )
          , Set.fromList
                [ RLRR24, RRRL24, RRRR24 ] )
        , ( ( SRSL24 , ESES24 )
          , Set.fromList
                [ RSER24 ] )
        , ( ( SRSL24 , LERE24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RRRL24, RRRR24 ] )
        , ( ( SRSL24 , LLLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, RELE24, RLLL24, RLLR24, RRLL24
                , RRLR24 ] )
        , ( ( SRSL24 , LLLR24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, RELE24, RLLL24, RLLR24, RRLL24
                , RRLR24 ] )
        , ( ( SRSL24 , LLRL24 )
          , Set.fromList
                [ ERRS24, LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RRRL24
                , RRRR24 ] )
        , ( ( SRSL24 , LLRR24 )
          , Set.fromList
                [ ERRS24, LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RRRL24
                , RRRR24 ] )
        , ( ( SRSL24 , LRLL24 )
          , Set.fromList
                [ ELLS24, LLLL24, LLLR24, LRLL24, RLLL24, RLLR24 ] )
        , ( ( SRSL24 , LRRL24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RLRR24, RRRL24, RRRR24 ] )
        , ( ( SRSL24 , LRRR24 )
          , Set.fromList
                [ ERRS24, LRRL24, LRRR24, RLRR24, RRRL24, RRRR24 ] )
        , ( ( SRSL24 , LSEL24 )
          , Set.fromList
                [ ESES24, LSEL24, RSER24 ] )
        , ( ( SRSL24 , RELE24 )
          , Set.fromList
                [ LRLL24, RRLL24, RRLR24 ] )
        , ( ( SRSL24 , RLLL24 )
          , Set.fromList
                [ LRLL24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24 ] )
        , ( ( SRSL24 , RLLR24 )
          , Set.fromList
                [ LRLL24, RELE24, RLLL24, RLLR24, RRLL24, RRLR24 ] )
        , ( ( SRSL24 , RLRR24 )
          , Set.fromList
                [ LERE24, LLRL24, LLRR24, LRRL24, LRRR24, RLRR24 ] )
        , ( ( SRSL24 , RRLL24 )
          , Set.fromList
                [ LLLL24, LLLR24, LRLL24, RRLL24, RRLR24 ] )
        , ( ( SRSL24 , RRLR24 )
          , Set.fromList
                [ LLLL24, LRLL24, RRLL24, RRLR24 ] )
        , ( ( SRSL24 , RRRL24 )
          , Set.fromList
                [ LLRR24, RLRR24, RRRL24, RRRR24 ] )
        , ( ( SRSL24 , RRRR24 )
          , Set.fromList
                [ LLRL24, LLRR24, RLRR24, RRRL24, RRRR24 ] )
        , ( ( SRSL24 , RSER24 )
          , Set.fromList
                [ LSEL24, RSER24 ] )
        , ( ( SRSL24 , SESE24 )
          , Set.fromList
                [ SRSL24 ] )
        , ( ( SRSL24 , SLSR24 )
          , Set.fromList
                [ SESE24, SLSR24, SRSL24 ] )
        , ( ( SRSL24 , SRSL24 )
          , Set.fromList
                [ SLSR24, SRSL24 ] )
        ]

