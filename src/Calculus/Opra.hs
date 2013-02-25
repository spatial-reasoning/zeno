{-# LANGUAGE ScopedTypeVariables #-}
module Calculus.Opra where

-- standard modules
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Ratio
import qualified Data.Set as Set

-- local modules
import Basics
import Helpful.General

--import Debug.Trace

data Opra = Opra Int Int Int
    deriving (Read, Ord, Eq)

instance Show Opra where
--    show (Opra m (-1) b) = "Opra" ++ show m ++ "_s_" ++ show b
--    show (Opra m a    b) = "Opra" ++ show m ++ "_" ++ show a ++ "_"
--                                                   ++ show b
    show (Opra m (-1) b) = "Opra " ++ show m ++ " s " ++ show b
    show (Opra m a    b) = "Opra " ++ show m ++ " " ++ show a ++ " "
                                                   ++ show b

instance Enum Opra where

instance Bounded Opra where
    minBound = Opra 0 0 0
    maxBound = Opra 20 20 20

instance Calculus Opra where
    rank _ = 2
    cName _ = "opra"
    cShowRel (Opra m (-1) b) = "Opra_" ++ show m ++ "_s_" ++ show b
    cShowRel (Opra m a    b) = "Opra_" ++ show m ++ "_" ++ show a ++ "_"
                                                        ++ show b

opraBaserelations m =
    [ Opra m a b | let range = [0..4*m - 1], a <- range ++ [-1], b <- range]

angleModulo :: (Num a, Ord a) => a -> a -> a
angleModulo modul ang =
    if ang < 0 then
        angleModulo modul $ ang + modul
    else if ang < modul then
        ang
    else
        angleModulo modul $ ang - modul

angle :: (Integral a, Integral a1, Integral a2) => a -> a1 -> a2 -> Ratio a
angle circle g s = angleModulo (fromIntegral circle) $
    (circle * fromIntegral s) % (4 * (fromIntegral g))

minAngle :: (Integral a, Integral a1, Integral a2) => a -> a1 -> a2 -> Ratio a
minAngle hc g s = if even s then angle hc g s else angle hc g (abs s - 1)

maxAngle :: (Integral a, Integral a1, Integral a2) => a -> a1 -> a2 -> Ratio a
maxAngle hc g s = if even s then angle hc g s else angle hc g (abs s + 1)

opraConvert :: (Opram a) => Int -> GRel a -> GRel a
opraConvert n (GRel relSet) =
    GRel $ Set.map (aRel . opraConvertAtomic n . ARel) relSet

opraConvertAtomic :: (Opram a) => Int -> ARel a -> ARel a
opraConvertAtomic n (ARel rel) =
    ARel $ read $ "Opra" ++ show n ++ "_" ++ x' ++ "_" ++ y'
  where
    (x, _:y) = break (== '_') $ drop 1 $ dropWhile (/= '_') $ show rel
    (x', y') =
        if x == "s" then
            (x , show $ mod (- read y) (4 * n))
        else
            (y, x)

areal baserelations = filter
    (\ a -> ( \(b, _:c) -> all odd $ map read [b,c]) $ break (== '_') $
        map (\x -> if x == 's' then '1' else x) $ drop 6 $ show a
    ) baserelations

nonAreal baserelations = filter
    (\ a -> ( \(b, _:c) -> all even $ map read [b,c]) $ break (== '_') $
        map (\x -> if x == 's' then '0' else x) $ drop 6 $ show a
    ) baserelations

same    baserelations = filter (\a -> ((show a)!!6) == 's') baserelations
nonSame baserelations = filter (\a -> ((show a)!!6) /= 's') baserelations

-- fixme: look over this one again.
class (Read a, Show a, Ord a) => Opram a where
    m :: a -> Int
--    m :: (Int, a)

    readOpram :: String -> a
    readOpram x = case maybeRead $ "Opra" ++ m ++ "_" ++ rels of
        Just z  -> z
        Nothing -> error $ show x ++ " is not an OPRA-" ++ show m
                                  ++ " relation."
      where
        (m, _:rels) = break (== '<') x -- '<' This symbol might cause problems!

    showOpram :: a -> String
    showOpram x = m ++ "<" ++ rels     -- '<' This symbol might cause problems!
      where
        (m, _:rels) = break (== '_') $ drop 4 $ show x

    sparqifyOpram :: a -> String
    sparqifyOpram = map Char.toUpper . drop 6 . show

    opraToOpram :: Opra -> a
    opraToOpram rel@(Opra a b c) =
        case maybeRead $ "Opra" ++ show n ++ "_" ++ showB ++ "_" ++ show c of
            Just z  -> z
            Nothing -> error $ show rel ++ " is not an OPRA-" ++ show n
                                        ++ " relation."
      where
        showB = case b of
                (-1) -> "s"
                x    -> show x
        n = m (undefined :: a)
            
    opramToOpra :: a -> Opra
    opramToOpra x = Opra (read m) readRel1 (read rel2)
      where
        (m, _:rels) = break (== '_') $ drop 4 $ show x
        (rel1, _:rel2) = break (== '_') rels
        readRel1 = case rel1 of
            "s" -> (-1)
            y   -> read y

    opramNetToOpraNetAtomic :: Network b (ARel a) -> Network b (ARel Opra)
    opramNetToOpraNetAtomic net@Network{nCons = cons} =
        net { nCons = Map.map (ARel . opramToOpra . aRel) cons }

    opramNetToOpraNet :: Network b (GRel a) -> Network b (GRel Opra)
    opramNetToOpraNet net@Network{nCons = cons} =
        net { nCons = Map.map (GRel . Set.map opramToOpra . gRel) cons }

