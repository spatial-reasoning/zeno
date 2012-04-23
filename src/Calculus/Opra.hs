{-# LANGUAGE ScopedTypeVariables #-}
module Calculus.Opra where

-- standard modules
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

-- local modules
import Basics
import Helpful.General

import Debug.Trace

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
    showRel (Opra m (-1) b) = "Opra_" ++ show m ++ "_s_" ++ show b
    showRel (Opra m a    b) = "Opra_" ++ show m ++ "_" ++ show a ++ "_"
                                                       ++ show b

opraBaserelations m =
    [ Opra m a b | let range = [0..4*m - 1], a <- range ++ [-1], b <- range]

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

    opramNetToOpraNetAtomic :: Network b a -> Network b Opra
    opramNetToOpraNetAtomic net@Network{nCons = cons} =
        net { nCons = Map.map opramToOpra cons }

    opramNetToOpraNet :: Network b (Set.Set a) -> Network b (Set.Set Opra)
    opramNetToOpraNet net@Network{nCons = cons} =
        net { nCons = Map.map (Set.map opramToOpra) cons }

