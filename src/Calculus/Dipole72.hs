module Calculus.Dipole72 where

-- standard modules
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set

-- local modules
import Basics
import Calculus.Helpful


-- maybe we can use this sometime?
--
-- sortAtomicDipoleConstraint :: Constraint -> Constraint
-- sortAtomicDipoleConstraint (ents, rel)
--     | rel > swappedRel = (reverse ents, swappedRel)
--     | otherwise     = (ents, rel)
--     where
--         swappedRel = Set.map swapRel rel
--
-- swapRel (a:b:c:d:s) = [c, d, a, b] ++ swappedSign
--     where
--         swappedSign = case s of
--                           "+" -> "-"
--                           "-" -> "+"
--                           _   -> s
--
-- dipoleSort :: [Constraint] -> [Constraint]
-- dipoleSort cons = map sortAtomicDipoleConstraint cons


data Dipole72 = BBBB72 | BBFF72 | BEIE72 | BFII72 | BIIF72 | BLRR72
              | BRLL72 | BSEF72 | EBIS72 | EFBS72 | EIFS72 | ELLS72
              | ERRS72 | ESES72 | FBII72 | FEFE72 | FFBB72 | FFFF72
              | FIFI72 | FLLL72 | FRRR72 | FSEI72 | IBIB72 | IEBE72
              | IFBI72 | IIBF72 | IIFB72 | ILLR72 | IRRL72 | ISEB72
              | LBLL72 | LERE72 | LFRR72 | LIRL72 | LLBR72 | LLFL72
              | LLLB72 | LLLL72 | LLLR72 | LLRF72 | LLRL72 | LLRR72
              | LRIL72 | LRLL72 | LRRI72 | LRRL72 | LRRR72 | LSEL72
              | RBRR72 | RELE72 | RFLL72 | RILR72 | RLIR72 | RLLI72
              | RLLL72 | RLLR72 | RLRR72 | RRBL72 | RRFR72 | RRLF72
              | RRLL72 | RRLR72 | RRRB72 | RRRL72 | RRRR72 | RSER72
              | SBSB72 | SESE72 | SFSI72 | SISF72 | SLSR72 | SRSL72
              deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Calculus Dipole72 where
    rank _ = 2
    readRel x = case y of
        Just z  -> z
        Nothing -> error $ show x ++ " is not a Dipole-72 relation."
      where
        y = maybeRead $ (++ "72") $ map Char.toUpper x
    showRel = (map Char.toLower) . (take 4) . show

instance BinaryCalculus Dipole72 where
    bcIdentity = SESE72

    bcConversion = Map.fromList
        [ ( BBBB72 , Set.singleton BBBB72 )
        , ( BBFF72 , Set.singleton FFBB72 )
        , ( BEIE72 , Set.singleton IEBE72 )
        , ( BFII72 , Set.singleton IIBF72 )
        , ( BIIF72 , Set.singleton IFBI72 )
        , ( BLRR72 , Set.singleton RRBL72 )
        , ( BRLL72 , Set.singleton LLBR72 )
        , ( BSEF72 , Set.singleton EFBS72 )
        , ( EBIS72 , Set.singleton ISEB72 )
        , ( EFBS72 , Set.singleton BSEF72 )
        , ( EIFS72 , Set.singleton FSEI72 )
        , ( ELLS72 , Set.singleton LSEL72 )
        , ( ERRS72 , Set.singleton RSER72 )
        , ( ESES72 , Set.singleton ESES72 )
        , ( FBII72 , Set.singleton IIFB72 )
        , ( FEFE72 , Set.singleton FEFE72 )
        , ( FFBB72 , Set.singleton BBFF72 )
        , ( FFFF72 , Set.singleton FFFF72 )
        , ( FIFI72 , Set.singleton FIFI72 )
        , ( FLLL72 , Set.singleton LLFL72 )
        , ( FRRR72 , Set.singleton RRFR72 )
        , ( FSEI72 , Set.singleton EIFS72 )
        , ( IBIB72 , Set.singleton IBIB72 )
        , ( IEBE72 , Set.singleton BEIE72 )
        , ( IFBI72 , Set.singleton BIIF72 )
        , ( IIBF72 , Set.singleton BFII72 )
        , ( IIFB72 , Set.singleton FBII72 )
        , ( ILLR72 , Set.singleton LRIL72 )
        , ( IRRL72 , Set.singleton RLIR72 )
        , ( ISEB72 , Set.singleton EBIS72 )
        , ( LBLL72 , Set.singleton LLLB72 )
        , ( LERE72 , Set.singleton RELE72 )
        , ( LFRR72 , Set.singleton RRLF72 )
        , ( LIRL72 , Set.singleton RLLI72 )
        , ( LLBR72 , Set.singleton BRLL72 )
        , ( LLFL72 , Set.singleton FLLL72 )
        , ( LLLB72 , Set.singleton LBLL72 )
        , ( LLLL72 , Set.singleton LLLL72 )
        , ( LLLR72 , Set.singleton LRLL72 )
        , ( LLRF72 , Set.singleton RFLL72 )
        , ( LLRL72 , Set.singleton RLLL72 )
        , ( LLRR72 , Set.singleton RRLL72 )
        , ( LRIL72 , Set.singleton ILLR72 )
        , ( LRLL72 , Set.singleton LLLR72 )
        , ( LRRI72 , Set.singleton RILR72 )
        , ( LRRL72 , Set.singleton RLLR72 )
        , ( LRRR72 , Set.singleton RRLR72 )
        , ( LSEL72 , Set.singleton ELLS72 )
        , ( RBRR72 , Set.singleton RRRB72 )
        , ( RELE72 , Set.singleton LERE72 )
        , ( RFLL72 , Set.singleton LLRF72 )
        , ( RILR72 , Set.singleton LRRI72 )
        , ( RLIR72 , Set.singleton IRRL72 )
        , ( RLLI72 , Set.singleton LIRL72 )
        , ( RLLL72 , Set.singleton LLRL72 )
        , ( RLLR72 , Set.singleton LRRL72 )
        , ( RLRR72 , Set.singleton RRRL72 )
        , ( RRBL72 , Set.singleton BLRR72 )
        , ( RRFR72 , Set.singleton FRRR72 )
        , ( RRLF72 , Set.singleton LFRR72 )
        , ( RRLL72 , Set.singleton LLRR72 )
        , ( RRLR72 , Set.singleton LRRR72 )
        , ( RRRB72 , Set.singleton RBRR72 )
        , ( RRRL72 , Set.singleton RLRR72 )
        , ( RRRR72 , Set.singleton RRRR72 )
        , ( RSER72 , Set.singleton ERRS72 )
        , ( SBSB72 , Set.singleton SBSB72 )
        , ( SESE72 , Set.singleton SESE72 )
        , ( SFSI72 , Set.singleton SISF72 )
        , ( SISF72 , Set.singleton SFSI72 )
        , ( SLSR72 , Set.singleton SRSL72 )
        , ( SRSL72 , Set.singleton SLSR72 ) ]

    bcComposition = Map.fromList
        [ ( ( BBBB72 , BBBB72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, SESE72, SFSI72, SISF72 ] )
        , ( ( BBBB72 , BBFF72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( BBBB72 , BEIE72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, SBSB72 ] )
        , ( ( BBBB72 , BFII72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, SBSB72 ] )
        , ( ( BBBB72 , BIIF72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, SBSB72 ] )
        , ( ( BBBB72 , BLRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, SRSL72 ] )
        , ( ( BBBB72 , BRLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, SLSR72 ] )
        , ( ( BBBB72 , BSEF72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, SBSB72 ] )
        , ( ( BBBB72 , EBIS72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72 ] )
        , ( ( BBBB72 , EFBS72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBBB72 , EIFS72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBBB72 , ELLS72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BBBB72 , ERRS72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BBBB72 , ESES72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBBB72 , FBII72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72 ] )
        , ( ( BBBB72 , FEFE72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBBB72 , FFBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBBB72 , FFFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBBB72 , FIFI72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBBB72 , FLLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BBBB72 , FRRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BBBB72 , FSEI72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBBB72 , IBIB72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72 ] )
        , ( ( BBBB72 , IEBE72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBBB72 , IFBI72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBBB72 , IIBF72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBBB72 , IIFB72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBBB72 , ILLR72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BBBB72 , IRRL72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BBBB72 , ISEB72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBBB72 , LBLL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RSER72 ] )
        , ( ( BBBB72 , LERE72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BBBB72 , LFRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BBBB72 , LIRL72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BBBB72 , LLBR72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BBBB72 , LLFL72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BBBB72 , LLLB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BBBB72 , LLLL72 )
          , Set.fromList
                [ RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( BBBB72 , LLLR72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BBBB72 , LLRF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BBBB72 , LLRL72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BBBB72 , LLRR72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( BBBB72 , LRIL72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BBBB72 , LRLL72 )
          , Set.fromList
                [ RLIR72, RLLI72, RLLL72, RLLR72, RLRR72 ] )
        , ( ( BBBB72 , LRRI72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BBBB72 , LRRL72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BBBB72 , LRRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BBBB72 , LSEL72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BBBB72 , RBRR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LSEL72 ] )
        , ( ( BBBB72 , RELE72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BBBB72 , RFLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BBBB72 , RILR72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BBBB72 , RLIR72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BBBB72 , RLLI72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BBBB72 , RLLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BBBB72 , RLLR72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BBBB72 , RLRR72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( BBBB72 , RRBL72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BBBB72 , RRFR72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BBBB72 , RRLF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BBBB72 , RRLL72 )
          , Set.fromList
                [ LLBR72, LLLB72, LLLL72, LLLR72, LLRR72 ] )
        , ( ( BBBB72 , RRLR72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BBBB72 , RRRB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BBBB72 , RRRL72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BBBB72 , RRRR72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRF72, LLRL72, LLRR72 ] )
        , ( ( BBBB72 , RSER72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BBBB72 , SBSB72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72 ] )
        , ( ( BBBB72 , SESE72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBBB72 , SFSI72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBBB72 , SISF72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBBB72 , SLSR72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BBBB72 , SRSL72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BBFF72 , BBBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBFF72 , BBFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBFF72 , BEIE72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBFF72 , BFII72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72 ] )
        , ( ( BBFF72 , BIIF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBFF72 , BLRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BBFF72 , BRLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BBFF72 , BSEF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBFF72 , EBIS72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBFF72 , EFBS72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72 ] )
        , ( ( BBFF72 , EIFS72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBFF72 , ELLS72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BBFF72 , ERRS72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BBFF72 , ESES72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBFF72 , FBII72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, SBSB72 ] )
        , ( ( BBFF72 , FEFE72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, SBSB72 ] )
        , ( ( BBFF72 , FFBB72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, SESE72, SFSI72, SISF72 ] )
        , ( ( BBFF72 , FFFF72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( BBFF72 , FIFI72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, SBSB72 ] )
        , ( ( BBFF72 , FLLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, SLSR72 ] )
        , ( ( BBFF72 , FRRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, SRSL72 ] )
        , ( ( BBFF72 , FSEI72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, SBSB72 ] )
        , ( ( BBFF72 , IBIB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBFF72 , IEBE72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBFF72 , IFBI72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72 ] )
        , ( ( BBFF72 , IIBF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBFF72 , IIFB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBFF72 , ILLR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BBFF72 , IRRL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BBFF72 , ISEB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBFF72 , LBLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BBFF72 , LERE72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BBFF72 , LFRR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LSEL72 ] )
        , ( ( BBFF72 , LIRL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BBFF72 , LLBR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BBFF72 , LLFL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BBFF72 , LLLB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BBFF72 , LLLL72 )
          , Set.fromList
                [ LLBR72, LLLB72, LLLL72, LLLR72, LLRR72 ] )
        , ( ( BBFF72 , LLLR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BBFF72 , LLRF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BBFF72 , LLRL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BBFF72 , LLRR72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRF72, LLRL72, LLRR72 ] )
        , ( ( BBFF72 , LRIL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BBFF72 , LRLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BBFF72 , LRRI72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BBFF72 , LRRL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BBFF72 , LRRR72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( BBFF72 , LSEL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BBFF72 , RBRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BBFF72 , RELE72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BBFF72 , RFLL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RSER72 ] )
        , ( ( BBFF72 , RILR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BBFF72 , RLIR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BBFF72 , RLLI72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BBFF72 , RLLL72 )
          , Set.fromList
                [ RLIR72, RLLI72, RLLL72, RLLR72, RLRR72 ] )
        , ( ( BBFF72 , RLLR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BBFF72 , RLRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BBFF72 , RRBL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BBFF72 , RRFR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BBFF72 , RRLF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BBFF72 , RRLL72 )
          , Set.fromList
                [ RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( BBFF72 , RRLR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BBFF72 , RRRB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BBFF72 , RRRL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BBFF72 , RRRR72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( BBFF72 , RSER72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BBFF72 , SBSB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BBFF72 , SESE72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBFF72 , SFSI72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72 ] )
        , ( ( BBFF72 , SISF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BBFF72 , SLSR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BBFF72 , SRSL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BEIE72 , BBBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BEIE72 , BBFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BEIE72 , BEIE72 )
          , Set.fromList
                [ BEIE72 ] )
        , ( ( BEIE72 , BFII72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( BEIE72 , BIIF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72 ] )
        , ( ( BEIE72 , BLRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BEIE72 , BRLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BEIE72 , BSEF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BEIE72 , EBIS72 )
          , Set.fromList
                [ EBIS72 ] )
        , ( ( BEIE72 , EFBS72 )
          , Set.fromList
                [ EFBS72 ] )
        , ( ( BEIE72 , EIFS72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72 ] )
        , ( ( BEIE72 , ELLS72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( BEIE72 , ERRS72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( BEIE72 , ESES72 )
          , Set.fromList
                [ EBIS72 ] )
        , ( ( BEIE72 , FBII72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( BEIE72 , FEFE72 )
          , Set.fromList
                [ FEFE72 ] )
        , ( ( BEIE72 , FFBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( BEIE72 , FFFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( BEIE72 , FIFI72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( BEIE72 , FLLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( BEIE72 , FRRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( BEIE72 , FSEI72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( BEIE72 , IBIB72 )
          , Set.fromList
                [ BBBB72, IBIB72, SBSB72 ] )
        , ( ( BEIE72 , IEBE72 )
          , Set.fromList
                [ BEIE72, IEBE72, SESE72 ] )
        , ( ( BEIE72 , IFBI72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( BEIE72 , IIBF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, SISF72 ] )
        , ( ( BEIE72 , IIFB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( BEIE72 , ILLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, SLSR72 ] )
        , ( ( BEIE72 , IRRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, SRSL72 ] )
        , ( ( BEIE72 , ISEB72 )
          , Set.fromList
                [ BBBB72, IBIB72, SBSB72 ] )
        , ( ( BEIE72 , LBLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BEIE72 , LERE72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( BEIE72 , LFRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( BEIE72 , LIRL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LSEL72 ] )
        , ( ( BEIE72 , LLBR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BEIE72 , LLFL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BEIE72 , LLLB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( BEIE72 , LLLL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BEIE72 , LLLR72 )
          , Set.fromList
                [ LLBR72, LLLR72, LLRR72 ] )
        , ( ( BEIE72 , LLRF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( BEIE72 , LLRL72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRL72 ] )
        , ( ( BEIE72 , LLRR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BEIE72 , LRIL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BEIE72 , LRLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BEIE72 , LRRI72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( BEIE72 , LRRL72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRL72 ] )
        , ( ( BEIE72 , LRRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( BEIE72 , LSEL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BEIE72 , RBRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BEIE72 , RELE72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( BEIE72 , RFLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( BEIE72 , RILR72 )
          , Set.fromList
                [ RBRR72, RILR72, RSER72 ] )
        , ( ( BEIE72 , RLIR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BEIE72 , RLLI72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( BEIE72 , RLLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( BEIE72 , RLLR72 )
          , Set.fromList
                [ RLIR72, RLLR72, RLRR72 ] )
        , ( ( BEIE72 , RLRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BEIE72 , RRBL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BEIE72 , RRFR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BEIE72 , RRLF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( BEIE72 , RRLL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BEIE72 , RRLR72 )
          , Set.fromList
                [ RRFR72, RRLR72, RRRR72 ] )
        , ( ( BEIE72 , RRRB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( BEIE72 , RRRL72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRL72 ] )
        , ( ( BEIE72 , RRRR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BEIE72 , RSER72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BEIE72 , SBSB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BEIE72 , SESE72 )
          , Set.fromList
                [ BEIE72 ] )
        , ( ( BEIE72 , SFSI72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( BEIE72 , SISF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72 ] )
        , ( ( BEIE72 , SLSR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BEIE72 , SRSL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BFII72 , BBBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BFII72 , BBFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BFII72 , BEIE72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( BFII72 , BFII72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( BFII72 , BIIF72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72 ] )
        , ( ( BFII72 , BLRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BFII72 , BRLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BFII72 , BSEF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BFII72 , EBIS72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( BFII72 , EFBS72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( BFII72 , EIFS72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72 ] )
        , ( ( BFII72 , ELLS72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( BFII72 , ERRS72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( BFII72 , ESES72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( BFII72 , FBII72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( BFII72 , FEFE72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( BFII72 , FFBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( BFII72 , FFFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( BFII72 , FIFI72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72 ] )
        , ( ( BFII72 , FLLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( BFII72 , FRRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( BFII72 , FSEI72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( BFII72 , IBIB72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, SBSB72 ] )
        , ( ( BFII72 , IEBE72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, SFSI72 ] )
        , ( ( BFII72 , IFBI72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, SFSI72 ] )
        , ( ( BFII72 , IIBF72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, SESE72, SFSI72, SISF72 ] )
        , ( ( BFII72 , IIFB72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( BFII72 , ILLR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, SLSR72 ] )
        , ( ( BFII72 , IRRL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, SRSL72 ] )
        , ( ( BFII72 , ISEB72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, SBSB72 ] )
        , ( ( BFII72 , LBLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BFII72 , LERE72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( BFII72 , LFRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( BFII72 , LIRL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LSEL72 ] )
        , ( ( BFII72 , LLBR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BFII72 , LLFL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BFII72 , LLLB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BFII72 , LLLL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BFII72 , LLLR72 )
          , Set.fromList
                [ LLBR72, LLLB72, LLLL72, LLLR72, LLRR72 ] )
        , ( ( BFII72 , LLRF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BFII72 , LLRL72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRF72, LLRL72, LLRR72 ] )
        , ( ( BFII72 , LLRR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BFII72 , LRIL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BFII72 , LRLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BFII72 , LRRI72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( BFII72 , LRRL72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( BFII72 , LRRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( BFII72 , LSEL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BFII72 , RBRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BFII72 , RELE72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( BFII72 , RFLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( BFII72 , RILR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RSER72 ] )
        , ( ( BFII72 , RLIR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BFII72 , RLLI72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( BFII72 , RLLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( BFII72 , RLLR72 )
          , Set.fromList
                [ RLIR72, RLLI72, RLLL72, RLLR72, RLRR72 ] )
        , ( ( BFII72 , RLRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BFII72 , RRBL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BFII72 , RRFR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BFII72 , RRLF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BFII72 , RRLL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BFII72 , RRLR72 )
          , Set.fromList
                [ RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( BFII72 , RRRB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BFII72 , RRRL72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( BFII72 , RRRR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BFII72 , RSER72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BFII72 , SBSB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BFII72 , SESE72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( BFII72 , SFSI72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( BFII72 , SISF72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72 ] )
        , ( ( BFII72 , SLSR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BFII72 , SRSL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BIIF72 , BBBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BIIF72 , BBFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BIIF72 , BEIE72 )
          , Set.fromList
                [ BIIF72 ] )
        , ( ( BIIF72 , BFII72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( BIIF72 , BIIF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72 ] )
        , ( ( BIIF72 , BLRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BIIF72 , BRLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BIIF72 , BSEF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BIIF72 , EBIS72 )
          , Set.fromList
                [ IBIB72 ] )
        , ( ( BIIF72 , EFBS72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( BIIF72 , EIFS72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( BIIF72 , ELLS72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( BIIF72 , ERRS72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( BIIF72 , ESES72 )
          , Set.fromList
                [ IBIB72 ] )
        , ( ( BIIF72 , FBII72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( BIIF72 , FEFE72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( BIIF72 , FFBB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72 ] )
        , ( ( BIIF72 , FFFF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72 ] )
        , ( ( BIIF72 , FIFI72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72, FBII72, FIFI72, FSEI72, IBIB72
                , IIFB72, ISEB72 ] )
        , ( ( BIIF72 , FLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72 ] )
        , ( ( BIIF72 , FRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72 ] )
        , ( ( BIIF72 , FSEI72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( BIIF72 , IBIB72 )
          , Set.fromList
                [ BBBB72, IBIB72, SBSB72 ] )
        , ( ( BIIF72 , IEBE72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( BIIF72 , IFBI72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, IEBE72, IFBI72, IIBF72, SESE72
                , SFSI72, SISF72 ] )
        , ( ( BIIF72 , IIBF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, SISF72 ] )
        , ( ( BIIF72 , IIFB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( BIIF72 , ILLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, SLSR72 ] )
        , ( ( BIIF72 , IRRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, SRSL72 ] )
        , ( ( BIIF72 , ISEB72 )
          , Set.fromList
                [ BBBB72, IBIB72, SBSB72 ] )
        , ( ( BIIF72 , LBLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BIIF72 , LERE72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( BIIF72 , LFRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72 ] )
        , ( ( BIIF72 , LIRL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LSEL72 ] )
        , ( ( BIIF72 , LLBR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BIIF72 , LLFL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BIIF72 , LLLB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( BIIF72 , LLLL72 )
          , Set.fromList
                [ LLLB72, LLLL72, LLLR72 ] )
        , ( ( BIIF72 , LLLR72 )
          , Set.fromList
                [ LLBR72, LLLR72, LLRR72 ] )
        , ( ( BIIF72 , LLRF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( BIIF72 , LLRL72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRL72 ] )
        , ( ( BIIF72 , LLRR72 )
          , Set.fromList
                [ LLRF72, LLRL72, LLRR72 ] )
        , ( ( BIIF72 , LRIL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BIIF72 , LRLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BIIF72 , LRRI72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( BIIF72 , LRRL72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRL72 ] )
        , ( ( BIIF72 , LRRR72 )
          , Set.fromList
                [ LRRI72, LRRL72, LRRR72 ] )
        , ( ( BIIF72 , LSEL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BIIF72 , RBRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BIIF72 , RELE72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( BIIF72 , RFLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72 ] )
        , ( ( BIIF72 , RILR72 )
          , Set.fromList
                [ RBRR72, RILR72, RSER72 ] )
        , ( ( BIIF72 , RLIR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BIIF72 , RLLI72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( BIIF72 , RLLL72 )
          , Set.fromList
                [ RLLI72, RLLL72, RLLR72 ] )
        , ( ( BIIF72 , RLLR72 )
          , Set.fromList
                [ RLIR72, RLLR72, RLRR72 ] )
        , ( ( BIIF72 , RLRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BIIF72 , RRBL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BIIF72 , RRFR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BIIF72 , RRLF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( BIIF72 , RRLL72 )
          , Set.fromList
                [ RRLF72, RRLL72, RRLR72 ] )
        , ( ( BIIF72 , RRLR72 )
          , Set.fromList
                [ RRFR72, RRLR72, RRRR72 ] )
        , ( ( BIIF72 , RRRB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( BIIF72 , RRRL72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRL72 ] )
        , ( ( BIIF72 , RRRR72 )
          , Set.fromList
                [ RRRB72, RRRL72, RRRR72 ] )
        , ( ( BIIF72 , RSER72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BIIF72 , SBSB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BIIF72 , SESE72 )
          , Set.fromList
                [ BIIF72 ] )
        , ( ( BIIF72 , SFSI72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( BIIF72 , SISF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72 ] )
        , ( ( BIIF72 , SLSR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BIIF72 , SRSL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BLRR72 , BBBB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BLRR72 , BBFF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BLRR72 , BEIE72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BLRR72 , BFII72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BLRR72 , BIIF72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BLRR72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( BLRR72 , BRLL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( BLRR72 , BSEF72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BLRR72 , EBIS72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BLRR72 , EFBS72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BLRR72 , EIFS72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BLRR72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72 ] )
        , ( ( BLRR72 , ERRS72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( BLRR72 , ESES72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BLRR72 , FBII72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BLRR72 , FEFE72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BLRR72 , FFBB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BLRR72 , FFFF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BLRR72 , FIFI72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BLRR72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72 ] )
        , ( ( BLRR72 , FRRR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( BLRR72 , FSEI72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BLRR72 , IBIB72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BLRR72 , IEBE72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BLRR72 , IFBI72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BLRR72 , IIBF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BLRR72 , IIFB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BLRR72 , ILLR72 )
          , Set.fromList
                [ LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72 ] )
        , ( ( BLRR72 , IRRL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( BLRR72 , ISEB72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BLRR72 , LBLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( BLRR72 , LERE72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLRR72 ] )
        , ( ( BLRR72 , LFRR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLRR72 ] )
        , ( ( BLRR72 , LIRL72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLRR72 ] )
        , ( ( BLRR72 , LLBR72 )
          , Set.fromList
                [ BBBB72, LLRR72, RRLL72 ] )
        , ( ( BLRR72 , LLFL72 )
          , Set.fromList
                [ BBFF72, LLLL72, RRRR72 ] )
        , ( ( BLRR72 , LLLB72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, RRLL72 ] )
        , ( ( BLRR72 , LLLL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( BLRR72 , LLLR72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, RRLL72 ] )
        , ( ( BLRR72 , LLRF72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RBRR72
                , RLRR72, RRRR72 ] )
        , ( ( BLRR72 , LLRL72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RBRR72
                , RLRR72, RRRR72 ] )
        , ( ( BLRR72 , LLRR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RBRR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( BLRR72 , LRIL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, LRLL72
                , RLRR72 ] )
        , ( ( BLRR72 , LRLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( BLRR72 , LRRI72 )
          , Set.fromList
                [ BLRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLRR72 ] )
        , ( ( BLRR72 , LRRL72 )
          , Set.fromList
                [ BLRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLRR72 ] )
        , ( ( BLRR72 , LRRR72 )
          , Set.fromList
                [ BLRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLRR72 ] )
        , ( ( BLRR72 , LSEL72 )
          , Set.fromList
                [ BBFF72, LBLL72, RBRR72 ] )
        , ( ( BLRR72 , RBRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( BLRR72 , RELE72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( BLRR72 , RFLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( BLRR72 , RILR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( BLRR72 , RLIR72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, LRLL72, RLRR72
                , SBSB72 ] )
        , ( ( BLRR72 , RLLI72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( BLRR72 , RLLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( BLRR72 , RLLR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( BLRR72 , RLRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( BLRR72 , RRBL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, LLRR72, RRLL72, SESE72, SFSI72
                , SISF72 ] )
        , ( ( BLRR72 , RRFR72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, LLLL72, RRRR72
                , SBSB72 ] )
        , ( ( BLRR72 , RRLF72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( BLRR72 , RRLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( BLRR72 , RRLR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( BLRR72 , RRRB72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( BLRR72 , RRRL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( BLRR72 , RRRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( BLRR72 , RSER72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, LBLL72, RBRR72
                , SBSB72 ] )
        , ( ( BLRR72 , SBSB72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BLRR72 , SESE72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BLRR72 , SFSI72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BLRR72 , SISF72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BLRR72 , SLSR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72 ] )
        , ( ( BLRR72 , SRSL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72
                , BSEF72 ] )
        , ( ( BRLL72 , BBBB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BRLL72 , BBFF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BRLL72 , BEIE72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BRLL72 , BFII72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BRLL72 , BIIF72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BRLL72 , BLRR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( BRLL72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72 ] )
        , ( ( BRLL72 , BSEF72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BRLL72 , EBIS72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BRLL72 , EFBS72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BRLL72 , EIFS72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BRLL72 , ELLS72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( BRLL72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( BRLL72 , ESES72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BRLL72 , FBII72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BRLL72 , FEFE72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BRLL72 , FFBB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BRLL72 , FFFF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BRLL72 , FIFI72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BRLL72 , FLLL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( BRLL72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( BRLL72 , FSEI72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BRLL72 , IBIB72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BRLL72 , IEBE72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BRLL72 , IFBI72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BRLL72 , IIBF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BRLL72 , IIFB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BRLL72 , ILLR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( BRLL72 , IRRL72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( BRLL72 , ISEB72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BRLL72 , LBLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( BRLL72 , LERE72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( BRLL72 , LFRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( BRLL72 , LIRL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( BRLL72 , LLBR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, LLRR72, RRLL72, SESE72, SFSI72
                , SISF72 ] )
        , ( ( BRLL72 , LLFL72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, LLLL72, RRRR72
                , SBSB72 ] )
        , ( ( BRLL72 , LLLB72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( BRLL72 , LLLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( BRLL72 , LLLR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( BRLL72 , LLRF72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( BRLL72 , LLRL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( BRLL72 , LLRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( BRLL72 , LRIL72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, LRLL72, RLRR72
                , SBSB72 ] )
        , ( ( BRLL72 , LRLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( BRLL72 , LRRI72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( BRLL72 , LRRL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( BRLL72 , LRRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( BRLL72 , LSEL72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, LBLL72, RBRR72
                , SBSB72 ] )
        , ( ( BRLL72 , RBRR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLRR72 ] )
        , ( ( BRLL72 , RELE72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( BRLL72 , RFLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( BRLL72 , RILR72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( BRLL72 , RLIR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, LRLL72
                , RLRR72 ] )
        , ( ( BRLL72 , RLLI72 )
          , Set.fromList
                [ BRLL72, LRLL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( BRLL72 , RLLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( BRLL72 , RLLR72 )
          , Set.fromList
                [ BRLL72, LRLL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( BRLL72 , RLRR72 )
          , Set.fromList
                [ BLRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLRR72 ] )
        , ( ( BRLL72 , RRBL72 )
          , Set.fromList
                [ BBBB72, LLRR72, RRLL72 ] )
        , ( ( BRLL72 , RRFR72 )
          , Set.fromList
                [ BBFF72, LLLL72, RRRR72 ] )
        , ( ( BRLL72 , RRLF72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( BRLL72 , RRLL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( BRLL72 , RRLR72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( BRLL72 , RRRB72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( BRLL72 , RRRL72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( BRLL72 , RRRR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RBRR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( BRLL72 , RSER72 )
          , Set.fromList
                [ BBFF72, LBLL72, RBRR72 ] )
        , ( ( BRLL72 , SBSB72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BRLL72 , SESE72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BRLL72 , SFSI72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BRLL72 , SISF72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BRLL72 , SLSR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72
                , BSEF72 ] )
        , ( ( BRLL72 , SRSL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72 ] )
        , ( ( BSEF72 , BBBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BSEF72 , BBFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BSEF72 , BEIE72 )
          , Set.fromList
                [ BSEF72 ] )
        , ( ( BSEF72 , BFII72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( BSEF72 , BIIF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BSEF72 , BLRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BSEF72 , BRLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BSEF72 , BSEF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BSEF72 , EBIS72 )
          , Set.fromList
                [ SBSB72 ] )
        , ( ( BSEF72 , EFBS72 )
          , Set.fromList
                [ SESE72, SFSI72, SISF72 ] )
        , ( ( BSEF72 , EIFS72 )
          , Set.fromList
                [ SBSB72 ] )
        , ( ( BSEF72 , ELLS72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( BSEF72 , ERRS72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( BSEF72 , ESES72 )
          , Set.fromList
                [ SBSB72 ] )
        , ( ( BSEF72 , FBII72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( BSEF72 , FEFE72 )
          , Set.fromList
                [ ESES72, FSEI72, ISEB72 ] )
        , ( ( BSEF72 , FFBB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72 ] )
        , ( ( BSEF72 , FFFF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72 ] )
        , ( ( BSEF72 , FIFI72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( BSEF72 , FLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72 ] )
        , ( ( BSEF72 , FRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72 ] )
        , ( ( BSEF72 , FSEI72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( BSEF72 , IBIB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BSEF72 , IEBE72 )
          , Set.fromList
                [ BSEF72 ] )
        , ( ( BSEF72 , IFBI72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( BSEF72 , IIBF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BSEF72 , IIFB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BSEF72 , ILLR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BSEF72 , IRRL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( BSEF72 , ISEB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BSEF72 , LBLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BSEF72 , LERE72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( BSEF72 , LFRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72 ] )
        , ( ( BSEF72 , LIRL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BSEF72 , LLBR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BSEF72 , LLFL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BSEF72 , LLLB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( BSEF72 , LLLL72 )
          , Set.fromList
                [ LLLB72, LLLL72, LLLR72 ] )
        , ( ( BSEF72 , LLLR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( BSEF72 , LLRF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( BSEF72 , LLRL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( BSEF72 , LLRR72 )
          , Set.fromList
                [ LLRF72, LLRL72, LLRR72 ] )
        , ( ( BSEF72 , LRIL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BSEF72 , LRLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BSEF72 , LRRI72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( BSEF72 , LRRL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( BSEF72 , LRRR72 )
          , Set.fromList
                [ LRRI72, LRRL72, LRRR72 ] )
        , ( ( BSEF72 , LSEL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( BSEF72 , RBRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BSEF72 , RELE72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( BSEF72 , RFLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72 ] )
        , ( ( BSEF72 , RILR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BSEF72 , RLIR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BSEF72 , RLLI72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( BSEF72 , RLLL72 )
          , Set.fromList
                [ RLLI72, RLLL72, RLLR72 ] )
        , ( ( BSEF72 , RLLR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BSEF72 , RLRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( BSEF72 , RRBL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BSEF72 , RRFR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BSEF72 , RRLF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( BSEF72 , RRLL72 )
          , Set.fromList
                [ RRLF72, RRLL72, RRLR72 ] )
        , ( ( BSEF72 , RRLR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( BSEF72 , RRRB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( BSEF72 , RRRL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( BSEF72 , RRRR72 )
          , Set.fromList
                [ RRRB72, RRRL72, RRRR72 ] )
        , ( ( BSEF72 , RSER72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( BSEF72 , SBSB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( BSEF72 , SESE72 )
          , Set.fromList
                [ BSEF72 ] )
        , ( ( BSEF72 , SFSI72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( BSEF72 , SISF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( BSEF72 , SLSR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( BSEF72 , SRSL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( EBIS72 , BBBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( EBIS72 , BBFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( EBIS72 , BEIE72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( EBIS72 , BFII72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( EBIS72 , BIIF72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( EBIS72 , BLRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( EBIS72 , BRLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( EBIS72 , BSEF72 )
          , Set.fromList
                [ FEFE72 ] )
        , ( ( EBIS72 , EBIS72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( EBIS72 , EFBS72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( EBIS72 , EIFS72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72 ] )
        , ( ( EBIS72 , ELLS72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( EBIS72 , ERRS72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( EBIS72 , ESES72 )
          , Set.fromList
                [ BEIE72 ] )
        , ( ( EBIS72 , FBII72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( EBIS72 , FEFE72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( EBIS72 , FFBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( EBIS72 , FFFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( EBIS72 , FIFI72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72 ] )
        , ( ( EBIS72 , FLLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( EBIS72 , FRRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( EBIS72 , FSEI72 )
          , Set.fromList
                [ BEIE72 ] )
        , ( ( EBIS72 , IBIB72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( EBIS72 , IEBE72 )
          , Set.fromList
                [ BBBB72, IBIB72, SBSB72 ] )
        , ( ( EBIS72 , IFBI72 )
          , Set.fromList
                [ BBBB72, IBIB72, SBSB72 ] )
        , ( ( EBIS72 , IIBF72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( EBIS72 , IIFB72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, SISF72 ] )
        , ( ( EBIS72 , ILLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, SRSL72 ] )
        , ( ( EBIS72 , IRRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, SLSR72 ] )
        , ( ( EBIS72 , ISEB72 )
          , Set.fromList
                [ BEIE72, IEBE72, SESE72 ] )
        , ( ( EBIS72 , LBLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( EBIS72 , LERE72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( EBIS72 , LFRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( EBIS72 , LIRL72 )
          , Set.fromList
                [ RBRR72, RILR72, RSER72 ] )
        , ( ( EBIS72 , LLBR72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( EBIS72 , LLFL72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( EBIS72 , LLLB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( EBIS72 , LLLL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( EBIS72 , LLLR72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRL72 ] )
        , ( ( EBIS72 , LLRF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( EBIS72 , LLRL72 )
          , Set.fromList
                [ RRFR72, RRLR72, RRRR72 ] )
        , ( ( EBIS72 , LLRR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( EBIS72 , LRIL72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( EBIS72 , LRLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( EBIS72 , LRRI72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( EBIS72 , LRRL72 )
          , Set.fromList
                [ RLIR72, RLLR72, RLRR72 ] )
        , ( ( EBIS72 , LRRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( EBIS72 , LSEL72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( EBIS72 , RBRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( EBIS72 , RELE72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( EBIS72 , RFLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( EBIS72 , RILR72 )
          , Set.fromList
                [ LBLL72, LIRL72, LSEL72 ] )
        , ( ( EBIS72 , RLIR72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( EBIS72 , RLLI72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( EBIS72 , RLLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( EBIS72 , RLLR72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRL72 ] )
        , ( ( EBIS72 , RLRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( EBIS72 , RRBL72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( EBIS72 , RRFR72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( EBIS72 , RRLF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( EBIS72 , RRLL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( EBIS72 , RRLR72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRL72 ] )
        , ( ( EBIS72 , RRRB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( EBIS72 , RRRL72 )
          , Set.fromList
                [ LLBR72, LLLR72, LLRR72 ] )
        , ( ( EBIS72 , RRRR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( EBIS72 , RSER72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( EBIS72 , SBSB72 )
          , Set.fromList
                [ EFBS72 ] )
        , ( ( EBIS72 , SESE72 )
          , Set.fromList
                [ EBIS72 ] )
        , ( ( EBIS72 , SFSI72 )
          , Set.fromList
                [ EBIS72 ] )
        , ( ( EBIS72 , SISF72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72 ] )
        , ( ( EBIS72 , SLSR72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( EBIS72 , SRSL72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( EFBS72 , BBBB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( EFBS72 , BBFF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, SISF72 ] )
        , ( ( EFBS72 , BEIE72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( EFBS72 , BFII72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( EFBS72 , BIIF72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( EFBS72 , BLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, SLSR72 ] )
        , ( ( EFBS72 , BRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, SRSL72 ] )
        , ( ( EFBS72 , BSEF72 )
          , Set.fromList
                [ BEIE72, IEBE72, SESE72 ] )
        , ( ( EFBS72 , EBIS72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( EFBS72 , EFBS72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( EFBS72 , EIFS72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( EFBS72 , ELLS72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( EFBS72 , ERRS72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( EFBS72 , ESES72 )
          , Set.fromList
                [ FEFE72 ] )
        , ( ( EFBS72 , FBII72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( EFBS72 , FEFE72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( EFBS72 , FFBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( EFBS72 , FFFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( EFBS72 , FIFI72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( EFBS72 , FLLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( EFBS72 , FRRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( EFBS72 , FSEI72 )
          , Set.fromList
                [ FEFE72 ] )
        , ( ( EFBS72 , IBIB72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( EFBS72 , IEBE72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( EFBS72 , IFBI72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( EFBS72 , IIBF72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( EFBS72 , IIFB72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( EFBS72 , ILLR72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( EFBS72 , IRRL72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( EFBS72 , ISEB72 )
          , Set.fromList
                [ FEFE72 ] )
        , ( ( EFBS72 , LBLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LSEL72 ] )
        , ( ( EFBS72 , LERE72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( EFBS72 , LFRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( EFBS72 , LIRL72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( EFBS72 , LLBR72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( EFBS72 , LLFL72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( EFBS72 , LLLB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( EFBS72 , LLLL72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRL72 ] )
        , ( ( EFBS72 , LLLR72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( EFBS72 , LLRF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( EFBS72 , LLRL72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( EFBS72 , LLRR72 )
          , Set.fromList
                [ LLBR72, LLLR72, LLRR72 ] )
        , ( ( EFBS72 , LRIL72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( EFBS72 , LRLL72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRL72 ] )
        , ( ( EFBS72 , LRRI72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( EFBS72 , LRRL72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( EFBS72 , LRRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( EFBS72 , LSEL72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( EFBS72 , RBRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RSER72 ] )
        , ( ( EFBS72 , RELE72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( EFBS72 , RFLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( EFBS72 , RILR72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( EFBS72 , RLIR72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( EFBS72 , RLLI72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( EFBS72 , RLLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( EFBS72 , RLLR72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( EFBS72 , RLRR72 )
          , Set.fromList
                [ RLIR72, RLLR72, RLRR72 ] )
        , ( ( EFBS72 , RRBL72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( EFBS72 , RRFR72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( EFBS72 , RRLF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( EFBS72 , RRLL72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRL72 ] )
        , ( ( EFBS72 , RRLR72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( EFBS72 , RRRB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( EFBS72 , RRRL72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( EFBS72 , RRRR72 )
          , Set.fromList
                [ RRFR72, RRLR72, RRRR72 ] )
        , ( ( EFBS72 , RSER72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( EFBS72 , SBSB72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72 ] )
        , ( ( EFBS72 , SESE72 )
          , Set.fromList
                [ EFBS72 ] )
        , ( ( EFBS72 , SFSI72 )
          , Set.fromList
                [ EFBS72 ] )
        , ( ( EFBS72 , SISF72 )
          , Set.fromList
                [ EFBS72 ] )
        , ( ( EFBS72 , SLSR72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( EFBS72 , SRSL72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( EIFS72 , BBBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( EIFS72 , BBFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( EIFS72 , BEIE72 )
          , Set.fromList
                [ FIFI72 ] )
        , ( ( EIFS72 , BFII72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( EIFS72 , BIIF72 )
          , Set.fromList
                [ FIFI72 ] )
        , ( ( EIFS72 , BLRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( EIFS72 , BRLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( EIFS72 , BSEF72 )
          , Set.fromList
                [ FEFE72 ] )
        , ( ( EIFS72 , EBIS72 )
          , Set.fromList
                [ IFBI72 ] )
        , ( ( EIFS72 , EFBS72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( EIFS72 , EIFS72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( EIFS72 , ELLS72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( EIFS72 , ERRS72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( EIFS72 , ESES72 )
          , Set.fromList
                [ IEBE72 ] )
        , ( ( EIFS72 , FBII72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( EIFS72 , FEFE72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( EIFS72 , FFBB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( EIFS72 , FFFF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, SISF72 ] )
        , ( ( EIFS72 , FIFI72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( EIFS72 , FLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, SRSL72 ] )
        , ( ( EIFS72 , FRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, SLSR72 ] )
        , ( ( EIFS72 , FSEI72 )
          , Set.fromList
                [ BEIE72, IEBE72, SESE72 ] )
        , ( ( EIFS72 , IBIB72 )
          , Set.fromList
                [ IFBI72 ] )
        , ( ( EIFS72 , IEBE72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( EIFS72 , IFBI72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( EIFS72 , IIBF72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( EIFS72 , IIFB72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( EIFS72 , ILLR72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( EIFS72 , IRRL72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( EIFS72 , ISEB72 )
          , Set.fromList
                [ IEBE72 ] )
        , ( ( EIFS72 , LBLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( EIFS72 , LERE72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( EIFS72 , LFRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RSER72 ] )
        , ( ( EIFS72 , LIRL72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( EIFS72 , LLBR72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( EIFS72 , LLFL72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( EIFS72 , LLLB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( EIFS72 , LLLL72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRL72 ] )
        , ( ( EIFS72 , LLLR72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( EIFS72 , LLRF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( EIFS72 , LLRL72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( EIFS72 , LLRR72 )
          , Set.fromList
                [ RRFR72, RRLR72, RRRR72 ] )
        , ( ( EIFS72 , LRIL72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( EIFS72 , LRLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( EIFS72 , LRRI72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( EIFS72 , LRRL72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( EIFS72 , LRRR72 )
          , Set.fromList
                [ RLIR72, RLLR72, RLRR72 ] )
        , ( ( EIFS72 , LSEL72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( EIFS72 , RBRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( EIFS72 , RELE72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( EIFS72 , RFLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LSEL72 ] )
        , ( ( EIFS72 , RILR72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( EIFS72 , RLIR72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( EIFS72 , RLLI72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( EIFS72 , RLLL72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRL72 ] )
        , ( ( EIFS72 , RLLR72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( EIFS72 , RLRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( EIFS72 , RRBL72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( EIFS72 , RRFR72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( EIFS72 , RRLF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( EIFS72 , RRLL72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRL72 ] )
        , ( ( EIFS72 , RRLR72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( EIFS72 , RRRB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( EIFS72 , RRRL72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( EIFS72 , RRRR72 )
          , Set.fromList
                [ LLBR72, LLLR72, LLRR72 ] )
        , ( ( EIFS72 , RSER72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( EIFS72 , SBSB72 )
          , Set.fromList
                [ EFBS72 ] )
        , ( ( EIFS72 , SESE72 )
          , Set.fromList
                [ EIFS72 ] )
        , ( ( EIFS72 , SFSI72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72 ] )
        , ( ( EIFS72 , SISF72 )
          , Set.fromList
                [ EIFS72 ] )
        , ( ( EIFS72 , SLSR72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( EIFS72 , SRSL72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( ELLS72 , BBBB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( ELLS72 , BBFF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( ELLS72 , BEIE72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( ELLS72 , BFII72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( ELLS72 , BIIF72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( ELLS72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( ELLS72 , BRLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( ELLS72 , BSEF72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( ELLS72 , EBIS72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( ELLS72 , EFBS72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( ELLS72 , EIFS72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( ELLS72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( ELLS72 , ERRS72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( ELLS72 , ESES72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( ELLS72 , FBII72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( ELLS72 , FEFE72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( ELLS72 , FFBB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( ELLS72 , FFFF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( ELLS72 , FIFI72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( ELLS72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( ELLS72 , FRRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( ELLS72 , FSEI72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( ELLS72 , IBIB72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( ELLS72 , IEBE72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( ELLS72 , IFBI72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( ELLS72 , IIBF72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( ELLS72 , IIFB72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( ELLS72 , ILLR72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( ELLS72 , IRRL72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( ELLS72 , ISEB72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( ELLS72 , LBLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( ELLS72 , LERE72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( ELLS72 , LFRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( ELLS72 , LIRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( ELLS72 , LLBR72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, LLLB72, RRRB72
                , SBSB72 ] )
        , ( ( ELLS72 , LLFL72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, LLRF72, RRLF72
                , SISF72 ] )
        , ( ( ELLS72 , LLLB72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( ELLS72 , LLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( ELLS72 , LLLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( ELLS72 , LLRF72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( ELLS72 , LLRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( ELLS72 , LLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( ELLS72 , LRIL72 )
          , Set.fromList
                [ BFII72, IFBI72, LRRI72, RLLI72, SFSI72 ] )
        , ( ( ELLS72 , LRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( ELLS72 , LRRI72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( ELLS72 , LRRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( ELLS72 , LRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( ELLS72 , LSEL72 )
          , Set.fromList
                [ BEIE72, IEBE72, LERE72, RELE72, SESE72 ] )
        , ( ( ELLS72 , RBRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( ELLS72 , RELE72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( ELLS72 , RFLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( ELLS72 , RILR72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( ELLS72 , RLIR72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72, LRRI72, RLLI72 ] )
        , ( ( ELLS72 , RLLI72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( ELLS72 , RLLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( ELLS72 , RLLR72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( ELLS72 , RLRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( ELLS72 , RRBL72 )
          , Set.fromList
                [ FFBB72, LLLB72, RRRB72 ] )
        , ( ( ELLS72 , RRFR72 )
          , Set.fromList
                [ FFFF72, LLRF72, RRLF72 ] )
        , ( ( ELLS72 , RRLF72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72
                , RRLL72 ] )
        , ( ( ELLS72 , RRLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72, RRBL72
                , RRLL72, RRRL72 ] )
        , ( ( ELLS72 , RRLR72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72
                , RRLL72 ] )
        , ( ( ELLS72 , RRRB72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RRRR72 ] )
        , ( ( ELLS72 , RRRL72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RRRR72 ] )
        , ( ( ELLS72 , RRRR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72, RRFR72
                , RRLR72, RRRR72 ] )
        , ( ( ELLS72 , RSER72 )
          , Set.fromList
                [ FEFE72, LERE72, RELE72 ] )
        , ( ( ELLS72 , SBSB72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( ELLS72 , SESE72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( ELLS72 , SFSI72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( ELLS72 , SISF72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( ELLS72 , SLSR72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72 ] )
        , ( ( ELLS72 , SRSL72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72 ] )
        , ( ( ERRS72 , BBBB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( ERRS72 , BBFF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( ERRS72 , BEIE72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( ERRS72 , BFII72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( ERRS72 , BIIF72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( ERRS72 , BLRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( ERRS72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( ERRS72 , BSEF72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( ERRS72 , EBIS72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( ERRS72 , EFBS72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( ERRS72 , EIFS72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( ERRS72 , ELLS72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( ERRS72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( ERRS72 , ESES72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( ERRS72 , FBII72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( ERRS72 , FEFE72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( ERRS72 , FFBB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( ERRS72 , FFFF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( ERRS72 , FIFI72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( ERRS72 , FLLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( ERRS72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( ERRS72 , FSEI72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( ERRS72 , IBIB72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( ERRS72 , IEBE72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( ERRS72 , IFBI72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( ERRS72 , IIBF72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( ERRS72 , IIFB72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( ERRS72 , ILLR72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( ERRS72 , IRRL72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( ERRS72 , ISEB72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( ERRS72 , LBLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( ERRS72 , LERE72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( ERRS72 , LFRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( ERRS72 , LIRL72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( ERRS72 , LLBR72 )
          , Set.fromList
                [ FFBB72, LLLB72, RRRB72 ] )
        , ( ( ERRS72 , LLFL72 )
          , Set.fromList
                [ FFFF72, LLRF72, RRLF72 ] )
        , ( ( ERRS72 , LLLB72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRL72 ] )
        , ( ( ERRS72 , LLLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72, RRBL72
                , RRLL72, RRRL72 ] )
        , ( ( ERRS72 , LLLR72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRL72 ] )
        , ( ( ERRS72 , LLRF72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLR72
                , RRRR72 ] )
        , ( ( ERRS72 , LLRL72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLR72
                , RRRR72 ] )
        , ( ( ERRS72 , LLRR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72, RRFR72
                , RRLR72, RRRR72 ] )
        , ( ( ERRS72 , LRIL72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72, LRRI72, RLLI72 ] )
        , ( ( ERRS72 , LRLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( ERRS72 , LRRI72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( ERRS72 , LRRL72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( ERRS72 , LRRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( ERRS72 , LSEL72 )
          , Set.fromList
                [ FEFE72, LERE72, RELE72 ] )
        , ( ( ERRS72 , RBRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( ERRS72 , RELE72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( ERRS72 , RFLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( ERRS72 , RILR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( ERRS72 , RLIR72 )
          , Set.fromList
                [ BFII72, IFBI72, LRRI72, RLLI72, SFSI72 ] )
        , ( ( ERRS72 , RLLI72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( ERRS72 , RLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( ERRS72 , RLLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( ERRS72 , RLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( ERRS72 , RRBL72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, LLLB72, RRRB72
                , SBSB72 ] )
        , ( ( ERRS72 , RRFR72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, LLRF72, RRLF72
                , SISF72 ] )
        , ( ( ERRS72 , RRLF72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( ERRS72 , RRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( ERRS72 , RRLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( ERRS72 , RRRB72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( ERRS72 , RRRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( ERRS72 , RRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( ERRS72 , RSER72 )
          , Set.fromList
                [ BEIE72, IEBE72, LERE72, RELE72, SESE72 ] )
        , ( ( ERRS72 , SBSB72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( ERRS72 , SESE72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( ERRS72 , SFSI72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( ERRS72 , SISF72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( ERRS72 , SLSR72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72 ] )
        , ( ( ERRS72 , SRSL72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72 ] )
        , ( ( ESES72 , BBBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( ESES72 , BBFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( ESES72 , BEIE72 )
          , Set.fromList
                [ FSEI72 ] )
        , ( ( ESES72 , BFII72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( ESES72 , BIIF72 )
          , Set.fromList
                [ FIFI72 ] )
        , ( ( ESES72 , BLRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( ESES72 , BRLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( ESES72 , BSEF72 )
          , Set.fromList
                [ FEFE72 ] )
        , ( ( ESES72 , EBIS72 )
          , Set.fromList
                [ SFSI72 ] )
        , ( ( ESES72 , EFBS72 )
          , Set.fromList
                [ SBSB72 ] )
        , ( ( ESES72 , EIFS72 )
          , Set.fromList
                [ SISF72 ] )
        , ( ( ESES72 , ELLS72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( ESES72 , ERRS72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( ESES72 , ESES72 )
          , Set.fromList
                [ SESE72 ] )
        , ( ( ESES72 , FBII72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( ESES72 , FEFE72 )
          , Set.fromList
                [ BSEF72 ] )
        , ( ( ESES72 , FFBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( ESES72 , FFFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( ESES72 , FIFI72 )
          , Set.fromList
                [ BIIF72 ] )
        , ( ( ESES72 , FLLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( ESES72 , FRRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( ESES72 , FSEI72 )
          , Set.fromList
                [ BEIE72 ] )
        , ( ( ESES72 , IBIB72 )
          , Set.fromList
                [ IFBI72 ] )
        , ( ( ESES72 , IEBE72 )
          , Set.fromList
                [ ISEB72 ] )
        , ( ( ESES72 , IFBI72 )
          , Set.fromList
                [ IBIB72 ] )
        , ( ( ESES72 , IIBF72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( ESES72 , IIFB72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( ESES72 , ILLR72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( ESES72 , IRRL72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( ESES72 , ISEB72 )
          , Set.fromList
                [ IEBE72 ] )
        , ( ( ESES72 , LBLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( ESES72 , LERE72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( ESES72 , LFRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( ESES72 , LIRL72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( ESES72 , LLBR72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( ESES72 , LLFL72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( ESES72 , LLLB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( ESES72 , LLLL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( ESES72 , LLLR72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( ESES72 , LLRF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( ESES72 , LLRL72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( ESES72 , LLRR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( ESES72 , LRIL72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( ESES72 , LRLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( ESES72 , LRRI72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( ESES72 , LRRL72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( ESES72 , LRRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( ESES72 , LSEL72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( ESES72 , RBRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( ESES72 , RELE72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( ESES72 , RFLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( ESES72 , RILR72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( ESES72 , RLIR72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( ESES72 , RLLI72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( ESES72 , RLLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( ESES72 , RLLR72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( ESES72 , RLRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( ESES72 , RRBL72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( ESES72 , RRFR72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( ESES72 , RRLF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( ESES72 , RRLL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( ESES72 , RRLR72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( ESES72 , RRRB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( ESES72 , RRRL72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( ESES72 , RRRR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( ESES72 , RSER72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( ESES72 , SBSB72 )
          , Set.fromList
                [ EFBS72 ] )
        , ( ( ESES72 , SESE72 )
          , Set.fromList
                [ ESES72 ] )
        , ( ( ESES72 , SFSI72 )
          , Set.fromList
                [ EBIS72 ] )
        , ( ( ESES72 , SISF72 )
          , Set.fromList
                [ EIFS72 ] )
        , ( ( ESES72 , SLSR72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( ESES72 , SRSL72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( FBII72 , BBBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FBII72 , BBFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FBII72 , BEIE72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( FBII72 , BFII72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( FBII72 , BIIF72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72 ] )
        , ( ( FBII72 , BLRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FBII72 , BRLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FBII72 , BSEF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FBII72 , EBIS72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( FBII72 , EFBS72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( FBII72 , EIFS72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72 ] )
        , ( ( FBII72 , ELLS72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( FBII72 , ERRS72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( FBII72 , ESES72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( FBII72 , FBII72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( FBII72 , FEFE72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( FBII72 , FFBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( FBII72 , FFFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( FBII72 , FIFI72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72 ] )
        , ( ( FBII72 , FLLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( FBII72 , FRRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( FBII72 , FSEI72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( FBII72 , IBIB72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, SFSI72 ] )
        , ( ( FBII72 , IEBE72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, SBSB72 ] )
        , ( ( FBII72 , IFBI72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, SBSB72 ] )
        , ( ( FBII72 , IIBF72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( FBII72 , IIFB72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, SESE72, SFSI72, SISF72 ] )
        , ( ( FBII72 , ILLR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, SRSL72 ] )
        , ( ( FBII72 , IRRL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, SLSR72 ] )
        , ( ( FBII72 , ISEB72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, SFSI72 ] )
        , ( ( FBII72 , LBLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FBII72 , LERE72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( FBII72 , LFRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( FBII72 , LIRL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RSER72 ] )
        , ( ( FBII72 , LLBR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FBII72 , LLFL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FBII72 , LLLB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FBII72 , LLLL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FBII72 , LLLR72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( FBII72 , LLRF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FBII72 , LLRL72 )
          , Set.fromList
                [ RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( FBII72 , LLRR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FBII72 , LRIL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FBII72 , LRLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FBII72 , LRRI72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( FBII72 , LRRL72 )
          , Set.fromList
                [ RLIR72, RLLI72, RLLL72, RLLR72, RLRR72 ] )
        , ( ( FBII72 , LRRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( FBII72 , LSEL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FBII72 , RBRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FBII72 , RELE72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( FBII72 , RFLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( FBII72 , RILR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LSEL72 ] )
        , ( ( FBII72 , RLIR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FBII72 , RLLI72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( FBII72 , RLLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( FBII72 , RLLR72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( FBII72 , RLRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FBII72 , RRBL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FBII72 , RRFR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FBII72 , RRLF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FBII72 , RRLL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FBII72 , RRLR72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRF72, LLRL72, LLRR72 ] )
        , ( ( FBII72 , RRRB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FBII72 , RRRL72 )
          , Set.fromList
                [ LLBR72, LLLB72, LLLL72, LLLR72, LLRR72 ] )
        , ( ( FBII72 , RRRR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FBII72 , RSER72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FBII72 , SBSB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FBII72 , SESE72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( FBII72 , SFSI72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( FBII72 , SISF72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72 ] )
        , ( ( FBII72 , SLSR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FBII72 , SRSL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FEFE72 , BBBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FEFE72 , BBFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FEFE72 , BEIE72 )
          , Set.fromList
                [ FEFE72 ] )
        , ( ( FEFE72 , BFII72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( FEFE72 , BIIF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FEFE72 , BLRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FEFE72 , BRLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FEFE72 , BSEF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FEFE72 , EBIS72 )
          , Set.fromList
                [ EFBS72 ] )
        , ( ( FEFE72 , EFBS72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72 ] )
        , ( ( FEFE72 , EIFS72 )
          , Set.fromList
                [ EFBS72 ] )
        , ( ( FEFE72 , ELLS72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( FEFE72 , ERRS72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( FEFE72 , ESES72 )
          , Set.fromList
                [ EFBS72 ] )
        , ( ( FEFE72 , FBII72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( FEFE72 , FEFE72 )
          , Set.fromList
                [ BEIE72, IEBE72, SESE72 ] )
        , ( ( FEFE72 , FFBB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( FEFE72 , FFFF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, SISF72 ] )
        , ( ( FEFE72 , FIFI72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( FEFE72 , FLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, SRSL72 ] )
        , ( ( FEFE72 , FRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, SLSR72 ] )
        , ( ( FEFE72 , FSEI72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( FEFE72 , IBIB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FEFE72 , IEBE72 )
          , Set.fromList
                [ FEFE72 ] )
        , ( ( FEFE72 , IFBI72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( FEFE72 , IIBF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FEFE72 , IIFB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FEFE72 , ILLR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FEFE72 , IRRL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FEFE72 , ISEB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FEFE72 , LBLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FEFE72 , LERE72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( FEFE72 , LFRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RSER72 ] )
        , ( ( FEFE72 , LIRL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FEFE72 , LLBR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FEFE72 , LLFL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FEFE72 , LLLB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( FEFE72 , LLLL72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRL72 ] )
        , ( ( FEFE72 , LLLR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FEFE72 , LLRF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( FEFE72 , LLRL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FEFE72 , LLRR72 )
          , Set.fromList
                [ RRFR72, RRLR72, RRRR72 ] )
        , ( ( FEFE72 , LRIL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FEFE72 , LRLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FEFE72 , LRRI72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( FEFE72 , LRRL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FEFE72 , LRRR72 )
          , Set.fromList
                [ RLIR72, RLLR72, RLRR72 ] )
        , ( ( FEFE72 , LSEL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FEFE72 , RBRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FEFE72 , RELE72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( FEFE72 , RFLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LSEL72 ] )
        , ( ( FEFE72 , RILR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FEFE72 , RLIR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FEFE72 , RLLI72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( FEFE72 , RLLL72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRL72 ] )
        , ( ( FEFE72 , RLLR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FEFE72 , RLRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FEFE72 , RRBL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FEFE72 , RRFR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FEFE72 , RRLF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( FEFE72 , RRLL72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRL72 ] )
        , ( ( FEFE72 , RRLR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FEFE72 , RRRB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( FEFE72 , RRRL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FEFE72 , RRRR72 )
          , Set.fromList
                [ LLBR72, LLLR72, LLRR72 ] )
        , ( ( FEFE72 , RSER72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FEFE72 , SBSB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FEFE72 , SESE72 )
          , Set.fromList
                [ FEFE72 ] )
        , ( ( FEFE72 , SFSI72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( FEFE72 , SISF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FEFE72 , SLSR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FEFE72 , SRSL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FFBB72 , BBBB72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( FFBB72 , BBFF72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, SESE72, SFSI72, SISF72 ] )
        , ( ( FFBB72 , BEIE72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, SFSI72 ] )
        , ( ( FFBB72 , BFII72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, SFSI72 ] )
        , ( ( FFBB72 , BIIF72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, SFSI72 ] )
        , ( ( FFBB72 , BLRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, SLSR72 ] )
        , ( ( FFBB72 , BRLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, SRSL72 ] )
        , ( ( FFBB72 , BSEF72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, SFSI72 ] )
        , ( ( FFBB72 , EBIS72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72 ] )
        , ( ( FFBB72 , EFBS72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFBB72 , EIFS72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFBB72 , ELLS72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FFBB72 , ERRS72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FFBB72 , ESES72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFBB72 , FBII72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72 ] )
        , ( ( FFBB72 , FEFE72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFBB72 , FFBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFBB72 , FFFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFBB72 , FIFI72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFBB72 , FLLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FFBB72 , FRRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FFBB72 , FSEI72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFBB72 , IBIB72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72 ] )
        , ( ( FFBB72 , IEBE72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFBB72 , IFBI72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFBB72 , IIBF72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFBB72 , IIFB72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFBB72 , ILLR72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FFBB72 , IRRL72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FFBB72 , ISEB72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFBB72 , LBLL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LSEL72 ] )
        , ( ( FFBB72 , LERE72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FFBB72 , LFRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FFBB72 , LIRL72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FFBB72 , LLBR72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FFBB72 , LLFL72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FFBB72 , LLLB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FFBB72 , LLLL72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRF72, LLRL72, LLRR72 ] )
        , ( ( FFBB72 , LLLR72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FFBB72 , LLRF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FFBB72 , LLRL72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FFBB72 , LLRR72 )
          , Set.fromList
                [ LLBR72, LLLB72, LLLL72, LLLR72, LLRR72 ] )
        , ( ( FFBB72 , LRIL72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FFBB72 , LRLL72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( FFBB72 , LRRI72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FFBB72 , LRRL72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FFBB72 , LRRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FFBB72 , LSEL72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FFBB72 , RBRR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RSER72 ] )
        , ( ( FFBB72 , RELE72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FFBB72 , RFLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FFBB72 , RILR72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FFBB72 , RLIR72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FFBB72 , RLLI72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FFBB72 , RLLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FFBB72 , RLLR72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FFBB72 , RLRR72 )
          , Set.fromList
                [ RLIR72, RLLI72, RLLL72, RLLR72, RLRR72 ] )
        , ( ( FFBB72 , RRBL72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FFBB72 , RRFR72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FFBB72 , RRLF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FFBB72 , RRLL72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( FFBB72 , RRLR72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FFBB72 , RRRB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FFBB72 , RRRL72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FFBB72 , RRRR72 )
          , Set.fromList
                [ RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( FFBB72 , RSER72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FFBB72 , SBSB72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72 ] )
        , ( ( FFBB72 , SESE72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFBB72 , SFSI72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFBB72 , SISF72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFBB72 , SLSR72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FFBB72 , SRSL72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FFFF72 , BBBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFFF72 , BBFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFFF72 , BEIE72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFFF72 , BFII72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72 ] )
        , ( ( FFFF72 , BIIF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFFF72 , BLRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FFFF72 , BRLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FFFF72 , BSEF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFFF72 , EBIS72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFFF72 , EFBS72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72 ] )
        , ( ( FFFF72 , EIFS72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFFF72 , ELLS72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FFFF72 , ERRS72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FFFF72 , ESES72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFFF72 , FBII72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, SFSI72 ] )
        , ( ( FFFF72 , FEFE72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, SFSI72 ] )
        , ( ( FFFF72 , FFBB72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( FFFF72 , FFFF72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, SESE72, SFSI72, SISF72 ] )
        , ( ( FFFF72 , FIFI72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, SFSI72 ] )
        , ( ( FFFF72 , FLLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, SRSL72 ] )
        , ( ( FFFF72 , FRRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, SLSR72 ] )
        , ( ( FFFF72 , FSEI72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, SFSI72 ] )
        , ( ( FFFF72 , IBIB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFFF72 , IEBE72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFFF72 , IFBI72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72 ] )
        , ( ( FFFF72 , IIBF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFFF72 , IIFB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFFF72 , ILLR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FFFF72 , IRRL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FFFF72 , ISEB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFFF72 , LBLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FFFF72 , LERE72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FFFF72 , LFRR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RSER72 ] )
        , ( ( FFFF72 , LIRL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FFFF72 , LLBR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FFFF72 , LLFL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FFFF72 , LLLB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FFFF72 , LLLL72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( FFFF72 , LLLR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FFFF72 , LLRF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FFFF72 , LLRL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FFFF72 , LLRR72 )
          , Set.fromList
                [ RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( FFFF72 , LRIL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FFFF72 , LRLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FFFF72 , LRRI72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FFFF72 , LRRL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FFFF72 , LRRR72 )
          , Set.fromList
                [ RLIR72, RLLI72, RLLL72, RLLR72, RLRR72 ] )
        , ( ( FFFF72 , LSEL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FFFF72 , RBRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FFFF72 , RELE72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FFFF72 , RFLL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LSEL72 ] )
        , ( ( FFFF72 , RILR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FFFF72 , RLIR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FFFF72 , RLLI72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FFFF72 , RLLL72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( FFFF72 , RLLR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FFFF72 , RLRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FFFF72 , RRBL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FFFF72 , RRFR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FFFF72 , RRLF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FFFF72 , RRLL72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRF72, LLRL72, LLRR72 ] )
        , ( ( FFFF72 , RRLR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FFFF72 , RRRB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FFFF72 , RRRL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FFFF72 , RRRR72 )
          , Set.fromList
                [ LLBR72, LLLB72, LLLL72, LLLR72, LLRR72 ] )
        , ( ( FFFF72 , RSER72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FFFF72 , SBSB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FFFF72 , SESE72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFFF72 , SFSI72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72 ] )
        , ( ( FFFF72 , SISF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FFFF72 , SLSR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FFFF72 , SRSL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FIFI72 , BBBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FIFI72 , BBFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FIFI72 , BEIE72 )
          , Set.fromList
                [ FIFI72 ] )
        , ( ( FIFI72 , BFII72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( FIFI72 , BIIF72 )
          , Set.fromList
                [ FEFE72, FFFF72, FIFI72 ] )
        , ( ( FIFI72 , BLRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FIFI72 , BRLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FIFI72 , BSEF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FIFI72 , EBIS72 )
          , Set.fromList
                [ IFBI72 ] )
        , ( ( FIFI72 , EFBS72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( FIFI72 , EIFS72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( FIFI72 , ELLS72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( FIFI72 , ERRS72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( FIFI72 , ESES72 )
          , Set.fromList
                [ IFBI72 ] )
        , ( ( FIFI72 , FBII72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( FIFI72 , FEFE72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( FIFI72 , FFBB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( FIFI72 , FFFF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, SISF72 ] )
        , ( ( FIFI72 , FIFI72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, IEBE72, IFBI72, IIBF72, SESE72
                , SFSI72, SISF72 ] )
        , ( ( FIFI72 , FLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, SRSL72 ] )
        , ( ( FIFI72 , FRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, SLSR72 ] )
        , ( ( FIFI72 , FSEI72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( FIFI72 , IBIB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IFBI72 ] )
        , ( ( FIFI72 , IEBE72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( FIFI72 , IFBI72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72, FBII72, FIFI72, FSEI72, IBIB72
                , IIFB72, ISEB72 ] )
        , ( ( FIFI72 , IIBF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72 ] )
        , ( ( FIFI72 , IIFB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72 ] )
        , ( ( FIFI72 , ILLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72 ] )
        , ( ( FIFI72 , IRRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72 ] )
        , ( ( FIFI72 , ISEB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IFBI72 ] )
        , ( ( FIFI72 , LBLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FIFI72 , LERE72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( FIFI72 , LFRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RSER72 ] )
        , ( ( FIFI72 , LIRL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72 ] )
        , ( ( FIFI72 , LLBR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FIFI72 , LLFL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FIFI72 , LLLB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( FIFI72 , LLLL72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRL72 ] )
        , ( ( FIFI72 , LLLR72 )
          , Set.fromList
                [ RRRB72, RRRL72, RRRR72 ] )
        , ( ( FIFI72 , LLRF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( FIFI72 , LLRL72 )
          , Set.fromList
                [ RRLF72, RRLL72, RRLR72 ] )
        , ( ( FIFI72 , LLRR72 )
          , Set.fromList
                [ RRFR72, RRLR72, RRRR72 ] )
        , ( ( FIFI72 , LRIL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FIFI72 , LRLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FIFI72 , LRRI72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( FIFI72 , LRRL72 )
          , Set.fromList
                [ RLLI72, RLLL72, RLLR72 ] )
        , ( ( FIFI72 , LRRR72 )
          , Set.fromList
                [ RLIR72, RLLR72, RLRR72 ] )
        , ( ( FIFI72 , LSEL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FIFI72 , RBRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FIFI72 , RELE72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( FIFI72 , RFLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LSEL72 ] )
        , ( ( FIFI72 , RILR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72 ] )
        , ( ( FIFI72 , RLIR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FIFI72 , RLLI72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( FIFI72 , RLLL72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRL72 ] )
        , ( ( FIFI72 , RLLR72 )
          , Set.fromList
                [ LRRI72, LRRL72, LRRR72 ] )
        , ( ( FIFI72 , RLRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FIFI72 , RRBL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FIFI72 , RRFR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FIFI72 , RRLF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( FIFI72 , RRLL72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRL72 ] )
        , ( ( FIFI72 , RRLR72 )
          , Set.fromList
                [ LLRF72, LLRL72, LLRR72 ] )
        , ( ( FIFI72 , RRRB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( FIFI72 , RRRL72 )
          , Set.fromList
                [ LLLB72, LLLL72, LLLR72 ] )
        , ( ( FIFI72 , RRRR72 )
          , Set.fromList
                [ LLBR72, LLLR72, LLRR72 ] )
        , ( ( FIFI72 , RSER72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FIFI72 , SBSB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FIFI72 , SESE72 )
          , Set.fromList
                [ FIFI72 ] )
        , ( ( FIFI72 , SFSI72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( FIFI72 , SISF72 )
          , Set.fromList
                [ FEFE72, FFFF72, FIFI72 ] )
        , ( ( FIFI72 , SLSR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FIFI72 , SRSL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FLLL72 , BBBB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FLLL72 , BBFF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FLLL72 , BEIE72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FLLL72 , BFII72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FLLL72 , BIIF72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FLLL72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( FLLL72 , BRLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( FLLL72 , BSEF72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FLLL72 , EBIS72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FLLL72 , EFBS72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FLLL72 , EIFS72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FLLL72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( FLLL72 , ERRS72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72 ] )
        , ( ( FLLL72 , ESES72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FLLL72 , FBII72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FLLL72 , FEFE72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FLLL72 , FFBB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FLLL72 , FFFF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FLLL72 , FIFI72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FLLL72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( FLLL72 , FRRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72 ] )
        , ( ( FLLL72 , FSEI72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FLLL72 , IBIB72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FLLL72 , IEBE72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FLLL72 , IFBI72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FLLL72 , IIBF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FLLL72 , IIFB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FLLL72 , ILLR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( FLLL72 , IRRL72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72 ] )
        , ( ( FLLL72 , ISEB72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FLLL72 , LBLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( FLLL72 , LERE72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( FLLL72 , LFRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( FLLL72 , LIRL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( FLLL72 , LLBR72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, LLLL72, RRRR72
                , SBSB72 ] )
        , ( ( FLLL72 , LLFL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, LLRR72, RRLL72, SESE72, SFSI72
                , SISF72 ] )
        , ( ( FLLL72 , LLLB72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( FLLL72 , LLLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( FLLL72 , LLLR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( FLLL72 , LLRF72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( FLLL72 , LLRL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( FLLL72 , LLRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( FLLL72 , LRIL72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, LRRR72, RLLL72
                , SFSI72 ] )
        , ( ( FLLL72 , LRLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( FLLL72 , LRRI72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( FLLL72 , LRRL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( FLLL72 , LRRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( FLLL72 , LSEL72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, LFRR72, RFLL72
                , SFSI72 ] )
        , ( ( FLLL72 , RBRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( FLLL72 , RELE72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72 ] )
        , ( ( FLLL72 , RFLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72 ] )
        , ( ( FLLL72 , RILR72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72 ] )
        , ( ( FLLL72 , RLIR72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72, LRRR72
                , RLLL72 ] )
        , ( ( FLLL72 , RLLI72 )
          , Set.fromList
                [ FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLL72 ] )
        , ( ( FLLL72 , RLLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLL72 ] )
        , ( ( FLLL72 , RLLR72 )
          , Set.fromList
                [ FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLL72 ] )
        , ( ( FLLL72 , RLRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( FLLL72 , RRBL72 )
          , Set.fromList
                [ FFBB72, LLLL72, RRRR72 ] )
        , ( ( FLLL72 , RRFR72 )
          , Set.fromList
                [ FFFF72, LLRR72, RRLL72 ] )
        , ( ( FLLL72 , RRLF72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RFLL72
                , RLLL72, RRLL72 ] )
        , ( ( FLLL72 , RRLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RFLL72
                , RLLL72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( FLLL72 , RRLR72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RFLL72
                , RLLL72, RRLL72 ] )
        , ( ( FLLL72 , RRRB72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72, RRRR72 ] )
        , ( ( FLLL72 , RRRL72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72, RRRR72 ] )
        , ( ( FLLL72 , RRRR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( FLLL72 , RSER72 )
          , Set.fromList
                [ FFFF72, LFRR72, RFLL72 ] )
        , ( ( FLLL72 , SBSB72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FLLL72 , SESE72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FLLL72 , SFSI72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FLLL72 , SISF72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FLLL72 , SLSR72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72 ] )
        , ( ( FLLL72 , SRSL72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72 ] )
        , ( ( FRRR72 , BBBB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FRRR72 , BBFF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FRRR72 , BEIE72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FRRR72 , BFII72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FRRR72 , BIIF72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FRRR72 , BLRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72 ] )
        , ( ( FRRR72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( FRRR72 , BSEF72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FRRR72 , EBIS72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FRRR72 , EFBS72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FRRR72 , EIFS72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FRRR72 , ELLS72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( FRRR72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( FRRR72 , ESES72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FRRR72 , FBII72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FRRR72 , FEFE72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FRRR72 , FFBB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FRRR72 , FFFF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FRRR72 , FIFI72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FRRR72 , FLLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( FRRR72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( FRRR72 , FSEI72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FRRR72 , IBIB72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FRRR72 , IEBE72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FRRR72 , IFBI72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FRRR72 , IIBF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FRRR72 , IIFB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FRRR72 , ILLR72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( FRRR72 , IRRL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( FRRR72 , ISEB72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FRRR72 , LBLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72 ] )
        , ( ( FRRR72 , LERE72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( FRRR72 , LFRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( FRRR72 , LIRL72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( FRRR72 , LLBR72 )
          , Set.fromList
                [ FFBB72, LLLL72, RRRR72 ] )
        , ( ( FRRR72 , LLFL72 )
          , Set.fromList
                [ FFFF72, LLRR72, RRLL72 ] )
        , ( ( FRRR72 , LLLB72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( FRRR72 , LLLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RFLL72
                , RLLL72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( FRRR72 , LLLR72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( FRRR72 , LLRF72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( FRRR72 , LLRL72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( FRRR72 , LLRR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( FRRR72 , LRIL72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72, LRRR72
                , RLLL72 ] )
        , ( ( FRRR72 , LRLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLL72 ] )
        , ( ( FRRR72 , LRRI72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( FRRR72 , LRRL72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( FRRR72 , LRRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( FRRR72 , LSEL72 )
          , Set.fromList
                [ FFFF72, LFRR72, RFLL72 ] )
        , ( ( FRRR72 , RBRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( FRRR72 , RELE72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( FRRR72 , RFLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( FRRR72 , RILR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( FRRR72 , RLIR72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, LRRR72, RLLL72
                , SFSI72 ] )
        , ( ( FRRR72 , RLLI72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( FRRR72 , RLLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( FRRR72 , RLLR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( FRRR72 , RLRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( FRRR72 , RRBL72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, LLLL72, RRRR72
                , SBSB72 ] )
        , ( ( FRRR72 , RRFR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, LLRR72, RRLL72, SESE72, SFSI72
                , SISF72 ] )
        , ( ( FRRR72 , RRLF72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( FRRR72 , RRLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( FRRR72 , RRLR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( FRRR72 , RRRB72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( FRRR72 , RRRL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( FRRR72 , RRRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( FRRR72 , RSER72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, LFRR72, RFLL72
                , SFSI72 ] )
        , ( ( FRRR72 , SBSB72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FRRR72 , SESE72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FRRR72 , SFSI72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FRRR72 , SISF72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FRRR72 , SLSR72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72 ] )
        , ( ( FRRR72 , SRSL72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72 ] )
        , ( ( FSEI72 , BBBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FSEI72 , BBFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FSEI72 , BEIE72 )
          , Set.fromList
                [ FSEI72 ] )
        , ( ( FSEI72 , BFII72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( FSEI72 , BIIF72 )
          , Set.fromList
                [ FEFE72, FFFF72, FIFI72 ] )
        , ( ( FSEI72 , BLRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FSEI72 , BRLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( FSEI72 , BSEF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( FSEI72 , EBIS72 )
          , Set.fromList
                [ SFSI72 ] )
        , ( ( FSEI72 , EFBS72 )
          , Set.fromList
                [ SBSB72 ] )
        , ( ( FSEI72 , EIFS72 )
          , Set.fromList
                [ SESE72, SFSI72, SISF72 ] )
        , ( ( FSEI72 , ELLS72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( FSEI72 , ERRS72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( FSEI72 , ESES72 )
          , Set.fromList
                [ SFSI72 ] )
        , ( ( FSEI72 , FBII72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( FSEI72 , FEFE72 )
          , Set.fromList
                [ BSEF72 ] )
        , ( ( FSEI72 , FFBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( FSEI72 , FFFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( FSEI72 , FIFI72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( FSEI72 , FLLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( FSEI72 , FRRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( FSEI72 , FSEI72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( FSEI72 , IBIB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IFBI72 ] )
        , ( ( FSEI72 , IEBE72 )
          , Set.fromList
                [ ESES72, FSEI72, ISEB72 ] )
        , ( ( FSEI72 , IFBI72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( FSEI72 , IIBF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72 ] )
        , ( ( FSEI72 , IIFB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72 ] )
        , ( ( FSEI72 , ILLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72 ] )
        , ( ( FSEI72 , IRRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72 ] )
        , ( ( FSEI72 , ISEB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IFBI72 ] )
        , ( ( FSEI72 , LBLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FSEI72 , LERE72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( FSEI72 , LFRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( FSEI72 , LIRL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72 ] )
        , ( ( FSEI72 , LLBR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FSEI72 , LLFL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FSEI72 , LLLB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( FSEI72 , LLLL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( FSEI72 , LLLR72 )
          , Set.fromList
                [ RRRB72, RRRL72, RRRR72 ] )
        , ( ( FSEI72 , LLRF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( FSEI72 , LLRL72 )
          , Set.fromList
                [ RRLF72, RRLL72, RRLR72 ] )
        , ( ( FSEI72 , LLRR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( FSEI72 , LRIL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FSEI72 , LRLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( FSEI72 , LRRI72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( FSEI72 , LRRL72 )
          , Set.fromList
                [ RLLI72, RLLL72, RLLR72 ] )
        , ( ( FSEI72 , LRRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( FSEI72 , LSEL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( FSEI72 , RBRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FSEI72 , RELE72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( FSEI72 , RFLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( FSEI72 , RILR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72 ] )
        , ( ( FSEI72 , RLIR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FSEI72 , RLLI72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( FSEI72 , RLLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( FSEI72 , RLLR72 )
          , Set.fromList
                [ LRRI72, LRRL72, LRRR72 ] )
        , ( ( FSEI72 , RLRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( FSEI72 , RRBL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FSEI72 , RRFR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FSEI72 , RRLF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( FSEI72 , RRLL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( FSEI72 , RRLR72 )
          , Set.fromList
                [ LLRF72, LLRL72, LLRR72 ] )
        , ( ( FSEI72 , RRRB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( FSEI72 , RRRL72 )
          , Set.fromList
                [ LLLB72, LLLL72, LLLR72 ] )
        , ( ( FSEI72 , RRRR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( FSEI72 , RSER72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( FSEI72 , SBSB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( FSEI72 , SESE72 )
          , Set.fromList
                [ FSEI72 ] )
        , ( ( FSEI72 , SFSI72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( FSEI72 , SISF72 )
          , Set.fromList
                [ FEFE72, FFFF72, FIFI72 ] )
        , ( ( FSEI72 , SLSR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( FSEI72 , SRSL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( IBIB72 , BBBB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72 ] )
        , ( ( IBIB72 , BBFF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72 ] )
        , ( ( IBIB72 , BEIE72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( IBIB72 , BFII72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( IBIB72 , BIIF72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72, FBII72, FIFI72, FSEI72, IBIB72
                , IIFB72, ISEB72 ] )
        , ( ( IBIB72 , BLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72 ] )
        , ( ( IBIB72 , BRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72 ] )
        , ( ( IBIB72 , BSEF72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( IBIB72 , EBIS72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( IBIB72 , EFBS72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( IBIB72 , EIFS72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72 ] )
        , ( ( IBIB72 , ELLS72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( IBIB72 , ERRS72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( IBIB72 , ESES72 )
          , Set.fromList
                [ BIIF72 ] )
        , ( ( IBIB72 , FBII72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( IBIB72 , FEFE72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( IBIB72 , FFBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( IBIB72 , FFFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( IBIB72 , FIFI72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72 ] )
        , ( ( IBIB72 , FLLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( IBIB72 , FRRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( IBIB72 , FSEI72 )
          , Set.fromList
                [ BIIF72 ] )
        , ( ( IBIB72 , IBIB72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, IEBE72, IFBI72, IIBF72, SESE72
                , SFSI72, SISF72 ] )
        , ( ( IBIB72 , IEBE72 )
          , Set.fromList
                [ BBBB72, IBIB72, SBSB72 ] )
        , ( ( IBIB72 , IFBI72 )
          , Set.fromList
                [ BBBB72, IBIB72, SBSB72 ] )
        , ( ( IBIB72 , IIBF72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( IBIB72 , IIFB72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, SISF72 ] )
        , ( ( IBIB72 , ILLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, SRSL72 ] )
        , ( ( IBIB72 , IRRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, SLSR72 ] )
        , ( ( IBIB72 , ISEB72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( IBIB72 , LBLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72 ] )
        , ( ( IBIB72 , LERE72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( IBIB72 , LFRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( IBIB72 , LIRL72 )
          , Set.fromList
                [ RBRR72, RILR72, RSER72 ] )
        , ( ( IBIB72 , LLBR72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IBIB72 , LLFL72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IBIB72 , LLLB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( IBIB72 , LLLL72 )
          , Set.fromList
                [ RRLF72, RRLL72, RRLR72 ] )
        , ( ( IBIB72 , LLLR72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRL72 ] )
        , ( ( IBIB72 , LLRF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( IBIB72 , LLRL72 )
          , Set.fromList
                [ RRFR72, RRLR72, RRRR72 ] )
        , ( ( IBIB72 , LLRR72 )
          , Set.fromList
                [ RRRB72, RRRL72, RRRR72 ] )
        , ( ( IBIB72 , LRIL72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( IBIB72 , LRLL72 )
          , Set.fromList
                [ RLLI72, RLLL72, RLLR72 ] )
        , ( ( IBIB72 , LRRI72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( IBIB72 , LRRL72 )
          , Set.fromList
                [ RLIR72, RLLR72, RLRR72 ] )
        , ( ( IBIB72 , LRRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( IBIB72 , LSEL72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( IBIB72 , RBRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72 ] )
        , ( ( IBIB72 , RELE72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( IBIB72 , RFLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( IBIB72 , RILR72 )
          , Set.fromList
                [ LBLL72, LIRL72, LSEL72 ] )
        , ( ( IBIB72 , RLIR72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( IBIB72 , RLLI72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( IBIB72 , RLLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( IBIB72 , RLLR72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRL72 ] )
        , ( ( IBIB72 , RLRR72 )
          , Set.fromList
                [ LRRI72, LRRL72, LRRR72 ] )
        , ( ( IBIB72 , RRBL72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( IBIB72 , RRFR72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( IBIB72 , RRLF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( IBIB72 , RRLL72 )
          , Set.fromList
                [ LLLB72, LLLL72, LLLR72 ] )
        , ( ( IBIB72 , RRLR72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRL72 ] )
        , ( ( IBIB72 , RRRB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( IBIB72 , RRRL72 )
          , Set.fromList
                [ LLBR72, LLLR72, LLRR72 ] )
        , ( ( IBIB72 , RRRR72 )
          , Set.fromList
                [ LLRF72, LLRL72, LLRR72 ] )
        , ( ( IBIB72 , RSER72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( IBIB72 , SBSB72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( IBIB72 , SESE72 )
          , Set.fromList
                [ IBIB72 ] )
        , ( ( IBIB72 , SFSI72 )
          , Set.fromList
                [ IBIB72 ] )
        , ( ( IBIB72 , SISF72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( IBIB72 , SLSR72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( IBIB72 , SRSL72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( IEBE72 , BBBB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( IEBE72 , BBFF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, SISF72 ] )
        , ( ( IEBE72 , BEIE72 )
          , Set.fromList
                [ BEIE72, IEBE72, SESE72 ] )
        , ( ( IEBE72 , BFII72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( IEBE72 , BIIF72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( IEBE72 , BLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, SLSR72 ] )
        , ( ( IEBE72 , BRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, SRSL72 ] )
        , ( ( IEBE72 , BSEF72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( IEBE72 , EBIS72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72 ] )
        , ( ( IEBE72 , EFBS72 )
          , Set.fromList
                [ EFBS72 ] )
        , ( ( IEBE72 , EIFS72 )
          , Set.fromList
                [ EIFS72 ] )
        , ( ( IEBE72 , ELLS72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( IEBE72 , ERRS72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( IEBE72 , ESES72 )
          , Set.fromList
                [ EIFS72 ] )
        , ( ( IEBE72 , FBII72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( IEBE72 , FEFE72 )
          , Set.fromList
                [ FEFE72 ] )
        , ( ( IEBE72 , FFBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( IEBE72 , FFFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( IEBE72 , FIFI72 )
          , Set.fromList
                [ FIFI72 ] )
        , ( ( IEBE72 , FLLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( IEBE72 , FRRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( IEBE72 , FSEI72 )
          , Set.fromList
                [ FIFI72 ] )
        , ( ( IEBE72 , IBIB72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( IEBE72 , IEBE72 )
          , Set.fromList
                [ IEBE72 ] )
        , ( ( IEBE72 , IFBI72 )
          , Set.fromList
                [ IFBI72 ] )
        , ( ( IEBE72 , IIBF72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( IEBE72 , IIFB72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( IEBE72 , ILLR72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( IEBE72 , IRRL72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( IEBE72 , ISEB72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( IEBE72 , LBLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LSEL72 ] )
        , ( ( IEBE72 , LERE72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( IEBE72 , LFRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( IEBE72 , LIRL72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( IEBE72 , LLBR72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( IEBE72 , LLFL72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( IEBE72 , LLLB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( IEBE72 , LLLL72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRL72 ] )
        , ( ( IEBE72 , LLLR72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( IEBE72 , LLRF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( IEBE72 , LLRL72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( IEBE72 , LLRR72 )
          , Set.fromList
                [ LLBR72, LLLR72, LLRR72 ] )
        , ( ( IEBE72 , LRIL72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( IEBE72 , LRLL72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRL72 ] )
        , ( ( IEBE72 , LRRI72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( IEBE72 , LRRL72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( IEBE72 , LRRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( IEBE72 , LSEL72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( IEBE72 , RBRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RSER72 ] )
        , ( ( IEBE72 , RELE72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( IEBE72 , RFLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( IEBE72 , RILR72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( IEBE72 , RLIR72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( IEBE72 , RLLI72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( IEBE72 , RLLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( IEBE72 , RLLR72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( IEBE72 , RLRR72 )
          , Set.fromList
                [ RLIR72, RLLR72, RLRR72 ] )
        , ( ( IEBE72 , RRBL72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IEBE72 , RRFR72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IEBE72 , RRLF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( IEBE72 , RRLL72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRL72 ] )
        , ( ( IEBE72 , RRLR72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IEBE72 , RRRB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( IEBE72 , RRRL72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IEBE72 , RRRR72 )
          , Set.fromList
                [ RRFR72, RRLR72, RRRR72 ] )
        , ( ( IEBE72 , RSER72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( IEBE72 , SBSB72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( IEBE72 , SESE72 )
          , Set.fromList
                [ IEBE72 ] )
        , ( ( IEBE72 , SFSI72 )
          , Set.fromList
                [ IFBI72 ] )
        , ( ( IEBE72 , SISF72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( IEBE72 , SLSR72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( IEBE72 , SRSL72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( IFBI72 , BBBB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( IFBI72 , BBFF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, SISF72 ] )
        , ( ( IFBI72 , BEIE72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( IFBI72 , BFII72 )
          , Set.fromList
                [ BFII72, IFBI72, SFSI72 ] )
        , ( ( IFBI72 , BIIF72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, IEBE72, IFBI72, IIBF72, SESE72
                , SFSI72, SISF72 ] )
        , ( ( IFBI72 , BLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, SLSR72 ] )
        , ( ( IFBI72 , BRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, SRSL72 ] )
        , ( ( IFBI72 , BSEF72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( IFBI72 , EBIS72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( IFBI72 , EFBS72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( IFBI72 , EIFS72 )
          , Set.fromList
                [ FEFE72, FFFF72, FIFI72 ] )
        , ( ( IFBI72 , ELLS72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( IFBI72 , ERRS72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( IFBI72 , ESES72 )
          , Set.fromList
                [ FIFI72 ] )
        , ( ( IFBI72 , FBII72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72 ] )
        , ( ( IFBI72 , FEFE72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( IFBI72 , FFBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( IFBI72 , FFFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( IFBI72 , FIFI72 )
          , Set.fromList
                [ FEFE72, FFFF72, FIFI72 ] )
        , ( ( IFBI72 , FLLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( IFBI72 , FRRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( IFBI72 , FSEI72 )
          , Set.fromList
                [ FIFI72 ] )
        , ( ( IFBI72 , IBIB72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72, FBII72, FIFI72, FSEI72, IBIB72
                , IIFB72, ISEB72 ] )
        , ( ( IFBI72 , IEBE72 )
          , Set.fromList
                [ EFBS72, FFBB72, IFBI72 ] )
        , ( ( IFBI72 , IFBI72 )
          , Set.fromList
                [ EFBS72, FFBB72, IFBI72 ] )
        , ( ( IFBI72 , IIBF72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72 ] )
        , ( ( IFBI72 , IIFB72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72 ] )
        , ( ( IFBI72 , ILLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72 ] )
        , ( ( IFBI72 , IRRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72 ] )
        , ( ( IFBI72 , ISEB72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( IFBI72 , LBLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LSEL72 ] )
        , ( ( IFBI72 , LERE72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( IFBI72 , LFRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( IFBI72 , LIRL72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72 ] )
        , ( ( IFBI72 , LLBR72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( IFBI72 , LLFL72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( IFBI72 , LLLB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( IFBI72 , LLLL72 )
          , Set.fromList
                [ LLFL72, LLLL72, LLRL72 ] )
        , ( ( IFBI72 , LLLR72 )
          , Set.fromList
                [ LLLB72, LLLL72, LLLR72 ] )
        , ( ( IFBI72 , LLRF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( IFBI72 , LLRL72 )
          , Set.fromList
                [ LLRF72, LLRL72, LLRR72 ] )
        , ( ( IFBI72 , LLRR72 )
          , Set.fromList
                [ LLBR72, LLLR72, LLRR72 ] )
        , ( ( IFBI72 , LRIL72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( IFBI72 , LRLL72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRL72 ] )
        , ( ( IFBI72 , LRRI72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( IFBI72 , LRRL72 )
          , Set.fromList
                [ LRRI72, LRRL72, LRRR72 ] )
        , ( ( IFBI72 , LRRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( IFBI72 , LSEL72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( IFBI72 , RBRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RSER72 ] )
        , ( ( IFBI72 , RELE72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( IFBI72 , RFLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( IFBI72 , RILR72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72 ] )
        , ( ( IFBI72 , RLIR72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( IFBI72 , RLLI72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( IFBI72 , RLLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( IFBI72 , RLLR72 )
          , Set.fromList
                [ RLLI72, RLLL72, RLLR72 ] )
        , ( ( IFBI72 , RLRR72 )
          , Set.fromList
                [ RLIR72, RLLR72, RLRR72 ] )
        , ( ( IFBI72 , RRBL72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IFBI72 , RRFR72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IFBI72 , RRLF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( IFBI72 , RRLL72 )
          , Set.fromList
                [ RRBL72, RRLL72, RRRL72 ] )
        , ( ( IFBI72 , RRLR72 )
          , Set.fromList
                [ RRLF72, RRLL72, RRLR72 ] )
        , ( ( IFBI72 , RRRB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( IFBI72 , RRRL72 )
          , Set.fromList
                [ RRRB72, RRRL72, RRRR72 ] )
        , ( ( IFBI72 , RRRR72 )
          , Set.fromList
                [ RRFR72, RRLR72, RRRR72 ] )
        , ( ( IFBI72 , RSER72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( IFBI72 , SBSB72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( IFBI72 , SESE72 )
          , Set.fromList
                [ IFBI72 ] )
        , ( ( IFBI72 , SFSI72 )
          , Set.fromList
                [ IFBI72 ] )
        , ( ( IFBI72 , SISF72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( IFBI72 , SLSR72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( IFBI72 , SRSL72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( IIBF72 , BBBB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( IIBF72 , BBFF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, SISF72 ] )
        , ( ( IIBF72 , BEIE72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( IIBF72 , BFII72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, IEBE72, IFBI72, IIBF72, SESE72
                , SFSI72, SISF72 ] )
        , ( ( IIBF72 , BIIF72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( IIBF72 , BLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, SLSR72 ] )
        , ( ( IIBF72 , BRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, SRSL72 ] )
        , ( ( IIBF72 , BSEF72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( IIBF72 , EBIS72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( IIBF72 , EFBS72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( IIBF72 , EIFS72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( IIBF72 , ELLS72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( IIBF72 , ERRS72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( IIBF72 , ESES72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( IIBF72 , FBII72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72, FBII72, FIFI72, FSEI72, IBIB72
                , IIFB72, ISEB72 ] )
        , ( ( IIBF72 , FEFE72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( IIBF72 , FFBB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72 ] )
        , ( ( IIBF72 , FFFF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72 ] )
        , ( ( IIBF72 , FIFI72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( IIBF72 , FLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72 ] )
        , ( ( IIBF72 , FRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72 ] )
        , ( ( IIBF72 , FSEI72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( IIBF72 , IBIB72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( IIBF72 , IEBE72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( IIBF72 , IFBI72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( IIBF72 , IIBF72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( IIBF72 , IIFB72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( IIBF72 , ILLR72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( IIBF72 , IRRL72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( IIBF72 , ISEB72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( IIBF72 , LBLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LSEL72 ] )
        , ( ( IIBF72 , LERE72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( IIBF72 , LFRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72 ] )
        , ( ( IIBF72 , LIRL72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( IIBF72 , LLBR72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( IIBF72 , LLFL72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( IIBF72 , LLLB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( IIBF72 , LLLL72 )
          , Set.fromList
                [ LLFL72, LLLB72, LLLL72, LLLR72, LLRL72 ] )
        , ( ( IIBF72 , LLLR72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( IIBF72 , LLRF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( IIBF72 , LLRL72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( IIBF72 , LLRR72 )
          , Set.fromList
                [ LLBR72, LLLR72, LLRF72, LLRL72, LLRR72 ] )
        , ( ( IIBF72 , LRIL72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( IIBF72 , LRLL72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRL72 ] )
        , ( ( IIBF72 , LRRI72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( IIBF72 , LRRL72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( IIBF72 , LRRR72 )
          , Set.fromList
                [ LRRI72, LRRL72, LRRR72 ] )
        , ( ( IIBF72 , LSEL72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( IIBF72 , RBRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RSER72 ] )
        , ( ( IIBF72 , RELE72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( IIBF72 , RFLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72 ] )
        , ( ( IIBF72 , RILR72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( IIBF72 , RLIR72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( IIBF72 , RLLI72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( IIBF72 , RLLL72 )
          , Set.fromList
                [ RLLI72, RLLL72, RLLR72 ] )
        , ( ( IIBF72 , RLLR72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( IIBF72 , RLRR72 )
          , Set.fromList
                [ RLIR72, RLLR72, RLRR72 ] )
        , ( ( IIBF72 , RRBL72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IIBF72 , RRFR72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IIBF72 , RRLF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IIBF72 , RRLL72 )
          , Set.fromList
                [ RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( IIBF72 , RRLR72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IIBF72 , RRRB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IIBF72 , RRRL72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IIBF72 , RRRR72 )
          , Set.fromList
                [ RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( IIBF72 , RSER72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( IIBF72 , SBSB72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( IIBF72 , SESE72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( IIBF72 , SFSI72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( IIBF72 , SISF72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( IIBF72 , SLSR72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( IIBF72 , SRSL72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( IIFB72 , BBBB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72 ] )
        , ( ( IIFB72 , BBFF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72 ] )
        , ( ( IIFB72 , BEIE72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( IIFB72 , BFII72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72, FBII72, FIFI72, FSEI72, IBIB72
                , IIFB72, ISEB72 ] )
        , ( ( IIFB72 , BIIF72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( IIFB72 , BLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72 ] )
        , ( ( IIFB72 , BRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72 ] )
        , ( ( IIFB72 , BSEF72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( IIFB72 , EBIS72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( IIFB72 , EFBS72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( IIFB72 , EIFS72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( IIFB72 , ELLS72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( IIFB72 , ERRS72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( IIFB72 , ESES72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( IIFB72 , FBII72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, IEBE72, IFBI72, IIBF72, SESE72
                , SFSI72, SISF72 ] )
        , ( ( IIFB72 , FEFE72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( IIFB72 , FFBB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, SBSB72 ] )
        , ( ( IIFB72 , FFFF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, SISF72 ] )
        , ( ( IIFB72 , FIFI72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( IIFB72 , FLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, SRSL72 ] )
        , ( ( IIFB72 , FRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, SLSR72 ] )
        , ( ( IIFB72 , FSEI72 )
          , Set.fromList
                [ BIIF72, IIBF72, SISF72 ] )
        , ( ( IIFB72 , IBIB72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( IIFB72 , IEBE72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( IIFB72 , IFBI72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( IIFB72 , IIBF72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( IIFB72 , IIFB72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( IIFB72 , ILLR72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( IIFB72 , IRRL72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( IIFB72 , ISEB72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( IIFB72 , LBLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72 ] )
        , ( ( IIFB72 , LERE72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( IIFB72 , LFRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RSER72 ] )
        , ( ( IIFB72 , LIRL72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( IIFB72 , LLBR72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IIFB72 , LLFL72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IIFB72 , LLLB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IIFB72 , LLLL72 )
          , Set.fromList
                [ RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( IIFB72 , LLLR72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IIFB72 , LLRF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IIFB72 , LLRL72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IIFB72 , LLRR72 )
          , Set.fromList
                [ RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( IIFB72 , LRIL72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( IIFB72 , LRLL72 )
          , Set.fromList
                [ RLLI72, RLLL72, RLLR72 ] )
        , ( ( IIFB72 , LRRI72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( IIFB72 , LRRL72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( IIFB72 , LRRR72 )
          , Set.fromList
                [ RLIR72, RLLR72, RLRR72 ] )
        , ( ( IIFB72 , LSEL72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( IIFB72 , RBRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72 ] )
        , ( ( IIFB72 , RELE72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( IIFB72 , RFLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LSEL72 ] )
        , ( ( IIFB72 , RILR72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( IIFB72 , RLIR72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( IIFB72 , RLLI72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( IIFB72 , RLLL72 )
          , Set.fromList
                [ LRIL72, LRLL72, LRRL72 ] )
        , ( ( IIFB72 , RLLR72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( IIFB72 , RLRR72 )
          , Set.fromList
                [ LRRI72, LRRL72, LRRR72 ] )
        , ( ( IIFB72 , RRBL72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( IIFB72 , RRFR72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( IIFB72 , RRLF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( IIFB72 , RRLL72 )
          , Set.fromList
                [ LLFL72, LLLB72, LLLL72, LLLR72, LLRL72 ] )
        , ( ( IIFB72 , RRLR72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( IIFB72 , RRRB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( IIFB72 , RRRL72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( IIFB72 , RRRR72 )
          , Set.fromList
                [ LLBR72, LLLR72, LLRF72, LLRL72, LLRR72 ] )
        , ( ( IIFB72 , RSER72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( IIFB72 , SBSB72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( IIFB72 , SESE72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( IIFB72 , SFSI72 )
          , Set.fromList
                [ IBIB72, IIFB72, ISEB72 ] )
        , ( ( IIFB72 , SISF72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( IIFB72 , SLSR72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( IIFB72 , SRSL72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( ILLR72 , BBBB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( ILLR72 , BBFF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( ILLR72 , BEIE72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( ILLR72 , BFII72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( ILLR72 , BIIF72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( ILLR72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( ILLR72 , BRLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( ILLR72 , BSEF72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( ILLR72 , EBIS72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( ILLR72 , EFBS72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( ILLR72 , EIFS72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( ILLR72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( ILLR72 , ERRS72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( ILLR72 , ESES72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( ILLR72 , FBII72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( ILLR72 , FEFE72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( ILLR72 , FFBB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( ILLR72 , FFFF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( ILLR72 , FIFI72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( ILLR72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( ILLR72 , FRRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( ILLR72 , FSEI72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( ILLR72 , IBIB72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( ILLR72 , IEBE72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( ILLR72 , IFBI72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( ILLR72 , IIBF72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( ILLR72 , IIFB72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( ILLR72 , ILLR72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( ILLR72 , IRRL72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( ILLR72 , ISEB72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( ILLR72 , LBLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( ILLR72 , LERE72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( ILLR72 , LFRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( ILLR72 , LIRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( ILLR72 , LLBR72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, LLLR72, RRRL72
                , SBSB72 ] )
        , ( ( ILLR72 , LLFL72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, LLRL72, RRLR72
                , SISF72 ] )
        , ( ( ILLR72 , LLLB72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLL72, RRRL72, SRSL72 ] )
        , ( ( ILLR72 , LLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( ILLR72 , LLLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLL72, RRRL72, SRSL72 ] )
        , ( ( ILLR72 , LLRF72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( ILLR72 , LLRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( ILLR72 , LLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72, SLSR72 ] )
        , ( ( ILLR72 , LRIL72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, IEBE72, IFBI72, IIBF72, LRRL72
                , RLLR72, SESE72, SFSI72, SISF72 ] )
        , ( ( ILLR72 , LRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( ILLR72 , LRRI72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( ILLR72 , LRRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( ILLR72 , LRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( ILLR72 , LSEL72 )
          , Set.fromList
                [ BIIF72, IIBF72, LIRL72, RILR72, SISF72 ] )
        , ( ( ILLR72 , RBRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( ILLR72 , RELE72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( ILLR72 , RFLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( ILLR72 , RILR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( ILLR72 , RLIR72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72, FBII72, FIFI72, FSEI72, IBIB72
                , IIFB72, ISEB72, LRRL72, RLLR72 ] )
        , ( ( ILLR72 , RLLI72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( ILLR72 , RLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( ILLR72 , RLLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( ILLR72 , RLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( ILLR72 , RRBL72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72, LLLR72
                , RRRL72 ] )
        , ( ( ILLR72 , RRFR72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72, LLRL72
                , RRLR72 ] )
        , ( ( ILLR72 , RRLF72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRLF72, RRLL72, RRLR72 ] )
        , ( ( ILLR72 , RRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( ILLR72 , RRLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRLF72, RRLL72, RRLR72 ] )
        , ( ( ILLR72 , RRRB72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( ILLR72 , RRRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( ILLR72 , RRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( ILLR72 , RSER72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72, LIRL72, RILR72 ] )
        , ( ( ILLR72 , SBSB72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( ILLR72 , SESE72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( ILLR72 , SFSI72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( ILLR72 , SISF72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( ILLR72 , SLSR72 )
          , Set.fromList
                [ IBIB72, IIFB72, ILLR72, IRRL72, ISEB72 ] )
        , ( ( ILLR72 , SRSL72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72, ILLR72, IRRL72 ] )
        , ( ( IRRL72 , BBBB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( IRRL72 , BBFF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( IRRL72 , BEIE72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( IRRL72 , BFII72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( IRRL72 , BIIF72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( IRRL72 , BLRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( IRRL72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( IRRL72 , BSEF72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( IRRL72 , EBIS72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( IRRL72 , EFBS72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IRRL72 , EIFS72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IRRL72 , ELLS72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( IRRL72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( IRRL72 , ESES72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( IRRL72 , FBII72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( IRRL72 , FEFE72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IRRL72 , FFBB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IRRL72 , FFFF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IRRL72 , FIFI72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IRRL72 , FLLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( IRRL72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( IRRL72 , FSEI72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( IRRL72 , IBIB72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( IRRL72 , IEBE72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IRRL72 , IFBI72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IRRL72 , IIBF72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( IRRL72 , IIFB72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( IRRL72 , ILLR72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( IRRL72 , IRRL72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( IRRL72 , ISEB72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( IRRL72 , LBLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( IRRL72 , LERE72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( IRRL72 , LFRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( IRRL72 , LIRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( IRRL72 , LLBR72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72, LLLR72
                , RRRL72 ] )
        , ( ( IRRL72 , LLFL72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72, LLRL72
                , RRLR72 ] )
        , ( ( IRRL72 , LLLB72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72 ] )
        , ( ( IRRL72 , LLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( IRRL72 , LLLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72 ] )
        , ( ( IRRL72 , LLRF72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( IRRL72 , LLRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( IRRL72 , LLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( IRRL72 , LRIL72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72, FBII72, FIFI72, FSEI72, IBIB72
                , IIFB72, ISEB72, LRRL72, RLLR72 ] )
        , ( ( IRRL72 , LRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( IRRL72 , LRRI72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( IRRL72 , LRRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( IRRL72 , LRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( IRRL72 , LSEL72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72, LIRL72, RILR72 ] )
        , ( ( IRRL72 , RBRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( IRRL72 , RELE72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( IRRL72 , RFLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( IRRL72 , RILR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( IRRL72 , RLIR72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, IEBE72, IFBI72, IIBF72, LRRL72
                , RLLR72, SESE72, SFSI72, SISF72 ] )
        , ( ( IRRL72 , RLLI72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( IRRL72 , RLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( IRRL72 , RLLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( IRRL72 , RLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( IRRL72 , RRBL72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, LLLR72, RRRL72
                , SBSB72 ] )
        , ( ( IRRL72 , RRFR72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, LLRL72, RRLR72
                , SISF72 ] )
        , ( ( IRRL72 , RRLF72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( IRRL72 , RRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( IRRL72 , RRLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( IRRL72 , RRRB72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72, RRRL72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( IRRL72 , RRRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72, RRRL72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( IRRL72 , RRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72, SLSR72 ] )
        , ( ( IRRL72 , RSER72 )
          , Set.fromList
                [ BIIF72, IIBF72, LIRL72, RILR72, SISF72 ] )
        , ( ( IRRL72 , SBSB72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( IRRL72 , SESE72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( IRRL72 , SFSI72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( IRRL72 , SISF72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( IRRL72 , SLSR72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72, ILLR72, IRRL72 ] )
        , ( ( IRRL72 , SRSL72 )
          , Set.fromList
                [ IBIB72, IIFB72, ILLR72, IRRL72, ISEB72 ] )
        , ( ( ISEB72 , BBBB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72 ] )
        , ( ( ISEB72 , BBFF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72 ] )
        , ( ( ISEB72 , BEIE72 )
          , Set.fromList
                [ ESES72, FSEI72, ISEB72 ] )
        , ( ( ISEB72 , BFII72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( ISEB72 , BIIF72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( ISEB72 , BLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72 ] )
        , ( ( ISEB72 , BRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72 ] )
        , ( ( ISEB72 , BSEF72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( ISEB72 , EBIS72 )
          , Set.fromList
                [ SESE72, SFSI72, SISF72 ] )
        , ( ( ISEB72 , EFBS72 )
          , Set.fromList
                [ SBSB72 ] )
        , ( ( ISEB72 , EIFS72 )
          , Set.fromList
                [ SISF72 ] )
        , ( ( ISEB72 , ELLS72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( ISEB72 , ERRS72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( ISEB72 , ESES72 )
          , Set.fromList
                [ SISF72 ] )
        , ( ( ISEB72 , FBII72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( ISEB72 , FEFE72 )
          , Set.fromList
                [ BSEF72 ] )
        , ( ( ISEB72 , FFBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( ISEB72 , FFFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( ISEB72 , FIFI72 )
          , Set.fromList
                [ BIIF72 ] )
        , ( ( ISEB72 , FLLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( ISEB72 , FRRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( ISEB72 , FSEI72 )
          , Set.fromList
                [ BIIF72 ] )
        , ( ( ISEB72 , IBIB72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( ISEB72 , IEBE72 )
          , Set.fromList
                [ ISEB72 ] )
        , ( ( ISEB72 , IFBI72 )
          , Set.fromList
                [ IBIB72 ] )
        , ( ( ISEB72 , IIBF72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( ISEB72 , IIFB72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( ISEB72 , ILLR72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( ISEB72 , IRRL72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( ISEB72 , ISEB72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( ISEB72 , LBLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72 ] )
        , ( ( ISEB72 , LERE72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( ISEB72 , LFRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( ISEB72 , LIRL72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( ISEB72 , LLBR72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( ISEB72 , LLFL72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( ISEB72 , LLLB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( ISEB72 , LLLL72 )
          , Set.fromList
                [ RRLF72, RRLL72, RRLR72 ] )
        , ( ( ISEB72 , LLLR72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( ISEB72 , LLRF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( ISEB72 , LLRL72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( ISEB72 , LLRR72 )
          , Set.fromList
                [ RRRB72, RRRL72, RRRR72 ] )
        , ( ( ISEB72 , LRIL72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( ISEB72 , LRLL72 )
          , Set.fromList
                [ RLLI72, RLLL72, RLLR72 ] )
        , ( ( ISEB72 , LRRI72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( ISEB72 , LRRL72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( ISEB72 , LRRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( ISEB72 , LSEL72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( ISEB72 , RBRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72 ] )
        , ( ( ISEB72 , RELE72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( ISEB72 , RFLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( ISEB72 , RILR72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( ISEB72 , RLIR72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( ISEB72 , RLLI72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( ISEB72 , RLLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( ISEB72 , RLLR72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( ISEB72 , RLRR72 )
          , Set.fromList
                [ LRRI72, LRRL72, LRRR72 ] )
        , ( ( ISEB72 , RRBL72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( ISEB72 , RRFR72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( ISEB72 , RRLF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( ISEB72 , RRLL72 )
          , Set.fromList
                [ LLLB72, LLLL72, LLLR72 ] )
        , ( ( ISEB72 , RRLR72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( ISEB72 , RRRB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( ISEB72 , RRRL72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( ISEB72 , RRRR72 )
          , Set.fromList
                [ LLRF72, LLRL72, LLRR72 ] )
        , ( ( ISEB72 , RSER72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( ISEB72 , SBSB72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( ISEB72 , SESE72 )
          , Set.fromList
                [ ISEB72 ] )
        , ( ( ISEB72 , SFSI72 )
          , Set.fromList
                [ IBIB72 ] )
        , ( ( ISEB72 , SISF72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( ISEB72 , SLSR72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( ISEB72 , SRSL72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( LBLL72 , BBBB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LBLL72 , BBFF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LBLL72 , BEIE72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( LBLL72 , BFII72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( LBLL72 , BIIF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LBLL72 , BLRR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LBLL72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72 ] )
        , ( ( LBLL72 , BSEF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LBLL72 , EBIS72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( LBLL72 , EFBS72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( LBLL72 , EIFS72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( LBLL72 , ELLS72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72
                , BSEF72 ] )
        , ( ( LBLL72 , ERRS72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72 ] )
        , ( ( LBLL72 , ESES72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( LBLL72 , FBII72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( LBLL72 , FEFE72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( LBLL72 , FFBB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( LBLL72 , FFFF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( LBLL72 , FIFI72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( LBLL72 , FLLL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( LBLL72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LBLL72 , FSEI72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( LBLL72 , IBIB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LBLL72 , IEBE72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( LBLL72 , IFBI72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( LBLL72 , IIBF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LBLL72 , IIFB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LBLL72 , ILLR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LBLL72 , IRRL72 )
          , Set.fromList
                [ LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72 ] )
        , ( ( LBLL72 , ISEB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LBLL72 , LBLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LBLL72 , LERE72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, LBLL72, RBRR72
                , SBSB72 ] )
        , ( ( LBLL72 , LFRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( LBLL72 , LIRL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LBLL72 , LLBR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LBLL72 , LLFL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LBLL72 , LLLB72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, LLRR72, RRLL72, SESE72, SFSI72
                , SISF72 ] )
        , ( ( LBLL72 , LLLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LBLL72 , LLLR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LBLL72 , LLRF72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, LLLL72, RRRR72
                , SBSB72 ] )
        , ( ( LBLL72 , LLRL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LBLL72 , LLRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LBLL72 , LRIL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LBLL72 , LRLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LBLL72 , LRRI72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, LRLL72, RLRR72
                , SBSB72 ] )
        , ( ( LBLL72 , LRRL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LBLL72 , LRRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( LBLL72 , LSEL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LBLL72 , RBRR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLRR72 ] )
        , ( ( LBLL72 , RELE72 )
          , Set.fromList
                [ BBFF72, LBLL72, RBRR72 ] )
        , ( ( LBLL72 , RFLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( LBLL72 , RILR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLRR72 ] )
        , ( ( LBLL72 , RLIR72 )
          , Set.fromList
                [ BLRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLRR72 ] )
        , ( ( LBLL72 , RLLI72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, LRLL72
                , RLRR72 ] )
        , ( ( LBLL72 , RLLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( LBLL72 , RLLR72 )
          , Set.fromList
                [ BLRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLRR72 ] )
        , ( ( LBLL72 , RLRR72 )
          , Set.fromList
                [ BLRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLRR72 ] )
        , ( ( LBLL72 , RRBL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, RRLL72 ] )
        , ( ( LBLL72 , RRFR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RBRR72
                , RLRR72, RRRR72 ] )
        , ( ( LBLL72 , RRLF72 )
          , Set.fromList
                [ BBFF72, LLLL72, RRRR72 ] )
        , ( ( LBLL72 , RRLL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( LBLL72 , RRLR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RBRR72
                , RLRR72, RRRR72 ] )
        , ( ( LBLL72 , RRRB72 )
          , Set.fromList
                [ BBBB72, LLRR72, RRLL72 ] )
        , ( ( LBLL72 , RRRL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, RRLL72 ] )
        , ( ( LBLL72 , RRRR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RBRR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LBLL72 , RSER72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLRR72 ] )
        , ( ( LBLL72 , SBSB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LBLL72 , SESE72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( LBLL72 , SFSI72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( LBLL72 , SISF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LBLL72 , SLSR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LBLL72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72 ] )
        , ( ( LERE72 , BBBB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LERE72 , BBFF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LERE72 , BEIE72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( LERE72 , BFII72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( LERE72 , BIIF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LERE72 , BLRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( LERE72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( LERE72 , BSEF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LERE72 , EBIS72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( LERE72 , EFBS72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( LERE72 , EIFS72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( LERE72 , ELLS72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72 ] )
        , ( ( LERE72 , ERRS72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72 ] )
        , ( ( LERE72 , ESES72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( LERE72 , FBII72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( LERE72 , FEFE72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( LERE72 , FFBB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( LERE72 , FFFF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( LERE72 , FIFI72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( LERE72 , FLLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( LERE72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LERE72 , FSEI72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( LERE72 , IBIB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LERE72 , IEBE72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( LERE72 , IFBI72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( LERE72 , IIBF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LERE72 , IIFB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LERE72 , ILLR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( LERE72 , IRRL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( LERE72 , ISEB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LERE72 , LBLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( LERE72 , LERE72 )
          , Set.fromList
                [ FEFE72, LERE72, RELE72 ] )
        , ( ( LERE72 , LFRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( LERE72 , LIRL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( LERE72 , LLBR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RRRR72 ] )
        , ( ( LERE72 , LLFL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72
                , RRLL72 ] )
        , ( ( LERE72 , LLLB72 )
          , Set.fromList
                [ FFBB72, LLLB72, RRRB72 ] )
        , ( ( LERE72 , LLLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72, RRBL72
                , RRLL72, RRRL72 ] )
        , ( ( LERE72 , LLLR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RRRR72 ] )
        , ( ( LERE72 , LLRF72 )
          , Set.fromList
                [ FFFF72, LLRF72, RRLF72 ] )
        , ( ( LERE72 , LLRL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72
                , RRLL72 ] )
        , ( ( LERE72 , LLRR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72, RRFR72
                , RRLR72, RRRR72 ] )
        , ( ( LERE72 , LRIL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( LERE72 , LRLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( LERE72 , LRRI72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72, LRRI72, RLLI72 ] )
        , ( ( LERE72 , LRRL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( LERE72 , LRRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( LERE72 , LSEL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( LERE72 , RBRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( LERE72 , RELE72 )
          , Set.fromList
                [ BEIE72, IEBE72, LERE72, RELE72, SESE72 ] )
        , ( ( LERE72 , RFLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( LERE72 , RILR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( LERE72 , RLIR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LERE72 , RLLI72 )
          , Set.fromList
                [ BFII72, IFBI72, LRRI72, RLLI72, SFSI72 ] )
        , ( ( LERE72 , RLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( LERE72 , RLLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LERE72 , RLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LERE72 , RRBL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( LERE72 , RRFR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( LERE72 , RRLF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, LLRF72, RRLF72
                , SISF72 ] )
        , ( ( LERE72 , RRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( LERE72 , RRLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( LERE72 , RRRB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, LLLB72, RRRB72
                , SBSB72 ] )
        , ( ( LERE72 , RRRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( LERE72 , RRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( LERE72 , RSER72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( LERE72 , SBSB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LERE72 , SESE72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( LERE72 , SFSI72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( LERE72 , SISF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LERE72 , SLSR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( LERE72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( LFRR72 , BBBB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LFRR72 , BBFF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LFRR72 , BEIE72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( LFRR72 , BFII72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( LFRR72 , BIIF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LFRR72 , BLRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72 ] )
        , ( ( LFRR72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LFRR72 , BSEF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LFRR72 , EBIS72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( LFRR72 , EFBS72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( LFRR72 , EIFS72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( LFRR72 , ELLS72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72 ] )
        , ( ( LFRR72 , ERRS72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72 ] )
        , ( ( LFRR72 , ESES72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( LFRR72 , FBII72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( LFRR72 , FEFE72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( LFRR72 , FFBB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( LFRR72 , FFFF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( LFRR72 , FIFI72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( LFRR72 , FLLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LFRR72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( LFRR72 , FSEI72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( LFRR72 , IBIB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LFRR72 , IEBE72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( LFRR72 , IFBI72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( LFRR72 , IIBF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LFRR72 , IIFB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LFRR72 , ILLR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72 ] )
        , ( ( LFRR72 , IRRL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LFRR72 , ISEB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LFRR72 , LBLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72 ] )
        , ( ( LFRR72 , LERE72 )
          , Set.fromList
                [ FFFF72, LFRR72, RFLL72 ] )
        , ( ( LFRR72 , LFRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( LFRR72 , LIRL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72 ] )
        , ( ( LFRR72 , LLBR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72, RRRR72 ] )
        , ( ( LFRR72 , LLFL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RFLL72
                , RLLL72, RRLL72 ] )
        , ( ( LFRR72 , LLLB72 )
          , Set.fromList
                [ FFBB72, LLLL72, RRRR72 ] )
        , ( ( LFRR72 , LLLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RFLL72
                , RLLL72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LFRR72 , LLLR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72, RRRR72 ] )
        , ( ( LFRR72 , LLRF72 )
          , Set.fromList
                [ FFFF72, LLRR72, RRLL72 ] )
        , ( ( LFRR72 , LLRL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RFLL72
                , RLLL72, RRLL72 ] )
        , ( ( LFRR72 , LLRR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( LFRR72 , LRIL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLL72 ] )
        , ( ( LFRR72 , LRLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLL72 ] )
        , ( ( LFRR72 , LRRI72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72, LRRR72
                , RLLL72 ] )
        , ( ( LFRR72 , LRRL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLL72 ] )
        , ( ( LFRR72 , LRRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( LFRR72 , LSEL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72 ] )
        , ( ( LFRR72 , RBRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LFRR72 , RELE72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, LFRR72, RFLL72
                , SFSI72 ] )
        , ( ( LFRR72 , RFLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( LFRR72 , RILR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LFRR72 , RLIR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LFRR72 , RLLI72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, LRRR72, RLLL72
                , SFSI72 ] )
        , ( ( LFRR72 , RLLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( LFRR72 , RLLR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LFRR72 , RLRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LFRR72 , RRBL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LFRR72 , RRFR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LFRR72 , RRLF72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, LLRR72, RRLL72, SESE72, SFSI72
                , SISF72 ] )
        , ( ( LFRR72 , RRLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LFRR72 , RRLR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LFRR72 , RRRB72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, LLLL72, RRRR72
                , SBSB72 ] )
        , ( ( LFRR72 , RRRL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LFRR72 , RRRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LFRR72 , RSER72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LFRR72 , SBSB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LFRR72 , SESE72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( LFRR72 , SFSI72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( LFRR72 , SISF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LFRR72 , SLSR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72 ] )
        , ( ( LFRR72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LIRL72 , BBBB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LIRL72 , BBFF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LIRL72 , BEIE72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( LIRL72 , BFII72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( LIRL72 , BIIF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LIRL72 , BLRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( LIRL72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( LIRL72 , BSEF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LIRL72 , EBIS72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( LIRL72 , EFBS72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( LIRL72 , EIFS72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( LIRL72 , ELLS72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72, ILLR72, IRRL72 ] )
        , ( ( LIRL72 , ERRS72 )
          , Set.fromList
                [ IBIB72, IIFB72, ILLR72, IRRL72, ISEB72 ] )
        , ( ( LIRL72 , ESES72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( LIRL72 , FBII72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( LIRL72 , FEFE72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( LIRL72 , FFBB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( LIRL72 , FFFF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( LIRL72 , FIFI72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( LIRL72 , FLLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( LIRL72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( LIRL72 , FSEI72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( LIRL72 , IBIB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LIRL72 , IEBE72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( LIRL72 , IFBI72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( LIRL72 , IIBF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LIRL72 , IIFB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LIRL72 , ILLR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( LIRL72 , IRRL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( LIRL72 , ISEB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LIRL72 , LBLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LIRL72 , LERE72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72, LIRL72, RILR72 ] )
        , ( ( LIRL72 , LFRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LIRL72 , LIRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LIRL72 , LLBR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LIRL72 , LLFL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRLF72, RRLL72, RRLR72 ] )
        , ( ( LIRL72 , LLLB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72, LLLR72
                , RRRL72 ] )
        , ( ( LIRL72 , LLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( LIRL72 , LLLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LIRL72 , LLRF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72, LLRL72
                , RRLR72 ] )
        , ( ( LIRL72 , LLRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRLF72, RRLL72, RRLR72 ] )
        , ( ( LIRL72 , LLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LIRL72 , LRIL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( LIRL72 , LRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( LIRL72 , LRRI72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72, FBII72, FIFI72, FSEI72, IBIB72
                , IIFB72, ISEB72, LRRL72, RLLR72 ] )
        , ( ( LIRL72 , LRRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( LIRL72 , LRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( LIRL72 , LSEL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LIRL72 , RBRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LIRL72 , RELE72 )
          , Set.fromList
                [ BIIF72, IIBF72, LIRL72, RILR72, SISF72 ] )
        , ( ( LIRL72 , RFLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( LIRL72 , RILR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LIRL72 , RLIR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LIRL72 , RLLI72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, IEBE72, IFBI72, IIBF72, LRRL72
                , RLLR72, SESE72, SFSI72, SISF72 ] )
        , ( ( LIRL72 , RLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( LIRL72 , RLLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LIRL72 , RLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LIRL72 , RRBL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLL72, RRRL72, SRSL72 ] )
        , ( ( LIRL72 , RRFR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( LIRL72 , RRLF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, LLRL72, RRLR72
                , SISF72 ] )
        , ( ( LIRL72 , RRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( LIRL72 , RRLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( LIRL72 , RRRB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, LLLR72, RRRL72
                , SBSB72 ] )
        , ( ( LIRL72 , RRRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLL72, RRRL72, SRSL72 ] )
        , ( ( LIRL72 , RRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LIRL72 , RSER72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LIRL72 , SBSB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LIRL72 , SESE72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( LIRL72 , SFSI72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( LIRL72 , SISF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LIRL72 , SLSR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( LIRL72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( LLBR72 , BBBB72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72, RRBL72, SRSL72 ] )
        , ( ( LLBR72 , BBFF72 )
          , Set.fromList
                [ LLBR72, RLIR72, RRFR72, RSER72, SLSR72 ] )
        , ( ( LLBR72 , BEIE72 )
          , Set.fromList
                [ LLBR72, RLIR72, SLSR72 ] )
        , ( ( LLBR72 , BFII72 )
          , Set.fromList
                [ LLBR72, RLIR72, SLSR72 ] )
        , ( ( LLBR72 , BIIF72 )
          , Set.fromList
                [ LLBR72, RLIR72, SLSR72 ] )
        , ( ( LLBR72 , BLRR72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72, RBRR72, RLRR72
                , RRRB72, RRRL72, RRRR72, SBSB72, SLSR72, SRSL72 ] )
        , ( ( LLBR72 , BRLL72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72, SESE72, SFSI72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( LLBR72 , BSEF72 )
          , Set.fromList
                [ LLBR72, RLIR72, SLSR72 ] )
        , ( ( LLBR72 , EBIS72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72 ] )
        , ( ( LLBR72 , EFBS72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLBR72 , EIFS72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLBR72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( LLBR72 , ERRS72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( LLBR72 , ESES72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLBR72 , FBII72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72 ] )
        , ( ( LLBR72 , FEFE72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLBR72 , FFBB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLBR72 , FFFF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLBR72 , FIFI72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLBR72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( LLBR72 , FRRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( LLBR72 , FSEI72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLBR72 , IBIB72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72 ] )
        , ( ( LLBR72 , IEBE72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLBR72 , IFBI72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLBR72 , IIBF72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLBR72 , IIFB72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLBR72 , ILLR72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( LLBR72 , IRRL72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( LLBR72 , ISEB72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLBR72 , LBLL72 )
          , Set.fromList
                [ BLRR72, BRLL72, BSEF72, LLRF72, LLRL72, LLRR72, LRLL72
                , LSEL72, RLRR72, RRLF72, RRLL72, RRLR72, RSER72 ] )
        , ( ( LLBR72 , LERE72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( LLBR72 , LFRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( LLBR72 , LIRL72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( LLBR72 , LLBR72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72
                , RRLL72 ] )
        , ( ( LLBR72 , LLFL72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72
                , RRRR72 ] )
        , ( ( LLBR72 , LLLB72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72
                , RRLL72 ] )
        , ( ( LLBR72 , LLLL72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, LBLL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRLL72, RBRR72, RLRR72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( LLBR72 , LLLR72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72
                , RRLL72 ] )
        , ( ( LLBR72 , LLRF72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72
                , RRRR72 ] )
        , ( ( LLBR72 , LLRL72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72
                , RRRR72 ] )
        , ( ( LLBR72 , LLRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRLL72, RBRR72, RLRR72
                , RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLBR72 , LRIL72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( LLBR72 , LRLL72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, BLRR72, BRLL72, LERE72, LFRR72
                , LIRL72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( LLBR72 , LRRI72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( LLBR72 , LRRL72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( LLBR72 , LRRR72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( LLBR72 , LSEL72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( LLBR72 , RBRR72 )
          , Set.fromList
                [ ELLS72, ERRS72, ESES72, FLLL72, FRRR72, FSEI72, ILLR72
                , IRRL72, ISEB72, LLLB72, LLLL72, LLLR72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLI72, RLLL72, RLLR72, RRRB72, RRRL72
                , RRRR72, RSER72 ] )
        , ( ( LLBR72 , RELE72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( LLBR72 , RFLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( LLBR72 , RILR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( LLBR72 , RLIR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLBR72 , RLLI72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLBR72 , RLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLBR72 , RLLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLBR72 , RLRR72 )
          , Set.fromList
                [ EBIS72, ELLS72, ERRS72, FBII72, FLLL72, FRRR72, IBIB72
                , ILLR72, IRRL72, LBLL72, LLLB72, LLLL72, LLLR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, RBRR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLBR72 , RRBL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LLBR72 , RRFR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( LLBR72 , RRLF72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( LLBR72 , RRLL72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LLBR72 , RRLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( LLBR72 , RRRB72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LLBR72 , RRRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LLBR72 , RRRR72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FEFE72, FFFF72, FIFI72, FLLL72
                , FRRR72, IIFB72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LLBR72 , RSER72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( LLBR72 , SBSB72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72 ] )
        , ( ( LLBR72 , SESE72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLBR72 , SFSI72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLBR72 , SISF72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLBR72 , SLSR72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( LLBR72 , SRSL72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( LLFL72 , BBBB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLFL72 , BBFF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLFL72 , BEIE72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLFL72 , BFII72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72 ] )
        , ( ( LLFL72 , BIIF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLFL72 , BLRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( LLFL72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( LLFL72 , BSEF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLFL72 , EBIS72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLFL72 , EFBS72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72 ] )
        , ( ( LLFL72 , EIFS72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLFL72 , ELLS72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( LLFL72 , ERRS72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( LLFL72 , ESES72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLFL72 , FBII72 )
          , Set.fromList
                [ LLBR72, RLIR72, SLSR72 ] )
        , ( ( LLFL72 , FEFE72 )
          , Set.fromList
                [ LLBR72, RLIR72, SLSR72 ] )
        , ( ( LLFL72 , FFBB72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72, RRBL72, SRSL72 ] )
        , ( ( LLFL72 , FFFF72 )
          , Set.fromList
                [ LLBR72, RLIR72, RRFR72, RSER72, SLSR72 ] )
        , ( ( LLFL72 , FIFI72 )
          , Set.fromList
                [ LLBR72, RLIR72, SLSR72 ] )
        , ( ( LLFL72 , FLLL72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72, SESE72, SFSI72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( LLFL72 , FRRR72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72, RBRR72, RLRR72
                , RRRB72, RRRL72, RRRR72, SBSB72, SLSR72, SRSL72 ] )
        , ( ( LLFL72 , FSEI72 )
          , Set.fromList
                [ LLBR72, RLIR72, SLSR72 ] )
        , ( ( LLFL72 , IBIB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLFL72 , IEBE72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLFL72 , IFBI72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72 ] )
        , ( ( LLFL72 , IIBF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLFL72 , IIFB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLFL72 , ILLR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( LLFL72 , IRRL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( LLFL72 , ISEB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLFL72 , LBLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( LLFL72 , LERE72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( LLFL72 , LFRR72 )
          , Set.fromList
                [ ELLS72, ERRS72, ESES72, FLLL72, FRRR72, FSEI72, ILLR72
                , IRRL72, ISEB72, LLLB72, LLLL72, LLLR72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLI72, RLLL72, RLLR72, RRRB72, RRRL72
                , RRRR72, RSER72 ] )
        , ( ( LLFL72 , LIRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( LLFL72 , LLBR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LLFL72 , LLFL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( LLFL72 , LLLB72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LLFL72 , LLLL72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LLFL72 , LLLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LLFL72 , LLRF72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( LLFL72 , LLRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( LLFL72 , LLRR72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FEFE72, FFFF72, FIFI72, FLLL72
                , FRRR72, IIFB72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LLFL72 , LRIL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLFL72 , LRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLFL72 , LRRI72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLFL72 , LRRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLFL72 , LRRR72 )
          , Set.fromList
                [ EBIS72, ELLS72, ERRS72, FBII72, FLLL72, FRRR72, IBIB72
                , ILLR72, IRRL72, LBLL72, LLLB72, LLLL72, LLLR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, RBRR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLFL72 , LSEL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( LLFL72 , RBRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( LLFL72 , RELE72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( LLFL72 , RFLL72 )
          , Set.fromList
                [ BLRR72, BRLL72, BSEF72, LLRF72, LLRL72, LLRR72, LRLL72
                , LSEL72, RLRR72, RRLF72, RRLL72, RRLR72, RSER72 ] )
        , ( ( LLFL72 , RILR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( LLFL72 , RLIR72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( LLFL72 , RLLI72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( LLFL72 , RLLL72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, BLRR72, BRLL72, LERE72, LFRR72
                , LIRL72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( LLFL72 , RLLR72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( LLFL72 , RLRR72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( LLFL72 , RRBL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72
                , RRLL72 ] )
        , ( ( LLFL72 , RRFR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72
                , RRRR72 ] )
        , ( ( LLFL72 , RRLF72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72
                , RRRR72 ] )
        , ( ( LLFL72 , RRLL72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, LBLL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRLL72, RBRR72, RLRR72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( LLFL72 , RRLR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72
                , RRRR72 ] )
        , ( ( LLFL72 , RRRB72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72
                , RRLL72 ] )
        , ( ( LLFL72 , RRRL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72
                , RRLL72 ] )
        , ( ( LLFL72 , RRRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRLL72, RBRR72, RLRR72
                , RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLFL72 , RSER72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( LLFL72 , SBSB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LLFL72 , SESE72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLFL72 , SFSI72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72 ] )
        , ( ( LLFL72 , SISF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LLFL72 , SLSR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( LLFL72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( LLLB72 , BBBB72 )
          , Set.fromList
                [ ERRS72, LERE72, LLRF72, LRRI72, RRRB72 ] )
        , ( ( LLLB72 , BBFF72 )
          , Set.fromList
                [ ELLS72, LLLB72, RELE72, RLLI72, RRLF72 ] )
        , ( ( LLLB72 , BEIE72 )
          , Set.fromList
                [ ELLS72, LLLB72, RLLI72 ] )
        , ( ( LLLB72 , BFII72 )
          , Set.fromList
                [ ELLS72, LLLB72, RLLI72 ] )
        , ( ( LLLB72 , BIIF72 )
          , Set.fromList
                [ ELLS72, LLLB72, RLLI72 ] )
        , ( ( LLLB72 , BLRR72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72, LBLL72, LIRL72
                , LLFL72, LLLL72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LLLB72 , BRLL72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, LFRR72, LLBR72, LLLR72, LLRR72
                , LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( LLLB72 , BSEF72 )
          , Set.fromList
                [ ELLS72, LLLB72, RLLI72 ] )
        , ( ( LLLB72 , EBIS72 )
          , Set.fromList
                [ LERE72, LLRF72, LRRI72 ] )
        , ( ( LLLB72 , EFBS72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLLB72 , EIFS72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLLB72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( LLLB72 , ERRS72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( LLLB72 , ESES72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLLB72 , FBII72 )
          , Set.fromList
                [ LERE72, LLRF72, LRRI72 ] )
        , ( ( LLLB72 , FEFE72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLLB72 , FFBB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLLB72 , FFFF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLLB72 , FIFI72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLLB72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( LLLB72 , FRRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( LLLB72 , FSEI72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLLB72 , IBIB72 )
          , Set.fromList
                [ LERE72, LLRF72, LRRI72 ] )
        , ( ( LLLB72 , IEBE72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLLB72 , IFBI72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLLB72 , IIBF72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLLB72 , IIFB72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLLB72 , ILLR72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( LLLB72 , IRRL72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( LLLB72 , ISEB72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLLB72 , LBLL72 )
          , Set.fromList
                [ BEIE72, BLRR72, BRLL72, IEBE72, ILLR72, IRRL72, LERE72
                , LLBR72, LLLR72, LLRR72, LRIL72, LRLL72, LRRL72, RELE72
                , RLIR72, RLLR72, RLRR72, RRBL72, RRLL72, RRRL72, SESE72
                , SLSR72, SRSL72 ] )
        , ( ( LLLB72 , LERE72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( LLLB72 , LFRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( LLLB72 , LIRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( LLLB72 , LLBR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( LLLB72 , LLFL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( LLLB72 , LLLB72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( LLLB72 , LLLL72 )
          , Set.fromList
                [ BBFF72, BIIF72, BLRR72, BRLL72, BSEF72, IIBF72, ILLR72
                , IRRL72, LBLL72, LIRL72, LLBR72, LLFL72, LLLL72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRL72, RRRR72, RSER72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( LLLB72 , LLLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( LLLB72 , LLRF72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( LLLB72 , LLRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( LLLB72 , LLRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( LLLB72 , LRIL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLB72 , LRLL72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, IFBI72, ILLR72, IRRL72, LFRR72
                , LLBR72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , RRBL72, RRLL72, RRRL72, SFSI72, SLSR72, SRSL72 ] )
        , ( ( LLLB72 , LRRI72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLB72 , LRRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLB72 , LRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLB72 , LSEL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( LLLB72 , RBRR72 )
          , Set.fromList
                [ FEFE72, FLLL72, FRRR72, LERE72, LLFL72, LLLL72, LLRL72
                , LRRR72, RELE72, RLLL72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( LLLB72 , RELE72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( LLLB72 , RFLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( LLLB72 , RILR72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( LLLB72 , RLIR72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( LLLB72 , RLLI72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( LLLB72 , RLLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( LLLB72 , RLLR72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( LLLB72 , RLRR72 )
          , Set.fromList
                [ FBII72, FIFI72, FLLL72, FRRR72, FSEI72, LBLL72, LIRL72
                , LLFL72, LLLL72, LLRL72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( LLLB72 , RRBL72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RRRR72 ] )
        , ( ( LLLB72 , RRFR72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72
                , RRLL72 ] )
        , ( ( LLLB72 , RRLF72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72
                , RRLL72 ] )
        , ( ( LLLB72 , RRLL72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LFRR72, LLBR72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LLRR72, LRRR72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLLB72 , RRLR72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72
                , RRLL72 ] )
        , ( ( LLLB72 , RRRB72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RRRR72 ] )
        , ( ( LLLB72 , RRRL72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RRRR72 ] )
        , ( ( LLLB72 , RRRR72 )
          , Set.fromList
                [ FFFF72, FLLL72, FRRR72, LFRR72, LLBR72, LLFL72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRR72, RFLL72, RLLL72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( LLLB72 , RSER72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( LLLB72 , SBSB72 )
          , Set.fromList
                [ LERE72, LLRF72, LRRI72 ] )
        , ( ( LLLB72 , SESE72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLLB72 , SFSI72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLLB72 , SISF72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLLB72 , SLSR72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( LLLB72 , SRSL72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( LLLL72 , BBBB72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRRR72 ] )
        , ( ( LLLL72 , BBFF72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRLL72 ] )
        , ( ( LLLL72 , BEIE72 )
          , Set.fromList
                [ FLLL72, LLLL72, RLLL72 ] )
        , ( ( LLLL72 , BFII72 )
          , Set.fromList
                [ FLLL72, LBLL72, LLLL72, LRLL72, RLLL72 ] )
        , ( ( LLLL72 , BIIF72 )
          , Set.fromList
                [ FLLL72, LLLL72, RLLL72 ] )
        , ( ( LLLL72 , BLRR72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72, FSEI72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LLLL72 , BRLL72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LBLL72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, LRRR72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLLL72 , BSEF72 )
          , Set.fromList
                [ FLLL72, LLLL72, RLLL72 ] )
        , ( ( LLLL72 , EBIS72 )
          , Set.fromList
                [ LFRR72, LLRR72, LRRR72 ] )
        , ( ( LLLL72 , EFBS72 )
          , Set.fromList
                [ LBLL72, LLLL72, LRLL72 ] )
        , ( ( LLLL72 , EIFS72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LLLL72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LLLL72 , ERRS72 )
          , Set.fromList
                [ LBLL72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, LRRR72 ] )
        , ( ( LLLL72 , ESES72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LLLL72 , FBII72 )
          , Set.fromList
                [ BLRR72, LFRR72, LLRR72, LRRR72, RLRR72 ] )
        , ( ( LLLL72 , FEFE72 )
          , Set.fromList
                [ BLRR72, LLRR72, RLRR72 ] )
        , ( ( LLLL72 , FFBB72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLL72 ] )
        , ( ( LLLL72 , FFFF72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRR72 ] )
        , ( ( LLLL72 , FIFI72 )
          , Set.fromList
                [ BLRR72, LLRR72, RLRR72 ] )
        , ( ( LLLL72 , FLLL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LLLL72 , FRRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, LRRR72, RBRR72, RLRR72
                , RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLLL72 , FSEI72 )
          , Set.fromList
                [ BLRR72, LLRR72, RLRR72 ] )
        , ( ( LLLL72 , IBIB72 )
          , Set.fromList
                [ LFRR72, LLRR72, LRRR72 ] )
        , ( ( LLLL72 , IEBE72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LLLL72 , IFBI72 )
          , Set.fromList
                [ LBLL72, LLLL72, LRLL72 ] )
        , ( ( LLLL72 , IIBF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LLLL72 , IIFB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LLLL72 , ILLR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LLLL72 , IRRL72 )
          , Set.fromList
                [ LBLL72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, LRRR72 ] )
        , ( ( LLLL72 , ISEB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LLLL72 , LBLL72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, EFBS72, ELLS72, ERRS72, FFBB72
                , FLLL72, FRRR72, IFBI72, ILLR72, IRRL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SFSI72
                , SLSR72, SRSL72 ] )
        , ( ( LLLL72 , LERE72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LLLL72 , LFRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, ELLS72, ERRS72, FBII72
                , FLLL72, FRRR72, IBIB72, ILLR72, IRRL72, LBLL72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( LLLL72 , LIRL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LLLL72 , LLBR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LLLL72 , LLFL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LLLL72 , LLLB72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LLLL72 , LLLL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SESE72, SFSI72
                , SISF72, SLSR72, SRSL72 ] )
        , ( ( LLLL72 , LLLR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LLLL72 , LLRF72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LLLL72 , LLRL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LLLL72 , LLRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, EIFS72, ELLS72, ERRS72
                , ESES72, FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72, IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72
                , LERE72, LFRR72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72
                , RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( LLLL72 , LRIL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRLL72, LRRR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLL72 , LRLL72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, EFBS72, ELLS72, ERRS72, FFBB72
                , FLLL72, FRRR72, IFBI72, ILLR72, IRRL72, LBLL72, LFRR72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72
                , LRRI72, LRRL72, LRRR72, RFLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SFSI72, SLSR72, SRSL72 ] )
        , ( ( LLLL72 , LRRI72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRLL72, LRRR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLL72 , LRRL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRLL72, LRRR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLL72 , LRRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, ELLS72, ERRS72, FBII72
                , FLLL72, FRRR72, IBIB72, ILLR72, IRRL72, LBLL72, LFRR72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72
                , LRRI72, LRRL72, LRRR72, RBRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( LLLL72 , LSEL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LLLL72 , RBRR72 )
          , Set.fromList
                [ BLRR72, FFFF72, FLLL72, FRRR72, LFRR72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRRR72, RFLL72, RLLL72, RLRR72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( LLLL72 , RELE72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72, RLRR72 ] )
        , ( ( LLLL72 , RFLL72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, FLLL72, LBLL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRLL72, RBRR72, RLLL72, RLRR72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( LLLL72 , RILR72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72, RLRR72 ] )
        , ( ( LLLL72 , RLIR72 )
          , Set.fromList
                [ BLRR72, FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72
                , LLLL72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RLLL72, RLRR72 ] )
        , ( ( LLLL72 , RLLI72 )
          , Set.fromList
                [ BLRR72, FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72
                , LLLL72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RLLL72, RLRR72 ] )
        , ( ( LLLL72 , RLLL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( LLLL72 , RLLR72 )
          , Set.fromList
                [ BLRR72, FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72
                , LLLL72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RLLL72, RLRR72 ] )
        , ( ( LLLL72 , RLRR72 )
          , Set.fromList
                [ BLRR72, FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( LLLL72 , RRBL72 )
          , Set.fromList
                [ BRLL72, FRRR72, LBLL72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, LRRR72, RRLL72, RRRR72 ] )
        , ( ( LLLL72 , RRFR72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RBRR72, RFLL72, RLLL72, RLRR72, RRLL72, RRRR72 ] )
        , ( ( LLLL72 , RRLF72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RBRR72, RFLL72, RLLL72, RLRR72, RRLL72, RRRR72 ] )
        , ( ( LLLL72 , RRLL72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, FFBB72, FLLL72, FRRR72, LBLL72
                , LFRR72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRLL72, LRRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LLLL72 , RRLR72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RBRR72, RFLL72, RLLL72, RLRR72, RRLL72, RRRR72 ] )
        , ( ( LLLL72 , RRRB72 )
          , Set.fromList
                [ BRLL72, FRRR72, LBLL72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, LRRR72, RRLL72, RRRR72 ] )
        , ( ( LLLL72 , RRRL72 )
          , Set.fromList
                [ BRLL72, FRRR72, LBLL72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, LRRR72, RRLL72, RRRR72 ] )
        , ( ( LLLL72 , RRRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, FFFF72, FLLL72, FRRR72, LBLL72
                , LFRR72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRLL72, LRRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LLLL72 , RSER72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72, RLRR72 ] )
        , ( ( LLLL72 , SBSB72 )
          , Set.fromList
                [ LFRR72, LLRR72, LRRR72 ] )
        , ( ( LLLL72 , SESE72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LLLL72 , SFSI72 )
          , Set.fromList
                [ LBLL72, LLLL72, LRLL72 ] )
        , ( ( LLLL72 , SISF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LLLL72 , SLSR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LLLL72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, LRRR72 ] )
        , ( ( LLLR72 , BBBB72 )
          , Set.fromList
                [ IRRL72, LIRL72, LLRL72, LRRL72, RRRL72 ] )
        , ( ( LLLR72 , BBFF72 )
          , Set.fromList
                [ ILLR72, LLLR72, RILR72, RLLR72, RRLR72 ] )
        , ( ( LLLR72 , BEIE72 )
          , Set.fromList
                [ ILLR72, LLLR72, RLLR72 ] )
        , ( ( LLLR72 , BFII72 )
          , Set.fromList
                [ ILLR72, LLLR72, RLLR72 ] )
        , ( ( LLLR72 , BIIF72 )
          , Set.fromList
                [ ILLR72, LLLR72, RLLR72 ] )
        , ( ( LLLR72 , BLRR72 )
          , Set.fromList
                [ IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( LLLR72 , BRLL72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( LLLR72 , BSEF72 )
          , Set.fromList
                [ ILLR72, LLLR72, RLLR72 ] )
        , ( ( LLLR72 , EBIS72 )
          , Set.fromList
                [ LIRL72, LLRL72, LRRL72 ] )
        , ( ( LLLR72 , EFBS72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLLR72 , EIFS72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLLR72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( LLLR72 , ERRS72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( LLLR72 , ESES72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLLR72 , FBII72 )
          , Set.fromList
                [ LIRL72, LLRL72, LRRL72 ] )
        , ( ( LLLR72 , FEFE72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLLR72 , FFBB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLLR72 , FFFF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLLR72 , FIFI72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLLR72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( LLLR72 , FRRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( LLLR72 , FSEI72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLLR72 , IBIB72 )
          , Set.fromList
                [ LIRL72, LLRL72, LRRL72 ] )
        , ( ( LLLR72 , IEBE72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLLR72 , IFBI72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLLR72 , IIBF72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLLR72 , IIFB72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLLR72 , ILLR72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( LLLR72 , IRRL72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( LLLR72 , ISEB72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLLR72 , LBLL72 )
          , Set.fromList
                [ BIIF72, BLRR72, BRLL72, IIBF72, ILLR72, IRRL72, LIRL72
                , LLBR72, LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72
                , LRRL72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SISF72, SLSR72, SRSL72 ] )
        , ( ( LLLR72 , LERE72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLR72 , LFRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLR72 , LIRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLR72 , LLBR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLL72, RRRL72, SRSL72 ] )
        , ( ( LLLR72 , LLFL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( LLLR72 , LLLB72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLL72, RRRL72, SRSL72 ] )
        , ( ( LLLR72 , LLLL72 )
          , Set.fromList
                [ BBFF72, BIIF72, BLRR72, BRLL72, BSEF72, IIBF72, ILLR72
                , IRRL72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72
                , LSEL72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRL72, RRRR72, RSER72
                , SISF72, SLSR72, SRSL72 ] )
        , ( ( LLLR72 , LLLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLL72, RRRL72, SRSL72 ] )
        , ( ( LLLR72 , LLRF72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( LLLR72 , LLRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( LLLR72 , LLRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72
                , LSEL72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72
                , RRFR72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( LLLR72 , LRIL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLR72 , LRLL72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, BLRR72, BRLL72, IEBE72, IFBI72
                , IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRLF72, RRLL72, RRLR72
                , RRRL72, SESE72, SFSI72, SISF72, SLSR72, SRSL72 ] )
        , ( ( LLLR72 , LRRI72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLR72 , LRRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLR72 , LRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLR72 , LSEL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLLR72 , RBRR72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FIFI72, FLLL72, FRRR72, IIFB72
                , ILLR72, IRRL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, LRRI72, LRRL72, LRRR72, RILR72, RLLI72, RLLL72
                , RLLR72, RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLLR72 , RELE72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLLR72 , RFLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLLR72 , RILR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLLR72 , RLIR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLLR72 , RLLI72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLLR72 , RLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLLR72 , RLLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLLR72 , RLRR72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72, FBII72, FIFI72
                , FLLL72, FRRR72, FSEI72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( LLLR72 , RRBL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLLR72 , RRFR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRLF72, RRLL72, RRLR72 ] )
        , ( ( LLLR72 , RRLF72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRLF72, RRLL72, RRLR72 ] )
        , ( ( LLLR72 , RRLL72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72
                , RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLLR72 , RRLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRLF72, RRLL72, RRLR72 ] )
        , ( ( LLLR72 , RRRB72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLLR72 , RRRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLLR72 , RRRR72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FEFE72, FFFF72, FIFI72, FLLL72
                , FRRR72, IIFB72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72
                , RLLI72, RLLL72, RLLR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLLR72 , RSER72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLLR72 , SBSB72 )
          , Set.fromList
                [ LIRL72, LLRL72, LRRL72 ] )
        , ( ( LLLR72 , SESE72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLLR72 , SFSI72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLLR72 , SISF72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLLR72 , SLSR72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( LLLR72 , SRSL72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( LLRF72 , BBBB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLRF72 , BBFF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLRF72 , BEIE72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLRF72 , BFII72 )
          , Set.fromList
                [ LERE72, LLRF72, LRRI72 ] )
        , ( ( LLRF72 , BIIF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLRF72 , BLRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( LLRF72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( LLRF72 , BSEF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLRF72 , EBIS72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLRF72 , EFBS72 )
          , Set.fromList
                [ LERE72, LLRF72, LRRI72 ] )
        , ( ( LLRF72 , EIFS72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLRF72 , ELLS72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( LLRF72 , ERRS72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( LLRF72 , ESES72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLRF72 , FBII72 )
          , Set.fromList
                [ ELLS72, LLLB72, RLLI72 ] )
        , ( ( LLRF72 , FEFE72 )
          , Set.fromList
                [ ELLS72, LLLB72, RLLI72 ] )
        , ( ( LLRF72 , FFBB72 )
          , Set.fromList
                [ ERRS72, LERE72, LLRF72, LRRI72, RRRB72 ] )
        , ( ( LLRF72 , FFFF72 )
          , Set.fromList
                [ ELLS72, LLLB72, RELE72, RLLI72, RRLF72 ] )
        , ( ( LLRF72 , FIFI72 )
          , Set.fromList
                [ ELLS72, LLLB72, RLLI72 ] )
        , ( ( LLRF72 , FLLL72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, LFRR72, LLBR72, LLLR72, LLRR72
                , LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( LLRF72 , FRRR72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72, LBLL72, LIRL72
                , LLFL72, LLLL72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LLRF72 , FSEI72 )
          , Set.fromList
                [ ELLS72, LLLB72, RLLI72 ] )
        , ( ( LLRF72 , IBIB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLRF72 , IEBE72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLRF72 , IFBI72 )
          , Set.fromList
                [ LERE72, LLRF72, LRRI72 ] )
        , ( ( LLRF72 , IIBF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLRF72 , IIFB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLRF72 , ILLR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( LLRF72 , IRRL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( LLRF72 , ISEB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLRF72 , LBLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( LLRF72 , LERE72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( LLRF72 , LFRR72 )
          , Set.fromList
                [ FEFE72, FLLL72, FRRR72, LERE72, LLFL72, LLLL72, LLRL72
                , LRRR72, RELE72, RLLL72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( LLRF72 , LIRL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( LLRF72 , LLBR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RRRR72 ] )
        , ( ( LLRF72 , LLFL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72
                , RRLL72 ] )
        , ( ( LLRF72 , LLLB72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RRRR72 ] )
        , ( ( LLRF72 , LLLL72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LFRR72, LLBR72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LLRR72, LRRR72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLRF72 , LLLR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RRRR72 ] )
        , ( ( LLRF72 , LLRF72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72
                , RRLL72 ] )
        , ( ( LLRF72 , LLRL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72
                , RRLL72 ] )
        , ( ( LLRF72 , LLRR72 )
          , Set.fromList
                [ FFFF72, FLLL72, FRRR72, LFRR72, LLBR72, LLFL72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRR72, RFLL72, RLLL72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( LLRF72 , LRIL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( LLRF72 , LRLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( LLRF72 , LRRI72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( LLRF72 , LRRL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( LLRF72 , LRRR72 )
          , Set.fromList
                [ FBII72, FIFI72, FLLL72, FRRR72, FSEI72, LBLL72, LIRL72
                , LLFL72, LLLL72, LLRL72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( LLRF72 , LSEL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( LLRF72 , RBRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( LLRF72 , RELE72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( LLRF72 , RFLL72 )
          , Set.fromList
                [ BEIE72, BLRR72, BRLL72, IEBE72, ILLR72, IRRL72, LERE72
                , LLBR72, LLLR72, LLRR72, LRIL72, LRLL72, LRRL72, RELE72
                , RLIR72, RLLR72, RLRR72, RRBL72, RRLL72, RRRL72, SESE72
                , SLSR72, SRSL72 ] )
        , ( ( LLRF72 , RILR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( LLRF72 , RLIR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRF72 , RLLI72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRF72 , RLLL72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, IFBI72, ILLR72, IRRL72, LFRR72
                , LLBR72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , RRBL72, RRLL72, RRRL72, SFSI72, SLSR72, SRSL72 ] )
        , ( ( LLRF72 , RLLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRF72 , RLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRF72 , RRBL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( LLRF72 , RRFR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( LLRF72 , RRLF72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( LLRF72 , RRLL72 )
          , Set.fromList
                [ BBFF72, BIIF72, BLRR72, BRLL72, BSEF72, IIBF72, ILLR72
                , IRRL72, LBLL72, LIRL72, LLBR72, LLFL72, LLLL72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRL72, RRRR72, RSER72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( LLRF72 , RRLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( LLRF72 , RRRB72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( LLRF72 , RRRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( LLRF72 , RRRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( LLRF72 , RSER72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( LLRF72 , SBSB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LLRF72 , SESE72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLRF72 , SFSI72 )
          , Set.fromList
                [ LERE72, LLRF72, LRRI72 ] )
        , ( ( LLRF72 , SISF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LLRF72 , SLSR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( LLRF72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( LLRL72 , BBBB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLRL72 , BBFF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLRL72 , BEIE72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLRL72 , BFII72 )
          , Set.fromList
                [ LIRL72, LLRL72, LRRL72 ] )
        , ( ( LLRL72 , BIIF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLRL72 , BLRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( LLRL72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( LLRL72 , BSEF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLRL72 , EBIS72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLRL72 , EFBS72 )
          , Set.fromList
                [ LIRL72, LLRL72, LRRL72 ] )
        , ( ( LLRL72 , EIFS72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLRL72 , ELLS72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( LLRL72 , ERRS72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( LLRL72 , ESES72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLRL72 , FBII72 )
          , Set.fromList
                [ ILLR72, LLLR72, RLLR72 ] )
        , ( ( LLRL72 , FEFE72 )
          , Set.fromList
                [ ILLR72, LLLR72, RLLR72 ] )
        , ( ( LLRL72 , FFBB72 )
          , Set.fromList
                [ IRRL72, LIRL72, LLRL72, LRRL72, RRRL72 ] )
        , ( ( LLRL72 , FFFF72 )
          , Set.fromList
                [ ILLR72, LLLR72, RILR72, RLLR72, RRLR72 ] )
        , ( ( LLRL72 , FIFI72 )
          , Set.fromList
                [ ILLR72, LLLR72, RLLR72 ] )
        , ( ( LLRL72 , FLLL72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( LLRL72 , FRRR72 )
          , Set.fromList
                [ IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( LLRL72 , FSEI72 )
          , Set.fromList
                [ ILLR72, LLLR72, RLLR72 ] )
        , ( ( LLRL72 , IBIB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLRL72 , IEBE72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLRL72 , IFBI72 )
          , Set.fromList
                [ LIRL72, LLRL72, LRRL72 ] )
        , ( ( LLRL72 , IIBF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLRL72 , IIFB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLRL72 , ILLR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( LLRL72 , IRRL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( LLRL72 , ISEB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLRL72 , LBLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLRL72 , LERE72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLRL72 , LFRR72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FIFI72, FLLL72, FRRR72, IIFB72
                , ILLR72, IRRL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, LRRI72, LRRL72, LRRR72, RILR72, RLLI72, RLLL72
                , RLLR72, RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLRL72 , LIRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLRL72 , LLBR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLRL72 , LLFL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRLF72, RRLL72, RRLR72 ] )
        , ( ( LLRL72 , LLLB72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLRL72 , LLLL72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72
                , RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLRL72 , LLLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLRL72 , LLRF72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRLF72, RRLL72, RRLR72 ] )
        , ( ( LLRL72 , LLRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRLF72, RRLL72, RRLR72 ] )
        , ( ( LLRL72 , LLRR72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FEFE72, FFFF72, FIFI72, FLLL72
                , FRRR72, IIFB72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72
                , RLLI72, RLLL72, RLLR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLRL72 , LRIL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLRL72 , LRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLRL72 , LRRI72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLRL72 , LRRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLRL72 , LRRR72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72, FBII72, FIFI72
                , FLLL72, FRRR72, FSEI72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( LLRL72 , LSEL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LLRL72 , RBRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRL72 , RELE72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRL72 , RFLL72 )
          , Set.fromList
                [ BIIF72, BLRR72, BRLL72, IIBF72, ILLR72, IRRL72, LIRL72
                , LLBR72, LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72
                , LRRL72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SISF72, SLSR72, SRSL72 ] )
        , ( ( LLRL72 , RILR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRL72 , RLIR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRL72 , RLLI72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRL72 , RLLL72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, BLRR72, BRLL72, IEBE72, IFBI72
                , IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRLF72, RRLL72, RRLR72
                , RRRL72, SESE72, SFSI72, SISF72, SLSR72, SRSL72 ] )
        , ( ( LLRL72 , RLLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRL72 , RLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRL72 , RRBL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLL72, RRRL72, SRSL72 ] )
        , ( ( LLRL72 , RRFR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( LLRL72 , RRLF72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( LLRL72 , RRLL72 )
          , Set.fromList
                [ BBFF72, BIIF72, BLRR72, BRLL72, BSEF72, IIBF72, ILLR72
                , IRRL72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72
                , LSEL72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRL72, RRRR72, RSER72
                , SISF72, SLSR72, SRSL72 ] )
        , ( ( LLRL72 , RRLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( LLRL72 , RRRB72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLL72, RRRL72, SRSL72 ] )
        , ( ( LLRL72 , RRRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLL72, RRRL72, SRSL72 ] )
        , ( ( LLRL72 , RRRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72
                , LSEL72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72
                , RRFR72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( LLRL72 , RSER72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRL72 , SBSB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LLRL72 , SESE72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLRL72 , SFSI72 )
          , Set.fromList
                [ LIRL72, LLRL72, LRRL72 ] )
        , ( ( LLRL72 , SISF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LLRL72 , SLSR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( LLRL72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( LLRR72 , BBBB72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLL72 ] )
        , ( ( LLRR72 , BBFF72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRR72 ] )
        , ( ( LLRR72 , BEIE72 )
          , Set.fromList
                [ BLRR72, LLRR72, RLRR72 ] )
        , ( ( LLRR72 , BFII72 )
          , Set.fromList
                [ BLRR72, LFRR72, LLRR72, LRRR72, RLRR72 ] )
        , ( ( LLRR72 , BIIF72 )
          , Set.fromList
                [ BLRR72, LLRR72, RLRR72 ] )
        , ( ( LLRR72 , BLRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, LRRR72, RBRR72, RLRR72
                , RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLRR72 , BRLL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LLRR72 , BSEF72 )
          , Set.fromList
                [ BLRR72, LLRR72, RLRR72 ] )
        , ( ( LLRR72 , EBIS72 )
          , Set.fromList
                [ LBLL72, LLLL72, LRLL72 ] )
        , ( ( LLRR72 , EFBS72 )
          , Set.fromList
                [ LFRR72, LLRR72, LRRR72 ] )
        , ( ( LLRR72 , EIFS72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LLRR72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, LRRR72 ] )
        , ( ( LLRR72 , ERRS72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LLRR72 , ESES72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LLRR72 , FBII72 )
          , Set.fromList
                [ FLLL72, LBLL72, LLLL72, LRLL72, RLLL72 ] )
        , ( ( LLRR72 , FEFE72 )
          , Set.fromList
                [ FLLL72, LLLL72, RLLL72 ] )
        , ( ( LLRR72 , FFBB72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRRR72 ] )
        , ( ( LLRR72 , FFFF72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRLL72 ] )
        , ( ( LLRR72 , FIFI72 )
          , Set.fromList
                [ FLLL72, LLLL72, RLLL72 ] )
        , ( ( LLRR72 , FLLL72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LBLL72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, LRRR72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LLRR72 , FRRR72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72, FSEI72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LLRR72 , FSEI72 )
          , Set.fromList
                [ FLLL72, LLLL72, RLLL72 ] )
        , ( ( LLRR72 , IBIB72 )
          , Set.fromList
                [ LBLL72, LLLL72, LRLL72 ] )
        , ( ( LLRR72 , IEBE72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LLRR72 , IFBI72 )
          , Set.fromList
                [ LFRR72, LLRR72, LRRR72 ] )
        , ( ( LLRR72 , IIBF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LLRR72 , IIFB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LLRR72 , ILLR72 )
          , Set.fromList
                [ LBLL72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, LRRR72 ] )
        , ( ( LLRR72 , IRRL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LLRR72 , ISEB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LLRR72 , LBLL72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, FLLL72, LBLL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRLL72, RBRR72, RLLL72, RLRR72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( LLRR72 , LERE72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72, RLRR72 ] )
        , ( ( LLRR72 , LFRR72 )
          , Set.fromList
                [ BLRR72, FFFF72, FLLL72, FRRR72, LFRR72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRRR72, RFLL72, RLLL72, RLRR72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( LLRR72 , LIRL72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72, RLRR72 ] )
        , ( ( LLRR72 , LLBR72 )
          , Set.fromList
                [ BRLL72, FRRR72, LBLL72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, LRRR72, RRLL72, RRRR72 ] )
        , ( ( LLRR72 , LLFL72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RBRR72, RFLL72, RLLL72, RLRR72, RRLL72, RRRR72 ] )
        , ( ( LLRR72 , LLLB72 )
          , Set.fromList
                [ BRLL72, FRRR72, LBLL72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, LRRR72, RRLL72, RRRR72 ] )
        , ( ( LLRR72 , LLLL72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, FFBB72, FLLL72, FRRR72, LBLL72
                , LFRR72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRLL72, LRRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LLRR72 , LLLR72 )
          , Set.fromList
                [ BRLL72, FRRR72, LBLL72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, LRRR72, RRLL72, RRRR72 ] )
        , ( ( LLRR72 , LLRF72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RBRR72, RFLL72, RLLL72, RLRR72, RRLL72, RRRR72 ] )
        , ( ( LLRR72 , LLRL72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RBRR72, RFLL72, RLLL72, RLRR72, RRLL72, RRRR72 ] )
        , ( ( LLRR72 , LLRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, FFFF72, FLLL72, FRRR72, LBLL72
                , LFRR72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRLL72, LRRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LLRR72 , LRIL72 )
          , Set.fromList
                [ BLRR72, FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72
                , LLLL72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RLLL72, RLRR72 ] )
        , ( ( LLRR72 , LRLL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( LLRR72 , LRRI72 )
          , Set.fromList
                [ BLRR72, FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72
                , LLLL72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RLLL72, RLRR72 ] )
        , ( ( LLRR72 , LRRL72 )
          , Set.fromList
                [ BLRR72, FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72
                , LLLL72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RLLL72, RLRR72 ] )
        , ( ( LLRR72 , LRRR72 )
          , Set.fromList
                [ BLRR72, FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( LLRR72 , LSEL72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72, RLRR72 ] )
        , ( ( LLRR72 , RBRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, ELLS72, ERRS72, FBII72
                , FLLL72, FRRR72, IBIB72, ILLR72, IRRL72, LBLL72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( LLRR72 , RELE72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LLRR72 , RFLL72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, EFBS72, ELLS72, ERRS72, FFBB72
                , FLLL72, FRRR72, IFBI72, ILLR72, IRRL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SFSI72
                , SLSR72, SRSL72 ] )
        , ( ( LLRR72 , RILR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LLRR72 , RLIR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRLL72, LRRR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRR72 , RLLI72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRLL72, LRRR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRR72 , RLLL72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, EFBS72, ELLS72, ERRS72, FFBB72
                , FLLL72, FRRR72, IFBI72, ILLR72, IRRL72, LBLL72, LFRR72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72
                , LRRI72, LRRL72, LRRR72, RFLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SFSI72, SLSR72, SRSL72 ] )
        , ( ( LLRR72 , RLLR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRLL72, LRRR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LLRR72 , RLRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, ELLS72, ERRS72, FBII72
                , FLLL72, FRRR72, IBIB72, ILLR72, IRRL72, LBLL72, LFRR72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72
                , LRRI72, LRRL72, LRRR72, RBRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( LLRR72 , RRBL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LLRR72 , RRFR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LLRR72 , RRLF72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LLRR72 , RRLL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SESE72, SFSI72
                , SISF72, SLSR72, SRSL72 ] )
        , ( ( LLRR72 , RRLR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LLRR72 , RRRB72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LLRR72 , RRRL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LLRR72 , RRRR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, EIFS72, ELLS72, ERRS72
                , ESES72, FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72, IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72
                , LERE72, LFRR72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72
                , RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( LLRR72 , RSER72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LLRR72 , SBSB72 )
          , Set.fromList
                [ LBLL72, LLLL72, LRLL72 ] )
        , ( ( LLRR72 , SESE72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LLRR72 , SFSI72 )
          , Set.fromList
                [ LFRR72, LLRR72, LRRR72 ] )
        , ( ( LLRR72 , SISF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LLRR72 , SLSR72 )
          , Set.fromList
                [ LBLL72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, LRRR72 ] )
        , ( ( LLRR72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LRIL72 , BBBB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LRIL72 , BBFF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LRIL72 , BEIE72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( LRIL72 , BFII72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( LRIL72 , BIIF72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72 ] )
        , ( ( LRIL72 , BLRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( LRIL72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( LRIL72 , BSEF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LRIL72 , EBIS72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( LRIL72 , EFBS72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( LRIL72 , EIFS72 )
          , Set.fromList
                [ RLIR72, RRFR72, RSER72 ] )
        , ( ( LRIL72 , ELLS72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( LRIL72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRIL72 , ESES72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( LRIL72 , FBII72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( LRIL72 , FEFE72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( LRIL72 , FFBB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( LRIL72 , FFFF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( LRIL72 , FIFI72 )
          , Set.fromList
                [ RLIR72, RRFR72, RSER72 ] )
        , ( ( LRIL72 , FLLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( LRIL72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRIL72 , FSEI72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( LRIL72 , IBIB72 )
          , Set.fromList
                [ LLBR72, RLIR72, SLSR72 ] )
        , ( ( LRIL72 , IEBE72 )
          , Set.fromList
                [ LRIL72, RRBL72, SRSL72 ] )
        , ( ( LRIL72 , IFBI72 )
          , Set.fromList
                [ LRIL72, RRBL72, SRSL72 ] )
        , ( ( LRIL72 , IIBF72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72, RRBL72, SRSL72 ] )
        , ( ( LRIL72 , IIFB72 )
          , Set.fromList
                [ LLBR72, RLIR72, RRFR72, RSER72, SLSR72 ] )
        , ( ( LRIL72 , ILLR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72, SESE72, SFSI72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( LRIL72 , IRRL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72, RBRR72, RLRR72
                , RRRB72, RRRL72, RRRR72, SBSB72, SLSR72, SRSL72 ] )
        , ( ( LRIL72 , ISEB72 )
          , Set.fromList
                [ LLBR72, RLIR72, SLSR72 ] )
        , ( ( LRIL72 , LBLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( LRIL72 , LERE72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LRIL72 , LFRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LRIL72 , LIRL72 )
          , Set.fromList
                [ ELLS72, ERRS72, ESES72, FLLL72, FRRR72, FSEI72, ILLR72
                , IRRL72, ISEB72, LLLB72, LLLL72, LLLR72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLI72, RLLL72, RLLR72, RRRB72, RRRL72
                , RRRR72, RSER72 ] )
        , ( ( LRIL72 , LLBR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LRIL72 , LLFL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( LRIL72 , LLLB72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( LRIL72 , LLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( LRIL72 , LLLR72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LRIL72 , LLRF72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LRIL72 , LLRL72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FEFE72, FFFF72, FIFI72, FLLL72
                , FRRR72, IIFB72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LRIL72 , LLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LRIL72 , LRIL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LRIL72 , LRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LRIL72 , LRRI72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRIL72 , LRRL72 )
          , Set.fromList
                [ EBIS72, ELLS72, ERRS72, FBII72, FLLL72, FRRR72, IBIB72
                , ILLR72, IRRL72, LBLL72, LLLB72, LLLL72, LLLR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, RBRR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRIL72 , LRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRIL72 , LSEL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( LRIL72 , RBRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( LRIL72 , RELE72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( LRIL72 , RFLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( LRIL72 , RILR72 )
          , Set.fromList
                [ BLRR72, BRLL72, BSEF72, LLRF72, LLRL72, LLRR72, LRLL72
                , LSEL72, RLRR72, RRLF72, RRLL72, RRLR72, RSER72 ] )
        , ( ( LRIL72 , RLIR72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( LRIL72 , RLLI72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( LRIL72 , RLLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( LRIL72 , RLLR72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, BLRR72, BRLL72, LERE72, LFRR72
                , LIRL72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( LRIL72 , RLRR72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( LRIL72 , RRBL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72
                , RRLL72 ] )
        , ( ( LRIL72 , RRFR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72
                , RRRR72 ] )
        , ( ( LRIL72 , RRLF72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( LRIL72 , RRLL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( LRIL72 , RRLR72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, LBLL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRLL72, RBRR72, RLRR72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRR72 ] )
        , ( ( LRIL72 , RRRB72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LRIL72 , RRRL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, RBRR72, RLRR72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRIL72 , RRRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LRIL72 , RSER72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( LRIL72 , SBSB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LRIL72 , SESE72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( LRIL72 , SFSI72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( LRIL72 , SISF72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72 ] )
        , ( ( LRIL72 , SLSR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( LRIL72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( LRLL72 , BBBB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LRLL72 , BBFF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LRLL72 , BEIE72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( LRLL72 , BFII72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( LRLL72 , BIIF72 )
          , Set.fromList
                [ LBLL72, LLLL72, LRLL72 ] )
        , ( ( LRLL72 , BLRR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LRLL72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72 ] )
        , ( ( LRLL72 , BSEF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LRLL72 , EBIS72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( LRLL72 , EFBS72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( LRLL72 , EIFS72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRR72 ] )
        , ( ( LRLL72 , ELLS72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( LRLL72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LRLL72 , ESES72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( LRLL72 , FBII72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( LRLL72 , FEFE72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( LRLL72 , FFBB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( LRLL72 , FFFF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( LRLL72 , FIFI72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRR72 ] )
        , ( ( LRLL72 , FLLL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( LRLL72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LRLL72 , FSEI72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( LRLL72 , IBIB72 )
          , Set.fromList
                [ BLRR72, LLRR72, RLRR72 ] )
        , ( ( LRLL72 , IEBE72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLL72 ] )
        , ( ( LRLL72 , IFBI72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLL72 ] )
        , ( ( LRLL72 , IIBF72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLL72 ] )
        , ( ( LRLL72 , IIFB72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRR72 ] )
        , ( ( LRLL72 , ILLR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LRLL72 , IRRL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, RBRR72, RLRR72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRLL72 , ISEB72 )
          , Set.fromList
                [ BLRR72, LLRR72, RLRR72 ] )
        , ( ( LRLL72 , LBLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LRLL72 , LERE72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( LRLL72 , LFRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( LRLL72 , LIRL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, ELLS72, ERRS72, FBII72
                , FLLL72, FRRR72, IBIB72, ILLR72, IRRL72, LBLL72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( LRLL72 , LLBR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LRLL72 , LLFL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LRLL72 , LLLB72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LRLL72 , LLLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LRLL72 , LLLR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SESE72, SFSI72
                , SISF72, SLSR72, SRSL72 ] )
        , ( ( LRLL72 , LLRF72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LRLL72 , LLRL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, EIFS72, ELLS72, ERRS72
                , ESES72, FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72, IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72
                , LERE72, LFRR72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72
                , RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( LRLL72 , LLRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LRLL72 , LRIL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LRLL72 , LRLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LRLL72 , LRRI72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( LRLL72 , LRRL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, ELLS72, ERRS72, FBII72
                , FLLL72, FRRR72, IBIB72, ILLR72, IRRL72, LBLL72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( LRLL72 , LRRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( LRLL72 , LSEL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LRLL72 , RBRR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLRR72 ] )
        , ( ( LRLL72 , RELE72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( LRLL72 , RFLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( LRLL72 , RILR72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, LBLL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRLL72, RBRR72, RLRR72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRR72 ] )
        , ( ( LRLL72 , RLIR72 )
          , Set.fromList
                [ BLRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLRR72 ] )
        , ( ( LRLL72 , RLLI72 )
          , Set.fromList
                [ BRLL72, LRLL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( LRLL72 , RLLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( LRLL72 , RLLR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LRLL72 , RLRR72 )
          , Set.fromList
                [ BLRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLRR72 ] )
        , ( ( LRLL72 , RRBL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, RRLL72 ] )
        , ( ( LRLL72 , RRFR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RBRR72
                , RLRR72, RRRR72 ] )
        , ( ( LRLL72 , RRLF72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( LRLL72 , RRLL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( LRLL72 , RRLR72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, LBLL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRLL72, RBRR72, RLRR72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRR72 ] )
        , ( ( LRLL72 , RRRB72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LRLL72 , RRRL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, RBRR72, RLRR72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRLL72 , RRRR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RBRR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRLL72 , RSER72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLRR72 ] )
        , ( ( LRLL72 , SBSB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LRLL72 , SESE72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( LRLL72 , SFSI72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( LRLL72 , SISF72 )
          , Set.fromList
                [ LBLL72, LLLL72, LRLL72 ] )
        , ( ( LRLL72 , SLSR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LRLL72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72 ] )
        , ( ( LRRI72 , BBBB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LRRI72 , BBFF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LRRI72 , BEIE72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( LRRI72 , BFII72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( LRRI72 , BIIF72 )
          , Set.fromList
                [ LERE72, LLRF72, LRRI72 ] )
        , ( ( LRRI72 , BLRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( LRRI72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( LRRI72 , BSEF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( LRRI72 , EBIS72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( LRRI72 , EFBS72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( LRRI72 , EIFS72 )
          , Set.fromList
                [ RELE72, RLLI72, RRLF72 ] )
        , ( ( LRRI72 , ELLS72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( LRRI72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LRRI72 , ESES72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( LRRI72 , FBII72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( LRRI72 , FEFE72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( LRRI72 , FFBB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( LRRI72 , FFFF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( LRRI72 , FIFI72 )
          , Set.fromList
                [ RELE72, RLLI72, RRLF72 ] )
        , ( ( LRRI72 , FLLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( LRRI72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LRRI72 , FSEI72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( LRRI72 , IBIB72 )
          , Set.fromList
                [ ELLS72, LLLB72, RLLI72 ] )
        , ( ( LRRI72 , IEBE72 )
          , Set.fromList
                [ ERRS72, LRRI72, RRRB72 ] )
        , ( ( LRRI72 , IFBI72 )
          , Set.fromList
                [ ERRS72, LRRI72, RRRB72 ] )
        , ( ( LRRI72 , IIBF72 )
          , Set.fromList
                [ ERRS72, LERE72, LLRF72, LRRI72, RRRB72 ] )
        , ( ( LRRI72 , IIFB72 )
          , Set.fromList
                [ ELLS72, LLLB72, RELE72, RLLI72, RRLF72 ] )
        , ( ( LRRI72 , ILLR72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, LFRR72, LLBR72, LLLR72, LLRR72
                , LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( LRRI72 , IRRL72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72, LBLL72, LIRL72
                , LLFL72, LLLL72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LRRI72 , ISEB72 )
          , Set.fromList
                [ ELLS72, LLLB72, RLLI72 ] )
        , ( ( LRRI72 , LBLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( LRRI72 , LERE72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( LRRI72 , LFRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( LRRI72 , LIRL72 )
          , Set.fromList
                [ FEFE72, FLLL72, FRRR72, LERE72, LLFL72, LLLL72, LLRL72
                , LRRR72, RELE72, RLLL72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( LRRI72 , LLBR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RRRR72 ] )
        , ( ( LRRI72 , LLFL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72
                , RRLL72 ] )
        , ( ( LRRI72 , LLLB72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRL72 ] )
        , ( ( LRRI72 , LLLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72, RRBL72
                , RRLL72, RRRL72 ] )
        , ( ( LRRI72 , LLLR72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRRI72 , LLRF72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLR72
                , RRRR72 ] )
        , ( ( LRRI72 , LLRL72 )
          , Set.fromList
                [ FFFF72, FLLL72, FRRR72, LFRR72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRRR72, RFLL72, RLLL72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRR72 ] )
        , ( ( LRRI72 , LLRR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72, RRFR72
                , RRLR72, RRRR72 ] )
        , ( ( LRRI72 , LRIL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( LRRI72 , LRLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( LRRI72 , LRRI72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( LRRI72 , LRRL72 )
          , Set.fromList
                [ FBII72, FIFI72, FLLL72, FRRR72, FSEI72, LBLL72, LIRL72
                , LLFL72, LLLL72, LLRL72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( LRRI72 , LRRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( LRRI72 , LSEL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( LRRI72 , RBRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( LRRI72 , RELE72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( LRRI72 , RFLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( LRRI72 , RILR72 )
          , Set.fromList
                [ BEIE72, BLRR72, BRLL72, IEBE72, ILLR72, IRRL72, LERE72
                , LLBR72, LLLR72, LLRR72, LRIL72, LRLL72, LRRL72, RELE72
                , RLIR72, RLLR72, RLRR72, RRBL72, RRLL72, RRRL72, SESE72
                , SLSR72, SRSL72 ] )
        , ( ( LRRI72 , RLIR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LRRI72 , RLLI72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( LRRI72 , RLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( LRRI72 , RLLR72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, IFBI72, ILLR72, IRRL72, LFRR72
                , LLBR72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , RRBL72, RRLL72, RRRL72, SFSI72, SLSR72, SRSL72 ] )
        , ( ( LRRI72 , RLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LRRI72 , RRBL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( LRRI72 , RRFR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( LRRI72 , RRLF72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( LRRI72 , RRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( LRRI72 , RRLR72 )
          , Set.fromList
                [ BBFF72, BIIF72, BLRR72, BRLL72, BSEF72, IIBF72, ILLR72
                , IRRL72, LBLL72, LIRL72, LLBR72, LLFL72, LLLL72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRL72, RRRR72, RSER72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( LRRI72 , RRRB72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( LRRI72 , RRRL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( LRRI72 , RRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( LRRI72 , RSER72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( LRRI72 , SBSB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( LRRI72 , SESE72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( LRRI72 , SFSI72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( LRRI72 , SISF72 )
          , Set.fromList
                [ LERE72, LLRF72, LRRI72 ] )
        , ( ( LRRI72 , SLSR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( LRRI72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( LRRL72 , BBBB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LRRL72 , BBFF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LRRL72 , BEIE72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( LRRL72 , BFII72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( LRRL72 , BIIF72 )
          , Set.fromList
                [ LIRL72, LLRL72, LRRL72 ] )
        , ( ( LRRL72 , BLRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( LRRL72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( LRRL72 , BSEF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( LRRL72 , EBIS72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( LRRL72 , EFBS72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( LRRL72 , EIFS72 )
          , Set.fromList
                [ RILR72, RLLR72, RRLR72 ] )
        , ( ( LRRL72 , ELLS72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( LRRL72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( LRRL72 , ESES72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( LRRL72 , FBII72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( LRRL72 , FEFE72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( LRRL72 , FFBB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( LRRL72 , FFFF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( LRRL72 , FIFI72 )
          , Set.fromList
                [ RILR72, RLLR72, RRLR72 ] )
        , ( ( LRRL72 , FLLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( LRRL72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( LRRL72 , FSEI72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( LRRL72 , IBIB72 )
          , Set.fromList
                [ ILLR72, LLLR72, RLLR72 ] )
        , ( ( LRRL72 , IEBE72 )
          , Set.fromList
                [ IRRL72, LRRL72, RRRL72 ] )
        , ( ( LRRL72 , IFBI72 )
          , Set.fromList
                [ IRRL72, LRRL72, RRRL72 ] )
        , ( ( LRRL72 , IIBF72 )
          , Set.fromList
                [ IRRL72, LIRL72, LLRL72, LRRL72, RRRL72 ] )
        , ( ( LRRL72 , IIFB72 )
          , Set.fromList
                [ ILLR72, LLLR72, RILR72, RLLR72, RRLR72 ] )
        , ( ( LRRL72 , ILLR72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( LRRL72 , IRRL72 )
          , Set.fromList
                [ IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( LRRL72 , ISEB72 )
          , Set.fromList
                [ ILLR72, LLLR72, RLLR72 ] )
        , ( ( LRRL72 , LBLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LRRL72 , LERE72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRRL72 , LFRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRRL72 , LIRL72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FIFI72, FLLL72, FRRR72, IIFB72
                , ILLR72, IRRL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, LRRI72, LRRL72, LRRR72, RILR72, RLLI72, RLLL72
                , RLLR72, RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRRL72 , LLBR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRRL72 , LLFL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRLF72, RRLL72, RRLR72 ] )
        , ( ( LRRL72 , LLLB72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72 ] )
        , ( ( LRRL72 , LLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( LRRL72 , LLLR72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LRRL72 , LLRF72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRRL72 , LLRL72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FEFE72, FFFF72, FIFI72, FLLL72
                , FRRR72, IIFB72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LRRL72 , LLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRRL72 , LRIL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( LRRL72 , LRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( LRRL72 , LRRI72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( LRRL72 , LRRL72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72, FBII72, FIFI72
                , FLLL72, FRRR72, FSEI72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( LRRL72 , LRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( LRRL72 , LSEL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LRRL72 , RBRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LRRL72 , RELE72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( LRRL72 , RFLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( LRRL72 , RILR72 )
          , Set.fromList
                [ BIIF72, BLRR72, BRLL72, IIBF72, ILLR72, IRRL72, LIRL72
                , LLBR72, LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72
                , LRRL72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SISF72, SLSR72, SRSL72 ] )
        , ( ( LRRL72 , RLIR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LRRL72 , RLLI72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( LRRL72 , RLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( LRRL72 , RLLR72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, BLRR72, BRLL72, IEBE72, IFBI72
                , IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRLF72, RRLL72, RRLR72
                , RRRL72, SESE72, SFSI72, SISF72, SLSR72, SRSL72 ] )
        , ( ( LRRL72 , RLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LRRL72 , RRBL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLL72, RRRL72, SRSL72 ] )
        , ( ( LRRL72 , RRFR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( LRRL72 , RRLF72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( LRRL72 , RRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( LRRL72 , RRLR72 )
          , Set.fromList
                [ BBFF72, BIIF72, BLRR72, BRLL72, BSEF72, IIBF72, ILLR72
                , IRRL72, LBLL72, LIRL72, LLBR72, LLFL72, LLLL72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRL72, RRRR72, RSER72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( LRRL72 , RRRB72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72, RRRL72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( LRRL72 , RRRL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( LRRL72 , RRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LRRL72 , RSER72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( LRRL72 , SBSB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( LRRL72 , SESE72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( LRRL72 , SFSI72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( LRRL72 , SISF72 )
          , Set.fromList
                [ LIRL72, LLRL72, LRRL72 ] )
        , ( ( LRRL72 , SLSR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( LRRL72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( LRRR72 , BBBB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LRRR72 , BBFF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LRRR72 , BEIE72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( LRRR72 , BFII72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( LRRR72 , BIIF72 )
          , Set.fromList
                [ LFRR72, LLRR72, LRRR72 ] )
        , ( ( LRRR72 , BLRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72 ] )
        , ( ( LRRR72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LRRR72 , BSEF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( LRRR72 , EBIS72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( LRRR72 , EFBS72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( LRRR72 , EIFS72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRLL72 ] )
        , ( ( LRRR72 , ELLS72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LRRR72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( LRRR72 , ESES72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( LRRR72 , FBII72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( LRRR72 , FEFE72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( LRRR72 , FFBB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( LRRR72 , FFFF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( LRRR72 , FIFI72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRLL72 ] )
        , ( ( LRRR72 , FLLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LRRR72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( LRRR72 , FSEI72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( LRRR72 , IBIB72 )
          , Set.fromList
                [ FLLL72, LLLL72, RLLL72 ] )
        , ( ( LRRR72 , IEBE72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRRR72 ] )
        , ( ( LRRR72 , IFBI72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRRR72 ] )
        , ( ( LRRR72 , IIBF72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRRR72 ] )
        , ( ( LRRR72 , IIFB72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRLL72 ] )
        , ( ( LRRR72 , ILLR72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRRR72 , IRRL72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72, FSEI72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LRRR72 , ISEB72 )
          , Set.fromList
                [ FLLL72, LLLL72, RLLL72 ] )
        , ( ( LRRR72 , LBLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72 ] )
        , ( ( LRRR72 , LERE72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( LRRR72 , LFRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( LRRR72 , LIRL72 )
          , Set.fromList
                [ FFFF72, FLLL72, FRRR72, LFRR72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRRR72, RFLL72, RLLL72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRR72 ] )
        , ( ( LRRR72 , LLBR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72, RRRR72 ] )
        , ( ( LRRR72 , LLFL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RFLL72
                , RLLL72, RRLL72 ] )
        , ( ( LRRR72 , LLLB72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LRRR72 , LLLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RFLL72
                , RLLL72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRRR72 , LLLR72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( LRRR72 , LLRF72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( LRRR72 , LLRL72 )
          , Set.fromList
                [ FFFF72, FLLL72, FRRR72, LFRR72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRRR72, RFLL72, RLLL72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRR72 ] )
        , ( ( LRRR72 , LLRR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( LRRR72 , LRIL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLL72 ] )
        , ( ( LRRR72 , LRLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLL72 ] )
        , ( ( LRRR72 , LRRI72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( LRRR72 , LRRL72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72, FSEI72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( LRRR72 , LRRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( LRRR72 , LSEL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72 ] )
        , ( ( LRRR72 , RBRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LRRR72 , RELE72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( LRRR72 , RFLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( LRRR72 , RILR72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, EFBS72, ELLS72, ERRS72, FFBB72
                , FLLL72, FRRR72, IFBI72, ILLR72, IRRL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SFSI72
                , SLSR72, SRSL72 ] )
        , ( ( LRRR72 , RLIR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LRRR72 , RLLI72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( LRRR72 , RLLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( LRRR72 , RLLR72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, EFBS72, ELLS72, ERRS72, FFBB72
                , FLLL72, FRRR72, IFBI72, ILLR72, IRRL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SFSI72
                , SLSR72, SRSL72 ] )
        , ( ( LRRR72 , RLRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( LRRR72 , RRBL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LRRR72 , RRFR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LRRR72 , RRLF72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LRRR72 , RRLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( LRRR72 , RRLR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SESE72, SFSI72
                , SISF72, SLSR72, SRSL72 ] )
        , ( ( LRRR72 , RRRB72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LRRR72 , RRRL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, EIFS72, ELLS72, ERRS72
                , ESES72, FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72, IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72
                , LERE72, LFRR72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72
                , RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( LRRR72 , RRRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( LRRR72 , RSER72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( LRRR72 , SBSB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( LRRR72 , SESE72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( LRRR72 , SFSI72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( LRRR72 , SISF72 )
          , Set.fromList
                [ LFRR72, LLRR72, LRRR72 ] )
        , ( ( LRRR72 , SLSR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72 ] )
        , ( ( LRRR72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( LSEL72 , BBBB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LSEL72 , BBFF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LSEL72 , BEIE72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( LSEL72 , BFII72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( LSEL72 , BIIF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LSEL72 , BLRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( LSEL72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( LSEL72 , BSEF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LSEL72 , EBIS72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( LSEL72 , EFBS72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( LSEL72 , EIFS72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( LSEL72 , ELLS72 )
          , Set.fromList
                [ SESE72, SFSI72, SISF72, SLSR72, SRSL72 ] )
        , ( ( LSEL72 , ERRS72 )
          , Set.fromList
                [ SBSB72, SLSR72, SRSL72 ] )
        , ( ( LSEL72 , ESES72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( LSEL72 , FBII72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( LSEL72 , FEFE72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( LSEL72 , FFBB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( LSEL72 , FFFF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( LSEL72 , FIFI72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( LSEL72 , FLLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( LSEL72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LSEL72 , FSEI72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( LSEL72 , IBIB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LSEL72 , IEBE72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( LSEL72 , IFBI72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( LSEL72 , IIBF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LSEL72 , IIFB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LSEL72 , ILLR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( LSEL72 , IRRL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( LSEL72 , ISEB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LSEL72 , LBLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( LSEL72 , LERE72 )
          , Set.fromList
                [ ESES72, FSEI72, ISEB72, LSEL72, RSER72 ] )
        , ( ( LSEL72 , LFRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LSEL72 , LIRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( LSEL72 , LLBR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LSEL72 , LLFL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( LSEL72 , LLLB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72, LLBR72
                , RRBL72 ] )
        , ( ( LSEL72 , LLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( LSEL72 , LLLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LSEL72 , LLRF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72, LLFL72
                , RRFR72 ] )
        , ( ( LSEL72 , LLRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( LSEL72 , LLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( LSEL72 , LRIL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LSEL72 , LRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LSEL72 , LRRI72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72, LRIL72, RLIR72 ] )
        , ( ( LSEL72 , LRRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( LSEL72 , LRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( LSEL72 , LSEL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( LSEL72 , RBRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( LSEL72 , RELE72 )
          , Set.fromList
                [ BSEF72, LSEL72, RSER72 ] )
        , ( ( LSEL72 , RFLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( LSEL72 , RILR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( LSEL72 , RLIR72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( LSEL72 , RLLI72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, LRIL72, RLIR72 ] )
        , ( ( LSEL72 , RLLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( LSEL72 , RLLR72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( LSEL72 , RLRR72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( LSEL72 , RRBL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72
                , RRLL72 ] )
        , ( ( LSEL72 , RRFR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72
                , RRRR72 ] )
        , ( ( LSEL72 , RRLF72 )
          , Set.fromList
                [ BBFF72, LLFL72, RRFR72 ] )
        , ( ( LSEL72 , RRLL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( LSEL72 , RRLR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72
                , RRRR72 ] )
        , ( ( LSEL72 , RRRB72 )
          , Set.fromList
                [ BBBB72, LLBR72, RRBL72 ] )
        , ( ( LSEL72 , RRRL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72
                , RRLL72 ] )
        , ( ( LSEL72 , RRRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( LSEL72 , RSER72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( LSEL72 , SBSB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( LSEL72 , SESE72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( LSEL72 , SFSI72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( LSEL72 , SISF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( LSEL72 , SLSR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( LSEL72 , SRSL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( RBRR72 , BBBB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RBRR72 , BBFF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RBRR72 , BEIE72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( RBRR72 , BFII72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( RBRR72 , BIIF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RBRR72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RBRR72 , BRLL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RBRR72 , BSEF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RBRR72 , EBIS72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( RBRR72 , EFBS72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( RBRR72 , EIFS72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( RBRR72 , ELLS72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72 ] )
        , ( ( RBRR72 , ERRS72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72
                , BSEF72 ] )
        , ( ( RBRR72 , ESES72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( RBRR72 , FBII72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( RBRR72 , FEFE72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( RBRR72 , FFBB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( RBRR72 , FFFF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( RBRR72 , FIFI72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( RBRR72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72 ] )
        , ( ( RBRR72 , FRRR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( RBRR72 , FSEI72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( RBRR72 , IBIB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RBRR72 , IEBE72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( RBRR72 , IFBI72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( RBRR72 , IIBF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RBRR72 , IIFB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RBRR72 , ILLR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RBRR72 , IRRL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RBRR72 , ISEB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RBRR72 , LBLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( RBRR72 , LERE72 )
          , Set.fromList
                [ BBFF72, LBLL72, RBRR72 ] )
        , ( ( RBRR72 , LFRR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLRR72 ] )
        , ( ( RBRR72 , LIRL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( RBRR72 , LLBR72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RBRR72 , LLFL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( RBRR72 , LLLB72 )
          , Set.fromList
                [ BBBB72, LLRR72, RRLL72 ] )
        , ( ( RBRR72 , LLLL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RBRR72 , LLLR72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RBRR72 , LLRF72 )
          , Set.fromList
                [ BBFF72, LLLL72, RRRR72 ] )
        , ( ( RBRR72 , LLRL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( RBRR72 , LLRR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RBRR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RBRR72 , LRIL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RBRR72 , LRLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RBRR72 , LRRI72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, LRLL72
                , RLRR72 ] )
        , ( ( RBRR72 , LRRL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RBRR72 , LRRR72 )
          , Set.fromList
                [ BLRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLRR72 ] )
        , ( ( RBRR72 , LSEL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( RBRR72 , RBRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RBRR72 , RELE72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, LBLL72, RBRR72
                , SBSB72 ] )
        , ( ( RBRR72 , RFLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( RBRR72 , RILR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RBRR72 , RLIR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( RBRR72 , RLLI72 )
          , Set.fromList
                [ BBBB72, EBIS72, FBII72, IBIB72, LRLL72, RLRR72
                , SBSB72 ] )
        , ( ( RBRR72 , RLLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( RBRR72 , RLLR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( RBRR72 , RLRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( RBRR72 , RRBL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RBRR72 , RRFR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RBRR72 , RRLF72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, LLLL72, RRRR72
                , SBSB72 ] )
        , ( ( RBRR72 , RRLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RBRR72 , RRLR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RBRR72 , RRRB72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, LLRR72, RRLL72, SESE72, SFSI72
                , SISF72 ] )
        , ( ( RBRR72 , RRRL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RBRR72 , RRRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RBRR72 , RSER72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RBRR72 , SBSB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RBRR72 , SESE72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( RBRR72 , SFSI72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( RBRR72 , SISF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RBRR72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RBRR72 , SRSL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RELE72 , BBBB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RELE72 , BBFF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RELE72 , BEIE72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( RELE72 , BFII72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( RELE72 , BIIF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RELE72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RELE72 , BRLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RELE72 , BSEF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RELE72 , EBIS72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( RELE72 , EFBS72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( RELE72 , EIFS72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( RELE72 , ELLS72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72 ] )
        , ( ( RELE72 , ERRS72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72 ] )
        , ( ( RELE72 , ESES72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( RELE72 , FBII72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( RELE72 , FEFE72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( RELE72 , FFBB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( RELE72 , FFFF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( RELE72 , FIFI72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( RELE72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( RELE72 , FRRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( RELE72 , FSEI72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( RELE72 , IBIB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RELE72 , IEBE72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( RELE72 , IFBI72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( RELE72 , IIBF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RELE72 , IIFB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RELE72 , ILLR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RELE72 , IRRL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RELE72 , ISEB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RELE72 , LBLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( RELE72 , LERE72 )
          , Set.fromList
                [ BEIE72, IEBE72, LERE72, RELE72, SESE72 ] )
        , ( ( RELE72 , LFRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( RELE72 , LIRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( RELE72 , LLBR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( RELE72 , LLFL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( RELE72 , LLLB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, LLLB72, RRRB72
                , SBSB72 ] )
        , ( ( RELE72 , LLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( RELE72 , LLLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( RELE72 , LLRF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, LLRF72, RRLF72
                , SISF72 ] )
        , ( ( RELE72 , LLRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( RELE72 , LLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( RELE72 , LRIL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( RELE72 , LRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( RELE72 , LRRI72 )
          , Set.fromList
                [ BFII72, IFBI72, LRRI72, RLLI72, SFSI72 ] )
        , ( ( RELE72 , LRRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( RELE72 , LRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( RELE72 , LSEL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( RELE72 , RBRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RELE72 , RELE72 )
          , Set.fromList
                [ FEFE72, LERE72, RELE72 ] )
        , ( ( RELE72 , RFLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( RELE72 , RILR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RELE72 , RLIR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RELE72 , RLLI72 )
          , Set.fromList
                [ FBII72, FIFI72, FSEI72, LRRI72, RLLI72 ] )
        , ( ( RELE72 , RLLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( RELE72 , RLLR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RELE72 , RLRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RELE72 , RRBL72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRL72 ] )
        , ( ( RELE72 , RRFR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLR72
                , RRRR72 ] )
        , ( ( RELE72 , RRLF72 )
          , Set.fromList
                [ FFFF72, LLRF72, RRLF72 ] )
        , ( ( RELE72 , RRLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72, RRBL72
                , RRLL72, RRRL72 ] )
        , ( ( RELE72 , RRLR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLR72
                , RRRR72 ] )
        , ( ( RELE72 , RRRB72 )
          , Set.fromList
                [ FFBB72, LLLB72, RRRB72 ] )
        , ( ( RELE72 , RRRL72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRL72 ] )
        , ( ( RELE72 , RRRR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72, RRFR72
                , RRLR72, RRRR72 ] )
        , ( ( RELE72 , RSER72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RELE72 , SBSB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RELE72 , SESE72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( RELE72 , SFSI72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( RELE72 , SISF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RELE72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RELE72 , SRSL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RFLL72 , BBBB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RFLL72 , BBFF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RFLL72 , BEIE72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( RFLL72 , BFII72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( RFLL72 , BIIF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RFLL72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RFLL72 , BRLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RFLL72 , BSEF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RFLL72 , EBIS72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( RFLL72 , EFBS72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( RFLL72 , EIFS72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( RFLL72 , ELLS72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72 ] )
        , ( ( RFLL72 , ERRS72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72 ] )
        , ( ( RFLL72 , ESES72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( RFLL72 , FBII72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( RFLL72 , FEFE72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( RFLL72 , FFBB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( RFLL72 , FFFF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( RFLL72 , FIFI72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( RFLL72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( RFLL72 , FRRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72 ] )
        , ( ( RFLL72 , FSEI72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( RFLL72 , IBIB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RFLL72 , IEBE72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( RFLL72 , IFBI72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( RFLL72 , IIBF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RFLL72 , IIFB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RFLL72 , ILLR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RFLL72 , IRRL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RFLL72 , ISEB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RFLL72 , LBLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RFLL72 , LERE72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, LFRR72, RFLL72
                , SFSI72 ] )
        , ( ( RFLL72 , LFRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( RFLL72 , LIRL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RFLL72 , LLBR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RFLL72 , LLFL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RFLL72 , LLLB72 )
          , Set.fromList
                [ BBBB72, EBIS72, EIFS72, ESES72, FBII72, FEFE72, FFFF72
                , FIFI72, FSEI72, IBIB72, IIFB72, ISEB72, LLLL72, RRRR72
                , SBSB72 ] )
        , ( ( RFLL72 , LLLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RFLL72 , LLLR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RFLL72 , LLRF72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BSEF72, EFBS72, FFBB72
                , IEBE72, IFBI72, IIBF72, LLRR72, RRLL72, SESE72, SFSI72
                , SISF72 ] )
        , ( ( RFLL72 , LLRL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RFLL72 , LLRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RFLL72 , LRIL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( RFLL72 , LRLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( RFLL72 , LRRI72 )
          , Set.fromList
                [ BFII72, EFBS72, FFBB72, IFBI72, LRRR72, RLLL72
                , SFSI72 ] )
        , ( ( RFLL72 , LRRL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( RFLL72 , LRRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( RFLL72 , LSEL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RFLL72 , RBRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( RFLL72 , RELE72 )
          , Set.fromList
                [ FFFF72, LFRR72, RFLL72 ] )
        , ( ( RFLL72 , RFLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72 ] )
        , ( ( RFLL72 , RILR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( RFLL72 , RLIR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RFLL72 , RLLI72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FSEI72, LRRR72
                , RLLL72 ] )
        , ( ( RFLL72 , RLLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLL72 ] )
        , ( ( RFLL72 , RLLR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RFLL72 , RLRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RFLL72 , RRBL72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RFLL72 , RRFR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( RFLL72 , RRLF72 )
          , Set.fromList
                [ FFFF72, LLRR72, RRLL72 ] )
        , ( ( RFLL72 , RRLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RFLL72
                , RLLL72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RFLL72 , RRLR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( RFLL72 , RRRB72 )
          , Set.fromList
                [ FFBB72, LLLL72, RRRR72 ] )
        , ( ( RFLL72 , RRRL72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RFLL72 , RRRR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RFLL72 , RSER72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( RFLL72 , SBSB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RFLL72 , SESE72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( RFLL72 , SFSI72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( RFLL72 , SISF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RFLL72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RFLL72 , SRSL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RILR72 , BBBB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RILR72 , BBFF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RILR72 , BEIE72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( RILR72 , BFII72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( RILR72 , BIIF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RILR72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RILR72 , BRLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RILR72 , BSEF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RILR72 , EBIS72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( RILR72 , EFBS72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( RILR72 , EIFS72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( RILR72 , ELLS72 )
          , Set.fromList
                [ IBIB72, IIFB72, ILLR72, IRRL72, ISEB72 ] )
        , ( ( RILR72 , ERRS72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72, ILLR72, IRRL72 ] )
        , ( ( RILR72 , ESES72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( RILR72 , FBII72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( RILR72 , FEFE72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( RILR72 , FFBB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( RILR72 , FFFF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( RILR72 , FIFI72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( RILR72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( RILR72 , FRRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( RILR72 , FSEI72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( RILR72 , IBIB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RILR72 , IEBE72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( RILR72 , IFBI72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( RILR72 , IIBF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RILR72 , IIFB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RILR72 , ILLR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RILR72 , IRRL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RILR72 , ISEB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RILR72 , LBLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RILR72 , LERE72 )
          , Set.fromList
                [ BIIF72, IIBF72, LIRL72, RILR72, SISF72 ] )
        , ( ( RILR72 , LFRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( RILR72 , LIRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RILR72 , LLBR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72, RRRL72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( RILR72 , LLFL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RILR72 , LLLB72 )
          , Set.fromList
                [ BBBB72, IBIB72, IIFB72, ISEB72, LLLR72, RRRL72
                , SBSB72 ] )
        , ( ( RILR72 , LLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RILR72 , LLLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72, RRRL72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( RILR72 , LLRF72 )
          , Set.fromList
                [ BBFF72, BIIF72, BSEF72, IIBF72, LLRL72, RRLR72
                , SISF72 ] )
        , ( ( RILR72 , LLRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RILR72 , LLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RILR72 , LRIL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RILR72 , LRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RILR72 , LRRI72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, IEBE72, IFBI72, IIBF72, LRRL72
                , RLLR72, SESE72, SFSI72, SISF72 ] )
        , ( ( RILR72 , LRRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RILR72 , LRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( RILR72 , LSEL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RILR72 , RBRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RILR72 , RELE72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72, LIRL72, RILR72 ] )
        , ( ( RILR72 , RFLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( RILR72 , RILR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RILR72 , RLIR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( RILR72 , RLLI72 )
          , Set.fromList
                [ EBIS72, EIFS72, ESES72, FBII72, FIFI72, FSEI72, IBIB72
                , IIFB72, ISEB72, LRRL72, RLLR72 ] )
        , ( ( RILR72 , RLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( RILR72 , RLLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( RILR72 , RLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( RILR72 , RRBL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72 ] )
        , ( ( RILR72 , RRFR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RILR72 , RRLF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72, LLRL72
                , RRLR72 ] )
        , ( ( RILR72 , RRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RILR72 , RRLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RILR72 , RRRB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72, LLLR72
                , RRRL72 ] )
        , ( ( RILR72 , RRRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72 ] )
        , ( ( RILR72 , RRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RILR72 , RSER72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RILR72 , SBSB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RILR72 , SESE72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( RILR72 , SFSI72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( RILR72 , SISF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RILR72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RILR72 , SRSL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RLIR72 , BBBB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RLIR72 , BBFF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RLIR72 , BEIE72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( RLIR72 , BFII72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( RLIR72 , BIIF72 )
          , Set.fromList
                [ RLIR72, RRFR72, RSER72 ] )
        , ( ( RLIR72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLIR72 , BRLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RLIR72 , BSEF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RLIR72 , EBIS72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( RLIR72 , EFBS72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( RLIR72 , EIFS72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72 ] )
        , ( ( RLIR72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( RLIR72 , ERRS72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( RLIR72 , ESES72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( RLIR72 , FBII72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( RLIR72 , FEFE72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( RLIR72 , FFBB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( RLIR72 , FFFF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( RLIR72 , FIFI72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72 ] )
        , ( ( RLIR72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( RLIR72 , FRRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( RLIR72 , FSEI72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( RLIR72 , IBIB72 )
          , Set.fromList
                [ LRIL72, RRBL72, SRSL72 ] )
        , ( ( RLIR72 , IEBE72 )
          , Set.fromList
                [ LLBR72, RLIR72, SLSR72 ] )
        , ( ( RLIR72 , IFBI72 )
          , Set.fromList
                [ LLBR72, RLIR72, SLSR72 ] )
        , ( ( RLIR72 , IIBF72 )
          , Set.fromList
                [ LLBR72, RLIR72, RRFR72, RSER72, SLSR72 ] )
        , ( ( RLIR72 , IIFB72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72, RRBL72, SRSL72 ] )
        , ( ( RLIR72 , ILLR72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72, RBRR72, RLRR72
                , RRRB72, RRRL72, RRRR72, SBSB72, SLSR72, SRSL72 ] )
        , ( ( RLIR72 , IRRL72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72, SESE72, SFSI72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( RLIR72 , ISEB72 )
          , Set.fromList
                [ LRIL72, RRBL72, SRSL72 ] )
        , ( ( RLIR72 , LBLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RLIR72 , LERE72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( RLIR72 , LFRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( RLIR72 , LIRL72 )
          , Set.fromList
                [ BLRR72, BRLL72, BSEF72, LLRF72, LLRL72, LLRR72, LRLL72
                , LSEL72, RLRR72, RRLF72, RRLL72, RRLR72, RSER72 ] )
        , ( ( RLIR72 , LLBR72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RLIR72 , LLFL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RLIR72 , LLLB72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72
                , RRLL72 ] )
        , ( ( RLIR72 , LLLL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RLIR72 , LLLR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, RBRR72, RLRR72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLIR72 , LLRF72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72
                , RRRR72 ] )
        , ( ( RLIR72 , LLRL72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, LBLL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRLL72, RBRR72, RLRR72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRR72 ] )
        , ( ( RLIR72 , LLRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RLIR72 , LRIL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RLIR72 , LRLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RLIR72 , LRRI72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( RLIR72 , LRRL72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, BLRR72, BRLL72, LERE72, LFRR72
                , LIRL72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RLIR72 , LRRR72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( RLIR72 , LSEL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RLIR72 , RBRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RLIR72 , RELE72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( RLIR72 , RFLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( RLIR72 , RILR72 )
          , Set.fromList
                [ ELLS72, ERRS72, ESES72, FLLL72, FRRR72, FSEI72, ILLR72
                , IRRL72, ISEB72, LLLB72, LLLL72, LLLR72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLI72, RLLL72, RLLR72, RRRB72, RRRL72
                , RRRR72, RSER72 ] )
        , ( ( RLIR72 , RLIR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLIR72 , RLLI72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( RLIR72 , RLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( RLIR72 , RLLR72 )
          , Set.fromList
                [ EBIS72, ELLS72, ERRS72, FBII72, FLLL72, FRRR72, IBIB72
                , ILLR72, IRRL72, LBLL72, LLLB72, LLLL72, LLLR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, RBRR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLIR72 , RLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLIR72 , RRBL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RLIR72 , RRFR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RLIR72 , RRLF72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RLIR72 , RRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RLIR72 , RRLR72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FEFE72, FFFF72, FIFI72, FLLL72
                , FRRR72, IIFB72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RLIR72 , RRRB72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RLIR72 , RRRL72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RLIR72 , RRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RLIR72 , RSER72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RLIR72 , SBSB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RLIR72 , SESE72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( RLIR72 , SFSI72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( RLIR72 , SISF72 )
          , Set.fromList
                [ RLIR72, RRFR72, RSER72 ] )
        , ( ( RLIR72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLIR72 , SRSL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RLLI72 , BBBB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RLLI72 , BBFF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RLLI72 , BEIE72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( RLLI72 , BFII72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( RLLI72 , BIIF72 )
          , Set.fromList
                [ RELE72, RLLI72, RRLF72 ] )
        , ( ( RLLI72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RLLI72 , BRLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RLLI72 , BSEF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RLLI72 , EBIS72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( RLLI72 , EFBS72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( RLLI72 , EIFS72 )
          , Set.fromList
                [ LERE72, LLRF72, LRRI72 ] )
        , ( ( RLLI72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( RLLI72 , ERRS72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( RLLI72 , ESES72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( RLLI72 , FBII72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( RLLI72 , FEFE72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( RLLI72 , FFBB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( RLLI72 , FFFF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( RLLI72 , FIFI72 )
          , Set.fromList
                [ LERE72, LLRF72, LRRI72 ] )
        , ( ( RLLI72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72 ] )
        , ( ( RLLI72 , FRRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLR72, LLRR72, LRRR72 ] )
        , ( ( RLLI72 , FSEI72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( RLLI72 , IBIB72 )
          , Set.fromList
                [ ERRS72, LRRI72, RRRB72 ] )
        , ( ( RLLI72 , IEBE72 )
          , Set.fromList
                [ ELLS72, LLLB72, RLLI72 ] )
        , ( ( RLLI72 , IFBI72 )
          , Set.fromList
                [ ELLS72, LLLB72, RLLI72 ] )
        , ( ( RLLI72 , IIBF72 )
          , Set.fromList
                [ ELLS72, LLLB72, RELE72, RLLI72, RRLF72 ] )
        , ( ( RLLI72 , IIFB72 )
          , Set.fromList
                [ ERRS72, LERE72, LLRF72, LRRI72, RRRB72 ] )
        , ( ( RLLI72 , ILLR72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72, LBLL72, LIRL72
                , LLFL72, LLLL72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RLLI72 , IRRL72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, LFRR72, LLBR72, LLLR72, LLRR72
                , LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RLLI72 , ISEB72 )
          , Set.fromList
                [ ERRS72, LRRI72, RRRB72 ] )
        , ( ( RLLI72 , LBLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( RLLI72 , LERE72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( RLLI72 , LFRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RLIR72, RLLR72
                , RLRR72, SLSR72 ] )
        , ( ( RLLI72 , LIRL72 )
          , Set.fromList
                [ BEIE72, BLRR72, BRLL72, IEBE72, ILLR72, IRRL72, LERE72
                , LLBR72, LLLR72, LLRR72, LRIL72, LRLL72, LRRL72, RELE72
                , RLIR72, RLLR72, RLRR72, RRBL72, RRLL72, RRRL72, SESE72
                , SLSR72, SRSL72 ] )
        , ( ( RLLI72 , LLBR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( RLLI72 , LLFL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( RLLI72 , LLLB72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( RLLI72 , LLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( RLLI72 , LLLR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( RLLI72 , LLRF72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( RLLI72 , LLRL72 )
          , Set.fromList
                [ BBFF72, BIIF72, BLRR72, BRLL72, BSEF72, IIBF72, ILLR72
                , IRRL72, LBLL72, LIRL72, LLBR72, LLFL72, LLLL72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRL72, RRRR72, RSER72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( RLLI72 , LLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( RLLI72 , LRIL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( RLLI72 , LRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( RLLI72 , LRRI72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( RLLI72 , LRRL72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, IFBI72, ILLR72, IRRL72, LFRR72
                , LLBR72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , RRBL72, RRLL72, RRRL72, SFSI72, SLSR72, SRSL72 ] )
        , ( ( RLLI72 , LRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( RLLI72 , LSEL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( RLLI72 , RBRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RLLI72 , RELE72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( RLLI72 , RFLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RLLL72 ] )
        , ( ( RLLI72 , RILR72 )
          , Set.fromList
                [ FEFE72, FLLL72, FRRR72, LERE72, LLFL72, LLLL72, LLRL72
                , LRRR72, RELE72, RLLL72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RLLI72 , RLIR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RLLI72 , RLLI72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( RLLI72 , RLLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72, LRIL72
                , LRLL72, LRRL72, LSEL72, RLLL72 ] )
        , ( ( RLLI72 , RLLR72 )
          , Set.fromList
                [ FBII72, FIFI72, FLLL72, FRRR72, FSEI72, LBLL72, LIRL72
                , LLFL72, LLLL72, LLRL72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RLLI72 , RLRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RLLI72 , RRBL72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRL72 ] )
        , ( ( RLLI72 , RRFR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLR72
                , RRRR72 ] )
        , ( ( RLLI72 , RRLF72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72
                , RRLL72 ] )
        , ( ( RLLI72 , RRLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRL72, RFLL72, RLLL72, RRBL72
                , RRLL72, RRRL72 ] )
        , ( ( RLLI72 , RRLR72 )
          , Set.fromList
                [ FFFF72, FLLL72, FRRR72, LFRR72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRRR72, RFLL72, RLLL72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRR72 ] )
        , ( ( RLLI72 , RRRB72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72
                , RRRR72 ] )
        , ( ( RLLI72 , RRRL72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLLI72 , RRRR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLR72, LLRR72, LRRR72, RRFR72
                , RRLR72, RRRR72 ] )
        , ( ( RLLI72 , RSER72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RLLI72 , SBSB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RLLI72 , SESE72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( RLLI72 , SFSI72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( RLLI72 , SISF72 )
          , Set.fromList
                [ RELE72, RLLI72, RRLF72 ] )
        , ( ( RLLI72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RLLI72 , SRSL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RLLL72 , BBBB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RLLL72 , BBFF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RLLL72 , BEIE72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( RLLL72 , BFII72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( RLLL72 , BIIF72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRLL72 ] )
        , ( ( RLLL72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RLLL72 , BRLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RLLL72 , BSEF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RLLL72 , EBIS72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( RLLL72 , EFBS72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( RLLL72 , EIFS72 )
          , Set.fromList
                [ LFRR72, LLRR72, LRRR72 ] )
        , ( ( RLLL72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( RLLL72 , ERRS72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72 ] )
        , ( ( RLLL72 , ESES72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( RLLL72 , FBII72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( RLLL72 , FEFE72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( RLLL72 , FFBB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( RLLL72 , FFFF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( RLLL72 , FIFI72 )
          , Set.fromList
                [ LFRR72, LLRR72, LRRR72 ] )
        , ( ( RLLL72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( RLLL72 , FRRR72 )
          , Set.fromList
                [ LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72 ] )
        , ( ( RLLL72 , FSEI72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( RLLL72 , IBIB72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRRR72 ] )
        , ( ( RLLL72 , IEBE72 )
          , Set.fromList
                [ FLLL72, LLLL72, RLLL72 ] )
        , ( ( RLLL72 , IFBI72 )
          , Set.fromList
                [ FLLL72, LLLL72, RLLL72 ] )
        , ( ( RLLL72 , IIBF72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRLL72 ] )
        , ( ( RLLL72 , IIFB72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRRR72 ] )
        , ( ( RLLL72 , ILLR72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72, FSEI72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RLLL72 , IRRL72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLLL72 , ISEB72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRRR72 ] )
        , ( ( RLLL72 , LBLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RLLL72 , LERE72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( RLLL72 , LFRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( RLLL72 , LIRL72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, EFBS72, ELLS72, ERRS72, FFBB72
                , FLLL72, FRRR72, IFBI72, ILLR72, IRRL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SFSI72
                , SLSR72, SRSL72 ] )
        , ( ( RLLL72 , LLBR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RLLL72 , LLFL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RLLL72 , LLLB72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RLLL72 , LLLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RLLL72 , LLLR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, EIFS72, ELLS72, ERRS72
                , ESES72, FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72, IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72
                , LERE72, LFRR72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72
                , RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( RLLL72 , LLRF72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RLLL72 , LLRL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SESE72, SFSI72
                , SISF72, SLSR72, SRSL72 ] )
        , ( ( RLLL72 , LLRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RLLL72 , LRIL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( RLLL72 , LRLL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( RLLL72 , LRRI72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( RLLL72 , LRRL72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, EFBS72, ELLS72, ERRS72, FFBB72
                , FLLL72, FRRR72, IFBI72, ILLR72, IRRL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SFSI72
                , SLSR72, SRSL72 ] )
        , ( ( RLLL72 , LRRR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LFRR72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRRR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( RLLL72 , LSEL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RLLL72 , RBRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( RLLL72 , RELE72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72 ] )
        , ( ( RLLL72 , RFLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLLL72 ] )
        , ( ( RLLL72 , RILR72 )
          , Set.fromList
                [ FFFF72, FLLL72, FRRR72, LFRR72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRRR72, RFLL72, RLLL72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRR72 ] )
        , ( ( RLLL72 , RLIR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RLLL72 , RLLI72 )
          , Set.fromList
                [ FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLL72 ] )
        , ( ( RLLL72 , RLLL72 )
          , Set.fromList
                [ FLLL72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLL72 ] )
        , ( ( RLLL72 , RLLR72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72, FSEI72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RLLL72 , RLRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RLLL72 , RRBL72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RLLL72 , RRFR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( RLLL72 , RRLF72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RFLL72
                , RLLL72, RRLL72 ] )
        , ( ( RLLL72 , RRLL72 )
          , Set.fromList
                [ FLLL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RFLL72
                , RLLL72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLLL72 , RRLR72 )
          , Set.fromList
                [ FFFF72, FLLL72, FRRR72, LFRR72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRRR72, RFLL72, RLLL72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRR72 ] )
        , ( ( RLLL72 , RRRB72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72, RRRR72 ] )
        , ( ( RLLL72 , RRRL72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRRR72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLLL72 , RRRR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RLLL72 , RSER72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( RLLL72 , SBSB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RLLL72 , SESE72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( RLLL72 , SFSI72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( RLLL72 , SISF72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRLL72 ] )
        , ( ( RLLL72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RLLL72 , SRSL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RLLR72 , BBBB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RLLR72 , BBFF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RLLR72 , BEIE72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( RLLR72 , BFII72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( RLLR72 , BIIF72 )
          , Set.fromList
                [ RILR72, RLLR72, RRLR72 ] )
        , ( ( RLLR72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RLLR72 , BRLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RLLR72 , BSEF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RLLR72 , EBIS72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( RLLR72 , EFBS72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( RLLR72 , EIFS72 )
          , Set.fromList
                [ LIRL72, LLRL72, LRRL72 ] )
        , ( ( RLLR72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( RLLR72 , ERRS72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( RLLR72 , ESES72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( RLLR72 , FBII72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( RLLR72 , FEFE72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( RLLR72 , FFBB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( RLLR72 , FFFF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( RLLR72 , FIFI72 )
          , Set.fromList
                [ LIRL72, LLRL72, LRRL72 ] )
        , ( ( RLLR72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72 ] )
        , ( ( RLLR72 , FRRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLBR72, LLLR72, LLRF72, LLRL72
                , LLRR72, LRRI72, LRRL72, LRRR72 ] )
        , ( ( RLLR72 , FSEI72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( RLLR72 , IBIB72 )
          , Set.fromList
                [ IRRL72, LRRL72, RRRL72 ] )
        , ( ( RLLR72 , IEBE72 )
          , Set.fromList
                [ ILLR72, LLLR72, RLLR72 ] )
        , ( ( RLLR72 , IFBI72 )
          , Set.fromList
                [ ILLR72, LLLR72, RLLR72 ] )
        , ( ( RLLR72 , IIBF72 )
          , Set.fromList
                [ ILLR72, LLLR72, RILR72, RLLR72, RRLR72 ] )
        , ( ( RLLR72 , IIFB72 )
          , Set.fromList
                [ IRRL72, LIRL72, LLRL72, LRRL72, RRRL72 ] )
        , ( ( RLLR72 , ILLR72 )
          , Set.fromList
                [ IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RLLR72 , IRRL72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RLLR72 , ISEB72 )
          , Set.fromList
                [ IRRL72, LRRL72, RRRL72 ] )
        , ( ( RLLR72 , LBLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RLLR72 , LERE72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( RLLR72 , LFRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RLIR72, RLLR72, RLRR72, SLSR72 ] )
        , ( ( RLLR72 , LIRL72 )
          , Set.fromList
                [ BIIF72, BLRR72, BRLL72, IIBF72, ILLR72, IRRL72, LIRL72
                , LLBR72, LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72
                , LRRL72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SISF72, SLSR72, SRSL72 ] )
        , ( ( RLLR72 , LLBR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72, RRRL72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( RLLR72 , LLFL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RLLR72 , LLLB72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLL72, RRRL72, SRSL72 ] )
        , ( ( RLLR72 , LLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RLLR72 , LLLR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( RLLR72 , LLRF72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( RLLR72 , LLRL72 )
          , Set.fromList
                [ BBFF72, BIIF72, BLRR72, BRLL72, BSEF72, IIBF72, ILLR72
                , IRRL72, LBLL72, LIRL72, LLBR72, LLFL72, LLLL72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRL72, RRRR72, RSER72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( RLLR72 , LLRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RLLR72 , LRIL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RLLR72 , LRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RLLR72 , LRRI72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( RLLR72 , LRRL72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, BLRR72, BRLL72, IEBE72, IFBI72
                , IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRLF72, RRLL72, RRLR72
                , RRRL72, SESE72, SFSI72, SISF72, SLSR72, SRSL72 ] )
        , ( ( RLLR72 , LRRR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LERE72, LFRR72, LIRL72, LLBR72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RLIR72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( RLLR72 , LSEL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RLLR72 , RBRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLLR72 , RELE72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( RLLR72 , RFLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( RLLR72 , RILR72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FIFI72, FLLL72, FRRR72, IIFB72
                , ILLR72, IRRL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, LRRI72, LRRL72, LRRR72, RILR72, RLLI72, RLLL72
                , RLLR72, RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLLR72 , RLIR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( RLLR72 , RLLI72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( RLLR72 , RLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LIRL72, LLFL72, LLLB72
                , LLLL72, LLLR72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RLLI72, RLLL72, RLLR72 ] )
        , ( ( RLLR72 , RLLR72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72, FBII72, FIFI72
                , FLLL72, FRRR72, FSEI72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RLLR72 , RLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( RLLR72 , RRBL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72 ] )
        , ( ( RLLR72 , RRFR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLLR72 , RRLF72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRLF72, RRLL72, RRLR72 ] )
        , ( ( RLLR72 , RRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72
                , RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RLLR72 , RRLR72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FEFE72, FFFF72, FIFI72, FLLL72
                , FRRR72, IIFB72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RLLR72 , RRRB72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLLR72 , RRRL72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RLLR72 , RRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRRI72, LRRL72, LRRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLLR72 , RSER72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLLR72 , SBSB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RLLR72 , SESE72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( RLLR72 , SFSI72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( RLLR72 , SISF72 )
          , Set.fromList
                [ RILR72, RLLR72, RRLR72 ] )
        , ( ( RLLR72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RLLR72 , SRSL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RLRR72 , BBBB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RLRR72 , BBFF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RLRR72 , BEIE72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( RLRR72 , BFII72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( RLRR72 , BIIF72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRR72 ] )
        , ( ( RLRR72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RLRR72 , BRLL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RLRR72 , BSEF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RLRR72 , EBIS72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( RLRR72 , EFBS72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( RLRR72 , EIFS72 )
          , Set.fromList
                [ LBLL72, LLLL72, LRLL72 ] )
        , ( ( RLRR72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72 ] )
        , ( ( RLRR72 , ERRS72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( RLRR72 , ESES72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( RLRR72 , FBII72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( RLRR72 , FEFE72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( RLRR72 , FFBB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( RLRR72 , FFFF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( RLRR72 , FIFI72 )
          , Set.fromList
                [ LBLL72, LLLL72, LRLL72 ] )
        , ( ( RLRR72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72 ] )
        , ( ( RLRR72 , FRRR72 )
          , Set.fromList
                [ LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72 ] )
        , ( ( RLRR72 , FSEI72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( RLRR72 , IBIB72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLL72 ] )
        , ( ( RLRR72 , IEBE72 )
          , Set.fromList
                [ BLRR72, LLRR72, RLRR72 ] )
        , ( ( RLRR72 , IFBI72 )
          , Set.fromList
                [ BLRR72, LLRR72, RLRR72 ] )
        , ( ( RLRR72 , IIBF72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRR72 ] )
        , ( ( RLRR72 , IIFB72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLL72 ] )
        , ( ( RLRR72 , ILLR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, RBRR72, RLRR72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLRR72 , IRRL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RLRR72 , ISEB72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLL72 ] )
        , ( ( RLRR72 , LBLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( RLRR72 , LERE72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLRR72 ] )
        , ( ( RLRR72 , LFRR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72
                , RLRR72 ] )
        , ( ( RLRR72 , LIRL72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, LBLL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRLL72, RBRR72, RLRR72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRR72 ] )
        , ( ( RLRR72 , LLBR72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RLRR72 , LLFL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( RLRR72 , LLLB72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, RRLL72 ] )
        , ( ( RLRR72 , LLLL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLBR72, LLLB72, LLLL72, LLLR72, LLRR72
                , LRLL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RLRR72 , LLLR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, RBRR72, RLRR72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLRR72 , LLRF72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RBRR72
                , RLRR72, RRRR72 ] )
        , ( ( RLRR72 , LLRL72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, LBLL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRLL72, RBRR72, RLRR72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRR72 ] )
        , ( ( RLRR72 , LLRR72 )
          , Set.fromList
                [ BLRR72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, RBRR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RLRR72 , LRIL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RLRR72 , LRLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RLRR72 , LRRI72 )
          , Set.fromList
                [ BLRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLRR72 ] )
        , ( ( RLRR72 , LRRL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RLRR72 , LRRR72 )
          , Set.fromList
                [ BLRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLRR72 ] )
        , ( ( RLRR72 , LSEL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72 ] )
        , ( ( RLRR72 , RBRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RLRR72 , RELE72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( RLRR72 , RFLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , SLSR72 ] )
        , ( ( RLRR72 , RILR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, ELLS72, ERRS72, FBII72
                , FLLL72, FRRR72, IBIB72, ILLR72, IRRL72, LBLL72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( RLRR72 , RLIR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( RLRR72 , RLLI72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( RLRR72 , RLLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LBLL72, LLBR72, LLLB72
                , LLLL72, LLLR72, LLRR72, LRLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, SLSR72 ] )
        , ( ( RLRR72 , RLLR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, ELLS72, ERRS72, FBII72
                , FLLL72, FRRR72, IBIB72, ILLR72, IRRL72, LBLL72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( RLRR72 , RLRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72, SRSL72 ] )
        , ( ( RLRR72 , RRBL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RLRR72 , RRFR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RLRR72 , RRLF72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RLRR72 , RRLL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RLRR72 , RRLR72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, EIFS72, ELLS72, ERRS72
                , ESES72, FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72, IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72
                , LERE72, LFRR72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72
                , RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( RLRR72 , RRRB72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RLRR72 , RRRL72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SESE72, SFSI72
                , SISF72, SLSR72, SRSL72 ] )
        , ( ( RLRR72 , RRRR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RLRR72 , RSER72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RLRR72 , SBSB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RLRR72 , SESE72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( RLRR72 , SFSI72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( RLRR72 , SISF72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRR72 ] )
        , ( ( RLRR72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RLRR72 , SRSL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RRBL72 , BBBB72 )
          , Set.fromList
                [ LLBR72, RLIR72, RRFR72, RSER72, SLSR72 ] )
        , ( ( RRBL72 , BBFF72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72, RRBL72, SRSL72 ] )
        , ( ( RRBL72 , BEIE72 )
          , Set.fromList
                [ LRIL72, RRBL72, SRSL72 ] )
        , ( ( RRBL72 , BFII72 )
          , Set.fromList
                [ LRIL72, RRBL72, SRSL72 ] )
        , ( ( RRBL72 , BIIF72 )
          , Set.fromList
                [ LRIL72, RRBL72, SRSL72 ] )
        , ( ( RRBL72 , BLRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72, SESE72, SFSI72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( RRBL72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72, RBRR72, RLRR72
                , RRRB72, RRRL72, RRRR72, SBSB72, SLSR72, SRSL72 ] )
        , ( ( RRBL72 , BSEF72 )
          , Set.fromList
                [ LRIL72, RRBL72, SRSL72 ] )
        , ( ( RRBL72 , EBIS72 )
          , Set.fromList
                [ RLIR72, RRFR72, RSER72 ] )
        , ( ( RRBL72 , EFBS72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRBL72 , EIFS72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRBL72 , ELLS72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RRBL72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRBL72 , ESES72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRBL72 , FBII72 )
          , Set.fromList
                [ RLIR72, RRFR72, RSER72 ] )
        , ( ( RRBL72 , FEFE72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRBL72 , FFBB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRBL72 , FFFF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRBL72 , FIFI72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRBL72 , FLLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RRBL72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRBL72 , FSEI72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRBL72 , IBIB72 )
          , Set.fromList
                [ RLIR72, RRFR72, RSER72 ] )
        , ( ( RRBL72 , IEBE72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRBL72 , IFBI72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRBL72 , IIBF72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRBL72 , IIFB72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRBL72 , ILLR72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RRBL72 , IRRL72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRBL72 , ISEB72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRBL72 , LBLL72 )
          , Set.fromList
                [ ELLS72, ERRS72, ESES72, FLLL72, FRRR72, FSEI72, ILLR72
                , IRRL72, ISEB72, LLLB72, LLLL72, LLLR72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLI72, RLLL72, RLLR72, RRRB72, RRRL72
                , RRRR72, RSER72 ] )
        , ( ( RRBL72 , LERE72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRBL72 , LFRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRBL72 , LIRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRBL72 , LLBR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RRBL72 , LLFL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RRBL72 , LLLB72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RRBL72 , LLLL72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FEFE72, FFFF72, FIFI72, FLLL72
                , FRRR72, IIFB72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRBL72 , LLLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RRBL72 , LLRF72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RRBL72 , LLRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RRBL72 , LLRR72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRBL72 , LRIL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRBL72 , LRLL72 )
          , Set.fromList
                [ EBIS72, ELLS72, ERRS72, FBII72, FLLL72, FRRR72, IBIB72
                , ILLR72, IRRL72, LBLL72, LLLB72, LLLL72, LLLR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, RBRR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRBL72 , LRRI72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRBL72 , LRRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRBL72 , LRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRBL72 , LSEL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRBL72 , RBRR72 )
          , Set.fromList
                [ BLRR72, BRLL72, BSEF72, LLRF72, LLRL72, LLRR72, LRLL72
                , LSEL72, RLRR72, RRLF72, RRLL72, RRLR72, RSER72 ] )
        , ( ( RRBL72 , RELE72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRBL72 , RFLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRBL72 , RILR72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRBL72 , RLIR72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRBL72 , RLLI72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRBL72 , RLLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRBL72 , RLLR72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRBL72 , RLRR72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, BLRR72, BRLL72, LERE72, LFRR72
                , LIRL72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRBL72 , RRBL72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RRBL72 , RRFR72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RRBL72 , RRLF72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RRBL72 , RRLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, RBRR72, RLRR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRBL72 , RRLR72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RRBL72 , RRRB72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RRBL72 , RRRL72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RRBL72 , RRRR72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, LBLL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRLL72, RBRR72, RLRR72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRBL72 , RSER72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRBL72 , SBSB72 )
          , Set.fromList
                [ RLIR72, RRFR72, RSER72 ] )
        , ( ( RRBL72 , SESE72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRBL72 , SFSI72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRBL72 , SISF72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRBL72 , SLSR72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RRBL72 , SRSL72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRFR72 , BBBB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRFR72 , BBFF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRFR72 , BEIE72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRFR72 , BFII72 )
          , Set.fromList
                [ RLIR72, RRFR72, RSER72 ] )
        , ( ( RRFR72 , BIIF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRFR72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRFR72 , BRLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RRFR72 , BSEF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRFR72 , EBIS72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRFR72 , EFBS72 )
          , Set.fromList
                [ RLIR72, RRFR72, RSER72 ] )
        , ( ( RRFR72 , EIFS72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRFR72 , ELLS72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRFR72 , ERRS72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RRFR72 , ESES72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRFR72 , FBII72 )
          , Set.fromList
                [ LRIL72, RRBL72, SRSL72 ] )
        , ( ( RRFR72 , FEFE72 )
          , Set.fromList
                [ LRIL72, RRBL72, SRSL72 ] )
        , ( ( RRFR72 , FFBB72 )
          , Set.fromList
                [ LLBR72, RLIR72, RRFR72, RSER72, SLSR72 ] )
        , ( ( RRFR72 , FFFF72 )
          , Set.fromList
                [ LLFL72, LRIL72, LSEL72, RRBL72, SRSL72 ] )
        , ( ( RRFR72 , FIFI72 )
          , Set.fromList
                [ LRIL72, RRBL72, SRSL72 ] )
        , ( ( RRFR72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72, RBRR72, RLRR72
                , RRRB72, RRRL72, RRRR72, SBSB72, SLSR72, SRSL72 ] )
        , ( ( RRFR72 , FRRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72, SESE72, SFSI72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( RRFR72 , FSEI72 )
          , Set.fromList
                [ LRIL72, RRBL72, SRSL72 ] )
        , ( ( RRFR72 , IBIB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRFR72 , IEBE72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRFR72 , IFBI72 )
          , Set.fromList
                [ RLIR72, RRFR72, RSER72 ] )
        , ( ( RRFR72 , IIBF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRFR72 , IIFB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRFR72 , ILLR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRFR72 , IRRL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RRFR72 , ISEB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRFR72 , LBLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRFR72 , LERE72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRFR72 , LFRR72 )
          , Set.fromList
                [ BLRR72, BRLL72, BSEF72, LLRF72, LLRL72, LLRR72, LRLL72
                , LSEL72, RLRR72, RRLF72, RRLL72, RRLR72, RSER72 ] )
        , ( ( RRFR72 , LIRL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRFR72 , LLBR72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RRFR72 , LLFL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RRFR72 , LLLB72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RRFR72 , LLLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, RBRR72, RLRR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRFR72 , LLLR72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RRFR72 , LLRF72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RRFR72 , LLRL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RRFR72 , LLRR72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, LBLL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRLL72, RBRR72, RLRR72, RRFR72, RRLF72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRFR72 , LRIL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRFR72 , LRLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRFR72 , LRRI72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRFR72 , LRRL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRFR72 , LRRR72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, BLRR72, BRLL72, LERE72, LFRR72
                , LIRL72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRFR72 , LSEL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RRFR72 , RBRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRFR72 , RELE72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRFR72 , RFLL72 )
          , Set.fromList
                [ ELLS72, ERRS72, ESES72, FLLL72, FRRR72, FSEI72, ILLR72
                , IRRL72, ISEB72, LLLB72, LLLL72, LLLR72, LRRI72, LRRL72
                , LRRR72, LSEL72, RLLI72, RLLL72, RLLR72, RRRB72, RRRL72
                , RRRR72, RSER72 ] )
        , ( ( RRFR72 , RILR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRFR72 , RLIR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRFR72 , RLLI72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRFR72 , RLLL72 )
          , Set.fromList
                [ EBIS72, ELLS72, ERRS72, FBII72, FLLL72, FRRR72, IBIB72
                , ILLR72, IRRL72, LBLL72, LLLB72, LLLL72, LLLR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, RBRR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRFR72 , RLLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRFR72 , RLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRFR72 , RRBL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RRFR72 , RRFR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RRFR72 , RRLF72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RRFR72 , RRLL72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FEFE72, FFFF72, FIFI72, FLLL72
                , FRRR72, IIFB72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRFR72 , RRLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RRFR72 , RRRB72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RRFR72 , RRRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RRFR72 , RRRR72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRFR72 , RSER72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRFR72 , SBSB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RRFR72 , SESE72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRFR72 , SFSI72 )
          , Set.fromList
                [ RLIR72, RRFR72, RSER72 ] )
        , ( ( RRFR72 , SISF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RRFR72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRFR72 , SRSL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RRLF72 , BBBB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRLF72 , BBFF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRLF72 , BEIE72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRLF72 , BFII72 )
          , Set.fromList
                [ RELE72, RLLI72, RRLF72 ] )
        , ( ( RRLF72 , BIIF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRLF72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRLF72 , BRLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RRLF72 , BSEF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRLF72 , EBIS72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRLF72 , EFBS72 )
          , Set.fromList
                [ RELE72, RLLI72, RRLF72 ] )
        , ( ( RRLF72 , EIFS72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRLF72 , ELLS72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRLF72 , ERRS72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RRLF72 , ESES72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRLF72 , FBII72 )
          , Set.fromList
                [ ERRS72, LRRI72, RRRB72 ] )
        , ( ( RRLF72 , FEFE72 )
          , Set.fromList
                [ ERRS72, LRRI72, RRRB72 ] )
        , ( ( RRLF72 , FFBB72 )
          , Set.fromList
                [ ELLS72, LLLB72, RELE72, RLLI72, RRLF72 ] )
        , ( ( RRLF72 , FFFF72 )
          , Set.fromList
                [ ERRS72, LERE72, LLRF72, LRRI72, RRRB72 ] )
        , ( ( RRLF72 , FIFI72 )
          , Set.fromList
                [ ERRS72, LRRI72, RRRB72 ] )
        , ( ( RRLF72 , FLLL72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72, LBLL72, LIRL72
                , LLFL72, LLLL72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRLF72 , FRRR72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, LFRR72, LLBR72, LLLR72, LLRR72
                , LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RRLF72 , FSEI72 )
          , Set.fromList
                [ ERRS72, LRRI72, RRRB72 ] )
        , ( ( RRLF72 , IBIB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRLF72 , IEBE72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRLF72 , IFBI72 )
          , Set.fromList
                [ RELE72, RLLI72, RRLF72 ] )
        , ( ( RRLF72 , IIBF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRLF72 , IIFB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRLF72 , ILLR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRLF72 , IRRL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RRLF72 , ISEB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRLF72 , LBLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( RRLF72 , LERE72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( RRLF72 , LFRR72 )
          , Set.fromList
                [ BEIE72, BLRR72, BRLL72, IEBE72, ILLR72, IRRL72, LERE72
                , LLBR72, LLLR72, LLRR72, LRIL72, LRLL72, LRRL72, RELE72
                , RLIR72, RLLR72, RLRR72, RRBL72, RRLL72, RRRL72, SESE72
                , SLSR72, SRSL72 ] )
        , ( ( RRLF72 , LIRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( RRLF72 , LLBR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( RRLF72 , LLFL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( RRLF72 , LLLB72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( RRLF72 , LLLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( RRLF72 , LLLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( RRLF72 , LLRF72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( RRLF72 , LLRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( RRLF72 , LLRR72 )
          , Set.fromList
                [ BBFF72, BIIF72, BLRR72, BRLL72, BSEF72, IIBF72, ILLR72
                , IRRL72, LBLL72, LIRL72, LLBR72, LLFL72, LLLL72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRL72, RRRR72, RSER72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( RRLF72 , LRIL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( RRLF72 , LRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( RRLF72 , LRRI72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( RRLF72 , LRRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( RRLF72 , LRRR72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, IFBI72, ILLR72, IRRL72, LFRR72
                , LLBR72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , RRBL72, RRLL72, RRRL72, SFSI72, SLSR72, SRSL72 ] )
        , ( ( RRLF72 , LSEL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( RRLF72 , RBRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RRLF72 , RELE72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RRLF72 , RFLL72 )
          , Set.fromList
                [ FEFE72, FLLL72, FRRR72, LERE72, LLFL72, LLLL72, LLRL72
                , LRRR72, RELE72, RLLL72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RRLF72 , RILR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RRLF72 , RLIR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRLF72 , RLLI72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRLF72 , RLLL72 )
          , Set.fromList
                [ FBII72, FIFI72, FLLL72, FRRR72, FSEI72, LBLL72, LIRL72
                , LLFL72, LLLL72, LLRL72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRLF72 , RLLR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRLF72 , RLRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRLF72 , RRBL72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRL72 ] )
        , ( ( RRLF72 , RRFR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLR72
                , RRRR72 ] )
        , ( ( RRLF72 , RRLF72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLR72
                , RRRR72 ] )
        , ( ( RRLF72 , RRLL72 )
          , Set.fromList
                [ FFFF72, FLLL72, FRRR72, LFRR72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRRR72, RFLL72, RLLL72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRL72, RRRR72 ] )
        , ( ( RRLF72 , RRLR72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLR72
                , RRRR72 ] )
        , ( ( RRLF72 , RRRB72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRL72 ] )
        , ( ( RRLF72 , RRRL72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRL72 ] )
        , ( ( RRLF72 , RRRR72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRRR72, RFLL72, RLLL72, RRBL72, RRFR72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLF72 , RSER72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RRLF72 , SBSB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRLF72 , SESE72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRLF72 , SFSI72 )
          , Set.fromList
                [ RELE72, RLLI72, RRLF72 ] )
        , ( ( RRLF72 , SISF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRLF72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRLF72 , SRSL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RRLL72 , BBBB72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRR72 ] )
        , ( ( RRLL72 , BBFF72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLL72 ] )
        , ( ( RRLL72 , BEIE72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLL72 ] )
        , ( ( RRLL72 , BFII72 )
          , Set.fromList
                [ BRLL72, LRLL72, RFLL72, RLLL72, RRLL72 ] )
        , ( ( RRLL72 , BIIF72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLL72 ] )
        , ( ( RRLL72 , BLRR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRLL72 , BRLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, RBRR72, RFLL72, RLLL72, RLRR72
                , RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLL72 , BSEF72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLL72 ] )
        , ( ( RRLL72 , EBIS72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRR72 ] )
        , ( ( RRLL72 , EFBS72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRLL72 ] )
        , ( ( RRLL72 , EIFS72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RRLL72 , ELLS72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RRLL72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RFLL72, RLLL72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRLL72 , ESES72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RRLL72 , FBII72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RLRR72, RRRR72 ] )
        , ( ( RRLL72 , FEFE72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRRR72 ] )
        , ( ( RRLL72 , FFBB72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRLL72 ] )
        , ( ( RRLL72 , FFFF72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRRR72 ] )
        , ( ( RRLL72 , FIFI72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRRR72 ] )
        , ( ( RRLL72 , FLLL72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72, FSEI72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRLL72 , FRRR72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRRR72, RBRR72, RFLL72, RLLL72, RLRR72
                , RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLL72 , FSEI72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRRR72 ] )
        , ( ( RRLL72 , IBIB72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRR72 ] )
        , ( ( RRLL72 , IEBE72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RRLL72 , IFBI72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRLL72 ] )
        , ( ( RRLL72 , IIBF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RRLL72 , IIFB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RRLL72 , ILLR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RRLL72 , IRRL72 )
          , Set.fromList
                [ RBRR72, RFLL72, RLLL72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRLL72 , ISEB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RRLL72 , LBLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, ELLS72, ERRS72, FBII72
                , FLLL72, FRRR72, IBIB72, ILLR72, IRRL72, LBLL72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( RRLL72 , LERE72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RRLL72 , LFRR72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, EFBS72, ELLS72, ERRS72, FFBB72
                , FLLL72, FRRR72, IFBI72, ILLR72, IRRL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SFSI72
                , SLSR72, SRSL72 ] )
        , ( ( RRLL72 , LIRL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RRLL72 , LLBR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RRLL72 , LLFL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RRLL72 , LLLB72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RRLL72 , LLLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, EIFS72, ELLS72, ERRS72
                , ESES72, FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72, IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72
                , LERE72, LFRR72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72
                , RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( RRLL72 , LLLR72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RRLL72 , LLRF72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RRLL72 , LLRL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RRLL72 , LLRR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SESE72, SFSI72
                , SISF72, SLSR72, SRSL72 ] )
        , ( ( RRLL72 , LRIL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RFLL72, RLLL72, RLRR72, RRBL72
                , RRLL72, RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RRLL72 , LRLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, ELLS72, ERRS72, FBII72
                , FLLL72, FRRR72, IBIB72, ILLR72, IRRL72, LBLL72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RFLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( RRLL72 , LRRI72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RFLL72, RLLL72, RLRR72, RRBL72
                , RRLL72, RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RRLL72 , LRRL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RFLL72, RLLL72, RLRR72, RRBL72
                , RRLL72, RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RRLL72 , LRRR72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, EFBS72, ELLS72, ERRS72, FFBB72
                , FLLL72, FRRR72, IFBI72, ILLR72, IRRL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RFLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SFSI72, SLSR72, SRSL72 ] )
        , ( ( RRLL72 , LSEL72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RRLL72 , RBRR72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, FRRR72, LBLL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRLL72, LRRR72, RBRR72, RLRR72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RRLL72 , RELE72 )
          , Set.fromList
                [ BRLL72, FRRR72, LRLL72, LRRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( RRLL72 , RFLL72 )
          , Set.fromList
                [ BRLL72, FFFF72, FLLL72, FRRR72, LFRR72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRLL72, LRRR72, RFLL72, RLLL72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RRLL72 , RILR72 )
          , Set.fromList
                [ BRLL72, FRRR72, LRLL72, LRRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( RRLL72 , RLIR72 )
          , Set.fromList
                [ BRLL72, FRRR72, LRLL72, LRRR72, RBRR72, RELE72, RFLL72
                , RILR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRLL72 , RLLI72 )
          , Set.fromList
                [ BRLL72, FRRR72, LRLL72, LRRR72, RBRR72, RELE72, RFLL72
                , RILR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRLL72 , RLLL72 )
          , Set.fromList
                [ BRLL72, FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RRLL72 , RLLR72 )
          , Set.fromList
                [ BRLL72, FRRR72, LRLL72, LRRR72, RBRR72, RELE72, RFLL72
                , RILR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRLL72 , RLRR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , FRRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RRLL72 , RRBL72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLLL72, LLRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLL72 , RRFR72 )
          , Set.fromList
                [ BRLL72, FRRR72, LBLL72, LFRR72, LLLL72, LLRR72, LRLL72
                , LRRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RRLL72 , RRLF72 )
          , Set.fromList
                [ BRLL72, FRRR72, LBLL72, LFRR72, LLLL72, LLRR72, LRLL72
                , LRRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RRLL72 , RRLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, FFFF72, FLLL72, FRRR72, LBLL72
                , LFRR72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRLL72, LRRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRLL72 , RRLR72 )
          , Set.fromList
                [ BRLL72, FRRR72, LBLL72, LFRR72, LLLL72, LLRR72, LRLL72
                , LRRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RRLL72 , RRRB72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLLL72, LLRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLL72 , RRRL72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLLL72, LLRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLL72 , RRRR72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, FFBB72, FLLL72, FRRR72, LBLL72
                , LFRR72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRLL72, LRRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRLL72 , RSER72 )
          , Set.fromList
                [ BRLL72, FRRR72, LRLL72, LRRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( RRLL72 , SBSB72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRR72 ] )
        , ( ( RRLL72 , SESE72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RRLL72 , SFSI72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRLL72 ] )
        , ( ( RRLL72 , SISF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RRLL72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RRLL72 , SRSL72 )
          , Set.fromList
                [ RBRR72, RFLL72, RLLL72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRLR72 , BBBB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRLR72 , BBFF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRLR72 , BEIE72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRLR72 , BFII72 )
          , Set.fromList
                [ RILR72, RLLR72, RRLR72 ] )
        , ( ( RRLR72 , BIIF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRLR72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RRLR72 , BRLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRLR72 , BSEF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRLR72 , EBIS72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRLR72 , EFBS72 )
          , Set.fromList
                [ RILR72, RLLR72, RRLR72 ] )
        , ( ( RRLR72 , EIFS72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRLR72 , ELLS72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RRLR72 , ERRS72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRLR72 , ESES72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRLR72 , FBII72 )
          , Set.fromList
                [ IRRL72, LRRL72, RRRL72 ] )
        , ( ( RRLR72 , FEFE72 )
          , Set.fromList
                [ IRRL72, LRRL72, RRRL72 ] )
        , ( ( RRLR72 , FFBB72 )
          , Set.fromList
                [ ILLR72, LLLR72, RILR72, RLLR72, RRLR72 ] )
        , ( ( RRLR72 , FFFF72 )
          , Set.fromList
                [ IRRL72, LIRL72, LLRL72, LRRL72, RRRL72 ] )
        , ( ( RRLR72 , FIFI72 )
          , Set.fromList
                [ IRRL72, LRRL72, RRRL72 ] )
        , ( ( RRLR72 , FLLL72 )
          , Set.fromList
                [ IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RRLR72 , FRRR72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRLR72 , FSEI72 )
          , Set.fromList
                [ IRRL72, LRRL72, RRRL72 ] )
        , ( ( RRLR72 , IBIB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRLR72 , IEBE72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRLR72 , IFBI72 )
          , Set.fromList
                [ RILR72, RLLR72, RRLR72 ] )
        , ( ( RRLR72 , IIBF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRLR72 , IIFB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRLR72 , ILLR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RRLR72 , IRRL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRLR72 , ISEB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRLR72 , LBLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRLR72 , LERE72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRLR72 , LFRR72 )
          , Set.fromList
                [ BIIF72, BLRR72, BRLL72, IIBF72, ILLR72, IRRL72, LIRL72
                , LLBR72, LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72
                , LRRL72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SISF72, SLSR72, SRSL72 ] )
        , ( ( RRLR72 , LIRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRLR72 , LLBR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72, RRRL72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( RRLR72 , LLFL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRLR72 , LLLB72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72, RRRL72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( RRLR72 , LLLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( RRLR72 , LLLR72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72, RRRL72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( RRLR72 , LLRF72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRLR72 , LLRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRLR72 , LLRR72 )
          , Set.fromList
                [ BBFF72, BIIF72, BLRR72, BRLL72, BSEF72, IIBF72, ILLR72
                , IRRL72, LBLL72, LIRL72, LLBR72, LLFL72, LLLL72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SISF72, SLSR72, SRSL72 ] )
        , ( ( RRLR72 , LRIL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRLR72 , LRLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRLR72 , LRRI72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRLR72 , LRRL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRLR72 , LRRR72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, BLRR72, BRLL72, IEBE72, IFBI72
                , IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRLF72, RRLL72, RRLR72
                , RRRL72, SESE72, SFSI72, SISF72, SLSR72, SRSL72 ] )
        , ( ( RRLR72 , LSEL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRLR72 , RBRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLR72 , RELE72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLR72 , RFLL72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FIFI72, FLLL72, FRRR72, IIFB72
                , ILLR72, IRRL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, LRRI72, LRRL72, LRRR72, RILR72, RLLI72, RLLL72
                , RLLR72, RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLR72 , RILR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLR72 , RLIR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( RRLR72 , RLLI72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( RRLR72 , RLLL72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72, FBII72, FIFI72
                , FLLL72, FRRR72, FSEI72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RRLR72 , RLLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( RRLR72 , RLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( RRLR72 , RRBL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRLR72 , RRFR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLR72 , RRLF72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLR72 , RRLL72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FEFE72, FFFF72, FIFI72, FLLL72
                , FRRR72, IIFB72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRBL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLR72 , RRLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLR72 , RRRB72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRLR72 , RRRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRLR72 , RRRR72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRBL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLR72 , RSER72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRLR72 , SBSB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRLR72 , SESE72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRLR72 , SFSI72 )
          , Set.fromList
                [ RILR72, RLLR72, RRLR72 ] )
        , ( ( RRLR72 , SISF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRLR72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RRLR72 , SRSL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRRB72 , BBBB72 )
          , Set.fromList
                [ ELLS72, LLLB72, RELE72, RLLI72, RRLF72 ] )
        , ( ( RRRB72 , BBFF72 )
          , Set.fromList
                [ ERRS72, LERE72, LLRF72, LRRI72, RRRB72 ] )
        , ( ( RRRB72 , BEIE72 )
          , Set.fromList
                [ ERRS72, LRRI72, RRRB72 ] )
        , ( ( RRRB72 , BFII72 )
          , Set.fromList
                [ ERRS72, LRRI72, RRRB72 ] )
        , ( ( RRRB72 , BIIF72 )
          , Set.fromList
                [ ERRS72, LRRI72, RRRB72 ] )
        , ( ( RRRB72 , BLRR72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, LFRR72, LLBR72, LLLR72, LLRR72
                , LRRR72, RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RRRB72 , BRLL72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72, LBLL72, LIRL72
                , LLFL72, LLLL72, LLRL72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRRB72 , BSEF72 )
          , Set.fromList
                [ ERRS72, LRRI72, RRRB72 ] )
        , ( ( RRRB72 , EBIS72 )
          , Set.fromList
                [ RELE72, RLLI72, RRLF72 ] )
        , ( ( RRRB72 , EFBS72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRRB72 , EIFS72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRRB72 , ELLS72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RRRB72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRRB72 , ESES72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRRB72 , FBII72 )
          , Set.fromList
                [ RELE72, RLLI72, RRLF72 ] )
        , ( ( RRRB72 , FEFE72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRRB72 , FFBB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRRB72 , FFFF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRRB72 , FIFI72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRRB72 , FLLL72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RRRB72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRRB72 , FSEI72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRRB72 , IBIB72 )
          , Set.fromList
                [ RELE72, RLLI72, RRLF72 ] )
        , ( ( RRRB72 , IEBE72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRRB72 , IFBI72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRRB72 , IIBF72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRRB72 , IIFB72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRRB72 , ILLR72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RRRB72 , IRRL72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRRB72 , ISEB72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( RRRB72 , LBLL72 )
          , Set.fromList
                [ FEFE72, FLLL72, FRRR72, LERE72, LLFL72, LLLL72, LLRL72
                , LRRR72, RELE72, RLLL72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RRRB72 , LERE72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RRRB72 , LFRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RRRB72 , LIRL72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RRRB72 , LLBR72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRL72 ] )
        , ( ( RRRB72 , LLFL72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLR72
                , RRRR72 ] )
        , ( ( RRRB72 , LLLB72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRL72 ] )
        , ( ( RRRB72 , LLLL72 )
          , Set.fromList
                [ FFFF72, FLLL72, FRRR72, LFRR72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRRR72, RFLL72, RLLL72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRL72, RRRR72 ] )
        , ( ( RRRB72 , LLLR72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRBL72, RRLL72
                , RRRL72 ] )
        , ( ( RRRB72 , LLRF72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLR72
                , RRRR72 ] )
        , ( ( RRRB72 , LLRL72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRFR72, RRLR72
                , RRRR72 ] )
        , ( ( RRRB72 , LLRR72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRRR72, RFLL72, RLLL72, RRBL72, RRFR72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRB72 , LRIL72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRRB72 , LRLL72 )
          , Set.fromList
                [ FBII72, FIFI72, FLLL72, FRRR72, FSEI72, LBLL72, LIRL72
                , LLFL72, LLLL72, LLRL72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRRB72 , LRRI72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRRB72 , LRRL72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRRB72 , LRRR72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRRB72 , LSEL72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRFR72, RRLR72, RRRR72 ] )
        , ( ( RRRB72 , RBRR72 )
          , Set.fromList
                [ BEIE72, BLRR72, BRLL72, IEBE72, ILLR72, IRRL72, LERE72
                , LLBR72, LLLR72, LLRR72, LRIL72, LRLL72, LRRL72, RELE72
                , RLIR72, RLLR72, RLRR72, RRBL72, RRLL72, RRRL72, SESE72
                , SLSR72, SRSL72 ] )
        , ( ( RRRB72 , RELE72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( RRRB72 , RFLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( RRRB72 , RILR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( RRRB72 , RLIR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( RRRB72 , RLLI72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( RRRB72 , RLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( RRRB72 , RLLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RFLL72, RLLL72
                , RRBL72, RRLL72, RRRL72, SRSL72 ] )
        , ( ( RRRB72 , RLRR72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, IFBI72, ILLR72, IRRL72, LFRR72
                , LLBR72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , RRBL72, RRLL72, RRRL72, SFSI72, SLSR72, SRSL72 ] )
        , ( ( RRRB72 , RRBL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( RRRB72 , RRFR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( RRRB72 , RRLF72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( RRRB72 , RRLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( RRRB72 , RRLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLL72, RRRL72
                , SRSL72 ] )
        , ( ( RRRB72 , RRRB72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( RRRB72 , RRRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRR72, RSER72
                , SLSR72 ] )
        , ( ( RRRB72 , RRRR72 )
          , Set.fromList
                [ BBFF72, BIIF72, BLRR72, BRLL72, BSEF72, IIBF72, ILLR72
                , IRRL72, LBLL72, LIRL72, LLBR72, LLFL72, LLLL72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRL72, RRRR72, RSER72, SISF72
                , SLSR72, SRSL72 ] )
        , ( ( RRRB72 , RSER72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLL72
                , RRRL72, SRSL72 ] )
        , ( ( RRRB72 , SBSB72 )
          , Set.fromList
                [ RELE72, RLLI72, RRLF72 ] )
        , ( ( RRRB72 , SESE72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRRB72 , SFSI72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRRB72 , SISF72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( RRRB72 , SLSR72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRBL72, RRLL72, RRRL72 ] )
        , ( ( RRRB72 , SRSL72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRRL72 , BBBB72 )
          , Set.fromList
                [ ILLR72, LLLR72, RILR72, RLLR72, RRLR72 ] )
        , ( ( RRRL72 , BBFF72 )
          , Set.fromList
                [ IRRL72, LIRL72, LLRL72, LRRL72, RRRL72 ] )
        , ( ( RRRL72 , BEIE72 )
          , Set.fromList
                [ IRRL72, LRRL72, RRRL72 ] )
        , ( ( RRRL72 , BFII72 )
          , Set.fromList
                [ IRRL72, LRRL72, RRRL72 ] )
        , ( ( RRRL72 , BIIF72 )
          , Set.fromList
                [ IRRL72, LRRL72, RRRL72 ] )
        , ( ( RRRL72 , BLRR72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLLR72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRBL72, RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRRL72 , BRLL72 )
          , Set.fromList
                [ IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRL72, LRIL72, LRLL72
                , LRRL72, LSEL72, RBRR72, RILR72, RLIR72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RRRL72 , BSEF72 )
          , Set.fromList
                [ IRRL72, LRRL72, RRRL72 ] )
        , ( ( RRRL72 , EBIS72 )
          , Set.fromList
                [ RILR72, RLLR72, RRLR72 ] )
        , ( ( RRRL72 , EFBS72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRRL72 , EIFS72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRRL72 , ELLS72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRRL72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RRRL72 , ESES72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRRL72 , FBII72 )
          , Set.fromList
                [ RILR72, RLLR72, RRLR72 ] )
        , ( ( RRRL72 , FEFE72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRRL72 , FFBB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRRL72 , FFFF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRRL72 , FIFI72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRRL72 , FLLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRRL72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RRRL72 , FSEI72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRRL72 , IBIB72 )
          , Set.fromList
                [ RILR72, RLLR72, RRLR72 ] )
        , ( ( RRRL72 , IEBE72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRRL72 , IFBI72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRRL72 , IIBF72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRRL72 , IIFB72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRRL72 , ILLR72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRRL72 , IRRL72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RRRL72 , ISEB72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( RRRL72 , LBLL72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FIFI72, FLLL72, FRRR72, IIFB72
                , ILLR72, IRRL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, LRRI72, LRRL72, LRRR72, RILR72, RLLI72, RLLL72
                , RLLR72, RRFR72, RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRL72 , LERE72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRL72 , LFRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRL72 , LIRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRL72 , LLBR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRRL72 , LLFL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRL72 , LLLB72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRRL72 , LLLL72 )
          , Set.fromList
                [ EIFS72, ELLS72, ERRS72, FEFE72, FFFF72, FIFI72, FLLL72
                , FRRR72, IIFB72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLFL72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRBL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRL72 , LLLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRRL72 , LLRF72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRL72 , LLRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRL72 , LLRR72 )
          , Set.fromList
                [ EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72
                , LLBR72, LLLB72, LLLL72, LLLR72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLLI72
                , RLLL72, RLLR72, RRBL72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRL72 , LRIL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( RRRL72 , LRLL72 )
          , Set.fromList
                [ EBIS72, EIFS72, ELLS72, ERRS72, ESES72, FBII72, FIFI72
                , FLLL72, FRRR72, FSEI72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLFL72, LLLB72, LLLL72, LLLR72
                , LLRL72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72
                , RRFR72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RRRL72 , LRRI72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( RRRL72 , LRRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( RRRL72 , LRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72
                , RRRL72, RRRR72, RSER72 ] )
        , ( ( RRRL72 , LSEL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRFR72
                , RRLR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRL72 , RBRR72 )
          , Set.fromList
                [ BIIF72, BLRR72, BRLL72, IIBF72, ILLR72, IRRL72, LIRL72
                , LLBR72, LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72
                , LRRL72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SISF72, SLSR72, SRSL72 ] )
        , ( ( RRRL72 , RELE72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRRL72 , RFLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRRL72 , RILR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRRL72 , RLIR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRRL72 , RLLI72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRRL72 , RLLL72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRRL72 , RLLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RELE72, RFLL72
                , RILR72, RLLI72, RLLL72, RLLR72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRRL72 , RLRR72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, BLRR72, BRLL72, IEBE72, IFBI72
                , IIBF72, ILLR72, IRRL72, LERE72, LFRR72, LIRL72, LLBR72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRLF72, RRLL72, RRLR72
                , RRRL72, SESE72, SFSI72, SISF72, SLSR72, SRSL72 ] )
        , ( ( RRRL72 , RRBL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72, RRRL72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( RRRL72 , RRFR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRRL72 , RRLF72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRRL72 , RRLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, IBIB72, IIFB72, ILLR72, IRRL72
                , ISEB72, LBLL72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( RRRL72 , RRLR72 )
          , Set.fromList
                [ BRLL72, IRRL72, LBLL72, LIRL72, LLFL72, LLLL72, LLRL72
                , LRIL72, LRLL72, LRRL72, LSEL72, RRBL72, RRLF72, RRLL72
                , RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRRL72 , RRRB72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72, RRRL72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( RRRL72 , RRRL72 )
          , Set.fromList
                [ BLRR72, ILLR72, LLBR72, LLLR72, LLRR72, RBRR72, RILR72
                , RLIR72, RLLR72, RLRR72, RRFR72, RRLR72, RRRB72, RRRL72
                , RRRR72, RSER72, SLSR72 ] )
        , ( ( RRRL72 , RRRR72 )
          , Set.fromList
                [ BBFF72, BIIF72, BLRR72, BRLL72, BSEF72, IIBF72, ILLR72
                , IRRL72, LBLL72, LIRL72, LLBR72, LLFL72, LLLL72, LLLR72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRL72, LSEL72
                , RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SISF72, SLSR72, SRSL72 ] )
        , ( ( RRRL72 , RSER72 )
          , Set.fromList
                [ BRLL72, IRRL72, LRIL72, LRLL72, LRRL72, RRBL72, RRLF72
                , RRLL72, RRLR72, RRRL72, SRSL72 ] )
        , ( ( RRRL72 , SBSB72 )
          , Set.fromList
                [ RILR72, RLLR72, RRLR72 ] )
        , ( ( RRRL72 , SESE72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRRL72 , SFSI72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRRL72 , SISF72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( RRRL72 , SLSR72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRBL72
                , RRLF72, RRLL72, RRLR72, RRRL72 ] )
        , ( ( RRRL72 , SRSL72 )
          , Set.fromList
                [ RBRR72, RILR72, RLIR72, RLLR72, RLRR72, RRFR72, RRLR72
                , RRRB72, RRRL72, RRRR72, RSER72 ] )
        , ( ( RRRR72 , BBBB72 )
          , Set.fromList
                [ FLLL72, LLLL72, RFLL72, RLLL72, RRLL72 ] )
        , ( ( RRRR72 , BBFF72 )
          , Set.fromList
                [ FRRR72, LFRR72, LLRR72, LRRR72, RRRR72 ] )
        , ( ( RRRR72 , BEIE72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRRR72 ] )
        , ( ( RRRR72 , BFII72 )
          , Set.fromList
                [ FRRR72, LRRR72, RBRR72, RLRR72, RRRR72 ] )
        , ( ( RRRR72 , BIIF72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRRR72 ] )
        , ( ( RRRR72 , BLRR72 )
          , Set.fromList
                [ FFBB72, FLLL72, FRRR72, LFRR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRRR72, RBRR72, RFLL72, RLLL72, RLRR72
                , RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRR72 , BRLL72 )
          , Set.fromList
                [ FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72, FSEI72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRRR72 , BSEF72 )
          , Set.fromList
                [ FRRR72, LRRR72, RRRR72 ] )
        , ( ( RRRR72 , EBIS72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRLL72 ] )
        , ( ( RRRR72 , EFBS72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRR72 ] )
        , ( ( RRRR72 , EIFS72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RRRR72 , ELLS72 )
          , Set.fromList
                [ RBRR72, RFLL72, RLLL72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRRR72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RRRR72 , ESES72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RRRR72 , FBII72 )
          , Set.fromList
                [ BRLL72, LRLL72, RFLL72, RLLL72, RRLL72 ] )
        , ( ( RRRR72 , FEFE72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLL72 ] )
        , ( ( RRRR72 , FFBB72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRR72 ] )
        , ( ( RRRR72 , FFFF72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLL72 ] )
        , ( ( RRRR72 , FIFI72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLL72 ] )
        , ( ( RRRR72 , FLLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, LBLL72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, LRLL72, RBRR72, RFLL72, RLLL72, RLRR72
                , RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRR72 , FRRR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72
                , RRRR72, RSER72 ] )
        , ( ( RRRR72 , FSEI72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLL72 ] )
        , ( ( RRRR72 , IBIB72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRLL72 ] )
        , ( ( RRRR72 , IEBE72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RRRR72 , IFBI72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRR72 ] )
        , ( ( RRRR72 , IIBF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RRRR72 , IIFB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RRRR72 , ILLR72 )
          , Set.fromList
                [ RBRR72, RFLL72, RLLL72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRRR72 , IRRL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RRRR72 , ISEB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( RRRR72 , LBLL72 )
          , Set.fromList
                [ BRLL72, FFFF72, FLLL72, FRRR72, LFRR72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRLL72, LRRR72, RFLL72, RLLL72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RRRR72 , LERE72 )
          , Set.fromList
                [ BRLL72, FRRR72, LRLL72, LRRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( RRRR72 , LFRR72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, FRRR72, LBLL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRLL72, LRRR72, RBRR72, RLRR72
                , RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RRRR72 , LIRL72 )
          , Set.fromList
                [ BRLL72, FRRR72, LRLL72, LRRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( RRRR72 , LLBR72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLLL72, LLRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRR72 , LLFL72 )
          , Set.fromList
                [ BRLL72, FRRR72, LBLL72, LFRR72, LLLL72, LLRR72, LRLL72
                , LRRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RRRR72 , LLLB72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLLL72, LLRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRR72 , LLLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, FFFF72, FLLL72, FRRR72, LBLL72
                , LFRR72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRLL72, LRRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRRR72 , LLLR72 )
          , Set.fromList
                [ BLRR72, FLLL72, LLLL72, LLRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RRRR72 , LLRF72 )
          , Set.fromList
                [ BRLL72, FRRR72, LBLL72, LFRR72, LLLL72, LLRR72, LRLL72
                , LRRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RRRR72 , LLRL72 )
          , Set.fromList
                [ BRLL72, FRRR72, LBLL72, LFRR72, LLLL72, LLRR72, LRLL72
                , LRRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72 ] )
        , ( ( RRRR72 , LLRR72 )
          , Set.fromList
                [ BBFF72, BLRR72, BRLL72, FFBB72, FLLL72, FRRR72, LBLL72
                , LFRR72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRLL72, LRRR72, RBRR72, RFLL72, RLLL72
                , RLRR72, RRBL72, RRFR72, RRLF72, RRLL72, RRLR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRRR72 , LRIL72 )
          , Set.fromList
                [ BRLL72, FRRR72, LRLL72, LRRR72, RBRR72, RELE72, RFLL72
                , RILR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRRR72 , LRLL72 )
          , Set.fromList
                [ BRLL72, FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RRRR72 , LRRI72 )
          , Set.fromList
                [ BRLL72, FRRR72, LRLL72, LRRR72, RBRR72, RELE72, RFLL72
                , RILR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRRR72 , LRRL72 )
          , Set.fromList
                [ BRLL72, FRRR72, LRLL72, LRRR72, RBRR72, RELE72, RFLL72
                , RILR72, RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRR72, RSER72 ] )
        , ( ( RRRR72 , LRRR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , FRRR72, LBLL72, LERE72, LFRR72, LIRL72, LLFL72, LLLL72
                , LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72
                , LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72 ] )
        , ( ( RRRR72 , LSEL72 )
          , Set.fromList
                [ BRLL72, FRRR72, LRLL72, LRRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72 ] )
        , ( ( RRRR72 , RBRR72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, EFBS72, ELLS72, ERRS72, FFBB72
                , FLLL72, FRRR72, IFBI72, ILLR72, IRRL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RFLL72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SFSI72
                , SLSR72, SRSL72 ] )
        , ( ( RRRR72 , RELE72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RRRR72 , RFLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, ELLS72, ERRS72, FBII72
                , FLLL72, FRRR72, IBIB72, ILLR72, IRRL72, LBLL72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RLIR72, RLLI72, RLLL72, RLLR72
                , RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72, SBSB72
                , SLSR72, SRSL72 ] )
        , ( ( RRRR72 , RILR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RRRR72 , RLIR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RFLL72, RLLL72, RLRR72, RRBL72
                , RRLL72, RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RRRR72 , RLLI72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RFLL72, RLLL72, RLRR72, RRBL72
                , RRLL72, RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RRRR72 , RLLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, ELLS72, ERRS72, FBII72
                , FLLL72, FRRR72, IBIB72, ILLR72, IRRL72, LBLL72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RFLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( RRRR72 , RLLR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RFLL72, RLLL72, RLRR72, RRBL72
                , RRLL72, RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RRRR72 , RLRR72 )
          , Set.fromList
                [ BFII72, BLRR72, BRLL72, EFBS72, ELLS72, ERRS72, FFBB72
                , FLLL72, FRRR72, IFBI72, ILLR72, IRRL72, LFRR72, LLBR72
                , LLLB72, LLLL72, LLLR72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RBRR72, RFLL72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SFSI72, SLSR72, SRSL72 ] )
        , ( ( RRRR72 , RRBL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RRRR72 , RRFR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RRRR72 , RRLF72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RRRR72 , RRLL72 )
          , Set.fromList
                [ BBBB72, BLRR72, BRLL72, EBIS72, EIFS72, ELLS72, ERRS72
                , ESES72, FBII72, FEFE72, FFFF72, FIFI72, FLLL72, FRRR72
                , FSEI72, IBIB72, IIFB72, ILLR72, IRRL72, ISEB72, LBLL72
                , LERE72, LFRR72, LIRL72, LLBR72, LLFL72, LLLB72, LLLL72
                , LLLR72, LLRF72, LLRL72, LLRR72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, LSEL72, RBRR72, RELE72, RFLL72, RILR72
                , RLIR72, RLLI72, RLLL72, RLLR72, RLRR72, RRBL72, RRFR72
                , RRLF72, RRLL72, RRLR72, RRRB72, RRRL72, RRRR72, RSER72
                , SBSB72, SLSR72, SRSL72 ] )
        , ( ( RRRR72 , RRLR72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLFL72, LLLL72, LLRF72, LLRL72, LLRR72, LRIL72
                , LRLL72, LRRI72, LRRL72, LRRR72, LSEL72, RRBL72, RRLL72
                , RRRB72, RRRL72, RRRR72, SRSL72 ] )
        , ( ( RRRR72 , RRRB72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RRRR72 , RRRL72 )
          , Set.fromList
                [ BLRR72, ELLS72, FLLL72, ILLR72, LLBR72, LLLB72, LLLL72
                , LLLR72, LLRR72, RBRR72, RELE72, RFLL72, RILR72, RLIR72
                , RLLI72, RLLL72, RLLR72, RLRR72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRR72, RSER72, SLSR72 ] )
        , ( ( RRRR72 , RRRR72 )
          , Set.fromList
                [ BBFF72, BEIE72, BFII72, BIIF72, BLRR72, BRLL72, BSEF72
                , EFBS72, ELLS72, ERRS72, FFBB72, FLLL72, FRRR72, IEBE72
                , IFBI72, IIBF72, ILLR72, IRRL72, LBLL72, LERE72, LFRR72
                , LIRL72, LLBR72, LLFL72, LLLB72, LLLL72, LLLR72, LLRF72
                , LLRL72, LLRR72, LRIL72, LRLL72, LRRI72, LRRL72, LRRR72
                , LSEL72, RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72
                , RLLL72, RLLR72, RLRR72, RRBL72, RRFR72, RRLF72, RRLL72
                , RRLR72, RRRB72, RRRL72, RRRR72, RSER72, SESE72, SFSI72
                , SISF72, SLSR72, SRSL72 ] )
        , ( ( RRRR72 , RSER72 )
          , Set.fromList
                [ BRLL72, ERRS72, FRRR72, IRRL72, LRIL72, LRLL72, LRRI72
                , LRRL72, LRRR72, RRBL72, RRLL72, RRRB72, RRRL72, RRRR72
                , SRSL72 ] )
        , ( ( RRRR72 , SBSB72 )
          , Set.fromList
                [ RFLL72, RLLL72, RRLL72 ] )
        , ( ( RRRR72 , SESE72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RRRR72 , SFSI72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRR72 ] )
        , ( ( RRRR72 , SISF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( RRRR72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RFLL72, RLLL72, RLRR72, RRBL72, RRLL72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RRRR72 , SRSL72 )
          , Set.fromList
                [ RBRR72, RELE72, RFLL72, RILR72, RLIR72, RLLI72, RLLL72
                , RLLR72, RLRR72, RRFR72, RRLF72, RRLL72, RRLR72, RRRR72
                , RSER72 ] )
        , ( ( RSER72 , BBBB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RSER72 , BBFF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RSER72 , BEIE72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( RSER72 , BFII72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( RSER72 , BIIF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RSER72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RSER72 , BRLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RSER72 , BSEF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RSER72 , EBIS72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( RSER72 , EFBS72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( RSER72 , EIFS72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( RSER72 , ELLS72 )
          , Set.fromList
                [ SBSB72, SLSR72, SRSL72 ] )
        , ( ( RSER72 , ERRS72 )
          , Set.fromList
                [ SESE72, SFSI72, SISF72, SLSR72, SRSL72 ] )
        , ( ( RSER72 , ESES72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( RSER72 , FBII72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( RSER72 , FEFE72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( RSER72 , FFBB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( RSER72 , FFFF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( RSER72 , FIFI72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( RSER72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( RSER72 , FRRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( RSER72 , FSEI72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( RSER72 , IBIB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RSER72 , IEBE72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( RSER72 , IFBI72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( RSER72 , IIBF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RSER72 , IIFB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RSER72 , ILLR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RSER72 , IRRL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RSER72 , ISEB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RSER72 , LBLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RSER72 , LERE72 )
          , Set.fromList
                [ BSEF72, LSEL72, RSER72 ] )
        , ( ( RSER72 , LFRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( RSER72 , LIRL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RSER72 , LLBR72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RSER72 , LLFL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RSER72 , LLLB72 )
          , Set.fromList
                [ BBBB72, LLBR72, RRBL72 ] )
        , ( ( RSER72 , LLLL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( RSER72 , LLLR72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RSER72 , LLRF72 )
          , Set.fromList
                [ BBFF72, LLFL72, RRFR72 ] )
        , ( ( RSER72 , LLRL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RSER72 , LLRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RSER72 , LRIL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RSER72 , LRLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RSER72 , LRRI72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, LRIL72, RLIR72 ] )
        , ( ( RSER72 , LRRL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RSER72 , LRRR72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( RSER72 , LSEL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( RSER72 , RBRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RSER72 , RELE72 )
          , Set.fromList
                [ ESES72, FSEI72, ISEB72, LSEL72, RSER72 ] )
        , ( ( RSER72 , RFLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( RSER72 , RILR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RSER72 , RLIR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RSER72 , RLLI72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72, LRIL72, RLIR72 ] )
        , ( ( RSER72 , RLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( RSER72 , RLLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RSER72 , RLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RSER72 , RRBL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RSER72 , RRFR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RSER72 , RRLF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72, LLFL72
                , RRFR72 ] )
        , ( ( RSER72 , RRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RSER72 , RRLR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RSER72 , RRRB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72, LLBR72
                , RRBL72 ] )
        , ( ( RSER72 , RRRL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( RSER72 , RRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( RSER72 , RSER72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( RSER72 , SBSB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( RSER72 , SESE72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( RSER72 , SFSI72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( RSER72 , SISF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( RSER72 , SLSR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( RSER72 , SRSL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( SBSB72 , BBBB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72 ] )
        , ( ( SBSB72 , BBFF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72 ] )
        , ( ( SBSB72 , BEIE72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( SBSB72 , BFII72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( SBSB72 , BIIF72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( SBSB72 , BLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72 ] )
        , ( ( SBSB72 , BRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72 ] )
        , ( ( SBSB72 , BSEF72 )
          , Set.fromList
                [ ESES72, FSEI72, ISEB72 ] )
        , ( ( SBSB72 , EBIS72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( SBSB72 , EFBS72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( SBSB72 , EIFS72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( SBSB72 , ELLS72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( SBSB72 , ERRS72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( SBSB72 , ESES72 )
          , Set.fromList
                [ BSEF72 ] )
        , ( ( SBSB72 , FBII72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( SBSB72 , FEFE72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( SBSB72 , FFBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( SBSB72 , FFFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( SBSB72 , FIFI72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( SBSB72 , FLLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( SBSB72 , FRRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( SBSB72 , FSEI72 )
          , Set.fromList
                [ BSEF72 ] )
        , ( ( SBSB72 , IBIB72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( SBSB72 , IEBE72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( SBSB72 , IFBI72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( SBSB72 , IIBF72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( SBSB72 , IIFB72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( SBSB72 , ILLR72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( SBSB72 , IRRL72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( SBSB72 , ISEB72 )
          , Set.fromList
                [ BSEF72 ] )
        , ( ( SBSB72 , LBLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72 ] )
        , ( ( SBSB72 , LERE72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( SBSB72 , LFRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( SBSB72 , LIRL72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( SBSB72 , LLBR72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( SBSB72 , LLFL72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( SBSB72 , LLLB72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( SBSB72 , LLLL72 )
          , Set.fromList
                [ RRLF72, RRLL72, RRLR72 ] )
        , ( ( SBSB72 , LLLR72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( SBSB72 , LLRF72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( SBSB72 , LLRL72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( SBSB72 , LLRR72 )
          , Set.fromList
                [ RRRB72, RRRL72, RRRR72 ] )
        , ( ( SBSB72 , LRIL72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( SBSB72 , LRLL72 )
          , Set.fromList
                [ RLLI72, RLLL72, RLLR72 ] )
        , ( ( SBSB72 , LRRI72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( SBSB72 , LRRL72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( SBSB72 , LRRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( SBSB72 , LSEL72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( SBSB72 , RBRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72 ] )
        , ( ( SBSB72 , RELE72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( SBSB72 , RFLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( SBSB72 , RILR72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( SBSB72 , RLIR72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( SBSB72 , RLLI72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( SBSB72 , RLLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( SBSB72 , RLLR72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( SBSB72 , RLRR72 )
          , Set.fromList
                [ LRRI72, LRRL72, LRRR72 ] )
        , ( ( SBSB72 , RRBL72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( SBSB72 , RRFR72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( SBSB72 , RRLF72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( SBSB72 , RRLL72 )
          , Set.fromList
                [ LLLB72, LLLL72, LLLR72 ] )
        , ( ( SBSB72 , RRLR72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( SBSB72 , RRRB72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( SBSB72 , RRRL72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( SBSB72 , RRRR72 )
          , Set.fromList
                [ LLRF72, LLRL72, LLRR72 ] )
        , ( ( SBSB72 , RSER72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( SBSB72 , SBSB72 )
          , Set.fromList
                [ SESE72, SFSI72, SISF72 ] )
        , ( ( SBSB72 , SESE72 )
          , Set.fromList
                [ SBSB72 ] )
        , ( ( SBSB72 , SFSI72 )
          , Set.fromList
                [ SBSB72 ] )
        , ( ( SBSB72 , SISF72 )
          , Set.fromList
                [ SBSB72 ] )
        , ( ( SBSB72 , SLSR72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( SBSB72 , SRSL72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( SESE72 , BBBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( SESE72 , BBFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( SESE72 , BEIE72 )
          , Set.fromList
                [ BEIE72 ] )
        , ( ( SESE72 , BFII72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( SESE72 , BIIF72 )
          , Set.fromList
                [ BIIF72 ] )
        , ( ( SESE72 , BLRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( SESE72 , BRLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( SESE72 , BSEF72 )
          , Set.fromList
                [ BSEF72 ] )
        , ( ( SESE72 , EBIS72 )
          , Set.fromList
                [ EBIS72 ] )
        , ( ( SESE72 , EFBS72 )
          , Set.fromList
                [ EFBS72 ] )
        , ( ( SESE72 , EIFS72 )
          , Set.fromList
                [ EIFS72 ] )
        , ( ( SESE72 , ELLS72 )
          , Set.fromList
                [ ELLS72 ] )
        , ( ( SESE72 , ERRS72 )
          , Set.fromList
                [ ERRS72 ] )
        , ( ( SESE72 , ESES72 )
          , Set.fromList
                [ ESES72 ] )
        , ( ( SESE72 , FBII72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( SESE72 , FEFE72 )
          , Set.fromList
                [ FEFE72 ] )
        , ( ( SESE72 , FFBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( SESE72 , FFFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( SESE72 , FIFI72 )
          , Set.fromList
                [ FIFI72 ] )
        , ( ( SESE72 , FLLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( SESE72 , FRRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( SESE72 , FSEI72 )
          , Set.fromList
                [ FSEI72 ] )
        , ( ( SESE72 , IBIB72 )
          , Set.fromList
                [ IBIB72 ] )
        , ( ( SESE72 , IEBE72 )
          , Set.fromList
                [ IEBE72 ] )
        , ( ( SESE72 , IFBI72 )
          , Set.fromList
                [ IFBI72 ] )
        , ( ( SESE72 , IIBF72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( SESE72 , IIFB72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( SESE72 , ILLR72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( SESE72 , IRRL72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( SESE72 , ISEB72 )
          , Set.fromList
                [ ISEB72 ] )
        , ( ( SESE72 , LBLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( SESE72 , LERE72 )
          , Set.fromList
                [ LERE72 ] )
        , ( ( SESE72 , LFRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( SESE72 , LIRL72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( SESE72 , LLBR72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( SESE72 , LLFL72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( SESE72 , LLLB72 )
          , Set.fromList
                [ LLLB72 ] )
        , ( ( SESE72 , LLLL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( SESE72 , LLLR72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( SESE72 , LLRF72 )
          , Set.fromList
                [ LLRF72 ] )
        , ( ( SESE72 , LLRL72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( SESE72 , LLRR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( SESE72 , LRIL72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( SESE72 , LRLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( SESE72 , LRRI72 )
          , Set.fromList
                [ LRRI72 ] )
        , ( ( SESE72 , LRRL72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( SESE72 , LRRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( SESE72 , LSEL72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( SESE72 , RBRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( SESE72 , RELE72 )
          , Set.fromList
                [ RELE72 ] )
        , ( ( SESE72 , RFLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( SESE72 , RILR72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( SESE72 , RLIR72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( SESE72 , RLLI72 )
          , Set.fromList
                [ RLLI72 ] )
        , ( ( SESE72 , RLLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( SESE72 , RLLR72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( SESE72 , RLRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( SESE72 , RRBL72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( SESE72 , RRFR72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( SESE72 , RRLF72 )
          , Set.fromList
                [ RRLF72 ] )
        , ( ( SESE72 , RRLL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( SESE72 , RRLR72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( SESE72 , RRRB72 )
          , Set.fromList
                [ RRRB72 ] )
        , ( ( SESE72 , RRRL72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( SESE72 , RRRR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( SESE72 , RSER72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( SESE72 , SBSB72 )
          , Set.fromList
                [ SBSB72 ] )
        , ( ( SESE72 , SESE72 )
          , Set.fromList
                [ SESE72 ] )
        , ( ( SESE72 , SFSI72 )
          , Set.fromList
                [ SFSI72 ] )
        , ( ( SESE72 , SISF72 )
          , Set.fromList
                [ SISF72 ] )
        , ( ( SESE72 , SLSR72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( SESE72 , SRSL72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( SFSI72 , BBBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( SFSI72 , BBFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( SFSI72 , BEIE72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( SFSI72 , BFII72 )
          , Set.fromList
                [ BFII72 ] )
        , ( ( SFSI72 , BIIF72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( SFSI72 , BLRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( SFSI72 , BRLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( SFSI72 , BSEF72 )
          , Set.fromList
                [ BSEF72 ] )
        , ( ( SFSI72 , EBIS72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( SFSI72 , EFBS72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( SFSI72 , EIFS72 )
          , Set.fromList
                [ FEFE72, FFFF72, FIFI72 ] )
        , ( ( SFSI72 , ELLS72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( SFSI72 , ERRS72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( SFSI72 , ESES72 )
          , Set.fromList
                [ FSEI72 ] )
        , ( ( SFSI72 , FBII72 )
          , Set.fromList
                [ FBII72 ] )
        , ( ( SFSI72 , FEFE72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( SFSI72 , FFBB72 )
          , Set.fromList
                [ FFBB72 ] )
        , ( ( SFSI72 , FFFF72 )
          , Set.fromList
                [ FFFF72 ] )
        , ( ( SFSI72 , FIFI72 )
          , Set.fromList
                [ FEFE72, FFFF72, FIFI72 ] )
        , ( ( SFSI72 , FLLL72 )
          , Set.fromList
                [ FLLL72 ] )
        , ( ( SFSI72 , FRRR72 )
          , Set.fromList
                [ FRRR72 ] )
        , ( ( SFSI72 , FSEI72 )
          , Set.fromList
                [ FSEI72 ] )
        , ( ( SFSI72 , IBIB72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( SFSI72 , IEBE72 )
          , Set.fromList
                [ EFBS72, FFBB72, IFBI72 ] )
        , ( ( SFSI72 , IFBI72 )
          , Set.fromList
                [ EFBS72, FFBB72, IFBI72 ] )
        , ( ( SFSI72 , IIBF72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72 ] )
        , ( ( SFSI72 , IIFB72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72 ] )
        , ( ( SFSI72 , ILLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72 ] )
        , ( ( SFSI72 , IRRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72 ] )
        , ( ( SFSI72 , ISEB72 )
          , Set.fromList
                [ ESES72, FSEI72, ISEB72 ] )
        , ( ( SFSI72 , LBLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( SFSI72 , LERE72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( SFSI72 , LFRR72 )
          , Set.fromList
                [ LFRR72 ] )
        , ( ( SFSI72 , LIRL72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72 ] )
        , ( ( SFSI72 , LLBR72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( SFSI72 , LLFL72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( SFSI72 , LLLB72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( SFSI72 , LLLL72 )
          , Set.fromList
                [ LLLL72 ] )
        , ( ( SFSI72 , LLLR72 )
          , Set.fromList
                [ LLLB72, LLLL72, LLLR72 ] )
        , ( ( SFSI72 , LLRF72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( SFSI72 , LLRL72 )
          , Set.fromList
                [ LLRF72, LLRL72, LLRR72 ] )
        , ( ( SFSI72 , LLRR72 )
          , Set.fromList
                [ LLRR72 ] )
        , ( ( SFSI72 , LRIL72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( SFSI72 , LRLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( SFSI72 , LRRI72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( SFSI72 , LRRL72 )
          , Set.fromList
                [ LRRI72, LRRL72, LRRR72 ] )
        , ( ( SFSI72 , LRRR72 )
          , Set.fromList
                [ LRRR72 ] )
        , ( ( SFSI72 , LSEL72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( SFSI72 , RBRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( SFSI72 , RELE72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( SFSI72 , RFLL72 )
          , Set.fromList
                [ RFLL72 ] )
        , ( ( SFSI72 , RILR72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72 ] )
        , ( ( SFSI72 , RLIR72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( SFSI72 , RLLI72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( SFSI72 , RLLL72 )
          , Set.fromList
                [ RLLL72 ] )
        , ( ( SFSI72 , RLLR72 )
          , Set.fromList
                [ RLLI72, RLLL72, RLLR72 ] )
        , ( ( SFSI72 , RLRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( SFSI72 , RRBL72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( SFSI72 , RRFR72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( SFSI72 , RRLF72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( SFSI72 , RRLL72 )
          , Set.fromList
                [ RRLL72 ] )
        , ( ( SFSI72 , RRLR72 )
          , Set.fromList
                [ RRLF72, RRLL72, RRLR72 ] )
        , ( ( SFSI72 , RRRB72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( SFSI72 , RRRL72 )
          , Set.fromList
                [ RRRB72, RRRL72, RRRR72 ] )
        , ( ( SFSI72 , RRRR72 )
          , Set.fromList
                [ RRRR72 ] )
        , ( ( SFSI72 , RSER72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( SFSI72 , SBSB72 )
          , Set.fromList
                [ SBSB72 ] )
        , ( ( SFSI72 , SESE72 )
          , Set.fromList
                [ SFSI72 ] )
        , ( ( SFSI72 , SFSI72 )
          , Set.fromList
                [ SFSI72 ] )
        , ( ( SFSI72 , SISF72 )
          , Set.fromList
                [ SESE72, SFSI72, SISF72 ] )
        , ( ( SFSI72 , SLSR72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( SFSI72 , SRSL72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( SISF72 , BBBB72 )
          , Set.fromList
                [ BBBB72 ] )
        , ( ( SISF72 , BBFF72 )
          , Set.fromList
                [ BBFF72 ] )
        , ( ( SISF72 , BEIE72 )
          , Set.fromList
                [ BIIF72 ] )
        , ( ( SISF72 , BFII72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72 ] )
        , ( ( SISF72 , BIIF72 )
          , Set.fromList
                [ BIIF72 ] )
        , ( ( SISF72 , BLRR72 )
          , Set.fromList
                [ BLRR72 ] )
        , ( ( SISF72 , BRLL72 )
          , Set.fromList
                [ BRLL72 ] )
        , ( ( SISF72 , BSEF72 )
          , Set.fromList
                [ BSEF72 ] )
        , ( ( SISF72 , EBIS72 )
          , Set.fromList
                [ IBIB72 ] )
        , ( ( SISF72 , EFBS72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( SISF72 , EIFS72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( SISF72 , ELLS72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( SISF72 , ERRS72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( SISF72 , ESES72 )
          , Set.fromList
                [ ISEB72 ] )
        , ( ( SISF72 , FBII72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72 ] )
        , ( ( SISF72 , FEFE72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( SISF72 , FFBB72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72 ] )
        , ( ( SISF72 , FFFF72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72 ] )
        , ( ( SISF72 , FIFI72 )
          , Set.fromList
                [ EIFS72, FIFI72, IIFB72 ] )
        , ( ( SISF72 , FLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72 ] )
        , ( ( SISF72 , FRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72 ] )
        , ( ( SISF72 , FSEI72 )
          , Set.fromList
                [ ESES72, FSEI72, ISEB72 ] )
        , ( ( SISF72 , IBIB72 )
          , Set.fromList
                [ IBIB72 ] )
        , ( ( SISF72 , IEBE72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( SISF72 , IFBI72 )
          , Set.fromList
                [ IEBE72, IFBI72, IIBF72 ] )
        , ( ( SISF72 , IIBF72 )
          , Set.fromList
                [ IIBF72 ] )
        , ( ( SISF72 , IIFB72 )
          , Set.fromList
                [ IIFB72 ] )
        , ( ( SISF72 , ILLR72 )
          , Set.fromList
                [ ILLR72 ] )
        , ( ( SISF72 , IRRL72 )
          , Set.fromList
                [ IRRL72 ] )
        , ( ( SISF72 , ISEB72 )
          , Set.fromList
                [ ISEB72 ] )
        , ( ( SISF72 , LBLL72 )
          , Set.fromList
                [ LBLL72 ] )
        , ( ( SISF72 , LERE72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( SISF72 , LFRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72 ] )
        , ( ( SISF72 , LIRL72 )
          , Set.fromList
                [ LIRL72 ] )
        , ( ( SISF72 , LLBR72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( SISF72 , LLFL72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( SISF72 , LLLB72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( SISF72 , LLLL72 )
          , Set.fromList
                [ LLLB72, LLLL72, LLLR72 ] )
        , ( ( SISF72 , LLLR72 )
          , Set.fromList
                [ LLLR72 ] )
        , ( ( SISF72 , LLRF72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( SISF72 , LLRL72 )
          , Set.fromList
                [ LLRL72 ] )
        , ( ( SISF72 , LLRR72 )
          , Set.fromList
                [ LLRF72, LLRL72, LLRR72 ] )
        , ( ( SISF72 , LRIL72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( SISF72 , LRLL72 )
          , Set.fromList
                [ LRLL72 ] )
        , ( ( SISF72 , LRRI72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( SISF72 , LRRL72 )
          , Set.fromList
                [ LRRL72 ] )
        , ( ( SISF72 , LRRR72 )
          , Set.fromList
                [ LRRI72, LRRL72, LRRR72 ] )
        , ( ( SISF72 , LSEL72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( SISF72 , RBRR72 )
          , Set.fromList
                [ RBRR72 ] )
        , ( ( SISF72 , RELE72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( SISF72 , RFLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72 ] )
        , ( ( SISF72 , RILR72 )
          , Set.fromList
                [ RILR72 ] )
        , ( ( SISF72 , RLIR72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( SISF72 , RLLI72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( SISF72 , RLLL72 )
          , Set.fromList
                [ RLLI72, RLLL72, RLLR72 ] )
        , ( ( SISF72 , RLLR72 )
          , Set.fromList
                [ RLLR72 ] )
        , ( ( SISF72 , RLRR72 )
          , Set.fromList
                [ RLRR72 ] )
        , ( ( SISF72 , RRBL72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( SISF72 , RRFR72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( SISF72 , RRLF72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( SISF72 , RRLL72 )
          , Set.fromList
                [ RRLF72, RRLL72, RRLR72 ] )
        , ( ( SISF72 , RRLR72 )
          , Set.fromList
                [ RRLR72 ] )
        , ( ( SISF72 , RRRB72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( SISF72 , RRRL72 )
          , Set.fromList
                [ RRRL72 ] )
        , ( ( SISF72 , RRRR72 )
          , Set.fromList
                [ RRRB72, RRRL72, RRRR72 ] )
        , ( ( SISF72 , RSER72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( SISF72 , SBSB72 )
          , Set.fromList
                [ SBSB72 ] )
        , ( ( SISF72 , SESE72 )
          , Set.fromList
                [ SISF72 ] )
        , ( ( SISF72 , SFSI72 )
          , Set.fromList
                [ SESE72, SFSI72, SISF72 ] )
        , ( ( SISF72 , SISF72 )
          , Set.fromList
                [ SISF72 ] )
        , ( ( SISF72 , SLSR72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( SISF72 , SRSL72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( SLSR72 , BBBB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( SLSR72 , BBFF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( SLSR72 , BEIE72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( SLSR72 , BFII72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( SLSR72 , BIIF72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( SLSR72 , BLRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( SLSR72 , BRLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( SLSR72 , BSEF72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( SLSR72 , EBIS72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( SLSR72 , EFBS72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( SLSR72 , EIFS72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( SLSR72 , ELLS72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( SLSR72 , ERRS72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( SLSR72 , ESES72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( SLSR72 , FBII72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( SLSR72 , FEFE72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( SLSR72 , FFBB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( SLSR72 , FFFF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( SLSR72 , FIFI72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( SLSR72 , FLLL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( SLSR72 , FRRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( SLSR72 , FSEI72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( SLSR72 , IBIB72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( SLSR72 , IEBE72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( SLSR72 , IFBI72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( SLSR72 , IIBF72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( SLSR72 , IIFB72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( SLSR72 , ILLR72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( SLSR72 , IRRL72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( SLSR72 , ISEB72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( SLSR72 , LBLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( SLSR72 , LERE72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( SLSR72 , LFRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( SLSR72 , LIRL72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( SLSR72 , LLBR72 )
          , Set.fromList
                [ BBBB72, LLBR72, RRBL72 ] )
        , ( ( SLSR72 , LLFL72 )
          , Set.fromList
                [ BBFF72, LLFL72, RRFR72 ] )
        , ( ( SLSR72 , LLLB72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72
                , RRLL72 ] )
        , ( ( SLSR72 , LLLL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( SLSR72 , LLLR72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72
                , RRLL72 ] )
        , ( ( SLSR72 , LLRF72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72
                , RRRR72 ] )
        , ( ( SLSR72 , LLRL72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72
                , RRRR72 ] )
        , ( ( SLSR72 , LLRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( SLSR72 , LRIL72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, LRIL72, RLIR72 ] )
        , ( ( SLSR72 , LRLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( SLSR72 , LRRI72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( SLSR72 , LRRL72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( SLSR72 , LRRR72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( SLSR72 , LSEL72 )
          , Set.fromList
                [ BSEF72, LSEL72, RSER72 ] )
        , ( ( SLSR72 , RBRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( SLSR72 , RELE72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( SLSR72 , RFLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( SLSR72 , RILR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( SLSR72 , RLIR72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72, LRIL72, RLIR72 ] )
        , ( ( SLSR72 , RLLI72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( SLSR72 , RLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( SLSR72 , RLLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( SLSR72 , RLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( SLSR72 , RRBL72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72, LLBR72
                , RRBL72 ] )
        , ( ( SLSR72 , RRFR72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72, LLFL72
                , RRFR72 ] )
        , ( ( SLSR72 , RRLF72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( SLSR72 , RRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( SLSR72 , RRLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( SLSR72 , RRRB72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( SLSR72 , RRRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( SLSR72 , RRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( SLSR72 , RSER72 )
          , Set.fromList
                [ ESES72, FSEI72, ISEB72, LSEL72, RSER72 ] )
        , ( ( SLSR72 , SBSB72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( SLSR72 , SESE72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( SLSR72 , SFSI72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( SLSR72 , SISF72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( SLSR72 , SLSR72 )
          , Set.fromList
                [ SBSB72, SLSR72, SRSL72 ] )
        , ( ( SLSR72 , SRSL72 )
          , Set.fromList
                [ SESE72, SFSI72, SISF72, SLSR72, SRSL72 ] )
        , ( ( SRSL72 , BBBB72 )
          , Set.fromList
                [ LLBR72 ] )
        , ( ( SRSL72 , BBFF72 )
          , Set.fromList
                [ LLFL72 ] )
        , ( ( SRSL72 , BEIE72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( SRSL72 , BFII72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( SRSL72 , BIIF72 )
          , Set.fromList
                [ LRIL72 ] )
        , ( ( SRSL72 , BLRR72 )
          , Set.fromList
                [ LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72, LRRI72
                , LRRL72, LRRR72 ] )
        , ( ( SRSL72 , BRLL72 )
          , Set.fromList
                [ LBLL72, LLLB72, LLLL72, LLLR72, LRLL72 ] )
        , ( ( SRSL72 , BSEF72 )
          , Set.fromList
                [ LSEL72 ] )
        , ( ( SRSL72 , EBIS72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( SRSL72 , EFBS72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( SRSL72 , EIFS72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( SRSL72 , ELLS72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( SRSL72 , ERRS72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( SRSL72 , ESES72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( SRSL72 , FBII72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( SRSL72 , FEFE72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( SRSL72 , FFBB72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( SRSL72 , FFFF72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( SRSL72 , FIFI72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( SRSL72 , FLLL72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( SRSL72 , FRRR72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( SRSL72 , FSEI72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( SRSL72 , IBIB72 )
          , Set.fromList
                [ RLIR72 ] )
        , ( ( SRSL72 , IEBE72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( SRSL72 , IFBI72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( SRSL72 , IIBF72 )
          , Set.fromList
                [ RRBL72 ] )
        , ( ( SRSL72 , IIFB72 )
          , Set.fromList
                [ RRFR72 ] )
        , ( ( SRSL72 , ILLR72 )
          , Set.fromList
                [ RELE72, RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( SRSL72 , IRRL72 )
          , Set.fromList
                [ RBRR72, RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( SRSL72 , ISEB72 )
          , Set.fromList
                [ RSER72 ] )
        , ( ( SRSL72 , LBLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RLLI72
                , RLLL72, RLLR72 ] )
        , ( ( SRSL72 , LERE72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( SRSL72 , LFRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( SRSL72 , LIRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( SRSL72 , LLBR72 )
          , Set.fromList
                [ EFBS72, FFBB72, IEBE72, IFBI72, IIBF72, LLBR72
                , RRBL72 ] )
        , ( ( SRSL72 , LLFL72 )
          , Set.fromList
                [ EIFS72, FEFE72, FFFF72, FIFI72, IIFB72, LLFL72
                , RRFR72 ] )
        , ( ( SRSL72 , LLLB72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( SRSL72 , LLLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( SRSL72 , LLLR72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LLLB72, LLLL72, LLLR72, RELE72
                , RFLL72, RILR72, RLLI72, RLLL72, RLLR72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( SRSL72 , LLRF72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( SRSL72 , LLRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( SRSL72 , LLRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LERE72, LFRR72, LIRL72, LLRF72
                , LLRL72, LLRR72, LRRI72, LRRL72, LRRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( SRSL72 , LRIL72 )
          , Set.fromList
                [ EBIS72, FBII72, IBIB72, LRIL72, RLIR72 ] )
        , ( ( SRSL72 , LRLL72 )
          , Set.fromList
                [ ELLS72, FLLL72, ILLR72, LBLL72, LLLB72, LLLL72, LLLR72
                , LRLL72, RLLI72, RLLL72, RLLR72 ] )
        , ( ( SRSL72 , LRRI72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( SRSL72 , LRRL72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( SRSL72 , LRRR72 )
          , Set.fromList
                [ ERRS72, FRRR72, IRRL72, LRRI72, LRRL72, LRRR72, RBRR72
                , RLRR72, RRRB72, RRRL72, RRRR72 ] )
        , ( ( SRSL72 , LSEL72 )
          , Set.fromList
                [ ESES72, FSEI72, ISEB72, LSEL72, RSER72 ] )
        , ( ( SRSL72 , RBRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RLRR72 ] )
        , ( ( SRSL72 , RELE72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( SRSL72 , RFLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( SRSL72 , RILR72 )
          , Set.fromList
                [ BRLL72, LRLL72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( SRSL72 , RLIR72 )
          , Set.fromList
                [ BEIE72, BFII72, BIIF72, LRIL72, RLIR72 ] )
        , ( ( SRSL72 , RLLI72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( SRSL72 , RLLL72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( SRSL72 , RLLR72 )
          , Set.fromList
                [ BRLL72, LRLL72, RELE72, RFLL72, RILR72, RLLI72, RLLL72
                , RLLR72, RRLF72, RRLL72, RRLR72 ] )
        , ( ( SRSL72 , RLRR72 )
          , Set.fromList
                [ BLRR72, LERE72, LFRR72, LIRL72, LLRF72, LLRL72, LLRR72
                , LRRI72, LRRL72, LRRR72, RLRR72 ] )
        , ( ( SRSL72 , RRBL72 )
          , Set.fromList
                [ BBBB72, LLBR72, RRBL72 ] )
        , ( ( SRSL72 , RRFR72 )
          , Set.fromList
                [ BBFF72, LLFL72, RRFR72 ] )
        , ( ( SRSL72 , RRLF72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( SRSL72 , RRLL72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLB72, LLLL72, LLLR72, LRLL72, RRLF72
                , RRLL72, RRLR72 ] )
        , ( ( SRSL72 , RRLR72 )
          , Set.fromList
                [ BRLL72, LBLL72, LLLL72, LRLL72, RRLF72, RRLL72
                , RRLR72 ] )
        , ( ( SRSL72 , RRRB72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( SRSL72 , RRRL72 )
          , Set.fromList
                [ BLRR72, LLRR72, RBRR72, RLRR72, RRRB72, RRRL72
                , RRRR72 ] )
        , ( ( SRSL72 , RRRR72 )
          , Set.fromList
                [ BLRR72, LLRF72, LLRL72, LLRR72, RBRR72, RLRR72, RRRB72
                , RRRL72, RRRR72 ] )
        , ( ( SRSL72 , RSER72 )
          , Set.fromList
                [ BSEF72, LSEL72, RSER72 ] )
        , ( ( SRSL72 , SBSB72 )
          , Set.fromList
                [ SLSR72 ] )
        , ( ( SRSL72 , SESE72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( SRSL72 , SFSI72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( SRSL72 , SISF72 )
          , Set.fromList
                [ SRSL72 ] )
        , ( ( SRSL72 , SLSR72 )
          , Set.fromList
                [ SESE72, SFSI72, SISF72, SLSR72, SRSL72 ] )
        , ( ( SRSL72 , SRSL72 )
          , Set.fromList
                [ SBSB72, SLSR72, SRSL72 ] )
        ]

