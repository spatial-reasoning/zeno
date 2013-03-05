{-# LANGUAGE Rank2Types, FlexibleInstances, OverlappingInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SpatioTemporalStructure.Interval
-- Copyright   :  (c) Edward Kmett 2010, André van Delden 2013
-- License     :  BSD3
-- Maintainer  :  andre.van.delden@uni-bremen.de
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Interval arithmetic
--
-----------------------------------------------------------------------------
module SpatioTemporalStructure.Interval where

--fixme: look over behaviour for NaN.
import Prelude hiding (null, elem, notElem)
import qualified Data.List as List
import Data.Function (on)

data ClosedOpen = C | O deriving (Eq, Ord, Read, Show)
data Interval a = Interval ClosedOpen ClosedOpen a a deriving (Eq, Read)

infix 3 |.|
infix 3 |..
infix 3 ..|
infix 3 ...

(|.|) :: a -> a -> Interval a
x |.| y = Interval C C x y

(|..) :: a -> a -> Interval a
x |.. y = Interval C O x y

(..|) :: a -> a -> Interval a
x ..| y = Interval O C x y

(...) :: a -> a -> Interval a
x ... y = Interval O O x y

instance Show a => Show (Interval a) where
    showsPrec n (Interval co1 co2 x y) =
      let
        coStr co = if co == C then "|" else "."
      in
        showParen (n > 3) $ showString "(" . showsPrec 3 x .
        showString (" " ++ coStr co1 ++ "." ++ coStr co2 ++ " ") .
        showsPrec 3 y . showString ")"

instance Ord a => Ord (Interval a) where
    compare (Interval coxi coxs xi xs) (Interval coyi coys yi ys)
        | xi < yi || xi == yi && (  coxi == C && coyi == O
                                 || coxi == coxs &&
                                     (  xs < ys
                                     || xs == ys && coxs == O && coys == C
                                     )
                                 ) = LT
        | xi == yi && xs == ys && coxi == coyi && coxs == coys = EQ
        | otherwise = GT

-- | The infinumum (lower bound) of an interval
inf :: Interval a -> a
inf (Interval _ _ a _) = a

-- | The supremum (upper bound) of an interval
sup :: Interval a -> a
sup (Interval _ _ _ b) = b

ends :: Interval a -> [a]
ends x = [inf x, sup x]

switch :: ClosedOpen -> ClosedOpen
switch co = if co == C then O else C

swap :: Interval a -> Interval a
swap (Interval co1 co2 x y) = Interval co2 co1 y x

onInf :: (a -> a) -> Interval a -> Interval a
onInf fun (Interval co co2 x y) = Interval co co2 (fun x) y

onSup :: (a -> a) -> Interval a -> Interval a
onSup fun (Interval co co2 x y) = Interval co co2 x (fun y)

onEnds :: (a -> b) -> Interval a -> Interval b
onEnds fun (Interval co co2 x y) = Interval co co2 (fun x) (fun y)

toInf :: a -> Interval a -> Interval a
toInf x = onInf (const x)

toSup :: a -> Interval a -> Interval a
toSup x = onSup (const x)

closeInf :: Interval a -> Interval a
closeInf (Interval _ co x y) = Interval C co x y

closeSup :: Interval a -> Interval a
closeSup (Interval co _ x y) = Interval co C x y

close :: Interval a -> Interval a
close (Interval _ _ x y) = Interval C C x y

openInf :: Interval a -> Interval a
openInf (Interval _ co x y) = Interval O co x y

openSup :: Interval a -> Interval a
openSup (Interval co _ x y) = Interval co O x y

open :: Interval a -> Interval a
open (Interval _ _ x y) = Interval O O x y

infClosed :: Interval a -> Bool
infClosed (Interval co _ _ _) = co == C

infOpen :: Interval a -> Bool
infOpen (Interval co _ _ _) = co == O

supClosed :: Interval a -> Bool
supClosed (Interval _ co _ _) = co == C

supOpen :: Interval a -> Bool
supOpen (Interval _ co _ _) = co == O

empty :: Num a => Interval a
empty = Interval O O 0 0

null :: Ord a => Interval a -> Bool
null (Interval C C x y) = not (x <= y)
null x = not (inf x < sup x)

-- | A singleton point
singleton :: a -> Interval a
singleton a = a |.| a

-- | Is the interval a singleton point?
-- N.B. This is fairly fragile and likely will not hold after
-- even a few operations that only involve singletons
singular :: Ord a => Interval a -> Bool
singular (Interval C C x y) = x == y
singular x                  = False

singularList :: Eq a => [Interval a] -> Bool
singularList [Interval C C x y] = x == y
singularList x                  = False

onSingularList :: Eq a => (a -> a) -> [Interval a] -> [Interval a]
onSingularList fun x
    | singularList x = map (onEnds fun) x
    | otherwise = error $ "onSingularList is only defined for singular lists."

getSingular :: Eq a => [Interval a] -> Interval a
getSingular ival = case ival of
    [Interval C C x y] | x == y -> Interval C C x y
    otherwise -> error "getSingular is only defined for singular lists."

-- | Calculate the width of an interval.
width :: Num a => Interval a -> a
width x = sup x - inf x

-- | magnitude
magnitude :: (Num a, Ord a) => Interval a -> a
magnitude x = (max `on` abs) (inf x) (sup x)

-- | "mignitude"
mignitude :: (Num a, Ord a) => Interval a -> a
mignitude x = (min `on` abs) (inf x) (sup x)

-- | For all @x@ in @X@, @y@ in @Y@. @x '<' y@
(<!)  :: Ord a => Interval a -> Interval a -> Bool
x <! y = null x || null y ||
         (sup x < inf y || sup x == inf y && (supOpen x || infOpen y))

-- | For all @x@ in @X@, @y@ in @Y@. @x '<=' y@
(<=!) :: Ord a => Interval a -> Interval a -> Bool
x <=! y = null x || null y ||
          sup x <= inf y

-- | For all @x@ in @X@, @y@ in @Y@. @x '==' y@
(==!) :: (Eq a, Ord a) => Interval a -> Interval a -> Bool
x ==! y = null x || null y ||
          (inf x == sup x && inf x == inf y && inf x == sup y)

-- | For all @x@ in @X@, @y@ in @Y@. @x '/=' y@
(/=!) :: Ord a => Interval a -> Interval a -> Bool
x /=! y = null x || null y ||
          ( sup x < inf y || inf x > sup y ||
            sup x == inf y && (supOpen x || infOpen y) ||
            inf x == sup y && (infOpen x || supOpen y)    )

-- | For all @x@ in @X@, @y@ in @Y@. @x '>' y@
(>!)  :: Ord a => Interval a -> Interval a -> Bool
x >! y = null x || null y ||
         (inf x > sup y || inf x == sup y && (infOpen x || supOpen y))

-- | For all @x@ in @X@, @y@ in @Y@. @x '>=' y@
(>=!) :: Ord a => Interval a -> Interval a -> Bool
x >=! y = null x || null y ||
          inf x >= sup y

-- | For all @x@ in @X@, @y@ in @Y@. @x `op` y@
certainly :: Ord a
          => (forall b. Ord b => b -> b -> Bool)
          -> Interval a -> Interval a -> Bool
certainly cmp l r
    | lt && eq && gt   = True
    | lt && eq         = l <=! r
    | lt &&       gt   = l /=! r
    | lt               = l <! r
    |       eq && gt   = l >=! r
    |       eq         = l ==! r
    |             gt   = l >! r
    | otherwise        = False
  where
    lt = cmp LT EQ
    eq = cmp EQ EQ
    gt = cmp GT EQ

contains :: Ord a => Interval a -> Interval a -> Bool
contains x y = null y || not (null x) &&
    ( (inf x < inf y || infClosed x && inf x == inf y) &&
      (sup y < sup x || supClosed x && sup y == sup x)    )

isSubsetOf :: Ord a => Interval a -> Interval a -> Bool
isSubsetOf = flip contains

-- | This includes overlaps.
touches :: (Ord a) => Interval a -> Interval a -> Bool
touches x y = not (null x) && not (null y) &&
    ( (supClosed x || infClosed y) && sup x >= inf y || sup x > inf y ) &&
    ( (infClosed x || supClosed y) && inf x <= sup y || inf x < sup y )


-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<' y@?
(<?) :: Ord a => Interval a -> Interval a -> Bool
x <? y = not (null x) && not (null y) &&
         inf x < sup y

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<=' y@?
(<=?) :: Ord a => Interval a -> Interval a -> Bool
x <=? y = not (null x) && not (null y) &&
          inf x < sup y || inf x == sup y && infClosed x && supClosed y

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '==' y@?
(==?) :: Ord a => Interval a -> Interval a -> Bool
x ==? y = not (null x) && not (null y) &&
    ( (sup x > inf y || sup x == inf y && supClosed x && infClosed y) &&
      (inf x < sup y || inf x == sup y && infClosed x && supClosed y)    )

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '/=' y@?
(/=?) :: (Ord a) => Interval a -> Interval a -> Bool
x /=? y = not (null x) && not (null y) &&
          (x /= y)

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>' y@?
(>?) :: Ord a => Interval a -> Interval a -> Bool
x >? y = not (null x) && not (null y) &&
         sup x > inf y

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>=' y@?
(>=?) :: Ord a => Interval a -> Interval a -> Bool
x >=? y = not (null x) && not (null y) &&
          sup x > inf y || sup x == inf y && supClosed x && infClosed y

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x `op` y@?
possibly :: Ord a
         => (forall b. Ord b => b -> b -> Bool)
         -> Interval a -> Interval a -> Bool
possibly cmp l r
    | lt && eq && gt = True
    | lt && eq       = l <=? r
    | lt &&       gt = l /=? r
    | lt             = l <? r
    |       eq && gt = l >=? r
    |       eq       = l ==? r
    |             gt = l >? r
    | otherwise      = False
    where
        lt = cmp LT EQ
        eq = cmp EQ EQ
        gt = cmp GT EQ

-- | Calculate the intersection of two intervals.
-- fixme: care for NaN here.
intersection :: (Num a, Ord a) => Interval a -> Interval a -> Interval a
intersection x@(Interval coxi coxs xi xs) y@(Interval coyi coys yi ys)
    | x /=! y   = empty
    | otherwise = case (coxi, coxs, coyi, coys) of
        (C, C, C, C)             -> Interval C C (max xi yi) (min xs ys)
        (C, C, C, O) | xs < ys   -> Interval C C (max xi yi) xs
                     | otherwise -> Interval C O (max xi yi) ys
        (C, C, O, C) | xi > yi   -> Interval C C xi (min xs ys)
                     | otherwise -> Interval O C yi (min xs ys)
        (C, C, O, O) | xi > yi   -> if xs < ys then
                                        Interval C C xi xs
                                    else
                                        Interval C O xi ys
                     | xs < ys   -> Interval O C yi xs
                     | otherwise -> Interval O O yi ys
        (C, O, C, O)             -> Interval C O (max xi yi) (min xs ys)
        (C, O, O, C) | xi > yi   -> if xs > ys then
                                        Interval C C xi ys
                                    else
                                        Interval C O xi xs
                     | xs > ys   -> Interval O C yi ys
                     | otherwise -> Interval O O yi xs
        (C, O, O, O) | xi > yi   -> Interval C O xi (min xs ys)
                     | otherwise -> Interval O O yi (min xs ys)
        (O, C, O, C)             -> Interval O C (max xi yi) (min xs ys)
        (O, C, O, O) | xs < ys   -> Interval O C (max xi yi) xs
                     | otherwise -> Interval O O (max xi yi) ys
        (O, O, O, O)             -> Interval O O (max xi yi) (min xs ys)
        otherwise                -> intersection y x

-- | Calculate the convex hull of two intervals
hull :: Ord a => Interval a -> Interval a -> Interval a
hull x@(Interval coxi coxs xi xs) y@(Interval coyi coys yi ys)
    | null x    = y
    | null y    = x
    | otherwise = case (coxi, coxs, coyi, coys) of
        (C, C, C, C)             -> Interval C C (min xi yi) (max xs ys)
        (C, C, C, O) | xs < ys   -> Interval C O (min xi yi) ys
                     | otherwise -> Interval C C (min xi yi) xs
        (C, C, O, C) | xi > yi   -> Interval O C yi (max xs ys)
                     | otherwise -> Interval C C xi (max xs ys)
        (C, C, O, O) | xi > yi   -> if xs < ys then
                                        Interval O O yi ys
                                    else
                                        Interval O C yi xs
                     | xs < ys   -> Interval C O xi ys
                     | otherwise -> Interval C C xi xs
        (C, O, C, O)             -> Interval C O (min xi yi) (max xs ys)
        (C, O, O, C) | xi > yi   -> if xs > ys then
                                        Interval O O yi xs
                                    else
                                        Interval O C yi ys
                     | xs > ys   -> Interval C O xi xs
                     | otherwise -> Interval C C xi ys
        (C, O, O, O) | xi > yi   -> Interval O O yi (max xs ys)
                     | otherwise -> Interval C O xi (max xs ys)
        (O, C, O, C)             -> Interval O C (min xi yi) (max xs ys)
        (O, C, O, O) | xs < ys   -> Interval O O (min xi yi) ys
                     | otherwise -> Interval O C (min xi yi) xs
        (O, O, O, O)             -> Interval O O (min xi yi) (max xs ys)
        otherwise                -> hull y x

-- fixme: This is totally wrong:
--minInterval :: Ord a => Interval a -> Interval a -> Interval a
--minInterval x y =
--    | null x = y
--    | null y = x
--    | otherwise = onSup (min (sup y)) $ onInf (min (inf y)) x
--
--maxInterval :: Ord a => Interval a -> Interval a -> Interval a
--maxInterval x y =
--    | null x = y
--    | null y = x
--    | otherwise = onSup (max (sup y)) $ onInf (max (inf y)) x


infix 3 `minus`
minus :: (Fractional a, Ord a) => Interval a -> Interval a -> [Interval a]
minus x y | x /=! y    = [x]
          | cmp1 xi yi = if cmp2 xs ys then
                             [ lowerIntervalType xi yi ]
                         else
                             [ lowerIntervalType xi yi
                             , upperIntervalType ys xs ]
          | cmp2 xs ys = [empty]
          | otherwise  = [ upperIntervalType ys xs ]
  where
    (xi, xs, yi, ys) = (inf x, sup x, inf y, sup y)
    cmp1 | infClosed y || infOpen y && infOpen x = (<)
         | otherwise                             = (<=)
    cmp2 | supOpen   y && supClosed x = (<)
         | otherwise                  = (<=)
    lowerIntervalType z = (if infClosed x then id else openInf) .
                          (if infOpen   y then id else openSup) .
                          Interval C C z
    upperIntervalType z = (if supOpen   y then id else openInf) .
                          (if supClosed x then id else openSup) .
                          Interval C C z


complement :: (Fractional a, Ord a) => Interval a -> Interval a -> [Interval a]
complement = minus

-- | Computes the complement of the given intervals with respect to the given
-- universe.
complements :: (Fractional a, Ord a)
            => Interval a -> [Interval a] -> [Interval a]
complements universe = foldr (\i (a:acc) -> (a `minus` i) ++ acc) [universe]


-- LISTS

nullOnly :: Ord a => [Interval a] -> Bool
nullOnly [] = False
nullOnly xs = all null xs

endsInList :: [Interval a] -> [a]
endsInList = concatMap ends

insert :: Ord a
       => Interval a -> [Interval a] -> [Interval a]
insert i = insert' . foldr
    (\ i' (acc, acc2) ->
        if touches i' acc then
            (hull i' acc, acc2)
        else
            (acc, i':acc2)
    ) (i, [])
  where
    insert' (i', is') = (\ (lesser, greater) -> lesser ++ (i':greater)
                        ) $ span ((< (inf i')) . sup) is'

-- | This assumes that the intervals in each list have properly ordered inf
-- and sup and also do not overlap and are in proper order in the list.
-- improve: using smarter folds we can eliminate the "dropWhile".
intersections :: (Num a, Ord a)
              => [Interval a] -> [Interval a] -> [Interval a]
intersections is is2
    | List.null is   = is2
    | List.null is2  = is
    | otherwise      = foldr
        (\ i1 acc1 -> (++ acc1) $ foldr
            (\ i2 acc2 -> (intersection i1 i2):acc2
            ) [] $ takeWhile (\ i' -> i' ==? i1) $
                   dropWhile (\ i' -> i' /=! i1) is
        ) [] is2

invertModulo :: (Num a, Ord a) => a -> Interval a -> [Interval a]
invertModulo n i | inf i /= 0 || infOpen i = [onEnds (\ x -> n - x) $ swap i]
                 | otherwise  = insert (singleton 0) $
                                [openSup $ toSup n $ toInf (n - sup i) i]

invertModuloList :: (Num a, Ord a) => a -> [Interval a] -> [Interval a]
invertModuloList n is = foldr
    (\ i acc -> foldr insert acc $ invertModulo n i
    ) [] is

--instance (Num a, Ord a) => Num (Interval a) where
--    I a b + I a' b' = (a + a') ... (b + b')
--    I a b - I a' b' = (a - b') ... (b - a')
--    I a b * I a' b' = minimum [a * a', a * b', b * a', b * b']
--                      ...
--                      maximum [a * a', a * b', b * a', b * b']
--    abs x@(I a b)
--        | a >= 0    = x
--        | b <= 0    = negate x
--        | otherwise = max (- a) b ... b
--
--    signum = increasing signum
--
--    fromInteger i = singleton (fromInteger i)
--
---- | Bisect an interval at its midpoint.
--bisection :: Fractional a => Interval a -> (Interval a, Interval a)
--bisection x = (inf x ... m, m ... sup x)
--    where m = midpoint x
--
---- | Nearest point to the midpoint of the interval.
--midpoint :: Fractional a => Interval a -> a
--midpoint x = inf x + (sup x - inf x) / 2
--
--elem :: Ord a => a -> Interval a -> Bool
--elem x xs = x >= inf xs && x <= sup xs
--
--notElem :: Ord a => a -> Interval a -> Bool
--notElem x xs = not (elem x xs)
--
---- | This means that realToFrac will use the midpoint
--
---- | What moron put an Ord instance requirement on Real!
--instance Real a => Real (Interval a) where
--    toRational x
--        | null x   = nan
--        | otherwise = a + (b - a) / 2
--        where
--            a = toRational (inf x)
--            b = toRational (sup x)
--
--instance Ord a => Ord (Interval a) where
--    compare x y
--        | sup x < inf y = LT
--        | inf x > sup y = GT
--        | sup x == inf y && inf x == sup y = EQ
--        | otherwise = error "Numeric.Interval.compare: ambiguous comparison"
--    min = minInterval
--    max = maxInterval
--
---- @'divNonZero' X Y@ assumes @0 `'notElem'` Y@
--divNonZero :: (Fractional a, Ord a) => Interval a -> Interval a -> Interval a
--divNonZero (I a b) (I a' b') =
--    minimum [a / a', a / b', b / a', b / b']
--    `I`
--    maximum [a / a', a / b', b / a', b / b']
--
---- @'divPositive' X y@ assumes y > 0, and divides @X@ by [0 ... y]
--divPositive :: (Fractional a, Ord a) => Interval a -> a -> Interval a
--divPositive x@(I a b) y
--    | a == 0 && b == 0 = x
--    -- b < 0 || isNegativeZero b = negInfinity `I` ( b / y)
--    | b < 0 = negInfinity `I` ( b / y)
--    | a < 0 = whole
--    | otherwise = (a / y) `I` posInfinity
--
---- divNegative assumes y < 0 and divides the interval @X@ by [y ... 0]
--divNegative :: (Fractional a, Ord a) => Interval a -> a -> Interval a
--divNegative x@(I a b) y
--    | a == 0 && b == 0 = - x -- flip negative zeros
--    -- b < 0 || isNegativeZero b = (b / y) `I` posInfinity
--    | b < 0 = (b / y) `I` posInfinity
--    | a < 0 = whole
--    | otherwise = negInfinity `I` (a / y)
--
--divZero :: (Fractional a, Ord a) => Interval a -> Interval a
--divZero x | inf x == 0 && sup x == 0 = x
--          | otherwise = whole
--
--instance (Fractional a, Ord a) => Fractional (Interval a) where
--    -- TODO: check isNegativeZero properly
--    x / y
--        | 0 `notElem` y = divNonZero x y
--        | iz && sz  = empty -- division by 0
--        | iz        = divPositive x (inf y)
--        |       sz  = divNegative x (sup y)
--        | otherwise = divZero x
--        where
--            iz = inf y == 0
--            sz = sup y == 0
--    recip (I a b)   = on min recip a b ... on max recip a b
--    fromRational r  = fromRational r ... fromRational r
--
--instance RealFloat a => RealFrac (Interval a) where
--    properFraction x = (b, x - fromIntegral b)
--        where
--            b = truncate (midpoint x)
--    ceiling x = ceiling (sup x)
--    floor x = floor (inf x)
--    round x = round (midpoint x)
--    truncate x = truncate (midpoint x)
--
--instance (RealExtras a, Ord a) => Floating (Interval a) where
--    pi = singleton pi
--    exp = increasing exp
--    log (I a b) = (if a > 0 then log a else negInfinity) ... log b
--    cos x
--        | null x = empty
--        | width t >= pi = (-1) ... 1
--        | inf t >= pi = - cos (t - pi)
--        | sup t <= pi = decreasing cos t
--        | sup t <= 2 * pi = (-1) ... cos ((pi * 2 - sup t) `min` inf t)
--        | otherwise = (-1) ... 1
--        where
--            t = fmod x (pi * 2)
--    sin x
--        | null x = empty
--        | otherwise = cos (x - pi / 2)
--    tan x
--        | null x = empty
--        | inf t' <= - pi / 2 || sup t' >= pi / 2 = whole
--        | otherwise = increasing tan x
--        where
--            t = x `fmod` pi
--            t' | t >= pi / 2 = t - pi
--               | otherwise    = t
--    asin x@(I a b)
--        | null x || b < -1 || a > 1 = empty
--        | otherwise =
--            (if a <= -1 then -halfPi else asin a)
--            `I`
--            (if b >= 1 then halfPi else asin b)
--        where
--            halfPi = pi / 2
--    acos x@(I a b)
--        | null x || b < -1 || a > 1 = empty
--        | otherwise =
--            (if b >= 1 then 0 else acos b)
--            `I`
--            (if a < -1 then pi else acos a)
--    atan = increasing atan
--    sinh = increasing sinh
--    cosh x@(I a b)
--        | null x = empty
--        | b < 0  = decreasing cosh x
--        | a >= 0 = increasing cosh x
--        | otherwise  = I 0 $ cosh $ if - a > b
--                                    then a
--                                    else b
--    tanh = increasing tanh
--    asinh = increasing asinh
--    acosh x@(I a b)
--        | null x || b < 1 = empty
--        | otherwise = I lo $ acosh b
--        where lo | a <= 1 = 0
--                 | otherwise = acosh a
--    atanh x@(I a b)
--        | null x || b < -1 || a > 1 = empty
--        | otherwise =
--                (if a <= - 1 then negInfinity else atanh a)
--                `I`
--                (if b >= 1 then posInfinity else atanh b)
--
---- | lift a monotone increasing function over a given interval
--increasing :: (a -> a) -> Interval a -> Interval a
--increasing f (I a b) = I (f a) (f b)
--
---- | lift a monotone increasing function over a given interval
--decreasing :: (a -> a) -> Interval a -> Interval a
--decreasing f (I a b) = I (f b) (f a)
--
---- | We have to play some semantic games to make these methods make sense.
---- Most compute with the midpoint of the interval.
--instance RealExtras a => RealFloat (Interval a) where
--    floatRadix = floatRadix . midpoint
--    floatDigits = floatDigits . midpoint
--    floatRange = floatRange . midpoint
--    decodeFloat = decodeFloat . midpoint
--    encodeFloat m e = singleton (encodeFloat m e)
--    exponent = exponent . midpoint
--    significand x = min a b ... max a b
--        where
--            (_ ,em) = decodeFloat (midpoint x)
--            (mi,ei) = decodeFloat (inf x)
--            (ms,es) = decodeFloat (sup x)
--            a = encodeFloat mi (ei - em - floatDigits x)
--            b = encodeFloat ms (es - em - floatDigits x)
--    scaleFloat n x = scaleFloat n (inf x) ... scaleFloat n (sup x)
--    isNaN x = isNaN (inf x) || isNaN (sup x)
--    isInfinite x = isInfinite (inf x) || isInfinite (sup x)
--    isDenormalized x = isDenormalized (inf x) || isDenormalized (sup x)
--    -- contains negative zero
--    isNegativeZero x = not (inf x > 0)
--                    && not (sup x < 0)
--                    && (  (sup x == 0 && (inf x < 0 || isNegativeZero (inf x)))
--                       || (inf x == 0 && isNegativeZero (inf x))
--                       || (inf x < 0 && sup x >= 0))
--    isIEEE x = isIEEE (inf x) && isIEEE (sup x)
--    atan2 = error "unimplemented"
--
---- TODO: (^), (^^) to give tighter bounds
--
--instance RealExtras a => RealExtras (Interval a) where
--    type C (Interval a) = C a
--    fmod x y | null y = empty
--             | otherwise = r -- `intersect` bounds
--        where
--            n :: Integer
--            n = floor (inf x / if inf x < 0 then inf y else sup y)
--            r = x - fromIntegral n * y
--            -- bounds | inf y >= 0 = y
--            --        | otherwise = y `hull` negate y
--    expm1 = increasing expm1
--    log1p (I a b) = (if a > (-1) then log1p a else negInfinity) `I` log1p b
--    hypot x y = hypot a a' `I` hypot b b'
--        where
--            I a b = abs x
--            I a' b' = abs y
--    cbrt = increasing cbrt
--    erf = increasing erf
--
--idouble :: Interval Double -> Interval Double
--idouble = id
--
--ifloat :: Interval Float -> Interval Float
--ifloat = id

