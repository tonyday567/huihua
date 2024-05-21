{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Unification of Double and Int Arrays, together with coertion logic.
module Huihua.ArrayU
  ( -- $usage

    ArrayU (..),
    CoerceU (..),
    coerceToD,
    coerceToI,
    coerceToExactI,
    coerceIfExactI,
    Res,
    liftU,
    liftUI,
    liftUD,
    lift2U,
    lift2IU,
    lift2DU,
    liftI',
    liftD',
    -- * uiua API
    duplicate,
    pop,
    identity,
    not,
    sign,
    negate,
    absoluteValue,
    sqrt,
    sine,
    floor,
    ceiling,
    round,
    length,
    shape',
    range,
    first,
    deshape,
    bits,
    transpose,
    rise,
    fall,
    where',
    classify,
    deduplicate,
{-
    sig,
    add,
    subtract,
    multiply,
    divide,
    equals,
    notequals,
    lt,
    lte,
    gt,
    gte,
    modulus,
    modulusD,
    power,
    logarithm,
    minimum,
    maximum,
    arctangent,
    indexA,
    reverse,
    fix,
    couple,
    match,
    pick,
    rotate,
    join,
    select,
    take,
    drop,
    reshape,
    addR,
    subtractR,
    equalsR,
    notEqualsR,
    lessThanR,
    lessOrEqualR,
    greaterThanR,
    greaterOrEqualR,
    divideR,
    multiplyR,
    minimumR,
    maximumR,
    modulusR,
    powerR,
    logarithmR,
-}
)
where

import Huihua.Array qualified as A
import NumHask.Array.Dynamic hiding (transpose)
import NumHask.Prelude hiding (not, negate, sqrt, sin, floor, ceiling, round, minimum, maximum, length, reverse, fix, take, drop)
import NumHask.Prelude qualified as P
import Huihua.Warning
import Prettyprinter hiding (equals)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Huihua.Array as A
-- >>> import NumHask.Array.Dynamic as D

-- |
data ArrayU = ArrayI (Array Int) | ArrayD (Array Double) deriving (Show, Eq)

instance Pretty ArrayU where
  pretty = viaShow

coerceToD :: Array Int -> Array Double
coerceToD = fmap fromIntegral

coerceToI :: Array Double -> Array Int
coerceToI = fmap (fromIntegral . P.floor)

coerceToExactI :: Array Double -> Either HuihuaWarning (Array Int)
coerceToExactI (Array s xs) = bool (Left NotNat) (Right $ Array s $ fmap P.floor xs) (all (\x -> x==fromIntegral (P.floor x)) xs)

coerceIfExactI :: ArrayU -> ArrayU
coerceIfExactI (ArrayD a) = bool (ArrayD a) (ArrayI (coerceToI a)) (all (\x -> x==fromIntegral (P.floor x)) a)

data CoerceU = CoerceToD | CoerceToI | CoerceToExactI | NoCoerce deriving (Eq, Show)

type Res = Either HuihuaWarning [ArrayU]

liftI' :: Either HuihuaWarning (Array Int) -> Either HuihuaWarning [ArrayU]
liftI' = fmap (pure . ArrayI)

liftD' :: Either HuihuaWarning (Array Double) -> Either HuihuaWarning [ArrayU]
liftD' = fmap (pure . ArrayD)

lift2U :: CoerceU -> (Array Int -> Array Int -> Either HuihuaWarning (Array Int)) -> (Array Double -> Array Double -> Either HuihuaWarning (Array Double)) -> ArrayU -> ArrayU -> Either HuihuaWarning [ArrayU]
lift2U _ fi _ (ArrayI x) (ArrayI y) = liftI' (fi x y)
lift2U _ _ fd (ArrayD x) (ArrayD y) = liftD' (fd x y)
lift2U c fi fd (ArrayI x) (ArrayD y) = case c of
  CoerceToD -> liftD' $ fd (coerceToD x) y
  CoerceToI -> liftI' $ fi x (coerceToI y)
  CoerceToExactI -> liftI' $ coerceToExactI y

lift2IU :: CoerceU -> (Array Int -> Array Int -> Either HuihuaWarning (Array Int)) -> (Array Double -> Array Double -> Either HuihuaWarning (Array Int)) -> ArrayU -> ArrayU -> Either HuihuaWarning [ArrayU]
lift2IU _ fi _ (ArrayI x) (ArrayI y) = liftI' (fi x y)
lift2IU _ _ fd (ArrayD x) (ArrayD y) = liftI' (fd x y)
lift2IU c fi fd (ArrayI x) (ArrayD y) = case c of
  CoerceToD -> liftI' $ fd (coerceToD x) y
  CoerceToI -> liftI' $ fi x (coerceToI y)
  -- FIXME:
  CoerceToExactI -> liftI' $ coerceToExactI y

lift2DU :: (Array Double -> Array Double -> Either HuihuaWarning (Array Double)) -> ArrayU -> ArrayU -> Either HuihuaWarning [ArrayU]
lift2DU f (ArrayI x) (ArrayI y) = fmap (fmap coerceIfExactI) $ liftD' (f (coerceToD x) (coerceToD y))
lift2DU f (ArrayD x) (ArrayD y) = liftD' (f x y)
lift2DU f (ArrayI x) (ArrayD y) = liftD' (f (coerceToD x) y)
lift2DU f (ArrayD x) (ArrayI y) = liftD' (f x (coerceToD y))

liftU :: (Array Int -> Either HuihuaWarning (Array Int)) -> (Array Double -> Either HuihuaWarning (Array Double)) -> ArrayU -> Either HuihuaWarning [ArrayU]
liftU fi _ (ArrayI x) = liftI' (fi x)
liftU _ fd (ArrayD x) = liftD' (fd x)

liftUI :: (Array Int -> Either HuihuaWarning (Array Int)) -> (Array Double -> Either HuihuaWarning (Array Int)) -> ArrayU -> Either HuihuaWarning [ArrayU]
liftUI fi _ (ArrayI x) = liftI' (fi x)
liftUI _ fd (ArrayD x) = liftI' (fd x)

liftUD :: (Array Double -> Either HuihuaWarning (Array Double)) -> ArrayU -> Either HuihuaWarning [ArrayU]
liftUD f (ArrayI x) = liftD' (f . coerceToD $ x)
liftUD f (ArrayD x) = liftD' (f x)

{-
lift2IndexU :: (Array Int -> Array Int -> Either HuihuaWarning (Array Int)) -> (Array Int -> Array Double -> Either HuihuaWarning (Array Double)) -> ArrayU -> ArrayU -> Either HuihuaWarning [ArrayU]
lift2IndexU fi _ (ArrayI x) (ArrayI y) = liftI' (fi x y)
lift2IndexU _ fd (ArrayI x) (ArrayD y) = liftD' (fd x y)
-- FIXME:
lift2IndexU _ _ (ArrayD x) (ArrayI y) = undefined -- liftI' (fi (fmap coerceIfExactI x) y)
lift2IndexU _ _ (ArrayD x) (ArrayD y) = undefined -- liftD' (fd (fmap coerceIfExactI x) y)
-}

duplicate :: ArrayU -> Res
duplicate x = Right [x,x]

pop :: ArrayU -> Res
pop _ = Right []

identity :: ArrayU -> Res
identity x = Right [x]

not :: ArrayU -> Res
not (ArrayI a) = Right . pure . ArrayI . A.not $ a
not (ArrayD a) = Right . pure . ArrayD . A.not $ a

sign :: ArrayU -> Res
sign (ArrayI a) = Right . pure . ArrayI . A.sign $ a
sign (ArrayD a) = Right . pure . ArrayD . A.sign $ a

negate :: ArrayU -> Res
negate (ArrayI a) = Right . pure . ArrayI . A.negate $ a
negate (ArrayD a) = Right . pure . ArrayD . A.negate $ a

absoluteValue :: ArrayU -> Res
absoluteValue (ArrayI a) = Right . pure . ArrayI . A.absolute $ a
absoluteValue (ArrayD a) = Right . pure . ArrayD . A.absolute $ a

sqrt :: ArrayU -> Res
sqrt (ArrayI a) = Right . pure . coerceIfExactI . ArrayD . A.sqrt . coerceToD $ a
sqrt (ArrayD a) = Right . pure . ArrayD . A.sqrt $ a

sine :: ArrayU -> Res
sine (ArrayI a) = Right . pure . ArrayD . A.sin . coerceToD $ a
sine (ArrayD a) = Right . pure . ArrayD . A.sin $ a

floor :: ArrayU -> Res
floor (ArrayI a) = Right . pure . ArrayI $ a
floor (ArrayD a) = Right . pure . ArrayI . A.floor $ a

ceiling :: ArrayU -> Res
ceiling (ArrayI a) = Right . pure . ArrayI $ a
ceiling (ArrayD a) = Right . pure . ArrayI . A.ceiling $ a

round :: ArrayU -> Res
round (ArrayI a) = Right . pure . ArrayI $ a
round (ArrayD a) = Right . pure . ArrayI . A.round $ a

length :: ArrayU -> Res
length (ArrayI a) = Right . pure . ArrayI . A.length $ a
length (ArrayD a) = Right . pure . ArrayI . A.length $ a

shape' :: ArrayU -> Res
shape' (ArrayI a) = Right . pure . ArrayI . A.shape' $ a
shape' (ArrayD a) = Right . pure . ArrayI . A.shape' $ a

range :: ArrayU -> Res
range (ArrayI a) = Right . pure . ArrayI . A.range $ a
range (ArrayD a) = fmap (pure . ArrayI . A.range) . coerceToExactI $ a

first :: ArrayU -> Res
first (ArrayI a) = Right . pure . ArrayI . A.first $ a
first (ArrayD a) = Right . pure . ArrayD . A.first $ a

deshape :: ArrayU -> Res
deshape (ArrayI a) = Right . pure . ArrayI . A.deshape $ a
deshape (ArrayD a) = Right . pure . ArrayD . A.deshape $ a

bits :: ArrayU -> Res
bits (ArrayI a) = Right . pure . ArrayI . A.bits $ a
bits (ArrayD a) = fmap (pure . ArrayI . A.bits) . coerceToExactI $ a

transpose :: ArrayU -> Res
transpose (ArrayI a) = Right . pure . ArrayI . A.transpose $ a
transpose (ArrayD a) = Right . pure . ArrayD . A.transpose $ a

rise :: ArrayU -> Res
rise (ArrayI a) = Right . pure . ArrayI . A.rise $ a
rise (ArrayD a) = Right . pure . ArrayI . A.rise $ a

fall :: ArrayU -> Res
fall (ArrayI a) = Right . pure . ArrayI . A.fall $ a
fall (ArrayD a) = Right . pure . ArrayI . A.fall $ a

where' :: ArrayU -> Res
where' (ArrayI a) = fmap (pure . ArrayI) $ A.where' a
where' (ArrayD a) = do
  i <- coerceToExactI a
  a' <- A.where' i
  pure (pure . ArrayI $ a')

classify :: ArrayU -> Res
classify (ArrayI a) = Right . pure . ArrayI . A.classify $ a
classify (ArrayD a) = Right . pure . ArrayI . A.classify $ a

deduplicate :: ArrayU -> Res
deduplicate (ArrayI a) = Right . pure . ArrayI . A.deduplicate $ a
deduplicate (ArrayD a) = Right . pure . ArrayD . A.deduplicate $ a
