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
    coerceToD,
    coerceToI,
    coerceToExactI,
    coerceIfExactI,
    Res,

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
    reverse,
    deshape,
    fix,
    bits,
    transpose,
    rise,
    fall,
    where',
    classify,
    deduplicate,

    -- * dyadics
    equals,
    notequals,
    lt,
    lte,
    gt,
    gte,
    add,
    subtract,
    multiply,
    divide,
    modulus,
    power,
    logarithm,
    minimum,
    maximum,
    arctangent,

    match,
    couple,
    join,
    select,
    pick,
    reshape,
    rerank,
    take,
    drop,
    rotate,
    windows,
    keep,
    find,

    -- * reductions
    equalsR,
    notEqualsR,
    lessThanR,
    lessOrEqualR,
    greaterThanR,
    greaterOrEqualR,
    addR,
    subtractR,
    multiplyR,
    divideR,
    modulusR,
    powerR,
    logarithmR,
    minimumR,
    maximumR,
)
where

import Huihua.Array qualified as A
import NumHask.Array.Dynamic hiding (transpose)
import NumHask.Prelude hiding (not, negate, sqrt, sin, floor, ceiling, round, minimum, maximum, length, reverse, fix, take, drop, find)
import NumHask.Prelude qualified as P
import Huihua.Warning
import Prettyprinter hiding (equals)
import Data.Bifunctor qualified as Bi

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

type Res = Either HuihuaWarning [ArrayU]

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
range (ArrayI a) = fmap (pure . ArrayI) . A.range $ a
range (ArrayD a) = do
  a' <- coerceToExactI a
  r <- A.range a'
  pure (pure . ArrayI $ r)

first :: ArrayU -> Res
first (ArrayI a) = Right . pure . ArrayI . A.first $ a
first (ArrayD a) = Right . pure . ArrayD . A.first $ a

reverse :: ArrayU -> Res
reverse (ArrayI a) = Right . pure . ArrayI . A.reverse $ a
reverse (ArrayD a) = Right . pure . ArrayD . A.reverse $ a

deshape :: ArrayU -> Res
deshape (ArrayI a) = Right . pure . ArrayI . A.deshape $ a
deshape (ArrayD a) = Right . pure . ArrayD . A.deshape $ a

fix :: ArrayU -> Res
fix (ArrayI a) = Right . pure . ArrayI . A.fix $ a
fix (ArrayD a) = Right . pure . ArrayD . A.fix $ a

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

-- * dyadic operators
equals :: ArrayU -> ArrayU -> Res
equals (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ A.equals x y
equals (ArrayD x) (ArrayD y) = fmap (pure . ArrayI) $ A.equals x y
equals (ArrayI x) (ArrayD y) = fmap (pure . ArrayI) $ A.equals (coerceToD x) y
equals (ArrayD x) (ArrayI y) = fmap (pure . ArrayI) $ A.equals x (coerceToD y)

notequals :: ArrayU -> ArrayU -> Res
notequals (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ A.notequals x y
notequals (ArrayD x) (ArrayD y) = fmap (pure . ArrayI) $ A.notequals x y
notequals (ArrayI x) (ArrayD y) = fmap (pure . ArrayI) $ A.notequals (coerceToD x) y
notequals (ArrayD x) (ArrayI y) = fmap (pure . ArrayI) $ A.notequals x (coerceToD y)

lt :: ArrayU -> ArrayU -> Res
lt (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ A.lt x y
lt (ArrayD x) (ArrayD y) = fmap (pure . ArrayI) $ A.lt x y
lt (ArrayI x) (ArrayD y) = fmap (pure . ArrayI) $ A.lt (coerceToD x) y
lt (ArrayD x) (ArrayI y) = fmap (pure . ArrayI) $ A.lt x (coerceToD y)

lte :: ArrayU -> ArrayU -> Res
lte (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ A.lte x y
lte (ArrayD x) (ArrayD y) = fmap (pure . ArrayI) $ A.lte x y
lte (ArrayI x) (ArrayD y) = fmap (pure . ArrayI) $ A.lte (coerceToD x) y
lte (ArrayD x) (ArrayI y) = fmap (pure . ArrayI) $ A.lte x (coerceToD y)

gt :: ArrayU -> ArrayU -> Res
gt (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ A.gt x y
gt (ArrayD x) (ArrayD y) = fmap (pure . ArrayI) $ A.gt x y
gt (ArrayI x) (ArrayD y) = fmap (pure . ArrayI) $ A.gt (coerceToD x) y
gt (ArrayD x) (ArrayI y) = fmap (pure . ArrayI) $ A.gt x (coerceToD y)

gte :: ArrayU -> ArrayU -> Res
gte (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ A.gte x y
gte (ArrayD x) (ArrayD y) = fmap (pure . ArrayI) $ A.gte x y
gte (ArrayI x) (ArrayD y) = fmap (pure . ArrayI) $ A.gte (coerceToD x) y
gte (ArrayD x) (ArrayI y) = fmap (pure . ArrayI) $ A.gte x (coerceToD y)

add :: ArrayU -> ArrayU -> Res
add (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ A.add x y
add (ArrayD x) (ArrayD y) = fmap (pure . ArrayD) $ A.add x y
add (ArrayI x) (ArrayD y) = fmap (pure . ArrayD) $ A.add (coerceToD x) y
add (ArrayD x) (ArrayI y) = fmap (pure . ArrayD) $ A.add x (coerceToD y)

subtract :: ArrayU -> ArrayU -> Res
subtract (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ A.subtract x y
subtract (ArrayD x) (ArrayD y) = fmap (pure . ArrayD) $ A.subtract x y
subtract (ArrayI x) (ArrayD y) = fmap (pure . ArrayD) $ A.subtract (coerceToD x) y
subtract (ArrayD x) (ArrayI y) = fmap (pure . ArrayD) $ A.subtract x (coerceToD y)

multiply :: ArrayU -> ArrayU -> Res
multiply (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ A.multiply x y
multiply (ArrayD x) (ArrayD y) = fmap (pure . ArrayD) $ A.multiply x y
multiply (ArrayI x) (ArrayD y) = fmap (pure . ArrayD) $ A.multiply (coerceToD x) y
multiply (ArrayD x) (ArrayI y) = fmap (pure . ArrayD) $ A.multiply x (coerceToD y)

divide :: ArrayU -> ArrayU -> Res
divide (ArrayI x) (ArrayI y) = fmap (pure . coerceIfExactI . ArrayD) $ A.divide (coerceToD x) (coerceToD y)
divide (ArrayD x) (ArrayD y) = fmap (pure . ArrayD) $ A.divide x y
divide (ArrayI x) (ArrayD y) = fmap (pure . ArrayD) $ A.divide (coerceToD x) y
divide (ArrayD x) (ArrayI y) = fmap (pure . ArrayD) $ A.divide x (coerceToD y)

modulus :: ArrayU -> ArrayU -> Res
modulus (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ A.modulus x y
modulus (ArrayD x) (ArrayD y) = fmap (pure . ArrayD) $ A.modulusD x y
modulus (ArrayI x) (ArrayD y) = fmap (pure . ArrayD) $ A.modulusD (coerceToD x) y
modulus (ArrayD x) (ArrayI y) = fmap (pure . ArrayD) $ A.modulusD x (coerceToD y)

power :: ArrayU -> ArrayU -> Res
power (ArrayI x) (ArrayI y) = fmap (pure . coerceIfExactI . ArrayD) $ A.power (coerceToD x) (coerceToD y)
power (ArrayD x) (ArrayD y) = fmap (pure . ArrayD) $ A.power x y
power (ArrayI x) (ArrayD y) = fmap (pure . ArrayD) $ A.power (coerceToD x) y
power (ArrayD x) (ArrayI y) = fmap (pure . ArrayD) $ A.power x (coerceToD y)

logarithm :: ArrayU -> ArrayU -> Res
logarithm (ArrayI x) (ArrayI y) = fmap (pure . coerceIfExactI . ArrayD) $ A.power (coerceToD x) (coerceToD y)
logarithm (ArrayD x) (ArrayD y) = fmap (pure . ArrayD) $ A.logarithm x y
logarithm (ArrayI x) (ArrayD y) = fmap (pure . ArrayD) $ A.logarithm (coerceToD x) y
logarithm (ArrayD x) (ArrayI y) = fmap (pure . ArrayD) $ A.logarithm x (coerceToD y)

minimum :: ArrayU -> ArrayU -> Res
minimum (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ A.minimum x y
minimum (ArrayD x) (ArrayD y) = fmap (pure . ArrayD) $ A.minimum x y
minimum (ArrayI x) (ArrayD y) = fmap (pure . ArrayD) $ A.minimum (coerceToD x) y
minimum (ArrayD x) (ArrayI y) = fmap (pure . ArrayD) $ A.minimum x (coerceToD y)

maximum :: ArrayU -> ArrayU -> Res
maximum (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ A.maximum x y
maximum (ArrayD x) (ArrayD y) = fmap (pure . ArrayD) $ A.maximum x y
maximum (ArrayI x) (ArrayD y) = fmap (pure . ArrayD) $ A.maximum (coerceToD x) y
maximum (ArrayD x) (ArrayI y) = fmap (pure . ArrayD) $ A.maximum x (coerceToD y)

arctangent :: ArrayU -> ArrayU -> Res
arctangent (ArrayI x) (ArrayI y) = fmap (pure . coerceIfExactI . ArrayD) $ A.power (coerceToD x) (coerceToD y)
arctangent (ArrayD x) (ArrayD y) = fmap (pure . ArrayD) $ A.arctangent x y
arctangent (ArrayI x) (ArrayD y) = fmap (pure . ArrayD) $ A.arctangent (coerceToD x) y
arctangent (ArrayD x) (ArrayI y) = fmap (pure . ArrayD) $ A.arctangent x (coerceToD y)

match :: ArrayU -> ArrayU -> Res
match (ArrayI x) (ArrayI y) = Right . pure . ArrayI $ A.match x y
match (ArrayD x) (ArrayD y) = Right . pure . ArrayI $ A.match x y
match (ArrayI x) (ArrayD y) = Right . pure . ArrayI $ A.match (coerceToD x) y
match (ArrayD x) (ArrayI y) = Right . pure . ArrayI $ A.match x (coerceToD y)

couple :: ArrayU -> ArrayU -> Res
couple (ArrayI x) (ArrayI y) = Right . pure . ArrayI $ A.couple x y
couple (ArrayD x) (ArrayD y) = Right . pure . ArrayD $ A.couple x y
couple (ArrayI x) (ArrayD y) = Right . pure . ArrayD $ A.couple (coerceToD x) y
couple (ArrayD x) (ArrayI y) = Right . pure . ArrayD $ A.couple x (coerceToD y)

join :: ArrayU -> ArrayU -> Res
join (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ Bi.first (const SizeMismatch) (A.join x y)
join (ArrayD x) (ArrayD y) = fmap (pure . ArrayD) $ Bi.first (const SizeMismatch) $ A.join x y
join (ArrayI x) (ArrayD y) = fmap (pure . ArrayD) $ Bi.first (const SizeMismatch) $ A.join (coerceToD x) y
join (ArrayD x) (ArrayI y) = fmap (pure . ArrayD) $ Bi.first (const SizeMismatch) $ A.join x (coerceToD y)

select :: ArrayU -> ArrayU -> Res
select (ArrayI x) (ArrayI y) = Right . pure . ArrayI $ A.select x y
select (ArrayD x) (ArrayD y) = coerceToExactI x & fmap (pure . ArrayD . (\x' -> A.select x' y))
select (ArrayI x) (ArrayD y) = Right . pure . ArrayD $ A.select x y
select (ArrayD x) (ArrayI y) = coerceToExactI x & fmap (pure . ArrayI . (\x' -> A.select x' y))

pick :: ArrayU -> ArrayU -> Res
pick (ArrayI x) (ArrayI y) = Right . pure . ArrayI $ A.pick x y
pick (ArrayD x) (ArrayD y) = coerceToExactI x & fmap (pure . ArrayD . (\x' -> A.pick x' y))
pick (ArrayI x) (ArrayD y) = Right . pure . ArrayD $ A.pick x y
pick (ArrayD x) (ArrayI y) = coerceToExactI x & fmap (pure . ArrayI . (\x' -> A.pick x' y))

reshape :: ArrayU -> ArrayU -> Res
reshape (ArrayI x) (ArrayI y) = Right . pure . ArrayI $ A.reshape x y
reshape (ArrayD x) (ArrayD y) = coerceToExactI x & fmap (pure . ArrayD . (\x' -> A.reshape x' y))
reshape (ArrayI x) (ArrayD y) = Right . pure . ArrayD $ A.reshape x y
reshape (ArrayD x) (ArrayI y) = coerceToExactI x & fmap (pure . ArrayI . (\x' -> A.reshape x' y))

rerank :: ArrayU -> ArrayU -> Res
rerank (ArrayI x) (ArrayI y) = A.rerank x y & fmap (pure . ArrayI)
rerank (ArrayD x) (ArrayD y) = do
  x' <- coerceToExactI x
  a' <- A.rerank x' y
  pure (pure . ArrayD $ a')
rerank (ArrayI x) (ArrayD y) = A.rerank x y & fmap (pure . ArrayD)
rerank (ArrayD x) (ArrayI y) = do
  x' <- coerceToExactI x
  a' <- A.rerank x' y
  pure (pure . ArrayI $ a')

take :: ArrayU -> ArrayU -> Res
take (ArrayI x) (ArrayI y) = Right . pure . ArrayI $ A.take x y
take (ArrayD x) (ArrayD y) = coerceToExactI x & fmap (pure . ArrayD . (\x' -> A.take x' y))
take (ArrayI x) (ArrayD y) = Right . pure . ArrayD $ A.take x y
take (ArrayD x) (ArrayI y) = coerceToExactI x & fmap (pure . ArrayI . (\x' -> A.take x' y))

drop :: ArrayU -> ArrayU -> Res
drop (ArrayI x) (ArrayI y) = Right . pure . ArrayI $ A.drop x y
drop (ArrayD x) (ArrayD y) = coerceToExactI x & fmap (pure . ArrayD . (\x' -> A.drop x' y))
drop (ArrayI x) (ArrayD y) = Right . pure . ArrayD $ A.drop x y
drop (ArrayD x) (ArrayI y) = coerceToExactI x & fmap (pure . ArrayI . (\x' -> A.drop x' y))

rotate :: ArrayU -> ArrayU -> Res
rotate (ArrayI x) (ArrayI y) = Right . pure . ArrayI $ A.rotate x y
rotate (ArrayD x) (ArrayD y) = coerceToExactI x & fmap (pure . ArrayD . (\x' -> A.rotate x' y))
rotate (ArrayI x) (ArrayD y) = Right . pure . ArrayD $ A.rotate x y
rotate (ArrayD x) (ArrayI y) = coerceToExactI x & fmap (pure . ArrayI . (\x' -> A.rotate x' y))

windows :: ArrayU -> ArrayU -> Res
windows (ArrayI x) (ArrayI y) = Right . pure . ArrayI $ A.windows x y
windows (ArrayD x) (ArrayD y) = do
  x' <- coerceToExactI x
  r <- pure $ A.windows x' y
  pure (pure . ArrayD $ r)
windows (ArrayI x) (ArrayD y) = (Right . pure . ArrayD) (A.windows x y)
windows (ArrayD x) (ArrayI y) = do
  x' <- coerceToExactI x
  r <- pure $ A.windows x' y
  pure (pure . ArrayI $ r)

keep :: ArrayU -> ArrayU -> Res
keep (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) (A.keep x y)
keep (ArrayD x) (ArrayD y) = do
  x' <- coerceToExactI x
  a' <- A.keep x' y
  pure (pure . ArrayD $ a')
keep (ArrayI x) (ArrayD y) = fmap (pure . ArrayD) (A.keep x y)
keep (ArrayD x) (ArrayI y) = do
  x' <- coerceToExactI x
  a' <- A.keep x' y
  pure (pure . ArrayI $ a')

find:: ArrayU -> ArrayU -> Res
find (ArrayI x) (ArrayI y) = fmap (pure . ArrayI) $ A.find x y
find (ArrayD x) (ArrayD y) = fmap (pure . ArrayI) $ A.find x y
find (ArrayI x) (ArrayD y) = fmap (pure . ArrayI) $ A.find (coerceToD x) y
find (ArrayD x) (ArrayI y) = fmap (pure . ArrayI) $ A.find x (coerceToD y)

equalsR :: ArrayU -> Res
equalsR (ArrayI x) = Right $ pure $ ArrayI $ A.equalsR x
equalsR (ArrayD x) = Right $ pure $ ArrayI $ A.equalsR x

notEqualsR :: ArrayU -> Res
notEqualsR (ArrayI x) = Right $ pure $ ArrayI $ A.notEqualsR x
notEqualsR (ArrayD x) = Right $ pure $ ArrayI $ A.notEqualsR x

lessThanR :: ArrayU -> Res
lessThanR (ArrayI x) = Right $ pure $ ArrayI $ A.lessThanR x
lessThanR (ArrayD x) = Right $ pure $ ArrayI $ A.lessThanR x

lessOrEqualR :: ArrayU -> Res
lessOrEqualR (ArrayI x) = Right $ pure $ ArrayI $ A.lessOrEqualR x
lessOrEqualR (ArrayD x) = Right $ pure $ ArrayI $ A.lessOrEqualR x

greaterThanR :: ArrayU -> Res
greaterThanR (ArrayI x) = Right $ pure $ ArrayI $ A.greaterThanR x
greaterThanR (ArrayD x) = Right $ pure $ ArrayI $ A.greaterThanR x

greaterOrEqualR :: ArrayU -> Res
greaterOrEqualR (ArrayI x) = Right $ pure $ ArrayI $ A.greaterOrEqualR x
greaterOrEqualR (ArrayD x) = Right $ pure $ ArrayI $ A.greaterOrEqualR x

addR :: ArrayU -> Res
addR (ArrayI x) = Right $ pure $ ArrayI $ A.addR x
addR (ArrayD x) = Right $ pure $ ArrayD $ A.addR x

subtractR :: ArrayU -> Res
subtractR (ArrayI x) = Right $ pure $ ArrayI $ A.subtractR x
subtractR (ArrayD x) = Right $ pure $ ArrayD $ A.subtractR x

multiplyR :: ArrayU -> Res
multiplyR (ArrayI x) = Right $ pure $ ArrayI $ A.multiplyR x
multiplyR (ArrayD x) = Right $ pure $ ArrayD $ A.multiplyR x

divideR :: ArrayU -> Res
divideR (ArrayI x) = Right $ pure $ ArrayD $ A.divideR $ coerceToD x
divideR (ArrayD x) = Right $ pure $ ArrayD $ A.divideR x

modulusR :: ArrayU -> Res
modulusR (ArrayI x) = Right $ pure $ ArrayI $ A.modulusR x
modulusR (ArrayD x) = Right $ pure $ ArrayI $ A.modulusR $ coerceToI x

powerR :: ArrayU -> Res
powerR (ArrayI x) = Right $ pure $ ArrayD $ A.powerR $ coerceToD x
powerR (ArrayD x) = Right $ pure $ ArrayD $ A.powerR x

logarithmR :: ArrayU -> Res
logarithmR (ArrayI x) = Right $ pure $ ArrayD $ A.logarithmR $ coerceToD x
logarithmR (ArrayD x) = Right $ pure $ ArrayD $ A.logarithmR x

minimumR :: ArrayU -> Res
minimumR (ArrayI x) = Right $ pure $ ArrayI $ A.minimumR x
minimumR (ArrayD x) = Right $ pure $ ArrayD $ A.minimumR x

maximumR :: ArrayU -> Res
maximumR (ArrayI x) = Right $ pure $ ArrayI $ A.maximumR x
maximumR (ArrayD x) = Right $ pure $ ArrayD $ A.maximumR x
