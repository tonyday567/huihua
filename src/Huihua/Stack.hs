{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Huihua.Stack
  (
  Item(..),
  Stack (..),
  push,
  duplicate,
  over,
  Huihua.Stack.flip,
  pop,
  identity,
  sign,
  Huihua.Stack.negate,
  absolute,
  Huihua.Stack.sqrt,
  Huihua.Stack.sin,
  Huihua.Stack.floor,
  Huihua.Stack.ceiling,
  Huihua.Stack.round,
  sig,
  unaryOp,
  unaryOpA,
  binOp,
  fromInt,
  add,
  subtract,
  multiply,
  divide,
  binOpD,
  binOpU,
  equals,
  notequals,
  lt,
  lte,
  gt,
  gte,
  modulus,
  power,
  logarithm,
  Huihua.Stack.minimum,
  Huihua.Stack.maximum,
  arctangent,
  Huihua.Stack.length,
  Huihua.Stack.shape,
  Huihua.Stack.range,
  itemCoerceDouble,
  itemCoerceInt,
  Huihua.Stack.first,
  )
where

import NumHask.Prelude
import NumHask.Prelude qualified as P
import NumHask.Array.Dynamic
import NumHask.Array.Shape
import Data.Distributive (Distributive (..))
import Data.Functor.Rep
import Data.Vector qualified as V
import Huihua.Warning

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Huihua.Stack as S
-- >>> import NumHask.Array.Dynamic as A

data Item = ItemArrayInt (Array Int) | ItemArrayDouble (Array Double) deriving (Show, Eq)

newtype Stack =
  Stack { stackList :: [Item] } deriving (Show, Eq)

push :: Item -> Stack -> Stack
push i (Stack s) = Stack (i:s)

duplicate :: Stack -> Either HuiHuaWarning Stack
duplicate (Stack []) = Left EmptyStack1
duplicate (Stack s@(x:_)) = Right (Stack (x:s))

over :: Stack -> Either HuiHuaWarning Stack
over (Stack []) = Left EmptyStack1
over (Stack [_]) = Left EmptyStack2
over (Stack s@(_:x:_)) = Right (Stack (x:s))

flip :: Stack -> Either HuiHuaWarning Stack
flip (Stack []) = Left EmptyStack1
flip (Stack [_]) = Left EmptyStack2
flip (Stack (x:x':xs)) = Right (Stack (x':x:xs))

pop :: Stack -> Either HuiHuaWarning Stack
pop (Stack []) = Left EmptyStack1
pop (Stack (_:xs)) = Right (Stack xs)

identity :: Stack -> Stack
identity = id

unaryOpA :: (Array Int -> Array Int) -> (Array Double -> Array Double) -> Item -> Item
unaryOpA op _ (ItemArrayInt xs) = ItemArrayInt (op xs)
unaryOpA _ op (ItemArrayDouble xs) = ItemArrayDouble (op xs)

unaryOp :: (Int -> Int) -> (Double -> Double) -> Item -> Item
unaryOp op _ (ItemArrayInt xs) = ItemArrayInt (fmap op xs)
unaryOp _ op (ItemArrayDouble xs) = ItemArrayDouble (fmap op xs)

unaryOpD :: (Double -> Double) -> Item -> Item
unaryOpD op (ItemArrayInt xs) = ItemArrayDouble (fmap (op . fromIntegral) xs)
unaryOpD op (ItemArrayDouble xs) = ItemArrayDouble (fmap op xs)

unaryOpC :: (Double -> Int) -> Item -> Item
unaryOpC _ i@(ItemArrayInt _) = i
unaryOpC op (ItemArrayDouble xs) = ItemArrayInt (fmap op xs)

fromInt :: Item -> Item
fromInt (ItemArrayInt xs) = ItemArrayDouble (fmap fromIntegral xs)
fromInt x = x

-- | sign
--
-- >>> sign (ItemArrayInt (toScalar 1))
-- ItemArrayInt 1
sign :: Item -> Item
sign = unaryOp signum signum

negate :: Item -> Item
negate = unaryOp NumHask.Prelude.negate NumHask.Prelude.negate

absolute :: Item -> Item
absolute = unaryOp NumHask.Prelude.abs NumHask.Prelude.abs

sqrt :: Item -> Item
sqrt = unaryOpD NumHask.Prelude.sqrt

sin :: Item -> Item
sin = unaryOpD NumHask.Prelude.sin

floor :: Item -> Item
floor = unaryOpC NumHask.Prelude.floor

ceiling :: Item -> Item
ceiling = unaryOpC NumHask.Prelude.ceiling

round :: Item -> Item
round = unaryOpC NumHask.Prelude.round

sig :: Bool -> Int
sig = bool zero one

binArray :: (a -> b -> c) -> Array a -> Array b -> Either HuiHuaWarning (Array c)
binArray op (Array s xs) (Array s' xs') =
  bool (Left SizeMismatch) (Right $ Array s (V.zipWith op xs xs')) (s==s')

binOp :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Item -> Item -> Either HuiHuaWarning Item
binOp op _ (ItemArrayInt xs) (ItemArrayInt xs') =
  fmap ItemArrayInt (binArray op xs xs')
binOp _ op (ItemArrayDouble xs) (ItemArrayDouble xs') =
    fmap ItemArrayDouble (binArray op xs xs')
binOp _ _ (ItemArrayInt _) _ = Left TypeMismatch
binOp _ _ (ItemArrayDouble _) _ = Left TypeMismatch

-- up type Int to a Double if we have to
binOpU :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Item -> Item -> Either HuiHuaWarning Item
binOpU op _ (ItemArrayInt xs) (ItemArrayInt xs') =
  fmap ItemArrayInt (binArray op xs xs')
binOpU _ op (ItemArrayDouble xs) (ItemArrayDouble xs') =
    fmap ItemArrayDouble (binArray op xs xs')
binOpU _ op (ItemArrayInt xs) (ItemArrayDouble xs') =
    fmap ItemArrayDouble (binArray op (fmap fromIntegral xs) xs')
binOpU _ op (ItemArrayDouble xs) (ItemArrayInt xs') =
    fmap ItemArrayDouble (binArray op xs (fmap fromIntegral xs'))

binOpD :: (Double -> Double -> Double) -> Item -> Item -> Either HuiHuaWarning Item
binOpD op (ItemArrayInt xs) (ItemArrayInt xs') =
  fmap ItemArrayDouble (binArray op (fmap fromIntegral xs) (fmap fromIntegral xs'))
binOpD op (ItemArrayDouble xs) (ItemArrayDouble xs') =
    fmap ItemArrayDouble (binArray op xs xs')
binOpD op (ItemArrayInt xs) (ItemArrayDouble xs') =
  fmap ItemArrayDouble (binArray op (fmap fromIntegral xs) xs')
binOpD op (ItemArrayDouble xs) (ItemArrayInt xs') =
  fmap ItemArrayDouble (binArray op xs (fmap fromIntegral xs'))

-- down type to Int
binToInt :: (Int -> Int -> Int) -> (Double -> Double -> Int) -> Item -> Item -> Either HuiHuaWarning Item
binToInt op _ (ItemArrayInt xs) (ItemArrayInt xs') =
  fmap ItemArrayInt (binArray op xs xs')
binToInt _ op (ItemArrayDouble xs) (ItemArrayDouble xs') =
    fmap ItemArrayInt (binArray op xs xs')
binToInt _ op (ItemArrayInt xs) (ItemArrayDouble xs') =
    fmap ItemArrayInt (binArray op (fmap fromIntegral xs) xs')
binToInt _ op (ItemArrayDouble xs) (ItemArrayInt xs') =
    fmap ItemArrayInt (binArray op xs (fmap fromIntegral xs'))

add :: Item -> Item -> Either HuiHuaWarning Item
add = binOp (+) (+)

subtract :: Item -> Item -> Either HuiHuaWarning Item
subtract = binOp (-) (-)

multiply :: Item -> Item -> Either HuiHuaWarning Item
multiply = binOp (*) (*)

divide :: Item -> Item -> Either HuiHuaWarning Item
divide = binOpD (/)

equals :: Item -> Item -> Either HuiHuaWarning Item
equals = binToInt (\x x' -> sig (x==x')) (\x x' -> sig (x==x'))

notequals :: Item -> Item -> Either HuiHuaWarning Item
notequals = binToInt (\x x' -> sig (x/=x')) (\x x' -> sig (x/=x'))

lt :: Item -> Item -> Either HuiHuaWarning Item
lt = binToInt (\x x' -> sig (x<x')) (\x x' -> sig (x<x'))

lte :: Item -> Item -> Either HuiHuaWarning Item
lte = binToInt (\x x' -> sig (x<=x')) (\x x' -> sig (x<=x'))

gt :: Item -> Item -> Either HuiHuaWarning Item
gt = binToInt (\x x' -> sig (x>x')) (\x x' -> sig (x>x'))

gte :: Item -> Item -> Either HuiHuaWarning Item
gte = binToInt (\x x' -> sig (x>=x')) (\x x' -> sig (x>=x'))

modulus :: Item -> Item -> Either HuiHuaWarning Item
modulus = binOpU mod (\d n -> n - d * fromIntegral (NumHask.Prelude.floor (n/d)))

power :: Item -> Item -> Either HuiHuaWarning Item
power = binOpU (\x x' -> NumHask.Prelude.floor ((fromIntegral x :: Double) ^^ x')) (**)

logarithm :: Item -> Item -> Either HuiHuaWarning Item
logarithm = binOpD (\x x' -> log x' - log x)

minimum :: Item -> Item -> Either HuiHuaWarning Item
minimum = binOpU P.min P.min

maximum :: Item -> Item -> Either HuiHuaWarning Item
maximum = binOpU P.max P.max

arctangent :: Item -> Item -> Either HuiHuaWarning Item
arctangent = binOpD atan2

length :: Item -> Item
length (ItemArrayInt xs) = ItemArrayInt $ toScalar (P.length xs)
length (ItemArrayDouble xs) = ItemArrayInt $ toScalar (P.length xs)

shape :: Item -> Item
shape (ItemArrayInt xs) = ItemArrayInt $ Array [P.length xs'] (V.fromList xs')
  where
    xs' = NumHask.Array.Dynamic.shape xs
shape (ItemArrayDouble xs) = ItemArrayInt $ Array [P.length xs'] (V.fromList xs')
  where
    xs' = NumHask.Array.Dynamic.shape xs

itemCoerceDouble :: Item -> Item
itemCoerceDouble (ItemArrayInt xs) = ItemArrayDouble (fmap fromIntegral xs)
itemCoerceDouble x = x

itemCoerceInt :: Item -> Either HuiHuaWarning Item
itemCoerceInt (ItemArrayDouble xs) = bool (Left NotNat) (Right $ ItemArrayInt (fmap P.floor xs)) (all (\x -> x==fromIntegral (P.floor x)) xs)
itemCoerceInt x = Right x

range :: Item -> Either HuiHuaWarning Item
-- FIXME: An applicative permute
-- table couple (fmap (range . ItemInt) xs)
range (ItemArrayInt _) = Left NYI
range (ItemArrayDouble _) = undefined -- Right $ range (itemCoerceInt x)

first :: Item -> Either HuiHuaWarning Item
first (ItemArrayInt (Array _ xs)) = bool (Right $ ItemArrayInt $ toScalar (V.head xs)) (Left EmptyArray) (V.empty == xs)
first (ItemArrayDouble (Array _ xs)) = bool (Right $ ItemArrayDouble $ toScalar (V.head xs)) (Left EmptyArray) (V.empty == xs)
