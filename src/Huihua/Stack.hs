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
  Huihua.Stack.not,
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

data Item =
  ItemInt Int | ItemDouble Double |
  ItemArrayInt (Array Int) | ItemArrayDouble (Array Double)
  | ItemFunction (Stack -> Stack) | ItemError HuiHuaWarning

newtype Stack =
  Stack { stackList :: [Item] }

data HuiHuaWarning =
    NYI | EmptyStack1 | EmptyStack2 | ApplyFunction | NotBox | TypeMismatch | SizeMismatch | NotNat | EmptyArray | NotArray deriving (Eq, Ord, Show)

push :: Item -> Stack -> Stack
push (ItemFunction f) s = f s
push i (Stack s) = Stack (i:s)

duplicate :: Stack -> Stack
duplicate (Stack []) = Stack [ItemError EmptyStack1]
duplicate (Stack s@(x:_)) = Stack (x:s)

over :: Stack -> Stack
over (Stack []) = Stack [ItemError EmptyStack1]
over (Stack [_]) = Stack [ItemError EmptyStack2]
over (Stack s@(_:x:_)) = Stack (x:s)

flip :: Stack -> Stack
flip (Stack []) = Stack [ItemError EmptyStack1]
flip (Stack [_]) = Stack [ItemError EmptyStack2]
flip (Stack s@(x:x':_)) = Stack (x':x:s)

pop :: Stack -> Stack
pop (Stack []) = Stack [ItemError EmptyStack1]
pop (Stack (_:xs)) = Stack xs

identity :: Stack -> Stack
identity = id

not :: Item -> Item
not (ItemInt i) = ItemInt (1 - i)
not (ItemDouble d) = ItemDouble (1 - d)
not (ItemArrayInt xs) = ItemArrayInt (fmap (1-) xs)
not (ItemArrayDouble xs) = ItemArrayDouble (fmap (1-) xs)
not (ItemFunction _) = ItemError ApplyFunction
not i@(ItemError _) = i

unaryOp :: (Int -> Int) -> (Double -> Double) -> Item -> Item
unaryOp op _ (ItemInt i) = ItemInt (op i)
unaryOp _ op (ItemDouble d) = ItemDouble (op d)
unaryOp op _ (ItemArrayInt xs) = ItemArrayInt (fmap op xs)
unaryOp _ op (ItemArrayDouble xs) = ItemArrayDouble (fmap op xs)
unaryOp _ _ (ItemFunction _) = ItemError ApplyFunction
unaryOp _ _ i@(ItemError _) = i

unaryOpD :: (Double -> Double) -> Item -> Item
unaryOpD op (ItemInt i) = ItemDouble ((op . fromIntegral) i)
unaryOpD op (ItemDouble d) = ItemDouble (op d)
unaryOpD op (ItemArrayInt xs) = ItemArrayDouble (fmap (op . fromIntegral) xs)
unaryOpD op (ItemArrayDouble xs) = ItemArrayDouble (fmap op xs)
unaryOpD _ (ItemFunction _) = ItemError ApplyFunction
unaryOpD _ i@(ItemError _) = i

unaryOpC :: (Double -> Int) -> Item -> Item
unaryOpC _ i@(ItemInt _) = i
unaryOpC op (ItemDouble d) = ItemInt (op d)
unaryOpC _ i@(ItemArrayInt _) = i
unaryOpC op (ItemArrayDouble xs) = ItemArrayInt (fmap op xs)
unaryOpC _ (ItemFunction _) = ItemError ApplyFunction
unaryOpC _ i@(ItemError _) = i

fromInt :: Item -> Item
fromInt (ItemInt x) = ItemDouble (fromIntegral x)
fromInt (ItemArrayInt xs) = ItemArrayDouble (fmap fromIntegral xs)
fromInt x = x

-- | sign
--
-- >>> sign (ItemInt 1)
-- ItemInt 1
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

binOp :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Item -> Item -> Item
binOp op _ (ItemInt i) (ItemInt i') = ItemInt $ op i i'
binOp _ op (ItemDouble d) (ItemDouble d') = ItemDouble $ op d d'
binOp op _ (ItemArrayInt xs) (ItemArrayInt xs') =
  either ItemError ItemArrayInt (binArray op xs xs')
binOp _ op (ItemArrayDouble xs) (ItemArrayDouble xs') =
    either ItemError ItemArrayDouble (binArray op xs xs')
binOp _ _ (ItemInt _) _ = ItemError TypeMismatch
binOp _ _ (ItemDouble _) _ = ItemError TypeMismatch
binOp _ _ (ItemArrayInt _) _ = ItemError TypeMismatch
binOp _ _ (ItemArrayDouble _) _ = ItemError TypeMismatch
binOp _ _ (ItemFunction _) (ItemFunction _) = ItemInt 0
binOp _ _ (ItemFunction _) _ = ItemError TypeMismatch
binOp _ _ _ e@(ItemError _) = e
binOp _ _ e@(ItemError _) _ = e

-- up type Int to a Double if we have to
binOpU :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Item -> Item -> Item
binOpU op _ (ItemInt i) (ItemInt i') = ItemInt $ op i i'
binOpU _ op (ItemDouble d) (ItemDouble d') = ItemDouble $ op d d'
binOpU _ op (ItemInt d) (ItemDouble d') = ItemDouble $ op (fromIntegral d) d'
binOpU _ op (ItemDouble d) (ItemInt d') = ItemDouble $ op d (fromIntegral d')
binOpU op _ (ItemArrayInt xs) (ItemArrayInt xs') =
  either ItemError ItemArrayInt (binArray op xs xs')
binOpU _ op (ItemArrayDouble xs) (ItemArrayDouble xs') =
    either ItemError ItemArrayDouble (binArray op xs xs')
binOpU _ op (ItemArrayInt xs) (ItemArrayDouble xs') =
    either ItemError ItemArrayDouble (binArray op (fmap fromIntegral xs) xs')
binOpU _ op (ItemArrayDouble xs) (ItemArrayInt xs') =
    either ItemError ItemArrayDouble (binArray op xs (fmap fromIntegral xs'))
binOpU _ _ (ItemInt _) _ = ItemError TypeMismatch
binOpU _ _ (ItemDouble _) _ = ItemError TypeMismatch
binOpU _ _ (ItemArrayInt _) _ = ItemError TypeMismatch
binOpU _ _ (ItemArrayDouble _) _ = ItemError TypeMismatch
binOpU _ _ (ItemFunction _) (ItemFunction _) = ItemInt 0
binOpU _ _ (ItemFunction _) _ = ItemError TypeMismatch
binOpU _ _ _ e@(ItemError _) = e
binOpU _ _ e@(ItemError _) _ = e

binOpD :: (Double -> Double -> Double) -> Item -> Item -> Item
binOpD op (ItemInt x) (ItemInt x') = ItemDouble $ op (fromIntegral x) (fromIntegral x')
binOpD op (ItemInt x) (ItemDouble x') = ItemDouble $ op (fromIntegral x) x'
binOpD op (ItemDouble x) (ItemInt x') = ItemDouble $ op x (fromIntegral x')
binOpD op (ItemDouble x) (ItemDouble x') = ItemDouble $ op x x'
binOpD op (ItemArrayInt xs) (ItemArrayInt xs') =
  either ItemError ItemArrayDouble (binArray op (fmap fromIntegral xs) (fmap fromIntegral xs'))
binOpD op (ItemArrayDouble xs) (ItemArrayDouble xs') =
    either ItemError ItemArrayDouble (binArray op xs xs')
binOpD op (ItemArrayInt xs) (ItemArrayDouble xs') =
  either ItemError ItemArrayDouble (binArray op (fmap fromIntegral xs) xs')
binOpD op (ItemArrayDouble xs) (ItemArrayInt xs') =
  either ItemError ItemArrayDouble (binArray op xs (fmap fromIntegral xs'))
binOpD _ (ItemInt _) _ = ItemError TypeMismatch
binOpD _ (ItemDouble _) _ = ItemError TypeMismatch
binOpD _ (ItemArrayInt _) _ = ItemError TypeMismatch
binOpD _ (ItemArrayDouble _) _ = ItemError TypeMismatch
binOpD _ (ItemFunction _) (ItemFunction _) = ItemInt 0
binOpD _ (ItemFunction _) _ = ItemError TypeMismatch
binOpD _ _ e@(ItemError _) = e
binOpD _ e@(ItemError _) _ = e

-- down type to Int
binToInt :: (Int -> Int -> Int) -> (Double -> Double -> Int) -> Item -> Item -> Item
binToInt op _ (ItemInt i) (ItemInt i') = ItemInt $ op i i'
binToInt _ op (ItemDouble d) (ItemDouble d') = ItemInt $ op d d'
binToInt _ op (ItemInt d) (ItemDouble d') = ItemInt $ op (fromIntegral d) d'
binToInt _ op (ItemDouble d) (ItemInt d') = ItemInt $ op d (fromIntegral d')
binToInt op _ (ItemArrayInt xs) (ItemArrayInt xs') =
  either ItemError ItemArrayInt (binArray op xs xs')
binToInt _ op (ItemArrayDouble xs) (ItemArrayDouble xs') =
    either ItemError ItemArrayInt (binArray op xs xs')
binToInt _ op (ItemArrayInt xs) (ItemArrayDouble xs') =
    either ItemError ItemArrayInt (binArray op (fmap fromIntegral xs) xs')
binToInt _ op (ItemArrayDouble xs) (ItemArrayInt xs') =
    either ItemError ItemArrayInt (binArray op xs (fmap fromIntegral xs'))
binToInt _ _ (ItemInt _) _ = ItemError TypeMismatch
binToInt _ _ (ItemDouble _) _ = ItemError TypeMismatch
binToInt _ _ (ItemArrayInt _) _ = ItemError TypeMismatch
binToInt _ _ (ItemArrayDouble _) _ = ItemError TypeMismatch
binToInt _ _ (ItemFunction _) (ItemFunction _) = ItemInt 0
binToInt _ _ (ItemFunction _) _ = ItemError TypeMismatch
binToInt _ _ _ e@(ItemError _) = e
binToInt _ _ e@(ItemError _) _ = e

add :: Item -> Item -> Item
add = binOp (+) (+)

subtract :: Item -> Item -> Item
subtract = binOp (-) (-)

multiply :: Item -> Item -> Item
multiply = binOp (*) (*)

divide :: Item -> Item -> Item
divide = binOpD (/)

equals :: Item -> Item -> Item
equals = binToInt (\x x' -> sig (x==x')) (\x x' -> sig (x==x'))

notequals :: Item -> Item -> Item
notequals = binToInt (\x x' -> sig (x/=x')) (\x x' -> sig (x/=x'))

lt :: Item -> Item -> Item
lt = binToInt (\x x' -> sig (x<x')) (\x x' -> sig (x<x'))

lte :: Item -> Item -> Item
lte = binToInt (\x x' -> sig (x<=x')) (\x x' -> sig (x<=x'))

gt :: Item -> Item -> Item
gt = binToInt (\x x' -> sig (x>x')) (\x x' -> sig (x>x'))

gte :: Item -> Item -> Item
gte = binToInt (\x x' -> sig (x>=x')) (\x x' -> sig (x>=x'))

modulus :: Item -> Item -> Item
modulus = binOpU mod (\d n -> n - d * fromIntegral (NumHask.Prelude.floor (n/d)))

power :: Item -> Item -> Item
power = binOpU (\x x' -> NumHask.Prelude.floor ((fromIntegral x :: Double) ^^ x')) (**)

logarithm :: Item -> Item -> Item
logarithm = binOpD (\x x' -> log x' - log x)

minimum :: Item -> Item -> Item
minimum = binOpU P.min  P.min

maximum :: Item -> Item -> Item
maximum = binOpU P.max  P.max

arctangent :: Item -> Item -> Item
arctangent = binOpD atan2

length :: Item -> Item
length (ItemArrayInt xs) = ItemInt $ P.length xs
length (ItemArrayDouble xs) = ItemInt $ P.length xs
length (ItemInt _) = ItemInt 1
length (ItemDouble _) = ItemInt 1
length (ItemFunction _) = ItemInt 1
length i@(ItemError _) = i

shape :: Item -> Item
shape (ItemArrayInt xs) = ItemArrayInt $ Array [P.length xs'] (V.fromList xs')
  where
    xs' = NumHask.Array.Dynamic.shape xs
shape (ItemArrayDouble xs) = ItemArrayInt $ Array [P.length xs'] (V.fromList xs')
  where
    xs' = NumHask.Array.Dynamic.shape xs
shape (ItemInt _) = ItemArrayInt (Array [0] (V.fromList []))
shape (ItemDouble _) = ItemArrayInt (Array [0] (V.fromList []))
shape (ItemFunction _) = ItemArrayInt (Array [0] (V.fromList []))
shape i@(ItemError _) = i

itemCoerceDouble :: Item -> Item
itemCoerceDouble (ItemInt x) = ItemDouble (fromIntegral x)
itemCoerceDouble (ItemArrayInt xs) = ItemArrayDouble (fmap fromIntegral xs)
itemCoerceDouble x = x

itemCoerceInt :: Item -> Item
itemCoerceInt (ItemDouble x) = bool (ItemError NotNat) (ItemInt (P.floor x)) (x==fromIntegral (P.floor x))
itemCoerceInt (ItemArrayDouble xs) = bool (ItemError NotNat) (ItemArrayInt (fmap P.floor xs)) (all (\x -> x==fromIntegral (P.floor x)) xs)
itemCoerceInt x = x

range :: Item -> Item
range (ItemInt x) = ItemArrayInt (Array [x] (V.fromList [0..(x-1)]))
range x@(ItemDouble _) = range (itemCoerceInt x)
-- FIXME: An applicative permute
-- table couple (fmap (range . ItemInt) xs)
range (ItemArrayInt _) = ItemError NYI
range x@(ItemArrayDouble _) = range (itemCoerceInt x)
range (ItemFunction _) = ItemError NotNat
range i@(ItemError _) = i

first :: Item -> Item
first (ItemArrayInt (Array _ xs)) = bool (ItemInt $ V.head xs) (ItemError EmptyArray) (V.empty == xs)
first (ItemArrayDouble (Array _ xs)) = bool (ItemDouble $ V.head xs) (ItemError EmptyArray) (V.empty == xs)
first i@(ItemError _) = i
first _ = ItemError NotArray
