{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Huihua.Array
  (
  Huihua.Array.not,
  sign,
  Huihua.Array.negate,
  absolute,
  Huihua.Array.sqrt,
  Huihua.Array.sin,
  Huihua.Array.floor,
  Huihua.Array.ceiling,
  Huihua.Array.round,
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
  power,
  logarithm,
  Huihua.Array.minimum,
  Huihua.Array.maximum,
  arctangent,
  Huihua.Array.length,
  Huihua.Array.shape,
  Huihua.Array.range,
  coerceInt,
  coerceDouble,
  modulusD,
  first,
  indexA,
  pick,
  fromList1,
  fromList2,
  deshape,
  bits,
  Huihua.Array.transpose,
  Huihua.Array.rotate,
  Huihua.Array.reverse,
  rise,
  fall,
  keep,
  where',
  classify,
  deduplicate,
  match,
  couple,
  join,
  select,
  Huihua.Array.reshape,
  Huihua.Array.take,
  Huihua.Array.drop,
  )
where

import NumHask.Prelude
import NumHask.Prelude qualified as P
import NumHask.Array.Dynamic
import NumHask.Array.Dynamic qualified as D
import NumHask.Array.Shape
import NumHask.Array.Sort qualified as Sort
import Data.Distributive (Distributive (..))
import Data.Functor.Rep
import Data.Vector qualified as V
import Data.Bits
import Data.Ord
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Huihua.Stack (Item (..))

data HuiHuaWarning =
    NYI | EmptyStack1 | EmptyStack2 | ApplyFunction | NotBox | TypeMismatch | SizeMismatch | NotNat | EmptyArray | NotArray deriving (Eq, Ord, Show)

-- $setup
--
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeFamilies
-- >>> :set -XFlexibleContexts
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Array.Dynamic
-- >>> import NumHask.Array.Shape
-- >>> let s = fromFlatList [] [1] :: Array Int
-- >>> let a = fromFlatList [2,3,4] [1..24] :: Array Int
-- >>> let v = fromFlatList [3] [1,2,3] :: Array Int
-- >>> let m = fromFlatList [3,4] [0..11] :: Array Int

not :: (Ring a) => Array a -> Array a
not = fmap (one-)

sign :: (Base a~a, Basis a) => Array a -> Array a
sign = fmap signum

negate :: (Subtractive a) => Array a -> Array a
negate = fmap P.negate

absolute :: (Mag a~a, Basis a) => Array a -> Array a
absolute = fmap P.abs

sqrt :: (ExpField a) => Array a -> Array a
sqrt = fmap NumHask.Prelude.sqrt

sin :: (TrigField a) => Array a -> Array a
sin = fmap NumHask.Prelude.sin

floor :: (QuotientField a, Ring (Whole a)) => Array a -> Array (Whole a)
floor = fmap NumHask.Prelude.floor

ceiling :: (QuotientField a, Ring (Whole a)) => Array a -> Array (Whole a)
ceiling = fmap NumHask.Prelude.ceiling

round :: (QuotientField a, Eq (Whole a), Ring (Whole a)) => Array a -> Array (Whole a)
round = fmap NumHask.Prelude.round

sig :: Bool -> Int
sig = bool zero one

binArray :: (a -> b -> c) -> Array a -> Array b -> Either HuiHuaWarning (Array c)
binArray op (Array s xs) (Array s' xs') =
  bool (Left SizeMismatch) (Right $ Array s (V.zipWith op xs xs')) (s==s')

add :: (Additive a) => Array a -> Array a -> Either HuiHuaWarning (Array a)
add = binArray (+)

subtract :: (Subtractive a) => Array a -> Array a -> Either HuiHuaWarning (Array a)
subtract = binArray (-)

multiply :: (Multiplicative a) => Array a -> Array a -> Either HuiHuaWarning (Array a)
multiply = binArray (*)

divide :: (Divisive a) => Array a -> Array a -> Either HuiHuaWarning (Array a)
divide = binArray (/)

binOpBool :: (a -> a -> Bool) -> Array a -> Array a -> Either HuiHuaWarning (Array Int)
binOpBool f = binArray (\x x' -> sig (f x x'))

equals :: (Eq a) => Array a -> Array a -> Either HuiHuaWarning (Array Int)
equals = binOpBool (==)

notequals :: (Eq a) => Array a -> Array a -> Either HuiHuaWarning (Array Int)
notequals = binOpBool (/=)

lt :: (Ord a) => Array a -> Array a -> Either HuiHuaWarning (Array Int)
lt = binOpBool (<)

lte :: (Ord a) => Array a -> Array a -> Either HuiHuaWarning (Array Int)
lte = binOpBool (<=)

gt :: (Ord a) => Array a -> Array a -> Either HuiHuaWarning (Array Int)
gt = binOpBool (>)

gte :: (Ord a) => Array a -> Array a -> Either HuiHuaWarning (Array Int)
gte = binOpBool (>=)

modulus :: (Integral a) => Array a -> Array a -> Either HuiHuaWarning (Array a)
modulus = binArray mod

modulusD :: (QuotientField a, Ring (Whole a), FromIntegral a (Whole a)) => Array a -> Array a -> Either HuiHuaWarning (Array a)
modulusD = binArray (\d n -> n - d * fromIntegral (NumHask.Prelude.floor (n/d)))

power :: (ExpField a) => Array a -> Array a -> Either HuiHuaWarning (Array a)
power = binArray (**)

logarithm :: (ExpField a) => Array a -> Array a -> Either HuiHuaWarning (Array a)
logarithm = binArray (\x x' -> log x' - log x)

minimum :: (Ord a) => Array a -> Array a -> Either HuiHuaWarning (Array a)
minimum = binArray P.min

maximum :: (Ord a) => Array a -> Array a -> Either HuiHuaWarning (Array a)
maximum = binArray P.max

arctangent :: (TrigField a) => Array a -> Array a -> Either HuiHuaWarning (Array a)
arctangent = binArray atan2

length :: Array a -> Array Int
length (Array [] _) = Array [] V.empty
length (Array (s:_) _) = Array [1] (V.singleton s)

shape :: Array a -> Array Int
shape (Array s _) = Array [P.length s] (V.fromList s)

coerceDouble :: Array Int -> Array Double
coerceDouble = fmap fromIntegral

coerceInt :: Array Double -> Either HuiHuaWarning (Array Int)
coerceInt (Array s xs) = bool (Left NotNat) (Right $ Array s $ fmap P.floor xs) (all (\x -> x==fromIntegral (P.floor x)) xs)

range :: Array Int -> Array Int
range (Array s xs) =
  D.squeeze $ D.tabulate (V.toList xs <> s) (\xs' -> xs' !! last xs')

first :: Array a -> Array a
first = selects [0] [0]

indexA :: Array Int -> Array a -> a
indexA i a = D.index a (snd $ D.toFlatList i)

-- |
-- >>> a = (fromFlatList [2,3] [1,2,3,4,5,6]) :: Array Int
-- >>> pick (range . Huihua.Array.shape $ a) a == a
-- >>> pick (fromFlatList [2,2] [0, 1, 1, 2]) a
pick :: Array Int -> Array a -> Array a
pick i a = fmap (\x -> indexA x a) (extracts [0..(P.length (D.shape i) - 2)] i)

fromList1 :: [a] -> Array a
fromList1 xs = fromFlatList [P.length xs] xs

fromList2 :: Int -> [a] -> Array a
fromList2 r xs = fromFlatList [r, P.length xs `div` r] xs

-- FIXME: technical, should off deshape along specified dimensions
deshape :: Array a -> Array a
deshape a = D.reshape [product (D.shape a)] a

bits' :: Int -> Array Int
bits' x =
  [0..] & P.take (finiteBitSize x - countLeadingZeros x) & fmap (\b -> x .&. bit b) & fromList1

bits :: Array Int -> Array Int
bits = fmap bits' >>> joins [0]

-- |
-- >>> D.transpose (fromFlatList [2,2,2] [1..8])
-- FIXME: huihua example transposes 001 to 010. A 1 rotation.
-- This transposes 001 to 100
transpose :: Array a -> Array a
transpose = D.transpose

-- | A.rotate (fromList1 [1]) (A.range (fromList1 [5]))
-- >>> A.rotate (fromList1 [1,2]) (fromFlatList [4,5] [0..19])
rotate :: Array Int -> Array a -> Array a
rotate r a = D.rotates (zip [0..] (snd $ toFlatList r)) a

reverse :: Array a -> Array a
reverse a = reverses [0] a

-- >>> rise (fromList1 [6,2,7,0,-1,5])
rise :: (Ord a) => Array a -> Array Int
rise a = Array [V.length o] o
  where
    o = Sort.order $ unArray $ fmap unArray $ extracts [0] a

-- >>> rise (fromList1 [6,2,7,0,-1,5])
fall :: (Ord a) => Array a -> Array Int
fall a = Array [V.length o] o
  where
    o = Sort.orderBy Down $ unArray $ fmap unArray $ extracts [0] a

-- >>> keep (fromList1 [1, 0, 2, 3, 1]) (fromList1 [8,3, 9, 2, 3::Int])
-- FIXME: fix Scalar version
-- >>> keep (toScalar 4) (fromList1 [1..5])
keep :: Array Int -> Array a -> Either String (Array a)
keep k a = fromList1 . fold <$> D.liftR2 ($) (fmap replicate k) a

-- >>> where' (fromList1 [1,2,3])
-- Right [0, 1, 1, 2, 2, 2]
where' :: Array Int -> Either String (Array Int)
where' a = Huihua.Array.length a & range & keep a

classify :: (Ord a) => Array a -> Array Int
classify a = fromList1 $ fst <$> mscan (toList $ extracts [0] a)
  where
    mscan [] = []
    mscan (x:xs) =
      scanl
      (\(s, m) k ->
         maybe (1+s, Map.insert k (1+s) m) (,m)
         (Map.lookup a m)) (0,Map.singleton x (0::Int))
      xs

-- >>> deduplicate (fromList1 [2,3,3,3,1,1::Int])
-- [2, 3, 1]
deduplicate :: (Ord a) => Array a -> Array a
deduplicate a = joins [0] $ fromList1 $ mapMaybe fst (dscan (toList $ extracts [0] a))
  where
    dscan [] = []
    dscan (x0:xs) =
      scanl
      (\(_, set) k ->
         bool (Just k, Set.insert k set) (Nothing, set)
         (Set.member k set)) (Just x0,Set.singleton x0)
      xs

match :: (Eq a) => Array a -> Array a -> Array Int
match a a' = fromList1 [bool 0 1 (a==a')]

-- |
-- >>> couple (fromList1 [1,2,3]) (fromList1 [4,5,6::Int])
-- [[1, 2, 3],
--  [4, 5, 6]]
couple :: Array a -> Array a -> Array a
couple a a' =
  concatenate 0 (D.reshape (1:D.shape a) a) (D.reshape (1:D.shape a') a')

-- |
-- >>> join (fromFlatList [2] [1,2]) (fromFlatList [2,2] [5,6,7,8::Int])
-- Right [[1, 2],
--  [5, 6],
--  [7, 8]]
join :: Array a -> Array a -> Either String (Array a)
join a a'
  | P.drop 1 (D.shape a) == P.drop 1 (D.shape a') = Right $ concatenate 0 a a'
  | D.shape a == P.drop 1 (D.shape a') = Right $ concatenate 0 (D.reshape (1:D.shape a) a) a'
  | P.drop 1 (D.shape a) == D.shape a' = Right $ concatenate 0 a (D.reshape (1:D.shape a') a')
  | otherwise = Left "Shape Mismatch"

-- (\x -> selects [0] [x] (fromFlatList [2,2] [5,6,7,8::Int])) <$> [1,0]
-- [[7, 8],[5, 6]]
-- (\x -> selects [0] [x] (fromFlatList [4] [5,6,7,8::Int])) <$> [1,0]
--  joins [0] $ (\x -> selects [0] [x] (fromFlatList [4] [2,3,5,7::Int])) <$> (fromFlatList [3,2] [0,1,1,2,2,3])
-- joins [0] $ (\x -> selects [0] [x] (fromFlatList [3,3] [1..9::Int])) <$> (fromFlatList [4] [0,2,1,1])
-- a2 = joins [0,1] ((\x -> selects [0] [x] (fromFlatList [2,2] [2,3,5,7::Int])) <$> (fromFlatList [2,2] [0,1,1,0]))
 -- a1 = joins [0] $ (\x -> selects [0] [x] (fromFlatList [4] [2,3,5,7::Int])) <$> (fromFlatList [4] [0,1,1,0])

-- |
-- >>> select (fromFlatList [4] [0,1,1,0]) (fromFlatList [4] [2,3,5,7::Int])
-- >>> select (fromFlatList [2,2] [0,1,1,0]) (fromFlatList [2,2] [2,3,5,7::Int])
select :: Array Int -> Array a -> Array a
select i a = joins [0..(P.length (D.shape i) - 1)] $ (\x -> selects [0] [x] a) <$> i

-- |
-- FIXME: Scalar version
--
-- >>> Huihua.Array.reshape (fromList1 [3,2]) (fromList1 [1..5::Int])
-- [[1, 2],
--  [3, 4],
--  [5, 1]]
-- >>>  Huihua.Array.reshape (fromList1 [3,-1]) (fromList1 [1..8::Int])
-- [[1, 2],
--  [3, 4],
--  [5, 6]]
reshape :: Array Int -> Array a -> Array a
reshape i a = Array i' (V.take (product i') (V.concat (replicate (1+ product i' `div` V.length (unArray a)) (unArray a))))
  where
    iflat = snd (toFlatList i)
    hasNeg = any (<0) iflat
    i' = bool iflat (fmap (\x -> bool x subDim (x<0)) iflat) hasNeg
    subDim = product (D.shape a) `div` product (filter (>=0) iflat)

-- |
--
-- >>> Huihua.Array.take (fromList1 [2]) (fromFlatList [3,3] [0..8::Int])
-- [[0, 1, 2],
--  [3, 4, 5]]
-- >>> Huihua.Array.take (fromList1 [2,2]) (fromFlatList [3,3] [0..8::Int])
-- [[0, 1],
--  [3, 4]]
--
--  Huihua.Array.take (fromList1 [-2]) (fromFlatList [3,3] [0..8::Int])
-- [[3, 4, 5],
--  [6, 7, 8]]
take :: Array Int -> Array a -> Array a
take i a = takes' i' a
  where
    iflat = snd $ toFlatList i
    i' = bool iflat (iflat <> P.drop (P.length iflat) (D.shape a)) (P.length iflat < P.length (D.shape a))

-- |
--
-- >>> Huihua.Array.drop (fromList1 [2]) (fromFlatList [3,3] [0..8::Int])
-- >>> Huihua.Array.drop (fromList1 [2,2]) (fromFlatList [3,3] [0..8::Int])
-- [[8]]
-- Huihua.Array.drop (fromList1 [-2]) (fromFlatList [3,3] [0..8::Int])
-- [[0, 1, 2]]
-- FIXME: add squeeze here???
drop :: Array Int -> Array a -> Array a
drop i a = drops' i' a
  where
    iflat = snd $ toFlatList i
    i' = bool iflat (iflat <> replicate (P.length (D.shape a) - P.length iflat) 0) (P.length iflat < P.length (D.shape a))
