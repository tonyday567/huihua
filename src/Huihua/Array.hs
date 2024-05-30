{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Arrays with a dynamic shape (shape only known at runtime).
module Huihua.Array
  (
    -- * Mirror of uiua API
    not,
    sign,
    negate,
    absolute,
    sqrt,
    sin,
    floor,
    ceiling,
    round,
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
    couple,
    match,
    pick,
    rotate,
    join,
    select,
    take,
    drop,
    reshape,
    rerankI,
    rerank,
    windows,
    windows',
    keep,
    find,

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
    minimumR,
    maximumR,
    modulusR,
    powerR,
    logarithmR,
  )
where

import NumHask.Array.Dynamic
import Data.Vector qualified as V
import NumHask.Prelude hiding (not, negate, sqrt, sin, floor, ceiling, round, minimum, maximum, length, reverse, fix, take, drop, find, diff)
import NumHask.Prelude qualified as P
import Data.Bits hiding (rotate)
import Data.Ord
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Huihua.Warning
import Data.Bifunctor qualified as B
import Data.List qualified as List

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Huihua.Array as A
-- >>> import NumHask.Array.Dynamic as D

-- * uiua api
not :: (Ring a) => Array a -> Array a
not = fmap (one-)

sign :: (Base a~a, Basis a) => Array a -> Array a
sign = fmap signum

negate :: (Subtractive a) => Array a -> Array a
negate = fmap P.negate

absolute :: (Mag a~a, Basis a) => Array a -> Array a
absolute = fmap P.abs

sqrt :: (ExpField a) => Array a -> Array a
sqrt = fmap P.sqrt

sin :: (TrigField a) => Array a -> Array a
sin = fmap P.sin

floor :: (QuotientField a, Ring (Whole a)) => Array a -> Array (Whole a)
floor = fmap P.floor

ceiling :: (QuotientField a, Ring (Whole a)) => Array a -> Array (Whole a)
ceiling = fmap P.ceiling

round :: (QuotientField a, Eq (Whole a), Ring (Whole a)) => Array a -> Array (Whole a)
round = fmap P.round

sig :: Bool -> Int
sig = bool zero one

binArray :: (a -> b -> c) -> Array a -> Array b -> Either HuihuaWarning (Array c)
binArray op (Array s xs) (Array s' xs') =
  bool (Left SizeMismatch) (Right $ Array s (V.zipWith op xs xs')) (s==s')

add :: (Additive a) => Array a -> Array a -> Either HuihuaWarning (Array a)
add = binArray (+)

subtract :: (Subtractive a) => Array a -> Array a -> Either HuihuaWarning (Array a)
subtract = binArray (-)

multiply :: (Multiplicative a) => Array a -> Array a -> Either HuihuaWarning (Array a)
multiply = binArray (*)

divide :: (Divisive a) => Array a -> Array a -> Either HuihuaWarning (Array a)
divide = binArray (/)

binOpBool :: (a -> a -> Bool) -> Array a -> Array a -> Either HuihuaWarning (Array Int)
binOpBool f = binArray (\x x' -> sig (f x x'))

equals :: (Eq a) => Array a -> Array a -> Either HuihuaWarning (Array Int)
equals = binOpBool (==)

notequals :: (Eq a) => Array a -> Array a -> Either HuihuaWarning (Array Int)
notequals = binOpBool (/=)

lt :: (Ord a) => Array a -> Array a -> Either HuihuaWarning (Array Int)
lt = binOpBool (<)

lte :: (Ord a) => Array a -> Array a -> Either HuihuaWarning (Array Int)
lte = binOpBool (<=)

gt :: (Ord a) => Array a -> Array a -> Either HuihuaWarning (Array Int)
gt = binOpBool (>)

gte :: (Ord a) => Array a -> Array a -> Either HuihuaWarning (Array Int)
gte = binOpBool (>=)

modulus :: (Integral a) => Array a -> Array a -> Either HuihuaWarning (Array a)
modulus = binArray mod

modulusD :: (Subtractive a, QuotientField a, Ring (Whole a), FromIntegral a (Whole a)) => Array a -> Array a -> Either HuihuaWarning (Array a)
modulusD = binArray (\d n -> n - d * fromIntegral (P.floor (n/d)))

power :: (ExpField a) => Array a -> Array a -> Either HuihuaWarning (Array a)
power = binArray (**)

logarithm :: (ExpField a) => Array a -> Array a -> Either HuihuaWarning (Array a)
logarithm = binArray (\x x' -> log x' - log x)

minimum :: (Ord a) => Array a -> Array a -> Either HuihuaWarning (Array a)
minimum = binArray P.min

maximum :: (Ord a) => Array a -> Array a -> Either HuihuaWarning (Array a)
maximum = binArray P.max

arctangent :: (TrigField a) => Array a -> Array a -> Either HuihuaWarning (Array a)
arctangent = binArray atan2

length :: Array a -> Array Int
length (Array [] _) = Array [] (V.singleton one)
length (Array (s:_) _) = Array [] (V.singleton s)

-- |
--
-- >>>  range (toScalar 3)
-- Right [0, 1, 2]
--
-- >>> range (fromList1 [3])
-- Right [[0],
--  [1],
--  [2]]
--
-- >>> range (fromList1 [2,3])
-- Right [[[0, 0],
--   [0, 1],
--   [0, 2]],
--  [[1, 0],
--   [1, 1],
--   [1, 2]]]
range :: Array Int -> Either HuihuaWarning (Array Int)
range a
  | rank a == 0 = Right $ sequent (snd $ toFlatList a)
  | rank a == 1 = Right $ joins (snd $ toFlatList $ sequent (shape a)) $ fmap fromList1 $ indices (snd $ toFlatList a)
  | otherwise = Left RankMismatch

shape' :: Array a -> Array Int
shape' a = Array [P.length xs'] (V.fromList xs')
  where
    xs' = NumHask.Array.Dynamic.shape a

first :: Array a -> Array a
first = selects [0] [0]

reverse :: Array a -> Array a
reverse a = reverses [0] a

-- FIXME: technical, should deshape have to be specified along dimensions?
deshape :: Array a -> Array a
deshape a = reshape' [P.product (shape a)] a

-- | Add another axis to an array (at the end)
fix :: Array a -> Array a
fix (Array i v) = (Array (i <> [1]) v)

bits' :: Int -> Array Int
bits' x =
  [0..] & P.take (finiteBitSize x - countLeadingZeros x) & fmap (\b -> x .&. bit b) & fromList1

bits :: Array Int -> Array Int
bits = fmap bits' >>> joins [0]


-- >>> keep (fromList1 [1, 0, 2, 3, 1]) (fromList1 [8,3, 9, 2, 3::Int])
-- FIXME: fix Scalar version
-- >>> keep (toScalar 4) (fromList1 [1..5])
keep :: Array Int -> Array a -> Either HuihuaWarning (Array a)
keep k a = B.first NumHaskError (fromList1 . fold <$> liftR2 ($) (fmap replicate k) a)

-- >>> where' (fromList1 [1,2,3])
-- Right [0, 1, 1, 2, 2, 2]
where' :: Array Int -> Either HuihuaWarning (Array Int)
where' a = do
  r <- range (length a)
  w <- keep r a
  pure w

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
  concatenate 0 (reshape' (1:shape a) a) (reshape' (1:shape a') a')

-- | ⊡
--
-- > a = (fromFlatList [2,3] [1,2,3,4,5,6]) :: Array Int
-- > pick (range . Huihua.Array.shape' $ a) a == a
-- True
--
-- > pick (fromFlatList [2,2] [0, 1, 1, 2]) a
-- [2, 6]
pick :: Array Int -> Array a -> Array a
pick i a = fmap (\x -> indexA x a) (extracts [0..(P.length (shape i) - 2)] i)
  where
    indexA x y = index y (snd $ toFlatList x)

-- | A.rotate (fromList1 [1]) (A.range (fromList1 [5]))
-- >>> rotate (fromList1 [1,2]) (fromFlatList [4,5] [0..19])
-- [[7, 8, 9, 5, 6],
--  [12, 13, 14, 10, 11],
--  [17, 18, 19, 15, 16],
--  [2, 3, 4, 0, 1]]
rotate :: Array Int -> Array a -> Array a
rotate r a = rotates (zip [0..] (snd $ toFlatList r)) a

-- |
-- >>> join (fromFlatList [2] [1,2]) (fromFlatList [2,2] [5,6,7,8::Int])
-- Right [[1, 2],
--  [5, 6],
--  [7, 8]]
join :: Array a -> Array a -> Either String (Array a)
join a a'
  | P.drop 1 (shape a) == P.drop 1 (shape a') = Right $ concatenate 0 a a'
  | shape a == P.drop 1 (shape a') = Right $ concatenate 0 (reshape' (1:shape a) a) a'
  | P.drop 1 (shape a) == shape a' = Right $ concatenate 0 a (reshape' (1:shape a') a')
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
-- [2, 3, 3, 2]
-- >>> select (fromFlatList [2,2] [0,1,1,0]) (fromFlatList [2,2] [2,3,5,7::Int])
-- [[[2, 3],
--   [5, 7]],
--  [[5, 7],
--   [2, 3]]]
select :: Array Int -> Array a -> Array a
select i a = joins [0..(P.length (shape i) - 1)] $ (\x -> selects [0] [x] a) <$> i

-- |
-- FIXME: Scalar version
--
-- >>> Huihua.Array.reshape (fromList1 [3,2]) (fromList1 [1..5::Int])
-- [[1, 2],
--  [3, 4],
--  [5, 1]]
-- >>> Huihua.Array.reshape (fromList1 [3,-1]) (fromList1 [1..8::Int])
-- [[1, 2],
--  [3, 4],
--  [5, 6]]
reshape :: Array Int -> Array a -> Array a
reshape i a = Array i' (V.take (product i') (V.concat (replicate (1+ product i' `div` V.length (unArray a)) (unArray a))))
  where
    iflat = snd (toFlatList i)
    hasNeg = any (<0) iflat
    i' = bool iflat (fmap (\x -> bool x subDim (x<0)) iflat) hasNeg
    subDim = product (shape a) `div` product (filter (>=0) iflat)

rerankI :: Int -> [Int] -> [Int]
rerankI r xs = xs'
  where
    rold = List.length xs
    r' = bool r (r + rold) (r<0)
    xs' = (List.replicate (r' - rold) one) <> (product (List.take (List.length xs - r') xs) : List.drop (List.length xs - r') xs)

rerank :: Array Int -> Array a -> Either HuihuaWarning (Array a)
rerank r (Array s v)
  | isScalar r = Left TypeMismatch
  | otherwise = Right (Array (rerankI (fromScalar r) s) v)

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
    i' = bool iflat (iflat <> P.drop (P.length iflat) (shape a)) (P.length iflat < P.length (shape a))

-- |
--
-- >>> Huihua.Array.drop (fromList1 [2]) (fromFlatList [3,3] [0..8::Int])
-- [[6, 7, 8]]
--
-- >>> Huihua.Array.drop (fromList1 [2,2]) (fromFlatList [3,3] [0..8::Int])
-- [[8]]
--
-- Huihua.Array.drop (fromList1 [-2]) (fromFlatList [3,3] [0..8::Int])
-- [[0, 1, 2]]
-- FIXME: add squeeze here???
drop :: Array Int -> Array a -> Array a
drop i a = drops i' a
  where
    iflat = snd $ toFlatList i
    i' = bool iflat (iflat <> replicate (P.length (shape a) - P.length iflat) 0) (P.length iflat < P.length (shape a))

--- >>> rise (fromList1 [6,2,7,0,-1,5])
rise :: (Ord a) => Array a -> Array Int
rise a = order $ extracts [0] a

--- >>> rise (fromList1 [6,2,7,0,-1,5])
fall :: (Ord a) => Array a -> Array Int
fall a = orderBy Down $ extracts [0] a

-- |
--
-- maps version of windows
-- >>> x = fromList2 3 [1..9] :: Array Int
-- >>> windows (fromList1 [2,2]) x
-- [[[[1, 2],
--    [4, 5]],
--   [[2, 3],
--    [5, 6]]],
--  [[[4, 5],
--    [7, 8]],
--   [[5, 6],
--    [8, 9]]]]
--
-- FIXME: segfaults on:
-- windows (toScalar 3) (fromList1 [0..3])
-- windows (fromList1 [2]) a
--
--
windows' :: Array Int -> Array a -> Either HuihuaWarning (Array a)
windows' w a = fmap (squeeze . maps (\y -> take w' (drop y a)) [0..(rank a - 1)]) (range (fromList1 s'))
  where
    s' = zipWith (-) (shape a) (replicate (rank a - (rank w + 1)) 0 <> replicate (rank w + 1) 1)
    w' = fromList1 (replicate (rank a - (rank w + 1)) 1 <> (snd $ toFlatList w))

-- |
-- tabulate version
-- TODO:
-- window must be rank 1 or 0
-- negative window sizes
windows :: Array Int -> Array a -> Array a
windows w a = tabulate sh' (index a . fxs)
  where
    sh1 = snd $ toFlatList $ take (fromList1 (shape w)) (fromList1 (shape a))
    sh1' = zipWith (\x y -> x - y + 1) sh1 (snd $ toFlatList w)
    sh2 = snd $ toFlatList w
    sh3 = snd $ toFlatList $ drop (fromList1 (shape w)) (fromList1 (shape a))
    sh' = sh1' <> sh2 <> sh3
    fxs xs = zipWith (+) (List.take (List.length sh1') xs) (List.take (List.length sh2) (List.drop (List.length sh1') xs)) <> List.drop (List.length sh1' + List.length sh2) xs

-- |
--
-- >>> (Right a) = fmap (reshape (fromList1 [4,4])) (range (toScalar 3))
-- >>> i = fromList1 [1,2]
-- >>> find i a
-- Right [[0, 1, 0, 0],
--  [1, 0, 0, 0],
--  [0, 0, 1, 0],
--  [0, 1, 0, 0]]
find :: (Eq a) => Array a -> Array a -> Either HuihuaWarning (Array Int)
find i a = do
    let w = fromList1 (replicate (rank a - rank i) 1 <> shape i)
    ws <- pure $ windows w a
    let xs = contract (sig . (== i)) [rank ws - 1] ws
    pure (reshapeFill (shape a) 0 xs)

-- reducing operators
equalsR :: (Eq a) => Array a -> Array Int
equalsR = reduceBool (==)

notEqualsR :: (Eq a) => Array a -> Array Int
notEqualsR = reduceBool (/=)

lessThanR :: (Ord a) => Array a -> Array Int
lessThanR = reduceBool (<)

lessOrEqualR :: (Ord a) => Array a -> Array Int
lessOrEqualR = reduceBool (<=)

greaterThanR :: (Ord a) => Array a -> Array Int
greaterThanR = reduceBool (>)

greaterOrEqualR :: (Ord a) => Array a -> Array Int
greaterOrEqualR = reduceBool (>=)

reduceBool :: (Ring b) => (a -> a -> Bool) -> Array a -> Array b
reduceBool f a = fmap (bool zero one . any id) (folds (diff 1 f) [0] a)

diff :: Int -> (a -> a -> b) -> Array a -> Array b
diff d f a = liftR2_ f (drops [d] a) (drops [(P.negate d)] a)

foldU :: (a -> b -> b) -> b -> Array a -> Array b
foldU f a0 a = fmap (foldl' (flip f) a0) (extracts [1..(rank a - 1)] a)

addR :: (Additive a) => Array a -> Array a
addR = foldU (+) zero

subtractR :: (Subtractive a) => Array a -> Array a
subtractR = foldU (-) zero

divideR :: (Divisive a) => Array a -> Array a
divideR = foldU (/) one

multiplyR :: (Multiplicative a) => Array a -> Array a
multiplyR = foldU (*) one

-- |
minimumR :: (Ord a, BoundedMeetSemiLattice a) => Array a -> Array a
minimumR = foldU min top

maximumR :: (Ord a, BoundedJoinSemiLattice a) => Array a -> Array a
maximumR = foldU max bottom

modulusR :: (Integral a) => Array a -> Array a
modulusR = foldU mod one

powerR :: (ExpField a, Multiplicative a) => Array a -> Array a
powerR = foldU (**) one

logarithmR :: (ExpField a) => Array a -> Array a
logarithmR = foldU logBase one
