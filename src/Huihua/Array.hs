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
    shape,
    range,
    first,
    reverse,
    deshape,
    fix,
    bits,
    -- transpose,
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

import NumHask.Array.Dynamic (Array(..))
import NumHask.Array.Dynamic qualified as D
import Data.Vector qualified as V
import NumHask.Prelude hiding (not, negate, sqrt, sin, floor, ceiling, round, minimum, maximum, length, reverse, fix, take, drop, find, diff)
import NumHask.Prelude qualified as P
import Data.Bits hiding (rotate)
import Data.Ord
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Huihua.Warning
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
binArray op (UnsafeArray s xs) (UnsafeArray s' xs') =
  bool (Left SizeMismatch) (Right $ D.array s (V.zipWith op xs xs')) (s==s')

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
length = D.toScalar . D.length

-- |
--
-- TODO: negative numbers
range :: Array Int -> Array Int
range = D.range

shape :: Array a -> Array Int
shape = D.shape

first :: Array a -> Array a
first = D.row 0

reverse :: Array a -> Array a
reverse a = D.reverses [0] a

-- FIXME: technical, should deshape have to be specified along dimensions?
deshape :: Array a -> Array a
deshape a = D.reshape [V.product (D.shape a)] a

-- | Add another axis to an array (at the end)
fix :: Array a -> Array a
fix (UnsafeArray i v) = D.array (i <> V.singleton 1) v

bits' :: Int -> Array Int
bits' x =
  [0..] & P.take (finiteBitSize x - countLeadingZeros x) & fmap (\b -> x .&. bit b) &  D.asVector & D.vectorAs

bits :: Array Int -> Array Int
bits = fmap bits' >>> D.joins [0]


-- >>> keep (fromList1 [1, 0, 2, 3, 1]) (fromList1 [8,3, 9, 2, 3::Int])
-- FIXME: fix Scalar version
-- >>> keep (toScalar 4) (fromList1 [1..5])
keep :: Array Int -> Array a -> Array a
keep k a = D.join $ fmap D.asArray $ D.folds [0] fold $ D.zipWith (\k' a' -> replicate k' a') k a

-- >>> where' (fromList1 [1,2,3])
-- Right [0, 1, 1, 2, 2, 2]
where' :: Array Int -> Array Int
where' a = keep (D.range (length a)) a

classify :: (Ord a) => Array a -> Array Int
classify a = D.asArray $ fst <$> mscan (toList $ D.extracts [0] a)
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
deduplicate a = D.joins [0] $ D.asArray $ mapMaybe fst (dscan (toList $ D.extracts [0] a))
  where
    dscan [] = []
    dscan (x0:xs) =
      scanl
      (\(_, set) k ->
         bool (Just k, Set.insert k set) (Nothing, set)
         (Set.member k set)) (Just x0,Set.singleton x0)
      xs

match :: (Eq a) => Array a -> Array a -> Array Int
match a a' = D.asArray [bool 0 1 (a==a')]

-- |
-- >>> couple (D.asArray [1,2,3]) (D.asArray [4,5,6::Int])
-- [[1, 2, 3],
--  [4, 5, 6]]
couple :: Array a -> Array a -> Array a
couple a a' =
  D.concatenate 0 (D.reshape (1:D.shape a) a) (D.reshape (1:D.shape a') a')

-- | ⊡
--
-- > a = (fromFlatList [2,3] [1,2,3,4,5,6]) :: Array Int
-- > pick (range . Huihua.Array.shape' $ a) a == a
-- True
--
-- > pick (fromFlatList [2,2] [0, 1, 1, 2]) a
-- [2, 6]
pick :: Array Int -> Array a -> Array a
pick i a = fmap (\x -> indexA x a) (D.extracts [0..(D.rank i) - 2] i)
  where
    indexA x y = D.index y x

-- | A.rotate (D.asArray [1]) (A.range (D.asArray [5]))
-- >>> rotate (D.asArray [1,2]) (fromFlatList [4,5] [0..19])
-- [[7, 8, 9, 5, 6],
--  [12, 13, 14, 10, 11],
--  [17, 18, 19, 15, 16],
--  [2, 3, 4, 0, 1]]
rotate :: Array Int -> Array a -> Array a
rotate r a = D.rotates (P.zip [0..] (D.arrayAs r)) a

-- |
-- >>> join (fromFlatList [2] [1,2]) (fromFlatList [2,2] [5,6,7,8::Int])
-- Right [[1, 2],
--  [5, 6],
--  [7, 8]]
join :: Array a -> Array a -> Either String (Array a)
join a a'
  | P.drop 1 (D.shape a) == P.drop 1 (D.shape a') = Right $ D.concatenate 0 a a'
  | D.shape a == P.drop 1 (D.shape a') = Right $ D.concatenate 0 (D.reshape (1:D.shape a) a) a'
  | P.drop 1 (D.shape a) == D.shape a' = Right $ D.concatenate 0 a (D.reshape (1:D.shape a') a')
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
select i a = D.joins [0..(D.rank i) - 1] $ (\x -> D.selects [(0,x)] a) <$> i

-- |
-- FIXME: Scalar version
--
-- >>> Huihua.Array.reshape (D.asArray [3,2]) (D.asArray [1..5::Int])
-- [[1, 2],
--  [3, 4],
--  [5, 1]]
-- >>> Huihua.Array.reshape (D.asArray [3,-1]) (D.asArray [1..8::Int])
-- [[1, 2],
--  [3, 4],
--  [5, 6]]
reshape :: Array Int -> Array a -> Array a
reshape i a = D.array i' (V.take (product i') (V.concat (replicate (1+ product i' `div` V.length (D.asVector a)) (D.asVector a))))
  where
    iflat = D.arrayAs i
    hasNeg = any (<0) iflat
    i' = bool iflat (fmap (\x -> bool x subDim (x<0)) iflat) hasNeg
    subDim = (D.size a) `div` product (filter (>=0) iflat)

rerankI :: Int -> [Int] -> [Int]
rerankI r xs = xs'
  where
    rold = List.length xs
    r' = bool r (r + rold) (r<0)
    xs' = (List.replicate (r' - rold) one) <> (product (List.take (List.length xs - r') xs) : List.drop (List.length xs - r') xs)

rerank :: Array Int -> Array a -> Either HuihuaWarning (Array a)
rerank r a
  | D.isScalar r = Left TypeMismatch
  | otherwise = Right (D.array (rerankI (D.fromScalar r) (D.shape a)) (D.asVector a))

-- |
--
-- >>> Huihua.Array.take (D.asArray [2]) (fromFlatList [3,3] [0..8::Int])
-- [[0, 1, 2],
--  [3, 4, 5]]
-- >>> Huihua.Array.take (D.asArray [2,2]) (fromFlatList [3,3] [0..8::Int])
-- [[0, 1],
--  [3, 4]]
--
--  Huihua.Array.take (D.asArray [-2]) (fromFlatList [3,3] [0..8::Int])
-- [[3, 4, 5],
--  [6, 7, 8]]
take :: Array Int -> Array a -> Array a
take i a = D.takes (zip [0..] i') a
  where
    iflat = D.arrayAs i
    i' = bool iflat (iflat <> P.drop (P.length iflat) (D.shape a)) (P.length iflat < (D.rank a))

-- |
--
-- >>> Huihua.Array.drop (D.asArray [2]) (fromFlatList [3,3] [0..8::Int])
-- [[6, 7, 8]]
--
-- >>> Huihua.Array.drop (D.asArray [2,2]) (fromFlatList [3,3] [0..8::Int])
-- [[8]]
--
-- Huihua.Array.drop (D.asArray [-2]) (fromFlatList [3,3] [0..8::Int])
-- [[0, 1, 2]]
-- FIXME: add squeeze here???
drop :: Array Int -> Array a -> Array a
drop i a = D.drops (zip [0..] i') a
  where
    iflat = D.arrayAs i
    i' = bool iflat (iflat <> replicate ((D.rank a) - P.length iflat) 0) (P.length iflat < (D.rank a))

--- >>> rise (D.asArray [6,2,7,0,-1,5])
rise :: (Ord a) => Array a -> Array Int
rise a = D.order $ D.extracts [0] a

--- >>> rise (D.asArray [6,2,7,0,-1,5])
fall :: (Ord a) => Array a -> Array Int
fall a = D.orderBy Down $ D.extracts [0] a

-- |
--
-- maps version of windows
-- >>> x = fromList2 3 [1..9] :: Array Int
-- >>> windows (D.asArray [2,2]) x
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
-- windows (toScalar 3) (D.asArray [0..3])
-- windows (D.asArray [2]) a
--
--
{-
windows' :: Array Int -> Array a -> Array a
windows' w a = fmap (D.squeeze . D.maps (\y -> D.take w' (D.drop y a)) [0..(D.rank a - 1)]) (D.range (D.asArray s'))
  where
    s' = P.zipWith (-) (D.shape a) (replicate (D.rank a - (D.rank w + 1)) 0 <> replicate (D.rank w + 1) 1)
    w' = D.asArray (replicate (D.rank a - (D.rank w + 1)) 1 <> (snd $ D.arrayAs w))

-}

-- |
-- tabulate version
-- TODO:
-- window must be rank 1 or 0
-- negative window sizes
windows :: Array Int -> Array a -> Array a
windows w a = D.tabulate sh' (D.index a . fxs)
  where
    sh1 = D.arrayAs $ D.takes (zip [0..] (D.shape w)) (D.shape a)
    sh1' = P.zipWith (\x y -> x - y + 1) sh1 (D.arrayAs w)
    sh2 = D.arrayAs w
    sh3 = D.arrayAs $ D.drops (zip [0..] (D.shape w)) (D.shape a)
    sh' = sh1' <> sh2 <> sh3
    fxs xs = P.zipWith (+) (List.take (List.length sh1') xs) (List.take (List.length sh2) (List.drop (List.length sh1') xs)) <> List.drop (List.length sh1' + List.length sh2) xs

-- |
--
-- >>> (Right a) = fmap (reshape (D.asArray [4,4])) (range (toScalar 3))
-- >>> i = D.asArray [1,2]
-- >>> find i a
-- Right [[0, 1, 0, 0],
--  [1, 0, 0, 0],
--  [0, 0, 1, 0],
--  [0, 1, 0, 0]]
find :: (Eq a) => Array a -> Array a -> Array Int
find i a = D.reshapeDef 0 (D.shape a :: [Int]) xs
  where
    w = D.asArray (replicate (D.rank a - D.rank i) 1 <> D.shape i)
    ws = windows w a
    xs = D.contract (sig . (== i)) [D.rank ws - 1] ws

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
reduceBool f a = fmap (bool zero one . any id) (D.folds [0] (diff 1 f) a)

diff :: Int -> (a -> a -> b) -> Array a -> Array b
diff d f a = D.zipWithE f (D.drops [(0,d)] a) (D.drops [(0,P.negate d)] a)

foldU :: (a -> b -> b) -> b -> Array a -> Array b
foldU f a0 a = fmap (foldl' (flip f) a0) (D.extracts [1..(D.rank a - 1)] a)

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
