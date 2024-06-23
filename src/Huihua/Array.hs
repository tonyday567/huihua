{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | uiua API over numhask-array
module Huihua.Array
  (
    dyadicPervasive,
    reduceU,
    reduce1U,

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
    lessThan,
    lessOrEqual,
    greaterThan,
    greaterOrEqual,
    modulus,
    modD,
    power,
    logarithm,
    minimum,
    maximum,
    atangent,
    length,
    shape,
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
-- >>> import Prettyprinter

-- | Dyadic pervasive
dyadicPervasive :: (b -> a -> c) -> Array a -> Array b -> Either HuihuaWarning (Array c)
dyadicPervasive op a b
  | (D.shape a) `List.isPrefixOf` (D.shape b) = Right $ (D.transmit (D.zipWithE (flip op)) a b)
  | (D.shape b) `List.isPrefixOf` (D.shape a) = Right $ (D.transmit (D.zipWithE op) b a)
  | otherwise = Left SizeMismatch

-- | Apply a binary boolean function, right-to-left
dyadicPervasiveBool :: (b -> a -> Bool) -> Array a -> Array b -> Either HuihuaWarning (Array Int)
dyadicPervasiveBool f = dyadicPervasive (\x x' -> sig (f x x'))

-- | https://www.uiua.org/docs/reduce
--
reduceU :: (a -> b -> b) -> b -> Array a -> Array b
reduceU f a0 a = D.reduces [0] (foldl' (flip f) a0) a

-- | Version for no identity functions
--
reduce1U :: (a -> a -> a) -> Array a -> Either HuihuaWarning (Array a)
reduce1U f a
  | null a = Left NoIdentity
  | D.length a == 1 = Right (D.selects [(0,0)] a)
  | otherwise =
      let (x D.:| xs) = a in
      Right (D.zipWithE (foldl' (flip f)) x (D.extractsExcept [0] xs))



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

add :: (Additive a) => Array a -> Array a -> Either HuihuaWarning (Array a)
add = dyadicPervasive (+)

subtract :: (Subtractive a) => Array a -> Array a -> Either HuihuaWarning (Array a)
subtract = dyadicPervasive (-)

multiply :: (Multiplicative a) => Array a -> Array a -> Either HuihuaWarning (Array a)
multiply = dyadicPervasive (*)

divide :: (Divisive a) => Array a -> Array a -> Either HuihuaWarning (Array a)
divide = dyadicPervasive (/)

equals :: (Eq a) => Array a -> Array a -> Either HuihuaWarning (Array Int)
equals = dyadicPervasiveBool (==)

notequals :: (Eq a) => Array a -> Array a -> Either HuihuaWarning (Array Int)
notequals = dyadicPervasiveBool (/=)

lessThan :: (Ord a) => Array a -> Array a -> Either HuihuaWarning (Array Int)
lessThan = dyadicPervasiveBool (<)

lessOrEqual :: (Ord a) => Array a -> Array a -> Either HuihuaWarning (Array Int)
lessOrEqual = dyadicPervasiveBool (<=)

greaterThan :: (Ord a) => Array a -> Array a -> Either HuihuaWarning (Array Int)
greaterThan = dyadicPervasiveBool (>)

greaterOrEqual :: (Ord a) => Array a -> Array a -> Either HuihuaWarning (Array Int)
greaterOrEqual = dyadicPervasiveBool (>=)

modulus :: Array Double -> Array Double -> Either HuihuaWarning (Array Double)
modulus = dyadicPervasive modD

-- >>> modD 7 5
modD :: Double -> Double -> Double
modD n d
  | d == infinity = n
  | d == 0 = nan
  | otherwise = n - d * fromIntegral (P.floor (n/d))

power :: (ExpField a) => Array a -> Array a -> Either HuihuaWarning (Array a)
power = dyadicPervasive (**)

logarithm :: (ExpField a) => Array a -> Array a -> Either HuihuaWarning (Array a)
logarithm = dyadicPervasive (\x x' -> log x / log x')

minimum :: (Ord a) => Array a -> Array a -> Either HuihuaWarning (Array a)
minimum = dyadicPervasive P.min

maximum :: (Ord a) => Array a -> Array a -> Either HuihuaWarning (Array a)
maximum = dyadicPervasive P.max

atangent :: (TrigField a) => Array a -> Array a -> Either HuihuaWarning (Array a)
atangent = dyadicPervasive (flip atan2)

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

transpose :: Array a -> Array a
transpose = D.transpose

-- >>> keep (fromList1 [1, 0, 2, 3, 1]) (fromList1 [8,3, 9, 2, 3::Int])
-- FIXME: fix Scalar version
-- >>> keep (toScalar 4) (fromList1 [1..5])
keep :: Array Int -> Array a -> Array a
keep _ _ = undefined -- D.join $ fmap D.asArray $ D.reduces [0] fold $ D.zipWith (\k' a' -> replicate k' a') k a

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
-- >>> pretty $ couple (D.asArray [1,2,3]) (D.asArray [4,5,6::Int])
-- [[1,2,3],
--  [4,5,6]]
couple :: Array a -> Array a -> Array a
couple a a' =
  D.concatenate 0 (D.reshape (1:D.shape a) a) (D.reshape (1:D.shape a') a')

-- | ⊡
--
-- > a = (array [2,3] [1,2,3,4,5,6]) :: Array Int
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
-- >>> pretty $ rotate (asArray [1,2]) (array [4,5] [0..19])
-- [[7,8,9,5,6],
--  [12,13,14,10,11],
--  [17,18,19,15,16],
--  [2,3,4,0,1]]
rotate :: Array Int -> Array a -> Array a
rotate r a = D.rotates (P.zip [0..] (D.arrayAs r)) a

-- |
-- >>> A.join (array [2] [1,2]) (array [2,2] [5,6,7,8::Int])
-- Right (UnsafeArray [3,2] [1,2,5,6,7,8])
join :: Array a -> Array a -> Either String (Array a)
join a a'
  | P.drop 1 (D.shape a) == P.drop 1 (D.shape a') = Right $ D.concatenate 0 a a'
  | D.shape a == P.drop 1 (D.shape a') = Right $ D.concatenate 0 (D.reshape (1:D.shape a) a) a'
  | P.drop 1 (D.shape a) == D.shape a' = Right $ D.concatenate 0 a (D.reshape (1:D.shape a') a')
  | otherwise = Left "Shape Mismatch"
-- (\x -> selects [0] [x] (array [2,2] [5,6,7,8::Int])) <$> [1,0]
-- [[7, 8],[5, 6]]
-- (\x -> selects [0] [x] (array [4] [5,6,7,8::Int])) <$> [1,0]
--  joins [0] $ (\x -> selects [0] [x] (array [4] [2,3,5,7::Int])) <$> (array [3,2] [0,1,1,2,2,3])
-- joins [0] $ (\x -> selects [0] [x] (array [3,3] [1..9::Int])) <$> (array [4] [0,2,1,1])
-- a2 = joins [0,1] ((\x -> selects [0] [x] (array [2,2] [2,3,5,7::Int])) <$> (array [2,2] [0,1,1,0]))
 -- a1 = joins [0] $ (\x -> selects [0] [x] (array [4] [2,3,5,7::Int])) <$> (array [4] [0,1,1,0])

-- |
-- >>> pretty $ select (array [4] [0,1,1,0]) (array [4] [2,3,5,7::Int])
-- [2,3,3,2]
-- >>> pretty $ select (array [2,2] [0,1,1,0]) (array [2,2] [2,3,5,7::Int])
-- [[[2,3],
--   [5,7]],
--  [[5,7],
--   [2,3]]]
select :: Array Int -> Array a -> Array a
select i a = D.joins [0..(D.rank i) - 1] $ (\x -> D.selects [(0,x)] a) <$> i

-- |
-- FIXME: Scalar version
--
-- >>> pretty $ Huihua.Array.reshape (D.asArray [3,2]) (D.asArray [1..5::Int])
-- [[1,2],
--  [3,4],
--  [5,1]]
-- >>> pretty $ Huihua.Array.reshape (D.asArray [3,-1]) (D.asArray [1..8::Int])
-- [[1,2],
--  [3,4],
--  [5,6]]
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
-- >>> pretty $ Huihua.Array.take (D.asArray [2]) (array [3,3] [0..8::Int])
-- [[0,1,2],
--  [3,4,5]]
-- >>> pretty $ Huihua.Array.take (D.asArray [2,2]) (array [3,3] [0..8::Int])
-- [[0,1],
--  [3,4]]
--
--  Huihua.Array.take (D.asArray [-2]) (array [3,3] [0..8::Int])
-- [[3, 4, 5],
--  [6, 7, 8]]
take :: Array Int -> Array a -> Array a
take i a = D.takes (zip [0..] i') a
  where
    iflat = D.arrayAs i
    i' = bool iflat (iflat <> P.drop (P.length iflat) (D.shape a)) (P.length iflat < (D.rank a))

-- |
--
-- >>> pretty $ Huihua.Array.drop (D.asArray [2]) (array [3,3] [0..8::Int])
-- [[6,7,8]]
--
-- >>> pretty $ Huihua.Array.drop (D.asArray [2,2]) (array [3,3] [0..8::Int])
-- [[8]]
--
-- pretty $ Huihua.Array.drop (D.asArray [-2]) (array [3,3] [0..8::Int])
-- [[0,1,2]]
-- FIXME: add squeeze here???
drop :: Array Int -> Array a -> Array a
drop i a = D.drops (zip [0..] i') a
  where
    iflat = D.arrayAs i
    i' = bool iflat (iflat <> replicate ((D.rank a) - P.length iflat) 0) (P.length iflat < (D.rank a))

--- >>> rise (D.asArray [6,2,7,0,-1,5])
rise :: (Ord a) => Array a -> Array Int
rise a = D.orders [0] $ D.extracts [0] a

--- >>> rise (D.asArray [6,2,7,0,-1,5])
fall :: (Ord a) => Array a -> Array Int
fall a = D.ordersBy [0] (fmap Down) $ D.extracts [0] a

-- |
--
-- maps version of windows
-- >>> x = array [3,3] [1..9] :: Array Int
-- >>> pretty $ windows (D.asArray [2,2]) x
-- [[[[1,2],
--    [4,5]],
--   [[2,3],
--    [5,6]]],
--  [[[4,5],
--    [7,8]],
--   [[5,6],
--    [8,9]]]]
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
-- > a = fmap (A.reshape (D.asArray [4,4])) (A.range (toScalar 3))
-- > i = D.asArray [1,2]
-- > find i a
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

-- * reducing operators
reduceBool :: (Ring b) => (a -> a -> Bool) -> Array a -> Array b
reduceBool f a = fmap (bool zero one . any id) (D.reduces [0] (D.diffE 1 f) a)

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

addR :: (Additive a) => Array a -> Array a
addR = reduceU (+) zero

subtractR :: (Subtractive a) => Array a -> Array a
subtractR = reduceU (-) zero

divideR :: (Divisive a) => Array a -> Array a
divideR = reduceU (/) one

multiplyR :: (Multiplicative a) => Array a -> Array a
multiplyR = reduceU (*) one

minimumR :: (Ord a, BoundedMeetSemiLattice a) => Array a -> Array a
minimumR = reduceU min top

maximumR :: (Ord a, BoundedJoinSemiLattice a) => Array a -> Array a
maximumR = reduceU max bottom

modulusR :: Array Double -> Array Double
modulusR = reduceU modD infinity

powerR :: (ExpField a, Multiplicative a) => Array a -> Array a
powerR = reduceU (**) one

logarithmR :: (ExpField a) => Array a -> Either HuihuaWarning (Array a)
logarithmR = reduce1U (flip logBase)
