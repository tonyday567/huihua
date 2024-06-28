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
    negate',
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
    rerank,
    windows,
    keep,
    find,
    mask,
    maskFind,

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
import NumHask.Prelude hiding (not, sqrt, sin, floor, ceiling, round, minimum, maximum, length, reverse, fix, take, drop, find, diff)
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

negate' :: (Subtractive a) => Array a -> Array a
negate' = fmap P.negate

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

keep :: Array Int -> Array a -> Array a
keep i a = D.join $ D.asArray $ fold $ D.zipWithE replicate (D.cycle (List.take 1 $ D.shape a) i) (D.extracts [0] a)

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
match a a' = D.toScalar (bool 0 1 (a==a'))

-- | ⊟ 1 2
-- >>> pretty $ couple (D.asArray [1,2,3]) (D.asArray [4,5,6::Int])
-- [[1,2,3],
--  [4,5,6]]
couple :: Array a -> Array a -> Array a
couple a a' =
  D.concatenate 0 (D.reshape (1:D.shape a) a) (D.reshape (1:D.shape a') a')

-- | ⊡
--
pick :: Array Int -> Array a -> Either HuihuaWarning (Array a)
pick i a
  | D.length i > D.rank a = Left BadPick
  | otherwise = Right $ D.join $ fmap (\s -> D.selects (List.zip [0..] (D.arrayAs s)) a) (D.extracts [0..(D.rank i) - 2] i)

rotate :: Array Int -> Array a -> Array a
rotate r a = D.rotates (P.zip [0..] (D.arrayAs r)) a

-- | https://www.uiua.org/docs/join
join :: Array a -> Array a -> Either String (Array a)
join a a'
  | P.drop 1 (D.shape a) == P.drop 1 (D.shape a') = Right $ D.concatenate 0 a a'
  | D.shape a == P.drop 1 (D.shape a') = Right $ D.prepend 0 a a'
  | P.drop 1 (D.shape a) == D.shape a' = Right $ D.append 0 a a'
  | (D.shape a) `List.isSuffixOf` (D.shape a') = Right $ D.prepend 0 (D.repeat (List.drop 1 $ D.shape a') a) a'
  | (D.shape a') `List.isSuffixOf` (D.shape a) = Right $ D.append 0 a (D.repeat (List.drop 1 $ D.shape a) a')
  | otherwise =  Left "Shape Mismatch"

-- | Select multiple rows from an array
select :: Array Int -> Array a -> Array a
select i a = D.joins [0..(D.rank i) - 1] $ (\x -> D.selects [(0,x)] a) <$> i

-- |
--
-- >>> pretty $ Huihua.Array.reshape (D.asArray [3,2]) (D.asArray [1..5::Int])
-- [[1,2],
--  [3,4],
--  [5,1]]
-- >>> pretty $ Huihua.Array.reshape (D.asArray [3,-1]) (D.asArray [1..8::Int])
-- [[1,2],
--  [3,4],
--  [5,6]]
--
-- FIXME: bless this mess
reshape :: Array Int -> Array a -> Array a
reshape i a
  | D.rank i == 0 = D.repeat ((D.fromScalar i):(D.shape a)) a
  | otherwise = D.array i' (V.take (product i') (V.concat (replicate (1+ product i' `div` V.length (D.asVector a)) (D.asVector a))))
  where
    iflat = D.arrayAs i
    hasNeg = any (<0) iflat
    i' = bool iflat (fmap (\x -> bool x subDim (x<0)) iflat) hasNeg
    subDim = (D.size a) `div` product (filter (>=0) iflat)

rerank :: Array Int -> Array a -> Either HuihuaWarning (Array a)
rerank r a
  | P.not (D.isScalar r) = Left TypeMismatch
  | otherwise = Right (D.rerank r' a)
    where
      x = D.fromScalar r
      r' = bool (x+1) (x + 1 + D.rank a) (x<0)

take :: Array Int -> Array a -> Either HuihuaWarning (Array a)
take i a
  | D.rank i > 1 || D.length i > D.rank a = Left BadTake
  | otherwise = Right $ D.takes (zip [0..] (D.arrayAs i)) a

drop :: Array Int -> Array a -> Either HuihuaWarning (Array a)
drop i a
  | D.rank i > 1 || D.length i > D.rank a = Left BadTake
  | otherwise = Right $ D.drops (zip [0..] (D.arrayAs i)) a

--- >>> rise (D.asArray [6,2,7,0,-1,5])
rise :: (Ord a) => Array a -> Array Int
rise a = D.orders [0] $ D.extracts [0] a

--- >>> rise (D.asArray [6,2,7,0,-1,5])
fall :: (Ord a) => Array a -> Array Int
fall a = D.ordersBy [0] (fmap Down) $ D.extracts [0] a

-- |
windows :: Array Int -> Array a -> Array a
windows ws a = D.windows ws' a
  where
    ws' = List.zipWith (\w s -> bool w (s - w + 1) (w<0)) (D.arrayAs ws) (D.shape a)

find :: (Eq a) => Array a -> Array a -> Array Int
find i a = D.pad 0 (D.shape a :: [Int]) (fmap sig $ D.find i a)

mask :: (Eq a) => Array a -> Array a -> Array Int
mask i a = m
  where
    iexp = D.rerank (D.rank a) i
    found = fmap sig $ D.findNoOverlap iexp a
    accf = V.drop 1 . V.map snd . V.scanl' (\(acc,_) x -> bool (acc+1,acc+1) (acc,0) (x==0)) (0,0)
    found' = D.unsafeModifyVector accf found
    found'' = D.pad 0 (D.shape @[Int] a) found'
    -- sh = List.zipWith (\i' a' -> a' - i' + 1) (D.shape iexp <> replicate (D.rank a - D.rank iexp) one) (D.shape a)
    start = (\s -> 1 - s) <$> D.shape iexp
    backchecks s = List.zip (List.zipWith (\s0 s' -> (max zero (s0+s'))) start s) (List.zipWith (\i' s' -> min i' (s'+1)) (D.shape iexp) s)
    m = D.tabulate (D.shape a) (\s -> sum (D.sliceR (backchecks s) found''))



-- | find the matching sub-array positions but only if they dont overlap with previous matches.  previous doing a lot of work here.
maskFind :: (Eq a) => Array a -> Array a -> Array Int
maskFind i a = r
  where
    start = (one -) <$> D.shape i
    r = D.tabulate (D.shape @[Int] a) go
    go s = (sig ((D.index l s == 1) && all (==0) (V.init (D.asVector (D.sliceR (backchecks s) r) ))))
    l = D.rotate start  (find i a)
    backchecks s = zip (List.zipWith (\s0 s' -> max zero (s0+s')) start s) (D.shape i)

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
