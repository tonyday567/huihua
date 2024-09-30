{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Compatability layer between Harry.Dynamic.Array and uiua arrays.
--
-- The main purpose is to change types from Double to Int and back as necessary. (all uiua arrays are doubles).
module Huihua.ArrayU
  ( -- $usage
    ArrayU (..),
    arrayi,
    arrayi',
    arrayi_,
    Res,
    isInt,
    asInt,
    showU,

    -- * uiua API
    duplicate,
    pop,
    identity,
    not,
    sign,
    negate',
    absoluteValue,
    sqrt,
    sine,
    floor,
    ceiling,
    round,
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
    unique,

    -- * dyadics
    equals,
    notequals,
    lessThan,
    lessOrEqual,
    greaterThan,
    greaterOrEqual,
    add,
    subtract,
    multiply,
    divide,
    modulus,
    power,
    logarithm,
    minimum,
    maximum,
    atangent,

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
    mask,
    member,
    indexOf,

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

    -- * testing only
    ptree,
    unfoldF,
)
where

import Huihua.Array qualified as A
import Harry.Dynamic (Array (..))
import Harry.Dynamic qualified as D
import Prelude hiding (not, sqrt, sin, floor, ceiling, round, minimum, maximum, length, reverse, take, drop, subtract)
import Prelude qualified as P
import Huihua.Warning
import Prettyprinter hiding (equals)
import Data.List qualified as List
import Data.Tree qualified as Tree
import Data.Either
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import GHC.Generics
import Data.Bool (bool)
import Data.Function ((&))
import Data.Maybe

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XQuasiQuotes
-- >>> import Data.String.Interpolate
-- >>> import Huihua.Array as A
-- >>> import Harry.Dynamic as D
-- >>> import Prettyprinter
-- >>> import Huihua.Parse

-- | A uiua array which is always an Array Double underneath.
--
-- >>> x = D.array [2,2] [2, 2.222,200.001,200.01] :: Array Double
-- >>> pretty (ArrayU x)
-- ╭─
-- ╷       2  2.222
--   200.001 200.01
--                  ╯
newtype ArrayU = ArrayU { arrayd :: Array Double } deriving (Eq, Show, Generic)

instance Pretty ArrayU where
  pretty (ArrayU x) = case D.rank x of
    0 -> pretty $ showU (D.fromScalar x)
    1 -> (pretty "[") <> hsep (fmap (pretty . showU) (D.arrayAs x)) <> (pretty "]")
    _ -> final
    where
    t = ptree (fmap showU x)
    maxp = D.reduces [1] (P.maximum . fmap Text.length) (D.join $ D.asArray $ rights t)
    s = fmap (fmap ((D.zipWith (\m a -> lpad' ' ' m a) maxp))) t
    sdoc = mconcat $ fmap (either (\n -> replicate (n-1) mempty) (pure . hsep . fmap pretty . D.arrayAs)) s
    sdocMin = D.concatenate 0 (D.konst [max 0 (D.rank x - P.length sdoc - 1)] mempty) (D.asArray sdoc)
    rankPrefix = fmap pretty (D.pad " " [D.length sdocMin] (D.konst [D.rank x - 1] "╷"))
    deco = zipWith (<+>) (D.arrayAs rankPrefix) (D.arrayAs sdocMin)
    final = (pretty "╭─") <> line <> (vsep deco) <> hang 1 (line <> pretty "╯")

showU :: Double -> Text
showU x = bool mempty (pack "¯") (x < 0) <> bool (pack $ show (abs x)) (pack $ show (asInt (abs x))) (isInt x)

lpad' :: Char -> Int -> Text -> Text
lpad' c maxl x = (pack $ replicate (maxl - P.length (unpack x)) c) <> x

unfoldF :: Either Int (Array a) -> (Maybe (Either Int (Array a)), [Either Int (Array a)])
unfoldF (Left n) = (Just (Left n), [])
unfoldF (Right a) = case D.rank a of
  1 -> (Just (Right a), [])
  _ -> (Nothing, List.intersperse (Left (D.rank a - 1)) (Right <$> D.arrayAs (D.extracts [0] a)))

ptree :: Array a -> [Either Int (Array a)]
ptree a = catMaybes $ Tree.flatten $ Tree.unfoldTree unfoldF (Right a)

arrayi :: ArrayU -> Array Int
arrayi (ArrayU a) = fmap asInt a

arrayi' :: ArrayU -> Either HuihuaWarning (Array Int)
arrayi' (ArrayU a) =
  bool
  (Left NotNat)
  (Right (fmap asInt a))
  (all isInt a)

isInt :: Double -> Bool
isInt x = x == fromIntegral (P.floor x :: Int)

asInt :: Double -> Int
asInt x = P.floor x

arrayi_ :: ArrayU -> Array Int
arrayi_ a = either (error . show) id (arrayi' a)

type Res = Either HuihuaWarning [ArrayU]

-- | .
--
-- >>> run ". [1 2 3]"
-- [1 2 3]
-- [1 2 3]
duplicate :: ArrayU -> Res
duplicate x = Right [x,x]

-- | ◌
--
-- >>> run [i|◌1 2|]
-- 2
pop :: ArrayU -> Res
pop _ = Right []

-- | ∘
--
-- >>> run [i|∘ 5|]
-- 5
identity :: ArrayU -> Res
identity x = Right [x]

-- | ¬
--
-- >>> run [i|¬0|]
-- 1
-- >>> run [i|¬[0 1 2 3]|]
-- [1 0 ¯1 ¯2]
not :: ArrayU -> Res
not (ArrayU a) = Right . pure . ArrayU . A.not $ a

-- | ±
--
-- >>> run [i|± 1|]
-- 1
sign :: ArrayU -> Res
sign (ArrayU a) = Right . pure . ArrayU . A.sign $ a

-- | ¯
--
-- negate is just about a reserved word in haskell eg -2 is parsed as negate 2.
--
-- >>> run [i|¯ 1|]
-- ¯1
negate' :: ArrayU -> Res
negate' (ArrayU a) = Right . pure . ArrayU . A.negate' $ a

-- | ⌵
--
-- >>> run [i|⌵ ¯1|]
-- 1
absoluteValue :: ArrayU -> Res
absoluteValue (ArrayU a) = Right . pure . ArrayU . A.absolute $ a


-- | √
--
-- >>> run [i|√4|]
-- 2
sqrt :: ArrayU -> Res
sqrt (ArrayU a) = Right . pure . ArrayU . A.sqrt $ a

-- | ∿
--
-- >>> run [i|∿ 1|]
-- 0.8414709848078965
sine :: ArrayU -> Res
sine (ArrayU a) = Right . pure . ArrayU . A.sin $ a

-- | ⌊
--
-- >>> run [i|⌊1.5|]
-- 1
floor :: ArrayU -> Res
floor (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.floor $ a

-- | ⌈
--
-- >>> run [i|⌈1.5|]
-- 2
ceiling :: ArrayU -> Res
ceiling (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.ceiling $ a

-- | ⁅
--
-- >>> run [i|⁅1.5|]
-- 2
round :: ArrayU -> Res
round (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.round $ a

-- | ⧻
--
-- >>> run [i|⧻[1 3 5]|]
-- 3
length :: ArrayU -> Res
length (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.length $ a


-- | △
--
-- >>> run [i|△1_2_3|]
-- [3]
-- >>> run [i|△1|]
-- []
-- >>> run [i|△[]|]
-- [0]
shape :: ArrayU -> Res
shape (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.shape $ a

-- | ⇡
--
-- >>> run [i|⇡5|]
-- [0 1 2 3 4]
-- >>> run [i|⇡2_3|]
-- ╭─
-- ╷ 0 0
-- ╷ 0 1
--   0 2
-- ...
--   1 0
--   1 1
--   1 2
--       ╯
-- >>> run [i|⇡[3]|]
-- ╭─
-- ╷ 0
--   1
--   2
--     ╯
range :: ArrayU -> Res
range (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.range $ fmap asInt a

-- | ⊢
--
-- >>> run [i|⊢1_2_3|]
-- 1
first :: ArrayU -> Res
first (ArrayU a) = Right . pure . ArrayU . A.first $ a

-- | ⇌
--
-- >>> run [i|⇌[1_2 3_4 5_6]|]
-- ╭─
-- ╷ 5 6
--   3 4
--   1 2
--       ╯
reverse :: ArrayU -> Res
reverse (ArrayU a) = Right . pure . ArrayU . A.reverse $ a

-- | ♭
--
-- >>> run [i|♭[1_2 3_4 5_6]|]
-- [1 2 3 4 5 6]
deshape :: ArrayU -> Res
deshape (ArrayU a) = Right . pure . ArrayU . A.deshape $ a

-- | ¤
--
-- >>> run [i|¤1_2]|]
-- ╭─
-- ╷ 1 2
--       ╯
fix :: ArrayU -> Res
fix (ArrayU a) = Right . pure . ArrayU . A.fix $ a

-- | ⋯
--
-- >>> run [i|⋯[1_2 3_4 5_6]|]
-- ╭─
-- ╷ 1 0 0
-- ╷ 0 1 0
-- ...
--   1 1 0
--   0 0 1
-- ...
--   1 0 1
--   0 1 1
--         ╯
bits :: ArrayU -> Res
bits (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.bits . fmap asInt $ a


-- | ⍉
--
-- >>> run [i|⍉[[1_2 3_4] [5_6 7_8]]|]
-- ╭─
-- ╷ 1 5
-- ╷ 2 6
-- ...
--   3 7
--   4 8
--       ╯
transpose :: ArrayU -> Res
transpose (ArrayU a) = Right . pure . ArrayU . A.transpose $ a

-- | ⍏
--
-- >>> run [i|⍏ 6_2_7_0_1_5|]
-- [3 4 1 5 0 2]
rise :: ArrayU -> Res
rise (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.rise $ a

-- | ⍖
--
-- >>> run [i|⍖ 6_2_7_0_1_5|]
-- [2 0 5 1 4 3]
fall :: ArrayU -> Res
fall (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.fall $ a


-- | ⊚
--
-- >>> run [i|⊚[1_0_0 0_1_1 0_2_0]|]
-- ╭─
-- ╷ 0 0
--   1 1
--   1 2
--   2 1
--   2 1
--       ╯
where' :: ArrayU -> Res
where' (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.where' . fmap asInt $ a

-- | ⊛
--
-- >>> run [i|⊛7_7_8_0_1_2_0|]
-- [0 0 1 2 3 4 2]
classify :: ArrayU -> Res
classify (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.classify $ a

-- | ◴
--
-- >>> run [i|◴ 7_7_8_0_1_2_0|]
-- [7 8 0 1 2]
deduplicate :: ArrayU -> Res
deduplicate (ArrayU a) = Right . pure . ArrayU . A.deduplicate $ a

-- | ◰
--
-- >>> run [i|◰ [3_2 1_4 3_2 5_6 1_4 7_8]|]
-- [1 1 0 1 0 1]
unique :: ArrayU -> Res
unique (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.unique $ a

-- | ∊
--
-- >>> run [i|∊ [1 2 3] [0 3 4 5 1]|]
-- [1 0 1]
member :: ArrayU -> ArrayU -> Res
member (ArrayU x) (ArrayU y) = Right . pure . ArrayU . fmap fromIntegral $ A.member x y

-- | ⊗
--
-- >>> run [i|⊗ 2 [1 2 3]|]
-- 1
indexOf :: ArrayU -> ArrayU -> Res
indexOf (ArrayU x) (ArrayU y) = Right . pure . ArrayU . fmap fromIntegral $ A.indexOf x y

-- * dyadic operators

-- | =
--
-- >>> run [i| =2 1|]
-- 0
-- >>> run [i| =2 [1 2 3]|]
-- [0 1 0]
-- >>> run [i| = [1 2 2] [1 2 3]|]
-- [1 1 0]
equals :: ArrayU -> ArrayU -> Res
equals (ArrayU x) (ArrayU y) = fmap (pure . ArrayU . fmap fromIntegral) $ A.equals x y

-- | ≠
--
-- >>> run [i| ≠2 1|]
-- 1
-- >>> run [i| ≠2 [1 2 3]|]
-- [1 0 1]
-- >>> run [i| ≠ [1 2 2] [1 2 3]|]
-- [0 0 1]
notequals :: ArrayU -> ArrayU -> Res
notequals (ArrayU x) (ArrayU y) = fmap (pure . ArrayU . fmap fromIntegral) $ A.notequals x y

-- | <
--
-- >>> run [i|<2 1|]
-- 1
-- >>> run [i|<2 [1 2 3]|]
-- [1 0 0]
-- >>> run [i|< [1 2 2] [1 2 3]|]
-- [0 0 0]
lessThan :: ArrayU -> ArrayU -> Res
lessThan (ArrayU x) (ArrayU y) = fmap (pure . ArrayU . fmap fromIntegral) $ A.lessThan x y

-- | ≤
--
-- >>> run [i|≤1 2|]
-- 0
-- >>> run [i|≤5 5|]
-- 1
lessOrEqual :: ArrayU -> ArrayU -> Res
lessOrEqual (ArrayU x) (ArrayU y) =fmap (pure . ArrayU . fmap fromIntegral) $ A.lessOrEqual x y

-- | >
--
-- >>> run [i| >1 2|]
-- 1
-- >>> run [i| >5 5|]
-- 0
greaterThan :: ArrayU -> ArrayU -> Res
greaterThan (ArrayU x) (ArrayU y) = fmap (pure . ArrayU . fmap fromIntegral) $ A.greaterThan x y

-- | ≥
--
-- >>> run [i| ≥1 2|]
-- 1
-- >>> run [i| ≥5 5|]
-- 1
greaterOrEqual :: ArrayU -> ArrayU -> Res
greaterOrEqual (ArrayU x) (ArrayU y) = fmap (pure . ArrayU . fmap fromIntegral) $ A.greaterOrEqual x y

-- | +
--
-- >>> run [i|+2 1|]
-- 3
-- >>> run [i|+2 [1 2 3]|]
-- [3 4 5]
-- >>> run [i|+ [1 2 2] [1 2 3]|]
-- [2 4 5]
-- >>> run [i|+ [1_2_3 4_5_6] [1 2]|]
-- ╭─
-- ╷ 2 3 4
--   6 7 8
--         ╯
-- >>> run [i|+ [1 2] [1_2_3 4_5_6]|]
-- ╭─
-- ╷ 2 3 4
--   6 7 8
--         ╯
add :: ArrayU -> ArrayU -> Res
add (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.add x y

-- | -
--
-- >>> run [i|-2 1|]
-- ¯1
-- >>> run [i|-2 [1 2 3]|]
-- [¯1 0 1]
-- >>> run [i|-[1 2 3] [4 5 6]|]
-- [3 3 3]
subtract :: ArrayU -> ArrayU -> Res
subtract (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.subtract x y

-- | ×
--
-- >>> run [i|×2 1|]
-- 2
-- >>> run [i|×2 [1 2 3]|]
-- [2 4 6]
multiply :: ArrayU -> ArrayU -> Res
multiply (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.multiply x y

-- | ÷
--
-- >>> run [i|÷2 1|]
-- 0.5
divide :: ArrayU -> ArrayU -> Res
divide (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.divide x y

-- | ◿
--
-- >>> run [i|◿10 27|]
-- 7
-- >>> run [i|◿5 [3 7 14]|]
-- [3 2 4]
modulus :: ArrayU -> ArrayU -> Res
modulus (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.modulus x y

-- | doctest bug
--
power :: ArrayU -> ArrayU -> Res
power (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.power x y

-- | doctest bug
--
logarithm :: ArrayU -> ArrayU -> Res
logarithm (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.logarithm x y

-- | ↧
--
-- >>> run [i|↧ 3 5|]
-- 3
-- >>> run [i|↧ [1 4 2] [3 7 1]|]
-- [1 4 1]
minimum :: ArrayU -> ArrayU -> Res
minimum (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.minimum x y

-- | ↥
--
-- >>> run [i|↥ 3 5|]
-- 5
-- >>> run [i|↥ [1 4 2] [3 7 1]|]
-- [3 7 2]
maximum :: ArrayU -> ArrayU -> Res
maximum (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.maximum x y


-- | ∠
--
-- >>> run [i|∠ 1 0|]
-- 1.5707963267948966
-- >>> run [i|∠ ¯1 0|]
-- ¯1.5707963267948966
atangent :: ArrayU -> ArrayU -> Res
atangent (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.atangent x y


-- | ≍
--
-- >>> run [i|≍ 1_2_3 [1 2 3]|]
-- 1
-- >>> run [i|≍ 1_2_3 1_2|]
-- 0
match :: ArrayU -> ArrayU -> Res
match (ArrayU x) (ArrayU y) = Right . pure . ArrayU . fmap fromIntegral $ A.match x y

-- | ⊟
--
-- >>> run [i|⊟ 1 2|]
-- [1 2]
-- >>> run [i|⊟ [1 2 3] [4 5 6]|]
-- ╭─
-- ╷ 1 2 3
--   4 5 6
--         ╯
couple :: ArrayU -> ArrayU -> Res
couple (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ A.couple x y

-- | ⊂
--
-- >>> run [i|⊂ 1 2|]
-- [1 2]
-- >>> run [i|⊂ [1 2 3] [4 5 6]|]
-- [1 2 3 4 5 6]
-- >>> run [i|⊂ 1 [2 3]|]
-- [1 2 3]
-- >>> run [i|⊂ [1 2] 3|]
-- [1 2 3]
-- >>> run [i|⊂ [1_2 3_4] 5_6|]
-- ╭─
-- ╷ 1 2
--   3 4
--   5 6
--       ╯
-- >>> run [i|⊂ [1_2] [3_4 5_6]|]
-- ╭─
-- ╷ 1 2
--   3 4
--   5 6
--       ╯
-- >>> run [i|⊂ 0 [1_2 3_4]|]
-- ╭─
-- ╷ 0 0
--   1 2
--   3 4
--       ╯
-- >>> run [i|⊂ [1_2 3_4] 0|]
-- ╭─
-- ╷ 1 2
--   3 4
--   0 0
--       ╯
join :: ArrayU -> ArrayU -> Res
join (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) (A.join x y)

-- | ⊂
--
-- >>> run [i|⊏ 2 [8 3 9 2 0]|]
-- 9
-- >>> run [i|⊏ 4_2 [8 3 9 2 0]|]
-- [0 9]
-- >>> run [i|⊏ 0_2_1_1 [1_2_3 4_5_6 7_8_9]|]
-- ╭─
-- ╷ 1 2 3
--   7 8 9
--   4 5 6
--   4 5 6
--         ╯
-- >>> run [i|⊏ [0_1 1_2 2_3] [2 3 5 7]|]
-- ╭─
-- ╷ 2 3
--   3 5
--   5 7
--       ╯
select :: ArrayU -> ArrayU -> Res
select (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ A.select (fmap asInt x) y

-- | ⊡
--
-- >>> run [i|⊡ [1_2 0_1] [1_2_3 4_5_6]|]
-- [6 2]
-- >>> run [i|⊡ 2_1 [8 3 9 2 0]|]
-- 9
-- >>> run [i|⊡ 1 [1_2_3 4_5_6]|]
-- [4 5 6]
pick :: ArrayU -> ArrayU -> Res
pick (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.pick (fmap asInt x) y

-- | ↯
--
-- >>> run [i|↯ 2_3 [1 2 3 4 5 6]|]
-- ╭─
-- ╷ 1 2 3
--   4 5 6
--         ╯
-- >>> run [i|↯ 2_2 [1_2_3 4_5_6]|]
-- ╭─
-- ╷ 1 2
--   3 4
--       ╯
-- >>> run [i|↯ [5] 2|]
-- [2 2 2 2 2]
-- >>> run [i|↯ 3_7 1_2_3_4|]
-- ╭─
-- ╷ 1 2 3 4 1 2 3
--   4 1 2 3 4 1 2
--   3 4 1 2 3 4 1
--                 ╯
-- >>> run [i|↯ 4 [1 2 3 4 5]|]
-- ╭─
-- ╷ 1 2 3 4 5
--   1 2 3 4 5
--   1 2 3 4 5
--   1 2 3 4 5
--             ╯
-- >>> run [i|↯ 2 [1_2_3 4_5_6]|]
-- ╭─
-- ╷ 1 2 3
-- ╷ 4 5 6
-- ...
--   1 2 3
--   4 5 6
--         ╯
reshape :: ArrayU -> ArrayU -> Res
reshape (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ A.reshape (fmap asInt x) y

-- | ☇
--
-- >>> run [i|☇ 1 ↯2_3_3⇡18|]
-- ╭─
-- ╷  0  1  2
--    3  4  5
--    6  7  8
--    9 10 11
--   12 13 14
--   15 16 17
--            ╯
-- >>> run [i|△☇ 3 ↯2_3_3⇡18|]
-- [1 2 3 3]
rerank :: ArrayU -> ArrayU -> Res
rerank (ArrayU x) (ArrayU y) = A.rerank (fmap asInt x) y & fmap (pure . ArrayU)

-- | ↙
--
-- >>> run [i|↙ [3] [8 3 9 2 0]|]
-- [8 3 9]
take :: ArrayU -> ArrayU -> Res
take (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.take (fmap asInt x) y

-- | ↘
--
-- >>> run [i|↘ 3 [8 3 9 2 0]|]
-- [2 0]
drop :: ArrayU -> ArrayU -> Res
drop (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.drop (fmap asInt x) y

-- | ↻
--
-- >>> run [i|↻1 ⇡5|]
-- [1 2 3 4 0]
rotate :: ArrayU -> ArrayU -> Res
rotate (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ A.rotate (fmap asInt x) y

-- | ▽
--
-- >>> run [i|▽ [3 2] [8 3 9 2 0]|]
-- [8 8 8 3 3 9 9 9 2 2 0 0 0]
keep :: ArrayU -> ArrayU -> Res
keep (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ (A.keep (fmap asInt x) y)

-- | ◫
--
-- >>> run [i|◫2 ⇡4|]
-- ╭─
-- ╷ 0 1
--   1 2
--   2 3
--       ╯
windows :: ArrayU -> ArrayU -> Res
windows (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ A.windows (fmap asInt x) y

-- | ⌕
--
-- >>> run [i|⌕ [5] [1 8 5 2 3 5 4 5 6 7]|]
-- [0 0 1 0 0 1 0 1 0 0]
-- >>> run [i|⌕ [1_2 2_0] ↯4_4⇡3|]
-- ╭─
-- ╷ 0 1 0 0
--   1 0 0 0
--   0 0 1 0
--   0 0 0 0
--           ╯
find:: ArrayU -> ArrayU -> Res
find (ArrayU x) (ArrayU y) = Right . pure . ArrayU . fmap fromIntegral $ A.find x y

-- | ⦷
--
-- >>> run [i|⦷ [1_1 1_1] ↯ [5 5] 1|]
-- ╭─
-- ╷ 1 1 2 2 0
--   1 1 2 2 0
--   3 3 4 4 0
--   3 3 4 4 0
--   0 0 0 0 0
--             ╯
mask :: ArrayU -> ArrayU -> Res
mask (ArrayU x) (ArrayU y) = Right . pure . ArrayU . fmap fromIntegral $ A.mask x y

-- | /=
--
-- >>> run [i|/=[0_1_0 0_4_3]|]
-- [1 0 0]
equalsR :: ArrayU -> Res
equalsR (ArrayU x) = Right . pure . ArrayU . fmap fromIntegral $ A.equalsR x

-- | /≠
--
-- >>> run [i|/≠[0_1_0 0_4_3]|]
-- [0 1 1]
notEqualsR :: ArrayU -> Res
notEqualsR (ArrayU x) = Right . pure . ArrayU . fmap fromIntegral $ A.notEqualsR x

-- | /<
--
-- >>> run [i|/<[2_1_0 0_4_3]|]
-- [1 0 0]
lessThanR :: ArrayU -> Res
lessThanR (ArrayU x) = Right . pure . ArrayU . fmap fromIntegral $ A.lessThanR x

-- | /≤
--
-- >>> run [i|/≤[0_1_0 0_4_3]|]
-- [1 0 0]
lessOrEqualR :: ArrayU -> Res
lessOrEqualR (ArrayU x) = Right . pure . ArrayU . fmap fromIntegral $ A.lessOrEqualR x

-- | />
--
-- >>> run [i|/>[0_1_0 0_4_3]|]
-- [0 1 1]
greaterThanR :: ArrayU -> Res
greaterThanR (ArrayU x) = Right . pure . ArrayU . fmap fromIntegral $ A.greaterThanR x

-- | /≥
--
-- >>> run [i|/≥[0_1_0 0_4_3]|]
-- [1 1 1]
greaterOrEqualR :: ArrayU -> Res
greaterOrEqualR (ArrayU x) = Right . pure . ArrayU . fmap fromIntegral $ A.greaterOrEqualR x


-- | /+
--
-- >>> run [i|/+[0_1_0 0_4_3]|]
-- [0 5 3]
addR :: ArrayU -> Res
addR (ArrayU x) = Right $ pure $ ArrayU $ A.addR x

-- | /-
--
-- >>> run [i|/- 1_2_3|]
-- 2
-- >>> run [i|/- [1_2_3 4_5_6]|]
-- [3 3 3]
subtractR :: ArrayU -> Res
subtractR (ArrayU x) = Right $ pure $ ArrayU $ A.subtractR x

-- | /×
--
-- >>> run [i|/× 1_2_3|]
-- 6
-- >>> run [i|/× [1_2_3 4_5_6]|]
-- [4 10 18]
multiplyR :: ArrayU -> Res
multiplyR (ArrayU x) = Right $ pure $ ArrayU $ A.multiplyR x

-- | /÷
--
-- >>> run [i|/÷ 1_2_3|]
-- 1.5
-- >>> run [i|/÷ [1_2_3 4_5_6]|]
-- [4 2.5 2]
divideR :: ArrayU -> Res
divideR (ArrayU x) = Right $ pure $ ArrayU $ A.divideR x

-- | /◿
--
-- >>> run [i|/◿ []|]
-- Infinity
-- >>> run [i|/◿ [2]|]
-- 2
-- >>> run [i|/◿ 2_1|]
-- 1
-- >>> run [i|/◿ [1_2_3 4_5_6]|]
-- [0 1 0]
modulusR :: ArrayU -> Res
modulusR (ArrayU x) = Right $ pure $ ArrayU $ A.modulusR x

-- | doctest subscript bug. see readme for test
powerR :: ArrayU -> Res
powerR (ArrayU x) = Right $ pure $ ArrayU $ A.powerR x

-- | doctest subscript bug. see readme for test
logarithmR :: ArrayU -> Res
logarithmR (ArrayU x) = fmap (pure . ArrayU) $ A.logarithmR x

-- | /↧
--
-- >>> run [i|/↧ []|]
-- Infinity
-- >>> run [i|/↧ [2]|]
-- 2
-- >>> run [i|/↧ 2_1|]
-- 1
-- >>> run [i|/↧ [1_2_3 4_5_6]|]
-- [1 2 3]
minimumR :: ArrayU -> Res
minimumR (ArrayU x) = Right $ pure $ ArrayU $ A.minimumR x

-- | /↥
--
-- >>> run [i|/↥ []|]
-- ¯Infinity
-- >>> run [i|/↥[2]|]
-- 2
-- >>> run [i|/↥ 2_1|]
-- 2
-- >>> run [i|/↥ [1_2_3 4_5_6]|]
-- [4 5 6]
maximumR :: ArrayU -> Res
maximumR (ArrayU x) = Right $ pure $ ArrayU $ A.maximumR x
