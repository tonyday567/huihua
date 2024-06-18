{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Compatability layer between NumHask.Dynamic.Array and uiua arrays.
--
-- Unlike uiua, this layer distinguishes between Int and Double arrays.
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
    negate,
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
import NumHask.Array.Dynamic (Array (..))
import NumHask.Array.Dynamic qualified as D
import NumHask.Prelude hiding (not, negate, sqrt, sin, floor, ceiling, round, minimum, maximum, length, reverse, fix, take, drop, find)
import NumHask.Prelude qualified as P
import Huihua.Warning
import Prettyprinter hiding (equals)
import Data.Bifunctor qualified as Bi
import Data.List qualified as List
import Data.Tree qualified as Tree
import Data.Either

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Huihua.Array as A
-- >>> import NumHask.Array.Dynamic as D

-- >>> x = D.array [2,2] [2, 2.222,200.001,200.01] :: Array Double
-- >>> pretty (ArrayU x)
newtype ArrayU = ArrayU { arrayd :: Array Double } deriving (Eq, Show, Generic)

instance Pretty ArrayU where
  pretty (ArrayU x) = case D.rank x of
    0 -> pretty $ showU (D.fromScalar x)
    1 -> (pretty "[") <> hsep (fmap (pretty . showU) (D.arrayAs x)) <> (pretty "]")
    _ -> final
    where
    t = ptree (fmap showU x)
    maxp = D.folds [1] (P.maximum . fmap P.length) (D.join $ D.asArray $ rights t)
    s = fmap (fmap ((D.zipWithE (\m a -> lpad ' ' m a) maxp))) t
    sdoc = mconcat $ fmap (either (\n -> replicate (n-1) mempty) (pure . hsep . fmap pretty . D.arrayAs)) s
    sdocMin = D.concatenate 0 (D.konst [max 0 (D.rank x - P.length sdoc - 1)] mempty) (D.asArray sdoc)
    rankPrefix = fmap pretty (D.reshapeDef " " [D.length sdocMin] (D.konst [D.rank x - 1] "╷"))
    deco = zipWith (<+>) (D.arrayAs rankPrefix) (D.arrayAs sdocMin)
    final = (pretty "╭─") <> line <> (vsep deco) <> hang 1 (line <> pretty "╯")

showU :: Double -> String
showU x = bool (show x) (show (asInt x)) (isInt x)

lpad :: Char -> Int -> String -> String
lpad c maxl x = (replicate (maxl - P.length x) c) <> x

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
isInt x = x == fromIntegral (P.floor x)

asInt :: Double -> Int
asInt x = P.floor x

arrayi_ :: ArrayU -> Array Int
arrayi_ a = either (error . show) id (arrayi' a)

type Res = Either HuihuaWarning [ArrayU]


duplicate :: ArrayU -> Res
duplicate x = Right [x,x]

pop :: ArrayU -> Res
pop _ = Right []

identity :: ArrayU -> Res
identity x = Right [x]

not :: ArrayU -> Res
not (ArrayU a) = Right . pure . ArrayU . A.not $ a

sign :: ArrayU -> Res
sign (ArrayU a) = Right . pure . ArrayU . A.sign $ a

negate :: ArrayU -> Res
negate (ArrayU a) = Right . pure . ArrayU . A.negate $ a

absoluteValue :: ArrayU -> Res
absoluteValue (ArrayU a) = Right . pure . ArrayU . A.absolute $ a

sqrt :: ArrayU -> Res
sqrt (ArrayU a) = Right . pure . ArrayU . A.sqrt $ a

sine :: ArrayU -> Res
sine (ArrayU a) = Right . pure . ArrayU . A.sin $ a

floor :: ArrayU -> Res
floor (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.floor $ a

ceiling :: ArrayU -> Res
ceiling (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.ceiling $ a

round :: ArrayU -> Res
round (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.round $ a

length :: ArrayU -> Res
length (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.length $ a

shape :: ArrayU -> Res
shape (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.shape $ a

range :: ArrayU -> Res
range (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.range $ fmap asInt a

first :: ArrayU -> Res
first (ArrayU a) = Right . pure . ArrayU . A.first $ a

reverse :: ArrayU -> Res
reverse (ArrayU a) = Right . pure . ArrayU . A.reverse $ a

deshape :: ArrayU -> Res
deshape (ArrayU a) = Right . pure . ArrayU . A.deshape $ a

fix :: ArrayU -> Res
fix (ArrayU a) = Right . pure . ArrayU . A.fix $ a

bits :: ArrayU -> Res
bits (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.bits . fmap asInt $ a

transpose :: ArrayU -> Res
transpose (ArrayU a) = Right . pure . ArrayU . A.transpose $ a

rise :: ArrayU -> Res
rise (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.rise $ a

fall :: ArrayU -> Res
fall (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.fall $ a

where' :: ArrayU -> Res
where' (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.where' . fmap asInt $ a

classify :: ArrayU -> Res
classify (ArrayU a) = Right . pure . ArrayU . fmap fromIntegral . A.classify $ a

deduplicate :: ArrayU -> Res
deduplicate (ArrayU a) = Right . pure . ArrayU . A.deduplicate $ a

-- * dyadic operators
equals :: ArrayU -> ArrayU -> Res
equals (ArrayU x) (ArrayU y) = fmap (pure . ArrayU . fmap fromIntegral) $ A.equals x y

notequals :: ArrayU -> ArrayU -> Res
notequals (ArrayU x) (ArrayU y) = fmap (pure . ArrayU . fmap fromIntegral) $ A.notequals x y

lt :: ArrayU -> ArrayU -> Res
lt (ArrayU x) (ArrayU y) = fmap (pure . ArrayU . fmap fromIntegral) $ A.lt x y

lte :: ArrayU -> ArrayU -> Res
lte (ArrayU x) (ArrayU y) =fmap (pure . ArrayU . fmap fromIntegral) $ A.lte x y

gt :: ArrayU -> ArrayU -> Res
gt (ArrayU x) (ArrayU y) = fmap (pure . ArrayU . fmap fromIntegral) $ A.gt x y

gte :: ArrayU -> ArrayU -> Res
gte (ArrayU x) (ArrayU y) = fmap (pure . ArrayU . fmap fromIntegral) $ A.gte x y

add :: ArrayU -> ArrayU -> Res
add (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.add x y

subtract :: ArrayU -> ArrayU -> Res
subtract (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.subtract x y

multiply :: ArrayU -> ArrayU -> Res
multiply (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.multiply x y

divide :: ArrayU -> ArrayU -> Res
divide (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.divide x y

modulus :: ArrayU -> ArrayU -> Res
modulus (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.modulusD x y

power :: ArrayU -> ArrayU -> Res
power (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.power x y

logarithm :: ArrayU -> ArrayU -> Res
logarithm (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.power x y

minimum :: ArrayU -> ArrayU -> Res
minimum (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.minimum x y

maximum :: ArrayU -> ArrayU -> Res
maximum (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.maximum x y

arctangent :: ArrayU -> ArrayU -> Res
arctangent (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ A.arctangent x y

match :: ArrayU -> ArrayU -> Res
match (ArrayU x) (ArrayU y) = Right . pure . ArrayU . fmap fromIntegral $ A.match x y

couple :: ArrayU -> ArrayU -> Res
couple (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ A.couple x y

join :: ArrayU -> ArrayU -> Res
join (ArrayU x) (ArrayU y) = fmap (pure . ArrayU) $ Bi.first (const SizeMismatch) (A.join x y)

select :: ArrayU -> ArrayU -> Res
select (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ A.select (fmap asInt x) y

pick :: ArrayU -> ArrayU -> Res
pick (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ A.pick (fmap asInt x) y

reshape :: ArrayU -> ArrayU -> Res
reshape (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ A.reshape (fmap asInt x) y

rerank :: ArrayU -> ArrayU -> Res
rerank (ArrayU x) (ArrayU y) = A.rerank (fmap asInt x) y & fmap (pure . ArrayU)

take :: ArrayU -> ArrayU -> Res
take (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ A.take (fmap asInt x) y

drop :: ArrayU -> ArrayU -> Res
drop (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ A.drop (fmap asInt x) y

rotate :: ArrayU -> ArrayU -> Res
rotate (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ A.rotate (fmap asInt x) y

windows :: ArrayU -> ArrayU -> Res
windows (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ A.windows (fmap asInt x) y

keep :: ArrayU -> ArrayU -> Res
keep (ArrayU x) (ArrayU y) = Right . pure . ArrayU $ (A.keep (fmap asInt x) y)

find:: ArrayU -> ArrayU -> Res
find (ArrayU x) (ArrayU y) = Right . pure . ArrayU . fmap fromIntegral $ A.find x y

equalsR :: ArrayU -> Res
equalsR (ArrayU x) = Right . pure . ArrayU . fmap fromIntegral $ A.equalsR x

notEqualsR :: ArrayU -> Res
notEqualsR (ArrayU x) = Right . pure . ArrayU . fmap fromIntegral $ A.notEqualsR x

lessThanR :: ArrayU -> Res
lessThanR (ArrayU x) = Right . pure . ArrayU . fmap fromIntegral $ A.lessThanR x

lessOrEqualR :: ArrayU -> Res
lessOrEqualR (ArrayU x) = Right . pure . ArrayU . fmap fromIntegral $ A.lessOrEqualR x

greaterThanR :: ArrayU -> Res
greaterThanR (ArrayU x) = Right . pure . ArrayU . fmap fromIntegral $ A.greaterThanR x

greaterOrEqualR :: ArrayU -> Res
greaterOrEqualR (ArrayU x) = Right . pure . ArrayU . fmap fromIntegral $ A.greaterOrEqualR x

addR :: ArrayU -> Res
addR (ArrayU x) = Right $ pure $ ArrayU $ A.addR x

subtractR :: ArrayU -> Res
subtractR (ArrayU x) = Right $ pure $ ArrayU $ A.subtractR x

multiplyR :: ArrayU -> Res
multiplyR (ArrayU x) = Right $ pure $ ArrayU $ A.multiplyR x

divideR :: ArrayU -> Res
divideR (ArrayU x) = Right $ pure $ ArrayU $ A.divideR x

modulusR :: ArrayU -> Res
modulusR (ArrayU x) = Right $ pure $ ArrayU $ fmap fromIntegral $ A.modulusR (fmap asInt x)

powerR :: ArrayU -> Res
powerR (ArrayU x) = Right $ pure $ ArrayU $ A.powerR x

logarithmR :: ArrayU -> Res
logarithmR (ArrayU x) = Right $ pure $ ArrayU $ A.logarithmR x

minimumR :: ArrayU -> Res
minimumR (ArrayU x) = Right $ pure $ ArrayU $ A.minimumR x

maximumR :: ArrayU -> Res
maximumR (ArrayU x) = Right $ pure $ ArrayU $ A.maximumR x
