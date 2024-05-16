{-# LANGUAGE RebindableSyntax #-}

module Huihua.Warning
  (
  HuiHuaWarning (..),
  showWarnings,
  Warn,
  warnError,
  warnEither,
  warnMaybe,
  )
where

import NumHask.Prelude
import Data.These
import Data.List qualified as List

data HuiHuaWarning =
    NYI | EmptyStack1 | EmptyStack2 | ApplyFunction | NotBox | TypeMismatch | SizeMismatch | NotNat | EmptyArray | NotArray | NoScalarOp | OutOfBounds | NoOpenArray | NotReduceable deriving (Eq, Ord, Show)

showWarnings :: [HuiHuaWarning] -> String
showWarnings = List.nub >>> fmap show >>> unlines

-- | A type synonym for the common returning type of many functions. A common computation pipeline is to take advantage of the 'These' Monad instance eg
--
-- > markup s bs = bs & (tokenize s >=> gather s) & second (Markup s)
type Warn a = These [HuiHuaWarning] a

-- | Convert any warnings to an 'error'
--
-- >>> warnError $ (tokenize Html) "<foo"
-- *** Exception: MarkupParser (ParserLeftover "<foo")
-- ...
warnError :: Warn a -> a
warnError = these (showWarnings >>> error) id (\xs a -> bool (error (showWarnings xs)) a (xs == []))

-- | Returns Left on any warnings
--
-- >>> warnEither $ (tokenize Html) "<foo><baz"
-- Left [MarkupParser (ParserLeftover "<baz")]
warnEither :: Warn a -> Either [HuiHuaWarning] a
warnEither = these Left Right (\xs a -> bool (Left xs) (Right a) (xs == []))

-- | Returns results, if any, ignoring warnings.
--
-- >>> warnMaybe $ (tokenize Html) "<foo><baz"
-- Just [OpenTag StartTag "foo" []]
warnMaybe :: Warn a -> Maybe a
warnMaybe = these (const Nothing) Just (\_ a -> Just a)
