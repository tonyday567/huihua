{-# LANGUAGE RebindableSyntax #-}

module Huihua.Warning
  (
  HuihuaWarning (..),
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

data HuihuaWarning =
    NumHaskError String | NYI | EmptyStack1 | EmptyStack2 | ApplyFunction | NotBox | TypeMismatch | SizeMismatch | NotNat | EmptyArray | NotArray | NoScalarOp | OutOfBounds | NoOpenArray | NotReduceable | ApplyNonOperator deriving (Eq, Ord, Show)

showWarnings :: [HuihuaWarning] -> String
showWarnings = List.nub >>> fmap show >>> unlines

-- | A type synonym for the common returning type of many functions. A common computation pipeline is to take advantage of the 'These' Monad instance eg
--
type Warn a = These [HuihuaWarning] a

-- | Convert any warnings to an 'error'
--
warnError :: Warn a -> a
warnError = these (showWarnings >>> error) id (\xs a -> bool (error (showWarnings xs)) a (xs == []))

-- | Returns Left on any warnings
--
warnEither :: Warn a -> Either [HuihuaWarning] a
warnEither = these Left Right (\xs a -> bool (Left xs) (Right a) (xs == []))

-- | Returns results, if any, ignoring warnings.
--
warnMaybe :: Warn a -> Maybe a
warnMaybe = these (const Nothing) Just (\_ a -> Just a)
