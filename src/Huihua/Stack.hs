{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Huihua.Stack
  ( Stack (..),
  )
where

import Data.Distributive (Distributive (..))
import Data.Functor.Rep
import Data.Vector qualified as V
import Harry.Array
import Harry.Shape
import Huihua.Array
import Huihua.ArrayU
import Huihua.Warning
import Prettyprinter hiding (equals)
import Prelude
import Prelude qualified as P

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Huihua.Stack as S
-- >>> import Harry.Array as A

newtype Stack
  = Stack {stackList :: [ArrayU]}
  deriving (Show, Eq, Semigroup, Monoid)

instance Pretty Stack where
  pretty (Stack xs) = vsep (pretty <$> xs)
