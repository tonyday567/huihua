{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Huihua.Stack
  (
  Stack (..),
  )
where

import NumHask.Prelude
import NumHask.Prelude qualified as P
import NumHask.Array.Dynamic
import NumHask.Array.Shape
import Data.Distributive (Distributive (..))
import Data.Functor.Rep
import Data.Vector qualified as V
import Huihua.Warning
import Prettyprinter hiding (equals)
import Huihua.Array
import Huihua.ArrayU

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Huihua.Stack as S
-- >>> import NumHask.Array.Dynamic as A

newtype Stack =
  Stack { stackList :: [ArrayU] } deriving (Show, Eq, Semigroup, Monoid)

instance Pretty Stack where
  pretty (Stack xs) = vsep (pretty <$> xs)


