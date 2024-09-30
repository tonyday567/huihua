{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Huihua.Stack
  (
  Stack (..),
  )
where

import Prelude
import Prelude qualified as P
import Harry.Dynamic
import Harry.Shape
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
-- >>> import Harry.Dynamic as A

newtype Stack =
  Stack { stackList :: [ArrayU] } deriving (Show, Eq, Semigroup, Monoid)

instance Pretty Stack where
  pretty (Stack xs) = vsep (pretty <$> xs)


