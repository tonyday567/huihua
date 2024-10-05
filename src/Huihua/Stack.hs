module Huihua.Stack
  ( Stack (..),
  )
where

import Huihua.ArrayU
import Prettyprinter hiding (equals)
import Prelude

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Huihua.Stack as S
-- >>> import Harpie.Array as A

newtype Stack
  = Stack {stackList :: [ArrayU]}
  deriving (Show, Eq, Semigroup, Monoid)

instance Pretty Stack where
  pretty (Stack xs) = vsep (pretty <$> xs)
