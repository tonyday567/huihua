{-# LANGUAGE QuasiQuotes #-}

module Huihua.Examples where

import Data.String.Interpolate
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import NumHask.Prelude
-- >>> import Huihua.Examples
-- >>> import Huihua.Parse as P
-- >>> import NumHask.Array.Dynamic as A
-- >>> import Data.List qualified as List
-- >>> import Data.ByteString.Char8 qualified as C
-- >>> import FlatParse.Basic

-- |
--
-- >>> run exPage1
-- ArrayI 4
exPage1 :: ByteString
exPage1 = encodeUtf8 [i|
[1 5 8 2]
/+. \# Sum
⧻:  \# Length
÷   \# Divide
|]

-- |
--
-- >>> run exPage2
-- ArrayI [[[0,1,2,3],
--          [4,5,6,7],
--          [8,9,10,11]],
--         [[12,13,14,15],
--          [16,17,18,19],
--          [20,21,22,23]]]
exPage2 :: ByteString
exPage2 = encodeUtf8 [i|
2_3_4
/×. \# Product
⇡   \# Range
↯:  \# Reshape
|]

-- | character arrays not yet implemented.
--
-- > run exPage3
--
-- FIXME: fix glyphtoken string
-- >>> exPage3 & C.lines & fmap (runParser tokens)
-- [OK [] "",OK [GlyphToken String,NameToken "Unabashedly",NameToken "I",NameToken "utilize",NameToken "arrays",GlyphToken String] "",OK [GlyphToken NotEquals,CharacterToken ' ',GlyphToken Duplicate,CommentToken " Mask of non-spaces"] "",OK [GlyphToken Partition,GlyphToken First,CommentToken " All first letters"] ""]
exPage3 :: ByteString
exPage3 = [i|
"Unabashedly I utilize arrays"
≠@ . \# Mask of non-spaces
⊜⊢   \# All first letters
|]
