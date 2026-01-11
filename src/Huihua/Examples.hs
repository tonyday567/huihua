{-# LANGUAGE OverloadedStrings #-}

module Huihua.Examples where

import Data.ByteString (ByteString)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Prelude
-- >>> import Huihua.Examples
-- >>> import Huihua.Parse as P
-- >>> import Harpie.Array as A
-- >>> import Data.List qualified as List
-- >>> import Data.ByteString.Char8 qualified as C
-- >>> import FlatParse.Basic
-- >>> import Data.Function ((&))
--
-- yet to be implemented
--
-- infinite and negative axes in reshape
--
-- multi-line arrays
--
-- >>> run nyiMultiArray
-- 7
-- 8
-- 9
--
--
-- ... format
-- >>> run "÷ 3 1"
-- 0.3333333333333333
--
-- negate strand combination.
-- >>> run "¯2_¯2"
-- ¯2
--
-- Implemented:
--
-- broadcasting scalars (and prefixed arrays?)
--
-- >>> run "<2 [1 2 3]"
-- [1 0 0]
--
-- multi-dim bool reductions
-- >>> run "/<[2_1_0 0_4_3]"
-- [1 0 0]
--
-- operators and stuff inside square brackets
--
-- >>> run "[. 1 2 3 4]"
-- [1 1 2 3 4]
--
-- display negate sign for numbers
--
-- >>> run "¯1"
-- ¯1
--
-- strand square bracket combination
--
-- >>> run "[1_2 3_4 5_6]"
-- ╭─
-- ╷ 1 2
--   3 4
--   5 6
--       ╯

-- |
--
-- >>> run exPage1
-- 4
exPage1 :: ByteString
exPage1 =
  "[1 5 8 2]\n"
    <> "/+. # Sum\n"
    <> "⧻:  # Length\n"
    <> "÷   # Divide\n"

-- |
--
-- >>> run exPage2
-- ╭─
-- ╷  0  1  2  3
-- ╷  4  5  6  7
--    8  9 10 11
-- ...
--   12 13 14 15
--   16 17 18 19
--   20 21 22 23
--               ╯
exPage2 :: ByteString
exPage2 =
  "2_3_4\n"
    <> "/×. # Product\n"
    <> "⇡   # Range\n"
    <> "↯:  # Reshape\n"

-- | character arrays not yet implemented.
--
-- >>> exPage3 & C.lines & fmap (runParser tokens)
-- [OK [GlyphToken String,NameToken "Unabashedly",NameToken "I",NameToken "utilize",NameToken "arrays",GlyphToken String] "",OK [GlyphToken NotEquals,CharacterToken ' ',GlyphToken Duplicate,CommentToken " Mask of non-spaces"] "",OK [GlyphToken Partition,GlyphToken First,CommentToken " All first letters"] ""]
exPage3 :: ByteString
exPage3 =
  "\"Unabashedly I utilize arrays\"\n"
    <> "≠@ . # Mask of non-spaces\n"
    <> "⊜⊢   # All first letters\n"

nyiMultiArray :: ByteString
nyiMultiArray =
  "[1 2 3\n"
    <> " 4 5 6\n"
    <> " 7 8 9]\n"
