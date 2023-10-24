{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Huihua.Parse where

import NumHask.Prelude hiding (First, (<|>))
import FlatParse.Basic
import Huihua.Parse.FlatParse
-- import MarkupParse qualified as MP
-- import Data.String.Interpolate
-- import qualified Data.ByteString as B
import Data.ByteString (ByteString)

-- Glyphs removed
-- Character "@"
-- "\"" -> pure String

data Glyph =
  Duplicate |
  Over |
  Flip |
  Pop |
  Identity |
  Not |
  Sign |
  Negate |
  AbsoluteValue |
  Sqrt |
  Sine |
  Floor |
  Ceiling |
  Round |
  Equals |
  NotEquals |
  LessThan |
  LessOrEqual |
  GreaterThan |
  GreaterOrEqual |
  Add |
  Subtract |
  Multiply |
  Divide |
  Modulus |
  Power |
  Logarithm |
  Minimum |
  Maximum |
  Atangent |
  Length |
  Shape |
  Range |
  First |
  Reverse |
  Deshape |
  Bits |
  Transpose |
  Rise |
  Fall |
  Where |
  Classify |
  Deduplicate |
  Box |
  Unbox |
  Match |
  Couple |
  Join |
  Select |
  Pick |
  Reshape |
  Take |
  Drop |
  Rotate |
  Windows |
  Keep |
  Find |
  Member |
  IndexOf |
  Reduce |
  Fold |
  Scan |
  Each |
  Rows |
  Distribute |
  Table |
  Cross |
  Repeat |
  Group |
  Partition |
  Invert |
  Gap |
  Dip |
  Both |
  Fork |
  Bracket |
  Under |
  Level |
  Fill |
  Bind |
  If |
  Try |
  Assert |
  Call |
  Break |
  Recur |
  Random |
  Eta |
  Pi |
  Tau |
  Infinity |
  Trace |
  Strand |
  ArrayLeft |
  ArrayRight |
  BoxArrayLeft |
  BoxArrayRight |
  FunctionLeft |
  FunctionRight |
  Negative |
  Format |
  String |
  Binding |
  Signature |
  Comment
  deriving (Eq, Ord, Show)

allTheGlyphs :: [Glyph]
allTheGlyphs =
  [ Duplicate
  , Over
  , Flip
  , Pop
  , Identity
  , Not
  , Sign
  , Negate
  , AbsoluteValue
  , Sqrt
  , Sine
  , Floor
  , Ceiling
  , Round
  , Equals
  , NotEquals
  , LessThan
  , LessOrEqual
  , GreaterThan
  , GreaterOrEqual
  , Add
  , Subtract
  , Multiply
  , Divide
  , Modulus
  , Power
  , Logarithm
  , Minimum
  , Maximum
  , Atangent
  , Length
  , Shape
  , Range
  , First
  , Reverse
  , Deshape
  , Bits
  , Transpose
  , Rise
  , Fall
  , Where
  , Classify
  , Deduplicate
  , Box
  , Unbox
  , Match
  , Couple
  , Join
  , Select
  , Pick
  , Reshape
  , Take
  , Drop
  , Rotate
  , Windows
  , Keep
  , Find
  , Member
  , IndexOf
  , Reduce
  , Fold
  , Scan
  , Each
  , Rows
  , Distribute
  , Table
  , Cross
  , Repeat
  , Group
  , Partition
  , Invert
  , Gap
  , Dip
  , Both
  , Fork
  , Bracket
  , Under
  , Level
  , Fill
  , Bind
  , If
  , Try
  , Assert
  , Call
  , Break
  , Recur
  , Random
  , Eta
  , Pi
  , Tau
  , Infinity
  , Trace
  , Strand
  , ArrayLeft
  , ArrayRight
  , BoxArrayLeft
  , BoxArrayRight
  , FunctionLeft
  , FunctionRight
  , Negative
  , Format
  , String
  , Binding
  , Signature
  , Comment
  ]

glyph :: Parser e Glyph
glyph =
  $( switch
       [|
         case _ of
           "." -> pure Duplicate
           "," -> pure Over
           "∶" -> pure Flip
           ";" -> pure Pop
           "∘" -> pure Identity
           "¬" -> pure Not
           "±" -> pure Sign
           "¯" -> pure Negate
           "⌵" -> pure AbsoluteValue
           "√" -> pure Sqrt
           "○" -> pure Sine
           "⌊" -> pure Floor
           "⌈" -> pure Ceiling
           "⁅" -> pure Round
           "=" -> pure Equals
           "≠" -> pure NotEquals
           "<" -> pure LessThan
           "≤" -> pure LessOrEqual
           ">" -> pure GreaterThan
           "≥" -> pure GreaterOrEqual
           "+" -> pure Add
           "-" -> pure Subtract
           "×" -> pure Multiply
           "÷" -> pure Divide
           "◿" -> pure Modulus
           "ⁿ" -> pure Power
           "ₙ" -> pure Logarithm
           "↧" -> pure Minimum
           "↥" -> pure Maximum
           "∠" -> pure Atangent
           "⧻" -> pure Length
           "△" -> pure Shape
           "⇡" -> pure Range
           "⊢" -> pure First
           "⇌" -> pure Reverse
           "♭" -> pure Deshape
           "⋯" -> pure Bits
           "⍉" -> pure Transpose
           "⍏" -> pure Rise
           "⍖" -> pure Fall
           "⊚" -> pure Where
           "⊛" -> pure Classify
           "⊝" -> pure Deduplicate
           "□" -> pure Box
           "⊔" -> pure Unbox
           "≅" -> pure Match
           "⊟" -> pure Couple
           "⊂" -> pure Join
           "⊏" -> pure Select
           "⊡" -> pure Pick
           "↯" -> pure Reshape
           "↙" -> pure Take
           "↘" -> pure Drop
           "↻" -> pure Rotate
           "◫" -> pure Windows
           "▽" -> pure Keep
           "⌕" -> pure Find
           "∊" -> pure Member
           "⊗" -> pure IndexOf
           "/" -> pure Reduce
           "∧" -> pure Fold
           "\\" -> pure Scan
           "∵" -> pure Each
           "≡" -> pure Rows
           "∺" -> pure Distribute
           "⊞" -> pure Table
           "⊠" -> pure Cross
           "⍥" -> pure Repeat
           "⊕" -> pure Group
           "⊜" -> pure Partition
           "⍘" -> pure Invert
           "⋅" -> pure Gap
           "⊙" -> pure Dip
           "∩" -> pure Both
           "⊃" -> pure Fork
           "⊓" -> pure Bracket
           "⍜" -> pure Under
           "⍚" -> pure Level
           "⬚" -> pure Fill
           "'" -> pure Bind
           "?" -> pure If
           "⍣" -> pure Try
           "⍤" -> pure Assert
           "!" -> pure Call
           "⎋" -> pure Break
           "↬" -> pure Recur
           "⚂" -> pure Random
           "η" -> pure Eta
           "π" -> pure Pi
           "τ" -> pure Tau
           "∞" -> pure Infinity
           "~" -> pure Trace
           "_" -> pure Strand
           "[" -> pure ArrayLeft
           "]" -> pure ArrayRight
           "{" -> pure BoxArrayLeft
           "}" -> pure BoxArrayRight
           "(" -> pure FunctionLeft
           ")" -> pure FunctionRight
           -- FIXME: WTF negative!
           -- "¯" -> pure Negative
           "negative" -> pure Negative
           "$" -> pure Format
           "←" -> pure Binding
           "|" -> pure Signature
           "#" -> pure Comment
           |])

data Token = StringToken ByteString | GlyphToken Glyph | IntToken Int | DoubleToken Double | CharacterToken Char | NameToken String | TypeToken deriving (Eq, Ord, Show)

-- |
-- Double token has precedence over duplicate
token :: Parser e Token
token =
  ((\x -> bool (DoubleToken x) (IntToken (NumHask.Prelude.floor x)) (x==(fromIntegral . floor) x)) <$> double) <|>
  (GlyphToken <$> glyph) <|>
  (StringToken <$> wrappedDq) <|>
  (CharacterToken <$> ($(char '@') *> anyChar)) <|>
  (NameToken <$> (some (satisfy isLatinLetter) <|> ((:[]) <$> undefined))) <|>
  (TypeToken <$ $(string "type"))

tokens :: Parser e [Token]
tokens = many (ws_ *> token) <* ws_


-- >>> sequence_ $ C.putStr <$> (ts <> ["\n"])
-- .,∶;∘¬±¯⌵√○⌊⌈⁅=≠&lt;≤&gt;≥+-×÷◿ⁿₙ↧↥∠⧻△⇡⊢⇌♭⋯⍉⍏⍖⊚⊛⊝□⊔≅⊟⊂⊏⊡↯↙↘↻◫▽⌕∊⊗/∧\∵≡∺⊞⊠⍥⊕⊜⍘⋅⊙∩⊃⊓⍜⍚⬚'?⍣⍤!⎋↬⚂ηπτ∞~_[]{}()¯@$"←|
allTheSymbols :: [ByteString]
allTheSymbols =  [".",",","\226\136\182",";","\226\136\152","\194\172","\194\177","\194\175","\226\140\181","\226\136\154","\226\151\139","\226\140\138","\226\140\136","\226\129\133","=","\226\137\160","&lt;","\226\137\164","&gt;","\226\137\165","+","-","\195\151","\195\183","\226\151\191","\226\129\191","\226\130\153","\226\134\167","\226\134\165","\226\136\160","\226\167\187","\226\150\179","\226\135\161","\226\138\162","\226\135\140","\226\153\173","\226\139\175","\226\141\137","\226\141\143","\226\141\150","\226\138\154","\226\138\155","\226\138\157","\226\150\161","\226\138\148","\226\137\133","\226\138\159","\226\138\130","\226\138\143","\226\138\161","\226\134\175","\226\134\153","\226\134\152","\226\134\187","\226\151\171","\226\150\189","\226\140\149","\226\136\138","\226\138\151","/","\226\136\167","\\","\226\136\181","\226\137\161","\226\136\186","\226\138\158","\226\138\160","\226\141\165","\226\138\149","\226\138\156","\226\141\152","\226\139\133","\226\138\153","\226\136\169","\226\138\131","\226\138\147","\226\141\156","\226\141\154","\226\172\154","'","?","\226\141\163","\226\141\164","!","\226\142\139","\226\134\172","\226\154\130","\206\183","\207\128","\207\132","\226\136\158","~","_","[","]","{","}","(",")","\194\175","@","$","\"","\226\134\144","|","#"]

