{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Huihua.Parse where

import NumHask.Prelude as P hiding (First, (<|>), null)
import FlatParse.Basic
import Huihua.Parse.FlatParse
-- import MarkupParse qualified as MP
import Data.String.Interpolate
-- import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import NumHask.Array.Dynamic
import Data.List qualified as List
-- import Data.These
-- import Data.Bifunctor
import Huihua.Stack as S
import Huihua.Warning
import Huihua.Array qualified as A
import Data.ByteString.Char8 qualified as C
import Data.Text.Encoding (encodeUtf8)
import Control.Monad

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
            "&lt;" -> pure LessThan
            "≤" -> pure LessOrEqual
            "&gt;" -> pure GreaterThan
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
            -- "¯" -> pure Negative
            "@" -> pure Format
            "$" -> pure String
            "\"" -> pure Binding
            "←" -> pure Signature
            "|" -> pure Comment
           |])

data Token = StringToken ByteString | GlyphToken Glyph | IntToken Int | DoubleToken Double | CharacterToken Char | NameToken String | CommentToken ByteString | TypeToken deriving (Eq, Ord, Show)

-- |
-- Double token has precedence over duplicate
token :: Parser e Token
token =
  ((\x -> bool (DoubleToken x) (IntToken (P.floor x)) (x==(fromIntegral . P.floor) x)) <$> double) <|>
  (GlyphToken <$> glyph) <|>
  (StringToken <$> wrappedDq) <|>
  (CharacterToken <$> ($(char '@') *> anyChar)) <|>
  (CommentToken <$> ($(char '#') *> nota '\n')) <|>
  (NameToken <$> some (satisfy isLatinLetter)) <|>
  (TypeToken <$ $(string "type"))

tokens :: Parser e [Token]
tokens = many (ws_ *> token) <* ws_

tokenize :: ByteString -> Either ByteString [[Token]]
tokenize bs = runParserEither (many tokens) bs

ops :: [Glyph]
ops =
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
  ]

nullaryOps :: [Glyph]
nullaryOps =
  [
    Random
  , Eta
  , Pi
  , Tau
  , Infinity
  ]

isNullaryOp :: Glyph -> Bool
isNullaryOp g = g `List.elem` nullaryOps

unaryOps :: [Glyph]
unaryOps =
  [ Duplicate
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
  , Rows
  ]

isUnaryOp :: Glyph -> Bool
isUnaryOp g = g `List.elem` unaryOps

binaryOps :: [Glyph]
binaryOps =
  [
    Over
  , Flip
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
  ]

isBinaryOp :: Glyph -> Bool
isBinaryOp g = g `List.elem` binaryOps

reduceOps :: [Glyph]
reduceOps =
  [
    Equals
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
  ]

isReduceOp :: Glyph -> Bool
isReduceOp op = op `elem` reduceOps

functionOps :: [Glyph]
functionOps =
  [ Reduce
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
  ]

isFunctionOp :: Glyph -> Bool
isFunctionOp g = g `List.elem` functionOps

notOps :: [Glyph]
notOps =
  [ Trace
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

data Assemble = ANotOp Glyph | AOp Glyph | AReduceOp Glyph | AInt Int | AInts [Int] | AArrayOpen | AArrayInt (Array Int) | AArrayDouble (Array Double) | AComment ByteString | AString ByteString | AChar Char | AName String | ADouble Double | ADoubles [Double] | AType | ANYI Token | AError HuiHuaWarning deriving (Eq, Show, Ord, Generic)

assemble :: Token -> [Assemble] -> [Assemble]
assemble (GlyphToken ArrayRight) as = AArrayOpen:as
assemble (GlyphToken ArrayLeft) (AArrayOpen:as) = AArrayInt (fromList1 []):as
assemble (GlyphToken ArrayLeft) [] = [AError NoOpenArray]
assemble (GlyphToken ArrayLeft) (AInts xs:as) = AArrayInt (fromList1 (List.reverse xs)):as
assemble (GlyphToken ArrayLeft) (ADoubles xs:as) = AArrayDouble (fromList1 (List.reverse xs)):as
assemble (GlyphToken Reduce) (AOp op:as) = bool (AError NotReduceable:as) (AReduceOp op:as) (isReduceOp op)
assemble (GlyphToken Reduce) as = AError NotReduceable:as

assemble (GlyphToken g) as =
  bool
  ((ANotOp g) : as)
  ((AOp g) : as)
  (g `List.elem` ops)
assemble (IntToken x) [] = [AInt x]
assemble (IntToken x) (AArrayOpen:as) = (AInts [x]:as)
assemble (IntToken x) (AInts xs:as) = (AInts (xs<>[x]):as)
assemble (IntToken x) xs = (AInt x:xs)
assemble (DoubleToken x) [] = [ADouble x]
assemble (DoubleToken x) (AArrayOpen:as) = (ADoubles [x]:as)
assemble (DoubleToken x) (ADoubles xs:as) = (ADoubles (xs<>[x]):as)
assemble (DoubleToken x) xs = (ADouble x:xs)
assemble (CommentToken c) xs = (AComment c:xs)
assemble t xs = (ANYI t:xs)

assemblef :: [Token] -> [Assemble]
assemblef ts = foldl' (P.flip assemble) [] ts

assemble' :: ByteString -> [Assemble]
assemble' bs = bs & C.lines & fmap (runParser_ tokens) & orderUiua & assemblef & P.reverse

isComment :: Token -> Bool
isComment (CommentToken _) = True
isComment _ = False

orderUiua :: [[Token]] -> [Token]
orderUiua tss = tss & fmap List.reverse & mconcat & List.filter (P.not . isComment)

compute1 :: Assemble -> Stack -> Either HuiHuaWarning Stack
compute1 (AOp Duplicate) s = duplicate s
compute1 (AOp Over) s = over s
compute1 (AOp Flip) s = S.flip s
compute1 (AOp Pop) s = pop s
compute1 (AOp Identity) s = Right (identity s)
compute1 (AOp _) (Stack []) = Left EmptyStack1
compute1 (AOp op) (Stack [x])
  | isBinaryOp op = Left EmptyStack2
  | otherwise = case op of
    Not -> Right (Stack [unaryOpA A.not A.not x])
    Length -> Right (Stack [S.length x])
    _ -> Left NYI
compute1 (AOp op) (Stack (x:y:xs)) = case op of
  Not -> Right (Stack (unaryOpA A.not A.not x:y:xs))
  Length -> Right (Stack (S.length x:y:xs))
  Divide -> fmap (Stack . (:xs)) (binOpD (/) y x)
  _ -> Left NYI
compute1 (AArrayInt x) (Stack s) = Right (Stack (ItemArrayInt x:s))
compute1 (AArrayDouble x) (Stack s) = Right (Stack (ItemArrayDouble x:s))
compute1 (AReduceOp op) (Stack (ItemArrayInt x:xs)) = Right (Stack $ ItemArrayInt (glyphReduceOp op x):xs)
compute1 _ (Stack _) = Left NYI

-- reduces are row-wise (first dimension) reductions
folds' :: (Array a -> b) -> Array a -> Array b
folds' f a = folds f [] a

-- reduces are row-wise (first dimension) reductions
fold0 :: (Array a -> b) -> Array a -> Array b
fold0 f a = folds f [0] a

equals :: (Eq a, Ring a) => Array a -> Array a
equals (x:|xs) = folds (bool zero one . all (==x)) [0] ((extracts [0] xs))

fold1 :: (Array a -> Array a -> Array a) -> Array a -> Array a
fold1 f (x:|xs) = go x xs
  where
    go x xs = let (x':|xs') = xs in bool (go (f x x') xs') x (null xs)

glyphReduceOp :: (Ord a, Ring a) => Glyph -> (Array a -> Array a)
glyphReduceOp Add = folds' sum
glyphReduceOp Equals = reduceBool (==)
-- glyphReduceOp Subtract = fold1 (-)
-- glyphReduceOp Divide = fold1 (/)
glyphReduceOp Multiply = folds' product
glyphReduceOp NotEquals = reduceBool (/=)
glyphReduceOp LessThan = reduceBool (<)
glyphReduceOp LessOrEqual = reduceBool (<=)
glyphReduceOp GreaterThan = reduceBool (>)
glyphReduceOp GreaterOrEqual = reduceBool (>=)
glyphReduceOp Minimum = fold1 min
glyphReduceOp Maximum = fold1 max
-- glyphReduceOp Modulus = fold1 mod
-- glyphReduceOp Power = fold1 (**)
-- glyphReduceOp Logarithm = fold1 log
glyphReduceOp _ = error "glyphReduceOp"

diff :: (a -> a -> b) -> Array a -> Array b
diff f a = liftR2_ f (drops' [1] a) (drops' [(P.negate 1)] a)

reduceBool :: (Ring b) => (a -> a -> Bool) -> Array a -> Array b
reduceBool f a = fmap (bool zero one . any id) (fold0 (Huihua.Parse.diff f) a)

interp_ :: [Assemble] -> Stack
interp_ as = foldr (\a s -> either (error . show) id (compute1 a s)) (Stack []) as

-- | compute a list of assembleds.
--
-- >>> interp (assemble' exPage1)
-- Right (Stack {stackList = [ItemArrayDouble 4.0]})
interp :: [Assemble] -> Either HuiHuaWarning Stack
interp as = foldr (>=>) pure (fmap compute1 as) (Stack [])

-- >>> sequence_ $ C.putStr <$> (ts <> ["\n"])
-- .,∶;∘¬±¯⌵√○⌊⌈⁅=≠&lt;≤&gt;≥+-×÷◿ⁿₙ↧↥∠⧻△⇡⊢⇌♭⋯⍉⍏⍖⊚⊛⊝□⊔≅⊟⊂⊏⊡↯↙↘↻◫▽⌕∊⊗/∧\∵≡∺⊞⊠⍥⊕⊜⍘⋅⊙∩⊃⊓⍜⍚⬚'?⍣⍤!⎋↬⚂ηπτ∞~_[]{}()¯@$"←|
allTheSymbols :: [ByteString]
allTheSymbols =  [".",",","\226\136\182",";","\226\136\152","\194\172","\194\177","\194\175","\226\140\181","\226\136\154","\226\151\139","\226\140\138","\226\140\136","\226\129\133","=","\226\137\160","&lt;","\226\137\164","&gt;","\226\137\165","+","-","\195\151","\195\183","\226\151\191","\226\129\191","\226\130\153","\226\134\167","\226\134\165","\226\136\160","\226\167\187","\226\150\179","\226\135\161","\226\138\162","\226\135\140","\226\153\173","\226\139\175","\226\141\137","\226\141\143","\226\141\150","\226\138\154","\226\138\155","\226\138\157","\226\150\161","\226\138\148","\226\137\133","\226\138\159","\226\138\130","\226\138\143","\226\138\161","\226\134\175","\226\134\153","\226\134\152","\226\134\187","\226\151\171","\226\150\189","\226\140\149","\226\136\138","\226\138\151","/","\226\136\167","\\","\226\136\181","\226\137\161","\226\136\186","\226\138\158","\226\138\160","\226\141\165","\226\138\149","\226\138\156","\226\141\152","\226\139\133","\226\138\153","\226\136\169","\226\138\131","\226\138\147","\226\141\156","\226\141\154","\226\172\154","'","?","\226\141\163","\226\141\164","!","\226\142\139","\226\134\172","\226\154\130","\206\183","\207\128","\207\132","\226\136\158","~","_","[","]","{","}","(",")","\194\175","@","$","\"","\226\134\144","|","#"]

exPage1 :: ByteString
exPage1 = encodeUtf8 [i|
[1 5 8 2]
/+. \# Sum
⧻∶  \# Length
÷   \# Divide
|]

exPage2 :: ByteString
exPage2 = encodeUtf8 [i|
2_3_4
/×. \# Product
⇡   \# Range
↯:  \# Reshape
|]

exPage3 :: ByteString
exPage3 = [i|
"Unabashedly I utilize arrays"
≠@ . \# Mask of non-spaces
⊜⊢   \# All first letters
|]
