{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Huihua.Parse where

import NumHask.Prelude as P hiding (First, null)
import FlatParse.Basic as FP
import Huihua.Parse.FlatParse
import Data.String.Interpolate
import Data.ByteString (ByteString)
import NumHask.Array.Dynamic
import Data.List qualified as List
import Huihua.Stack as S
import Huihua.ArrayU
import Huihua.Warning
import Data.ByteString.Char8 qualified as C
import Data.Text.Encoding (encodeUtf8)
import Control.Monad
import Prettyprinter
import Huihua.Glyphs

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Huihua.Parse as P
-- >>> import NumHask.Array.Dynamic as A
--

-- |
--
data Token = StringToken ByteString | GlyphToken Glyph | IntToken Int | DoubleToken Double | CharacterToken Char | NameToken String | CommentToken ByteString | TypeToken deriving (Eq, Ord, Show)

-- | Double token has precedence over duplicate
token :: Parser e Token
token =
  ((\x -> bool (DoubleToken x) (IntToken (P.floor x)) (x==(fromIntegral . P.floor) x)) <$> double) FP.<|>
  (GlyphToken <$> glyphP) FP.<|>
  (StringToken <$> wrappedDq) FP.<|>
  (CharacterToken <$> ($(char '@') *> anyChar)) FP.<|>
  (CommentToken <$> ($(char '#') *> nota '\n')) FP.<|>
  (NameToken <$> some (satisfy isLatinLetter)) FP.<|>
  (TypeToken <$ $(string "type"))

tokens :: Parser e [Token]
tokens = many (ws_ *> token) <* ws_

tokenize :: ByteString -> Either ByteString [[Token]]
tokenize bs = runParserEither (many tokens) bs

aOp :: Assembler Token Glyph
aOp = Assembler $ \xs -> case xs of
  (GlyphToken g:xs) -> bool Nothing (Just (g, xs)) (isOperator g)
  _ -> Nothing

aNoOp :: Assembler Token Glyph
aNoOp = Assembler $ \xs -> case xs of
  (GlyphToken g:xs) -> bool (Just (g, xs)) Nothing (P.not $ isOperator g)
  _ -> Nothing

aReduce :: Assembler Token ()
aReduce = Assembler $ \xs -> case xs of
  (GlyphToken Reduce:xs) -> Just ((), xs)
  _ -> Nothing

aReduceOp :: Assembler Token Glyph
aReduceOp = Assembler $ \xs -> case xs of
  (GlyphToken Reduce:GlyphToken g:xs) -> bool Nothing (Just (g, xs)) (isOperator g)
  _ -> Nothing

aComment :: Assembler Token ByteString
aComment = Assembler $ \xs -> case xs of
  (CommentToken c:xs) -> Just (c, xs)
  _ -> Nothing

aDouble :: Assembler Token Double
aDouble = Assembler $ \xs -> case xs of
  (DoubleToken d:xs) -> Just (d, xs)
  (IntToken i:xs) -> Just (fromIntegral i, xs)
  _ -> Nothing

aInt :: Assembler Token Int
aInt = Assembler $ \xs -> case xs of
  (IntToken i:xs) -> Just (i, xs)
  _ -> Nothing

aChar :: Assembler Token Char
aChar = Assembler $ \xs -> case xs of
  (CharacterToken x:xs) -> Just (x, xs)
  _ -> Nothing

aString :: Assembler Token ByteString
aString = Assembler $ \xs -> case xs of
  (StringToken x:xs) -> Just (x, xs)
  _ -> Nothing

aArrayRight :: Assembler Token ()
aArrayRight = Assembler $ \xs -> case xs of
  (GlyphToken ArrayRight:xs) -> Just ((), xs)
  _ -> Nothing

aArrayLeft :: Assembler Token ()
aArrayLeft = Assembler $ \xs -> case xs of
  (GlyphToken ArrayLeft:xs) -> Just ((), xs)
  _ -> Nothing

aArray :: Assembler Token a -> Assembler Token (Array a)
aArray a = aArrayLeft *> (fromList1 <$> many a) <* aArrayRight

aToken :: Assembler Token Token
aToken = Assembler $ \xs -> case xs of
  (x:xs) -> Just (x, xs)
  _ -> Nothing

data Instruction = IOp Glyph | IReduceOp Glyph | IArrayI (Array Int) | IArrayD (Array Double) | INYI Token deriving (Show, Eq)

aInstruction :: Assembler Token Instruction
aInstruction =
  (IReduceOp <$> aReduceOp) P.<|>
  (IOp <$> aOp) P.<|>
  (IArrayI <$> aArray aInt) P.<|>
  (IArrayD <$> aArray aDouble) P.<|>
  (INYI <$> aToken)

aInstructions :: Assembler Token [Instruction]
aInstructions = many aInstruction

instructionize :: [Token] -> [Instruction]
instructionize ts = fromMaybe [] (fmap fst (assemble aInstructions ts))

parseI :: ByteString -> [Instruction]
parseI bs = bs & C.lines & fmap (runParser_ tokens) & orderUiua & instructionize

parseT :: ByteString -> [Token]
parseT bs = bs & C.lines & fmap (runParser_ tokens) & orderUiua

isComment :: Token -> Bool
isComment (CommentToken _) = True
isComment _ = False

orderUiua :: [[Token]] -> [Token]
orderUiua tss = tss & List.reverse & mconcat & List.filter (P.not . isComment)

newtype Assembler t a = Assembler { assemble :: [t] -> Maybe (a, [t]) } deriving (Functor)

instance Applicative (Assembler t) where
  pure a = Assembler (\xs -> Just (a,xs))

  f <*> a = Assembler $ \xs -> case assemble f xs of
    Nothing -> Nothing
    Just (f', xs') -> case (assemble a) xs' of
      Nothing -> Nothing
      Just (a', xs'') -> Just (f' a', xs'')

instance Alternative (Assembler t) where
  empty = Assembler (const Nothing)
  (<|>) a b = Assembler $ \xs -> case assemble a xs of
    Nothing -> assemble b xs
    Just x -> Just x

istep :: Instruction -> Stack -> Either HuihuaWarning Stack
istep (IOp op) s = applyOp op s
istep (IArrayI x) (Stack s) = Right (Stack (ArrayI x:s))
istep (IArrayD x) (Stack s) = Right (Stack (ArrayD x:s))
-- istep (IReduceOp op) (Stack (ArrayI x:xs)) = Right (Stack $ ArrayI undefined (glyphReduceOp op x):xs)
istep _ (Stack _) = Left NYI

-- | compute a list of instructions.
--
-- >>> interpI (parseI exPage1)
-- Right (Stack {stackList = [ItemArrayDouble 4.0]})
interpI :: [Instruction] -> Either HuihuaWarning Stack
interpI as = foldr (>=>) pure (fmap istep (List.reverse as)) (Stack [])

-- | compute a list of instructions.
--
-- >>> interpI (parseI exPage1)
-- Right (Stack {stackList = [ItemArrayDouble 4.0]})
interpI_ :: [Instruction] -> Stack
interpI_ = either (error . show) id . interpI

-- |
--
-- >>> run exPage1
-- 4.0
run :: ByteString -> Doc ann
run bs = either viaShow pretty (interpI (parseI bs))

-- >>> sequence_ $ C.putStr <$> (ts <> ["\n"])
-- .,∶;∘¬±¯⌵√○⌊⌈⁅=≠&lt;≤&gt;≥+-×÷◿ⁿₙ↧↥∠⧻△⇡⊢⇌♭⋯⍉⍏⍖⊚⊛⊝□⊔≅⊟⊂⊏⊡↯↙↘↻◫▽⌕∊⊗/∧\∵≡∺⊞⊠⍥⊕⊜⍘⋅⊙∩⊃⊓⍜⍚⬚'?⍣⍤!⎋↬⚂ηπτ∞~_[]{}()¯@$"←|
allTheSymbols :: [ByteString]
allTheSymbols =  [".",",","\226\136\182",";","\226\136\152","\194\172","\194\177","\194\175","\226\140\181","\226\136\154","\226\151\139","\226\140\138","\226\140\136","\226\129\133","=","\226\137\160","&lt;","\226\137\164","&gt;","\226\137\165","+","-","\195\151","\195\183","\226\151\191","\226\129\191","\226\130\153","\226\134\167","\226\134\165","\226\136\160","\226\167\187","\226\150\179","\226\135\161","\226\138\162","\226\135\140","\226\153\173","\226\139\175","\226\141\137","\226\141\143","\226\141\150","\226\138\154","\226\138\155","\226\138\157","\226\150\161","\226\138\148","\226\137\133","\226\138\159","\226\138\130","\226\138\143","\226\138\161","\226\134\175","\226\134\153","\226\134\152","\226\134\187","\226\151\171","\226\150\189","\226\140\149","\226\136\138","\226\138\151","/","\226\136\167","\\","\226\136\181","\226\137\161","\226\136\186","\226\138\158","\226\138\160","\226\141\165","\226\138\149","\226\138\156","\226\141\152","\226\139\133","\226\138\153","\226\136\169","\226\138\131","\226\138\147","\226\141\156","\226\141\154","\226\172\154","'","?","\226\141\163","\226\141\164","!","\226\142\139","\226\134\172","\226\154\130","\206\183","\207\128","\207\132","\226\136\158","~","_","[","]","{","}","(",")","\194\175","@","$","\"","\226\134\144","|","#"]

-- |
--
-- >>> either error id (interp (assemble' exPage1))
--
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

-- |
--
-- >>> 1
glyphP :: Parser e Glyph
glyphP =
  $( switch
       [|
         case _ of
            "." -> pure Duplicate
            "," -> pure Over
            "∶" -> pure Flip
            ";" -> pure Pop
            "⟜" -> pure On
            "⊸" -> pure By
            "?" -> pure Stack'
            "⸮" -> pure Trace
            "dump" -> pure Dump
            "∘" -> pure Identity
            "⋅" -> pure Gap
            "⊙" -> pure Dip
            "∩" -> pure Both
            "⊃" -> pure Fork
            "⊓" -> pure Bracket
            "η" -> pure Eta
            "π" -> pure Pi
            "τ" -> pure Tau
            "∞" -> pure Infinity
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
            "ℂ" -> pure Complex'
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
            "◰" -> pure Unique
            "□" -> pure Box
            "≅" -> pure Match
            "⊟" -> pure Couple
            "⊂" -> pure Join
            "⊏" -> pure Select
            "⊡" -> pure Pick
            "↯" -> pure Reshape
            "☇" -> pure Rerank
            "↙" -> pure Take
            "↘" -> pure Drop
            "↻" -> pure Rotate
            "◫" -> pure Windows
            "▽" -> pure Keep
            "⌕" -> pure Find
            "⦷" -> pure Mask
            "∊" -> pure Member
            "⊗" -> pure IndexOf
            "⟔" -> pure Coordinate
            "∵" -> pure Each
            "≡" -> pure Rows
            "⊞" -> pure Table
            "⍚" -> pure Inventory
            "⍥" -> pure Repeat
            "⍢" -> pure Do
            "/" -> pure Reduce
            "∧" -> pure Fold
            "\\" -> pure Scan
            "⊕" -> pure Group
            "⊜" -> pure Partition
            "°" -> pure Un
            "setinv" -> pure Setinv
            "setund" -> pure Setund
            "⍜" -> pure Under
            "◇" -> pure Content
            "⬚" -> pure Fill
            "⋕" -> pure Parse
            "⍣" -> pure Try
            "⍤" -> pure Assert
            "⚂" -> pure Random
            "_" -> pure Strand
            "[" -> pure ArrayLeft
            "]" -> pure ArrayRight
            "{" -> pure BoxArrayLeft
            "}" -> pure BoxArrayRight
            "(" -> pure FunctionLeft
            ")" -> pure FunctionRight
            "⟨" -> pure SwitchLeft
            "⟩" -> pure SwitchRight
            -- "¯" -> pure Negative
            "@" -> pure Character
            "$" -> pure Format
            "\"" -> pure String
            "!" -> pure Macro
            "^" -> pure Placeholder
            "←" -> pure Binding
            "↚" -> pure PrivateBinding
            "~" -> pure Import'
            "|" -> pure Signature
            "#" -> pure Comment
           |])
