{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Huihua.Parse where

import Control.Applicative as A
import Control.Monad
import Data.Bifunctor
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.Function ((&))
import Data.List qualified as List
import FlatParse.Basic as FP
import Harry.Array (Array)
import Harry.Array qualified as D
import Huihua.ArrayU
import Huihua.Glyphs
import Huihua.Parse.FlatParse
import Huihua.Stack as S
import Huihua.Warning
import Prettyprinter
import Prelude as P hiding (null)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Huihua.Parse as P
-- >>> import Harry.Array as A
-- >>> import Data.List qualified as List
-- >>> import Huihua.Examples
-- >>> import Prettyprinter

data Token = StringToken ByteString | GlyphToken Glyph | DoubleToken Double | CharacterToken Char | NameToken String | CommentToken ByteString | TypeToken deriving (Eq, Ord, Show)

-- | Double token has precedence over duplicate
token :: Parser e Token
token =
  (DoubleToken <$> double)
    FP.<|> (CharacterToken <$> ($(char '@') *> anyChar))
    FP.<|> (CommentToken <$> ($(char '#') *> takeRest))
    FP.<|> (GlyphToken <$> glyphP)
    FP.<|> (StringToken <$> wrappedDq)
    FP.<|> (NameToken <$> some (satisfy isLatinLetter))
    FP.<|> (TypeToken <$ $(string "type"))

tokens :: Parser e [Token]
tokens = many (ws_ *> token) <* ws_

tokenize :: ByteString -> Either ByteString [[Token]]
tokenize bs = runParserEither (many tokens) bs

newtype Assembler t a = Assembler {assemble :: [t] -> Maybe (a, [t])} deriving (Functor)

instance Applicative (Assembler t) where
  pure a = Assembler (\xs -> Just (a, xs))

  f <*> a = Assembler $ \xs -> case assemble f xs of
    Nothing -> Nothing
    Just (f', xs') -> case assemble a xs' of
      Nothing -> Nothing
      Just (a', xs'') -> Just (f' a', xs'')

instance Alternative (Assembler t) where
  empty = Assembler (const Nothing)
  (<|>) a b = Assembler $ \xs -> case assemble a xs of
    Nothing -> assemble b xs
    Just x -> Just x

aOp :: Assembler Token Glyph
aOp = Assembler $ \case
  (GlyphToken g : xs) -> bool Nothing (Just (g, xs)) (isOperator g)
  _ -> Nothing

aNoOp :: Assembler Token Glyph
aNoOp = Assembler $ \case
  (GlyphToken g : xs) -> bool (Just (g, xs)) Nothing (P.not $ isOperator g)
  _ -> Nothing

aReduce :: Assembler Token ()
aReduce = Assembler $ \case
  (GlyphToken Reduce : xs) -> Just ((), xs)
  _ -> Nothing

aReduceOp :: Assembler Token Glyph
aReduceOp = Assembler $ \case
  (GlyphToken Reduce : GlyphToken g : xs) -> bool Nothing (Just (g, xs)) (isOperator g)
  _ -> Nothing

aComment :: Assembler Token ByteString
aComment = Assembler $ \case
  (CommentToken c : xs) -> Just (c, xs)
  _ -> Nothing

aDouble :: Assembler Token Double
aDouble = Assembler $ \case
  (DoubleToken d : xs) -> Just (d, xs)
  _ -> Nothing

aChar :: Assembler Token Char
aChar = Assembler $ \case
  (CharacterToken x : xs) -> Just (x, xs)
  _ -> Nothing

aString :: Assembler Token ByteString
aString = Assembler $ \case
  (StringToken x : xs) -> Just (x, xs)
  _ -> Nothing

aArrayRight :: Assembler Token ()
aArrayRight = Assembler $ \case
  (GlyphToken ArrayRight : xs) -> Just ((), xs)
  _ -> Nothing

aArrayLeft :: Assembler Token ()
aArrayLeft = Assembler $ \case
  (GlyphToken ArrayLeft : xs) -> Just ((), xs)
  _ -> Nothing

aStrand :: Assembler Token ()
aStrand = Assembler $ \case
  (GlyphToken Strand : xs) -> Just ((), xs)
  _ -> Nothing

aArray :: Assembler Token a -> Assembler Token (Array a)
aArray a = aArrayLeft *> (D.asArray <$> many a) <* aArrayRight

aArrayStrand :: Assembler Token a -> Assembler Token (Array a)
aArrayStrand a = fmap D.asArray . (:) <$> a <*> some (aStrand *> a)

aToken :: Assembler Token Token
aToken = Assembler $ \case
  (x : xs) -> Just (x, xs)
  _ -> Nothing

data Instruction = IOp Glyph | IReduceOp Glyph | WArray (Array Instruction) | IArray (Array Double) | INYI Token deriving (Show, Eq)

aInstruction :: Assembler Token Instruction
aInstruction =
  (IReduceOp <$> aReduceOp)
    A.<|> (IOp <$> aOp)
    A.<|> (IArray <$> aArray aDouble)
    A.<|> (IArray <$> aArrayStrand aDouble)
    A.<|> (WArray <$> aArray aInstruction)
    A.<|> (IArray . D.toScalar <$> aDouble)

aInstructions :: Assembler Token [Instruction]
aInstructions = many aInstruction

instructionize :: [Token] -> [Instruction]
instructionize ts = foldMap fst (assemble aInstructions ts)

-- |
-- >>> parseI exPage1
-- [IOp Divide,IOp Length,IOp Flip,IReduceOp Add,IOp Duplicate,IArray (UnsafeArray [4] [1.0,5.0,8.0,2.0])]
parseI :: ByteString -> [Instruction]
parseI bs = parseT bs & instructionize

-- |
-- >>> parseT exPage1
-- [GlyphToken Divide,GlyphToken Length,GlyphToken Flip,GlyphToken Reduce,GlyphToken Add,GlyphToken Duplicate,GlyphToken ArrayLeft,DoubleToken 1.0,DoubleToken 5.0,DoubleToken 8.0,DoubleToken 2.0,GlyphToken ArrayRight]
parseT :: ByteString -> [Token]
parseT bs = bs & C.lines & fmap (runParser_ tokens) & List.reverse & mconcat & filter (P.not . isComment)

isComment :: Token -> Bool
isComment (CommentToken _) = True
isComment _ = False

istep :: Instruction -> Stack -> Either HuihuaWarning Stack
istep (IOp op) s = applyOp op s
istep (IArray x) (Stack s) = Right (Stack (ArrayU x : s))
istep (WArray x) (Stack s) = second (Stack . (: s) . ArrayU) a
  where
    a = case interpI (D.arrayAs x) of
      (Right (Stack xs)) -> maybe (Left RaggedInternal) Right (D.joinSafe (D.asArray (fmap arrayd xs)))
      (Left w) -> Left w
istep (IReduceOp op) s = applyReduceOp op s
istep _ (Stack _) = Left NYI

-- | compute a list of instructions executing from right to left.
--
-- >>> interpI (parseI exPage1)
-- Right (Stack {stackList = [ArrayU {arrayd = UnsafeArray [] [4.0]}]})
interpI :: [Instruction] -> Either HuihuaWarning Stack
interpI as = foldr ((>=>) . istep) pure (List.reverse as) (Stack [])

-- |
--
-- >>> run exPage1
-- 4
run :: ByteString -> Doc ann
run bs = either viaShow pretty (interpI (parseI bs))

-- >>> sequence_ $ C.putStr <$> (ts <> ["\n"])
-- .,∶;∘¬±¯⌵√○⌊⌈⁅=≠&lt;≤&gt;≥+-×÷◿ⁿₙ↧↥∠⧻△⇡⊢⇌♭⋯⍉⍏⍖⊚⊛⊝□⊔≅⊟⊂⊏⊡↯↙↘↻◫▽⌕∊⊗/∧\∵≡∺⊞⊠⍥⊕⊜⍘⋅⊙∩⊃⊓⍜⍚⬚'?⍣⍤!⎋↬⚂ηπτ∞~_[]{}()¯@$"←|
allTheSymbols :: [ByteString]
allTheSymbols = [".", ",", "\226\136\182", ";", "\226\136\152", "\194\172", "\194\177", "\194\175", "\226\140\181", "\226\136\154", "\226\151\139", "\226\140\138", "\226\140\136", "\226\129\133", "=", "\226\137\160", "&lt;", "\226\137\164", "&gt;", "\226\137\165", "+", "-", "\195\151", "\195\183", "\226\151\191", "\226\129\191", "\226\130\153", "\226\134\167", "\226\134\165", "\226\136\160", "\226\167\187", "\226\150\179", "\226\135\161", "\226\138\162", "\226\135\140", "\226\153\173", "\226\139\175", "\226\141\137", "\226\141\143", "\226\141\150", "\226\138\154", "\226\138\155", "\226\138\157", "\226\150\161", "\226\138\148", "\226\137\133", "\226\138\159", "\226\138\130", "\226\138\143", "\226\138\161", "\226\134\175", "\226\134\153", "\226\134\152", "\226\134\187", "\226\151\171", "\226\150\189", "\226\140\149", "\226\136\138", "\226\138\151", "/", "\226\136\167", "\\", "\226\136\181", "\226\137\161", "\226\136\186", "\226\138\158", "\226\138\160", "\226\141\165", "\226\138\149", "\226\138\156", "\226\141\152", "\226\139\133", "\226\138\153", "\226\136\169", "\226\138\131", "\226\138\147", "\226\141\156", "\226\141\154", "\226\172\154", "'", "?", "\226\141\163", "\226\141\164", "!", "\226\142\139", "\226\134\172", "\226\154\130", "\206\183", "\207\128", "\207\132", "\226\136\158", "~", "_", "[", "]", "{", "}", "(", ")", "\194\175", "@", "$", "\"", "\226\134\144", "|", "#"]

glyphP :: Parser e Glyph
glyphP =
  $( switch
       [|
         case _ of
           "." -> pure Duplicate
           "," -> pure Over
           ":" -> pure Flip
           "◌" -> pure Pop
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
           "∿" -> pure Sine
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
           "ℂ" -> pure Complex'
           "⧻" -> pure Length
           "△" -> pure Shape
           "⇡" -> pure Range
           "⊢" -> pure First
           "⇌" -> pure Reverse
           "♭" -> pure Deshape
           "¤" -> pure Fix
           "⋯" -> pure Bits
           "⍉" -> pure Transpose
           "⍏" -> pure Rise
           "⍖" -> pure Fall
           "⊚" -> pure Where
           "⊛" -> pure Classify
           "◴" -> pure Deduplicate
           "◰" -> pure Unique
           "□" -> pure Box
           "≍" -> pure Match
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
         |]
   )
