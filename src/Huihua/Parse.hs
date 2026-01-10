{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Harpie.Array (Array)
import Harpie.Array qualified as D
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
-- >>> import Harpie.Array as A
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

-- | Parse a glyph (operator/function symbol).
--
-- __Note on UTF-8 limitation:__ This parser uses FlatParse's @switch@ combinator,
-- which compiles string literals into an optimized byte-level trie. This works
-- correctly for single-byte ASCII glyphs (e.g., @"."@, @"+"@), but has a
-- known limitation with multi-byte UTF-8 sequences.
--
-- Multi-byte UTF-8 glyphs (e.g., @"◌"@ (U+9676), @"∘"@ (U+2218)) fail to match
-- at runtime, even when encoded correctly as UTF-8 octal escapes in the source.
-- See <https://github.com/tonyday567/huihua/issues/> for investigation details.
--
-- __Workaround:__ Currently, doctests using multi-byte glyphs in expressions
-- cannot be validated. These are marked as incomplete examples pending a fix
-- to the parser implementation.
--
-- __Future fix:__ Consider replacing @switch@ with a manual ByteString matching
-- function that understands UTF-8 encoding, or migrating to a parser combinator
-- library with built-in UTF-8 support.
glyphP :: Parser e Glyph
glyphP =
  $( switch
       [|
         case _ of
           "." -> pure Duplicate
           "," -> pure Over
           ":" -> pure Flip
           "\342\227\214" -> pure Pop
           "\342\237\234" -> pure On
           "\342\212\270" -> pure By
           "?" -> pure Stack'
           "\342\270\256" -> pure Trace
           "dump" -> pure Dump
           "\342\210\230" -> pure Identity
           "\342\213\205" -> pure Gap
           "\342\212\231" -> pure Dip
           "\342\210\251" -> pure Both
           "\342\212\203" -> pure Fork
           "\342\212\223" -> pure Bracket
           "\316\267" -> pure Eta
           "\317\200" -> pure Pi
           "\317\204" -> pure Tau
           "\342\210\236" -> pure Infinity
           "\302\254" -> pure Not
           "\302\261" -> pure Sign
           "\302\257" -> pure Negate
           "\342\214\265" -> pure AbsoluteValue
           "\342\210\232" -> pure Sqrt
           "\342\210\277" -> pure Sine
           "\342\214\212" -> pure Floor
           "\342\214\210" -> pure Ceiling
           "\342\201\205" -> pure Round
           "=" -> pure Equals
           "\342\211\240" -> pure NotEquals
           "<" -> pure LessThan
           "\342\211\244" -> pure LessOrEqual
           ">" -> pure GreaterThan
           "\342\211\245" -> pure GreaterOrEqual
           "+" -> pure Add
           "-" -> pure Subtract
           "\303\227" -> pure Multiply
           "\303\267" -> pure Divide
           "\342\227\277" -> pure Modulus
           "\342\201\277" -> pure Power
           "\342\202\231" -> pure Logarithm
           "\342\206\247" -> pure Minimum
           "\342\206\245" -> pure Maximum
           "\342\210\240" -> pure Atangent
           "\342\204\202" -> pure Complex'
           "\342\247\273" -> pure Length
           "\342\226\263" -> pure Shape
           "\342\207\241" -> pure Range
           "\342\212\242" -> pure First
           "\342\207\214" -> pure Reverse
           "\342\231\255" -> pure Deshape
           "\302\244" -> pure Fix
           "\342\213\257" -> pure Bits
           "\342\215\211" -> pure Transpose
           "\342\215\217" -> pure Rise
           "\342\215\226" -> pure Fall
           "\342\212\232" -> pure Where
           "\342\212\233" -> pure Classify
           "\342\227\264" -> pure Deduplicate
           "\342\227\260" -> pure Unique
           "\342\226\241" -> pure Box
           "\342\211\215" -> pure Match
           "\342\212\237" -> pure Couple
           "\342\212\202" -> pure Join
           "\342\212\217" -> pure Select
           "\342\212\241" -> pure Pick
           "\342\206\257" -> pure Reshape
           "\342\230\207" -> pure Rerank
           "\342\206\231" -> pure Take
           "\342\206\230" -> pure Drop
           "\342\206\273" -> pure Rotate
           "\342\227\253" -> pure Windows
           "\342\226\275" -> pure Keep
           "\342\214\225" -> pure Find
           "\342\246\267" -> pure Mask
           "\342\210\212" -> pure Member
           "\342\212\227" -> pure IndexOf
           "\342\237\224" -> pure Coordinate
           "\342\210\265" -> pure Each
           "\342\211\241" -> pure Rows
           "\342\212\236" -> pure Table
           "\342\215\232" -> pure Inventory
           "\342\215\245" -> pure Repeat
           "\342\215\242" -> pure Do
           "/" -> pure Reduce
           "\342\210\247" -> pure Fold
           "\\" -> pure Scan
           "\342\212\225" -> pure Group
           "\342\212\234" -> pure Partition
           "\302\260" -> pure Un
           "setinv" -> pure Setinv
           "setund" -> pure Setund
           "\342\215\234" -> pure Under
           "\342\227\207" -> pure Content
           "\342\254\232" -> pure Fill
           "\342\213\225" -> pure Parse
           "\342\215\243" -> pure Try
           "\342\215\244" -> pure Assert
           "\342\232\202" -> pure Random
           "_" -> pure Strand
           "[" -> pure ArrayLeft
           "]" -> pure ArrayRight
           "{" -> pure BoxArrayLeft
           "}" -> pure BoxArrayRight
           "(" -> pure FunctionLeft
           ")" -> pure FunctionRight
           "\342\237\250" -> pure SwitchLeft
           "\342\237\251" -> pure SwitchRight
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
