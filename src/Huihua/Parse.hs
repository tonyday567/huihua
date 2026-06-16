{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Huihua.Parse where

import Circuit.Parser
import Circuit.Parser qualified as CP
import Control.Applicative as A
import Control.Monad
import Data.Bifunctor
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.Char (ord)
import Data.Function ((&))
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Harpie.Array (Array)
import Harpie.Array qualified as D
import Huihua.ArrayU
import Huihua.Glyphs
import Huihua.Stack as S
import Huihua.Warning
import Prettyprinter
import Prelude as P hiding (null)

data Token = StringToken ByteString | GlyphToken Glyph | DoubleToken Double | CharacterToken Char | NameToken ByteString | CommentToken ByteString | TypeToken deriving (Eq, Ord, Show)

type P = Parser Text Char

-- | Double token has precedence over duplicate
token :: P Token
token =
  (DoubleToken <$> double)
    CP.<|> (CharacterToken <$> (char '@' *> anyToken))
    CP.<|> (CommentToken <$> (encodeUtf8 <$> (char '#' *> takeRest)))
    CP.<|> (GlyphToken <$> glyphP)
    CP.<|> (StringToken <$> wrappedDq)
    CP.<|> (NameToken <$> (encodeUtf8 . T.pack <$> CP.some (satisfy isLatinLetter)))
    CP.<|> (TypeToken <$ string "type")

tokens :: P [Token]
tokens = CP.many (ws_ *> token) <* ws_

-- | Parse ByteString input via UTF-8 decode.
tokenize :: Text -> Either ByteString [[Token]]
tokenize t = case runParser (CP.many tokens) t of
  That _ -> Left "parse error"
  This a -> Right a
  These a _ -> Right a

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
aArray a = aArrayLeft *> (D.asArray <$> A.many a) <* aArrayRight

aArrayStrand :: Assembler Token a -> Assembler Token (Array a)
aArrayStrand a = fmap D.asArray . (:) <$> a <*> A.some (aStrand *> a)

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
aInstructions = A.many aInstruction

instructionize :: [Token] -> [Instruction]
instructionize ts = foldMap fst (assemble aInstructions ts)

-- |
-- >>> parseI exPage1
-- [IOp Divide,IOp Length,IOp Flip,IReduceOp Add,IOp Duplicate,IArray (UnsafeArray [4] [1.0,5.0,8.0,2.0])]
parseI :: Text -> [Instruction]
parseI t = parseT t & instructionize

-- |
-- >>> parseT exPage1
-- [GlyphToken Divide,GlyphToken Length,GlyphToken Flip,GlyphToken Reduce,GlyphToken Add,GlyphToken Duplicate,GlyphToken ArrayLeft,DoubleToken 1.0,DoubleToken 5.0,DoubleToken 8.0,DoubleToken 2.0,GlyphToken ArrayRight]
parseT :: Text -> [Token]
parseT t = T.lines t & fmap runParser_ & List.reverse & mconcat & filter (P.not . isComment)

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
run :: Text -> Doc ann
run t = either viaShow pretty (interpI (parseI t))

allTheSymbols :: [ByteString]
allTheSymbols = [".", ",", "\226\136\182", ";", "\226\136\152", "\194\172", "\194\177", "\194\175", "\226\140\181", "\226\136\154", "\226\151\139", "\226\140\138", "\226\140\136", "\226\129\133", "=", "\226\137\160", "&lt;", "\226\137\164", "&gt;", "\226\137\165", "+", "-", "\195\151", "\195\183", "\226\151\191", "\226\129\191", "\226\130\153", "\226\134\167", "\226\134\165", "\226\136\160", "\226\167\187", "\226\150\179", "\226\135\161", "\226\138\162", "\226\135\140", "\226\153\173", "\226\139\175", "\226\141\137", "\226\141\143", "\226\141\150", "\226\138\154", "\226\138\155", "\226\138\157", "\226\150\161", "\226\138\148", "\226\137\133", "\226\138\159", "\226\138\130", "\226\138\143", "\226\138\161", "\226\134\175", "\226\134\153", "\226\134\152", "\226\134\187", "\226\151\171", "\226\150\189", "\226\140\149", "\226\136\138", "\226\138\151", "/", "\226\136\167", "\\", "\226\136\181", "\226\137\161", "\226\136\186", "\226\138\158", "\226\138\160", "\226\141\165", "\226\138\149", "\226\138\156", "\226\141\152", "\226\139\133", "\226\138\153", "\226\136\169", "\226\138\131", "\226\138\147", "\226\141\156", "\226\141\154", "\226\172\154", "'", "?", "\226\141\163", "\226\141\164", "!", "\226\142\139", "\226\134\172", "\226\154\130", "\206\183", "\207\128", "\207\132", "\226\136\158", "~", "_", "[", "]", "{", "}", "(", ")", "\194\175", "@", "$", "\"", "\226\134\144", "|", "#"]

-- | Parse a glyph (operator/function symbol).
glyphP :: P Glyph
glyphP =
  (const Duplicate <$> string ".")
    CP.<|> (const Over <$> string ",")
    CP.<|> (const Flip <$> string ":")
    CP.<|> (const Pop <$> string "◌")
    CP.<|> (const On <$> string "⟜")
    CP.<|> (const By <$> string "⊸")
    CP.<|> (const Stack' <$> string "?")
    CP.<|> (const Trace <$> string "⸮")
    CP.<|> (const Dump <$> string "dump")
    CP.<|> (const Identity <$> string "∘")
    CP.<|> (const Gap <$> string "⋅")
    CP.<|> (const Dip <$> string "⊡")
    CP.<|> (const Both <$> string "∩")
    CP.<|> (const Fork <$> string "⊃")
    CP.<|> (const Bracket <$> string "⊣")
    CP.<|> (const Eta <$> string "η")
    CP.<|> (const Pi <$> string "π")
    CP.<|> (const Tau <$> string "τ")
    CP.<|> (const Infinity <$> string "∞")
    CP.<|> (const Not <$> string "¬")
    CP.<|> (const Sign <$> string "±")
    CP.<|> (const Negate <$> string "¯")
    CP.<|> (const AbsoluteValue <$> string "⌵")
    CP.<|> (const Sqrt <$> string "√")
    CP.<|> (const Sine <$> string "∿")
    CP.<|> (const Floor <$> string "⌊")
    CP.<|> (const Ceiling <$> string "⌈")
    CP.<|> (const Round <$> string "⁅")
    CP.<|> (const Equals <$> string "=")
    CP.<|> (const NotEquals <$> string "≠")
    CP.<|> (const LessThan <$> string "<")
    CP.<|> (const LessOrEqual <$> string "≤")
    CP.<|> (const GreaterThan <$> string ">")
    CP.<|> (const GreaterOrEqual <$> string "≥")
    CP.<|> (const Add <$> string "+")
    CP.<|> (const Subtract <$> string "-")
    CP.<|> (const Multiply <$> string "×")
    CP.<|> (const Divide <$> string "÷")
    CP.<|> (const Modulus <$> string "◿")
    CP.<|> (const Power <$> string "ⁿ")
    CP.<|> (const Logarithm <$> string "ₙ")
    CP.<|> (const Minimum <$> string "↧")
    CP.<|> (const Maximum <$> string "↥")
    CP.<|> (const Atangent <$> string "∠")
    CP.<|> (const Complex' <$> string "ℂ")
    CP.<|> (const Length <$> string "⧻")
    CP.<|> (const Shape <$> string "△")
    CP.<|> (const Range <$> string "⇡")
    CP.<|> (const First <$> string "⊢")
    CP.<|> (const Reverse <$> string "⇌")
    CP.<|> (const Deshape <$> string "♭")
    CP.<|> (const Fix <$> string "¤")
    CP.<|> (const Bits <$> string "⋯")
    CP.<|> (const Transpose <$> string "⍉")
    CP.<|> (const Rise <$> string "⍏")
    CP.<|> (const Fall <$> string "⍖")
    CP.<|> (const Where <$> string "⊚")
    CP.<|> (const Classify <$> string "⊛")
    CP.<|> (const Deduplicate <$> string "◴")
    CP.<|> (const Unique <$> string "◰")
    CP.<|> (const Box <$> string "▱")
    CP.<|> (const Match <$> string "≅")
    CP.<|> (const Couple <$> string "⊟")
    CP.<|> (const Join <$> string "⊂")
    CP.<|> (const Select <$> string "⊏")
    CP.<|> (const Pick <$> string "⊡")
    CP.<|> (const Reshape <$> string "⇯")
    CP.<|> (const Rerank <$> string "☇")
    CP.<|> (const Take <$> string "⇣")
    CP.<|> (const Drop <$> string "⇢")
    CP.<|> (const Rotate <$> string "⇳")
    CP.<|> (const Windows <$> string "◫")
    CP.<|> (const Keep <$> string "▽")
    CP.<|> (const Find <$> string "⌕")
    CP.<|> (const Mask <$> string "⦷")
    CP.<|> (const Member <$> string "∊")
    CP.<|> (const IndexOf <$> string "⊗")
    CP.<|> (const Coordinate <$> string "⟔")
    CP.<|> (const Each <$> string "∥")
    CP.<|> (const Rows <$> string "≡")
    CP.<|> (const Table <$> string "⊞")
    CP.<|> (const Inventory <$> string "⍚")
    CP.<|> (const Repeat <$> string "⍥")
    CP.<|> (const Do <$> string "⍢")
    CP.<|> (const Reduce <$> string "/")
    CP.<|> (const Fold <$> string "∧")
    CP.<|> (const Scan <$> string "\\")
    CP.<|> (const Group <$> string "⊕")
    CP.<|> (const Partition <$> string "⊜")
    CP.<|> (const Un <$> string "°")
    CP.<|> (const Setinv <$> string "setinv")
    CP.<|> (const Setund <$> string "setund")
    CP.<|> (const Under <$> string "⍘")
    CP.<|> (const Content <$> string "◇")
    CP.<|> (const Fill <$> string "⬚")
    CP.<|> (const Parse <$> string "⋸")
    CP.<|> (const Try <$> string "⍣")
    CP.<|> (const Assert <$> string "⍤")
    CP.<|> (const Random <$> string "⚂")
    CP.<|> (const Strand <$> string "_")
    CP.<|> (const ArrayLeft <$> string "[")
    CP.<|> (const ArrayRight <$> string "]")
    CP.<|> (const BoxArrayLeft <$> string "{")
    CP.<|> (const BoxArrayRight <$> string "}")
    CP.<|> (const FunctionLeft <$> string "(")
    CP.<|> (const FunctionRight <$> string ")")
    CP.<|> (const SwitchLeft <$> string "⟨")
    CP.<|> (const SwitchRight <$> string "⟩")
    CP.<|> (const Character <$> string "@")
    CP.<|> (const Format <$> string "$")
    CP.<|> (const String <$> string "\"")
    CP.<|> (const Macro <$> string "!")
    CP.<|> (const Placeholder <$> string "^")
    CP.<|> (const Binding <$> string "←")
    CP.<|> (const PrivateBinding <$> string "↚")
    CP.<|> (const Import' <$> string "~")
    CP.<|> (const Signature <$> string "|")
    CP.<|> (const Comment <$> string "#")

----------------------------------------------------------------------
-- Whitespace
----------------------------------------------------------------------

isWhitespace :: Char -> Bool
isWhitespace ' ' = True
isWhitespace '\t' = True
isWhitespace '\n' = True
isWhitespace '\r' = True
isWhitespace '\f' = True
isWhitespace _ = False

ws_ :: P ()
ws_ = skipMany (satisfy isWhitespace)

ws :: P Char
ws = satisfy isWhitespace

----------------------------------------------------------------------
-- Character predicates
----------------------------------------------------------------------

isLatinLetter :: Char -> Bool
isLatinLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

----------------------------------------------------------------------
-- Numeric parsers
----------------------------------------------------------------------

digit :: P Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

digits :: P (Int, Int)
digits = do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> CP.empty
    _ -> pure (place, n)

double :: P Double
double = do
  (placel, nl) <- digits
  withOption
    (char '.' *> digits)
    ( \(placer, nr) ->
        case placel of
          1 -> CP.empty
          _ -> pure $ fromIntegral nl + fromIntegral nr / fromIntegral placer
    )
    ( case placel of
        1 -> CP.empty
        _ -> pure $ fromIntegral nl
    )

----------------------------------------------------------------------
-- Quoted strings
----------------------------------------------------------------------

wrappedDq :: P ByteString
wrappedDq = encodeUtf8 . T.pack <$> (char '"' *> CP.many (satisfy (/= '"')) <* char '"')

----------------------------------------------------------------------
-- Parser runner (strict: must consume all input)
----------------------------------------------------------------------

runParser_ :: Text -> [Token]
runParser_ t = case runParser tokens t of
  These a _ -> a
  This a -> a
  That _ -> error "uncaught parse error"
