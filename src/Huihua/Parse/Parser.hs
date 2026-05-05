{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Internal parser implementation for huihua
-- Provides a drop-in replacement for FlatParse.Basic combinators
module Huihua.Parse.Parser
  ( -- * Core types
    Parser,
    Result (..),

    -- * Running parsers
    runParser,
    runParserMaybe,
    runParserEither,

    -- * String utilities
    strToUtf8,
    utf8ToStr,

    -- * Basic combinators
    satisfy,
    satisfyAscii,
    byteStringOf,

    -- * Repetition
    many,
    some,
    optional,
    skipMany,

    -- * Literals
    char,
    string,
    byteString,
    anyChar,
    takeRest,

    -- * Character predicates
    isDigit,
    isLatinLetter,
    isWhitespace,

    -- * Whitespace helpers
    ws_,
    ws,

    -- * Utilities
    withOption,
    chainr,
    withSpan,
    unsafeSpanToByteString,

    -- * Numeric parsers
    digit,
    digits,
    double,

    -- * Quoted string parsers
    wrappedDq,

    -- * Runner with error handling
    runParser_,

    -- * Alternative operators
    (<|>),
    empty,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord)
import Data.String (IsString)
import Data.Word (Word8)
import Control.Applicative hiding (many, some, optional)
import Control.Monad

-- | Parser type
-- 'e' is the error type (often ByteString)
-- 'a' is the result type
newtype Parser e a = Parser
  { runParser :: ByteString -> Result e a
  }

-- | Result of parsing
data Result e a
  = OK a ByteString      -- ^ Success: result and remaining input
  | Err e                -- ^ Explicit error
  | Fail                 -- ^ Uncaught failure
  deriving (Eq, Show)

instance Functor (Result e) where
  fmap f = \case
    OK a rest -> OK (f a) rest
    Err e -> Err e
    Fail -> Fail

instance Functor (Parser e) where
  fmap f (Parser p) = Parser $ \bs -> case p bs of
    OK a rest -> OK (f a) rest
    Err e -> Err e
    Fail -> Fail

instance Applicative (Parser e) where
  pure a = Parser $ \bs -> OK a bs

  Parser pf <*> Parser pa = Parser $ \bs -> case pf bs of
    OK f rest -> case pa rest of
      OK a rest' -> OK (f a) rest'
      Err e -> Err e
      Fail -> Fail
    Err e -> Err e
    Fail -> Fail

instance Monad (Parser e) where
  Parser p >>= f = Parser $ \bs -> case p bs of
    OK a rest -> runParser (f a) rest
    Err e -> Err e
    Fail -> Fail

instance Alternative (Parser e) where
  empty = Parser $ \_ -> Fail

  Parser p1 <|> Parser p2 = Parser $ \bs -> case p1 bs of
    Fail -> p2 bs
    other -> other

-- | Parse a single character satisfying a predicate
satisfy :: (Char -> Bool) -> Parser e Char
satisfy f = Parser $ \bs ->
  if B.null bs
    then Fail
    else
      let c = B.head bs
          ch = toEnum (fromEnum c) :: Char
       in if f ch
            then OK ch (B.tail bs)
            else Fail

-- | ASCII-only version of satisfy (for performance)
satisfyAscii :: (Char -> Bool) -> Parser e Char
satisfyAscii f = Parser $ \bs ->
  if B.null bs
    then Fail
    else
      let c = B.head bs
       in if c < 128
            then
              let ch = toEnum (fromEnum c) :: Char
               in if f ch
                    then OK ch (B.tail bs)
                    else Fail
            else Fail

-- | Parse any single character
anyChar :: Parser e Char
anyChar = satisfy (const True)

-- | Capture the ByteString consumed by a parser
byteStringOf :: Parser e a -> Parser e ByteString
byteStringOf (Parser p) = Parser $ \bs -> case p bs of
  OK _ rest ->
    let !consumed = B.take (B.length bs - B.length rest) bs
     in OK consumed rest
  Err e -> Err e
  Fail -> Fail

-- | Zero or more repetitions
many :: Parser e a -> Parser e [a]
many p = go []
  where
    go !acc = (do
      x <- p
      go (x : acc)) <|> pure (reverse acc)

-- | One or more repetitions
some :: Parser e a -> Parser e [a]
some p = (:) <$> p <*> many p

-- | Zero or one repetition
optional :: Parser e a -> Parser e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- | Skip zero or more repetitions (don't accumulate results)
skipMany :: Parser e a -> Parser e ()
skipMany p = go
  where
    go = (p >> go) <|> pure ()

-- | Parse a specific character
char :: Char -> Parser e ()
char c = satisfy (== c) >> pure ()

-- | Parse a specific string (uses ByteString internally)
string :: ByteString -> Parser e ()
string bs = Parser $ \input ->
  if bs `B.isPrefixOf` input
    then OK () (B.drop (B.length bs) input)
    else Fail

-- | Alias for string (for UTF-8 multi-byte support)
byteString :: ByteString -> Parser e ()
byteString = string

-- | Consume all remaining input
takeRest :: Parser e ByteString
takeRest = Parser $ \bs -> OK bs B.empty

-- ============================================================================
-- Character predicates
-- ============================================================================

-- | ASCII digit predicate
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
{-# INLINE isDigit #-}

-- | ASCII letter predicate
isLatinLetter :: Char -> Bool
isLatinLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
{-# INLINE isLatinLetter #-}

-- | Whitespace predicate: space, tab, newline, carriage return, form feed
isWhitespace :: Char -> Bool
isWhitespace ' ' = True
isWhitespace '\t' = True
isWhitespace '\n' = True
isWhitespace '\r' = True
isWhitespace '\f' = True
isWhitespace _ = False
{-# INLINE isWhitespace #-}

-- ============================================================================
-- Whitespace helpers
-- ============================================================================

-- | Consume zero or more whitespace characters
ws_ :: Parser e ()
ws_ = skipMany (satisfy isWhitespace)
{-# INLINE ws_ #-}

-- | Parse a single whitespace character
ws :: Parser e Char
ws = satisfy isWhitespace

-- ============================================================================
-- Combinator utilities
-- ============================================================================

-- | Try a parser with a fallback
withOption :: Parser e a -> (a -> Parser e b) -> Parser e b -> Parser e b
withOption p f def = (p >>= f) <|> def
{-# INLINE withOption #-}

-- | Right-fold chain combinator
chainr :: (a -> b -> b) -> Parser e a -> Parser e b -> Parser e b
chainr f p z = go
  where
    go = (f <$> p <*> go) <|> z
{-# INLINE chainr #-}

-- | Parse while tracking span/position (returns input, result, and remaining)
withSpan :: Parser e a -> Parser e (ByteString, a, ByteString)
withSpan (Parser p) = Parser $ \bs -> case p bs of
  OK a rest ->
    let !consumed = B.take (B.length bs - B.length rest) bs
     in OK (consumed, a, rest) rest
  Err e -> Err e
  Fail -> Fail

-- | Extract the span as a ByteString (for compatibility with FlatParse)
unsafeSpanToByteString :: (ByteString, a, ByteString) -> ByteString
unsafeSpanToByteString (span, _, _) = span

-- ============================================================================
-- Parser runners
-- ============================================================================

-- | Run a parser, returning Nothing on Fail or Err
runParserMaybe :: Parser e a -> ByteString -> Maybe a
runParserMaybe (Parser p) bs = case p bs of
  OK a _ -> Just a
  Fail -> Nothing
  Err _ -> Nothing

-- | Run a parser, returning Either with error on Fail or Err
runParserEither :: (IsString e) => Parser e a -> ByteString -> Either e a
runParserEither (Parser p) bs = case p bs of
  Err e -> Left e
  OK a _ -> Right a
  Fail -> Left "uncaught parse error"

-- ============================================================================
-- String/ByteString conversions
-- ============================================================================

-- | Convert String to UTF-8 ByteString
strToUtf8 :: String -> ByteString
strToUtf8 = B.pack . map (toEnum . fromEnum)

-- | Convert UTF-8 ByteString to String
utf8ToStr :: ByteString -> String
utf8ToStr = map (toEnum . fromEnum) . B.unpack

-- ============================================================================
-- Numeric parsers
-- ============================================================================

-- | A single digit (0-9) parsed as an Int
--
-- >>> runParserMaybe digit "5"
-- Just 5
digit :: Parser e Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

-- | Parse digits and return (multiplier for next place, accumulated number)
-- Internal helper for int/double parsing
digits :: Parser e (Int, Int)
digits = do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure (place, n)

-- | A 'Double' parser. Does not parse .1 as a double (must have leading digit).
--
-- >>> runParserMaybe double "1.234"
-- Just 1.234
--
-- >>> runParserMaybe double "123"
-- Just 123.0
double :: Parser e Double
double = do
  (placel, nl) <- digits
  withOption
    (char '.' *> digits)
    ( \(placer, nr) ->
        case placel of
          1 -> empty
          _ -> pure $ fromIntegral nl + fromIntegral nr / fromIntegral placer
    )
    ( case placel of
        1 -> empty
        _ -> pure $ fromIntegral nl
    )

-- ============================================================================
-- Quoted string parsers
-- ============================================================================

-- | Parse while not matching a specific character (helper for wrappedDq)
nota :: Char -> Parser e ByteString
nota c = unsafeSpanToByteString <$> withSpan (skipMany (satisfy (/= c)))
{-# INLINE nota #-}

-- | A double-quoted string (content between double quotes)
--
-- >>> runParserMaybe wrappedDq "\"hello\""
-- Just "hello"
wrappedDq :: Parser e ByteString
wrappedDq = char '"' *> nota '"' <* char '"'
{-# INLINE wrappedDq #-}

-- ============================================================================
-- Error-handling runner
-- ============================================================================

-- | Run parser, discard leftovers & throw an error on failure.
--
-- >>> runParser_ (char 'x') "x"
-- ()
--
-- >>> runParser_ (char 'x') "y"
-- *** Exception: uncaught parse error
-- ...
runParser_ :: Parser String a -> ByteString -> a
runParser_ p bs = case runParser p bs of
  Err e -> error e
  OK a "" -> a
  OK _ _ -> error "leftovers"
  Fail -> error "uncaught parse error"
