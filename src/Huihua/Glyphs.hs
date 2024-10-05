{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Huihua.Glyphs where

import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Harpie.Array qualified as D
import Huihua.ArrayU as U
import Huihua.Stack
import Huihua.Warning
import Prelude as P hiding (null)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Huihua.Parse as P
-- >>> import Harpie.Array as A

data InputArity = Monadic | Dyadic deriving (Eq, Show)

data OutputArity = NoOutput | SingleOutput | DoubleOutput deriving (Eq, Show)

data Action = Pervasive | ArrayAction deriving (Eq, Show)

data Reducing = Reducing | NotReducing deriving (Eq, Show)

data Modifier = Iterating | Aggregating | Inversion | OtherModifier deriving (Eq, Show)

data StackOp = BasicStack | Planet deriving (Eq, Show)

data GlyphCategory = StackG StackOp | ConstantG | OperatorG InputArity OutputArity Action | ModifierG Modifier | MiscellaneousG | NonOperatorG deriving (Eq, Show)

data Glyph
  = -- StackG BasicStack
    Identity
  | Duplicate
  | Over
  | Flip
  | Pop
  | On
  | By
  | Stack'
  | Trace
  | Dump
  | -- StackG Planet
    Gap
  | Dip
  | Both
  | Fork
  | Bracket
  | -- ConstantG
    Eta
  | Pi
  | Tau
  | Infinity
  | -- OpertorG Monadic _ Pervasive
    Not
  | Sign
  | Negate
  | AbsoluteValue
  | Sqrt
  | Sine
  | Floor
  | Ceiling
  | Round
  | -- OperatorG Dyadic _ Pervasive
    Equals
  | NotEquals
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulus
  | Power
  | Logarithm
  | Minimum
  | Maximum
  | Atangent
  | Complex'
  | -- OperatorG Monadic _ ArrayAction
    Length
  | Shape
  | Range
  | First
  | Reverse
  | Deshape
  | Fix
  | Bits
  | Transpose
  | Rise
  | Fall
  | Where
  | Classify
  | Deduplicate
  | Unique
  | Box
  | -- OperatorG Monadic _ ArrayAction
    Match
  | Couple
  | Join
  | Select
  | Pick
  | Reshape
  | Rerank
  | Take
  | Drop
  | Rotate
  | Windows
  | Keep
  | Find
  | Mask
  | Member
  | IndexOf
  | Coordinate
  | -- ModifierG Iterating
    Each
  | Rows
  | Table
  | Inventory
  | Repeat
  | Do
  | -- ModifierG Aggregating
    Reduce
  | Fold
  | Scan
  | Group
  | Partition
  | -- ModifierG Inversion
    Un
  | Setinv
  | Setund
  | Under
  | -- ModifierG OtherModifier
    Content
  | Fill
  | -- MiscellaneousG
    Parse
  | Try
  | Assert
  | Random
  | -- NonOperatorG
    Strand
  | ArrayLeft
  | ArrayRight
  | BoxArrayLeft
  | BoxArrayRight
  | FunctionLeft
  | FunctionRight
  | SwitchLeft
  | SwitchRight
  | Negative
  | Character
  | Format
  | String
  | Macro
  | Placeholder
  | Binding
  | PrivateBinding
  | Import'
  | Signature
  | Comment
  deriving (Eq, Ord, Show)

data GlyphDeets = GlyphDeets {glyph :: Glyph, symbol :: ByteString, glyphCategory :: GlyphCategory}

glyphM :: Map.Map Glyph GlyphDeets
glyphM = Map.fromList $ zip (fmap glyph glyphs) glyphs

glyphs :: [GlyphDeets]
glyphs =
  [ -- StackG BasicStack
    GlyphDeets Identity "∘" (StackG BasicStack),
    GlyphDeets Duplicate "." (StackG BasicStack),
    GlyphDeets Over "," (StackG BasicStack),
    GlyphDeets Flip "∶" (StackG BasicStack),
    GlyphDeets Pop "◌" (StackG BasicStack),
    GlyphDeets On "⟜" (StackG BasicStack),
    GlyphDeets By "⊸" (StackG BasicStack),
    GlyphDeets Stack' "?" (StackG BasicStack),
    GlyphDeets Trace "⸮" (StackG BasicStack),
    GlyphDeets Dump "dump" (StackG BasicStack),
    -- StackG Planet
    GlyphDeets Gap "⋅" (StackG Planet),
    GlyphDeets Dip "⊙" (StackG Planet),
    GlyphDeets Both "∩" (StackG Planet),
    GlyphDeets Fork "⊃" (StackG Planet),
    GlyphDeets Bracket "⊓" (StackG Planet),
    -- ConstantG
    GlyphDeets Eta "η" ConstantG,
    GlyphDeets Pi "π" ConstantG,
    GlyphDeets Tau "τ" ConstantG,
    GlyphDeets Infinity "∞" ConstantG,
    -- OpertorG Monadic _ Pervasive
    GlyphDeets Not "¬" (OperatorG Monadic SingleOutput Pervasive),
    GlyphDeets Sign "±" (OperatorG Monadic SingleOutput Pervasive),
    GlyphDeets Negate "¯" (OperatorG Monadic SingleOutput Pervasive),
    GlyphDeets AbsoluteValue "⌵" (OperatorG Monadic SingleOutput Pervasive),
    GlyphDeets Sqrt "√" (OperatorG Monadic SingleOutput Pervasive),
    GlyphDeets Sine "○" (OperatorG Monadic SingleOutput Pervasive),
    GlyphDeets Floor "⌊" (OperatorG Monadic SingleOutput Pervasive),
    GlyphDeets Ceiling "⌈" (OperatorG Monadic SingleOutput Pervasive),
    GlyphDeets Round "⁅" (OperatorG Monadic SingleOutput Pervasive),
    -- OperatorG Dyadic _ Pervasive
    GlyphDeets Equals "=" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets NotEquals "≠" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets LessThan "&lt;" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets LessOrEqual "≤" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets GreaterThan "&gt;" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets GreaterOrEqual "≥" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets Add "+" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets Subtract "-" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets Multiply "×" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets Divide "÷" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets Modulus "◿" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets Power "ⁿ" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets Logarithm "ₙ" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets Minimum "↧" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets Maximum "↥" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets Atangent "∠" (OperatorG Dyadic SingleOutput Pervasive),
    GlyphDeets Complex' "ℂ" (OperatorG Dyadic SingleOutput Pervasive),
    -- OperatorG Monadic _ ArrayAction
    GlyphDeets Length "⧻" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Shape "△" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Range "⇡" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets First "⊢" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Reverse "⇌" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Deshape "♭" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Fix "¤" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Bits "⋯" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Transpose "⍉" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Rise "⍏" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Fall "⍖" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Where "⊚" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Classify "⊛" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Deduplicate "⊝" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Unique "◰" (OperatorG Monadic SingleOutput ArrayAction),
    GlyphDeets Box "□" (OperatorG Monadic SingleOutput ArrayAction),
    -- OperatorG Monadic _ ArrayAction
    GlyphDeets Match "≅" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Couple "⊟" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Join "⊂" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Select "⊏" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Pick "⊡" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Reshape "↯" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Rerank "☇" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Take "↙" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Drop "↘" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Rotate "↻" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Windows "◫" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Keep "▽" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Find "⌕" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Mask "⦷" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Member "∊" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets IndexOf "⊗" (OperatorG Dyadic SingleOutput ArrayAction),
    GlyphDeets Coordinate "⟔" (OperatorG Dyadic SingleOutput ArrayAction),
    -- ModifierG Iterating
    GlyphDeets Each "∵" (ModifierG Iterating),
    GlyphDeets Rows "≡" (ModifierG Iterating),
    GlyphDeets Table "⊞" (ModifierG Iterating),
    GlyphDeets Inventory "⍚" (ModifierG Iterating),
    GlyphDeets Repeat "⍥" (ModifierG Iterating),
    GlyphDeets Do "⍢" (ModifierG Iterating),
    -- ModifierG Aggregating
    GlyphDeets Reduce "/" (ModifierG Aggregating),
    GlyphDeets Fold "∧" (ModifierG Aggregating),
    GlyphDeets Scan "\\" (ModifierG Aggregating),
    GlyphDeets Group "⊕" (ModifierG Aggregating),
    GlyphDeets Partition "⊜" (ModifierG Aggregating),
    -- ModifierG Inversion
    GlyphDeets Un "°" (ModifierG Inversion),
    GlyphDeets Setinv "setinv" (ModifierG Inversion),
    GlyphDeets Setund "setund" (ModifierG Inversion),
    GlyphDeets Under "⍜" (ModifierG Inversion),
    -- ModifierG OtherModifier
    GlyphDeets Content "◇" (ModifierG OtherModifier),
    GlyphDeets Fill "⬚" (ModifierG OtherModifier),
    -- MiscellaneousG
    GlyphDeets Parse "⋕" MiscellaneousG,
    GlyphDeets Try "⍣" MiscellaneousG,
    GlyphDeets Assert "⍤" MiscellaneousG,
    GlyphDeets Random "⚂" MiscellaneousG,
    -- NonOperatorG
    GlyphDeets Strand "_" NonOperatorG,
    GlyphDeets ArrayLeft "[" NonOperatorG,
    GlyphDeets ArrayRight "]" NonOperatorG,
    GlyphDeets BoxArrayLeft "{" NonOperatorG,
    GlyphDeets BoxArrayRight "}" NonOperatorG,
    GlyphDeets FunctionLeft "(" NonOperatorG,
    GlyphDeets FunctionRight ")" NonOperatorG,
    GlyphDeets SwitchLeft "⟨" NonOperatorG,
    GlyphDeets SwitchRight "⟩" NonOperatorG,
    GlyphDeets Negative "¯" NonOperatorG,
    GlyphDeets Character "@" NonOperatorG,
    GlyphDeets Format "@" NonOperatorG,
    GlyphDeets String "$" NonOperatorG,
    GlyphDeets Macro "!" NonOperatorG,
    GlyphDeets Placeholder "^" NonOperatorG,
    GlyphDeets Binding "←" NonOperatorG,
    GlyphDeets PrivateBinding "↚" NonOperatorG,
    GlyphDeets Import' "~" NonOperatorG,
    GlyphDeets Signature "|" NonOperatorG,
    GlyphDeets Comment "#" NonOperatorG
  ]

isOperator :: Glyph -> Bool
isOperator g = case fmap glyphCategory (Map.lookup g glyphM) of
  (Just (OperatorG {})) -> True
  (Just (StackG BasicStack)) -> True
  (Just ConstantG) -> True
  _ -> False

isNonadicOp :: Glyph -> Bool
isNonadicOp Random = True
isNonadicOp g = Just ConstantG == fmap glyphCategory (Map.lookup g glyphM)

isMonadicOp :: Glyph -> Bool
isMonadicOp g = case fmap glyphCategory (Map.lookup g glyphM) of
  (Just (OperatorG Monadic _ _)) -> True
  _ -> False

isDyadicOp :: Glyph -> Bool
isDyadicOp g = case fmap glyphCategory (Map.lookup g glyphM) of
  (Just (OperatorG Dyadic _ _)) -> True
  _ -> False

isStackOp :: Glyph -> Bool
isStackOp g = case fmap glyphCategory (Map.lookup g glyphM) of
  (Just (StackG _)) -> True
  _ -> False

applyStack :: Glyph -> Stack -> Either HuihuaWarning Stack
applyStack Identity (Stack (x : xs)) = Right (Stack (x : xs))
applyStack Duplicate (Stack (x : xs)) = Right (Stack (x : x : xs))
applyStack Over (Stack (x : y : xs)) = Right (Stack (y : x : y : xs))
applyStack Flip (Stack (x : y : xs)) = Right (Stack (y : x : xs))
applyStack Pop (Stack (_ : xs)) = Right (Stack xs)
applyStack On _ = Left NYI
applyStack By _ = Left NYI
applyStack Stack' _ = Left NYI
applyStack Trace _ = Left NYI
applyStack Dump _ = Left NYI
applyStack Gap _ = Left NYI
applyStack Dip _ = Left NYI
applyStack Both _ = Left NYI
applyStack Fork _ = Left NYI
applyStack _ (Stack []) = Left EmptyStack1
applyStack _ (Stack [_]) = Left EmptyStack2

applyNonadic :: Glyph -> Res
applyNonadic Eta = Right $ pure $ ArrayU (D.toScalar $ 0.5 * pi)
applyNonadic Pi = Right $ pure $ ArrayU (D.toScalar pi)
applyNonadic Tau = Right $ pure $ ArrayU (D.toScalar $ 2 * pi)
applyNonadic Infinity = Right $ pure $ ArrayU (D.toScalar $ 1 / 0)
applyNonadic Random = Left NYI
applyNonadic _ = Left NYI

applyMonadic :: Glyph -> ArrayU -> Res
applyMonadic Not x = U.not x
applyMonadic Sign x = U.sign x
applyMonadic Negate x = U.negate' x
applyMonadic AbsoluteValue x = U.absoluteValue x
applyMonadic Sqrt x = U.sqrt x
applyMonadic Sine x = U.sine x
applyMonadic Floor x = U.floor x
applyMonadic Ceiling x = U.ceiling x
applyMonadic Round x = U.round x
applyMonadic Length x = U.length x
applyMonadic Shape x = U.shape x
applyMonadic Range x = U.range x
applyMonadic First x = U.first x
applyMonadic Reverse x = U.reverse x
applyMonadic Deshape x = U.deshape x
applyMonadic Bits x = U.bits x
applyMonadic Fix x = U.fix x
applyMonadic Transpose x = U.transpose x
applyMonadic Rise x = U.rise x
applyMonadic Fall x = U.fall x
applyMonadic Where x = U.where' x
applyMonadic Classify x = U.classify x
applyMonadic Deduplicate x = U.deduplicate x
applyMonadic Unique x = U.unique x
applyMonadic _ _ = Left NYI

applyDyadic :: Glyph -> ArrayU -> ArrayU -> Res
applyDyadic Equals x y = U.equals x y
applyDyadic NotEquals x y = U.notequals x y
applyDyadic LessThan x y = U.lessThan x y
applyDyadic LessOrEqual x y = U.lessOrEqual x y
applyDyadic GreaterThan x y = U.greaterThan x y
applyDyadic GreaterOrEqual x y = U.greaterOrEqual x y
applyDyadic Add x y = U.add x y
applyDyadic Subtract x y = U.subtract x y
applyDyadic Multiply x y = U.multiply x y
applyDyadic Divide x y = U.divide x y
applyDyadic Modulus x y = U.modulus x y
applyDyadic Power x y = U.power x y
applyDyadic Logarithm x y = U.logarithm x y
applyDyadic Minimum x y = U.minimum x y
applyDyadic Maximum x y = U.maximum x y
applyDyadic Atangent x y = U.atangent x y
applyDyadic Complex' _ _ = undefined
applyDyadic Match x y = U.match x y
applyDyadic Couple x y = U.couple x y
applyDyadic Join x y = U.join x y
applyDyadic Select x y = U.select x y
applyDyadic Pick x y = U.pick x y
applyDyadic Reshape x y = U.reshape x y
applyDyadic Rerank x y = U.rerank x y
applyDyadic Take x y = U.take x y
applyDyadic Drop x y = U.drop x y
applyDyadic Rotate x y = U.rotate x y
applyDyadic Windows x y = U.windows x y
applyDyadic Keep x y = U.keep x y
applyDyadic Find x y = U.find x y
applyDyadic Mask x y = U.mask x y
applyDyadic Member x y = U.member x y
applyDyadic IndexOf x y = U.indexOf x y
applyDyadic _ _ _ = Left NYI

pushRes :: [ArrayU] -> Res -> Either HuihuaWarning Stack
pushRes xs (Right rs) = Right (Stack (rs <> xs))
pushRes _ (Left e) = Left e

applyOp :: Glyph -> Stack -> Either HuihuaWarning Stack
applyOp g s
  | isStackOp g = applyStack g s
  | isNonadicOp g = applyNonadic g & pushRes (stackList s)
  | isMonadicOp g = case s of
      (Stack []) -> Left EmptyStack1
      (Stack (x : xs)) -> applyMonadic g x & pushRes xs
  | isDyadicOp g = case s of
      (Stack []) -> Left EmptyStack1
      (Stack [_]) -> Left EmptyStack2
      (Stack (x : y : xs)) -> applyDyadic g x y & pushRes xs
  | otherwise = Left ApplyNonOperator

applyReduceOp :: Glyph -> Stack -> Either HuihuaWarning Stack
applyReduceOp _ (Stack []) = Left EmptyStack1
applyReduceOp g (Stack (x : xs)) = reduceOp g x & pushRes xs

reduceOp :: Glyph -> ArrayU -> Res
reduceOp Equals x = U.equalsR x
reduceOp NotEquals x = U.notEqualsR x
reduceOp LessThan x = U.lessThanR x
reduceOp LessOrEqual x = U.lessOrEqualR x
reduceOp GreaterThan x = U.greaterThanR x
reduceOp GreaterOrEqual x = U.greaterOrEqualR x
reduceOp Add x = U.addR x
reduceOp Subtract x = U.subtractR x
reduceOp Multiply x = U.multiplyR x
reduceOp Divide x = U.divideR x
reduceOp Modulus x = U.modulusR x
reduceOp Power x = U.powerR x
reduceOp Logarithm x = U.logarithmR x
reduceOp Minimum x = U.minimumR x
reduceOp Maximum x = U.maximumR x
reduceOp _ _ = Left NYI
