{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Huihua.Glyphs where

import Data.Map.Strict qualified as Map
import NumHask.Prelude as P hiding (First, null, diff)
import Data.ByteString (ByteString)
import NumHask.Array.Dynamic
import Huihua.Warning
import Huihua.Array as A
import Huihua.ArrayU as U
import Huihua.Stack

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Huihua.Parse as P
-- >>> import NumHask.Array.Dynamic as A
--

data InputArity = Monadic | Dyadic deriving (Eq, Show)
data OutputArity = NoOutput | SingleOutput | DoubleOutput deriving (Eq, Show)
data Action = Pervasive | ArrayAction deriving (Eq, Show)
data Reducing = Reducing | NotReducing deriving (Eq, Show)
data Modifier = Iterating | Aggregating | Inversion | OtherModifier deriving (Eq, Show)
data StackOp = BasicStack | Planet deriving (Eq, Show)
data GlyphCategory = StackG StackOp | ConstantG | OperatorG InputArity OutputArity Action | ModifierG Modifier | MiscellaneousG | NonOperatorG deriving (Eq, Show)

-- |
--
data Glyph =
  -- StackG BasicStack
  Duplicate |
  Over |
  Flip |
  Pop |
  On |
  By |
  Stack' |
  Trace |
  Dump |

  -- StackG Planet
  Identity |
  Gap |
  Dip |
  Both |
  Fork |
  Bracket |

  -- ConstantG
  Eta |
  Pi |
  Tau |
  Infinity |

  -- OpertorG Monadic _ Pervasive
  Not |
  Sign |
  Negate |
  AbsoluteValue |
  Sqrt |
  Sine |
  Floor |
  Ceiling |
  Round |

  -- OperatorG Dyadic _ Pervasive
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
  Complex' |

  -- OperatorG Monadic _ ArrayAction
  Length |
  Shape |
  Range |
  First |
  Reverse |
  Deshape |
  Fix |
  Bits |
  Transpose |
  Rise |
  Fall |
  Where |
  Classify |
  Deduplicate |
  Unique |
  Box |

  -- OperatorG Monadic _ ArrayAction
  Match |
  Couple |
  Join |
  Select |
  Pick |
  Reshape |
  Rerank |
  Take |
  Drop |
  Rotate |
  Windows |
  Keep |
  Find |
  Mask |
  Member |
  IndexOf |
  Coordinate |

  -- ModifierG Iterating
  Each |
  Rows |
  Table |
  Inventory |
  Repeat |
  Do |

  -- ModifierG Aggregating
  Reduce |
  Fold |
  Scan |
  Group |
  Partition |

  -- ModifierG Inversion
  Un |
  Setinv |
  Setund |
  Under |

  -- ModifierG OtherModifier
  Content |
  Fill |

  -- MiscellaneousG
  Parse |
  Try |
  Assert |
  Random |

  -- NonOperatorG
  Strand |
  ArrayLeft |
  ArrayRight |
  BoxArrayLeft |
  BoxArrayRight |
  FunctionLeft |
  FunctionRight |
  SwitchLeft |
  SwitchRight |
  Negative |
  Character |
  Format |
  String |
  Macro |
  Placeholder |
  Binding |
  PrivateBinding |
  Import' |
  Signature |
  Comment
  deriving (Eq, Ord, Show)

data GlyphDeets = GlyphDeets { glyph :: Glyph, symbol :: ByteString, glyphCategory :: GlyphCategory }

glyphM :: Map.Map Glyph GlyphDeets
glyphM = fromList $ zip (fmap glyph glyphs) glyphs

glyphs :: [GlyphDeets]
glyphs =
  [
  -- StackG BasicStack
    GlyphDeets Duplicate "." (StackG BasicStack)
  , GlyphDeets Over "," (StackG BasicStack)
  , GlyphDeets Flip "∶" (StackG BasicStack)
  , GlyphDeets Pop "◌" (StackG BasicStack)
  , GlyphDeets On "⟜" (StackG BasicStack)
  , GlyphDeets By "⊸" (StackG BasicStack)
  , GlyphDeets Stack' "?" (StackG BasicStack)
  , GlyphDeets Trace "⸮" (StackG BasicStack)
  , GlyphDeets Dump "dump" (StackG BasicStack)

  -- StackG Planet
  , GlyphDeets Identity "∘" (StackG Planet)
  , GlyphDeets Gap "⋅" (StackG Planet)
  , GlyphDeets Dip "⊙" (StackG Planet)
  , GlyphDeets Both "∩" (StackG Planet)
  , GlyphDeets Fork "⊃" (StackG Planet)
  , GlyphDeets Bracket "⊓" (StackG Planet)

  -- ConstantG
  , GlyphDeets Eta "η" ConstantG
  , GlyphDeets Pi "π" ConstantG
  , GlyphDeets Tau "τ" ConstantG
  , GlyphDeets Infinity "∞" ConstantG

  -- OpertorG Monadic _ Pervasive
  , GlyphDeets Not "¬" (OperatorG Monadic SingleOutput Pervasive)
  , GlyphDeets Sign "±" (OperatorG Monadic SingleOutput Pervasive)
  , GlyphDeets Negate "¯" (OperatorG Monadic SingleOutput Pervasive)
  , GlyphDeets AbsoluteValue "⌵" (OperatorG Monadic SingleOutput Pervasive)
  , GlyphDeets Sqrt "√" (OperatorG Monadic SingleOutput Pervasive)
  , GlyphDeets Sine "○" (OperatorG Monadic SingleOutput Pervasive)
  , GlyphDeets Floor "⌊" (OperatorG Monadic SingleOutput Pervasive)
  , GlyphDeets Ceiling "⌈" (OperatorG Monadic SingleOutput Pervasive)
  , GlyphDeets Round "⁅" (OperatorG Monadic SingleOutput Pervasive)

  -- OperatorG Dyadic _ Pervasive
  , GlyphDeets Equals "=" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets NotEquals "≠" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets LessThan "&lt;" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets LessOrEqual "≤" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets GreaterThan "&gt;" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets GreaterOrEqual "≥" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets Add "+" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets Subtract "-" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets Multiply "×" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets Divide "÷" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets Modulus "◿" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets Power "ⁿ" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets Logarithm "ₙ" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets Minimum "↧" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets Maximum "↥" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets Atangent "∠" (OperatorG Dyadic SingleOutput Pervasive)
  , GlyphDeets Complex' "ℂ" (OperatorG Dyadic SingleOutput Pervasive)

  -- OperatorG Monadic _ ArrayAction
  , GlyphDeets Length "⧻" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets Shape "△" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets Range "⇡" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets First "⊢" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets Reverse "⇌" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets Deshape "♭" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets Bits "⋯" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets Transpose "⍉" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets Rise "⍏" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets Fall "⍖" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets Where "⊚" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets Classify "⊛" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets Deduplicate "⊝" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets Unique "◰" (OperatorG Monadic SingleOutput ArrayAction)
  , GlyphDeets Box "□" (OperatorG Monadic SingleOutput ArrayAction)

  -- OperatorG Monadic _ ArrayAction
  , GlyphDeets Match "≅" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Couple "⊟" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Join "⊂" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Select "⊏" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Pick "⊡" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Reshape "↯" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Rerank "☇" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Take "↙" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Drop "↘" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Rotate "↻" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Windows "◫" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Keep "▽" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Find "⌕" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Mask "⦷" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Member "∊" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets IndexOf "⊗" (OperatorG Dyadic SingleOutput ArrayAction)
  , GlyphDeets Coordinate "⟔" (OperatorG Dyadic SingleOutput ArrayAction)

  -- ModifierG Iterating
  , GlyphDeets Each "∵" (ModifierG Iterating)
  , GlyphDeets Rows "≡" (ModifierG Iterating)
  , GlyphDeets Table "⊞" (ModifierG Iterating)
  , GlyphDeets Inventory "⍚" (ModifierG Iterating)
  , GlyphDeets Repeat "⍥" (ModifierG Iterating)
  , GlyphDeets Do "⍢" (ModifierG Iterating)

  -- ModifierG Aggregating
  , GlyphDeets Reduce "/" (ModifierG Aggregating)
  , GlyphDeets Fold "∧" (ModifierG Aggregating)
  , GlyphDeets Scan "\\" (ModifierG Aggregating)
  , GlyphDeets Group "⊕" (ModifierG Aggregating)
  , GlyphDeets Partition "⊜" (ModifierG Aggregating)

  -- ModifierG Inversion
  , GlyphDeets Un "°" (ModifierG Inversion)
  , GlyphDeets Setinv "setinv" (ModifierG Inversion)
  , GlyphDeets Setund "setund" (ModifierG Inversion)
  , GlyphDeets Under "⍜" (ModifierG Inversion)

  -- ModifierG OtherModifier
  , GlyphDeets Content "◇" (ModifierG OtherModifier)
  , GlyphDeets Fill "⬚" (ModifierG OtherModifier)

  -- MiscellaneousG
  , GlyphDeets Parse "⋕" MiscellaneousG
  , GlyphDeets Try "⍣" MiscellaneousG
  , GlyphDeets Assert "⍤" MiscellaneousG
  , GlyphDeets Random "⚂" MiscellaneousG

  -- NonOperatorG
  , GlyphDeets Strand "_" NonOperatorG
  , GlyphDeets ArrayLeft "[" NonOperatorG
  , GlyphDeets ArrayRight "]" NonOperatorG
  , GlyphDeets BoxArrayLeft "{" NonOperatorG
  , GlyphDeets BoxArrayRight "}" NonOperatorG
  , GlyphDeets FunctionLeft "(" NonOperatorG
  , GlyphDeets FunctionRight ")" NonOperatorG
  , GlyphDeets SwitchLeft "⟨" NonOperatorG
  , GlyphDeets SwitchRight "⟩" NonOperatorG
  , GlyphDeets Negative "¯" NonOperatorG
  , GlyphDeets Character "@" NonOperatorG
  , GlyphDeets Format "@" NonOperatorG
  , GlyphDeets String "$" NonOperatorG
  , GlyphDeets Macro "!" NonOperatorG
  , GlyphDeets Placeholder "^" NonOperatorG
  , GlyphDeets Binding "←" NonOperatorG
  , GlyphDeets PrivateBinding "↚" NonOperatorG
  , GlyphDeets Import' "~" NonOperatorG
  , GlyphDeets Signature "|" NonOperatorG
  , GlyphDeets Comment "#" NonOperatorG
  ]

isOperator :: Glyph -> Bool
isOperator g = case fmap glyphCategory (Map.lookup g glyphM) of
  (Just (OperatorG _ _ _)) -> True
  (Just ConstantG) -> True
  _ -> False

isNonadicOp :: Glyph -> Bool
isNonadicOp Random = True
isNonadicOp g = (Just ConstantG) == fmap glyphCategory (Map.lookup g glyphM)

isMonadicOp :: Glyph -> Bool
isMonadicOp g = case fmap glyphCategory (Map.lookup g glyphM) of
  (Just (OperatorG Monadic _ _)) -> True
  _ -> False

isDyadicOp :: Glyph -> Bool
isDyadicOp g = case fmap glyphCategory (Map.lookup g glyphM) of
  (Just (OperatorG Dyadic _ _)) -> True
  _ -> False

applyNonadic :: Glyph -> Res
applyNonadic Eta = Right $ pure $ ArrayD (toScalar $ 0.5 * pi)
applyNonadic Pi = Right $ pure $ ArrayD (toScalar pi)
applyNonadic Tau = Right $ pure $ ArrayD (toScalar $ 2 * pi)
applyNonadic Infinity = Right $ pure $ ArrayD (toScalar infinity)
applyNonadic Random = Left NYI
applyNonadic _ = Left NYI

applyMonadic :: Glyph -> ArrayU -> Res
applyMonadic Duplicate x = U.duplicate x
applyMonadic Pop x = U.pop x
applyMonadic Identity x = U.identity x
applyMonadic Not x = U.not x
applyMonadic Sign x = U.sign x
applyMonadic Negate x = U.negate x
applyMonadic AbsoluteValue x = U.absoluteValue x
applyMonadic Sqrt x = U.sqrt x
applyMonadic Sine x = U.sine x
applyMonadic Floor x = U.floor x
applyMonadic Ceiling x = U.ceiling x
applyMonadic Round x = U.round x
applyMonadic Length x = U.length x
applyMonadic Shape x = U.shape' x
applyMonadic Range x = U.range x
applyMonadic First x = U.first x
applyMonadic Deshape x = U.deshape x
applyMonadic Bits x = U.bits x
applyMonadic Transpose x = U.transpose x
applyMonadic Rise x = U.rise x
applyMonadic Fall x = U.fall x
applyMonadic Where x = U.where' x
applyMonadic Classify x = U.classify x
applyMonadic Deduplicate x = U.deduplicate x
applyMonadic _ _ = Left NYI

applyDyadic :: Glyph -> ArrayU -> ArrayU -> Res
applyDyadic Over x y = Right $ [y,x,y]
applyDyadic Flip x y = Right $ [y,x]
applyDyadic Equals x y = lift2IU CoerceToD A.equals A.equals x y
applyDyadic NotEquals x y = lift2IU CoerceToD A.notequals A.notequals x y
applyDyadic LessThan x y = lift2IU CoerceToD A.lt A.lt x y
applyDyadic LessOrEqual x y = lift2IU CoerceToD A.lte A.lte x y
applyDyadic GreaterThan x y = lift2IU CoerceToD A.gt A.gt x y
applyDyadic GreaterOrEqual x y = lift2IU CoerceToD A.gte A.gte x y
applyDyadic Add x y = lift2U CoerceToD A.add A.add x y
applyDyadic Subtract x y = lift2U CoerceToD A.subtract A.subtract x y
applyDyadic Multiply x y = lift2U CoerceToD A.multiply A.multiply x y
applyDyadic Divide x y = lift2DU A.divide x y
applyDyadic Modulus x y = lift2U CoerceToD A.modulus A.modulusD x y
applyDyadic Power x y = lift2DU A.power x y
applyDyadic Logarithm x y = lift2DU A.logarithm x y
applyDyadic Minimum x y = lift2U CoerceToD A.minimum A.minimum x y
applyDyadic Maximum x y = lift2U CoerceToD A.maximum A.maximum x y
applyDyadic Atangent x y = lift2DU A.arctangent x y
applyDyadic Match x y = lift2IU CoerceToD ((Right .) . A.match) ((Right .) . A.match) x y
applyDyadic Couple x y = lift2U CoerceToD ((Right .) . A.couple) ((Right .) . A.couple) x y
-- FIXME
applyDyadic Pick _ _ = undefined
applyDyadic Reshape _ _ = undefined
applyDyadic Take _ _ = undefined
applyDyadic Drop _ _ = undefined
applyDyadic Rotate _ _ = undefined
applyDyadic Windows _ _ = undefined
applyDyadic Keep _ _ = undefined
applyDyadic Find _ _ = undefined
applyDyadic Member _ _ = undefined
applyDyadic IndexOf _ _ = undefined
applyDyadic Table _ _ = undefined
applyDyadic _ _ _ = Left NYI

pushRes :: [ArrayU] -> Res -> Either HuihuaWarning Stack
pushRes xs (Right rs) = Right (Stack (rs <> xs))
pushRes _ (Left e) = Left e

applyOp :: Glyph -> Stack -> Either HuihuaWarning Stack
applyOp g s
  | isNonadicOp g = applyNonadic g & pushRes (stackList s)
  | isMonadicOp g = case s of
      (Stack []) -> Left EmptyStack1
      (Stack (x:xs)) -> applyMonadic g x & pushRes xs
  | isDyadicOp g = case s of
      (Stack []) -> Left EmptyStack1
      (Stack [_]) -> Left EmptyStack2
      (Stack (x:y:xs)) -> applyDyadic g x y & pushRes xs
  | otherwise = Left ApplyNonOperator

reduceOp :: Glyph -> ArrayU -> Res
reduceOp Equals x = liftUI (Right . A.equalsR) (Right . A.equalsR) x
reduceOp NotEquals x = liftUI (Right . A.notEqualsR) (Right . A.notEqualsR) x
reduceOp LessThan x = liftUI (Right . A.lessThanR) (Right . A.lessThanR) x
reduceOp LessOrEqual x = liftUI (Right . A.lessOrEqualR) (Right . A.lessOrEqualR) x
reduceOp GreaterThan x = liftUI (Right . A.greaterThanR) (Right . A.greaterThanR) x
reduceOp GreaterOrEqual x = liftUI (Right . A.greaterOrEqualR) (Right . A.greaterOrEqualR) x
reduceOp Add x = liftU (Right . A.addR) (Right . A.addR) x
-- reduceOp Subtract x = liftU (Right . A.subtractR) (Right . A.subtractR) x
reduceOp Multiply x = liftU (Right . A.multiplyR) (Right . A.multiplyR) x
-- reduceOp Divide x = liftU (Right . A.divideR) (Right . A.divideR) x
-- reduceOp Modulus x = liftU (Right . A.modulusR) (Right . A.modulusR) x
-- reduceOp Power x = liftU (Right . A.powerR) (Right . A.powerR) x
-- reduceOp Logarithm x = liftU (Right . A.logarithmR) (Right . A.logarithmR) x
reduceOp Minimum x = liftU (Right . A.minimumR) (Right . A.minimumR) x
reduceOp Maximum x = liftU (Right . A.maximumR) (Right . A.maximumR) x
reduceOp _ _ = Left NYI
