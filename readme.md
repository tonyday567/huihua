
# Table of Contents

1.  [huihua](#org87f1013)
2.  [Development](#orgf1e5c06)
    1.  [range](#org3b14889)
    2.  [member](#org655454f)
    3.  [parser debug pipeline](#orgae19c55)
3.  [development notes](#org7a18277)
    1.  [pad](#org46ab58d)
    2.  [find & mask](#org3cf2af9)
        1.  [tests](#org7b27e70)
    3.  [find](#org9a4d451)
    4.  [window](#org13169f1)
    5.  [keep](#orgf248672)
    6.  [ArrayH pretty](#org99df4ae)
    7.  [doctest subscript bug](#org8c47e7b)
    8.  [reduce1U dev](#org2767580)
4.  [combo array parsing](#org63c7857)
5.  [number parsing](#org667b04b)
6.  [token parsing](#org614f875)
    1.  [basics](#org27caac3)
    2.  [interp debug](#org3bc07aa)
        1.  [ex1](#org8808ecb)
7.  [NYI](#org8728dac)
8.  [test isms](#org932cfdc)
9.  [negative bug](#org7419533)
10. [Creating the glyph list](#org610cdb1)
11. [equality in haskell code](#orgbd0be32)
12. [Symbol Extraction](#org9bc3927)


<a id="org87f1013"></a>

# huihua

[![img](https://img.shields.io/hackage/v/huihua.svg)](https://hackage.haskell.org/package/huihua)
[![img](https://github.com/tonyday567/huihua/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/huihua/actions?query=workflow%3Ahaskell-ci)

`huihua` is a Haskell port of [uiua](https://www.uiua.org/)

<https://www.uiua.org/docs/>


<a id="orgf1e5c06"></a>

# Development

    :r
    :set -Wno-type-defaults
    :set -Wno-name-shadowing
    :set -XOverloadedStrings
    :set -XRebindableSyntax
    :set -XQuasiQuotes
    import Prelude qualified
    import Prelude hiding ((.))
    import Prelude qualified as P
    import Harpie.Array qualified as D
    import Harpie.Shape qualified as Shape
    import Huihua.ArrayU as U
    import Huihua.Array qualified as A
    import Huihua.Stack
    import Huihua.Examples
    import Huihua.Glyphs as G
    import Huihua.Stack qualified as S
    import Huihua.Parse
    import Huihua.Parse.FlatParse
    import Data.ByteString.Char8 qualified as C
    import Data.String.Interpolate
    import Data.ByteString (ByteString)
    import Data.Text.Encoding
    import MarkupParse
    import Data.List qualified as List
    import Prettyprinter hiding (equals)

    Build profile: -w ghc-9.8.2 -O1
    In order, the following will be built (use -v for more details):
     - huihua-0.0.1 (lib) (file src/Huihua/Array.hs changed)
    Preprocessing library for huihua-0.0.1..
    GHCi, version 9.8.2: https://www.haskell.org/ghc/  :? for help
    [1 of 8] Compiling Huihua.Examples  ( src/Huihua/Examples.hs, interpreted )
    [2 of 8] Compiling Huihua.Parse.FlatParse ( src/Huihua/Parse/FlatParse.hs, interpreted )
    [3 of 8] Compiling Huihua.Warning   ( src/Huihua/Warning.hs, interpreted )
    [4 of 8] Compiling Huihua.Array     ( src/Huihua/Array.hs, interpreted )
    [5 of 8] Compiling Huihua.ArrayU    ( src/Huihua/ArrayU.hs, interpreted )
    [6 of 8] Compiling Huihua.Stack     ( src/Huihua/Stack.hs, interpreted )
    [7 of 8] Compiling Huihua.Glyphs    ( src/Huihua/Glyphs.hs, interpreted )
    [8 of 8] Compiling Huihua.Parse     ( src/Huihua/Parse.hs, interpreted )
    Ok, 8 modules loaded.
    Ok, 8 modules loaded.

    run [i|⊡ [1_2 0_1 0_0] [1_2_3 4_5_6 ]|]
    D.length $ A.first $ D.iota [2]
    D.rank $ (D.asArray [8,3,9,2,0] :: Array Int)
    A.pick (D.array [2,1] [2,1]) (D.asArray [8,3,9,2,0] :: Array Int)
    run [i|⊡ 2_1 [8 3 9 2 0]|]

    [6 2 1]
    1
    1
    Right (UnsafeArray [2] [9,3])
    9


<a id="org3b14889"></a>

## range

    A.range (D.toScalar 3)
    A.range (D.asArray [3])
    A.range (D.asArray [-2,3])
    A.range (D.toScalar (-3))

    UnsafeArray [3] [0,1,2]
    UnsafeArray [3,1] [0,1,2]
    UnsafeArray [2,3,2] [-1,0,-1,1,-1,2,-2,0,-2,1,-2,2]
    UnsafeArray [3] [-1,-2,-3]
    [[[0,0],
      [0,1],
      [0,2]],
     [[1,0],
      [1,1],
      [1,2]]]
    [[0],
     [1],
     [2]]

    A.range (D.toScalar 4)
    A.range (D.asArray [4])
    D.range (D.asArray [4])
    -- a = D.asArray [4]
    a = D.toScalar [4]
    D.join $ D.tabulateA (fmap abs a) (\s -> D.zipWithE (\ab si -> bool ab (negate one - ab) (si<0)) s (fmap signum a))

    UnsafeArray [4] [0,1,2,3]
    UnsafeArray [4] [0,1,2,3]
    UnsafeArray [4,1] [0,1,2,3]
    <interactive>:80:32: error: [GHC-83865]
        • Couldn't match type ‘[a0]’ with ‘Int’
          Expected: Array Int
            Actual: Array [a0]
        • In the second argument of ‘fmap’, namely ‘a’
          In the first argument of ‘D.tabulateA’, namely ‘(fmap abs a)’
          In the second argument of ‘($)’, namely
            ‘D.tabulateA
               (fmap abs a)
               (\ s
                  -> D.zipWithE
                       (\ ab si -> bool ab (negate one - ab) (si < 0)) s (fmap signum a))’


<a id="org655454f"></a>

## member

    i = D.toScalar 2 :: Array Int
    a = D.array [2,2,3] [1,2,3,4,5,6,2,1,1,2,2,2] :: Array Int
    A.indexOf i a
    findI xs i' = fromMaybe (List.length xs) . List.findIndex (==i') . toList $ xs
    (\x -> findI x (D.fromScalar i)) <$> (D.extractsExcept [D.rank a - 1] a)

    UnsafeArray [2,2] [1,3,0,0]
    UnsafeArray [2,2] [1,3,0,0]

a 2 i 1

    i = D.asArray [4,5,6] :: Array Int
    a = D.array [2,3] [1,2,3,4,5,6] :: Array Int
    A.indexOf i a
    fmap (findI (D.extractsExcept [D.rank a - 1] a)) (D.extractsExcept [D.rank i - 1] i)

    UnsafeArray [] [1]
    UnsafeArray [] [1]

a 1 i 1

    i = D.array [3] [1,2,3] :: Array Int
    a = D.array [5] [0,3,4,5,1] :: Array Int
    A.indexOf i a
    fmap (findI a) i

    UnsafeArray [3] [4,5,1]
    UnsafeArray [3] [4,5,1]

a 1 i 2

    i = D.array [2,3] [1..6] :: Array Int
    a = D.asArray [3,4,5] :: Array Int
    A.indexOf i a
    fmap (findI a) i

    UnsafeArray [2,3] [3,3,0,1,2,3]
    UnsafeArray [2,3] [3,3,0,1,2,3]

a 2 i 2

    i = D.array [2,3] [1..6] :: Array Int
    a = D.array [1,3] [4,5,6] :: Array Int
    A.indexOf i a
    fmap (findI (D.extractsExcept [D.rank a - 1] a)) (D.extractsExcept [D.rank i - 1] i)

    UnsafeArray [2] [1,0]
    UnsafeArray [2] [1,0]

member

    -- Shape.takeIndexes [] [0]
    -- D.join $ D.extracts [0] (D.toScalar 2)
    -- D.join $ D.extracts [0] (D.asSingleton $ D.toScalar 2)
    
    A.member (D.toScalar 2) (D.asArray [1,2,3])
    A.member (D.asArray [2]) (D.asArray [1,2,3])
    A.member (D.toScalar 5) (D.asArray [1,2,3])
    A.member (D.asArray [1,2,3]) (D.asArray [0,3,4,5,1])
    A.member (D.asArray [3,4,5]) (D.iota [2,3])
    A.member (D.iota [2,3]) (D.asArray [2,3,4])
    A.member (D.toScalar 2) (D.iota [2,3])

    UnsafeArray [] [1]
    UnsafeArray [1] [1]
    UnsafeArray [] [0]
    UnsafeArray [3] [1,0,1]
    UnsafeArray [1] [1]
    UnsafeArray [2,3] [0,0,1,1,1,0]
    UnsafeArray [2] [1,0]


<a id="orgae19c55"></a>

## parser debug pipeline

    x = [i|⍏ 6_2_7_0_1_5|]
    runParser tokens x
    parseT x
    xis = parseT x & instructionize
    xis
    interpI xis
    run x

    OK [GlyphToken Rise,DoubleToken 6.0,GlyphToken Strand,DoubleToken 2.0,GlyphToken Strand,DoubleToken 7.0,GlyphToken Strand,DoubleToken 0.0,GlyphToken Strand,DoubleToken 1.0,GlyphToken Strand,DoubleToken 5.0] ""
    [GlyphToken Rise,DoubleToken 6.0,GlyphToken Strand,DoubleToken 2.0,GlyphToken Strand,DoubleToken 7.0,GlyphToken Strand,DoubleToken 0.0,GlyphToken Strand,DoubleToken 1.0,GlyphToken Strand,DoubleToken 5.0]
    [IOp Rise,IArray (UnsafeArray [6] [6.0,2.0,7.0,0.0,1.0,5.0])]
    Right (Stack {stackList = [ArrayU {arrayd = UnsafeArray [6] [3.0,4.0,1.0,5.0,0.0,2.0]}]})
    [3 4 1 5 0 2]

    i = parseT x
    instructionize $ List.take 1 i

    []

    D.folds [0] sum (D.iota 5)

    UnsafeArray [5] [0,1,2,3,4]

    List.drop 2 xis
    either viaShow pretty $ interpI $ List.drop 2 xis
    a = (D.asArray [1,5,8,2] :: Array Double)
    run [i|+0_1_0 0_4_3|]
    run [i|+[0_1_0 0_4_3] [1_1_1 1_1_1]|]

    [IOp Flip,IReduceOp Add,IOp Duplicate,IArray (UnsafeArray [4] [1.0,5.0,8.0,2.0])]
    [1 5 8 2]
    [1 5 8 2]
    [0 5 3]
    ╭─
    ╷ 1 5 4
      1 2 1
            ╯

    runParser tokens "◌1 2"
    runParser tokens [i|◌1 2|]
    runParser tokens (encodeUtf8 [i|◌1 2|])

    OK [] "\204\&1 2"
    OK [GlyphToken Pop,IntToken 1,IntToken 2] ""
    OK [GlyphToken Pop,IntToken 1,IntToken 2] ""


<a id="org7a18277"></a>

# development notes


<a id="org46ab58d"></a>

## pad

    a = D.iota [2] :: Array Int
    s' = [3,3] :: [Int]
    pretty $ D.pad 69 s' a
    pretty $ D.lpad 69 s' a

    [[0,1,69],
     [69,69,69],
     [69,69,69]]
    [[69,69,69],
     [69,69,69],
     [69,0,1]]


<a id="org3cf2af9"></a>

## find & mask

findNoOverlap deconstruction

    a = D.konst [4,4] 1 :: Array Int
    i = D.konst [2,2] 1 :: Array Int
    pretty $ fmap sig $ D.findNoOverlap i a

    [[1,0,1],
     [0,0,0],
     [1,0,1]]

    iexp = rerank (rank a) i
    f = find iexp a
    cl sh = List.filter (\xs -> P.not (any (>0) (List.init xs))) $ List.filter (P.not . (all (>=0))) $ D.arrayAs $ D.tabulate ((\x -> 2 * x - 1) <$> sh) (\s -> List.zipWith (\x x0 -> x - x0 + 1) s sh)
    go r' s = D.index f s && all P.not (D.index r' <$> (List.filter (S.inside x (D.shape f)) $ fmap (List.zipWith (+) s) (cl (shape iexp))))
    r = tabulate (shape f) (go r)
    
    cl sh = List.filter (\xs -> P.not (any (>0) (List.init xs))) $ List.filter (P.not . (all (>=0))) $ D.arrayAs $ D.tabulate ((\x -> 2 * x - 1) <$> sh) (\s -> List.zipWith (\x x0 -> x - x0 + 1) s sh)
    inside x' x = all id $ List.zipWith (\y' y -> y >= 0 && y < y') x' x
    go r' s = D.index f s && all P.not (D.index r' <$> (List.filter (inside (D.shape f)) $ fmap (List.zipWith (+) s) (cl (D.shape iexp))))
    r = D.tabulate (D.shape f) (go r)
    pretty $ fmap sig $ D.find i a
    pretty $ fmap sig r


<a id="org7b27e70"></a>

### tests

    a = D.iota [2,2] :: Array Int
    i = D.toScalar 2 :: Array Int
    D.find i a
    D.findNoOverlap i a
    A.find i a
    A.mask i a

    UnsafeArray [2,2] [False,False,True,False]
    UnsafeArray [2,2] [False,False,True,False]
    UnsafeArray [2,2] [0,0,1,0]
    UnsafeArray [2,2] [0,0,1,0]

    a = D.iota [4] :: Array Int
    i = D.array [1] [2] :: Array Int
    D.find i a
    D.findNoOverlap i a
    A.find i a
    A.mask i a

    UnsafeArray [4] [False,False,True,False]
    UnsafeArray [4] [False,False,True,False]
    UnsafeArray [4] [0,0,1,0]
    UnsafeArray [4] [0,0,1,0]

    a = D.iota [2,3,4] :: Array Int
    i = D.array [2,2] [0,1,4,5] :: Array Int
    D.find i a
    D.findNoOverlap i a
    A.find i a
    pretty $ A.mask i a

    UnsafeArray [2,2,3] [True,False,False,False,False,False,False,False,False,False,False,False]
    UnsafeArray [2,2,3] [True,False,False,False,False,False,False,False,False,False,False,False]
    UnsafeArray [2,3,4] [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    [[[1,1,0,0],
      [1,1,0,0],
      [0,0,0,0]],
     [[0,0,0,0],
      [0,0,0,0],
      [0,0,0,0]]]

    a = D.cycle [4,4] (D.iota [3]) :: Array Int
    i = D.array [2] [1,2] :: Array Int
    pretty $ D.find i a
    pretty $ D.findNoOverlap i a
    pretty $ A.find i a
    pretty $ A.mask i a

    [[False,True,False],
     [True,False,False],
     [False,False,True],
     [False,True,False]]
    [[False,True,False],
     [True,False,False],
     [False,False,True],
     [False,True,False]]
    [[0,1,0,0],
     [1,0,0,0],
     [0,0,1,0],
     [0,1,0,0]]
    [[0,1,1,0],
     [2,2,0,0],
     [0,0,3,3],
     [0,4,4,0]]

    a = D.konst [5,5] 1 :: Array Int
    i = D.konst [2,2] 1 :: Array Int
    D.find i a
    pretty $ D.findNoOverlap i a
    pretty $ A.find i a
    pretty $ A.mask i a

    UnsafeArray [4,4] [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
    [[True,True,True,True],
     [True,True,True,True],
     [True,True,True,True],
     [True,True,True,True]]
    [[1,1,1,1,0],
     [1,1,1,1,0],
     [1,1,1,1,0],
     [1,1,1,1,0],
     [0,0,0,0,0]]
    [[1,3,5,7,4],
     [6,14,18,22,12],
     [14,30,34,38,20],
     [22,46,50,54,28],
     [13,27,29,31,16]]

    a = D.cycle [4,4] (D.iota [3]) :: Array Int
    i = D.array [2,2] [1,2,2,0] :: Array Int
    D.find i a
    pretty $ D.findNoOverlap i a
    pretty $ A.find i a
    pretty $ A.mask i a

    UnsafeArray [3,3] [False,True,False,True,False,False,False,False,True]
    [[False,True,False],
     [True,False,False],
     [False,False,True]]
    [[0,1,0,0],
     [1,0,0,0],
     [0,0,1,0],
     [0,0,0,0]]
    [[0,1,1,0],
     [2,3,1,0],
     [2,2,3,3],
     [0,0,3,3]]

    run [i|⌕ [1_2 2_0] ↯4_4⇡3|]

    ╭─
    ╷ 0 1 0 0
      1 0 0 0
      0 0 1 0
      0 0 0 0
              ╯

    run [i|⦷ [1_2 2_0] ↯4_4⇡3|]

    <interactive>:77:8: error: [GHC-83865]
        • Couldn't match expected type ‘template-haskell-2.21.0.0:Language.Haskell.TH.Quote.QuasiQuoter’
                      with actual type ‘Array Int’
        • In the first argument of ‘template-haskell-2.21.0.0:Language.Haskell.TH.Quote.quoteExp’, namely
            ‘i’
          In the expression:
            template-haskell-2.21.0.0:Language.Haskell.TH.Quote.quoteExp
              i "\10679 [1_2 2_0] \8623\&4_4\8673\&3"
          In the quasi-quotation: [i|⦷ [1_2 2_0] ↯4_4⇡3|]


<a id="org9a4d451"></a>

## find

    run[i|⌕ [1_2 2_0] .↯4_4_4⇡3|]

    ╭─
    ╷ 0 1 0 0
    ╷ 1 0 0 1
      0 0 1 0
      0 0 0 0
    
      1 0 0 1
      0 0 1 0
      0 1 0 0
      0 0 0 0
    
      0 0 1 0
      0 1 0 0
      1 0 0 1
      0 0 0 0
    
      0 0 0 0
      0 0 0 0
      0 0 0 0
      0 0 0 0
              ╯
    ╭─
    ╷ 0 1 2 0
    ╷ 1 2 0 1
      2 0 1 2
      0 1 2 0
    
      1 2 0 1
      2 0 1 2
      0 1 2 0
      1 2 0 1
    
      2 0 1 2
      0 1 2 0
      1 2 0 1
      2 0 1 2
    
      0 1 2 0
      1 2 0 1
      2 0 1 2
      0 1 2 0
              ╯

    a = D.cycle [4,4] (D.iota [3]) :: Array Int
    i = D.array [2,2] [1,2,2,0] :: Array Int
    pretty a
    pretty i
    pretty $ A.find a i
    ws = D.windows (D.shape i) a
    -- D.shape ws :: [Int]
    -- pretty ws
    -- pretty a
    -- fmap (sig . (==i)) (D.extracts (D.arrayAs (D.iota [D.rank i]) <> [D.rank i * 2 .. (D.rank ws - 1)]) ws)

    [[0,1,2,0],
     [1,2,0,1],
     [2,0,1,2],
     [0,1,2,0]]
    [[1,2],
     [2,0]]
    [[0,0],
     [0,0]]


<a id="org13169f1"></a>

## window

    D.shape @[Int] $ D.windows [2,2] (D.iota [4,3,2])

    [3,2,2,2,2]

    pretty $ D.windows [2] (D.iota [4])

    [[0,1],
     [1,2],
     [2,3]]

    pretty $ D.windows [4] (D.iota [6])

    [[0,1,2,3],
     [1,2,3,4],
     [2,3,4,5]]

Consistent for scalars

    D.windows [1] (D.toScalar 5)

    UnsafeArray [1] [5]

    :{
    run [i|
    △ .◫2 .⇡4
    △ .◫4 .⇡6
    △ ◫2_2 .[1_2_3 4_5_6 7_8_9]
    △ .◫¯2 .↯4_4⇡16
    △ .◫¯3 .↯4_4⇡16
    △ .◫¯1_2 .↯4_4⇡16
    
    |]
    :}

    :{
    run [i|
    △ ◫2 ⇡4
    
    |]
    :}

    ghci| ghci| ghci| ghci| ghci| [3 2]
    ╭─
    ╷ 0 1
      1 2
      2 3
          ╯


<a id="orgf248672"></a>

## keep

    
    i = D.asArray [3,2] :: Array Int
    x = D.asArray [8,3,9,2,0] :: Array Int
    D.asArray $ fold $ D.zipWithE (replicate) (D.cycle (D.shape x) i) x

    UnsafeArray [13] [8,8,8,3,3,9,9,9,2,2,0,0,0]

    i = D.asArray [3,2] :: Array Int
    x = D.asArray [8,3,9,2,0] :: Array Int
    x2 = D.iota [3,4] :: Array Int
    D.join $ D.asArray $ fold $ D.zipWithE replicate (D.cycle (List.take 1 $ D.shape x2) i) (D.extracts [0] x2)

    UnsafeArray [8,4] [0,1,2,3,0,1,2,3,0,1,2,3,4,5,6,7,4,5,6,7,8,9,10,11,8,9,10,11,8,9,10,11]


<a id="org99df4ae"></a>

## ArrayH pretty

    x = D.array [2,2,2,2] ([0..14] <> [100]) :: Array Double
    x
    pretty x
    pretty (ArrayH x)

    UnsafeArray [2,2,2,2] [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,100.0]
    [[[[0.0,1.0],
       [2.0,3.0]],
      [[4.0,5.0],
       [6.0,7.0]]],
     [[[8.0,9.0],
       [10.0,11.0]],
      [[12.0,13.0],
       [14.0,100.0]]]]
    ╭─
    ╷ .0 ..1
    ╷ .2 ..3
    ╷
      .4 ..5
      .6 ..7
    
    
      .8 ..9
      10 .11
    
      12 .13
      14 100
             ╯

    import Data.Either
    t = ptree (fmap showU x)
    maxp = D.folds [1] (P.maximum . fmap P.length) (D.join $ D.asArray $ rights t)
    s = fmap (fmap ((D.zipWithE (\m a -> lpad '.' m a) maxp))) t
    sdoc = mconcat $ fmap (either (\n -> replicate (n-1) mempty) (pure . hsep . fmap pretty . D.arrayAs)) s
    sdocMin = D.concatenate 0 (D.konst [max 0 (D.rank x - P.length sdoc)] mempty) (D.asArray sdoc)
    rankPrefix = fmap pretty (D.reshapeDef " " [D.length sdocMin] (D.konst [D.rank x - 1] "╷"))
    deco = zipWith (<+>) (D.arrayAs rankPrefix) (D.arrayAs sdocMin)
    final = (pretty "╭─") <> line <> (vsep deco) <> hang 1 (line <> pretty "╯")
    final

    ╭─
    ╷ .0 ..1
    ╷ .2 ..3
    ╷
      .4 ..5
      .6 ..7
    
    
      .8 ..9
      10 .11
    
      12 .13
      14 100
             ╯

    x = D.array [2,2] [2, 2.222,200.001,200.01] :: Array Double
    pretty (U.ArrayH x)
    a = D.konst [2,2] (3 / P.pi) :: Array Double
    pretty (U.ArrayH a)
    a3 = D.konst [1,1,1] 1
    pretty (U.ArrayH a3)
    a1 = D.asArray [1..4]
    pretty (U.ArrayH a1)

    ╭─
    ╷ ......2 .2.222
      200.001 200.01
                     ╯
    ╭─
    ╷ 0.954929658551372 0.954929658551372
      0.954929658551372 0.954929658551372
                                          ╯
    ╭─
    ╷
    ╷ 1
        ╯
    [1 2 3 4]

deconstruction

    -- x = D.array [2,2] [2, 2.222,200.001,200.01] :: Array Double
    -- x = D.array [1,1,1] [1] :: Array Double
    x = D.array [2,2,2,2] ([0..14] <> [100]) :: Array Double
    dtable = D.maps [1] (\a -> U.lpad '.' (P.maximum (P.length <$> a)) <$> a) (U.showU <$> x)
    dtable1 = D.folds [0] (hsep . D.arrayAs . fmap pretty) dtable
    dtableMin = D.concatenate 0 (D.konst [max 0 (D.rank x - 1 - D.length dtable1)] mempty) dtable1
    rankPrefix = fmap pretty (D.reshapeDef "x" [D.length dtableMin] (D.konst [D.rank x - 1] "╷"))
    decotable = D.zipWithE (<+>) rankPrefix dtableMin
    final = (pretty "╭─") <> line <> ((vsep . D.arrayAs) decotable) <> hang 1 (line <> pretty "╯")
    final
    dtable

    ╭─
    ╷
    ╷ .0 .1 .2 .3 ..4 ..5 ..6 ..7
    ╷ .8 .9 10 11 .12 .13 .14 100
                                  ╯
    UnsafeArray [2,2,2,2] [".0",".1",".2",".3","..4","..5","..6","..7",".8",".9","10","11",".12",".13",".14","100"]


<a id="org8c47e7b"></a>

## doctest subscript bug

    -- |
    --
    -- > run [i|ⁿ2 3|]
    -- 9
    -- > run [i|ⁿ2 [1 2 3]|]
    -- [1 4 9]
    --
    -- >>> 1+1
    --

    -- | ₙ
    --
    -- > run [i|ₙ2 8|]
    -- 4
    -- > run [i|ₙ [2 3 4] [16 27 1024]|]
    --

    run [i|ⁿ2 3|]
    run [i|ⁿ2 [1 2 3]|]
    run [i|ₙ2 8|]
    run [i|ₙ [2 3 4] [16 27 1024]|]
    log 8 / log 2

    -- | /ⁿ
    --
    -- >>> run [i|/ⁿ[]|]
    -- 1
    -- >>> run [i|/ⁿ [2]|]
    -- 2
    -- >>> run [i|/ⁿ 2_1|]
    -- 2
    -- >>> run [i|/ⁿ [1_2_3 4_5_6]|]
    -- [4 25 216]

    run [i|/ⁿ[]|]
    run [i|/ⁿ [2]|]
    run [i|/ⁿ 2_1|]
    run [i|/ⁿ [1_2_3 4_5_6]|]

    1
    2
    1
    [4 25 216]

    -- | /ₙ
    --
    -- >>> run [i|/ₙ[]|]
    -- 1
    -- >>> run [i|/ₙ [2]|]
    -- 2
    -- >>> run [i|/ₙ 2_1|]
    -- 2
    -- >>> run [i|/ₙ [1_2_3 4_5_6]|]
    -- [4 25 216]

    run [i|/ₙ[]|]
    run [i|/ₙ [2]|] -- 2
    run [i|/ₙ 2_8|] -- 3
    run [i|/ₙ [1_2_3 4_5_6]|] -- [∞ 2.321928094887362 1.6309297535714575]
    logBase 2 8

    NoIdentity
    2
    2.9999999999999996
    [Infinity 2.321928094887362 1.6309297535714573]
    2.9999999999999996


<a id="org2767580"></a>

## reduce1U dev

    a = D.iota [3,4]
    pretty a
    (x D.:| xs) = a
    x
    xs
    D.extracts [1] xs
    D.zips [0] (D.zipWithE (\x' xs' -> foldl' (+) (D.fromScalar x') xs')) (D.extracts [0] x) (D.extractsExcept [0] xs)

    [[0,1,2,3],
     [4,5,6,7],
     [8,9,10,11]]
    UnsafeArray [4] [0,1,2,3]
    UnsafeArray [2,4] [4,5,6,7,8,9,10,11]
    UnsafeArray [4] [UnsafeArray [2] [4,8],UnsafeArray [2] [5,9],UnsafeArray [2] [6,10],UnsafeArray [2] [7,11]]
    UnsafeArray [4] [12,15,18,21]

    xy a = let (x D.:| xs) = a in D.zipWithE (\a as -> foldl' (+) a as) x (D.extractsExcept [0] xs)

    let a3 = D.iota [2,3,4]
    reduceU (+) zero a3
    xy a3

    UnsafeArray [3,4] [12,14,16,18,20,22,24,26,28,30,32,34]
    UnsafeArray [3,4] [12,14,16,18,20,22,24,26,28,30,32,34]

    import Prelude qualified
    :t Prelude.mod infinity (Prelude.3)

    Build profile: -w ghc-9.8.2 -O1
    In order, the following will be built (use -v for more details):
     - huihua-0.0.1 (lib) (file src/Huihua/ArrayU.hs changed)
    Preprocessing library for huihua-0.0.1..
    GHCi, version 9.8.2: https://www.haskell.org/ghc/  :? for help
    [1 of 8] Compiling Huihua.Examples  ( src/Huihua/Examples.hs, interpreted )
    [2 of 8] Compiling Huihua.Parse.FlatParse ( src/Huihua/Parse/FlatParse.hs, interpreted )
    [3 of 8] Compiling Huihua.Warning   ( src/Huihua/Warning.hs, interpreted )
    [4 of 8] Compiling Huihua.Array     ( src/Huihua/Array.hs, interpreted )
    
    src/Huihua/Array.hs:495:35: error: [GHC-83865]
        • Couldn't match expected type: Array a -> Array b
                      with actual type: Array a
        • Possible cause: ‘D.reduces’ is applied to too many arguments
          In the expression: D.reduces [0] (foldl' (flip f) x) xs
          In an equation for ‘reduceNoIdentityU’:
              reduceNoIdentityU f (x D.:| xs)
                = D.reduces [0] (foldl' (flip f) x) xs
        • Relevant bindings include
            xs :: Array a (bound at src/Huihua/Array.hs:495:29)
            x :: Array a (bound at src/Huihua/Array.hs:495:22)
            f :: a -> a -> a (bound at src/Huihua/Array.hs:495:19)
            reduceNoIdentityU :: (a -> a -> a) -> a -> Array a -> Array b
              (bound at src/Huihua/Array.hs:495:1)
        |
    495 | reduceNoIdentityU f (x D.:| xs) = D.reduces [0] (foldl' (flip f) x) xs
        |                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Failed, three modules loaded.
    <interactive>:1:13: error: [GHC-88464]
        Variable not in scope: infinity :: a -> c
    
    <interactive>:1:23: error: [GHC-88464]
        Data constructor not in scope: Prelude :: b0 -> c


<a id="org63c7857"></a>

# combo array parsing

    x = [i|[2_1_0 0_4_3]|]
    runParser tokens x
    parseT x
    parseT x & instructionize
    interpI (List.reverse $ parseI x)
    run x

    OK [GlyphToken ArrayLeft,DoubleToken 2.0,GlyphToken Strand,DoubleToken 1.0,GlyphToken Strand,DoubleToken 0.0,DoubleToken 0.0,GlyphToken Strand,DoubleToken 4.0,GlyphToken Strand,DoubleToken 3.0,GlyphToken ArrayRight] ""
    [GlyphToken ArrayLeft,DoubleToken 2.0,GlyphToken Strand,DoubleToken 1.0,GlyphToken Strand,DoubleToken 0.0,DoubleToken 0.0,GlyphToken Strand,DoubleToken 4.0,GlyphToken Strand,DoubleToken 3.0,GlyphToken ArrayRight]
    [WArray (UnsafeArray [2] [IArray (UnsafeArray [3] [2.0,1.0,0.0]),IArray (UnsafeArray [3] [0.0,4.0,3.0])])]
    Right (Stack {stackList = [ArrayU {arrayd = UnsafeArray [2,3] [2.0,1.0,0.0,0.0,4.0,3.0]}]})
    ╭─
    ╷ 2 1 0
      0 4 3
            ╯

    :t D.extracts [0,1]
    let a = D.array [2,3,4] [0..23] :: D.Array Int
    
    a' = D.extracts [0,1] a
    :t D.drops [(0,0),(1,1)] a'

    D.extracts [0,1] :: Array a -> Array (Array a)
    D.drops [(0,0),(1,1)] a' :: Array (Array Int)

    run [i|/>.[2_1_0 0_4_3]|]

    [0 1]
    ╭─
    ╷ 2 1 0
      0 4 3
            ╯

    import Huihua.Glyphs
    ts = parseT x
    :t ts
    -- assemble (aArrayLeft *> many aToken <* aArrayRight) [GlyphToken ArrayLeft,GlyphToken ArrayRight]
    assemble (aArrayLeft *> many aToken) [GlyphToken ArrayLeft,GlyphToken ArrayRight]

    ts :: [Huihua.Parse.Token]
    Just ([GlyphToken ArrayRight],[])


<a id="org667b04b"></a>

# number parsing

    run "123. 5"

    123
    5
    5

    showU (P.negate 2)

    showU (P.negate 2) :: Data.Text.Internal.Text


<a id="org614f875"></a>

# token parsing


<a id="org27caac3"></a>

## basics

    runParser tokens "[@u @i @u @a]"

    OK [GlyphToken ArrayLeft,CharacterToken 'u',CharacterToken 'i',CharacterToken 'u',CharacterToken 'a',GlyphToken ArrayRight] ""

    runParser tokens [i|△."Hello, World!"|]

    OK [GlyphToken Shape,GlyphToken Duplicate,StringToken "Hello, World!"] ""

    
    runParser tokens [i|⊂ 1_2 3|]

    OK [GlyphToken Join,IntToken 1,GlyphToken Strand,IntToken 2,IntToken 3] ""

    P.length <$> C.unpack <$> allTheSymbols

    [1,1,3,1,3,2,2,2,3,3,3,3,3,3,1,3,4,3,4,3,1,1,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,3,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,1,3,3,1,3,3,3,2,2,2,3,1,1,1,1,1,1,1,1,2,1,1,1,3,1,1]


<a id="org3bc07aa"></a>

## interp debug

**\*** not

    x1 = "\194\172" :: ByteString
    x1
    encodeUtf8 "¬"
    C.putStrLn x1
    runParser glyph "¬"
    runParser glyph x1
    runParser glyph (encodeUtf8 "¬")

    "\194\172"
    "\194\172"
    ¬
    Fail
    OK Not ""
    OK Not ""

    bs = encodeUtf8 "¬ 3 [0 1]"
    interp bs
    C.putStrLn bs

    That (Stack {stackList = [-2,[1, 0]]})
    ¬ 3 [0 1]


<a id="org8808ecb"></a>

### ToDo ex1

-   [X] reverse order of arrays
-   [X] reverse order of assembled ops
-   [X] array left bug
-   [X] implement in compute1
-   [ ] refactor
    -   do away with raw Ints and Doubles

    bs = exPage1

pipeline

    bs
    C.putStr bs
    -- C.lines bs
    -- C.lines bs & fmap (runParser tokens)
    -- C.lines bs & fmap (runParser_ tokens) & orderUiua
    as = assemble' bs
    interp as
    s

    "\n[1 5 8 2]\n/+. # Sum\n\226\167\187\226\136\182  # Length\n\195\183   # Divide\n"
    [1 5 8 2]
    /+. # Sum
    ⧻∶  # Length
    ÷   # Divide
    Left EmptyStack1
    Stack {stackList = *** Exception: NYI
    CallStack (from HasCallStack):
      error, called at <interactive>:47:92 in interactive:Ghci23

    interp (assemble' exPage1)

    Left EmptyStack1

shapes are ok &#x2026;

    :set -Wno-incomplete-uni-patterns
    (Stack (ItemArrayInt x:ItemArrayInt y:xs)) = s3
    shape x
    shape y
    (ItemArrayDouble z) = binOpD (/) (ItemArrayInt y) (ItemArrayInt x)
    shape z

    []
    []
    []

    C.lines bs & fmap (runParser_ tokens) & orderUiua & assemblef & foldr compute1 (Stack [])

    Stack {stackList = [4.0]}


<a id="org8728dac"></a>

# NYI

`&p` is an effect-only. Nothing is added or subtracted form the stack.
`---` scope
`use`
`&i` imports
`~~~`  test scope

[All Functions](https://www.uiua.org/docs/all-functions)


<a id="org932cfdc"></a>

# test isms

<https://www.uiua.org/docs/isms>


<a id="org7419533"></a>

# negative bug

This character doesn&rsquo;t parse properly in a \*.hs file.
&ldquo;¯&rdquo;

    :t Negative

    Negative :: Glyph


<a id="org610cdb1"></a>

# Creating the glyph list

Note that direct comparison between a ByteString and it&rsquo;s representation may not be what you expect eg

    symNot = allTheSymbols List.!! 5
    C.putStrLn symNot
    symNot
    "¬" == symNot
    "\194\172" == symNot

    ¬
    "\194\172"
    False
    True

    xs = (zipWith (\s g -> [i|"#{s}" -> pure #{g}|]) allTheSymbols allTheGlyphs :: [ByteString])
    traverse_ C.putStrLn xs

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
    "\" -> pure Scan
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
    "¯" -> pure Negative
    "@" -> pure Format
    "$" -> pure String
    """ -> pure Binding
    "←" -> pure Signature
    "|" -> pure Comment

    traverse_ C.putStrLn allTheSymbols


<a id="orgbd0be32"></a>

# equality in haskell code

    symNot = allTheSymbols List.!! 5
    C.putStrLn symNot
    symNot
    -- traverse_ C.putStrLn (P.take 8 allTheSymbols)
    "¬" == symNot
    "\194\172" == symNot

    ¬
    "\194\172"
    False
    True


<a id="org9bc3927"></a>

# Symbol Extraction

    :{
    symbolsnippet :: ByteString
    symbolsnippet = [i|
     <div class="glyph-buttons"><button class="glyph-button glyph-title" data-title="duplicate"><div class="code-font stack-function-button">.</div></button><button class="glyph-button glyph-title" data-title="over"><div class="code-font stack-function-button">,</div></button><button class="glyph-button glyph-title" data-title="(:) flip"><div class="code-font stack-function-button">∶</div></button><button class="glyph-button glyph-title" data-title="pop"><div class="code-font stack-function-button">;</div></button><button class="glyph-button glyph-title" data-title="identity"><div class="code-font stack-function-button">∘</div></button><button class="glyph-button glyph-title" data-title="not"><div class="code-font monadic-function">¬</div></button><button class="glyph-button glyph-title" data-title="sign"><div class="code-font monadic-function">±</div></button><button class="glyph-button glyph-title" data-title="(`) negate"><div class="code-font monadic-function">¯</div></button><button class="glyph-button glyph-title" data-title="absolute value"><div class="code-font monadic-function">⌵</div></button><button class="glyph-button glyph-title" data-title="sqrt"><div class="code-font monadic-function">√</div></button><button class="glyph-button glyph-title" data-title="sine"><div class="code-font monadic-function">○</div></button><button class="glyph-button glyph-title" data-title="floor"><div class="code-font monadic-function">⌊</div></button><button class="glyph-button glyph-title" data-title="ceiling"><div class="code-font monadic-function">⌈</div></button><button class="glyph-button glyph-title" data-title="round"><div class="code-font monadic-function">⁅</div></button><button class="glyph-button glyph-title" data-title="(=) equals"><div class="code-font dyadic-function">=</div></button><button class="glyph-button glyph-title" data-title="(!=) not equals"><div class="code-font dyadic-function">≠</div></button><button class="glyph-button glyph-title" data-title="less than"><div class="code-font dyadic-function">&lt;</div></button><button class="glyph-button glyph-title" data-title="(<=) less or equal"><div class="code-font dyadic-function">≤</div></button><button class="glyph-button glyph-title" data-title="greater than"><div class="code-font dyadic-function">&gt;</div></button><button class="glyph-button glyph-title" data-title="(>=) greater or equal"><div class="code-font dyadic-function">≥</div></button><button class="glyph-button glyph-title" data-title="add"><div class="code-font dyadic-function">+</div></button><button class="glyph-button glyph-title" data-title="subtract"><div class="code-font dyadic-function">-</div></button><button class="glyph-button glyph-title" data-title="(*) multiply"><div class="code-font dyadic-function">×</div></button><button class="glyph-button glyph-title" data-title="(%) divide"><div class="code-font dyadic-function">÷</div></button><button class="glyph-button glyph-title" data-title="modulus"><div class="code-font dyadic-function">◿</div></button><button class="glyph-button glyph-title" data-title="power"><div class="code-font dyadic-function">ⁿ</div></button><button class="glyph-button glyph-title" data-title="logarithm"><div class="code-font dyadic-function">ₙ</div></button><button class="glyph-button glyph-title" data-title="minimum"><div class="code-font dyadic-function">↧</div></button><button class="glyph-button glyph-title" data-title="maximum"><div class="code-font dyadic-function">↥</div></button><button class="glyph-button glyph-title" data-title="atangent"><div class="code-font dyadic-function">∠</div></button><button class="glyph-button glyph-title" data-title="length"><div class="code-font monadic-function">⧻</div></button><button class="glyph-button glyph-title" data-title="shape"><div class="code-font monadic-function">△</div></button><button class="glyph-button glyph-title" data-title="range"><div class="code-font monadic-function">⇡</div></button><button class="glyph-button glyph-title" data-title="first"><div class="code-font monadic-function">⊢</div></button><button class="glyph-button glyph-title" data-title="reverse"><div class="code-font monadic-function">⇌</div></button><button class="glyph-button glyph-title" data-title="deshape"><div class="code-font monadic-function">♭</div></button><button class="glyph-button glyph-title" data-title="bits"><div class="code-font monadic-function">⋯</div></button><button class="glyph-button glyph-title" data-title="transpose"><div class="code-font monadic-function trans">⍉</div></button><button class="glyph-button glyph-title" data-title="rise"><div class="code-font monadic-function">⍏</div></button><button class="glyph-button glyph-title" data-title="fall"><div class="code-font monadic-function">⍖</div></button><button class="glyph-button glyph-title" data-title="where"><div class="code-font monadic-function">⊚</div></button><button class="glyph-button glyph-title" data-title="classify"><div class="code-font monadic-function">⊛</div></button><button class="glyph-button glyph-title" data-title="deduplicate"><div class="code-font monadic-function">⊝</div></button><button class="glyph-button glyph-title" data-title="box"><div class="code-font monadic-function">□</div></button><button class="glyph-button glyph-title" data-title="unbox"><div class="code-font monadic-function">⊔</div></button><button class="glyph-button glyph-title" data-title="match"><div class="code-font dyadic-function">≅</div></button><button class="glyph-button glyph-title" data-title="couple"><div class="code-font dyadic-function">⊟</div></button><button class="glyph-button glyph-title" data-title="join"><div class="code-font dyadic-function">⊂</div></button><button class="glyph-button glyph-title" data-title="select"><div class="code-font dyadic-function">⊏</div></button><button class="glyph-button glyph-title" data-title="pick"><div class="code-font dyadic-function">⊡</div></button><button class="glyph-button glyph-title" data-title="reshape"><div class="code-font dyadic-function">↯</div></button><button class="glyph-button glyph-title" data-title="take"><div class="code-font dyadic-function">↙</div></button><button class="glyph-button glyph-title" data-title="drop"><div class="code-font dyadic-function">↘</div></button><button class="glyph-button glyph-title" data-title="rotate"><div class="code-font dyadic-function">↻</div></button><button class="glyph-button glyph-title" data-title="windows"><div class="code-font dyadic-function">◫</div></button><button class="glyph-button glyph-title" data-title="keep"><div class="code-font dyadic-function">▽</div></button><button class="glyph-button glyph-title" data-title="find"><div class="code-font dyadic-function">⌕</div></button><button class="glyph-button glyph-title" data-title="member"><div class="code-font dyadic-function">∊</div></button><button class="glyph-button glyph-title" data-title="indexof"><div class="code-font dyadic-function">⊗</div></button><button class="glyph-button glyph-title" data-title="reduce"><div class="code-font monadic-modifier">/</div></button><button class="glyph-button glyph-title" data-title="fold"><div class="code-font monadic-modifier">∧</div></button><button class="glyph-button glyph-title" data-title="scan"><div class="code-font monadic-modifier">\\</div></button><button class="glyph-button glyph-title" data-title="each"><div class="code-font monadic-modifier">∵</div></button><button class="glyph-button glyph-title" data-title="rows"><div class="code-font monadic-modifier">≡</div></button><button class="glyph-button glyph-title" data-title="distribute"><div class="code-font monadic-modifier">∺</div></button><button class="glyph-button glyph-title" data-title="table"><div class="code-font monadic-modifier">⊞</div></button><button class="glyph-button glyph-title" data-title="cross"><div class="code-font monadic-modifier">⊠</div></button><button class="glyph-button glyph-title" data-title="repeat"><div class="code-font monadic-modifier">⍥</div></button><button class="glyph-button glyph-title" data-title="group"><div class="code-font monadic-modifier">⊕</div></button><button class="glyph-button glyph-title" data-title="partition"><div class="code-font monadic-modifier">⊜</div></button><button class="glyph-button glyph-title" data-title="invert"><div class="code-font monadic-modifier">⍘</div></button><button class="glyph-button glyph-title" data-title="gap"><div class="code-font monadic-modifier">⋅</div></button><button class="glyph-button glyph-title" data-title="dip"><div class="code-font monadic-modifier">⊙</div></button><button class="glyph-button glyph-title" data-title="both"><div class="code-font monadic-modifier">∩</div></button><button class="glyph-button glyph-title" data-title="fork"><div class="code-font dyadic-modifier">⊃</div></button><button class="glyph-button glyph-title" data-title="bracket"><div class="code-font dyadic-modifier">⊓</div></button><button class="glyph-button glyph-title" data-title="under"><div class="code-font dyadic-modifier">⍜</div></button><button class="glyph-button glyph-title" data-title="level"><div class="code-font dyadic-modifier">⍚</div></button><button class="glyph-button glyph-title" data-title="fill"><div class="code-font dyadic-modifier">⬚</div></button><button class="glyph-button glyph-title" data-title="bind"><div class="code-font dyadic-modifier">'</div></button><button class="glyph-button glyph-title" data-title="if"><div class="code-font dyadic-modifier">?</div></button><button class="glyph-button glyph-title" data-title="try"><div class="code-font dyadic-modifier">⍣</div></button><button class="glyph-button glyph-title" data-title="assert"><div class="code-font dyadic-function">⍤</div></button><button class="glyph-button glyph-title" data-title="call"><div class="code-font variadic-function-button">!</div></button><button class="glyph-button glyph-title" data-title="break"><div class="code-font monadic-function">⎋</div></button><button class="glyph-button glyph-title" data-title="recur"><div class="code-font monadic-function">↬</div></button><button class="glyph-button glyph-title" data-title="random"><div class="code-font noadic-function-button">⚂</div></button><button class="glyph-button glyph-title" data-title="eta"><div class="code-font noadic-function-button">η</div></button><button class="glyph-button glyph-title" data-title="pi"><div class="code-font noadic-function-button">π</div></button><button class="glyph-button glyph-title" data-title="tau"><div class="code-font noadic-function-button">τ</div></button><button class="glyph-button glyph-title" data-title="infinity"><div class="code-font noadic-function-button">∞</div></button><button class="glyph-button glyph-title" data-title="trace"><div class="code-font stack-function-button">~</div></button><button class="glyph-button strand-span" data-title="strand">_</button><button class="glyph-button " data-title="array">[]</button><button class="glyph-button " data-title="box array">{}</button><button class="glyph-button " data-title="function">()</button><button class="glyph-button number-literal-span" data-title="negative (`)">¯</button><button class="glyph-button string-literal-span" data-title="character">@</button><button class="glyph-button string-literal-span" data-title="format/multiline string">$</button><button class="glyph-button string-literal-span" data-title="string">"</button><button class="glyph-button " data-title="binding (=)">←</button><button class="glyph-button " data-title="signature / terminate modifier">|</button><button class="glyph-button comment-span" data-title="comment"></button><!----></div>
    |]
    :}

    bs = elements (markup_ Html symbolsnippet) !! 1
    ts = [x | (Content x) <- toList bs]

    ts

    [".",",","\226\136\182",";","\226\136\152","\194\172","\194\177","\194\175","\226\140\181","\226\136\154","\226\151\139","\226\140\138","\226\140\136","\226\129\133","=","\226\137\160","&lt;","\226\137\164","&gt;","\226\137\165","+","-","\195\151","\195\183","\226\151\191","\226\129\191","\226\130\153","\226\134\167","\226\134\165","\226\136\160","\226\167\187","\226\150\179","\226\135\161","\226\138\162","\226\135\140","\226\153\173","\226\139\175","\226\141\137","\226\141\143","\226\141\150","\226\138\154","\226\138\155","\226\138\157","\226\150\161","\226\138\148","\226\137\133","\226\138\159","\226\138\130","\226\138\143","\226\138\161","\226\134\175","\226\134\153","\226\134\152","\226\134\187","\226\151\171","\226\150\189","\226\140\149","\226\136\138","\226\138\151","/","\226\136\167","\\","\226\136\181","\226\137\161","\226\136\186","\226\138\158","\226\138\160","\226\141\165","\226\138\149","\226\138\156","\226\141\152","\226\139\133","\226\138\153","\226\136\169","\226\138\131","\226\138\147","\226\141\156","\226\141\154","\226\172\154","'","?","\226\141\163","\226\141\164","!","\226\142\139","\226\134\172","\226\154\130","\206\183","\207\128","\207\132","\226\136\158","~","_","[]","{}","()","\194\175","@","$","\"","\226\134\144","|"]

    mapM_ print ts

    "."
    ","
    "\226\136\182"
    ";"
    "\226\136\152"
    "\194\172"
    "\194\177"
    "\194\175"
    "\226\140\181"
    "\226\136\154"
    "\226\151\139"
    "\226\140\138"
    "\226\140\136"
    "\226\129\133"
    "="
    "\226\137\160"
    "&lt;"
    "\226\137\164"
    "&gt;"
    "\226\137\165"
    "+"
    "-"
    "\195\151"
    "\195\183"
    "\226\151\191"
    "\226\129\191"
    "\226\130\153"
    "\226\134\167"
    "\226\134\165"
    "\226\136\160"
    "\226\167\187"
    "\226\150\179"
    "\226\135\161"
    "\226\138\162"
    "\226\135\140"
    "\226\153\173"
    "\226\139\175"
    "\226\141\137"
    "\226\141\143"
    "\226\141\150"
    "\226\138\154"
    "\226\138\155"
    "\226\138\157"
    "\226\150\161"
    "\226\138\148"
    "\226\137\133"
    "\226\138\159"
    "\226\138\130"
    "\226\138\143"
    "\226\138\161"
    "\226\134\175"
    "\226\134\153"
    "\226\134\152"
    "\226\134\187"
    "\226\151\171"
    "\226\150\189"
    "\226\140\149"
    "\226\136\138"
    "\226\138\151"
    "/"
    "\226\136\167"
    "\\"
    "\226\136\181"
    "\226\137\161"
    "\226\136\186"
    "\226\138\158"
    "\226\138\160"
    "\226\141\165"
    "\226\138\149"
    "\226\138\156"
    "\226\141\152"
    "\226\139\133"
    "\226\138\153"
    "\226\136\169"
    "\226\138\131"
    "\226\138\147"
    "\226\141\156"
    "\226\141\154"
    "\226\172\154"
    "'"
    "?"
    "\226\141\163"
    "\226\141\164"
    "!"
    "\226\142\139"
    "\226\134\172"
    "\226\154\130"
    "\206\183"
    "\207\128"
    "\207\132"
    "\226\136\158"
    "~"
    "_"
    "[]"
    "{}"
    "()"
    "\194\175"
    "@"
    "$"
    "\""
    "\226\134\144"
    "|"

    padSymbols = ".,:◌∘¬±¯⌵√∿⌊⌈⁅=≠<≤>≥+-×÷◿ⁿₙ↧↥∠ℂ⧻△⇡⊢⇌♭¤⋯⍉⍏⍖⊚⊛◴◰□⋕≍⊟⊂⊏⊡↯☇↙↘↻◫▽⌕⦷∊⊗⟔/∧\\∵≡⊞⍚⍥⊕⊜◇⋅⊙⟜⊸∩°⍜⊃⊓⍢⬚⍣⍤⚂ηπτ∞?⸮_[]{}()⟨⟩‿¯@$\"!^←↚~|#" :: ByteString

    T.putStrLn $ decodeUtf8Lenient padSymbols

    .,:����5?
    E=`<d>e+-������ �����m��IOV������M�����������
    ��/'\5a�Ze���řܸ)�\��bcd����?._[]{}()��?�@$"!^��~|#

    padSymbols == (C.pack $ C.unpack padSymbols)

    True

    encodeUtf8 ".,:◌∘¬±¯⌵√∿⌊⌈⁅=≠<≤>≥+-×÷◿ⁿₙ↧↥∠ℂ⧻△⇡⊢⇌♭¤⋯⍉⍏⍖⊚⊛◴◰□⋕≍⊟⊂⊏⊡↯☇↙↘↻◫▽⌕⦷∊⊗⟔/∧\\∵≡⊞⍚⍥⊕⊜◇⋅⊙⟜⊸∩°⍜⊃⊓⍢⬚⍣⍤⚂ηπτ∞?⸮_[]{}()⟨⟩‿¯@$\"!^←↚~|#"
    -- print $ C.singleton <$> C.unpack padSymbols

    ".,:\226\151\140\226\136\152\194\172\194\177\194\175\226\140\181\226\136\154\226\136\191\226\140\138\226\140\136\226\129\133=\226\137\160<\226\137\164>\226\137\165+-\195\151\195\183\226\151\191\226\129\191\226\130\153\226\134\167\226\134\165\226\136\160\226\132\130\226\167\187\226\150\179\226\135\161\226\138\162\226\135\140\226\153\173\194\164\226\139\175\226\141\137\226\141\143\226\141\150\226\138\154\226\138\155\226\151\180\226\151\176\226\150\161\226\139\149\226\137\141\226\138\159\226\138\130\226\138\143\226\138\161\226\134\175\226\152\135\226\134\153\226\134\152\226\134\187\226\151\171\226\150\189\226\140\149\226\166\183\226\136\138\226\138\151\226\159\148/\226\136\167\\\226\136\181\226\137\161\226\138\158\226\141\154\226\141\165\226\138\149\226\138\156\226\151\135\226\139\133\226\138\153\226\159\156\226\138\184\226\136\169\194\176\226\141\156\226\138\131\226\138\147\226\141\162\226\172\154\226\141\163\226\141\164\226\154\130\206\183\207\128\207\132\226\136\158?\226\184\174_[]{}()\226\159\168\226\159\169\226\128\191\194\175@$\"!^\226\134\144\226\134\154~|#"

    print $ decodeUtf8Lenient $ encodeUtf8 ".,:◌∘¬±¯⌵√∿⌊⌈⁅=≠<≤>≥+-×÷◿ⁿₙ↧↥∠ℂ⧻△⇡⊢⇌♭¤⋯⍉⍏⍖⊚⊛◴◰□⋕≍⊟⊂⊏⊡↯☇↙↘↻◫▽⌕⦷∊⊗⟔/∧\\∵≡⊞⍚⍥⊕⊜◇⋅⊙⟜⊸∩°⍜⊃⊓⍢⬚⍣⍤⚂ηπτ∞?⸮_[]{}()⟨⟩‿¯@$\"!^←↚~|#"
    -- print $ C.singleton <$> C.unpack padSymbols

    ".,:\9676\8728\172\177\175\9013\8730\8767\8970\8968\8261=\8800<\8804>\8805+-\215\247\9727\8319\8345\8615\8613\8736\8450\10747\9651\8673\8866\8652\9837\164\8943\9033\9039\9046\8858\8859\9716\9712\9633\8917\8781\8863\8834\8847\8865\8623\9735\8601\8600\8635\9707\9661\8981\10679\8714\8855\10196/\8743\\\8757\8801\8862\9050\9061\8853\8860\9671\8901\8857\10204\8888\8745\176\9052\8835\8851\9058\11034\9059\9060\9858\951\960\964\8734?\11822_[]{}()\10216\10217\8255\175@$\"!^\8592\8602~|#"

    import Data.Text (Text)
    import Data.Text qualified as T
    import Data.Text.IO qualified as T
    symText =  ".,:◌∘¬±¯⌵√∿⌊⌈⁅=≠<≤>≥+-×÷◿ⁿₙ↧↥∠ℂ⧻△⇡⊢⇌♭¤⋯⍉⍏⍖⊚⊛◴◰□⋕≍⊟⊂⊏⊡↯☇↙↘↻◫▽⌕⦷∊⊗⟔/∧\\∵≡⊞⍚⍥⊕⊜◇⋅⊙⟜⊸∩°⍜⊃⊓⍢⬚⍣⍤⚂ηπτ∞?⸮_[]{}()⟨⟩‿¯@$\"!^←↚~|#" :: Text

    T.putStrLn symText

    .,:◌∘¬±¯⌵√∿⌊⌈⁅=≠<≤>≥+-×÷◿ⁿₙ↧↥∠ℂ⧻△⇡⊢⇌♭¤⋯⍉⍏⍖⊚⊛◴◰□⋕≍⊟⊂⊏⊡↯☇↙↘↻◫▽⌕⦷∊⊗⟔/∧\∵≡⊞⍚⍥⊕⊜◇⋅⊙⟜⊸∩°⍜⊃⊓⍢⬚⍣⍤⚂ηπτ∞?⸮_[]{}()⟨⟩‿¯@$"!^←↚~|#

