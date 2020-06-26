# predicate-typed

[![Hackage](https://img.shields.io/hackage/v/predicate-typed.svg?colorB=5d0ef0&style=flat)](https://hackage.haskell.org/package/predicate-typed)

what this library provides:
1. a dsl for building refinement types
2. Refined is a simple refinement type that validates the input against a predicate
3. Refined2 has an extra internal type which can be different from the input
4. Refined3 has an extra parameter to control output formatting
5. validation against input values
6. visualisation of each step in the evaluation of an expression
7  template haskell methods for creating the refinement types at compile time
8. ToJSON and FromJSON instances
9. Read and Show instances
10. Binary instances
11. Hashable instances
12. IsString instances
13. quickcheck arbitrary methods for Refined and Refined3
14. database encoders and decoders using hdbc(sqlhandler-odbc) or odbc(sqlhandler-odbcalt)

To run all the examples you will need these settings (ghc>=8.2)
```haskell
:set -XTypeApplications
:set -XDataKinds
:set -XPolyKinds
:set -XTemplateHaskell
:set -XNoStarIsType -- if ghc 8.6 or greater
```

### Simplest refinement type
```haskell
data Refined p a = Refined a
```
* **_a_** is the input type
* **_p_** predicate on **_a_**

### Examples of Refined (for more information see [doctests](src/Predicate/Refined.hs))

* if using unix you can replace 'oa' with 'ou' to get unicode output
* 'ol' summarises the output in one line
* 'oa' shows the tree of output with colors

1. reads in a number and checks to see that it is greater than 99
```haskell
>prtRefinedIO @(ReadP Int Id > 99) ol "123"
Right (Refined {unRefined = "123"})
```

2. tries to read in a number but fails
```haskell
>prtRefinedIO @(ReadP Int Id > 99) ol "1x2y3"
Left (FailP "ReadP Int (1x2y3) failed")
```

3. reads in a hexadecimal string and checks to see that it is between 99 and 256
```haskell
--  (>>) acts like the monadic operator (>>=)
>prtRefinedIO @(ReadBase Int 16 Id >> Between 99 256 Id) ol "000fe"
Right (Refined {unRefined = "000fe"})
```

4. reads in a hexadecimal string but fails the predicate check
```haskell
>prtRefinedIO @(ReadBase Int 16 Id >> Between 99 253 Id) ol "000fe"
Left FalseP
```

5. same as 4. above but now we get details of where it went wrong
```haskell
>prtRefinedIO @(ReadBase Int 16 Id >> Between 99 253 Id) oa "000fe"
```

6. reads in a string as time and does simple validation
```haskell
>prtRefinedIO @(Resplit ":" Id >> Map (ReadP Int Id) Id >> Len == 3) ol "12:01:05"
Right (Refined {unRefined = "12:01:05"})
```
  * `Resplit ":" Id`
     split using regex using a colon as a delimiter  ["12","01","05"]
  * `Map (ReadP Int Id) Id`
     Read in the values as Ints                      [12,1,5]
  * `Len == 3`
     Check to see that the length of the list of Ints is 3

### Testing out predicates
When using _Refined_ the expression in _p_ must result in a True/False\
_pe2_ does not have that restriction so you can run the whole thing or the individual pieces\
for less detail use _pl_\

```haskell
>pa @(Resplit ":" Id >> Map (ReadP Int Id) Id >> Len == 3) "12:01:05"

>pa @(Resplit ":" Id) "12:01:05"

>pa @(Map (ReadP Int Id) Id) ["12","01","05"]

>pa @(Len == 3) [12,1,5]
```

### An example using Refined2 (for more information see [doctests](src/Predicate/Refined2.hs))

```haskell
>type Hex = '(ReadBase Int 16 Id, Between 0 255 Id, String)

>prtEval2P (Proxy @Hex) ol "0000fe"
Refined2 {r2In = 254, r2Out = "0000fe"}

>prtEval2P (Proxy @Hex) ol "1ffe"
Left "Step 2. False Boolean Check(op) | {8190 <= 255}"

>import qualified Data.Aeson as A
>type Js = '(ParseJson (Int,String) Id, Msg "0-255:" (Between 0 255 (Fst Id)) && Msg "length:" (Length (Snd Id) == 3), String)

>prtEval2P (Proxy @Js) ol "[10,\"Abc\"]"
Right (Refined2 {r2In = (10,"Abc"), r2Out = "[10,\"Abc\"]"})

>prtEval2P (Proxy @Js) ol "[10,\"Abcdef\"]"
Left Step 2. False Boolean Check(op) | {True && False | (length:6 == 3)}

>prtEval2P (Proxy @Js) ol "[-10,\"Abcdef\"]"
Left Step 2. False Boolean Check(op) | {False && False | (0-255:0 <= -10) && (length:6 == 3)}
```

### An example using Refined3 (for more information see [doctests](src/Predicate/Examples/Refined3.hs))

```haskell
>type Hex = '(ReadBase Int 16 Id, Between 0 255 Id, ShowBase 16 Id, String)

>prtEval3PIO (Proxy @Hex) ol "0000fe"
Refined3 {r3In = 254, r3Out = "fe"}
```
1. `ReadBase Int 16 Id`
    reads a hexadecimal string and returns 254
2. `Between 0 255 Id`
    checks to make sure the predicate holds ie the number is between 0 and 255
3. `ShowBase 16 Id`
    formats the output as "fe" which is compatible with the input

run this to get details in color of each evaluation step on failure:
```haskell
>prtEval3PIO (Proxy @Hex) oa "0000ffe"

*** Step 1. Success Initial Conversion(ip) [4094] ***

P ReadBase(Int,16) 4094 | "0000ffe"
|
`- P Id "0000ffe"

*** Step 2. False Boolean Check(op) ***

False 4094 <= 255
|
+- P Id 4094
|
+- P '0
|
`- P '255```

Read in the string "0000fe" as input to `ReadBase Int 16` and produce 254 as output
```haskell
>pa @(ReadBase Int 16 Id) "0000fe"
PresentT 254

>pa @(Between 0 255 Id) 254
TrueT

>pa @(ShowBase 16 Id) 254 = "fe"
PresentT "fe"
```

**_Replace '$$(refinedTH ...)' '$$(refinedTH' oa ...)' for an evaluation tree_**

### Template Haskell versions

```haskell
ex1 :: Refined (ReadP Int Id >> Id > 99) String
ex1 = $$(refinedTH "123")
```

Refined2 and Refined3 are the most useful refinement types as you can control the input and output types (see documentation and [doctests](src/Predicate/Refined2.hs) and [doctests](src/Predicate/Refined3.hs))

**_Replace '$$(refined3TH ...)' with '$$(refined3TH' oa ...)' for a colored evaluation tree_**

```haskell
type Hex = '(ReadBase Int 16 Id, Between 0 255 Id, ShowBase 16 Id, String)

$$(refined3TH "0000fe") :: MakeR3 Hex
```

Here is an example where the predicate fails at compile-time and we choose to show the details using oa.
```haskell
>type Hex = '(ReadBase Int 16 Id, Between 0 255 Id, ShowBase 16 Id, String)

>$$(refined3TH' oa "000ffff") :: MakeR3 Hex

<interactive>:18:4: error:
    *
*** Step 1. Success Initial Conversion(ip) [65535] ***

P ReadBase(Int,16) 65535 | "000ffff"
|
`- P Id "000ffff"

*** Step 2. False Boolean Check(op) ***

False 65535 <= 255
|
+- P Id 65535
|
+- P '0
|
`- P '255

refined3TH: predicate failed with Step 2. False Boolean Check(op) | {65535 <= 255}
    * In the Template Haskell splice $$(refined3TH' oa "000ffff")
      In the expression: $$(refined3TH' oa "000ffff") :: MakeR3 Hex
      In an equation for `it':
          it = $$(refined3TH' oa "000ffff") :: MakeR3 Hex
```

### Any valid Read/Show instance can be used with Refined3
```haskell
>$$(refined3TH "13 % 3") :: ReadShowR Rational
Refined3 {r3In = 13 % 3, r3Out = "13 % 3"}

>$$(refined3TH "2016-11-09") :: ReadShowR Day
Refined3 {r3In = 2016-11-09, r3Out = "2016-11-09"}
```

An example of an invalid refined3TH call
```haskell
>$$(refined3TH "2016-xy-09") :: ReadShowR Day

<interactive>:171:4: error:
    * refined3TH: predicate failed with Step 1. Initial Conversion(ip) Failed | ReadP Day (2016-xy-09) failed
    * In the Template Haskell splice $$(refined3TH "2016-xy-09")
      In the expression: $$(refined3TH "2016-xy-09") :: ReadShowR Day
      In an equation for `it':
          it = $$(refined3TH "2016-xy-09") :: ReadShowR Day
```

### Json decoding

#### This example is successful as it is a valid hexadecimal and is between 10 though 256
```haskell
>eitherDecode' @(Refined3 (ReadBase Int 16 Id) (Id > 10 && Id < 256) (ShowP Id) String) "\"00fe\""
Right (Refined3 {r3In = 254, r3Out = "254"})
```

#### This example fails as the value is not a valid hexadecimal string
```haskell
>either putStrLn print $ eitherDecode' @(Refined3 (ReadBase Int 16 Id) 'True (ShowP Id) String) "\"00feg\""
Error in $: Refined3:Step 1. Initial Conversion(ip) Failed | invalid base 16

***Step 1. Initial Conversion(ip) Failed ***

[Error invalid base 16] ReadBase(Int,16) as=00feg err=[(254,"g")]
|
`- P Id "00feg"
```

#### This example fails as the hexadecimal value is valid but is not between 10 and 256

```haskell
>either putStrLn print $ eitherDecode' @(Refined3 (ReadBase Int 16 Id) (Id > 10 && Id < 256) (ShowP Id) String) "\"00fe443a\""
Error in $: Refined3:Step 2. False Boolean Check(op) | {True && False | {16663610 < 256}}

***Step 1. Success Initial Conversion(ip) [16663610] ***

P ReadBase(Int,16) 16663610 | "00fe443a"
|
`- P Id "00fe443a"

***Step 2. False Boolean Check(op) = FalseP ***

False True && False | {16663610 < 256}
|
+- True 16663610 > 10
|  |
|  +- P Id 16663610
|  |
|  `- P '10
|
`- False 16663610 < 256
   |
   +- P Id 16663610
   |
   `- P '256
```

#### some more examples
```
>$$(refinedTH' @(Lt 3 || Gt 55) oa 44)

<interactive>:21:4: error:
    *
False False || False | (44 < 3) || (44 > 55)
|
+- False 44 < 3
|  |
|  +- P I
|  |
|  `- P '3
|
`- False 44 > 55
   |
   +- P I
   |
   `- P '55

refinedTH: predicate failed with FalseP (False || False | (44 < 3) || (44 > 55))
    * In the Template Haskell splice
        $$(refinedTH' @(Lt 3 || Gt 55) oa 44)
      In the expression: $$(refinedTH' @(Lt 3 || Gt 55) oa 44)
      In an equation for `it': it = $$(refinedTH' @(Lt 3 || Gt 55) oa 44)
```

```
>$$(refinedTH' @(Len > 7 || Elem 3 Id) oa [1..5])
Refined {unRefined = [1,2,3,4,5]}
it :: Refined ((Len > 7) || Elem 3 Id) [Int]
```

```
>$$(refinedTH' @(Len > 7 || Elem 7 Id) oa [1..5])

<interactive>:26:4: error:
    *
False False || False | (5 > 7) || (7 `elem` [1,2,3,4,5])
|
+- False 5 > 7
|  |
|  +- P Len 5 | [1,2,3,4,5]
|  |
|  `- P '7
|
`- False 7 `elem` [1,2,3,4,5]
   |
   +- P '7
   |
   `- P Id [1,2,3,4,5]

refinedTH: predicate failed with FalseP (False || False | (5 > 7) || (7 `elem` [1,2,3,4,5]))
    * In the Template Haskell splice
        $$(refinedTH' @(Len > 7 || Elem 7 Id) oa [1 .. 5])
      In the expression:
        $$(refinedTH' @(Len > 7 || Elem 7 Id) oa [1 .. 5])
      In an equation for `it':
          it = $$(refinedTH' @(Len > 7 || Elem 7 Id) oa [1 .. 5])
```

```
>$$(refinedTH' @(Re "^[A-Z][a-z]+$" Id) oa "smith")

<interactive>:30:4: error:
    *
False Re' [] (^[A-Z][a-z]+$) | smith
|
+- P '^[A-Z][a-z]+$
|
`- P Id "smith"

refinedTH: predicate failed with FalseP (Re' [] (^[A-Z][a-z]+$) | smith)
    * In the Template Haskell splice
        $$(refinedTH' @(Re "^[A-Z][a-z]+$" Id) oa "smith")
      In the expression:
        $$(refinedTH' @(Re "^[A-Z][a-z]+$" Id) oa "smith")
      In an equation for `it':
          it = $$(refinedTH' @(Re "^[A-Z][a-z]+$" Id) oa "smith")
```

```
>$$(refinedTH' @(Re "^[A-Z][a-z]+$" Id) oa "Smith")
Refined {unRefined = "Smith"}
```

```
>$$(refinedTH' @(Msg "expected title case:" $ Re "^[A-Z][a-z]+$" Id) oa "smith")

<interactive>:36:4: error:
    *
False expected title case:Re' [] (^[A-Z][a-z]+$) | smith
|
+- P '^[A-Z][a-z]+$
|
`- P Id "smith"

refinedTH: predicate failed with FalseP (expected title case:Re' [] (^[A-Z][a-z]+$) | smith)
    * In the Template Haskell splice
        $$(refinedTH'
             @(Msg "expected title case:" $ Re "^[A-Z][a-z]+$" Id) oa "smith")
      In the expression:
        $$(refinedTH'
             @(Msg "expected title case:" $ Re "^[A-Z][a-z]+$" Id) oa "smith")
      In an equation for `it':
          it
            = $$(refinedTH'
                   @(Msg "expected title case:" $ Re "^[A-Z][a-z]+$" Id) oa "smith")
```

```
>$$(refinedTH' @(Guard "expected title case:" (Re "^[A-Z][a-z]+$" Id) >> True) oa "smith")

<interactive>:52:4: error:
    *
[Error expected title case:] (>>) lhs failed
|
`- [Error expected title case:] Guard(failed) [expected title case:] | "smith"
   |
   `- False Re' [] (^[A-Z][a-z]+$) | smith
      |
      +- P '^[A-Z][a-z]+$
      |
      `- P Id "smith"

refinedTH: predicate failed with FailP "expected title case:" ((>>) lhs failed)
    * In the Template Haskell splice
        $$(refinedTH'
             @(Guard "expected title case:" (Re "^[A-Z][a-z]+$" Id) >> True)
             oa
             "smith")
      In the expression:
        $$(refinedTH'
             @(Guard "expected title case:" (Re "^[A-Z][a-z]+$" Id) >> True)
             oa
             "smith")
      In an equation for `it':
          it
            = $$(refinedTH'
                   @(Guard "expected title case:" (Re "^[A-Z][a-z]+$" Id) >> True)
                   oa
                   "smith")
```