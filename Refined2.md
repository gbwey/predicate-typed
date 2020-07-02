Refined2 allows control the input type (see documentation [doctests](src/Predicate/Refined2.hs))

:load Predicate.Examples.Refined2

### An example using Refined2 (for more information see [doctests](src/Predicate/Refined2.hs))

```haskell
data Refined2 opts ip op i
```
* **_opts_** display options eg
        * 'OZ (no output:zero)
        * 'OL (one line:lite)
        * 'OA (ansi colors)
        * 'OU (unicode colors) -- for unicode on windows run: chcp 65001
* **_ip_** converts the external type **_i_** to an internal type
* **_op_** predicate on internal type
* **_i_** input type

:load Predicate.Examples.Refined2

```haskell
>type Hex = '( 'OL, ReadBase Int 16 Id, Between 0 255 Id, String)

>prtEval2P (Proxy @Hex) "0000fe"
Refined2 {r2In = 254, r2Out = "0000fe"}

>prtEval2P (Proxy @Hex) "1ffe"
Left "Step 2. False Boolean Check(op) | {8190 <= 255}"

>import qualified Data.Aeson as A
>import Data.ByteString (ByteString)
>type Js = '( 'OL, ParseJson (Int,String) Id, Msg "0-255:" (Between 0 255 (Fst Id)) && Msg "length:" (Length (Snd Id) == 3), ByteString)

>prtEval2P (Proxy @Js) "[10,\"Abc\"]"
Right (Refined2 {r2In = (10,"Abc"), r2Out = "[10,\"Abc\"]"})

>prtEval2P (Proxy @Js) "[10,\"Abcdef\"]"
Left Step 2. False Boolean Check(op) | {True && False | (length:6 == 3)}

>prtEval2P (Proxy @Js) "[-10,\"Abcdef\"]"
Left Step 2. False Boolean Check(op) | {False && False | (0-255:0 <= -10) && (length:6 == 3)}
```

```haskell
type Hex opts = '(opts, ReadBase Int 16 Id, Between 0 255 Id, String)

$$(refined2TH "0000fe") :: MakeR2 (Hex 'OL)
```

Here is an example where the predicate fails at compile-time and we choose to show the details using 'OU
```haskell
>type Hex opts = '(opts, ReadBase Int 16 Id, Between 0 255 Id, String)

>$$(refined2TH "000ffff") :: MakeR2 (Hex 'OU)

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

refined2TH: predicate failed with Step 2. False Boolean Check(op) | {65535 <= 255}
    * In the Template Haskell splice $$(refined2TH "000ffff")
      In the expression: $$(refined2TH "000ffff") :: MakeR2 (Hex 'OU)
      In an equation for `it':
          it = $$(refined2TH "000ffff") :: MakeR2 Hex
```

### Any valid Read/Show instance can be used with Refined2
```haskell
>$$(refined2TH "13 % 3") :: Refined2 'OU (ReadP Rational Id) (Id > 12 % 4) String
Refined2 {r2In = 13 % 3, r2Out = "13 % 3"}

>$$(refined2TH "2016-11-09") :: Refined2 'OU (ReadP Day Id) (Id > Just (MkDay '(2012,1,1))) String
Refined2 {r2In = 2016-11-09, r2Out = "2016-11-09"}
```

An example of an invalid refined2TH call
```haskell
>$$(refined2TH "2016-xy-09") :: ReadShowR 'OU Day

<interactive>:171:4: error:
    * refined2TH: predicate failed with Step 1. Initial Conversion(ip) Failed | ReadP Day (2016-xy-09) failed
    * In the Template Haskell splice $$(refined2TH "2016-xy-09")
      In the expression: $$(refined2TH "2016-xy-09") :: ReadShowR 'OU Day
      In an equation for `it':
          it = $$(refined2TH "2016-xy-09") :: ReadShowR Day
```

### Json decoding

#### This example is successful as it is a valid hexadecimal and is between 10 though 256
```haskell
>eitherDecode' @(Refined2 'OU (ReadBase Int 16 Id) (Id > 10 && Id < 256) String) "\"00fe\""
Right (Refined2 {r2In = 254, r2Out = "00fe"})
```

#### This example fails as the value is not a valid hexadecimal string
```haskell
>either putStrLn print $ eitherDecode' @(Refined2 'OU (ReadBase Int 16 Id) 'True String) "\"00feg\""
Error in $: Refined2:Step 1. Initial Conversion(ip) Failed | invalid base 16

*** Step 1. Initial Conversion(ip) Failed ***

[Error invalid base 16] ReadBase(Int,16) as=00feg err=[(254,"g")]
|
`- P Id "00feg"

```

#### This example fails as the hexadecimal value is valid but is not between 10 and 256

```haskell
>either putStrLn print $ eitherDecode' @(Refined2 'OU (ReadBase Int 16 Id) (Id > 10 && Id < 256) (ShowP Id) String) "\"00fe443a\""
Error in $: Refined2:Step 2. False Boolean Check(op) | {True && False | (16663610 < 256)}

*** Step 1. Success Initial Conversion(ip) [16663610] ***

P ReadBase(Int,16) 16663610 | "00fe443a"
|
`- P Id "00fe443a"

*** Step 2. False Boolean Check(op) ***

False True && False | (16663610 < 256)
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

