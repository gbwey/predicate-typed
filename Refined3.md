Refined3 validates after first converting to an internal value then creates a canonical value as output
Both the internal value and the formatted output are stored in Refined3
(see documentation [doctests](src/Predicate/Refined3.hs))

:load Predicate.Examples.Refined3

### An example using Refined3 (for more information see [doctests](src/Predicate/Examples/Refined3.hs))

```haskell
data Refined3 opt ip op fmt i
```
* **_opt_** display options see [README](README.md)
* **_ip_** converts the external type **_i_** to an internal type
* **_op_** predicate on the internal type
* **_fmt_** converts the internal type back to the external type (canonical value)
* **_i_** input type

converts a base 16 String to an Int and validates that the number is between 0 and 255
and then roundtrips the value to a string

```haskell
>type Hex opt = '(opt, ReadBase Int 16 Id, Between 0 0xff Id, ShowBase 16 Id, String)

>prtEval3PIO (Proxy @(Hex OL)) "0000fe"
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
>prtEval3PIO (Proxy @(Hex OU)) "0000ffe"

*** Step 1. Success Initial Conversion(ip) [4094] ***

P ReadBase(Int,16) 4094
|
`- P Id "0000ffe"

*** Step 2. False Boolean Check(op) ***

False 4094 <= 255
|
+- P Id 4094
|
+- P '0
|
`- P '255
```

Read in the string "0000fe" as input to `ReadBase Int 16` and produce 254 as output
```haskell
>pu @(ReadBase Int 16 Id) "0000fe"
PresentT 254

>pu @(Between 0 255 Id) 254
TrueT

>pu @(ShowBase 16 Id) 254 = "fe"
PresentT "fe"
```


```haskell
type Hex opt = '(opt, ReadBase Int 16 Id, Between 0 0xff Id, ShowBase 16 Id, String)

$$(refined3TH "0000fe") :: MakeR3 (Hex OL)
```

Here is an example where the predicate fails at compile-time and we choose to show the details using OU
```haskell
>type Hex opt = '(opt, ReadBase Int 16 Id, Between 0 0xff Id, ShowBase 16 Id, String)

>$$(refined3TH "000ffff") :: MakeR3 (Hex OU)

<interactive>:18:4: error:
    *
*** Step 1. Success Initial Conversion(ip) (65535) ***

P ReadBase(Int,16) 65535
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
    * In the Template Haskell splice $$(refined3TH "000ffff")
      In the expression: $$(refined3TH "000ffff") :: MakeR3 (Hex OU)
      In an equation for `it':
          it = $$(refined3TH "000ffff") :: MakeR3 Hex
```

### Any valid Read/Show instance can be used with Refined3
```haskell
>$$(refined3TH "13 % 3") :: ReadShowR OU Rational
Refined3 {r3In = 13 % 3, r3Out = "13 % 3"}

>$$(refined3TH "2016-11-09") :: ReadShowR OU Day
Refined3 {r3In = 2016-11-09, r3Out = "2016-11-09"}
```

An example of an invalid refined3TH call
```haskell
>$$(refined3TH "2016-xy-09") :: ReadShowR OU Day

<interactive>:719:4: error:
    *
*** Step 1. Initial Conversion(ip) Failed ***

[Error ReadP Day (2016-xy-09)]
|
`- P Id "2016-xy-09"

refined3TH: predicate failed with Step 1. Initial Conversion(ip) Failed | ReadP Day (2016-xy-09)
    * In the Template Haskell splice $$(refined3TH "2016-xy-09")
      In the expression:
          $$(refined3TH "2016-xy-09") :: ReadShowR  OU Day
      In an equation for `it`:
          it = $$(refined3TH "2016-xy-09") :: ReadShowR  OU Day
```

### Json decoding

#### This example is successful as it is a valid hexadecimal and is between 10 though 256
```haskell
>eitherDecode' @(Refined3 OU (ReadBase Int 16 Id) (Id > 10 && Id < 256) (ShowP Id) String) "\"00fe\""
Right (Refined3 {r3In = 254, r3Out = "254"})
```

#### This example fails as the value is not a valid hexadecimal string
```haskell
>either putStrLn print $ eitherDecode' @(Refined3 OU (ReadBase Int 16 Id) 'True (ShowP Id) String) "\"00feg\""
Error in $: Refined3:Step 1. Initial Conversion(ip) Failed | invalid base 16

***Step 1. Initial Conversion(ip) Failed ***

[Error invalid base 16] ReadBase(Int,16) as=00feg err=[(254,"g")]
|
`- P Id "00feg"
```

#### This example fails as the hexadecimal value is valid but is not between 10 and 256

```haskell
>either putStrLn print $ eitherDecode' @(Refined3 OU (ReadBase Int 16 Id) (Id > 10 && Id < 256) (ShowP Id) String) "\"00fe443a\""
Error in $: Refined3:Step 2. False Boolean Check(op) | {True && False | {16663610 < 256}}

***Step 1. Success Initial Conversion(ip) (16663610) ***

P ReadBase(Int,16) 16663610
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

