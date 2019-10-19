# predicate-typed

what this library provides:
1. a dsl for building refinement types
2. Refined is simple refinement type that just validates the input against a predicate
3. Refined3 is a more complex refinement type that allows you to change the input
4. validation against input values
5. visualisation of each step in the process
6  template haskell methods for creating the refinement types at compile time
7. ToJSON and FromJSON instances for Refined and Refined3
8. Read and Show instance for Refined and Refined3
9. database encoders and decoders using odbc(sqlhandler-odbcalt) or hdbc((sqlhandler-odbc)
10. quickcheck arbitrary methods

```haskell
data Refined p a = Refined a
```
* **_a_** is the input type
* **_p_** predicate on **_a_**

### Examples of Refined (for more information see [doctests](src/Refined.hs))
1. reads in a number and checks to see that it is greater than 99
```haskell
>prtRefinedIO @(ReadP Int >> Id > 99) ol "123"
Right (Refined {unRefined = "123"})
```

2. reads in a number but fails at compile-time
```haskell
>prtRefinedIO @(ReadP Int >> Id > 99) ol "1x2y3"
Left (FailP "ReadP Int (1x2y3) failed")
```

3. reads in a hexadecimal string and checks to see that it is between 99 and 256
```haskell
>prtRefinedIO @(ReadBase Int 16 >> Between 99 256) ol "000fe"
Right (Refined {unRefined = "000fe"})
```

4. reads in a hexadecimal string but fails the predicate check so doesnt compile
```haskell
>prtRefinedIO @(ReadBase Int 16 >> Between 99 253) ol "000fe"
Left FalseP
```

5. same as 4. above but now we get details of where it went wrong
```haskell
>prtRefinedIO @(ReadBase Int 16 >> Between 99 253) o2 "000fe"
```

6. reads in a string as time and does simple validation
```haskell
>prtRefinedIO @(Resplit ":" >> Map (ReadP Int) >> Len == 3) ol "12:01:05"
Right (Refined {unRefined = "12:01:05"})
```
  * `Resplit ":"`
     split using regex using a colon as a delimiter  ["12","01","05"]
  * `Map (ReadP Int)`
     Read in the values as Ints                      [12,1,5]
  * `Len == 3`
     Check to see that the length of the list of Ints is 3


### Testing out predicates
When using _Refined_ the expression in _p_ must result in a True/False\
_pe2_ does not have that restriction so you can run the whole thing or the individual pieces\
(for less detail use _pl_)

```haskell
>pe2 @(Resplit ":" >> Map (ReadP Int) >> Len == 3) "12:01:05"

>pe2 @(Resplit ":") "12:01:05"

>pe2 @(Map (ReadP Int)) ["12","01","05"]

>pe2 @(Len == 3) [12,1,5]
```

### An example using Refined3 (for more information see [doctests](src/Refined3.hs) and [doctests](src/Refined3Helper.hs))

```haskell
>type Hex = '(ReadBase Int 16, Between 0 255, ShowBase 16, String)

>prtEval3PIO (Proxy @Hex) ol "0000fe"
Refined3 {in3 = 254, out3 = "fe"}
```
1. `ReadBase Int 16`
    reads a hexadecimal string and returns 254
2. `Between 0 255`
    checks to make sure the predicate holds ie the number is between 0 and 255
3. `ShowBase 16`
    formats the output as "fe" which is compatible with the input

run this to get details in color of each evaluation step:
```haskell
>prtEval3PIO (Proxy @Hex) o2 "0000fe"

***Step 1. Success Initial Conversion(ip) [254] ***

P ReadBase(Int) 16 254 | "0000fe"
|
`- P Id "0000fe"

***Step 2. Success Boolean Check(op) ***

True  True && True
|
+- True  254 >= 0
|  |
|  +- P I
|  |
|  `- P '0
|
`- True  254 <= 255
   |
   +- P I
   |
   `- P '255

***Step 3. Success Output Conversion(fmt) = "fe" ***

P ShowBase 16 fe | 254
```

Read in the string "0000fe" as input to `ReadBase Int 16` and produce 254 as output
```haskell
>pe2 @(ReadBase Int 16) "0000fe"
PresentT 254

>pe2 @(Between 0 255) 254
TrueT

>pe2 @(ShowBase 16) 254 = "fe"
PresentT "fe"
```

### Template Haskell versions

```haskell
ex1 :: Refined (ReadP Int >> Id > 99) String
ex1 = $$(refinedTH "123")
```

```haskell
type Hex = '(ReadBase Int 16, Between 0 255, ShowBase 16, String)

ex2 :: MakeR3 Hex
ex2 = $$(refined3TH "0000fe")
```

### Any valid Read/Show instance can be used with Refined3
```haskell
>$$(refined3TH "13 % 3") :: ReadShowR Rational
Refined3 {r3In = 13 % 3, r3Out = "13 % 3"}
```

### Json decoding

#### This example is successful as it is a valid hexadecimal and is in the range 10 though 256
```haskell
>eitherDecode' @(Refined3 (ReadBase Int 16) (Id > 10 && Id < 256) ShowP String) "\"00fe\""
Right (Refined3 {in3 = 254, out3 = "254"})
```

#### This example fails as the value is not a valid hexadecimal string
```haskell
>either putStrLn print $ eitherDecode' @(Refined3 (ReadBase Int 16) 'True ShowP String) "\"00feg\""
Error in $: Refined3:Step 1. Initial Conversion(ip) Failed | invalid base 16

***Step 1. Initial Conversion(ip) Failed ***

[Error invalid base 16] ReadBase(Int) 16 as=00feg err=[(254,"g")]
|
`- P Id "00feg"
```

#### This example fails as the hexadecimal value is valid but is not between 10 and 256

```haskell
>either putStrLn print $ eitherDecode' @(Refined3 (ReadBase Int 16) (Id > 10 && Id < 256) ShowP String) "\"00fe443a\""
Error in $: Refined3:Step 2. False Boolean Check(op) | FalseP

***Step 1. Success Initial Conversion(ip) [16663610] ***

P ReadBase(Int,16) 16663610 | "00fe443a"
|
`- P Id "00fe443a"

***Step 2. False Boolean Check(op) = FalseP ***

False True && False
|
+- True  16663610 > 10
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
