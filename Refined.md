### Refined is the simplest refinement type which validates the input

(see documentation [doctests](src/Predicate/Refined.hs))

:load Predicate

```haskell
newtype Refined opt p a
```
* **_opt_** display options see [README](README.md)
* **_p_** predicate on **_a_**
* **_a_** input type

1. reads in a number and checks to see that it is greater than 99
```haskell
>prtRefinedIO @OL @(ReadP Int Id > 99) "123"
Right (Refined "123")
```

2. tries to read in a number but fails
```haskell
>prtRefinedIO @OU @(ReadP Int Id > 99) "1x2y3"
Left (FailP "ReadP Int (1x2y3) failed")
```

3. reads in a hexadecimal string and checks to see that it is between 99 and 256
```haskell
--  (>>) forward composition
>prtRefinedIO @OL @(ReadBase Int 16 Id >> Between 99 256 Id) "000fe"
Right (Refined "000fe")
```

4. reads in a hexadecimal string but fails the predicate check
```haskell
>prtRefinedIO @OL @(ReadBase Int 16 Id >> Between 99 253 Id) "000fe"
Left FalseP
```

5. same as 4. above but now we get details of where it went wrong
```haskell
>prtRefinedIO @OU @(ReadBase Int 16 Id >> Between 99 253 Id) "000fe"
```

6. reads in a string as time and does simple validation
```haskell
>prtRefinedIO @OL @(Resplit ":" Id >> Map (ReadP Int Id) Id >> Len == 3) "12:01:05"
Right (Refined "12:01:05")
```
  * `Resplit ":" Id`
     split using regex using a colon as a delimiter  ["12","01","05"]
  * `Map (ReadP Int Id) Id`
     Read in the values as Ints                      [12,1,5]
  * `Len == 3`
     Check to see that the length of the list of Ints is 3

### Testing out predicates
When using _Refined_ the expression in _p_ must result in a True/False\
_pab_ does not have that restriction so you can run the whole thing or the individual pieces\
for less detail use _pl_\

```haskell
>pu @(Resplit ":" Id >> Map (ReadP Int Id) Id >> Len == 3) "12:01:05"

>pu @(Resplit ":" Id) "12:01:05"

>pu @(Map (ReadP Int Id) Id) ["12","01","05"]

>pu @(Len == 3) [12,1,5]
```

```haskell
ex1 :: Refined OL (ReadP Int Id >> Id > 99) String
ex1 = $$(refinedTH "123")
```

```
>$$(refinedTH @OU @(Lt 3 || Gt 55) 44)

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
        $$(refinedTH @OU @(Lt 3 || Gt 55) 44)
      In the expression: $$(refinedTH @OU @(Lt 3 || Gt 55) 44)
      In an equation for `it': it = $$(refinedTH OU @(Lt 3 || Gt 55) 44)
```

```
>$$(refinedTH @OU @(Len > 7 || Elem 3 Id) [1..5])
Refined [1,2,3,4,5]
it :: Refined ((Len > 7) || Elem 3 Id) [Int]
```

```
>$$(refinedTH @OU @(Len > 7 || Elem 7 Id) [1..5])

<interactive>:26:4: error:
    *
False False || False | (5 > 7) || (7 `elem` [1,2,3,4,5])
|
+- False 5 > 7
|  |
|  +- P Len 5
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
        $$(refinedTH @OU @(Len > 7 || Elem 7 Id) [1 .. 5])
      In the expression:
        $$(refinedTH @OU @(Len > 7 || Elem 7 Id) [1 .. 5])
      In an equation for `it':
          it = $$(refinedTH @OU @(Len > 7 || Elem 7 Id) [1 .. 5])
```

```
>$$(refinedTH @OU @(Re "^[A-Z][a-z]+$" Id) "smith")

<interactive>:30:4: error:
    *
False Re' [] (^[A-Z][a-z]+$)
|
+- P '"^[A-Z][a-z]+$"
|
`- P Id "smith"

refinedTH: predicate failed with FalseP (Re' [] (^[A-Z][a-z]+$) | smith)
    * In the Template Haskell splice
        $$(refinedTH @OU @(Re "^[A-Z][a-z]+$" Id) "smith")
      In the expression:
        $$(refinedTH @OU @(Re "^[A-Z][a-z]+$" Id) "smith")
      In an equation for `it':
          it = $$(refinedTH @OU @(Re "^[A-Z][a-z]+$" Id) "smith")
```

```
>$$(refinedTH @OU @(Re "^[A-Z][a-z]+$" Id) "Smith")
Refined "Smith"
```

```
>$$(refinedTH @OU @(Msg "expected title case:" $ Re "^[A-Z][a-z]+$" Id) "smith")

<interactive>:36:4: error:
    *
False expected title case:Re' [] (^[A-Z][a-z]+$)
|
+- P '^[A-Z][a-z]+$
|
`- P Id "smith"

refinedTH: predicate failed with FalseP (expected title case:Re' [] (^[A-Z][a-z]+$) | smith)
    * In the Template Haskell splice
        $$(refinedTH @OU
             @(Msg "expected title case:" $ Re "^[A-Z][a-z]+$" Id) "smith")
      In the expression:
        $$(refinedTH @OU
             @(Msg "expected title case:" $ Re "^[A-Z][a-z]+$" Id) "smith")
      In an equation for `it':
          it
            = $$(refinedTH @OU
                   @(Msg "expected title case:" $ Re "^[A-Z][a-z]+$" Id) "smith")
```

```
>$$(refinedTH @OU @(GuardBool "expected title case:" (Re "^[A-Z][a-z]+$" Id)) "smith")

<interactive>:52:4: error:
    *
[Error expected title case:]
|
`- [Error expected title case:] GuardBool | "smith"
   |
   `- False Re' [] (^[A-Z][a-z]+$)
      |
      +- P '^[A-Z][a-z]+$
      |
      `- P Id "smith"

refinedTH: predicate failed with FailP "expected title case:
    * In the Template Haskell splice
        $$(refinedTH @OU
             @(GuardBool "expected title case:" (Re "^[A-Z][a-z]+$" Id))
             "smith")
      In the expression:
        $$(refinedTH @OU
             @(GuardBool "expected title case:" (Re "^[A-Z][a-z]+$" Id))
             "smith")
      In an equation for `it':
          it
            = $$(refinedTH @OU
                   @(GuardBool "expected title case:" (Re "^[A-Z][a-z]+$" Id))
                   "smith")
```
