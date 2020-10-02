### Refined is the simplest refinement type which validates the input

(see documentation [doctests](src/Predicate/Refined.hs))

:load Predicate

```haskell
newtype Refined opts p a
```
* **_opt_** display options see [README](README.md)
* **_p_** predicate on **_a_**
* **_a_** input type

1. reads in a number and checks to see that it is greater than 99
```haskell
>newRefined @OL @(ReadP Int Id > 99) "123"
Right (Refined "123")
```

2. tries to read in a number but fails
```haskell
>newRefined @OU @(ReadP Int Id > 99) "1x2y3"
Left (FailT "ReadP Int (1x2y3) failed")
```

3. reads in a hexadecimal string and checks to see that it is between 99 and 256
```haskell
--  (>>) forward composition
>newRefined @OL @(ReadBase Int 16 >> Between 99 256 Id) "000fe"
Right (Refined "000fe")
```

4. reads in a hexadecimal string but fails the predicate check
```haskell
>newRefined @OL @(ReadBase Int 16 >> Between 99 253 Id) "000fe"
Left PresentT False
```

5. same as 4. above but now we get details of where it went wrong
```haskell
>newRefined @OU @(ReadBase Int 16 >> Between 99 253 Id) "000fe"
```

6. reads in a string as time and does simple validation
```haskell
>newRefined @OL @(Resplit ":" >> Map (ReadP Int Id) Id >> Len == 3) "12:01:05"
Right (Refined "12:01:05")
```
  * `Resplit ":"`
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
>pu @(Resplit ":" >> Map (ReadP Int Id) Id >> Len == 3) "12:01:05"

>pu @(Resplit ":") "12:01:05"

>pu @(Map (ReadP Int Id) Id) ["12","01","05"]

>pu @(Len == 3) [12,1,5]
```

```haskell
ex1 :: Refined OL (ReadP Int Id > 99) String
ex1 = $$(refinedTH "123")
```

```haskell
>$$(refinedTH @OU @(Lt 3 || Gt 55) 44)

<interactive>:36:4: error:
    * refinedTH: predicate failed with PresentT False (False || False | (44 < 3) || (44 > 55))
False:False || False | (44 < 3) || (44 > 55)
|
+- False:44 < 3
|  |
|  +- I
|  |
|  `- '3
|
`- False:44 > 55
   |
   +- I
   |
   `- '55

    * In the Template Haskell splice
        $$(refinedTH @OU @(Lt 3 || Gt 55) 44)
      In the expression: $$(refinedTH @OU @(Lt 3 || Gt 55) 44)
```

```haskell
>$$(refinedTH @OU @(Len > 7 || Elem 3 Id) [1..5])
Refined [1,2,3,4,5]
it :: Refined ((Len > 7) || Elem 3 Id) [Int]
```

```haskell
>$$(refinedTH @OU @(Len > 7 || Elem 7 Id) [1..5])


<interactive>:31:4: error:
    * refinedTH: predicate failed with PresentT False (False || False | (5 > 7) || (7 `elem` [1,2,3,4,5]))
False:False || False | (5 > 7) || (7 `elem` [1,2,3,4,5])
|
+- False:5 > 7
|  |
|  +- Len 5
|  |
|  `- '7
|
`- False:7 `elem` [1,2,3,4,5]
   |
   +- '7
   |
   `- Id [1,2,3,4,5]

    * In the Template Haskell splice
        $$(refinedTH @OU @(Len > 7 || Elem 7 Id) [1 .. 5])
      In the expression:
        $$(refinedTH @OU @(Len > 7 || Elem 7 Id) [1 .. 5])
```

```haskell
>$$(refinedTH @OU @(Re "^[A-Z][a-z]+$") "smith")

<interactive>:32:4: error:
    * refinedTH: predicate failed with PresentT False (Re (^[A-Z][a-z]+$))
False:Re (^[A-Z][a-z]+$)
|
+- '"^[A-Z][a-z]+$"
|
`- Id "smith"

    * In the Template Haskell splice
        $$(refinedTH @OU @(Re "^[A-Z][a-z]+$") "smith")
      In the expression:
        $$(refinedTH @OU @(Re "^[A-Z][a-z]+$") "smith")
```

```haskell
>$$(refinedTH @OU @(Re "^[A-Z][a-z]+$") "Smith")
Refined "Smith"
```

```haskell
>$$(refinedTH @OU @(Msg "expected title case" $ Re "^[A-Z][a-z]+$") "smith")

<interactive>:34:4: error:
    * refinedTH: predicate failed with PresentT False (expected title case Re (^[A-Z][a-z]+$))
False:expected title case Re (^[A-Z][a-z]+$)
|
+- '"^[A-Z][a-z]+$"
|
`- Id "smith"

    * In the Template Haskell splice
        $$(refinedTH
             @OU @(Msg "expected title case" $ Re "^[A-Z][a-z]+$") "smith")
      In the expression:
        $$(refinedTH
             @OU @(Msg "expected title case" $ Re "^[A-Z][a-z]+$") "smith")
```

```haskell
>$$(refinedTH @OU @(GuardBool "expected title case" (Re "^[A-Z][a-z]+$")) "smith")

<interactive>:22:4: error:
    * refinedTH: predicate failed with FailT expected title case (GuardBool (Re (^[A-Z][a-z]+$)))
[Error expected title case] GuardBool (Re (^[A-Z][a-z]+$))
|
+- False:Re (^[A-Z][a-z]+$)
|  |
|  +- '"^[A-Z][a-z]+$"
|  |
|  `- Id "smith"
|
`- '"expected title case"

    * In the Template Haskell splice
        $$(refinedTH
             @OU @(GuardBool "expected title case" (Re "^[A-Z][a-z]+$"))
             "smith")
      In the expression:
        $$(refinedTH
             @OU @(GuardBool "expected title case" (Re "^[A-Z][a-z]+$"))
             "smith")
```
