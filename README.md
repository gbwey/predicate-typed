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
$$(refinedTH "123") :: Refined (ReadP Int >> Id > 99) String
```

2. reads in a number but fails at compile-time
```haskell
$$(refinedTH "1x2x3") :: Refined (ReadP Int >> Id > 99) String
```

3. reads in a hexadecimal string and checks to see that it is between 99 and 256
```haskell
$$(refinedTH "000fe") :: Refined (ReadBase Int 16 >> Between 99 256) String
```

4. reads in a hexadecimal string but fails the predicate check so doesnt compile
```haskell
$$(refinedTH "000fe") :: Refined (ReadBase Int 16 >> Between 99 253) String
```

5. same as 4. above but now we get details of where it went wrong
```haskell
$$(refinedTH' o2 "000fe") :: Refined (ReadBase Int 16 >> Between 99 253) String
```

6. reads in a string as time and does simple validation
```haskell
$$(refinedTH "12:01:05") :: Refined (Resplit ":" >> Map (ReadP Int) >> Len == 3) String
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
pe2 (Resplit ":" >> Map (ReadP Int) >> Len == 3) "12:01:05"
pe2 @(Resplit ":") "12:01:05"
pe2 @(Map (ReadP Int)) ["12","01","05"]
pe2 @(Map (Len == 3) [12,1,5]
```

### An example using Refined3 (for more information see [doctests](src/Refined3.hs))

```haskell
type Hex = '(ReadBase Int 16, Between 0 255, ShowBase 16, String)

$$(refined3TH "0000fe") :: MakeR3 Hex
Refined3 {in3 = 255, out3 = "fe"}
```
1. `ReadBase Int 16`
    reads a hexadecimal string and returns 254
2. `Between 0 255`
    checks to make sure the predicate holds ie the number is between 0 and 255
3. `ShowBase 16`
    formats the output as "fe" which is compatible with the input

run this to get details in color of each evaluation step:
`$$(refined3TH' o2 "0000fe") :: MakeR3 Hex`

Here we read in the string "0000fe" as input to `ReadBase Int 16` and produce 254 as output
```haskell
pe2 @(ReadBase Int 16) "0000fe" == 254

pe2 @(Between 0 255) 254 = True

pe2 @(ShowBase 16) 254 = "fe"
```

