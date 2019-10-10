# predicate-typed

what this library provides:
1. a dsl for building refinement types
2. Refined is simple refinement type that just validates the input against a predicate
3. Refined3 is a more complex refinement type that allows you to change the input
4. validation against input values
5. visualisation of each step in the process
6  template haskell methods for creating the refinement types at compile time
7. json encoder and decoders
8. Read and Show instance for Refined and Refined3
9. database encoder decoders using odbc(sqlhandler-odbcalt) or hdbc((sqlhandler-odbc)
10. quickcheck arbitrary instances

type Hex = '(ReadBase Int 16, Between 0 255, ShowBase 16, String)

$$(refined3TH "0000fe") :: MakeR3 Hex
Refined3 {in3 = 255, out3 = "fe"}

1 . ReadBase Int 16
    reads in a hexadecimal String and returns 254
2.  Between 0 255
    checks to make sure the predicate holds ie the number is between 0 and 255
3.  ShowBase 16
    formats the output as "fe" which is compatible with the input

-- for less output use pl or plc instead of pe2
pe2 @(ReadBase Int 16) "0000fe" == 254
pe2 @(Between 0 255) 254 = True
pe2 @(ShowBase 16) 254 = "fe"


data Refined3 ip op fmt i = Refined3 { in3 :: PP ip i, out3 :: PP fmt (PP ip i) }
'i' is the input type
'PP ip i' is the internal type (in3)
'PP op (PP ip i)' must evaluate to a boolean: ie validates the internal type
'PP fmt (PP ip i)' must evaluate back to type 'i': ie formats the output using the internal type

PP op (PP ip i) ~ Bool
PP fmt (PP ip i) ~ i


$$(refinedTH "12:01:05") :: Refined (Resplit ":" >> Map (ReadP Int) >> Len >= 2) String

1. Resplit ":"
   split using regex using a colon as a delimiter  ["12","01","05"]
2. Map (ReadP Int)
   Read in the values as Ints                      [12,1,5]
3. Len >= 2
   Check to see that the length of the ints >= 2   (3>=2)

pe2 @(Resplit ":" >> Map (ReadP Int) >> Len >= 2) "12:01:05"  [returns True]

data Refined p a = Refined a

1. 'a' is the input type
2. PP p a ~ Bool is the predicate on 'a'
