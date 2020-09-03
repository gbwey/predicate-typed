# predicate-typed: a refinement type library

[![Hackage](https://img.shields.io/hackage/v/predicate-typed.svg?colorB=5d0ef0&style=flat)](https://hackage.haskell.org/package/predicate-typed)

what this library provides:
1. a rich dsl for building refinement types eg regex expressions / arithmetic / datetime manipulation etc
1. visualisation of each step in the evaluation of an expression using a colorized tree
1. three types of Refinement types Refined/Refined2/Refined3
1. Template Haskell methods for creating the refinement types
1. Aeson (FromJSON ToJSON) instances
1. Read and Show instances
1. Binary instances
1. Hashable instances
1. IsString instances
1. Database encoders and decoders for refinement types (sqlhandler-odbc)

To run the examples you will need these settings (ghc>=8.6)
```haskell
:set -XTypeApplications
:set -XDataKinds
:set -XPolyKinds
:set -XTemplateHaskell
:set -XNoStarIsType
:set -XTypeFamilies
```

[Refined](Refined.md)

*[Refined2](Refined2.md)

[Refined3](Refined3.md)

# General information

* **_opts_** common display options for evaluating the typelevel expression
  * OZ no output:zero
  * OL one line:lite
  * OA ascii plus colors
  * OAN ascii without colors
  * OU unicode plus colors (for Windows: chcp 65001)

* BoolT is a GADT that holds the return value from evaluating the type level expression
   * TrueT : predicate is true
   * FalseT : predicate is false
   * PresentT a : 'a' is any other value
   * FailT String : indicates a failure with an error message

# testing the dsl

 * pu  is a shortcut for run @OU  (unicode with colors)
 * pa  is a shortcut for run @OA  (ascii with colors)
 * pl  is a shortcut for run @OL  (short one liner)
 * pan is a shortcut for run @OAN (ascii without colors)

```haskell
> pu @(Between 4 10 Id) 7
True 4 <= 7 <= 10
|
+- P Id 7
|
+- P '4
|
`- P '10
TrueT
```

```haskell
> pu @(Between 4 10 Id) 11
False 11 <= 10
|
+- P Id 11
|
+- P '4
|
`- P '10
FalseT
```

```haskell
> pu @(Between (4 % 7) (10 % 2) Id) 7
...
False (7 % 1 <= 5 % 1)
FalseT
```

```haskell
> pu @(Re "^[[:upper:]][[:lower:]]+" Id) "Fred"
...
TrueT
```

```haskell
pu @(Re "^[[:upper:]][[:lower:]]+" Id) "fred"
...
FalseT
```

```haskell
> pu @(Resplit "\\s+" Id >> GuardSimple (Len > 0 && All (Re "^[[:upper:]][[:lower:]]+" Id) Id)) "Fred Abel Bart Jimmy"
...
PresentT ["Fred","Abel","Bart","Jimmy"]
```

```haskell
> pu @(Resplit "\\s+" Id >> GuardSimple (Len > 0 && All (Re "^[[:upper:]][[:lower:]]+" Id) Id)) "Fred Abel bart Jimmy"
...
FailT "(True && False | (All(4) i=2 (Re' [] (^[[:upper:]][[:lower:]]+) | bart)))"
```

```haskell
>pu @(ReadP Day Id >> ToWeekDate Id >> Snd Id == "Monday") "2020-07-13"
...
TrueT
```

```haskell
> pu @(ReadP Day Id >> ToWeekDate Id >> Snd Id == "Monday") "2020-07-14"
...
False (>>) False | {"Tuesday" == "Monday"}
FalseT
```

```haskell
> pu @(ReadP Day Id >> ToWeekDate Id >> GuardSimple (Snd Id == "Monday")) "2020-07-13"
...
PresentT (1,"Monday")
```


