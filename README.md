# predicate-typed

[![Hackage](https://img.shields.io/hackage/v/predicate-typed.svg?colorB=5d0ef0&style=flat)](https://hackage.haskell.org/package/predicate-typed)

what this library provides:
1. a dsl for building refinement types
2. Refined is a simple refinement type that validates the input against a predicate
3. *Refined2 has an extra parameter to convert to an internal type
4. Refined3 has an extra parameter for formatting to a canonical value
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

To run the examples you will need these settings (ghc>=8.6)
```haskell
:set -XTypeApplications
:set -XDataKinds
:set -XPolyKinds
:set -XTemplateHaskell
:set -XNoStarIsType
```

[Refined](Refined.md)

[Refined2](Refined2.md)

[Refined3](Refined3.md)