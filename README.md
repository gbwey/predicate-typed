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