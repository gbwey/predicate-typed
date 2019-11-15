{- |
     Provides a type-level Dsl for refinement types

     "Predicate.Refined2" and "Predicate.Refined3" contain the advanced refinement types where you can convert the input type

-}
module Predicate (
   module Predicate.Core
 , module Predicate.Prelude
 , module Predicate.Util
 , module Predicate.Refined
 ) where
import Predicate.Core
import Predicate.Util
import Predicate.Prelude
import Predicate.Refined