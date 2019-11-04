{- |
     Provides a type-level Dsl for refinement types

     To use refinement types you will need to also import "Predicate.Refined" and/or "Predicate.Refined3"
-}
module Predicate (
   module Predicate.Core
 , module Predicate.Prelude
 , module Predicate.Util
 ) where
import Predicate.Core
import Predicate.Util
import Predicate.Prelude
