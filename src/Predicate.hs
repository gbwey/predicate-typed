-- | Provides a type-level Dsl for refinement types
--
--   "Predicate.Refined2" and "Predicate.Refined3" hold the more advanced refinement types allowing changes to the input type
--
module Predicate (
   module Predicate.Core
 , module Predicate.Prelude
 , module Predicate.Util
 , module Predicate.Util_TH
 , module Predicate.Refined
 ) where
import Predicate.Core
import Predicate.Util
import Predicate.Util_TH
import Predicate.Prelude
import Predicate.Refined
import Predicate.TH_Orphans ()