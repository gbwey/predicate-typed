{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
-- | orphan Lift instances for Data.Time
module Predicate.TH_Orphans () where
import Language.Haskell.TH.Syntax (Lift)
import Data.Time
import Data.Fixed (Fixed(..))
import qualified Language.Haskell.TH.Lift as TL
import Data.Proxy (Proxy)
import Data.These (These(..))

deriving instance Lift Day
deriving instance Lift LocalTime
deriving instance Lift ZonedTime
deriving instance Lift TimeZone
deriving instance Lift TimeOfDay
deriving instance Lift (Fixed a)
deriving instance Lift Ordering
deriving instance (Lift a, Lift b) => Lift (These a b)

$(TL.deriveLift ''DiffTime)

deriving instance Lift UTCTime

$(TL.deriveLift ''Proxy)



