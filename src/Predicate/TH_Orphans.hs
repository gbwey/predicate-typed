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
import System.Random
import Data.Proxy

deriving instance Lift Day
deriving instance Lift LocalTime
deriving instance Lift ZonedTime
deriving instance Lift TimeZone
deriving instance Lift TimeOfDay
deriving instance Lift (Fixed a)

$(TL.deriveLift ''DiffTime)

deriving instance Lift UTCTime

$(TL.deriveLift ''StdGen)

$(TL.deriveLift ''Proxy)



