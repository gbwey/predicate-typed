{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{- |
     orphan Lift instances for Data.Time
-}
module Predicate.TH_Orphans () where
import Language.Haskell.TH.Syntax
import Data.Time
import Data.Fixed
import qualified Language.Haskell.TH.Lift as TL

deriving instance Lift Day
deriving instance Lift LocalTime
deriving instance Lift ZonedTime
deriving instance Lift TimeZone
deriving instance Lift TimeOfDay
deriving instance Lift (Fixed a)

$(TL.deriveLift ''DiffTime)

deriving instance Lift UTCTime


