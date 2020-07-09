{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     Mainly contains useful Template Haskell Lift instances for Date Time
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
--instance Lift DiffTime where
--  lift x = return $ LitE (IntegerL $ diffTimeToPicoseconds x)

deriving instance Lift UTCTime


