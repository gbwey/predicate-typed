{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module TH_Orphans where
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Instances () -- other orphans
import qualified Language.Haskell.TH.Lift as TL
import Data.Time
import Data.Fixed

deriving instance TH.Lift Day
deriving instance TH.Lift LocalTime
deriving instance TH.Lift TimeOfDay
deriving instance TH.Lift (Fixed a)

deriving instance TH.Lift UTCTime

$(TL.deriveLift ''DiffTime)

