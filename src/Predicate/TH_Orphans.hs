{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PolyKinds #-}
-- | orphan Lift instances for Data.Time, Proxy, These and Ordering
module Predicate.TH_Orphans () where
import Language.Haskell.TH.Syntax (Lift)
import qualified Language.Haskell.TH.Syntax as TH
import Data.Time
import Data.Fixed (Fixed(..))
#if MIN_VERSION_template_haskell(2,16,0)
import qualified Language.Haskell.TH.Lift as TL
#endif
import qualified Language.Haskell.TH.Lib as TLib
import Data.Proxy (Proxy(..))
import Data.These (These(..))

deriving instance Lift Day
deriving instance Lift (Fixed a)
deriving instance Lift LocalTime
deriving instance Lift ZonedTime
deriving instance Lift TimeZone
deriving instance Lift TimeOfDay
deriving instance Lift Ordering
deriving instance (Lift a, Lift b) => Lift (These a b)
deriving instance Lift UTCTime

instance Lift DiffTime where
    lift = TH.liftData
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = TH.unsafeTExpCoerce . TL.lift
#endif
instance Lift (Proxy (a :: k)) where
    lift Proxy = TLib.conE 'Proxy
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = TH.unsafeTExpCoerce . TL.lift
#endif
