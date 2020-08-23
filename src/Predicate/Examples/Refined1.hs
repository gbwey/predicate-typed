{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     Contains imports needed for 'Refined1'
-}
module Predicate.Examples.Refined1 () where
import Predicate.Examples.Common
import Predicate.Refined
import Predicate.Refined1
import Predicate.Refined3
import Predicate.Examples.Refined3
import Predicate
import Data.Proxy
import GHC.TypeLits (KnownNat, Nat)
import Data.Kind (Type)
import Data.Time

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XTemplateHaskell
-- >>> :m + Data.Ratio

-- | refined1 tests
--
-- >>> newRefined1P (readshow @OZ @Rational) "13 % 3"
-- Right (Refined1 (13 % 3))
--
-- >>> newRefined1P (readshow @OZ @Rational) "13x % 3"
-- Left "Step 1. Initial Conversion(ip) Failed | ReadP Ratio Integer (13x % 3)"
--
-- >>> newRefined1P (Proxy @(ReadShow' OZ Rational (11 -% 2 <..> 3 -% 1))) "-13 % 3"
-- Right (Refined1 ((-13) % 3))
--
