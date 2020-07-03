{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE TypeOperators #-}
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
{-# LANGUAGE TemplateHaskell #-}
{- |
     Contains prepackaged 4-tuples to use with 'Refined2'
-}
module Predicate.Examples.Refined2 (
  -- ** date time checkers
    DateTime1
  , datetime1

  , daten
  , DateN
  , datetimen
  , DateTimeN
  , DateTimeNR

  -- *** time checkers
  , hms
  , Hms
  , HmsR

--  , hms'
  , Hms'
  , HmsR'

  -- ** credit cards
  , Ccn
  , cc11

  -- ** ssn
  , ssn
  , Ssn
  , SsnR

  -- ** ipv4
  , ip4
  , Ip4
  , Ip4R

  , ip4'
  , Ip4'
  , Ip4R'

  -- ** ipv6
  , ip6
  , Ip6
  , Ip6R

 -- ** base n
  , BaseN
  , BaseN'
  , BaseIJ
  , BaseIJ'
  , BaseIJip
   ) where
import Predicate.Core
import Predicate.Refined
import Predicate.Refined2
import Predicate.Examples.Common
import Predicate.Prelude
import Predicate.Util
import Predicate.Util_TH
import Predicate.TH_Orphans ()
import GHC.TypeLits (Nat)
import Data.Time
import Data.Kind
import Data.Proxy

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :m + Predicate.Refined2
-- >>> :m + Predicate.Util

-- | credit card with luhn algorithm
--
-- >>> prtEval2 @'OZ @Ccip @(Ccop 11) "1234-5678-901"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> prtEval2 @'OZ @Ccip @(Ccop 11) "1234-5678-903"
-- Right (Refined2 {r2In = [1,2,3,4,5,6,7,8,9,0,3], r2Out = "1234-5678-903"})
--
-- >>> pz @(Ccip >> Ccop 11) "79927398713"
-- TrueT
--
-- >>> pz @(Ccip >> Ccop 10) "79927398713"
-- FailT "expected 10 digits but found 11"
--
type Ccn (opts :: OptT) (n :: Nat) = '(opts, Ccip, Ccop n, String)

-- | read in a valid datetime
--
-- >>> prtEval2 @'OL @(Dtip LocalTime) @'True "2018-09-14 02:57:04"
-- Right (Refined2 {r2In = 2018-09-14 02:57:04, r2Out = "2018-09-14 02:57:04"})
--
-- >>> prtEval2 @'OL @(Dtip LocalTime) @'True "2018-09-99 12:12:12"
-- Left Step 1. Initial Conversion(ip) Failed | ParseTimeP LocalTime (%F %T) failed to parse
--
datetime1 :: Proxy (DateTime1 opts t)
datetime1 = mkProxy2

type DateTime1 (opts :: OptT) (t :: Type) = '(opts, Dtip t, 'True, String)

datetimen :: OptTC opts => Proxy (DateTimeN opts)
datetimen = mkProxy2'

-- valid dates for for DateFmts are "2001-01-01" "Jan 24 2009" and "03/29/07"
type DateN (opts :: OptT) = '(opts,ParseTimes Day DateFmts Id, 'True, String)

daten :: OptTC opts => Proxy (DateN opts)
daten = mkProxy2'

type DateTimeNR (opts :: OptT) = MakeR2 (DateTimeN opts)
type DateTimeN (opts :: OptT) = '(opts, ParseTimes UTCTime DateTimeFmts Id, 'True, String)

-- | read in an ssn
--
-- >>> prtEval2 @'OZ @Ssnip @Ssnop "134-01-2211"
-- Right (Refined2 {r2In = [134,1,2211], r2Out = "134-01-2211"})
--
-- >>> prtEval2 @'OL @Ssnip @Ssnop "666-01-2211"
-- Left Step 2. False Boolean Check(op) | {Bool(0) [number for group 0 invalid: found 666] (True && False | (666 /= 666))}
--
-- >>> prtEval2 @'OL @Ssnip @Ssnop "667-00-2211"
-- Left Step 2. False Boolean Check(op) | {Bool(1) [number for group 1 invalid: found 0] (1 <= 0)}
--

ssn :: OptTC opts => Proxy (Ssn opts)
ssn = mkProxy2'

type SsnR (opts :: OptT) = MakeR2 (Ssn opts)
type Ssn (opts :: OptT) = '(opts, Ssnip, Ssnop, String)


-- | read in a time and validate it
--
-- >>> prtEval2 @'OL @Hmsip @Hmsop' "23:13:59"
-- Right (Refined2 {r2In = [23,13,59], r2Out = "23:13:59"})
--
-- >>> prtEval2 @'OL @Hmsip @Hmsop' "23:13:60"
-- Left Step 2. False Boolean Check(op) | {Bool(2) [seconds] (60 <= 59)}
--
-- >>> prtEval2 @'OL @Hmsip @Hmsop' "26:13:59"
-- Left Step 2. False Boolean Check(op) | {Bool(0) [hours] (26 <= 23)}
--
hms :: OptTC opts => Proxy (Hms opts)
hms = mkProxy2'

type HmsR (opts :: OptT) = MakeR2 (Hms opts)
type Hms (opts :: OptT) = '(opts, Hmsip, Hmsop >> 'True, String)

--hms' :: Proxy (Hms' 'OZ)
--hms' = mkProxy2'

type HmsR' (opts :: OptT) = MakeR2 (Hms' opts)
type Hms' (opts :: OptT) = '(opts, Hmsip, Hmsop', String)

-- | read in an ipv4 address and validate it
--
-- >>> prtEval2 @'OZ @Ip4ip @Ip4op' "001.223.14.1"
-- Right (Refined2 {r2In = [1,223,14,1], r2Out = "001.223.14.1"})
--
-- >>> prtEval2 @'OL @Ip4ip @Ip4op' "001.223.14.999"
-- Left Step 2. False Boolean Check(op) | {Bool(3) [octet 3 out of range 0-255 found 999] (999 <= 255)}
--
-- >>> prtEval2P (ip4 @'OL) "001.223.14.999"
-- Left Step 2. Failed Boolean Check(op) | octet 3 out of range 0-255 found 999
--
-- >>> prtEval2P (ip4 @'OL) "001.223.14.999.1"
-- Left Step 2. Failed Boolean Check(op) | Guards:invalid length(5) expected 4
--
-- >>> prtEval2P (ip4 @'OL) "001.257.14.1"
-- Left Step 2. Failed Boolean Check(op) | octet 1 out of range 0-255 found 257
--
type Ip4R (opts :: OptT) = MakeR2 (Ip4 opts)
type Ip4 (opts :: OptT) = '(opts, Ip4ip, Ip4op >> 'True, String) -- guards

ip4 :: Proxy (Ip4 opts)
ip4 = Proxy

type Ip4R' (opts :: OptT) = MakeR2 (Ip4' opts)
type Ip4' (opts :: OptT) = '(opts, Ip4ip, Ip4op', String) -- boolean predicates

ip4' :: Proxy (Ip4' opts)
ip4' = Proxy

type Ip6R (opts :: OptT) = MakeR2 (Ip6 opts)
type Ip6 (opts :: OptT) = '(opts, Ip6ip, Ip6op, String) -- guards

ip6 :: Proxy (Ip6 opts)
ip6 = Proxy


cc11 :: Proxy (Ccn opts 11)
cc11 = Proxy

-- | convert a string from a given base \'i\' and store it internally as an base 10 integer
--
-- >>> prtEval2 @'OZ @(ReadBase Int 16 Id) @'True "00fe"
-- Right (Refined2 {r2In = 254, r2Out = "00fe"})
--
-- >>> prtEval2 @'OZ @(ReadBase Int 16 Id) @(Between 100 400 Id) "00fe"
-- Right (Refined2 {r2In = 254, r2Out = "00fe"})
--
-- >>> prtEval2 @'OZ @(ReadBase Int 16 Id) @(GuardSimple (Id < 400) >> 'True) "f0fe"
-- Left Step 2. Failed Boolean Check(op) | (61694 < 400)
--
-- >>> prtEval2 @'OL @(ReadBase Int 16 Id) @(Id < 400) "f0fe" -- todo: why different parens vs braces
-- Left Step 2. False Boolean Check(op) | {61694 < 400}
--
type BaseN (opts :: OptT) (n :: Nat) = BaseN' opts n 'True
type BaseN' (opts :: OptT) (n :: Nat) p = '(opts,ReadBase Int n Id, p, String)


-- | Luhn check
--
-- >>> prtEval2 @'OZ @Luhnip @(Luhnop 4) "1230"
-- Right (Refined2 {r2In = [1,2,3,0], r2Out = "1230"})
--
-- >>> prtEval2 @'OL @Luhnip @(Luhnop 4) "1234"
-- Left Step 2. False Boolean Check(op) | {True && False | (Luhn map=[4,6,2,2] sum=14 ret=4 | [1,2,3,4])}
--
-- | uses builtin 'Luhn'

-- | convert a string from a given base \'i\' and store it internally as a base \'j\' string
--
-- >>> prtEval2 @'OZ @(BaseIJip 16 2) @'True "fe"
-- Right (Refined2 {r2In = "11111110", r2Out = "fe"})
--
-- >>> prtEval2 @'OZ @(BaseIJip 16 2) @'True "fge"
-- Left Step 1. Initial Conversion(ip) Failed | invalid base 16
--
-- >>> prtEval2 @'OL @(BaseIJip 16 2) @(ReadBase Int 2 Id < 1000) "ffe"
-- Left Step 2. False Boolean Check(op) | {4094 < 1000}
--
type BaseIJip (i :: Nat) (j :: Nat) = ReadBase Int i Id >> ShowBase j Id

type BaseIJ (i :: Nat) (j :: Nat) = BaseIJ' i j 'True
type BaseIJ' (i :: Nat) (j :: Nat) p = '(ReadBase Int i Id >> ShowBase j Id, p, String)

-- | take any valid Read/Show instance and turn it into a valid 'Predicate.Refined2.Refined2'
--
-- >>> :m + Data.Ratio
-- >>> prtEval2 @'OZ @(ReadP Rational Id) @'True "13 % 3"
-- Right (Refined2 {r2In = 13 % 3, r2Out = "13 % 3"})
--
-- >>> prtEval2 @'OZ @(ReadP Rational Id) @'True "13x % 3"
-- Left Step 1. Initial Conversion(ip) Failed | ReadP Ratio Integer (13x % 3)
--
-- >>> prtEval2 @'OZ @(ReadP Rational Id) @(3 % 1 <..> 5 % 1) "13 % 3"
-- Right (Refined2 {r2In = 13 % 3, r2Out = "13 % 3"})
--
-- >>> prtEval2 @'OZ @(ReadP Rational Id) @(11 -% 2 <..> 3 -% 1) "-13 % 3"
-- Right (Refined2 {r2In = (-13) % 3, r2Out = "-13 % 3"})
--
-- >>> prtEval2 @'OZ @(ReadP Rational Id) @(Id > (15 % 1)) "13 % 3"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> prtEval2 @'OL @(ReadP Rational Id) @(Msg (PrintF "invalid=%3.2f" (FromRational Double Id)) (Id > (15 % 1))) "13 % 3"
-- Left Step 2. False Boolean Check(op) | {invalid=4.3313 % 3 > 15 % 1}
--
-- >>> prtEval2 @'OZ @(ReadP Rational Id) @(Id > (11 % 1)) "13 % 3"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> let tmString = "2018-10-19 14:53:11.5121359 UTC"
-- >>> let tm = read tmString :: UTCTime
-- >>> prtEval2 @'OZ @(ReadP UTCTime Id) @'True tmString
-- Right (Refined2 {r2In = 2018-10-19 14:53:11.5121359 UTC, r2Out = "2018-10-19 14:53:11.5121359 UTC"})
--
-- >>> :m + Data.Aeson
-- >>> prtEval2 @'OZ @(ReadP Value Id) @'True "String \"jsonstring\""
-- Right (Refined2 {r2In = String "jsonstring", r2Out = "String \"jsonstring\""})
--
-- >>> prtEval2 @'OZ @(ReadP Value Id) @'True "Number 123.4"
-- Right (Refined2 {r2In = Number 123.4, r2Out = "Number 123.4"})
--
