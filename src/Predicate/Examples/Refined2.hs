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

  -- ** isbn10
  , isbn10
  , Isbn10
  , Isbn10R

  -- ** isbn13
  , isbn13
  , Isbn13
  , Isbn13R

 -- ** base n
  , BaseN
  , BaseN'
  , BaseIJ
  , BaseIJ'
  , BaseIJip
   ) where
import Predicate.Refined
import Predicate.Refined2
import Predicate.Examples.Common
import Predicate
import GHC.TypeLits (Nat)
import Data.Time
import Data.Kind
import Data.Proxy

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XTemplateHaskell

-- | credit card with luhn algorithm
--
-- >>> newRefined2 @OZ @Ccip @(Ccop 11) "1234-5678-901"
-- Left "Step 2. False Boolean Check(op) | FalseP"
--
-- >>> newRefined2 @OZ @Ccip @(Ccop 11) "1234-5678-903"
-- Right (Refined2 {r2In = [1,2,3,4,5,6,7,8,9,0,3], r2Out = "1234-5678-903"})
--
-- >>> pz @(Ccip >> Ccop 11) "79927398713"
-- TrueT
--
-- >>> pz @(Ccip >> Ccop 10) "79927398713"
-- FailT "expected 10 digits but found 11"
--
type Ccn (opts :: Opt) (n :: Nat) = '(opts, Ccip, Ccop n, String)

-- | read in a valid datetime
--
-- >>> newRefined2 @OL @(Dtip LocalTime) @'True "2018-09-14 02:57:04"
-- Right (Refined2 {r2In = 2018-09-14 02:57:04, r2Out = "2018-09-14 02:57:04"})
--
-- >>> newRefined2 @OL @(Dtip LocalTime) @'True "2018-09-99 12:12:12"
-- Left "Step 1. Initial Conversion(ip) Failed | ParseTimeP LocalTime (%F %T) failed to parse"
--
datetime1 :: Proxy (DateTime1 opts t)
datetime1 = mkProxy2

type DateTime1 (opts :: Opt) (t :: Type) = '(opts, Dtip t, 'True, String)

datetimen :: OptC opts => Proxy (DateTimeN opts)
datetimen = mkProxy2'

-- valid dates for for DateFmts are "2001-01-01" "Jan 24 2009" and "03/29/07"
type DateN (opts :: Opt) = '(opts,ParseTimes Day DateFmts Id, 'True, String)

daten :: OptC opts => Proxy (DateN opts)
daten = mkProxy2'

type DateTimeNR (opts :: Opt) = MakeR2 (DateTimeN opts)
type DateTimeN (opts :: Opt) = '(opts, ParseTimes UTCTime DateTimeFmts Id, 'True, String)

-- | read in an ssn
--
-- >>> newRefined2 @OZ @Ssnip @Ssnop "134-01-2211"
-- Right (Refined2 {r2In = [134,1,2211], r2Out = "134-01-2211"})
--
-- >>> newRefined2 @OL @Ssnip @Ssnop "666-01-2211"
-- Left "Step 2. False Boolean Check(op) | {Bool(0) [number for group 0 invalid: found 666] (True && False | (666 /= 666))}"
--
-- >>> newRefined2 @OL @Ssnip @Ssnop "667-00-2211"
-- Left "Step 2. False Boolean Check(op) | {Bool(1) [number for group 1 invalid: found 0] (1 <= 0)}"
--

ssn :: OptC opts => Proxy (Ssn opts)
ssn = mkProxy2'

type SsnR (opts :: Opt) = MakeR2 (Ssn opts)
type Ssn (opts :: Opt) = '(opts, Ssnip, Ssnop, String)


-- | read in a time and validate it
--
-- >>> newRefined2 @OL @Hmsip @Hmsop' "23:13:59"
-- Right (Refined2 {r2In = [23,13,59], r2Out = "23:13:59"})
--
-- >>> newRefined2 @OL @Hmsip @Hmsop' "23:13:60"
-- Left "Step 2. False Boolean Check(op) | {Bool(2) [seconds] (60 <= 59)}"
--
-- >>> newRefined2 @OL @Hmsip @Hmsop' "26:13:59"
-- Left "Step 2. False Boolean Check(op) | {Bool(0) [hours] (26 <= 23)}"
--
hms :: OptC opts => Proxy (Hms opts)
hms = mkProxy2'

type HmsR (opts :: Opt) = MakeR2 (Hms opts)
type Hms (opts :: Opt) = '(opts, Hmsip, Hmsop, String)

--hms' :: Proxy (Hms' OZ)
--hms' = mkProxy2'

type HmsR' (opts :: Opt) = MakeR2 (Hms' opts)
type Hms' (opts :: Opt) = '(opts, Hmsip, Hmsop', String)

-- | read in an ipv4 address and validate it
--
-- >>> newRefined2 @OZ @Ip4ip @Ip4op' "001.223.14.1"
-- Right (Refined2 {r2In = [1,223,14,1], r2Out = "001.223.14.1"})
--
-- >>> newRefined2 @OL @Ip4ip @Ip4op' "001.223.14.999"
-- Left "Step 2. False Boolean Check(op) | {Bool(3) [octet 3 out of range 0-255 found 999] (999 <= 255)}"
--
-- >>> newRefined2P (ip4 @OL) "001.223.14.999"
-- Left "Step 2. Failed Boolean Check(op) | octet 3 out of range 0-255 found 999"
--
-- >>> newRefined2P (ip4 @OL) "001.223.14.999.1"
-- Left "Step 2. Failed Boolean Check(op) | Guards:invalid length(5) expected 4"
--
-- >>> newRefined2P (ip4 @OL) "001.257.14.1"
-- Left "Step 2. Failed Boolean Check(op) | octet 1 out of range 0-255 found 257"
--
type Ip4R (opts :: Opt) = MakeR2 (Ip4 opts)
type Ip4 (opts :: Opt) = '(opts, Ip4ip, Ip4op, String) -- guards

ip4 :: Proxy (Ip4 opts)
ip4 = Proxy

type Ip4R' (opts :: Opt) = MakeR2 (Ip4' opts)
type Ip4' (opts :: Opt) = '(opts, Ip4ip, Ip4op', String) -- boolean predicates

ip4' :: Proxy (Ip4' opts)
ip4' = Proxy

type Ip6R (opts :: Opt) = MakeR2 (Ip6 opts)
type Ip6 (opts :: Opt) = '(opts, Ip6ip, Ip6op, String) -- guards

ip6 :: Proxy (Ip6 opts)
ip6 = Proxy

-- | validate isbn10
--
-- >>> newRefined2P (isbn10 @OZ) "0-306-40611-X"
-- Right (Refined2 {r2In = ([0,3,0,6,4,0,6,1,1],10), r2Out = "0-306-40611-X"})
--
-- >>> newRefined2P (isbn10 @OZ) "0-306-40611-9"
-- Left "Step 2. Failed Boolean Check(op) | mod 0 oops"
--
type Isbn10R (opts :: Opt) = MakeR2 (Isbn10 opts)
type Isbn10 (opts :: Opt) = '(opts, Isbn10ip, Isbn10op, String) -- guards

isbn10 :: Proxy (Isbn10 opts)
isbn10 = Proxy

-- | validate isbn13
--
-- >>> newRefined2P (isbn13 @OZ) "978-0-306-40615-7"
-- Right (Refined2 {r2In = [9,7,8,0,3,0,6,4,0,6,1,5,7], r2Out = "978-0-306-40615-7"})
--
-- >>> newRefined2P (isbn13 @OZ) "978-0-306-40615-8"
-- Left "Step 2. Failed Boolean Check(op) | sum=101 mod 10=1"
--
type Isbn13R (opts :: Opt) = MakeR2 (Isbn13 opts)
type Isbn13 (opts :: Opt) = '(opts, Isbn13ip, Isbn13op, String) -- guards

isbn13 :: Proxy (Isbn13 opts)
isbn13 = Proxy



cc11 :: Proxy (Ccn opts 11)
cc11 = Proxy

-- | convert a string from a given base \'i\' and store it internally as an base 10 integer
--
-- >>> newRefined2 @OZ @(ReadBase Int 16 Id) @'True "00fe"
-- Right (Refined2 {r2In = 254, r2Out = "00fe"})
--
-- >>> newRefined2 @OZ @(ReadBase Int 16 Id) @(Between 100 400 Id) "00fe"
-- Right (Refined2 {r2In = 254, r2Out = "00fe"})
--
-- >>> newRefined2 @OZ @(ReadBase Int 16 Id) @(GuardSimple (Id < 400) >> 'True) "f0fe"
-- Left "Step 2. Failed Boolean Check(op) | (61694 < 400)"
--
-- >>> newRefined2 @OL @(ReadBase Int 16 Id) @(Id < 400) "f0fe" -- todo: why different parens vs braces
-- Left "Step 2. False Boolean Check(op) | {61694 < 400}"
--
type BaseN (opts :: Opt) (n :: Nat) = BaseN' opts n 'True
type BaseN' (opts :: Opt) (n :: Nat) p = '(opts,ReadBase Int n Id, p, String)


-- | Luhn check
--
-- >>> newRefined2 @OZ @Luhnip @(Luhnop 4) "1230"
-- Right (Refined2 {r2In = [1,2,3,0], r2Out = "1230"})
--
-- >>> newRefined2 @OL @Luhnip @(Luhnop 4) "1234"
-- Left "Step 2. False Boolean Check(op) | {True && False | (IsLuhn map=[4,6,2,2] sum=14 ret=4 | [1,2,3,4])}"
--
-- | uses builtin 'IsLuhn'

-- | convert a string from a given base \'i\' and store it internally as a base \'j\' string
--
-- >>> newRefined2 @OZ @(BaseIJip 16 2) @'True "fe"
-- Right (Refined2 {r2In = "11111110", r2Out = "fe"})
--
-- >>> newRefined2 @OZ @(BaseIJip 16 2) @'True "fge"
-- Left "Step 1. Initial Conversion(ip) Failed | invalid base 16"
--
-- >>> newRefined2 @OL @(BaseIJip 16 2) @(ReadBase Int 2 Id < 1000) "ffe"
-- Left "Step 2. False Boolean Check(op) | {4094 < 1000}"
--
type BaseIJip (i :: Nat) (j :: Nat) = ReadBase Int i Id >> ShowBase j Id

type BaseIJ (i :: Nat) (j :: Nat) = BaseIJ' i j 'True
type BaseIJ' (i :: Nat) (j :: Nat) p = '(ReadBase Int i Id >> ShowBase j Id, p, String)

-- | take any valid Read/Show instance and turn it into a valid 'Predicate.Refined2.Refined2'
--
-- >>> :m + Data.Ratio
-- >>> newRefined2 @OZ @(ReadP Rational Id) @'True "13 % 3"
-- Right (Refined2 {r2In = 13 % 3, r2Out = "13 % 3"})
--
-- >>> newRefined2 @OZ @(ReadP Rational Id) @'True "13x % 3"
-- Left "Step 1. Initial Conversion(ip) Failed | ReadP Ratio Integer (13x % 3)"
--
-- >>> newRefined2 @OZ @(ReadP Rational Id) @(3 % 1 <..> 5 % 1) "13 % 3"
-- Right (Refined2 {r2In = 13 % 3, r2Out = "13 % 3"})
--
-- >>> newRefined2 @OZ @(ReadP Rational Id) @(11 -% 2 <..> 3 -% 1) "-13 % 3"
-- Right (Refined2 {r2In = (-13) % 3, r2Out = "-13 % 3"})
--
-- >>> newRefined2 @OZ @(ReadP Rational Id) @(Id > (15 % 1)) "13 % 3"
-- Left "Step 2. False Boolean Check(op) | FalseP"
--
-- >>> newRefined2 @OL @(ReadP Rational Id) @(Msg (PrintF "invalid=%3.2f" (FromRational Double Id)) (Id > (15 % 1))) "13 % 3"
-- Left "Step 2. False Boolean Check(op) | {invalid=4.33 13 % 3 > 15 % 1}"
--
-- >>> newRefined2 @OZ @(ReadP Rational Id) @(Id > (11 % 1)) "13 % 3"
-- Left "Step 2. False Boolean Check(op) | FalseP"
--
-- >>> newRefined2 @OZ @(ReadP UTCTime Id) @'True "2018-10-19 14:53:11.5121359 UTC"
-- Right (Refined2 {r2In = 2018-10-19 14:53:11.5121359 UTC, r2Out = "2018-10-19 14:53:11.5121359 UTC"})
--
-- >>> :m + Data.Aeson
-- >>> newRefined2 @OZ @(ReadP Value Id) @'True "String \"jsonstring\""
-- Right (Refined2 {r2In = String "jsonstring", r2Out = "String \"jsonstring\""})
--
-- >>> newRefined2 @OZ @(ReadP Value Id) @'True "Number 123.4"
-- Right (Refined2 {r2In = Number 123.4, r2Out = "Number 123.4"})
--
