{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
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
{- |
     Contains prepackaged 4-tuples and proxies to use with 'Refined3'
-}
module Predicate.Examples.Refined3 (
  -- ** date time checkers
    datetime1
  , DateTime1

  , daten
  , DateN
  , datetimen
  , DateTimeN
  , DateTimeNR

  -- *** time checkers
  , hms
  , Hms
  , HmsR

  , hms'
  , Hms'
  , HmsR'

  -- ** credit cards
  , ccn
  , ccn'
  , Ccn
  , cc11
  , CC11
  , LuhnR
  , LuhnT

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
  , basen
  , base2
  , base16
  , basen'
  , base2'
  , base16'
  , BaseN
  , BaseN'
  , BaseIJ
  , BaseIJ'

  -- ** read / show
  , readshow
  , ReadShow
  , ReadShowR
  , readshow'
  , ReadShow'
  , ReadShowR'

  -- ** between
  , between
  , BetweenR
  , BetweenN

  -- ** miscellaneous
  , ok
  , Ok
  , OkR
  , oknot
  , OkNot
  , OkNotR
   ) where
import Predicate.Examples.Common
import Predicate.Refined3
import Predicate.Core
import Predicate.Prelude
import Predicate.Util
import Data.Proxy
import GHC.TypeLits (KnownNat, Nat)
import Data.Kind (Type)
import Data.Time

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators

-- | credit card with luhn algorithm
--
-- >>> prtEval3P cc11 oz "1234-5678-901"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> prtEval3P cc11 oz "1234-5678-903"
-- Right (Refined3 {r3In = [1,2,3,4,5,6,7,8,9,0,3], r3Out = "1234-5678-903"})
--
-- >>> pz @(Ccip >> Ccop 11) "79927398713"
-- True
-- TrueT
--
-- >>> pz @(Ccip >> Ccop 10) "79927398713"
-- Error expected 10 digits but found 11
-- FailT "expected 10 digits but found 11"
--

type Ccn (ns :: [Nat]) = '(Ccip, Ccop (SumT ns), Ccfmt ns, String)

type CC11 = Ccn '[4,4,3]

ccn :: Proxy (Ccn ns)
ccn = mkProxy3

-- works but have to add all the constraints
ccn' :: (PP ns String ~ [Integer], KnownNat (SumT ns), P ns String) => Proxy (Ccn ns)
ccn' = mkProxy3'

cc11 :: Proxy (Ccn '[4,4,3])   -- or Proxy CC11
cc11 = mkProxy3'

-- | read in a valid datetime
--
-- >>> prtEval3P (datetime1 @LocalTime) ol "2018-09-14 02:57:04"
-- Right (Refined3 {r3In = 2018-09-14 02:57:04, r3Out = "2018-09-14 02:57:04"})
--
-- >>> prtEval3P (datetime1 @LocalTime) ol "2018-09-99 12:12:12"
-- Left Step 1. Initial Conversion(ip) Failed | ParseTimeP LocalTime (%F %T) failed to parse
--
datetime1 :: Proxy (DateTime1 t)
datetime1 = mkProxy3

-- now that time is actually validated we dont need Dtop*
type DateTime1 (t :: Type) = '(Dtip t, 'True, Dtfmt, String)

-- fixed in time-1.9
-- extra check to validate the time as parseTime doesnt validate the time component
-- ZonedTime LocalTime and TimeOfDay don't do validation and allow invalid stuff through : eg 99:98:97 is valid
-- UTCTime will do the same but any overages get tacked on to the day and time as necessary: makes the time valid! 99:98:97 becomes 04:39:37
--    2018-09-14 99:00:96 becomes 2018-09-18 03:01:36

ssn :: Proxy Ssn
ssn = mkProxy3'

-- | read in an ssn
--
-- >>> prtEval3P ssn oz "134-01-2211"
-- Right (Refined3 {r3In = [134,1,2211], r3Out = "134-01-2211"})
--
-- >>> prtEval3P ssn ol "666-01-2211"
-- Left Step 2. False Boolean Check(op) | {Bool(0) [number for group 0 invalid: found 666] (True && False | (666 /= 666))}
--
-- >>> prtEval3P ssn ol "667-00-2211"
-- Left Step 2. False Boolean Check(op) | {Bool(1) [number for group 1 invalid: found 0] (1 <= 0)}
--
type Ssn = '(Ssnip, Ssnop, Ssnfmt, String)
type SsnR = MakeR3 Ssn
-- | read in a time and validate it
--
-- >>> prtEval3P hms ol "23:13:59"
-- Right (Refined3 {r3In = [23,13,59], r3Out = "23:13:59"})
--
-- >>> prtEval3P hms ol "23:13:60"
-- Left Step 2. Failed Boolean Check(op) | seconds invalid: found 60
--
-- >>> prtEval3P hms ol "26:13:59"
-- Left Step 2. Failed Boolean Check(op) | hours invalid: found 26
--
hms :: Proxy Hms
hms = mkProxy3'

type HmsR = MakeR3 Hms

type Hms = '(Hmsip, Hmsop >> 'True, Hmsfmt, String)


hms' :: Proxy Hms'
hms' = mkProxy3'

type HmsR' = MakeR3 Hms'

type Hms' = '(Hmsip, Hmsop', Hmsfmt, String)


-- | read in an ipv4 address and validate it
--
-- >>> prtEval3P ip4 oz "001.223.14.1"
-- Right (Refined3 {r3In = [1,223,14,1], r3Out = "001.223.014.001"})
--
-- >>> prtEval3P ip4 ol "001.223.14.999"
-- Left Step 2. Failed Boolean Check(op) | octet 3 out of range 0-255 found 999
--
-- >>> prtEval3P ip4 oz "001.223.14.999.1"
-- Left Step 2. Failed Boolean Check(op) | Guards:invalid length(5) expected 4
--
-- >>> prtEval3P ip4 ol "001.257.14.1"
-- Left Step 2. Failed Boolean Check(op) | octet 1 out of range 0-255 found 257
--
type Ip4R = MakeR3 Ip4

type Ip4 = '(Ip4ip, Ip4op >> 'True, Ip4fmt, String) -- guards

ip4 :: Proxy Ip4
ip4 = mkProxy3'

type Ip4R' = MakeR3 Ip4'
type Ip4' = '(Ip4ip, Ip4op', Ip4fmt, String) -- boolean predicates

ip4' :: Proxy Ip4'
ip4' = mkProxy3'

type Ip6R = MakeR3 Ip6
type Ip6 = '(Ip6ip, Ip6op, Ip6fmt, String) -- guards

ip6 :: Proxy Ip6
ip6 = Proxy

-- valid dates for for DateFmts are "2001-01-01" "Jan 24 2009" and "03/29/07"
type DateN = '(ParseTimes Day DateFmts Id, 'True, FormatTimeP "%Y-%m-%d" Id, String)

type DateTimeNR = MakeR3 DateTimeN
type DateTimeN = '(ParseTimes UTCTime DateTimeFmts Id, 'True, FormatTimeP "%Y-%m-%d %H:%M:%S" Id, String)

-- | convert a string from a given base \'i\' and store it internally as an base 10 integer
--
-- >>> prtEval3P base16 oz "00fe"
-- Right (Refined3 {r3In = 254, r3Out = "fe"})
--
-- >>> prtEval3P (basen' @16 @(100 <..> 400)) oz "00fe"
-- Right (Refined3 {r3In = 254, r3Out = "fe"})
--
-- >>> prtEval3P (basen' @16 @(GuardSimple (Id < 400) >> 'True)) oz "f0fe"
-- Left Step 2. Failed Boolean Check(op) | (61694 < 400)
--
-- >>> prtEval3P (basen' @16 @(Id < 400)) ol "f0fe" -- todo: why different parens vs braces
-- Left Step 2. False Boolean Check(op) | {61694 < 400}
--
type BaseN (n :: Nat) = BaseN' n 'True
type BaseN' (n :: Nat) p = '(ReadBase Int n Id, p, ShowBase n Id, String)

base16 :: Proxy (BaseN 16)
base16 = basen

base16' :: Proxy (BaseN' 16 p)
base16' = basen'

base2 :: Proxy (BaseN 2)
base2 = basen

base2' :: Proxy (BaseN' 2 p)
base2' = basen'

basen :: Proxy (BaseN n)
basen = mkProxy3

basen' :: Proxy (BaseN' n p)
basen' = mkProxy3

{-
basen' :: forall n p
       . (P p Int
       , PP p Int ~ Bool
       , KnownNat n
       , (n GN.<=? 36) ~ 'True
       , (2 GN.<=? n) ~ 'True
       ) => Proxy (BaseN' n p)
basen' = mkProxy3
-}
daten :: Proxy DateN
daten = mkProxy3'

datetimen :: Proxy DateTimeN
datetimen = mkProxy3'

-- | ensures that two numbers are in a given range (emulates 'Refined.Refined')
--
-- >>> prtEval3P (between @10 @16) oz 14
-- Right (Refined3 {r3In = 14, r3Out = 14})
--
-- >>> prtEval3P (between @10 @16) oz 17
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> prtEval3P (between @10 @16) o0 17
-- Left Step 2. False Boolean Check(op) | {17 <= 16}
-- <BLANKLINE>
-- *** Step 1. Success Initial Conversion(ip) [17] ***
-- <BLANKLINE>
-- P Id 17
-- <BLANKLINE>
-- *** Step 2. False Boolean Check(op) ***
-- <BLANKLINE>
-- False 17 <= 16
-- |
-- +- P Id 17
-- |
-- +- P '10
-- |
-- `- P '16
-- <BLANKLINE>
--
between :: Proxy (BetweenN m n)
between = mkProxy3

type BetweenN m n = '(Id, Between m n Id, Id, Int)
type BetweenR m n = RefinedEmulate (Between m n Id) Int

type LuhnR (n :: Nat) = MakeR3 (LuhnT n)

-- | Luhn check
--
-- >>> prtEval3P (Proxy @(LuhnT 4)) oz "1230"
-- Right (Refined3 {r3In = [1,2,3,0], r3Out = "1230"})
--
-- >>> prtEval3P (Proxy @(LuhnT 4)) ol "1234"
-- Left Step 2. False Boolean Check(op) | {True && False | (Luhn map=[4,6,2,2] sum=14 ret=4 | [1,2,3,4])}
--
-- | uses builtin 'Luhn'
type LuhnT (n :: Nat) =
   '(Map (ReadP Int Id) (Ones Id)
   , Msg "incorrect number of digits:"
         (Len == n) && Luhn Id
   , ConcatMap (ShowP Id) Id
   , String)

-- | noop true
type Ok (t :: Type) = '(Id, 'True, Id, t)
type OkR (t :: Type) = MakeR3 (Ok t)

ok :: Proxy (Ok t)
ok = mkProxy3

-- | noop false
type OkNot (t :: Type) = '(Id, 'False, Id, t)
type OkNotR (t :: Type) = MakeR3 (OkNot t)

oknot :: Proxy (OkNot t)
oknot = mkProxy3

-- | convert a string from a given base \'i\' and store it internally as a base \'j\' string
--
-- >>> prtEval3P (Proxy @(BaseIJ 16 2)) oz "fe"
-- Right (Refined3 {r3In = "11111110", r3Out = "fe"})
--
-- >>> prtEval3P (Proxy @(BaseIJ 16 2)) oz "fge"
-- Left Step 1. Initial Conversion(ip) Failed | invalid base 16
--
-- >>> prtEval3P (Proxy @(BaseIJ' 16 2 (ReadBase Int 2 Id < 1000))) ol "ffe"
-- Left Step 2. False Boolean Check(op) | {4094 < 1000}
--
type BaseIJ (i :: Nat) (j :: Nat) = BaseIJ' i j 'True
type BaseIJ' (i :: Nat) (j :: Nat) p = '(ReadBase Int i Id >> ShowBase j Id, p, ReadBase Int j Id >> ShowBase i Id, String)

-- | take any valid Read/Show instance and turn it into a valid 'Refined3'
--
-- >>> :m + Data.Ratio
-- >>> prtEval3P (readshow @Rational) oz "13 % 3"
-- Right (Refined3 {r3In = 13 % 3, r3Out = "13 % 3"})
--
-- >>> prtEval3P (readshow @Rational) oz "13x % 3"
-- Left Step 1. Initial Conversion(ip) Failed | ReadP Ratio Integer (13x % 3)
--
-- >>> prtEval3P (readshow' @Rational @(3 % 1 <..> 5 % 1)) oz "13 % 3"
-- Right (Refined3 {r3In = 13 % 3, r3Out = "13 % 3"})
--
-- >>> prtEval3P (Proxy @(ReadShow' Rational (11 -% 2 <..> 3 -% 1))) oz "-13 % 3"
-- Right (Refined3 {r3In = (-13) % 3, r3Out = "(-13) % 3"})
--
-- >>> prtEval3P (Proxy @(ReadShow' Rational (Id > (15 % 1)))) oz "13 % 3"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> prtEval3P (Proxy @(ReadShow' Rational (Msg (PrintF "invalid=%3.2f" (FromRational Double Id)) (Id > (15 % 1))))) ol "13 % 3"
-- Left Step 2. False Boolean Check(op) | {invalid=4.3313 % 3 > 15 % 1}
--
-- >>> prtEval3P (Proxy @(ReadShow' Rational (Id > (11 % 1)))) oz "13 % 3"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> let tmString = "2018-10-19 14:53:11.5121359 UTC"
-- >>> let tm = read tmString :: UTCTime
-- >>> prtEval3P (readshow @UTCTime) oz tmString
-- Right (Refined3 {r3In = 2018-10-19 14:53:11.5121359 UTC, r3Out = "2018-10-19 14:53:11.5121359 UTC"})
--
-- >>> :m + Data.Aeson
-- >>> prtEval3P (readshow @Value) oz "String \"jsonstring\""
-- Right (Refined3 {r3In = String "jsonstring", r3Out = "String \"jsonstring\""})
--
-- >>> prtEval3P (readshow @Value) oz "Number 123.4"
-- Right (Refined3 {r3In = Number 123.4, r3Out = "Number 123.4"})
--
type ReadShow (t :: Type) = '(ReadP t Id, 'True, ShowP Id, String)
type ReadShowR (t :: Type) = MakeR3 (ReadShow t)

type ReadShow' (t :: Type) p = '(ReadP t Id, p, ShowP Id, String)
type ReadShowR' (t :: Type) p = MakeR3 (ReadShow' t p)

readshow :: Proxy (ReadShow t)
readshow = mkProxy3

readshow' :: Proxy (ReadShow' t p)
readshow' = mkProxy3

