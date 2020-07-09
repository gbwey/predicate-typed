{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# OPTIONS -Wincomplete-record-updates #-}
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
     Contains prepackaged 5-tuples and proxies to use with 'Refined3'
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

--  , hms'
  , Hms'
  , HmsR'

  -- ** credit cards
  , ccn
  , ccn'
  , Ccn
  , cc11
  , Cc11
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
import Predicate.Refined
import Predicate.Refined3
import Predicate.Core
import Predicate.Prelude
import Predicate.Util
import Predicate.Util_TH
import Predicate.TH_Orphans ()
import Data.Proxy
import GHC.TypeLits (KnownNat, Nat)
import Data.Kind (Type)
import Data.Time

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XTemplateHaskell

-- | credit card with luhn algorithm
--
-- >>> newRefined3P (cc11 @'OZ) "1234-5678-901"
-- Left "Step 2. False Boolean Check(op) | FalseP"
--
-- >>> newRefined3P (cc11 @'OZ) "1234-5678-903"
-- Right (Refined3 {r3In = [1,2,3,4,5,6,7,8,9,0,3], r3Out = "1234-5678-903"})
--
-- >>> pz @(Ccip >> Ccop 11) "79927398713"
-- TrueT
--
-- >>> pz @(Ccip >> Ccop 10) "79927398713"
-- FailT "expected 10 digits but found 11"
--

type Ccn (opts :: OptT) (ns :: [Nat]) = '(opts, Ccip, Ccop (SumT ns), Ccfmt ns, String)

type Cc11 (opts :: OptT) = Ccn opts '[4,4,3]

ccn :: Proxy (Ccn opts ns)
ccn = mkProxy3

-- works but have to add all the constraints
ccn' :: ( OptTC opts
        , PP ns String ~ [Integer]
        , KnownNat (SumT ns)
        , P ns String
        ) => Proxy (Ccn opts ns)
ccn' = mkProxy3'

cc11 :: OptTC opts => Proxy (Ccn opts '[4,4,3])   -- or Proxy Cc11
cc11 = mkProxy3'

-- | read in a valid datetime
--
-- >>> newRefined3P (datetime1 @'OL @LocalTime) "2018-09-14 02:57:04"
-- Right (Refined3 {r3In = 2018-09-14 02:57:04, r3Out = "2018-09-14 02:57:04"})
--
-- >>> newRefined3P (datetime1 @'OL @LocalTime) "2018-09-99 12:12:12"
-- Left "Step 1. Initial Conversion(ip) Failed | ParseTimeP LocalTime (%F %T) failed to parse"
--
datetime1 :: Proxy (DateTime1 opts t)
datetime1 = mkProxy3

-- now that time is actually validated we dont need Dtop*
type DateTime1 (opts :: OptT) (t :: Type) = '( opts, Dtip t, 'True, Dtfmt, String)

-- fixed in time-1.9
-- extra check to validate the time as parseTime doesnt validate the time component
-- ZonedTime LocalTime and TimeOfDay don't do validation and allow invalid stuff through : eg 99:98:97 is valid
-- UTCTime will do the same but any overages get tacked on to the day and time as necessary: makes the time valid! 99:98:97 becomes 04:39:37
--    2018-09-14 99:00:96 becomes 2018-09-18 03:01:36

-- valid dates for for DateFmts are "2001-01-01" "Jan 24 2009" and "03/29/07"
type DateN (opts :: OptT) = '( opts, ParseTimes Day DateFmts Id, 'True, FormatTimeP "%Y-%m-%d" Id, String)

type DateTimeNR (opts :: OptT) = MakeR3 (DateTimeN opts)
type DateTimeN (opts :: OptT) = '(opts, ParseTimes UTCTime DateTimeFmts Id, 'True, FormatTimeP "%Y-%m-%d %H:%M:%S" Id, String)

ssn :: OptTC opts => Proxy (Ssn opts)
ssn = mkProxy3'

-- | read in an ssn
--
-- >>> newRefined3P (ssn @'OZ) "134-01-2211"
-- Right (Refined3 {r3In = [134,1,2211], r3Out = "134-01-2211"})
--
-- >>> newRefined3P (ssn @'OL) "666-01-2211"
-- Left "Step 2. False Boolean Check(op) | {Bool(0) [number for group 0 invalid: found 666] (True && False | (666 /= 666))}"
--
-- >>> newRefined3P (ssn @'OL) "667-00-2211"
-- Left "Step 2. False Boolean Check(op) | {Bool(1) [number for group 1 invalid: found 0] (1 <= 0)}"
--
type Ssn (opts :: OptT) = '(opts, Ssnip, Ssnop, Ssnfmt, String)
type SsnR (opts :: OptT) = MakeR3 (Ssn opts)
-- | read in a time and validate it
--
-- >>> newRefined3P (hms @'OL) "23:13:59"
-- Right (Refined3 {r3In = [23,13,59], r3Out = "23:13:59"})
--
-- >>> newRefined3P (hms @'OL) "23:13:60"
-- Left "Step 2. Failed Boolean Check(op) | seconds invalid: found 60"
--
-- >>> newRefined3P (hms @'OL) "26:13:59"
-- Left "Step 2. Failed Boolean Check(op) | hours invalid: found 26"
--
hms :: OptTC opts => Proxy (Hms opts)
hms = mkProxy3'

type HmsR (opts :: OptT) = MakeR3 (Hms opts)
type Hms (opts :: OptT) = '(opts, Hmsip, Hmsop >> 'True, Hmsfmt, String)

type HmsR' (opts :: OptT) = MakeR3 (Hms' opts)
type Hms' (opts :: OptT) = '(opts, Hmsip, Hmsop', Hmsfmt, String)


-- | read in an ipv4 address and validate it
--
-- >>> newRefined3P (ip4 @'OZ) "001.223.14.1"
-- Right (Refined3 {r3In = [1,223,14,1], r3Out = "001.223.014.001"})
--
-- >>> newRefined3P (ip4 @'OL) "001.223.14.999"
-- Left "Step 2. Failed Boolean Check(op) | octet 3 out of range 0-255 found 999"
--
-- >>> newRefined3P (ip4 @'OZ) "001.223.14.999.1"
-- Left "Step 2. Failed Boolean Check(op) | Guards:invalid length(5) expected 4"
--
-- >>> newRefined3P (ip4 @'OL) "001.257.14.1"
-- Left "Step 2. Failed Boolean Check(op) | octet 1 out of range 0-255 found 257"
--
type Ip4R (opts :: OptT) = MakeR3 (Ip4 opts)
type Ip4 (opts :: OptT) = '( opts, Ip4ip, Ip4op >> 'True, Ip4fmt, String) -- guards

ip4 :: OptTC opts => Proxy (Ip4 opts)
ip4 = mkProxy3'

type Ip4R' (opts :: OptT) = MakeR3 (Ip4' opts)
type Ip4' (opts :: OptT) = '( opts, Ip4ip, Ip4op', Ip4fmt, String) -- boolean predicates

ip4' :: OptTC opts => Proxy (Ip4' opts)
ip4' = mkProxy3'

type Ip6R (opts :: OptT) = MakeR3 (Ip6 opts)
type Ip6 (opts :: OptT) = '( opts, Ip6ip, Ip6op, Ip6fmt, String) -- guards

ip6 :: Proxy (Ip6 opts)
ip6 = Proxy

-- | convert a string from a given base \'i\' and store it internally as an base 10 integer
--
-- >>> newRefined3P (base16 @'OZ) "00fe"
-- Right (Refined3 {r3In = 254, r3Out = "fe"})
--
-- >>> newRefined3P (basen' @'OZ @16 @(100 <..> 400)) "00fe"
-- Right (Refined3 {r3In = 254, r3Out = "fe"})
--
-- >>> newRefined3P (basen' @'OZ @16 @(GuardSimple (Id < 400) >> 'True)) "f0fe"
-- Left "Step 2. Failed Boolean Check(op) | (61694 < 400)"
--
-- >>> newRefined3P (basen' @'OL @16 @(Id < 400)) "f0fe" -- todo: why different parens vs braces
-- Left "Step 2. False Boolean Check(op) | {61694 < 400}"
--
type BaseN (opts :: OptT) (n :: Nat) = BaseN' opts n 'True
type BaseN' (opts :: OptT) (n :: Nat) p = '( opts, ReadBase Int n Id, p, ShowBase n Id, String)

base16 :: Proxy (BaseN opts 16)
base16 = basen

base16' :: Proxy (BaseN' opts 16 p)
base16' = basen'

base2 :: Proxy (BaseN opts 2)
base2 = basen

base2' :: Proxy (BaseN' opts 2 p)
base2' = basen'

basen :: Proxy (BaseN opts n)
basen = mkProxy3

basen' :: Proxy (BaseN' opts n p)
basen' = mkProxy3

daten :: OptTC opts => Proxy (DateN opts)
daten = mkProxy3'

datetimen :: OptTC opts => Proxy (DateTimeN opts)
datetimen = mkProxy3'

-- | ensures that two numbers are in a given range (emulates 'Refined.Refined')
--
-- >>> newRefined3P (between @'OZ @10 @16) 14
-- Right (Refined3 {r3In = 14, r3Out = 14})
--
-- >>> newRefined3P (between @'OZ @10 @16) 17
-- Left "Step 2. False Boolean Check(op) | FalseP"
--
-- >>> prtEval3P (between @'OAN @10 @16) 17
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
between :: Proxy (BetweenN opts m n)
between = mkProxy3

type BetweenN (opts :: OptT) m n = '( opts, Id, Between m n Id, Id, Int)
type BetweenR (opts :: OptT) m n = RefinedEmulate opts (Between m n Id) Int

type LuhnR (opts :: OptT) (n :: Nat) = MakeR3 (LuhnT opts n)

-- | Luhn check
--
-- >>> newRefined3P (Proxy @(LuhnT 'OZ 4)) "1230"
-- Right (Refined3 {r3In = [1,2,3,0], r3Out = "1230"})
--
-- >>> newRefined3P (Proxy @(LuhnT 'OL 4)) "1234"
-- Left "Step 2. False Boolean Check(op) | {True && False | (Luhn map=[4,6,2,2] sum=14 ret=4 | [1,2,3,4])}"
--
-- | uses builtin 'Luhn'
type LuhnT (opts :: OptT) (n :: Nat) =
   '( opts
    , Map (ReadP Int Id) (Ones Id)
    , Msg "incorrect number of digits:"
          (Len == n) && Luhn Id
    , ConcatMap (ShowP Id) Id
    , String)

-- | noop true
type Ok (opts :: OptT) (t :: Type) = '(opts, Id, 'True, Id, t)
type OkR (opts :: OptT) (t :: Type) = MakeR3 (Ok opts t)

ok :: Proxy (Ok opts t)
ok = mkProxy3

-- | noop false
type OkNot (t :: Type) = '( 'OA, Id, 'False, Id, t)
type OkNotR (t :: Type) = MakeR3 (OkNot t)

oknot :: Proxy (OkNot t)
oknot = mkProxy3

-- | convert a string from a given base \'i\' and store it internally as a base \'j\' string
--
-- >>> newRefined3P (Proxy @(BaseIJ 'OZ 16 2)) "fe"
-- Right (Refined3 {r3In = "11111110", r3Out = "fe"})
--
-- >>> newRefined3P (Proxy @(BaseIJ 'OZ 16 2)) "fge"
-- Left "Step 1. Initial Conversion(ip) Failed | invalid base 16"
--
-- >>> newRefined3P (Proxy @(BaseIJ' 'OL 16 2 (ReadBase Int 2 Id < 1000))) "ffe"
-- Left "Step 2. False Boolean Check(op) | {4094 < 1000}"
--
type BaseIJ (opts :: OptT) (i :: Nat) (j :: Nat) = BaseIJ' opts i j 'True
type BaseIJ' (opts :: OptT) (i :: Nat) (j :: Nat) p = '(opts, ReadBase Int i Id >> ShowBase j Id, p, ReadBase Int j Id >> ShowBase i Id, String)

-- | take any valid Read/Show instance and turn it into a valid 'Refined3'
--
-- >>> :m + Data.Ratio
-- >>> newRefined3P (readshow @'OZ @Rational) "13 % 3"
-- Right (Refined3 {r3In = 13 % 3, r3Out = "13 % 3"})
--
-- >>> newRefined3P (readshow @'OZ @Rational) "13x % 3"
-- Left "Step 1. Initial Conversion(ip) Failed | ReadP Ratio Integer (13x % 3)"
--
-- >>> newRefined3P (readshow' @'OZ @Rational @(3 % 1 <..> 5 % 1)) "13 % 3"
-- Right (Refined3 {r3In = 13 % 3, r3Out = "13 % 3"})
--
-- >>> newRefined3P (Proxy @(ReadShow' 'OZ Rational (11 -% 2 <..> 3 -% 1))) "-13 % 3"
-- Right (Refined3 {r3In = (-13) % 3, r3Out = "(-13) % 3"})
--
-- >>> newRefined3P (Proxy @(ReadShow' 'OZ Rational (Id > (15 % 1)))) "13 % 3"
-- Left "Step 2. False Boolean Check(op) | FalseP"
--
-- >>> newRefined3P (Proxy @(ReadShow' 'OL Rational (Msg (PrintF "invalid=%3.2f" (FromRational Double Id)) (Id > (15 % 1))))) "13 % 3"
-- Left "Step 2. False Boolean Check(op) | {invalid=4.3313 % 3 > 15 % 1}"
--
-- >>> newRefined3P (Proxy @(ReadShow' 'OZ Rational (Id > (11 % 1)))) "13 % 3"
-- Left "Step 2. False Boolean Check(op) | FalseP"
--
-- >>> let tmString = "2018-10-19 14:53:11.5121359 UTC"
-- >>> let tm = read tmString :: UTCTime
-- >>> newRefined3P (readshow @'OZ @UTCTime) tmString
-- Right (Refined3 {r3In = 2018-10-19 14:53:11.5121359 UTC, r3Out = "2018-10-19 14:53:11.5121359 UTC"})
--
-- >>> :m + Data.Aeson
-- >>> newRefined3P (readshow @'OZ @Value) "String \"jsonstring\""
-- Right (Refined3 {r3In = String "jsonstring", r3Out = "String \"jsonstring\""})
--
-- >>> newRefined3P (readshow @'OZ @Value) "Number 123.4"
-- Right (Refined3 {r3In = Number 123.4, r3Out = "Number 123.4"})
--
type ReadShow (opts :: OptT) (t :: Type) = '( opts, ReadP t Id, 'True, ShowP Id, String)
type ReadShowR (opts :: OptT) (t :: Type) = MakeR3 (ReadShow opts t)

type ReadShow' (opts :: OptT) (t :: Type) p = '( opts, ReadP t Id, p, ShowP Id, String)
type ReadShowR' (opts :: OptT) (t :: Type) p = MakeR3 (ReadShow' opts t p)

readshow :: Proxy (ReadShow opts t)
readshow = mkProxy3

readshow' :: Proxy (ReadShow' opts t p)
readshow' = mkProxy3

-- | test tuple type families
--
-- >>> pl @(T5_2 (Ip4 'OL)) "1.2.3.4"
-- Present [1,2,3,4] (Map [1,2,3,4] | ["1","2","3","4"])
-- PresentT [1,2,3,4]
--
--
-- >>> pl @(T5_3 (Ip4 'OL)) [141,213,308,4]
-- Error octet 2 out of range 0-255 found 308 ((>>) lhs failed)
-- FailT "octet 2 out of range 0-255 found 308"
--
--
-- >>> pl @(T5_3 (Ip4 'OL)) [141,213,308,4,8]
-- Error Guards:invalid length(5) expected 4 ((>>) lhs failed)
-- FailT "Guards:invalid length(5) expected 4"
--
--
-- >>> pl @(T5_4 (Ip4 'OL)) [141,513,9,4]
-- Present "141.513.009.004" (PrintL(4) [141.513.009.004] | s=%03d.%03d.%03d.%03d)
-- PresentT "141.513.009.004"
--
