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
{-# LANGUAGE NoStarIsType #-}
{- |
     Contains prepackaged 4-tuples to use with 'Refined3'
-}
module Predicate.Refined3Helper (
  -- ** date time checkers
    datetime1
  , DateTime1

  , datetime1'
  , DateTime1'

  , daten
  , DateN
  , datetimen
  , DateTimeN
  , DateTimeNR
  , DateFmts
  , DateTimeFmts

  -- *** time checkers
  , hms
  , Hms
  , Hmsip
  , Hmsop
  , Hmsfmt
  , HmsRE

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

  -- ** ipv4
  , ip
  , Ip
  , OctetRE
  , Ip4StrictRE

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
import Predicate.Refined3
import Predicate.Core
import Predicate.Prelude
import Predicate.Util
import Data.Proxy
import GHC.TypeLits (KnownNat, AppendSymbol,Nat)
import Data.Kind (Type)
import Data.Time

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XNoStarIsType

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
type Ccip = Map (ReadP Int Id) (Ones (Remove "-" Id))
type Ccop (n :: Nat) = Guard (PrintT "expected %d digits but found %d" '(n,Len)) (Len == n) >> Luhn Id
type Ccfmt (ns :: [Nat]) = ConcatMap (ShowP Id) Id >> SplitAts ns Id >> Concat (Intercalate '["-"] Id)

type Ccn (ns :: [Nat]) = '(Ccip, Ccop (SumT ns), Ccfmt ns, String)

type CC11 = Ccn '[4,4,3]

ccn :: Proxy (Ccn ns)
ccn = mkProxy3

-- works but have to add all the constraints
ccn' :: (PP ns [Char] ~ [Integer], KnownNat (SumT ns), P ns [Char]) => Proxy (Ccn ns)
ccn' = mkProxy3'

cc11 :: Proxy (Ccn '[4,4,3])   -- or Proxy CC11
cc11 = mkProxy3'

-- | read in a valid datetime
--
-- >>> prtEval3P (datetime1 @LocalTime) oz "2018-09-14 02:57:04"
-- Right (Refined3 {r3In = 2018-09-14 02:57:04, r3Out = "2018-09-14 02:57:04"})
--
-- >>> prtEval3P (datetime1' @LocalTime) oz "2018-09-14 99:98:97"
-- Left Step 2. Failed Boolean Check(op) | invalid hours 99
--
-- >>> prtEval3P (datetime1 @LocalTime) ol "2018-09-14 99:98:97"
-- Left Step 2. False Boolean Check(op) | {(>>) False | {GuardBool(0) [hours] (99 <= 23)}}
--
-- >>> prtEval3P (datetime1' @LocalTime) oz "2018-09-14 23:01:97"
-- Left Step 2. Failed Boolean Check(op) | invalid seconds 97
--
-- >>> prtEval3P (datetime1 @LocalTime) ol "2018-09-14 23:01:97"
-- Left Step 2. False Boolean Check(op) | {(>>) False | {GuardBool(2) [seconds] (97 <= 59)}}
--
-- >>> prtEval3P (Proxy @(DateTime1 UTCTime)) oz "2018-09-14 99:98:97"
-- Right (Refined3 {r3In = 2018-09-18 04:39:37 UTC, r3Out = "2018-09-18 04:39:37"})
--
datetime1 :: Proxy (DateTime1 t)
datetime1 = mkProxy3

type DateTime1 (t :: Type) = '(Dtip t, Dtop, Dtfmt, String)
type Dtip t = ParseTimeP t "%F %T" Id

datetime1' :: Proxy (DateTime1' t)
datetime1' = mkProxy3

type DateTime1' (t :: Type) = '(Dtip t, Dtop', Dtfmt, String)

-- extra check to validate the time as parseTime doesnt validate the time component
-- ZonedTime LocalTime and TimeOfDay don't do validation and allow invalid stuff through : eg 99:98:97 is valid
-- UTCTime will do the same but any overages get tacked on to the day and time as necessary: makes the time valid! 99:98:97 becomes 04:39:37
--    2018-09-14 99:00:96 becomes 2018-09-18 03:01:36

type Dtop' =
   Map (ReadP Int Id) (FormatTimeP "%H %M %S" Id >> Resplit "\\s+" Id)
     >> GuardsDetail "invalid %s %d"
               '[ '("hours", Between 0 23)
                , '("minutes", Between 0 59)
                , '("seconds", Between 0 59)
                ] >> 'True
{-
type Dtop'' =
   Map (ReadP Int Id) (FormatTimeP "%H %M %S" Id >> Resplit "\\s+" Id)
     >> Guards '[ '(PrintT "guard %d invalid hours %d" Id, Between 0 23)
                , '(PrintT "guard %d invalid minutes %d" Id, Between 0 59)
                , '(PrintT "guard %d invalid seconds %d" Id, Between 0 59)
                ] >> 'True
-}

type Dtop =
   Map (ReadP Int Id) (FormatTimeP "%H %M %S" Id >> Resplit "\\s+" Id)
     >> Bools '[ '("hours", Between 0 23)
               , '("minutes",Between 0 59)
               , '("seconds",Between 0 59)
               ]

type Dtfmt = FormatTimeP "%F %T" Id

ssn :: Proxy Ssn
ssn = mkProxy3'

-- | read in an ssn
--
-- >>> prtEval3P ssn oz "134-01-2211"
-- Right (Refined3 {r3In = [134,1,2211], r3Out = "134-01-2211"})
--
-- >>> prtEval3P ssn ol "666-01-2211"
-- Left Step 2. False Boolean Check(op) | {GuardBool(0) [number for group 0 invalid: found 666] (True && False | (666 /= 666))}
--
-- >>> prtEval3P ssn ol "667-00-2211"
-- Left Step 2. False Boolean Check(op) | {GuardBool(1) [number for group 1 invalid: found 0] (1 <= 0)}
--
type Ssn = '(Ssnip, Ssnop, Ssnfmt, String)

type Ssnip = Map (ReadP Int Id) (Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" Id >> Snd OneP)
type Ssnop = BoolsQuick (PrintT "number for group %d invalid: found %d" Id)
                     '[Between 1 899 && Id /= 666, Between 1 99, Between 1 9999]

{-
type Ssnop' = GuardsDetail "%s invalid: found %d"
                          '[ '("first", Between 1 899 && Id /= 666)
                           , '("second", Between 1 99)
                           , '("third" , Between 1 9999)
                           ] >> 'True
-}
type Ssnfmt = PrintL 3 "%03d-%02d-%04d" Id

-- | read in a time and validate it
--
-- >>> prtEval3P hms ol "23:13:59"
-- Right (Refined3 {r3In = [23,13,59], r3Out = "23:13:59"})
--
-- >>> prtEval3P hms ol "23:13:60"
-- Left Step 2. False Boolean Check(op) | {GuardBool(2) [seconds] (60 <= 59)}
--
-- >>> prtEval3P hms ol "26:13:59"
-- Left Step 2. False Boolean Check(op) | {GuardBool(0) [hours] (26 <= 23)}
--
hms :: Proxy Hms
hms = mkProxy3'

type Hms = '(Hmsip, Hmsop, Hmsfmt, String)

type Hmsip = Map (ReadP Int Id) (Resplit ":" Id)
{-
type Hmsop = BoolsQuick ""
              '[ Msg "hours:"   (Between 0 23)
              ,  Msg "minutes:" (Between 0 59)
              ,  Msg "seconds:" (Between 0 59)]
-}
type Hmsop = Bools
             '[ '("hours", Between 0 23)
              , '("minutes",Between 0 59)
               ,'("seconds",Between 0 59)]
{-
type Hmsop = Guard (PrintF "expected len 3 but found %d" Len) (Length Id == 3)
             >> Guards '[ '(PrintT "guard(%d) %d hours is out of range" Id, Between 0 23)
                        , '(PrintT "guard(%d) %d mins is out of range" Id, Between 0 59)
                        , '(PrintT "guard(%d) %d secs is out of range" Id, Between 0 59)]
-}
type Hmsfmt = PrintL 3 "%02d:%02d:%02d" Id

-- | read in an ipv4 address and validate it
--
-- >>> prtEval3P ip oz "001.223.14.1"
-- Right (Refined3 {r3In = [1,223,14,1], r3Out = "001.223.014.001"})
--
-- >>> prtEval3P ip ol "001.223.14.999"
-- Left Step 2. False Boolean Check(op) | {GuardBool(3) [guard(3) octet out of range 0-255 found 999] (999 <= 255)}
--
-- >>> prtEval3P ip oz "001.223.14.999.1"
-- Left Step 1. Initial Conversion(ip) Failed | Regex no results
--
-- >>> prtEval3P ip ol "001.257.14.1"
-- Left Step 2. False Boolean Check(op) | {GuardBool(1) [guard(1) octet out of range 0-255 found 257] (257 <= 255)}
--
type Ip = '(Ipip, Ipop, Ipfmt, String)

ip :: Proxy Ip
ip = mkProxy3'

type Ipip = Map (ReadP Int Id) (Rescan "^(\\d{1,3}).(\\d{1,3}).(\\d{1,3}).(\\d{1,3})$" Id >> OneP >> Snd Id)
-- RepeatT is a type family so it expands everything! replace RepeatT with a type class
type Ipop = BoolsN (PrintT "guard(%d) octet out of range 0-255 found %d" Id) 4 (Between 0 255)
type Ipfmt = PrintL 4 "%03d.%03d.%03d.%03d" Id

type HmsRE = "^([0-1][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9])$" -- strict validation should only be done in 'op' not 'ip'

type OctetRE = "(25[0-5]|2[0..4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])" -- no padded numbers allowed
--type Ip4StrictRE = "^" `AppendSymbol` OctetRE `AppendSymbol` "\\." `AppendSymbol` OctetRE `AppendSymbol` "\\." `AppendSymbol` OctetRE `AppendSymbol` "\\." `AppendSymbol` OctetRE `AppendSymbol` "$"
type Ip4StrictRE = "^" `AppendSymbol` IntersperseT "\\." (RepeatT 4 OctetRE) `AppendSymbol` "$"

-- valid dates for for DateFmts are "2001-01-01" "Jan 24 2009" and "03/29/07"
type DateFmts = '["%Y-%m-%d", "%m/%d/%y", "%B %d %Y"]
type DateN = '(ParseTimes Day DateFmts Id, 'True, FormatTimeP "%Y-%m-%d" Id, String)

type DateTimeFmts = '["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"]

type DateTimeNR = MakeR3 DateTimeN
type DateTimeN = '(ParseTimes UTCTime DateTimeFmts Id, 'True, FormatTimeP "%Y-%m-%d %H:%M:%S" Id, String)

-- | convert a string from a given base \'i\' and store it internally as an base 10 integer
--
-- >>> prtEval3P base16 oz "00fe"
-- Right (Refined3 {r3In = 254, r3Out = "fe"})
--
-- >>> prtEval3P (basen' @16 @(Between 100 400)) oz "00fe"
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

-- replace with BetweenT 2 36 n
--basen :: forall n . (KnownNat n, (n GN.<=? 36) ~ 'True, (2 GN.<=? n) ~ 'True) => Proxy (BaseN n)
--basen = mkProxy3

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

type BetweenN m n = '(Id, Between m n, Id, Int)
type BetweenR m n = RefinedEmulate (Between m n) Int

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
-- Left Step 1. Initial Conversion(ip) Failed | ReadP Ratio Integer (13x % 3) failed
--
-- >>> prtEval3P (readshow' @Rational @(Between (3 % 1) (5 % 1))) oz "13 % 3"
-- Right (Refined3 {r3In = 13 % 3, r3Out = "13 % 3"})
--
-- >>> prtEval3P (Proxy @(ReadShow' Rational (Between (11 %- 2) (3 %- 1)))) oz "-13 % 3"
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
