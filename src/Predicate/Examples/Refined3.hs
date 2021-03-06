{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoStarIsType #-}
-- | Contains prepackaged 5-tuples and proxies to use with 'Predicate.Refined3.Refined3'
module Predicate.Examples.Refined3 (
  -- ** datetime
    datetime1
  , DateTime1
  , daten
  , DateN
  , datetimen
  , DateTimeN
  , DateTimeNR

  -- ** time
  , hms
  , Hms
  , HmsR
  , Hms'
  , HmsR'

  -- ** luhn check
  , luhn
  , Luhn
  , luhn11
  , Luhn11
  , LuhnR
  , LuhnT
  -- ** sim card check
  , SimT

  -- ** ssn
  , ssn
  , Ssn
  , SsnR

  -- ** ipv4
  , ip4
  , Ip4
  , Ip4R

  , Ip4'

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
  , basen
  , base2
  , base16
  , BaseN
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
import Predicate
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat)
import Data.Kind (Type)
import Data.Time (Day, UTCTime)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XTemplateHaskell
-- >>> :set -XTypeApplications
-- >>> :m + Data.Time

-- | credit card validator using the luhn algorithm
--
-- >>> newRefined3P (luhn11 @OZ) "1234-5678-901"
-- Left Step 2. Failed Boolean Check(op) | invalid checkdigit
--
-- >>> newRefined3P (luhn11 @OZ) "1234-5678-903"
-- Right (Refined3 [1,2,3,4,5,6,7,8,9,0,3] "1234-5678-903")
--
-- >>> pz @(Luhnip >> Luhnop 11) "79927398713"
-- Val True
--
-- >>> pz @(Luhnip >> Luhnop 10) "79927398713"
-- Fail "expected 10 digits but found 11"
--
luhn11 :: OptC opts => Proxy (Luhn opts '[4,4,3])
luhn11 = mkProxy3'

-- | credit card validator using the luhn algorithm
type Luhn (opts :: Opt) (ns :: [Nat]) = '(opts, Luhnip, Luhnop (SumT ns), Luhnfmt ns, String)

-- | credit card validator using the luhn algorithm with format nnnn-nnnn-nnn
type Luhn11 (opts :: Opt) = Luhn opts '[4,4,3]

-- | proxy for 'Luhn'
luhn :: Proxy (Luhn opts ns)
luhn = mkProxy3

-- | read in a valid datetime
--
-- >>> newRefined3P (datetime1 @OL @LocalTime) "2018-09-14 02:57:04"
-- Right (Refined3 2018-09-14 02:57:04 "2018-09-14 02:57:04")
--
-- >>> newRefined3P (datetime1 @OL @LocalTime) "2018-09-99 12:12:12"
-- Left Step 1. Failed Initial Conversion(ip) | ParseTimeP LocalTime (%F %T) failed to parse
--
datetime1 :: Proxy (DateTime1 opts t)
datetime1 = mkProxy3

-- now that time is actually validated we dont need Dtop*
-- | datetime validator
type DateTime1 (opts :: Opt) (t :: Type) = '(opts, Dtip t, 'True, Dtfmt, String)

-- fixed in time-1.9
-- extra check to validate the time as parseTime doesnt validate the time component
-- ZonedTime LocalTime and TimeOfDay dont do validation and allow invalid stuff through : eg 99:98:97 is valid
-- UTCTime will do the same but any overages get tacked on to the day and time as necessary: makes the time valid! 99:98:97 becomes 04:39:37
--    2018-09-14 99:00:96 becomes 2018-09-18 03:01:36

-- valid dates for for DateFmts are "2001-01-01" "Jan 24 2009" and "03/29/07"
-- | date validator which tries to match against multiple date formats
type DateN (opts :: Opt) = '(opts, ParseTimes Day DateFmts Id, 'True, FormatTimeP "%Y-%m-%d", String)

-- | 'Predicate.Refined3.Refined3' type signature for 'DateTimeN'
type DateTimeNR (opts :: Opt) = MakeR3 (DateTimeN opts)

-- | datetime validator which matches against multiple datetime formats
type DateTimeN (opts :: Opt) = '(opts, ParseTimes UTCTime DateTimeFmts Id, 'True, FormatTimeP "%Y-%m-%d %H:%M:%S" , String)

-- | read in an ssn
--
-- >>> newRefined3P (ssn @OZ) "134-01-2211"
-- Right (Refined3 [134,1,2211] "134-01-2211")
--
-- >>> newRefined3P (ssn @OL) "666-01-2211"
-- Left Step 2. Failed Boolean Check(op) | Bool(0) [number for group 0 invalid: found 666] (True && False | (666 /= 666))
--
-- >>> newRefined3P (ssn @OL) "667-00-2211"
-- Left Step 2. Failed Boolean Check(op) | Bool(1) [number for group 1 invalid: found 0] (1 <= 0)
--
ssn :: OptC opts => Proxy (Ssn opts)
ssn = mkProxy3'

-- | ssn validator
type Ssn (opts :: Opt) = '(opts, Ssnip, Ssnop, Ssnfmt, String)

-- | 'Predicate.Refined3.Refined3' type signature for 'Ssn'
type SsnR (opts :: Opt) = MakeR3 (Ssn opts)
-- | read in a time and validate it
--
-- >>> newRefined3P (hms @OL) "23:13:59"
-- Right (Refined3 [23,13,59] "23:13:59")
--
-- >>> newRefined3P (hms @OL) "23:13:60"
-- Left Step 2. Failed Boolean Check(op) | seconds invalid: found 60
--
-- >>> newRefined3P (hms @OL) "26:13:59"
-- Left Step 2. Failed Boolean Check(op) | hours invalid: found 26
--
hms :: OptC opts => Proxy (Hms opts)
hms = mkProxy3'

-- | 'Predicate.Refined3.Refined3' type signature for 'Hms'
type HmsR (opts :: Opt) = MakeR3 (Hms opts)
-- | validator for time
type Hms (opts :: Opt) = '(opts, Hmsip, Hmsop, Hmsfmt, String)

-- | 'Predicate.Refined3.Refined3' type signature for 'HmsR'
type HmsR' (opts :: Opt) = MakeR3 (Hms' opts)

-- | alternate validator for time
type Hms' (opts :: Opt) = '(opts, Hmsip, Hmsop', Hmsfmt, String)


-- | read in an ipv4 address and validate it using guards
--
-- >>> newRefined3P (ip4 @OZ) "001.223.14.1"
-- Right (Refined3 [1,223,14,1] "001.223.014.001")
--
-- >>> newRefined3P (ip4 @OL) "001.223.14.999"
-- Left Step 2. Failed Boolean Check(op) | octet 3 out of range 0-255 found 999
--
-- >>> newRefined3P (ip4 @OZ) "001.223.14.999.1"
-- Left Step 2. Failed Boolean Check(op) | Guards:invalid length(5) expected 4
--
-- >>> newRefined3P (ip4 @OL) "001.257.14.1"
-- Left Step 2. Failed Boolean Check(op) | octet 1 out of range 0-255 found 257
--
ip4 :: OptC opts => Proxy (Ip4 opts)
ip4 = mkProxy3'

-- | ipv4 validator using guards
type Ip4 (opts :: Opt) = '(opts, Ip4ip, Ip4op, Ip4fmt, String)

-- | 'Predicate.Refined3.Refined3' type signature for 'Ip4'
type Ip4R (opts :: Opt) = MakeR3 (Ip4 opts)

-- | alternate ipv4 validator using boolean predicates
type Ip4' (opts :: Opt) = '(opts, Ip4ip, Ip4op', Ip4fmt, String)

-- | 'Predicate.Refined3.Refined3' signature for 'Ip6'
type Ip6R (opts :: Opt) = MakeR3 (Ip6 opts)
-- | Ipv6 validator using guards
type Ip6 (opts :: Opt) = '(opts, Ip6ip, Ip6op, Ip6fmt, String)

-- | proxy for 'Ip6'
ip6 :: Proxy (Ip6 opts)
ip6 = Proxy

-- | validate isbn10 using guards
--
-- >>> newRefined3P (isbn10 @OZ) "0-306-40611-X"
-- Right (Refined3 ([0,3,0,6,4,0,6,1,1],10) "030640611-X")
--
-- >>> newRefined3P (isbn10 @OZ) "0-306-40611-9"
-- Left Step 2. Failed Boolean Check(op) | mod 0 oops
--
isbn10 :: Proxy (Isbn10 opts)
isbn10 = Proxy

-- | validate isbn10 using guards
type Isbn10 (opts :: Opt) = '(opts, Isbn10ip, Isbn10op, Isbn10fmt, String)

-- | 'Predicate.Refined3.Refined3' type signature for 'Isbn10'
type Isbn10R (opts :: Opt) = MakeR3 (Isbn10 opts)

-- | validate isbn13 using guards
--
-- >>> newRefined3P (isbn13 @OZ) "978-0-306-40615-7"
-- Right (Refined3 [9,7,8,0,3,0,6,4,0,6,1,5,7] "978030640615-7")
--
-- >>> newRefined3P (isbn13 @OZ) "978-0-306-40615-8"
-- Left Step 2. Failed Boolean Check(op) | sum=101 mod 10=1
--
isbn13 :: Proxy (Isbn13 opts)
isbn13 = Proxy

-- | validate isbn13 using guards
type Isbn13 (opts :: Opt) = '(opts, Isbn13ip, Isbn13op, Isbn13fmt, String)

-- | 'Predicate.Refined3.Refined3' signature for 'Isbn13'
type Isbn13R (opts :: Opt) = MakeR3 (Isbn13 opts)

-- | base 16 validator with a predicate @p@
--
-- >>> newRefined3P (base16 @OZ @'True) "00fe"
-- Right (Refined3 254 "fe")
--
base16 :: Proxy (BaseN opts 16 p)
base16 = basen

-- | read show for base @n@ with a predicate
--
-- >>> newRefined3P (basen @OZ @16 @(100 <..> 400)) "00fe"
-- Right (Refined3 254 "fe")
--
-- >>> newRefined3P (basen @OZ @16 @(GuardSimple (Id < 400) >> 'True)) "f0fe"
-- Left Step 2. Failed Boolean Check(op) | (61694 < 400)
--
-- >>> newRefined3P (basen @OZ @16 @(GuardBool (PrintF "oops bad hex=%d" Id) (Id < 400))) "f0fe"
-- Left Step 2. Failed Boolean Check(op) | oops bad hex=61694
--
-- >>> newRefined3P (basen @OL @16 @(Id < 400)) "f0fe"
-- Left Step 2. False Boolean Check(op) | {61694 < 400}
--
basen :: Proxy (BaseN opts n p)
basen = mkProxy3

-- | read show for base @n@
type BaseN (opts :: Opt) (n :: Nat) p = '(opts, ReadBase Int n, p, ShowBase n, String)

-- | base 2 validator with a predicate @p@
base2 :: Proxy (BaseN opts 2 p)
base2 = basen

-- | proxy for the 'DateN' validator
daten :: OptC opts => Proxy (DateN opts)
daten = mkProxy3'

-- | proxy for the 'DateTimeN' validator
datetimen :: OptC opts => Proxy (DateTimeN opts)
datetimen = mkProxy3'

-- | ensures that two numbers are in a given range (emulates 'Predicate.Refined.Refined')
--
-- >>> newRefined3P (between @OZ @10 @16) 14
-- Right (Refined3 14 14)
--
-- >>> newRefined3P (between @OZ @10 @16) 17
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> newRefined3P (between @OAN @10 @16) 17
-- Left Step 2. False Boolean Check(op) | {17 <= 16}
-- *** Step 1. Success Initial Conversion(ip) (17) ***
-- P Id 17
-- *** Step 2. False Boolean Check(op) ***
-- False 17 <= 16
-- |
-- +- P Id 17
-- |
-- +- P '10
-- |
-- `- P '16
--
between :: Proxy (BetweenN opts m n)
between = mkProxy3

-- | validate an int is between @m@ and @n@
type BetweenN (opts :: Opt) m n = '(opts, Id, Between m n Id, Id, Int)

-- | luhn check a string of a given length @n@
type LuhnR (opts :: Opt) (n :: Nat) = MakeR3 (LuhnT opts n)

-- | Luhn check using builtin 'Predicate.Data.Extra.IsLuhn'
--
-- >>> newRefined3P (Proxy @(LuhnT OZ 4)) "1230"
-- Right (Refined3 [1,2,3,0] "1230")
--
-- >>> newRefined3P (Proxy @(LuhnT OL 4)) "1234"
-- Left Step 2. False Boolean Check(op) | {True && False | (IsLuhn map=[2,2,6,4] sum=14 ret=4 | [1,2,3,4])}
--
type LuhnT (opts :: Opt) (n :: Nat) =
   '(opts
    , Map' (ReadP Int Id) Ones
    , Msg "incorrect number of digits:"
          (Len == n) && IsLuhn
    , ConcatMap (ShowP Id) Id
    , String)

-- | Sim card check using builtin 'Predicate.Data.Extra.LuhnDigit'
--
-- >>> newRefined3P (Proxy @(SimT OZ)) "12345678901234567859"
-- Right (Refined3 [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,5,9] "12345678901234567859")
--
-- >>> newRefined3P (Proxy @(SimT OZ)) "12345678901234567"
-- Left Step 2. Failed Boolean Check(op) | invalid length expected 20 found 17
--
-- >>> newRefined3P (Proxy @(SimT OZ)) "12345678901234567819"
-- Left Step 2. Failed Boolean Check(op) | bad first checkdigit was given as 1 but should be 5
--
-- >>> newRefined3P (Proxy @(SimT OZ)) "12345678901234567852"
-- Left Step 2. Failed Boolean Check(op) | bad second checkdigit was given as 2 but should be 9
--
type SimT (opts :: Opt) =
   '(opts
    , Map' (ReadP Int Id) Ones
    , Guard (PrintF "invalid length expected 20 found %d" Len) (Len == 20)
   >> SplitAt 18 Id
   >> '(LuhnDigit << Fst,LuhnDigit << Drop 6 Fst,Snd)
   >> Guard (PrintT "bad first checkdigit was given as %d but should be %d" '(Head << Thd,Fst)) ((Head << Thd) == Fst)
   >> Guard (PrintT "bad second checkdigit was given as %d but should be %d" '(Last << Thd,Snd)) ((Last << Thd) == Snd)
   >> 'True
    , ConcatMap (ShowP Id) Id
    , String)

-- | noop true
type Ok (opts :: Opt) (t :: Type) = '(opts, Id, 'True, Id, t)
-- | 'Predicate.Refined3.Refined3' signature for 'Ok'
type OkR (opts :: Opt) (t :: Type) = MakeR3 (Ok opts t)

-- | noop true proxy
ok :: Proxy (Ok opts t)
ok = mkProxy3

-- | noop false
type OkNot (t :: Type) = '(OAN, Id, 'False, Id, t)
-- | 'Predicate.Refined3.Refined3' signature for 'OkNot'
type OkNotR (t :: Type) = MakeR3 (OkNot t)

-- | noop false proxy
oknot :: Proxy (OkNot t)
oknot = mkProxy3

-- | convert a string from a given base \'i\' and store it internally as a base \'j\' string with a predicate \'p\'
--
-- >>> newRefined3P (Proxy @(BaseIJ' OL 16 2 (ReadBase Int 2 < 1000))) "ffe"
-- Left Step 2. False Boolean Check(op) | {4094 < 1000}
--
type BaseIJ' (opts :: Opt) (i :: Nat) (j :: Nat) p = '(opts, ReadBase Int i >> ShowBase j, p, ReadBase Int j >> ShowBase i, String)

-- | convert a string from a given base \'i\' and store it internally as a base \'j\' string
--
-- >>> newRefined3P (Proxy @(BaseIJ OZ 16 2)) "fe"
-- Right (Refined3 "11111110" "fe")
--
-- >>> newRefined3P (Proxy @(BaseIJ OZ 16 2)) "fge"
-- Left Step 1. Failed Initial Conversion(ip) | invalid base 16
--
type BaseIJ (opts :: Opt) (i :: Nat) (j :: Nat) = BaseIJ' opts i j 'True

-- | take any valid Read/Show instance and turn it into a valid 'Predicate.Refined3.Refined3'
--
-- >>> :m + Data.Ratio
-- >>> newRefined3P (readshow @OZ @Rational) "13 % 3"
-- Right (Refined3 (13 % 3) "13 % 3")
--
-- >>> newRefined3P (readshow @OZ @Rational) "13x % 3"
-- Left Step 1. Failed Initial Conversion(ip) | ReadP Ratio Integer (13x % 3)
--
-- >>> newRefined3P (readshow' @OZ @Rational @(3 % 1 <..> 5 % 1)) "13 % 3"
-- Right (Refined3 (13 % 3) "13 % 3")
--
-- >>> newRefined3P (readshow @OZ @UTCTime) "2018-10-19 14:53:11.5121359 UTC"
-- Right (Refined3 2018-10-19 14:53:11.5121359 UTC "2018-10-19 14:53:11.5121359 UTC")
--
-- >>> :m + Data.Aeson
-- >>> newRefined3P (readshow @OZ @Value) "String \"jsonstring\""
-- Right (Refined3 (String "jsonstring") "String \"jsonstring\"")
--
-- >>> newRefined3P (readshow @OZ @Value) "Number 123.4"
-- Right (Refined3 (Number 123.4) "Number 123.4")
--
readshow :: Proxy (ReadShow opts t)
readshow = mkProxy3

-- | take any valid Read/Show instance and turn it into a valid 'Predicate.Refined3.Refined3'
type ReadShow (opts :: Opt) (t :: Type) = '(opts, ReadP t Id, 'True, ShowP Id, String)

-- | 'Predicate.Refined3.Refined3' signature for 'ReadShow'
type ReadShowR (opts :: Opt) (t :: Type) = MakeR3 (ReadShow opts t)

-- | take any valid Read/Show instance and turn it into a valid 'Predicate.Refined3.Refined3' with a given predicate @p@
--
-- >>> :m + Data.Ratio
-- >>> newRefined3P (Proxy @(ReadShow' OZ Rational (11 -% 2 <..> 3 -% 1))) "-13 % 3"
-- Right (Refined3 ((-13) % 3) "(-13) % 3")
--
-- >>> newRefined3P (Proxy @(ReadShow' OZ Rational (Id > (15 % 1)))) "13 % 3"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> newRefined3P (Proxy @(ReadShow' OL Rational (Msg (PrintF "invalid=%3.2f" (FromRational Double)) (Id > (15 % 1))))) "13 % 3"
-- Left Step 2. False Boolean Check(op) | {invalid=4.33 13 % 3 > 15 % 1}
--
-- >>> newRefined3P (Proxy @(ReadShow' OZ Rational (Id > (11 % 1)))) "13 % 3"
-- Left Step 2. False Boolean Check(op) | FalseP
--
type ReadShow' (opts :: Opt) (t :: Type) p = '(opts, ReadP t Id, p, ShowP Id, String)
-- | 'Predicate.Refined3.Refined3' signature for 'ReadShow''
type ReadShowR' (opts :: Opt) (t :: Type) p = MakeR3 (ReadShow' opts t p)

-- | simple read show proxy with a predicate
readshow' :: Proxy (ReadShow' opts t p)
readshow' = mkProxy3

-- | test tuple type families
--
-- >>> pl @(T5_2 (Ip4 OL)) "1.2.3.4"
-- Present [1,2,3,4] (Map [1,2,3,4] | ["1","2","3","4"])
-- Val [1,2,3,4]
--
-- >>> pl @(T5_3 (Ip4 OL)) [141,213,308,4]
-- Error octet 2 out of range 0-255 found 308 (Guard(2) 308)
-- Fail "octet 2 out of range 0-255 found 308"
--
-- >>> pl @(T5_3 (Ip4 OL)) [141,213,308,4,8]
-- Error Guards:invalid length(5) expected 4
-- Fail "Guards:invalid length(5) expected 4"
--
-- >>> pl @(T5_4 (Ip4 OL)) [141,513,9,4]
-- Present "141.513.009.004" ((>>) "141.513.009.004" | {PrintI [141.513.009.004] | s=%03d.%03d.%03d.%03d})
-- Val "141.513.009.004"
--
