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
-- | Contains prepackaged 4-tuples to use with 'Predicate.Refined2.Refined2'
module Predicate.Examples.Refined2 (
 -- ** datetime
    DateTime1
  , datetime1
  , daten
  , DateN
  , datetimen
  , DateTimeN
  , DateTimeNR

  -- ** time
  , hms
  , Hms
  , HmsR

  -- ** luhn check
  , Luhn
  , luhn11

  -- ** ssn
  , ssn
  , Ssn
  , SsnR

  -- ** ipv4
  , ip4
  , Ip4
  , Ip4R

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
  , BaseIJ
  , BaseIJ'
  , BaseIJip
   ) where
import Predicate.Refined2
import Predicate.Examples.Common
import Predicate
import GHC.TypeLits (Nat)
import Data.Time (Day, UTCTime)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XTemplateHaskell
-- >>> :set -XTypeApplications
-- >>> :m + Data.Time
-- >>> :m + Control.Lens

-- | credit card check using luhn algorithm
--
-- >>> newRefined2 @OZ @Luhnip @(Luhnop 11) "1234-5678-901"
-- Left Step 2. Failed Boolean Check(op) | invalid checkdigit
--
-- >>> newRefined2 @OZ @Luhnip @(Luhnop 11) "1234-5678-903"
-- Right (Refined2 [1,2,3,4,5,6,7,8,9,0,3] "1234-5678-903")
--
-- >>> pz @(Luhnip >> Luhnop 11) "79927398713"
-- Val True
--
-- >>> pz @(Luhnip >> Luhnop 10) "79927398713"
-- Fail "expected 10 digits but found 11"
--
-- >>> newRefined2P (luhn11 @OZ) "1234-5678-903"
-- Right (Refined2 [1,2,3,4,5,6,7,8,9,0,3] "1234-5678-903")
--
-- >>> newRefined2 @OZ @Luhnip @(Luhnop 4) "1230"
-- Right (Refined2 [1,2,3,0] "1230")
--
-- >>> newRefined2 @OL @Luhnip @(Luhnop 4) "1234"
-- Left Step 2. Failed Boolean Check(op) | invalid checkdigit
--
luhn11 :: Proxy (Luhn opts 11)
luhn11 = Proxy

-- | luhn checkdigit validator for @n@ digits
type Luhn (opts :: Opt) (n :: Nat) = '(opts, Luhnip, Luhnop n, String)


-- | read in a valid datetime
--
-- >>> newRefined2 @OL @(Dtip LocalTime) @'True "2018-09-14 02:57:04"
-- Right (Refined2 2018-09-14 02:57:04 "2018-09-14 02:57:04")
--
-- >>> newRefined2 @OL @(Dtip LocalTime) @'True "2018-09-99 12:12:12"
-- Left Step 1. Failed Initial Conversion(ip) | ParseTimeP LocalTime (%F %T) failed to parse
--
-- >>> newRefined2P (datetime1 @OZ @LocalTime) "2018-09-14 02:57:04"
-- Right (Refined2 2018-09-14 02:57:04 "2018-09-14 02:57:04")
--
datetime1 :: Proxy (DateTime1 opts t)
datetime1 = mkProxy2

-- | datetime validator with default predicate of true
type DateTime1 (opts :: Opt) (t :: Type) = '(opts, Dtip t, 'True, String)

-- | proxy for 'DateTimeN'
datetimen :: OptC opts => Proxy (DateTimeN opts)
datetimen = mkProxy2'

-- valid dates for for DateFmts eg "2001-01-01" "Jan 24 2009" and "03/29/07"
-- | date validator which tries to match against multiple date formats
type DateN (opts :: Opt) = '(opts,ParseTimes Day DateFmts Id, 'True, String)

-- | proxy for 'DateN'
daten :: OptC opts => Proxy (DateN opts)
daten = mkProxy2'

-- | 'Predicate.Refined2.Refined2' type signature for 'DateTimeN'
type DateTimeNR (opts :: Opt) = MakeR2 (DateTimeN opts)

-- | datetime validator which tries to match against multiple datetime formats
type DateTimeN (opts :: Opt) = '(opts, ParseTimes UTCTime DateTimeFmts Id, 'True, String)

-- | read in an ssn
--
-- >>> newRefined2 @OZ @Ssnip @Ssnop "134-01-2211"
-- Right (Refined2 [134,1,2211] "134-01-2211")
--
-- >>> newRefined2 @OL @Ssnip @Ssnop "666-01-2211"
-- Left Step 2. Failed Boolean Check(op) | Bool(0) [number for group 0 invalid: found 666] (True && False | (666 /= 666))
--
-- >>> newRefined2 @OL @Ssnip @Ssnop "667-00-2211"
-- Left Step 2. Failed Boolean Check(op) | Bool(1) [number for group 1 invalid: found 0] (1 <= 0)
--
-- >>> newRefined2P (ssn @OL) "667-00-2211"
-- Left Step 2. Failed Boolean Check(op) | Bool(1) [number for group 1 invalid: found 0] (1 <= 0)
--
ssn :: OptC opts => Proxy (Ssn opts)
ssn = mkProxy2'

-- | 'Predicate.Refined2.Refined2' signature for 'Ssn'
type SsnR (opts :: Opt) = MakeR2 (Ssn opts)

-- | ssn validator
type Ssn (opts :: Opt) = '(opts, Ssnip, Ssnop, String)


-- | read in a time and validate it
--
-- >>> newRefined2 @OL @Hmsip @Hmsop' "23:13:59"
-- Right (Refined2 [23,13,59] "23:13:59")
--
-- >>> newRefined2 @OL @Hmsip @Hmsop' "23:13:60"
-- Left Step 2. Failed Boolean Check(op) | Bool(2) [seconds] (60 <= 59)
--
-- >>> newRefined2 @OL @Hmsip @Hmsop' "26:13:59"
-- Left Step 2. Failed Boolean Check(op) | Bool(0) [hours] (26 <= 23)
--
-- >>> newRefined2P (hms @OL) "23:13:59"
-- Right (Refined2 [23,13,59] "23:13:59")
--
hms :: OptC opts => Proxy (Hms opts)
hms = mkProxy2'

-- | 'Predicate.Refined2.Refined2' type signature for 'Hms'
type HmsR (opts :: Opt) = MakeR2 (Hms opts)

-- | time validator
type Hms (opts :: Opt) = '(opts, Hmsip, Hmsop, String)

-- | read in an ipv4 address and validate it using guards
--
-- >>> newRefined2 @OZ @Ip4ip @Ip4op' "001.223.14.1"
-- Right (Refined2 [1,223,14,1] "001.223.14.1")
--
-- >>> newRefined2 @OL @Ip4ip @Ip4op' "001.223.14.999"
-- Left Step 2. Failed Boolean Check(op) | Bool(3) [octet 3 out of range 0-255 found 999] (999 <= 255)
--
-- >>> newRefined2P (ip4 @OL) "001.223.14.999"
-- Left Step 2. Failed Boolean Check(op) | octet 3 out of range 0-255 found 999
--
-- >>> newRefined2P (ip4 @OL) "001.223.14.999.1"
-- Left Step 2. Failed Boolean Check(op) | Guards:invalid length(5) expected 4
--
-- >>> newRefined2P (ip4 @OL) "001.257.14.1"
-- Left Step 2. Failed Boolean Check(op) | octet 1 out of range 0-255 found 257
--
ip4 :: Proxy (Ip4 opts)
ip4 = Proxy

-- | validator for ipv4 addresses
type Ip4 (opts :: Opt) = '(opts, Ip4ip, Ip4op, String)

-- | 'Predicate.Refined2.Refined2' type signature for 'Ip4'
type Ip4R (opts :: Opt) = MakeR2 (Ip4 opts)

-- | 'Predicate.Refined2.Refined2' type signature for 'Ip6'
type Ip6R (opts :: Opt) = MakeR2 (Ip6 opts)
-- | validator for ipv6 using guards
type Ip6 (opts :: Opt) = '(opts, Ip6ip, Ip6op, String)

-- | proxy for 'Ip6'
ip6 :: Proxy (Ip6 opts)
ip6 = Proxy

-- | validate isbn10 using guards
--
-- >>> newRefined2P (isbn10 @OZ) "0-306-40611-X"
-- Right (Refined2 ([0,3,0,6,4,0,6,1,1],10) "0-306-40611-X")
--
-- >>> newRefined2P (isbn10 @OZ) "0-306-40611-9"
-- Left Step 2. Failed Boolean Check(op) | mod 0 oops
--
isbn10 :: Proxy (Isbn10 opts)
isbn10 = Proxy

-- | validator for isbn10
type Isbn10 (opts :: Opt) = '(opts, Isbn10ip, Isbn10op, String)

-- | 'Predicate.Refined2.Refined2' type signature for 'Isbn10'
type Isbn10R (opts :: Opt) = MakeR2 (Isbn10 opts)

-- | validate isbn13
--
-- >>> newRefined2P (isbn13 @OZ) "978-0-306-40615-7"
-- Right (Refined2 [9,7,8,0,3,0,6,4,0,6,1,5,7] "978-0-306-40615-7")
--
-- >>> newRefined2P (isbn13 @OZ) "978-0-306-40615-8"
-- Left Step 2. Failed Boolean Check(op) | sum=101 mod 10=1
--
isbn13 :: Proxy (Isbn13 opts)
isbn13 = Proxy

-- | validate isbn13 using guards
type Isbn13 (opts :: Opt) = '(opts, Isbn13ip, Isbn13op, String)
-- | 'Predicate.Refined2.Refined2' type signature for 'Isbn10'
type Isbn13R (opts :: Opt) = MakeR2 (Isbn13 opts)

-- | 4-tuple for reading for base @n@
type BaseN (opts :: Opt) (n :: Nat) p = '(opts, ReadBase Int n, p, String)

-- | convert a string from a given base \'i\' and store it internally as a base \'j\' string
--
-- >>> newRefined2 @OZ @(BaseIJip 16 2) @'True "fe"
-- Right (Refined2 "11111110" "fe")
--
-- >>> newRefined2 @OZ @(BaseIJip 16 2) @'True "fge"
-- Left Step 1. Failed Initial Conversion(ip) | invalid base 16
--
-- >>> newRefined2 @OL @(BaseIJip 16 2) @(ReadBase Int 2 < 1000) "ffe"
-- Left Step 2. False Boolean Check(op) | {4094 < 1000}
--
type BaseIJip (i :: Nat) (j :: Nat) = ReadBase Int i >> ShowBase j

-- | convert a string from a given base \'i\' and store it internally as a base \'j\' string with a predicate \'p\'
type BaseIJ' (i :: Nat) (j :: Nat) p = '(ReadBase Int i >> ShowBase j, p, String)

-- | convert a string from a given base \'i\' and store it internally as a base \'j\' string
type BaseIJ (i :: Nat) (j :: Nat) = BaseIJ' i j 'True
