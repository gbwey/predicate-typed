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
-- |
-- Module      : Refined3Helper
-- Description : Contains convenient prepackaged 4-tuples to use with Refined3
-- Copyright   : (c) Grant Weyburne, 2019
-- License     : BSD-3
-- Maintainer  : gbwey9@gmail.com
--
-- Prepackaged proxies for use with 'Refined3.Refined3'
--
module Refined3Helper where
import Refined3
import Predicate
import Data.Proxy
import GHC.TypeLits (AppendSymbol,Nat,KnownNat)
import Data.Kind (Type)
import Data.Time
import qualified Data.Semigroup as SG

-- | credit card with luhn algorithm
--
-- >>> prtEval3P cc11 ol "1234-5678-901"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> prtEval3P cc11 ol "1234-5678-903"
-- Right (Refined3 {r3In = [1,2,3,4,5,6,7,8,9,0,3], r3Out = "1234-5678-903"})
--
type Ccip = Map (ReadP Int) (Ones (Remove "-" Id))
type Ccop (n :: Nat) = Guard ('(n,Len) >> Printf2 "expected %d digits but found %d") (Len >> Same n) >> Luhn Id
type Ccfmt (ns :: [Nat]) = ConcatMap (ShowP Id) Id >> SplitAts ns Id >> Concat (Intercalate '["-"] Id)

type Ccn (ns :: [Nat]) = '(Ccip, Ccop (SumT ns), Ccfmt ns, String)

type CC11 = Ccn '[4,4,3]

-- not great for the general case: but specific case is easier
ccn :: forall (ns :: [Nat]) . (KnownNat (SumT ns), P ns String, PP ns String ~ [Integer]) => Proxy (Ccn ns)
ccn = mkProxy3

cc11 :: Proxy (Ccn '[4,4,3])   -- or Proxy CC11
cc11 = mkProxy3P

-- | read in a datetime
--
-- >>> prtEval3P (Proxy @(DateTime1 UTCTime)) ol "2018-09-14 02:57:04"
-- Right (Refined3 {r3In = 2018-09-14 02:57:04 UTC, r3Out = "2018-09-14 02:57:04"})
--
type DateTime1 (t :: Type) = '(Dtip1 t, Dtop1, Dtfmt1, String)
type Dtip1 t = ParseTimeP t "%F %T" Id

-- extra check to validate the time as parseTime doesnt validate the time component
type Dtop1 =
   Map (ReadP Int) (FormatTimeP "%H %M %S" Id >> Resplit "\\s+" Id)
     >> Guards '[ '(Printf2 "guard %d invalid hours %d", Between 0 23)
                , '(Printf2 "guard %d invalid minutes %d", Between 0 59)
                , '(Printf2 "guard %d invalid seconds %d", Between 0 59)
                ] >> 'True
type Dtfmt1 = FormatTimeP "%F %T" Id

ssn :: Proxy Ssn
ssn = mkProxy3

-- | read in an ssn
--
-- >>> prtEval3P ssn ol "134-01-2211"
-- Right (Refined3 {r3In = [134,1,2211], r3Out = "134-01-2211"})
--
-- >>> prtEval3P ssn ol "666-01-2211"
-- Left Step 2. Failed Boolean Check(op) | number for group 1 invalid: found 666
--
-- >>> prtEval3P ssn ol "666-01-2211"
-- Left Step 2. Failed Boolean Check(op) | number for group 1 invalid: found 666
--
-- >>> prtEval3P ssn ol "667-00-2211"
-- Left Step 2. Failed Boolean Check(op) | number for group 2 invalid: found 0
--
-- >>> prtEval3P ssn ol "666-01-2211"
-- Left Step 2. Failed Boolean Check(op) | number for group 1 invalid: found 666
--
-- >>> prtEval3P ssn ol "991-22-9999"
-- Left Step 2. Failed Boolean Check(op) | number for group 1 invalid: found 991
--
type Ssn = '(Ssnip, Ssnop, Ssnfmt, String)

type Ssnip = Map (ReadP Int) (Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" Id >> Snd OneP)
type Ssnop = GuardsQuick (Printf2 "number for group %d invalid: found %d")
                     '[Between 1 899 && Id /= 666, Between 1 99, Between 1 9999]
                      >> 'True
type Ssnop' = Guards '[ '(Printf2 "guard %d invalid: found %d", Between 1 899 && Id /= 666)
                  , '(Printf2 "group %d invalid: found %d", Between 1 99)
                  , '(Printf2 "group %d invalid: found %d", Between 1 9999)
                  ] >> 'True
type Ssnfmt = Printfnt 3 "%03d-%02d-%04d"

-- | read in a time and validate it
--
-- >>> prtEval3P hms ol "23:13:59"
-- Right (Refined3 {r3In = [23,13,59], r3Out = "23:13:59"})
--
-- >>> prtEval3P hms ol "23:13:60"
-- Left Step 2. Failed Boolean Check(op) | guard(3) 60 secs is out of range
--
-- >>> prtEval3P hms ol "26:13:59"
-- Left Step 2. Failed Boolean Check(op) | guard(1) 26 hours is out of range
--
hms :: Proxy Hms
hms = mkProxy3

type Hms = '(Hmsip, Hmsop >> 'True, Hmsfmt, String)

type Hmsip = Map (ReadP Int) (Resplit ":" Id)
type Hmsop = Guard (Printf "expected len 3 but found %d" Len) (Len >> Same 3)
             >> Guards '[ '(Printf2 "guard(%d) %d hours is out of range", Between 0 23)
                        , '(Printf2 "guard(%d) %d mins is out of range", Between 0 59)
                        , '(Printf2 "guard(%d) %d secs is out of range", Between 0 59)]
type Hmsfmt = Printfnt 3 "%02d:%02d:%02d"

-- | read in an ipv4 address and validate it
--
-- >>> prtEval3P ip ol "001.223.14.1"
-- Right (Refined3 {r3In = [1,223,14,1], r3Out = "001.223.014.001"})
--
-- >>> prtEval3P ip ol "001.223.14.999"
-- Left Step 2. Failed Boolean Check(op) | guard(4) octet out of range 0-255 found 999
--
-- >>> prtEval3P ip ol "001.223.14.999.1"
-- Left Step 1. Initial Conversion(ip) Failed | Regex no results
--
-- >>> prtEval3P ip ol "001.257.14.1"
-- Left Step 2. Failed Boolean Check(op) | guard(2) octet out of range 0-255 found 257
--
type Ip = '(Ipip, Ipop, Ipfmt, String)

ip :: Proxy Ip
ip = mkProxy3

type Ipip = Map (ReadP Int) (Rescan "^(\\d{1,3}).(\\d{1,3}).(\\d{1,3}).(\\d{1,3})$" Id >> OneP >> Snd Id)
-- RepeatT is a type family so it expands everything! replace RepeatT with a type class
type Ipop = GuardsN (Printf2 "guard(%d) octet out of range 0-255 found %d") 4 (Between 0 255) >> 'True
type Ipfmt = Printfnt 4 "%03d.%03d.%03d.%03d"

type HmsRE = "^([0-1][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9])$" -- padded only -- dumb because strict validation should not be done twice: ie in ip and op!
type Hmsconv = Do '[Rescan HmsRE Id, Head, (Snd Id), Map (ReadBaseInt 10) Id]
type Hmsval = GuardsQuick (Printf2 "guard(%d) %d is out of range") '[Between 0 23, Between 0 59, Between 0 59]

type Hms4 = '(Hmsconv, Hmsval >> 'True, Hmsfmt, String)

hms4 :: Proxy Hms4
hms4 = mkProxy3

type OctetRE = "(25[0-5]|2[0..4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])" -- no padded numbers allowed
--type Ip4StrictRE = "^" `AppendSymbol` OctetRE `AppendSymbol` "\\." `AppendSymbol` OctetRE `AppendSymbol` "\\." `AppendSymbol` OctetRE `AppendSymbol` "\\." `AppendSymbol` OctetRE `AppendSymbol` "$"
type Ip4StrictRE = "^" `AppendSymbol` IntersperseT "\\." (RepeatT 4 OctetRE) `AppendSymbol` "$"

-- valid dates for for DateFmts are "2001-01-01" "Jan 24 2009" and "03/29/07"
type DateFmts = '["%Y-%m-%d", "%m/%d/%y", "%B %d %Y"]
type DateN = '(ParseTimes Day DateFmts Id, 'True, FormatTimeP "%Y-%m-%d" Id, String)

type DateTimeFmts = '["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"]
type DateTimeN = '(ParseTimes UTCTime DateTimeFmts Id, 'True, FormatTimeP "%Y-%m-%d %H:%M:%S" Id, String)

-- | convert a string from the given base \'i\' but stores it internally as a string of base \'j\'
--
-- >>> prtEval3P (Proxy @(BaseN 16)) ol "00fe"
-- Right (Refined3 {r3In = 254, r3Out = "fe"})
--
type BaseN (n :: Nat) = '(ReadBase Integer n, 'True, ShowBase n, String)

base16 :: Proxy (BaseN 16)
base16 = mkProxy3

daten :: Proxy DateN
daten = mkProxy3

datetimen :: Proxy DateTimeN
datetimen = mkProxy3

type BetweenR m n = Refined3 Id (Between m n) Id Int

type LuhnR (n :: Nat) = MakeR3 (LuhnY n)
type LuhnR' (n :: Nat) = MakeR3 (LuhnX n)

-- uses builtin Luhn vs long winded version LuhnX
type LuhnY (n :: Nat) =
   '(Map (ReadP Int) (Ones Id)
   , Guard (Printfn "incorrect number of digits found %d but expected %d in [%s]" (TupleI '[Len, W n, ShowP Id]))
           (Len >> Same n)
     >> GuardSimple (Luhn Id) >> 'True
   , ConcatMap (ShowP Id) Id
   , String)

type LuhnX (n :: Nat) =
   '(Map (ReadP Int) (Ones Id)
   , Luhn'' n >> 'True
   , ConcatMap (ShowP Id) Id
   , String)

type Luhn'' (n :: Nat) =
         Guard (Printfn "incorrect number of digits found %d but expected %d in [%s]" (TupleI '[Len, W n, ShowP Id])) (Len >> Same n)
      >> Do '[
              Reverse
             ,Ziplc [1,2] Id
             ,Map (Fst Id * Snd Id >> If (Id >= 10) (Id - 9) Id) Id
             ,FoldMap (SG.Sum Int) Id
             ]
        >> Guard (Printfn "expected %d mod 10 = 0 but found %d" (TupleI '[Id, Id `Mod` 10])) (Mod Id 10 >> Same 0)

type Luhn' (n :: Nat) =
       Msg "Luhn'" (Do
       '[Guard (Printfn "incorrect number of digits found %d but expected %d in [%s]" (TupleI '[Len, W n, Id])) (Len >> Same n)
        ,Do
            '[Ones Id
            ,Map (ReadP Int) Id
            ,Reverse
            ,Ziplc [1,2] Id
            ,Map (Fst Id * Snd Id >> If (Id >= 10) (Id - 9) Id) Id
            ,FoldMap (SG.Sum Int) Id
           ]
        ,Guard (Printfn "expected %d mod 10 = 0 but found %d" (TupleI '[Id, Id `Mod` 10])) (Mod Id 10 >> Same 0)
        ])

-- noop true
type Ok (t :: Type) = '(Id, 'True, Id, t)
type OkR (t :: Type) = MakeR3 (Ok t)

-- noop false
type OkNot (t :: Type) = '(Id, 'False, Id, t)
type OkNotR (t :: Type) = MakeR3 (OkNot t)

-- | convert a string from the given base \'i\' but stores it internally as a string of base \'j\'
--
-- >>> prtEval3P (Proxy @(BaseIJ 16 2)) ol "fe"
-- Right (Refined3 {r3In = "11111110", r3Out = "fe"})
--
-- >>> prtEval3P (Proxy @(BaseIJ 16 2)) ol "fge"
-- Left Step 1. Initial Conversion(ip) Failed | invalid base 16
--
type BaseIJ (i :: Nat) (j :: Nat) = BaseIJ' i j 'True
type BaseIJ' (i :: Nat) (j :: Nat) p = '(ReadBase Int i >> ShowBase j, p, ReadBase Int j >> ShowBase i, String)

-- | take any valid Read/Show instance and turn it into a valid Refined3
--
-- >>> :m + Data.Ratio
-- >>> prtEval3P (Proxy @(ReadShow Rational)) ol "13 % 3"
-- Right (Refined3 {r3In = 13 % 3, r3Out = "13 % 3"})
--
-- >>> prtEval3P (Proxy @(ReadShow Rational)) ol "13x % 3"
-- Left Step 1. Initial Conversion(ip) Failed | ReadP Ratio Integer (13x % 3) failed
--
-- >>> prtEval3P (Proxy @(ReadShow' Rational (Between (3 % 1) (5 % 1)))) ol "13 % 3"
-- Right (Refined3 {r3In = 13 % 3, r3Out = "13 % 3"})
--
-- >>> prtEval3P (Proxy @(ReadShow' Rational (Between (11 %- 2) (3 %- 1)))) ol "-13 % 3"
-- Right (Refined3 {r3In = (-13) % 3, r3Out = "(-13) % 3"})
--
-- >>> prtEval3P (Proxy @(ReadShow' Rational (Id > (15 % 1)))) ol "13 % 3"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> prtEval3P (Proxy @(ReadShow' Rational (Guard (Printf "invalid=%3.2f" (FromRational Double Id)) (Id > (15 % 1)) >> 'True))) ol "13 % 3"
-- Left Step 2. Failed Boolean Check(op) | invalid=4.33
--
-- >>> prtEval3P (Proxy @(ReadShow' Rational (Id > (11 % 1)))) ol "13 % 3"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> let tmString = "2018-10-19 14:53:11.5121359 UTC"
-- >>> let tm = read tmString :: UTCTime
-- >>> prtEval3P (Proxy @(ReadShow UTCTime)) ol tmString
-- Right (Refined3 {r3In = 2018-10-19 14:53:11.5121359 UTC, r3Out = "2018-10-19 14:53:11.5121359 UTC"})
--
-- >>> :m + Data.Aeson
-- >>> prtEval3P (Proxy @(ReadShow Value)) ol "String \"jsonstring\""
-- Right (Refined3 {r3In = String "jsonstring", r3Out = "String \"jsonstring\""})
--
-- >>> prtEval3P (Proxy @(ReadShow Value)) ol "Number 123.4"
-- Right (Refined3 {r3In = Number 123.4, r3Out = "Number 123.4"})
--
type ReadShow (t :: Type) = '(ReadP t, 'True, ShowP Id, String)
type ReadShowR (t :: Type) = MakeR3 (ReadShow t)

type ReadShow' (t :: Type) p = '(ReadP t, p, ShowP Id, String)
type ReadShowR' (t :: Type) p = MakeR3 (ReadShow' t p)
