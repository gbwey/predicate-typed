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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
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
import UtilP
import Data.Proxy
import GHC.TypeLits (AppendSymbol,Nat,KnownNat)
import Data.Kind (Type)
import Data.Time
import qualified Data.Semigroup as SG

-- credit card with luhn algorithm
type Ccip = Remove "-" Id >> Ones >> Map (ReadP Int)
type Ccop (n :: Nat) = Guard ('(n,Len) >> Printf2 "expected %d digits but found %d") (Len >> Same n) >> Luhn
type Ccfmt (ns :: [Nat]) = Map ShowP >> Concat >> SplitAts ns >> Intercalate '["-"] Id >> Concat

type Ccn (ns :: [Nat]) = '(Ccip, Ccop (SumT ns), Ccfmt ns, String)

type CC11 = Ccn '[4,4,3]

-- not great for the general case: but specific case is easier
ccn :: forall (ns :: [Nat]) . (KnownNat (SumT ns), P ns String, PP ns String ~ [Integer]) => Proxy (Ccn ns)
ccn = mkProxy3

cc11 :: Proxy (Ccn '[4,4,3])   -- or Proxy CC11
cc11 = mkProxy3P

type DateTime1 (t :: Type) = '(Dtip1 t, Dtop1, Dtfmt1, String)
type Dtip1 t = ParseTimeP t "%F %T" Id

-- extra check to validate the time as parseTime doesnt validate the time component
type Dtop1 =
   FormatTimeP "%H %M %S" >> Resplit "\\s+" >> Map (ReadP Int)
     >> Guards '[ '(Printf2 "guard %d invalid hours %d", Between 0 23)
                , '(Printf2 "guard %d invalid minutes %d", Between 0 59)
                , '(Printf2 "guard %d invalid seconds %d", Between 0 59)
                ] >> 'True
type Dtfmt1 = FormatTimeP "%F %T"

ssn :: Proxy Ssn
ssn = mkProxy3

type Ssn = '(Ssnip, Ssnop, Ssnfmt, String)

type Ssnip = Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" >> OneP >> Snd >> Map (ReadP Int)
type Ssnop = GuardsQuick (Printf2 "number for group %d invalid: found %d")
                     '[Between 1 899 && Id /= 666, Between 1 99, Between 1 9999]
                      >> 'True
type Ssnop' = Guards '[ '(Printf2 "guard %d invalid: found %d", Between 1 899 && Id /= 666)
                  , '(Printf2 "group %d invalid: found %d", Between 1 99)
                  , '(Printf2 "group %d invalid: found %d", Between 1 9999)
                  ] >> 'True
type Ssnfmt = Printfnt 3 "%03d-%02d-%04d"

hms :: Proxy Hms
hms = mkProxy3

type Hms = '(Hmsip, Hmsop >> 'True, Hmsfmt, String)

type Hmsip = Resplit ":" >> Map (ReadP Int)
type Hmsop = Guard (Len >> Printf "expected len 3 but found %d") (Len >> Same 3)
             >> Guards '[ '(Printf2 "guard(%d) %d hours is out of range", Between 0 23)
                        , '(Printf2 "guard(%d) %d mins is out of range", Between 0 59)
                        , '(Printf2 "guard(%d) %d secs is out of range", Between 0 59)]
type Hmsfmt = Printfnt 3 "%02d:%02d:%02d"

type Ip = '(Ipip, Ipop, Ipfmt, String)
type Ip1 = '(Ipip, Ipop', Ipfmt, String)

ip :: Proxy Ip
ip = mkProxy3

ip1 :: Proxy Ip1
ip1 = mkProxy3

type Ipip = Rescan "^(\\d{1,3}).(\\d{1,3}).(\\d{1,3}).(\\d{1,3})$" >> OneP >> Snd >> Map (ReadP Int)
type Ipop = GuardsQuick (Printf2 "guard(%d) octet out of range 0-255 found %d") (RepeatT 4 (Between 0 255)) >> 'True
type Ipop' = Guards '[
          '(Printf2 "octet %d out of range 0-255 found %d", Between 0 255)
        , '(Printf2 "octet %d out of range 0-255 found %d", Between 0 255)
        , '(Printf2 "octet %d out of range 0-255 found %d", Between 0 255)
        , '(Printf2 "octet %d out of range 0-255 found %d", Between 0 255)
        ] >> 'True
type Ipfmt = Printfnt 4 "%03d.%03d.%03d.%03d"

type HmsRE = "^([0-1][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9])$" -- padded only -- dumb because strict validation should not be done twice: ie in ip and op!
type Hmsconv = Do '[Rescan HmsRE, Head, Snd, Map (ReadBaseInt 10)]
type Hmsval = GuardsQuick (Printf2 "guard(%d) %d is out of range") '[Between 0 23, Between 0 59, Between 0 59]

type Hms4 = '(Hmsconv, Hmsval >> 'True, Hmsfmt, String)

hms4 :: Proxy Hms4
hms4 = mkProxy3

type OctetRE = "(25[0-5]|2[0..4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])" -- no padded numbers allowed
--type Ip4strictRE = "^" `AppendSymbol` OctetRE `AppendSymbol` "\\." `AppendSymbol` OctetRE `AppendSymbol` "\\." `AppendSymbol` OctetRE `AppendSymbol` "\\." `AppendSymbol` OctetRE `AppendSymbol` "$"
type Ip4strictRE = "^" `AppendSymbol` IntersperseT "\\." (RepeatT 4 OctetRE) `AppendSymbol` "$"

-- valid dates for for DateFmts are "2001-01-01" "Jan 24 2009" and "03/29/07"
type DateFmts = '["%Y-%m-%d", "%m/%d/%y", "%B %d %Y"]
type DateN = '(ParseTimes Day DateFmts Id, 'True, FormatTimeP "%Y-%m-%d", String)

type DateTimeFmts = '["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"]
type DateTimeN = '(ParseTimes UTCTime DateTimeFmts Id, 'True, FormatTimeP "%Y-%m-%d %H:%M:%S", String)

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
   '(Ones >> Map (ReadP Int)
   , Guard ( TupleI '[Len, W n, ShowP] >> Printfn "incorrect number of digits found %d but expected %d in [%s]")
           (Len >> Same n)
     >> Guard ("luhn check failed") Luhn >> 'True
   , Map ShowP >> Concat
   , String)

type LuhnX (n :: Nat) =
   '(Ones >> Map (ReadP Int)
   , Luhn'' n >> 'True
   , Map ShowP >> Concat
   , String)

type Luhn'' (n :: Nat) =
         Guard ( TupleI '[Len, W n, ShowP] >> Printfn "incorrect number of digits found %d but expected %d in [%s]") (Len >> Same n)
      >> Do '[
              Reverse
             ,Ziplc [1,2] Id
             ,Map (Fst * Snd >> If (Id >= 10) (Id - 9) Id)
             ,Foldmap (SG.Sum Int)
             ]
        >> Guard (TupleI '[Id, Id `Mod` 10] >> Printfn "expected %d mod 10 = 0 but found %d") (Mod Id 10 >> Same 0)

type Luhn' (n :: Nat) =
       Msg "Luhn'" (Do
       '[Guard ( TupleI '[Len, W n, Id] >> Printfn "incorrect number of digits found %d but expected %d in [%s]") (Len >> Same n)
        ,Do
            '[Ones
            ,Map (ReadP Int)
            ,Reverse
            ,Ziplc [1,2] Id
            ,Map (Fst * Snd >> If (Id >= 10) (Id - 9) Id)
            ,Foldmap (SG.Sum Int)
           ]
        ,Guard (TupleI '[Id, Id `Mod` 10] >> Printfn "expected %d mod 10 = 0 but found %d") (Mod Id 10 >> Same 0)
        ])

-- noop true
type Ok (t :: Type) = '(Id, 'True, Id, t)
type OkR (t :: Type) = MakeR3 (Ok t)

-- noop false
type OkNot (t :: Type) = '(Id, 'False, Id, t)
type OkNotR (t :: Type) = MakeR3 (OkNot t)

-- | convert a string from the given base \'i\' but stores it internally as a string of base \'j\'
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> prtEval3P (Proxy @(BaseIJ 16 2)) ol "fe"
-- Right (Refined3 {r3In = "11111110", r3Out = "fe"})
--
-- >>> prtEval3P (Proxy @(BaseIJ 16 2)) ol "fge"
-- Left Step 1. Initial Conversion(ip) Failed | invalid base 16
--
type BaseIJ (i :: Nat) (j :: Nat) = '(ReadBase Int i >> ShowBase j, 'True, ReadBase Int j >> ShowBase i, String)

-- | take any valid Read/Show instance and turn it into a valid Refined3
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> :m + Data.Ratio
-- >>> prtEval3P (Proxy @(ReadShow Rational)) ol "13 % 3"
-- Right (Refined3 {r3In = 13 % 3, r3Out = "13 % 3"})
--
-- >>> prtEval3P (Proxy @(ReadShow Rational)) ol "13x % 3"
-- Left Step 1. Initial Conversion(ip) Failed | ReadP Ratio Integer (13x % 3) failed
--
-- >>> prtEval3P (Proxy @(ReadShow' Rational (Between (Pos 3) (Pos 5)))) ol "13 % 3"
-- Right (Refined3 {r3In = 13 % 3, r3Out = "13 % 3"})
--
-- >>> prtEval3P (Proxy @(ReadShow' Rational (Between (NegR 11 2) (Neg 3)))) ol "-13 % 3"
-- Right (Refined3 {r3In = (-13) % 3, r3Out = "(-13) % 3"})
--
-- >>> prtEval3P (Proxy @(ReadShow' Rational (Id > Pos 11))) ol "13 % 3"
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
type ReadShow (t :: Type) = '(ReadP t, 'True, ShowP, String)
type ReadShowR (t :: Type) = MakeR3 (ReadShow t)

type ReadShow' (t :: Type) p = '(ReadP t, p, ShowP, String)
type ReadShowR' (t :: Type) p = MakeR3 (ReadShow' t p)
