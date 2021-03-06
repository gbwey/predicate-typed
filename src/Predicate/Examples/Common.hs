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
-- | Common predicates for use with Refined, Refined2, and Refined3
module Predicate.Examples.Common (
  -- ** datetime
    DateNip
  , DateFmts
  , DateTimeFmts
  , DateTimeNip
  , Dtip
  , Dtfmt
  , DdmmyyyyRE
  , Ddmmyyyyop
  , Ddmmyyyyop'
  , JsonMicrosoftDateTime

  -- ** time
  , Hmsip
  , Hmsop
  , Hmsop'
  , Hmsfmt
  , HmsRE

  -- ** luhn check
  , Luhnip
  , Luhnop
  , Luhnfmt
  , Luhn'
  , Luhnop'

  -- ** ssn
  , Ssnip
  , Ssnop
  , Ssnfmt

  -- ** ipv4
  , Ip4ip
  , Ip4ip'
  , Ip4op
  , Ip4op'
  , Ip4fmt
  , OctetRE
  , Ip4RE
  , Ip4StrictRE

  -- ** ipv6
  , Ip6ip
  , Ip6op
  , Ip6fmt

  -- ** isbn10
  , Isbn10ip
  , Isbn10op
  , Isbn10fmt

  -- ** isbn13
  , Isbn13ip
  , Isbn13op
  , Isbn13fmt
   ) where
import Predicate
import GHC.TypeLits (Nat)
import Data.Time (Day, UTCTime, ZonedTime)

-- | @ip@ type for converting a credit card number to a list of singleton digits
type Luhnip = Map' (ReadP Int Id) (Remove "-" Id >> Ones)

-- | @op@ type for validating a credit card number by check digit
type Luhnop (n :: Nat) = GuardBool (PrintT "expected %d digits but found %d" '(n,Len)) (Len == n) && GuardBool "invalid checkdigit" IsLuhn

-- | @fmt@ type for formatting a credit card using @ns@ as the format
type Luhnfmt (ns :: [Nat]) = ConcatMap (ShowP Id) Id >> SplitAts ns Id >> Intercalate '["-"] Id >> Concat

-- now that time is actually validated we dont need Dtop*
-- | @ip@ type for reading in a date time
type Dtip t = ParseTimeP t "%F %T"
-- | @fmt@ type for formatting the date time compatible ith 'Dtip'
type Dtfmt = FormatTimeP "%F %T"

-- | @ip@ type for reading in a ssn
type Ssnip = Map' (ReadP Int Id) (Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" >> OneP >> Snd)
-- | @op@ type for validating a ssn
type Ssnop = BoolsQuick (PrintT "number for group %d invalid: found %d" Id)
                     '[Between 1 899 Id && Id /= 666, Between 1 99 Id, Between 1 9999 Id]

{-
type Ssnop' = GuardsDetail "%s invalid: found %d"
                          '[ '("first", Between 1 899 Id && Id /= 666)
                           , '("second", Between 1 99 Id)
                           , '("third" , Between 1 9999 Id)
                           ] >> 'True
-}

-- | @fmt@ type for formatting the ssn compatible with 'Ssnip'
type Ssnfmt = PrintL 3 "%03d-%02d-%04d" Id

-- | @ip@ type for reading in time
type Hmsip = Map' (ReadP Int Id) (Resplit ":")
-- type Hmsop' = BoolsQuick "" '[Msg "hours:"   (Between 0 23 Id), Msg "minutes:" (Between 0 59 Id), Msg "seconds:" (Between 0 59 Id)]

-- | @op@ type for validating the time using a guard
type Hmsop = GuardsDetail "%s invalid: found %d" '[ '("hours", Between 0 23 Id),'("minutes",Between 0 59 Id),'("seconds",Between 0 59 Id)] >> 'True

-- | @op@ type for validating the time using predicate
type Hmsop' = Bools '[ '("hours", Between 0 23 Id), '("minutes",Between 0 59 Id), '("seconds",Between 0 59 Id) ]

-- | @fmt@ type for formatting the time
type Hmsfmt = PrintL 3 "%02d:%02d:%02d" Id

-- | regular expression for a time component
type HmsRE = "^([0-1][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9])$" -- strict validation should only be done in 'op' not 'ip'

-- | regular expression for an ip4 address
type Ip4RE = "^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$"

-- | @ip@ type for reading in an ip4 address
type Ip4ip = Map' (ReadP Int Id) (Resplit "\\.")

-- | @ip@ type for reading in an ip4 address using a regular expression
type Ip4ip' = Map' (ReadP Int Id) (Rescan Ip4RE >> OneP >> Snd)
-- RepeatT is a type family so it expands everything! replace RepeatT with a type class

-- | @op@ type for validating an ip4 address using a predicate
type Ip4op' = BoolsN (PrintT "octet %d out of range 0-255 found %d" Id) 4 (0 <..> 0xff)
-- | @op@ type for validating an ip4 address using a guard
type Ip4op = GuardsN 4 (PrintT "octet %d out of range 0-255 found %d" Id) (0 <..> 0xff) >> 'True

-- | @fmt@ type for formatting an ip4 address
type Ip4fmt = PrintL 4 "%03d.%03d.%03d.%03d" Id

-- | regular expression for an octet
type OctetRE = "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])" -- no padded numbers allowed
--type Ip4StrictRE = "^" <%> OctetRE <%> "\\." <%> OctetRE <%> "\\." <%> OctetRE <%> "\\." <%> OctetRE <%> "$"
-- | regular expression for an ip4 address
type Ip4StrictRE = "^" <%> IntersperseT "\\." (RepeatT 4 OctetRE) <%> "$"


-- | @ip@ type for reading in an ip6 address
type Ip6ip = Resplit ":"
         >> Map (If (Id == "") "0" Id)
         >> Map (ReadBase Int 16)
         >> PadL 8 0 Id

--type Ip6ip' = Map' (If (Id == "") 0 (ReadBase Int 16)) (Resplit ":") >> PadL 8 0 Id

-- | @op@ type for validating an ip6 address using predicates
type Ip6op = Msg "count is bad:" (Len == 8)
          && Msg "out of bounds:" (All (0 <..> 0xffff))

-- | @fmt@ type for formatting an ip6 address
type Ip6fmt = PrintL 8 "%04x:%04x:%04x:%04x:%04x:%04x:%04x:%04x" Id

-- | isbn 10 converter
type Isbn10ip = Resplit "-"
             >> Concat
             >> 'Just Unsnoc
             >> Map (ReadP Int (Singleton Id)) *** If (Singleton Id ==~ "X") 10 (ReadP Int (Singleton Id))

-- | isbn 10 validator
type Isbn10op = GuardSimple ((Fst >> All (0 <..> 9)) && Between 0 10 Snd)
             >> ZipWith (Fst * Snd) (1...10 >> Reverse) (Fst +: Snd)
             >> Sum
             >> GuardBool "mod 0 oops" (Id `Mod` 11 == 0)

-- | isbn 10 formatter
type Isbn10fmt = ConcatMap (ShowP Id) Id *** If (Id == 10) "X" (ShowP Id)
                 >> Fst <> "-" <> Snd  -- no standard format: just hyphen before checkdigit


-- | isbn 13 converter
type Isbn13ip = Resplit "-"
             >> Concat
             >> Map (ReadP Int (Singleton Id))

-- | isbn 13 validator
type Isbn13op = ZipWith (Fst * Snd) (Cycle 13 [1,3] >> Reverse) Id
             >> Sum
             >> '(Id,Id `Mod` 10)
             >> GuardBool (PrintT "sum=%d mod 10=%d" Id) (Snd == 0)

-- | isbn 13 formatter
type Isbn13fmt = 'Just Unsnoc >> ConcatMap (ShowP Id) Fst <> "-" <> ShowP Snd

-- valid dates for for DateFmts are "2001-01-01" "Jan 24 2009" and "03/29/07"
-- | selected date formats used in the examples
type DateFmts = '["%Y-%m-%d", "%m/%d/%y", "%B %d %Y"]

-- | @ip@ type for reading one of many date formats from 'DateFmts'
type DateNip = ParseTimes Day DateFmts Id

-- | selected datetime formats used in the examples
type DateTimeFmts = '["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"]

-- | @ip@ type for reading one of many date time formats from 'DateTimeFmts'
type DateTimeNip = ParseTimes UTCTime DateTimeFmts Id

-- ParseTimeP is easier and accurate
-- | simple date regular expression
type DdmmyyyyRE = "^(\\d{2})-(\\d{2})-(\\d{4})$"
{-
type Ddmmyyyyop =
    Guards '[ '(PrintT "guard(%d) day %d is out of range" Id, Between 1 31 Id)
            , '(PrintT "guard(%d) month %d is out of range" Id, Between 1 12 Id)
            , '(PrintT "guard(%d) year %d is out of range" Id, Between 1990 2050 Id) ]
          >> 'True
-}
--type Ddmmyyyyop' = GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]
-- | date validator for a three value list containing day month and year using GuardsDetail
type Ddmmyyyyop = GuardsDetail "%s %d is out of range" '[ '("day", Between 1 31 Id), '("month", Between 1 12 Id), '("year", Between 1990 2050 Id) ] >> 'True
-- | date validator for a three value list containing day month and year using Bools
type Ddmmyyyyop' = Bools '[ '("day", Between 1 31 Id), '("month", Between 1 12 Id), '("year", Between 1990 2050 Id) ]
--type Ddmmyyyyop'''' = BoolsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]

-- | luhn check digit validator
type Luhnop' (n :: Nat) =
         Guard (PrintT "incorrect number of digits found %d but expected %d in [%s]" '(Len, n, ShowP Id)) (Len == n)
      >> Do '[
              Reverse
             ,ZipWith (Fst * Snd >> If (Id >= 10) (Id - 9) Id) (Cycle n [1,2]) Id
             ,Sum
             ]
        >> Guard (PrintT "expected %d mod 10 = 0 but found %d" '(Id, Id `Mod` 10)) (Mod Id 10 == 0)

-- | luhn check digit validator (alternate version)
type Luhn' (n :: Nat) =
       Msg "Luhn'" (Do
       '[Guard (PrintT "incorrect length: found %d but expected %d in [%s]" '(Len, n, Id)) (Len == n)
        ,Do
            '[Ones
            ,Map (ReadP Int Id)
            ,Reverse
            ,ZipWith (Fst * Snd >> If (Id >= 10) (Id - 9) Id) (Cycle n [1,2]) Id
            ,Sum
           ]
        ,Guard (PrintT "expected %d mod 10 = 0 but found %d" '(Id, Id `Mod` 10)) (Mod Id 10 == 0)
        ])

-- | convert json microsoft datetime to zonedtime
--
-- >>> pz @JsonMicrosoftDateTime "Date(1593460089052+0800)"
-- Val 2020-06-30 03:48:09.052 +0800
--
-- >>> pz @JsonMicrosoftDateTime "Date(0+0800)"
-- Val 1970-01-01 08:00:00 +0800
--
-- >>> pz @JsonMicrosoftDateTime "Date(12+0800)"
-- Val 1970-01-01 08:00:00.012 +0800
--
-- >>> pz @JsonMicrosoftDateTime "Date(123+0800)"
-- Val 1970-01-01 08:00:00.123 +0800
--
-- >>> pz @JsonMicrosoftDateTime "Date(+1234+0800)"
-- Val 1970-01-01 08:00:01.234 +0800
--
-- >>> pz @JsonMicrosoftDateTime "Date(-123456+0000)"
-- Val 1969-12-31 23:57:57.456 +0000
--
type JsonMicrosoftDateTime =
  Do '[ Rescan "^Date\\(([-+])?(\\d*?)(\\d{0,3})([+-]\\d{4})\\)"
      , Head
      , Snd
      , If (Id !! 0 == "-") "-" ""
        <> PadL 1 (C "0") (Id !! 1)
        <> "."
        <> PadL 3 (C "0") (Id !! 2)
        <> Id !! 3
      , ParseTimeP ZonedTime "%s%Q%z"
      ]