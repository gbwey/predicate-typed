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
     Common predicates for use with Refined, Refined2, and Refined3
-}
module Predicate.Examples.Common (
  -- ** date time checkers
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
  -- *** time checkers
  , Hmsip
  , Hmsop
  , Hmsop'
  , Hmsfmt
  , HmsRE

  -- ** credit cards
  , Ccip
  , Ccop
  , Ccfmt
  , Luhnip
  , Luhnop
  , Luhn'
  , Luhnop'
  , Luhn''
--  , Luhnfmt

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

   ) where
import Predicate.Core
import Predicate.Prelude
import Predicate.Util
import GHC.TypeLits (Nat)
import Data.Time

-- | \'ip\' type for converting a credit card number to a list of singleton digits
type Ccip = Map (ReadP Int Id) (Ones (Remove "-" Id))

-- | \'op\' type for validating a credit card number by check digit
type Ccop (n :: Nat) = Guard (PrintT "expected %d digits but found %d" '(n,Len)) (Len == n) >> Luhn Id

-- | \'fmt\' type for formatting a credit card using \'ns\' as the format
type Ccfmt (ns :: [Nat]) = ConcatMap (ShowP Id) Id >> SplitAts ns Id >> Concat (Intercalate '["-"] Id)

-- | uses builtin 'Luhn'
type Luhnip = Map (ReadP Int Id) (Ones Id)
type Luhnop (n :: Nat) = Msg "incorrect number of digits:" (Len == n) && Luhn Id

-- now that time is actually validated we dont need Dtop*
-- | \'ip\' type for reading in a date time
type Dtip t = ParseTimeP t "%F %T" Id
-- | \'fmt\' type for formatting the date time compatible ith 'Dtip'
type Dtfmt = FormatTimeP "%F %T" Id

-- | \'ip\' type for reading in a ssn
type Ssnip = Map (ReadP Int Id) (Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" Id >> Snd (OneP Id))
-- | \'op\' type for validating a ssn
type Ssnop = BoolsQuick (PrintT "number for group %d invalid: found %d" Id)
                     '[Between 1 899 Id && Id /= 666, Between 1 99 Id, Between 1 9999 Id]

{-
type Ssnop' = GuardsDetail "%s invalid: found %d"
                          '[ '("first", Between 1 899 Id && Id /= 666)
                           , '("second", Between 1 99 Id)
                           , '("third" , Between 1 9999 Id)
                           ] >> 'True
-}

-- | \'fmt\' type for formatting the ssn compatible with 'Ssnip'
type Ssnfmt = PrintL 3 "%03d-%02d-%04d" Id

-- | \'ip\' type for reading in time
type Hmsip = Map (ReadP Int Id) (Resplit ":" Id)
-- type Hmsop' = BoolsQuick "" '[ Msg "hours:"   (Between 0 23 Id), Msg "minutes:" (Between 0 59 Id), Msg "seconds:" (Between 0 59 Id)]

-- | \'op\' type for validating the time using a guard
type Hmsop = GuardsDetail "%s invalid: found %d" '[ '("hours", Between 0 23 Id),'("minutes",Between 0 59 Id),'("seconds",Between 0 59 Id)]

-- | \'op\' type for validating the time using predicate
type Hmsop' = Bools '[ '("hours", Between 0 23 Id), '("minutes",Between 0 59 Id), '("seconds",Between 0 59 Id) ]

-- | \'fmt\' type for formatting the time
type Hmsfmt = PrintL 3 "%02d:%02d:%02d" Id

-- | regular expression for a time component
type HmsRE = "^([0-1][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9])$" -- strict validation should only be done in 'op' not 'ip'

-- | regular expression for an ip4 address
type Ip4RE = "^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$"

-- | \'ip\' type for reading in an ip4 address
type Ip4ip = Map (ReadP Int Id) (Resplit "\\." Id)

-- | \'ip\' type for reading in an ip4 address using a regular expression
type Ip4ip' = Map (ReadP Int Id) (Rescan Ip4RE Id >> Snd (OneP Id))
-- RepeatT is a type family so it expands everything! replace RepeatT with a type class

-- | \'op\' type for validating an ip4 address using a predicate
type Ip4op' = BoolsN (PrintT "octet %d out of range 0-255 found %d" Id) 4 (Between 0 255 Id)
-- | \'op\' type for validating an ip4 address using a guard
type Ip4op = GuardsN (PrintT "octet %d out of range 0-255 found %d" Id) 4 (Between 0 255 Id)

-- | \'fmt\' type for formatting an ip4 address
type Ip4fmt = PrintL 4 "%03d.%03d.%03d.%03d" Id

-- | regular expression for an octet
type OctetRE = "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])" -- no padded numbers allowed
--type Ip4StrictRE = "^" <%> OctetRE <%> "\\." <%> OctetRE <%> "\\." <%> OctetRE <%> "\\." <%> OctetRE <%> "$"
-- | regular expression for an ip4 address
type Ip4StrictRE = "^" <%> IntersperseT "\\." (RepeatT 4 OctetRE) <%> "$"


-- | \'ip\' type for reading in an ip6 address
type Ip6ip = Resplit ":" Id
         >> Map (If (Id == "") "0" Id) Id
         >> Map (ReadBase Int 16 Id) Id
         >> PadL 8 0 Id

--type Ip6ip' = Map (If (Id == "") 0 (ReadBase Int 16 Id)) (Resplit ":" Id) >> PadL 8 0 Id

-- | \'op\' type for validating an ip6 address using predicates
type Ip6op = Msg "count is bad:" (Len == 8)
          && Msg "out of bounds:" (All (Between 0 65535 Id) Id)

-- | \'fmt\' type for formatting an ip6 address
type Ip6fmt = PrintL 8 "%04x:%04x:%04x:%04x:%04x:%04x:%04x:%04x" Id

-- valid dates for for DateFmts are "2001-01-01" "Jan 24 2009" and "03/29/07"
type DateFmts = '["%Y-%m-%d", "%m/%d/%y", "%B %d %Y"]

-- | \'ip\' type for reading one of many date formats from 'DateFmts'
type DateNip = ParseTimes Day DateFmts Id

type DateTimeFmts = '["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"]

-- | \'ip\' type for reading one of many date time formats from 'DateTimeFmts'
type DateTimeNip = ParseTimes UTCTime DateTimeFmts Id

-- ParseTimeP is easier and accurate
type DdmmyyyyRE = "^(\\d{2})-(\\d{2})-(\\d{4})$"
{-
type Ddmmyyyyop =
    Guards '[ '(PrintT "guard(%d) day %d is out of range" Id, Between 1 31 Id)
            , '(PrintT "guard(%d) month %d is out of range" Id, Between 1 12 Id)
            , '(PrintT "guard(%d) year %d is out of range" Id, Between 1990 2050 Id) ]
-}
--type Ddmmyyyyop' = GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]
type Ddmmyyyyop = GuardsDetail "%s %d is out of range" '[ '("day", Between 1 31 Id), '("month", Between 1 12 Id), '("year", Between 1990 2050 Id) ]
type Ddmmyyyyop' = Bools '[ '("day", Between 1 31 Id), '("month", Between 1 12 Id), '("year", Between 1990 2050 Id) ]
--type Ddmmyyyyop'''' = BoolsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]

type Luhnop' (n :: Nat) =
         Guard (PrintT "incorrect number of digits found %d but expected %d in [%s]" '(Len, n, ShowP Id)) (Len == n)
      >> Do '[
              Reverse
             ,Zip (Cycle n [1,2]) Id
             ,Map (Fst Id * Snd Id >> If (Id >= 10) (Id - 9) Id) Id
             ,Sum
             ]
        >> Guard (PrintT "expected %d mod 10 = 0 but found %d" '(Id, Id `Mod` 10)) (Mod Id 10 == 0)

type Luhn'' (n :: Nat) = Luhnip >> Luhnop' n

type Luhn' (n :: Nat) =
       Msg "Luhn'" (Do
       '[Guard (PrintT "incorrect number of digits found %d but expected %d in [%s]" '(Len, n, Id)) (Len == n)
        ,Do
            '[Ones Id
            ,Map (ReadP Int Id) Id
            ,Reverse
            ,Zip (Cycle n [1,2]) Id
            ,Map (Fst Id * Snd Id >> If (Id >= 10) (Id - 9) Id) Id
            ,Sum
           ]
        ,Guard (PrintT "expected %d mod 10 = 0 but found %d" '(Id, Id `Mod` 10)) (Mod Id 10 == 0)
        ])

-- convert json microsoft datetime to zonedtime
--type JsonMicrosoftDateTime = Rescan "^Date\\((\\d+[+-]\\d{4})\\)" Id >> Head Id >> Snd Id >> Id !! 0 >> ReplaceOneString 'RPrepend "\\d{3}[+-]" "." Id >> ParseTimeP ZonedTime "%s%Q%z" Id

type JsonMicrosoftDateTime =
  Do '[ Rescan "^Date\\((\\d+[+-]\\d{4})\\)" Id
      , Head Id
      , Snd Id
      , Id !! 0
      , ReplaceOneString 'RPrepend "\\d{3}[+-]" "." Id
      , ParseTimeP ZonedTime "%s%Q%z" Id
      ]