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
  , Ddmmyyyyval
  , Ddmmyyyyval'

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
  , Ip4op''
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
import GHC.TypeLits (AppendSymbol, Nat)
import Data.Time
import qualified Data.Semigroup as SG

type Ccip = Map (ReadP Int Id) (Ones (Remove "-" Id))
type Ccop (n :: Nat) = Guard (PrintT "expected %d digits but found %d" '(n,Len)) (Len == n) >> Luhn Id
type Ccfmt (ns :: [Nat]) = ConcatMap (ShowP Id) Id >> SplitAts ns Id >> Concat (Intercalate '["-"] Id)

-- | uses builtin 'Luhn'
type Luhnip = Map (ReadP Int Id) (Ones Id)
type Luhnop (n :: Nat) = Msg "incorrect number of digits:" (Len == n) && Luhn Id

-- now that time is actually validated we dont need Dtop*
type Dtip t = ParseTimeP t "%F %T" Id
type Dtfmt = FormatTimeP "%F %T" Id

type Ssnip = Map (ReadP Int Id) (Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" Id >> Snd (OneP Id))
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

type Hmsip = Map (ReadP Int Id) (Resplit ":" Id)
-- type Hmsop' = BoolsQuick "" '[ Msg "hours:"   (Between 0 23), Msg "minutes:" (Between 0 59), Msg "seconds:" (Between 0 59)]
type Hmsop = Bools '[ '("hours", Between 0 23), '("minutes",Between 0 59), '("seconds",Between 0 59) ]
type Hmsop' = GuardsDetail "%s invalid: found %d" '[ '("hours", Between 0 23),'("minutes",Between 0 59),'("seconds",Between 0 59)]

type Hmsfmt = PrintL 3 "%02d:%02d:%02d" Id

type HmsRE = "^([0-1][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9])$" -- strict validation should only be done in 'op' not 'ip'

type Ip4RE = "^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$"

type Ip4ip' = Map (ReadP Int Id) (Resplit "\\." Id)

type Ip4ip = Map (ReadP Int Id) (Rescan Ip4RE Id >> Snd (OneP Id))
-- RepeatT is a type family so it expands everything! replace RepeatT with a type class
type Ip4op = BoolsN (PrintT "guard(%d) octet out of range 0-255 found %d" Id) 4 (Between 0 255)
type Ip4op'' = GuardsN (PrintT "guard(%d) octet out of range 0-255 found %d" Id) 4 (Between 0 255)

type Ip4fmt = PrintL 4 "%03d.%03d.%03d.%03d" Id

type OctetRE = "(25[0-5]|2[0..4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])" -- no padded numbers allowed
--type Ip4StrictRE = "^" `AppendSymbol` OctetRE `AppendSymbol` "\\." `AppendSymbol` OctetRE `AppendSymbol` "\\." `AppendSymbol` OctetRE `AppendSymbol` "\\." `AppendSymbol` OctetRE `AppendSymbol` "$"
type Ip4StrictRE = "^" `AppendSymbol` IntersperseT "\\." (RepeatT 4 OctetRE) `AppendSymbol` "$"

type Ip4op' = Guard "expected 4 numbers" (Len == 4)
         >> Guard "each number must be between 0 and 255" (All (Between 0 255) Id)
         >> 'True

type Ip6ip = Resplit ":" Id
         >> Map (If (Id == "") "0" Id) Id
         >> Map (ReadBaseInt 16 Id) Id
         >> PadL 8 0 Id

--type Ip6ip' = Map (If (Id == "") 0 (ReadBaseInt 16 Id)) (Resplit ":" Id) >> PadL 8 0 Id

type Ip6op = Msg "count is bad:" (Len == 8)
          && Msg "out of bounds:" (All (Between 0 65535) Id)

type Ip6fmt = PrintL 8 "%04x:%04x:%04x:%04x:%04x:%04x:%04x:%04x" Id

-- valid dates for for DateFmts are "2001-01-01" "Jan 24 2009" and "03/29/07"
type DateFmts = '["%Y-%m-%d", "%m/%d/%y", "%B %d %Y"]

type DateNip = ParseTimes Day DateFmts Id

type DateTimeFmts = '["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"]

type DateTimeNip = ParseTimes UTCTime DateTimeFmts Id

-- this works but ParseTimeP is easier
type DdmmyyyyRE = "^(\\d{2})-(\\d{2})-(\\d{4})$"
type Ddmmyyyyval' = GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31, Between 1 12, Between 1990 2050]
type Ddmmyyyyval =
    Guards '[ '(PrintT "guard(%d) day %d is out of range" Id, Between 1 31)
            , '(PrintT "guard(%d) month %d is out of range" Id, Between 1 12)
            , '(PrintT "guard(%d) year %d is out of range" Id, Between 1990 2050) ]

type Luhn'' (n :: Nat) =
         Guard (PrintT "incorrect number of digits found %d but expected %d in [%s]" '(Len, n, ShowP Id)) (Len == n)
      >> Do '[
              Reverse
             ,Zip (Cycle n [1,2]) Id
             ,Map (Fst Id * Snd Id >> If (Id >= 10) (Id - 9) Id) Id
             ,FoldMap (SG.Sum Int) Id
             ]
        >> Guard (PrintT "expected %d mod 10 = 0 but found %d" '(Id, Id `Mod` 10)) (Mod Id 10 == 0)

type Luhn' (n :: Nat) =
       Msg "Luhn'" (Do
       '[Guard (PrintT "incorrect number of digits found %d but expected %d in [%s]" '(Len, n, Id)) (Len == n)
        ,Do
            '[Ones Id
            ,Map (ReadP Int Id) Id
            ,Reverse
            ,Zip (Cycle n [1,2]) Id
            ,Map (Fst Id * Snd Id >> If (Id >= 10) (Id - 9) Id) Id
            ,FoldMap (SG.Sum Int) Id
           ]
        ,Guard (PrintT "expected %d mod 10 = 0 but found %d" '(Id, Id `Mod` 10)) (Mod Id 10 == 0)
        ])
