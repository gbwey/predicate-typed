{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE EmptyDataDeriving #-}
-- | promoted read, show, and printf functions
module Predicate.Data.ReadShow (

    ShowP
  , ReadP
  , ReadP'
  , ReadMaybe
  , ReadMaybe'

  -- ** print expressions
  , PrintF
  , PrintC
  , PrintL
  , PrintT
  , PrintI

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Predicate.Data.Tuple (ToITuple, ToITupleList, ReverseITuple)
import GHC.TypeLits (Nat)
import Data.Proxy (Proxy(Proxy))
import Data.Kind (Type)
import Text.Printf (PrintfArg, printf, PrintfType)
import Data.Typeable (Typeable)
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate.Prelude
-- >>> import Data.Time

-- | similar to 'show'
--
-- >>> pz @(ShowP Id) [4,8,3,9]
-- Val "[4,8,3,9]"
--
-- >>> pz @(ShowP Id) 'x'
-- Val "'x'"
--
-- >>> pz @(ShowP (42 -% 10)) 'x'
-- Val "(-21) % 5"
--
data ShowP p deriving Show

instance ( Show (PP p x)
         , P p x
         ) => P (ShowP p) x where
  type PP (ShowP p) x = String
  eval _ opts x = do
    let msg0 = "ShowP"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = show p
        in mkNode opts (Val d) (msg0 <> " " <> litL opts d <> showVerbose opts " | " p) [hh pp]

-- | uses the 'Read' of the given type @t@ and @p@ which points to the content to read
data ReadP' t p deriving Show

instance ( P p x
         , PP p x ~ String
         , Typeable (PP t x)
         , Show (PP t x)
         , Read (PP t x)
         ) => P (ReadP' t p) x where
  type PP (ReadP' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "ReadP " <> t
        t = showT @(PP t x)
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right s ->
        let hhs = [hh pp]
        in case reads @(PP t x) s of
           [(b,"")] -> mkNode opts (Val b) (msg0 <> " " ++ showL opts b) hhs
           o -> mkNode opts (Fail (msg0 <> " (" ++ s ++ ")")) (showVerbose opts "" o) hhs

-- | uses the 'Read' of the given type @t@ and @p@ which points to the content to read
--
-- >>> pz @(ReadP Rational Id) "4 % 5"
-- Val (4 % 5)
--
-- >>> pz @(Between (ReadP Day "2017-04-11") (ReadP Day "2018-12-30") (ReadP Day Id)) "2018-10-12"
-- Val True
--
-- >>> pz @(Between (ReadP Day "2017-04-11") (ReadP Day "2018-12-30") (ReadP Day Id)) "2016-10-12"
-- Val False
--
-- >>> pl @(ReadP Rational Id) "123 % 4"
-- Present 123 % 4 (ReadP Ratio Integer 123 % 4)
-- Val (123 % 4)
--
-- >>> pl @(ReadP Rational Id) "x123 % 4"
-- Error ReadP Ratio Integer (x123 % 4) ([])
-- Fail "ReadP Ratio Integer (x123 % 4)"
--
-- >>> pl @(ReadP Day Id) "1999-11-30"
-- Present 1999-11-30 (ReadP Day 1999-11-30)
-- Val 1999-11-30
--
-- >>> pl @(ReadP Day Id) "1999-02-29"
-- Error ReadP Day (1999-02-29) ([])
-- Fail "ReadP Day (1999-02-29)"
--
-- >>> pl @(ReadP TimeOfDay Id) "14:59:20"
-- Present 14:59:20 (ReadP TimeOfDay 14:59:20)
-- Val 14:59:20
--
data ReadP (t :: Type) p deriving Show
type ReadPT (t :: Type) p = ReadP' (Hole t) p

instance P (ReadPT t p) x => P (ReadP t p) x where
  type PP (ReadP t p) x = PP (ReadPT t p) x
  eval _ = eval (Proxy @(ReadPT t p))

data ReadMaybe' t p deriving Show

instance ( P p x
         , PP p x ~ String
         , Typeable (PP t x)
         , Show (PP t x)
         , Read (PP t x)
         ) => P (ReadMaybe' t p) x where
  type PP (ReadMaybe' t p) x = Maybe (PP t x, String)
  eval _ opts x = do
    let msg0 = "ReadMaybe " <> t
        t = showT @(PP t x)
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right s ->
        let msg1 = msg0 <> " (" <> s <> ")"
            hhs = [hh pp]
        in case reads @(PP t x) s of
           [(b,rest)] -> mkNode opts (Val (Just (b,rest))) (lit3 opts msg1 b "" s) hhs
           o -> mkNode opts (Val Nothing) (msg1 <> " failed" <> showVerbose opts " " o) hhs

-- | Read but returns the Maybe of the value and any remaining unparsed string
--
-- >>> pz @(ReadMaybe Int Id) "123x"
-- Val (Just (123,"x"))
--
-- >>> pz @(ReadMaybe Int Id) "123"
-- Val (Just (123,""))
--
-- >>> pz @(ReadMaybe Int Id) "x123"
-- Val Nothing
--
data ReadMaybe (t :: Type) p deriving Show
type ReadMaybeT (t :: Type) p = ReadMaybe' (Hole t) p

instance P (ReadMaybeT t p) x => P (ReadMaybe t p) x where
  type PP (ReadMaybe t p) x = PP (ReadMaybeT t p) x
  eval _ = eval (Proxy @(ReadMaybeT t p))

-- | uses PrintF (unsafe) to format output for a single value
--
-- >>> pz @(PrintF "value=%03d" Id) 12
-- Val "value=012"
--
-- >>> pz @(PrintF "%s" Fst) ("abc",'x')
-- Val "abc"
--
-- >>> pz @(PrintF "%d" Fst) ("abc",'x')
-- Fail "PrintF (IO e=printf: bad formatting char 'd')"
--
-- >>> pl @(PrintF "someval %d" Id) "!23"
-- Error PrintF (IO e=printf: bad formatting char 'd') ("!23" s=someval %d)
-- Fail "PrintF (IO e=printf: bad formatting char 'd')"
--
-- >>> pl @(PrintF "%-6s" Id) 1234
-- Error PrintF (IO e=printf: bad formatting char 's') (1234 s=%-6s)
-- Fail "PrintF (IO e=printf: bad formatting char 's')"
--
-- >>> pl @(PrintF "%06x" Id) 1234
-- Present "0004d2" (PrintF [0004d2] | p=1234 | s=%06x)
-- Val "0004d2"
--
-- >>> pl @(Msg (PrintF "digits=%d" Len) Head) [1..4]
-- Present 1 (digits=4 Head 1 | [1,2,3,4])
-- Val 1
--
-- >>> pl @(PrintF "ask%%dfas%%kef%05d hey %%" Id) 35
-- Present "ask%dfas%kef00035 hey %" (PrintF [ask%dfas%kef00035 hey %] | p=35 | s=ask%%dfas%%kef%05d hey %%)
-- Val "ask%dfas%kef00035 hey %"
--
-- >>> pl @(Fail () (PrintF "someval int=%d" Id)) 45
-- Error someval int=45
-- Fail "someval int=45"
--
data PrintF s p deriving Show

instance ( PrintfArg (PP p x)
         , Show (PP p x)
         , PP s x ~ String
         , P s x
         , P p x
         ) => P (PrintF s p) x where
  type PP (PrintF s p) x = String
  eval _ opts x = do
    let msg0 = "PrintF"
    lrx <- runPQ NoInline msg0 (Proxy @s) (Proxy @p) opts x []
    case lrx of
      Left e -> pure e
      Right (s,p,ss,pp) -> do
        lr <- catchitNF (printf s p)
        pure $ case lr of
          Left e -> mkNode opts (Fail (msg0 <> " (" <> e <> ")")) (showL opts p <> " s=" <> s) [hh ss, hh pp]
          Right ret -> mkNode opts (Val ret) (msg0 <> " [" <> litL opts ret <> "]" <> showVerbose opts " | p=" p <> litVerbose opts " | s=" s) [hh ss, hh pp]


-- | uses inductive tuples to replace variable arguments
--
class PrintC x where
  prtC :: (PrintfArg a, PrintfType r) => String -> (a,x) -> r
instance PrintC () where
  prtC s (a,()) = printf s a
instance ( PrintfArg a
         , PrintC rs
         ) => PrintC (a,rs) where
  prtC s (a,rs) = prtC s rs a

-- | print for flat n-tuples of size two or larger
--
-- >>> pl @(PrintT "%d %s %s %s" '(Fst, Snd, Snd,Snd)) (10,"Asdf")
-- Present "10 Asdf Asdf Asdf" ((>>) "10 Asdf Asdf Asdf" | {PrintI [10 Asdf Asdf Asdf] | s=%d %s %s %s})
-- Val "10 Asdf Asdf Asdf"
--
-- >>> pl @(PrintT "%c %d %s" Id) ('x', 10,"Asdf")
-- Present "x 10 Asdf" ((>>) "x 10 Asdf" | {PrintI [x 10 Asdf] | s=%c %d %s})
-- Val "x 10 Asdf"
--
-- >>> pz @(PrintT "fst=%s snd=%03d" Id) ("ab",123)
-- Val "fst=ab snd=123"
--
-- >>> pz @(PrintT "fst=%s snd=%03d thd=%s" Id) ("ab",123,"xx")
-- Val "fst=ab snd=123 thd=xx"
--
-- >>> pl @(PrintT "%s %d %c %s" '(W "xyz", Fst, Snd, Thd)) (123,'x',"ab")
-- Present "xyz 123 x ab" ((>>) "xyz 123 x ab" | {PrintI [xyz 123 x ab] | s=%s %d %c %s})
-- Val "xyz 123 x ab"
--
-- >>> pl @(PrintT "%d %c %s" Id) (123,'x')
-- Error PrintI(IO e=printf: argument list ended prematurely) (PrintI %d %c %s | ('x',(123,())))
-- Fail "PrintI(IO e=printf: argument list ended prematurely)"
--
-- >>> pl @(PrintT "%d %c %s" Id) (123,'x',"abc",11)
-- Error PrintI(IO e=printf: formatting string ended prematurely) (PrintI %d %c %s | (11,("abc",('x',(123,())))))
-- Fail "PrintI(IO e=printf: formatting string ended prematurely)"
--
-- >>> pl @(PrintT "lhs = %d rhs = %s" Id) (123,"asdf")
-- Present "lhs = 123 rhs = asdf" ((>>) "lhs = 123 rhs = asdf" | {PrintI [lhs = 123 rhs = asdf] | s=lhs = %d rhs = %s})
-- Val "lhs = 123 rhs = asdf"
--
-- >>> pl @(PrintT "d=%03d s=%s" Id) (9,"ab")
-- Present "d=009 s=ab" ((>>) "d=009 s=ab" | {PrintI [d=009 s=ab] | s=d=%03d s=%s})
-- Val "d=009 s=ab"
--
-- >>> pl @(PrintT "d=%03d s=%s c=%c f=%4.2f" Id) (9,"ab",'x',1.54)
-- Present "d=009 s=ab c=x f=1.54" ((>>) "d=009 s=ab c=x f=1.54" | {PrintI [d=009 s=ab c=x f=1.54] | s=d=%03d s=%s c=%c f=%4.2f})
-- Val "d=009 s=ab c=x f=1.54"
--
-- >>> pl @(PrintT "d=%03d s=%s" Id) (9, "ab",'x',1.54)
-- Error PrintI(IO e=printf: formatting string ended prematurely) (PrintI d=%03d s=%s | (1.54,('x',("ab",(9,())))))
-- Fail "PrintI(IO e=printf: formatting string ended prematurely)"
--
-- >>> pl @(PrintT "lhs = %d rhs = %s c=%d" Id) (123,"asdf",'x')
-- Present "lhs = 123 rhs = asdf c=120" ((>>) "lhs = 123 rhs = asdf c=120" | {PrintI [lhs = 123 rhs = asdf c=120] | s=lhs = %d rhs = %s c=%d})
-- Val "lhs = 123 rhs = asdf c=120"
--
-- >>> pl @(PrintT "hello d=%d %c %s" '(12, Char1 "z", "someval")) ()
-- Present "hello d=12 z someval" ((>>) "hello d=12 z someval" | {PrintI [hello d=12 z someval] | s=hello d=%d %c %s})
-- Val "hello d=12 z someval"
--
-- >>> pl @(PrintT "ipaddress %03d.%03d.%03d.%03d" '(1,2,3,4)) ()
-- Present "ipaddress 001.002.003.004" ((>>) "ipaddress 001.002.003.004" | {PrintI [ipaddress 001.002.003.004] | s=ipaddress %03d.%03d.%03d.%03d})
-- Val "ipaddress 001.002.003.004"
--
data PrintT s p deriving Show
type PrintTT s p = p >> ToITuple >> ReverseITuple >> PrintI s

instance P (PrintTT s p) x => P (PrintT s p) x where
  type PP (PrintT s p) x = PP (PrintTT s p) x
  eval _ = eval (Proxy @(PrintTT s p))

-- | prints inductive tuples in reverse order
--
-- >>> pz @(PrintI "d=%d s=%s f=%f") (1.73,("abc",(12,())))
-- Val "d=12 s=abc f=1.73"
--
-- >>> pz @(PrintI "d=%d s=%s f=%f") ("abc",(12,()))
-- Fail "PrintI(IO e=printf: argument list ended prematurely)"
--
-- >>> pz @(PrintI "d=%s s=%d") ("abc",('x',()))
-- Fail "PrintI(IO e=printf: bad formatting char 's')"
--
data PrintI s deriving Show
instance ( PrintC bs
         , (b,bs) ~ x
         , PrintfArg b
         , PP s x ~ String
         , P s x
         ) => P (PrintI s) x where
  type PP (PrintI s) x = String
  eval _ opts x = do
    let msg0 = "PrintI"
    ss <- eval (Proxy @s) opts x
    case getValueLR NoInline opts msg0 ss [] of
      Left e -> pure e
      Right s -> do
        let hhs = [hh ss]
        lr <- catchitNF (prtC @bs s x)
        pure $ case lr of
          Left e -> mkNode opts (Fail (msg0 <> "(" <> e <> ")")) (msg0 <> " " <> s) hhs
          Right ret -> mkNode opts (Val ret) (msg0 <> " [" <> litL opts ret <> "] | s=" <> litL opts s) hhs

-- | print for lists  -- use 'PrintT' as it is safer than 'PrintL'
--
-- >>> pl @(PrintL 4 "%s %s %s %s" '[W "xyz", ShowP Fst, ShowP Snd, Thd]) (123,'x',"ab")
-- Present "xyz 123 'x' ab" ((>>) "xyz 123 'x' ab" | {PrintI [xyz 123 'x' ab] | s=%s %s %s %s})
-- Val "xyz 123 'x' ab"
--
-- >>> pz @(PrintL 1 "%05d" '[Id]) 123  -- tick is required for a one element lis)
-- Val "00123"
--
-- >>> pz @(PrintL 2 "%d %05d" [Fst,Snd]) (29,123)
-- Val "29 00123"
--
-- >>> pl @(PrintL 3 "first=%d second=%d third=%d" Id) [10,11,12]
-- Present "first=10 second=11 third=12" ((>>) "first=10 second=11 third=12" | {PrintI [first=10 second=11 third=12] | s=first=%d second=%d third=%d})
-- Val "first=10 second=11 third=12"
--
-- >>> pl @(PrintL 2 "first=%d second=%d third=%d" Id) [10,11,12]
-- Error toITupleListC: expected exactly 2 values (ToITupleList(2) instead found 3)
-- Fail "toITupleListC: expected exactly 2 values"
--
-- >>> pl @(PrintL 4 "first=%d second=%d third=%d" Id) [10,11,12]
-- Error toITupleListC: expected exactly 4 values (ToITupleList(4) instead found 3)
-- Fail "toITupleListC: expected exactly 4 values"
--
-- >>> pl @(PrintL 4 "%03d.%03d.%03d.%03d" Id) [1,2,3,4]
-- Present "001.002.003.004" ((>>) "001.002.003.004" | {PrintI [001.002.003.004] | s=%03d.%03d.%03d.%03d})
-- Val "001.002.003.004"
--
-- >>> pl @(PrintL 4 "%03d.%03d.%03d.%03d" Id) [1,2,3,4,5]
-- Error toITupleListC: expected exactly 4 values (ToITupleList(4) instead found 5)
-- Fail "toITupleListC: expected exactly 4 values"
--
-- >>> pl @(PrintL 4 "%03d.%03d.%03d.%03d" Id) [1,2,3]
-- Error toITupleListC: expected exactly 4 values (ToITupleList(4) instead found 3)
-- Fail "toITupleListC: expected exactly 4 values"
--
-- >>> pl @(PrintL 4 "%03d.%03d.%03d.%03d" Id) [1,2,3,4]
-- Present "001.002.003.004" ((>>) "001.002.003.004" | {PrintI [001.002.003.004] | s=%03d.%03d.%03d.%03d})
-- Val "001.002.003.004"
--
-- >>> pl @(PrintL 4 "%d %4d %-d %03d" Id) [1..4]
-- Present "1    2 3 004" ((>>) "1    2 3 004" | {PrintI [1    2 3 004] | s=%d %4d %-d %03d})
-- Val "1    2 3 004"
--
data PrintL (n :: Nat) s p deriving Show
type PrintLT (n :: Nat) s p = p >> ToITupleList n >> ReverseITuple >> PrintI s

instance P (PrintLT n s p) x => P (PrintL n s p) x where
  type PP (PrintL n s p) x = PP (PrintLT n s p) x
  eval _ = eval (Proxy @(PrintLT n s p))

