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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     promoted read, show, and printf functions
-}
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

 ) where
import Predicate.Core
import Predicate.Util
import GHC.TypeLits (Nat,KnownNat)
import qualified GHC.TypeLits as GL
import Data.Proxy
import Data.Kind (Type)
import Text.Printf
import qualified Control.Exception as E
import Data.Typeable
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
-- PresentT "[4,8,3,9]"
--
-- >>> pz @(ShowP Id) 'x'
-- PresentT "'x'"
--
-- >>> pz @(ShowP (42 -% 10)) 'x'
-- PresentT "(-21) % 5"
--
data ShowP p

instance ( Show (PP p x)
         , P p x
         ) => P (ShowP p) x where
  type PP (ShowP p) x = String
  eval _ opts x = do
    let msg0 = "ShowP"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = show p
        in mkNode opts (PresentT d) (msg0 <> " " <> litL opts d <> showVerbose opts " | " p) [hh pp]

-- | uses the 'Read' of the given type \'t\' and \'p\' which points to the content to read
data ReadP' t p

instance (P p x
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
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right s ->
        let hhs = [hh pp]
        in case reads @(PP t x) s of
           [(b,"")] -> mkNode opts (PresentT b) (msg0 <> " " ++ showL opts b) hhs
           o -> mkNode opts (FailT (msg0 <> " (" ++ s ++ ")")) (showVerbose opts "" o) hhs

-- | uses the 'Read' of the given type \'t\' and \'p\' which points to the content to read
--
-- >>> pz @(ReadP Rational Id) "4 % 5"
-- PresentT (4 % 5)
--
-- >>> pz @(Between (ReadP Day "2017-04-11") (ReadP Day "2018-12-30") (ReadP Day Id)) "2018-10-12"
-- TrueT
--
-- >>> pz @(Between (ReadP Day "2017-04-11") (ReadP Day "2018-12-30") (ReadP Day Id)) "2016-10-12"
-- FalseT
--
-- >>> pl @(ReadP Rational Id) "123 % 4"
-- Present 123 % 4 (ReadP Ratio Integer 123 % 4)
-- PresentT (123 % 4)
--
-- >>> pl @(ReadP Rational Id) "x123 % 4"
-- Error ReadP Ratio Integer (x123 % 4) ([])
-- FailT "ReadP Ratio Integer (x123 % 4)"
--
-- >>> pl @(ReadP Day Id) "1999-11-30"
-- Present 1999-11-30 (ReadP Day 1999-11-30)
-- PresentT 1999-11-30
--
-- >>> pl @(ReadP Day Id) "1999-02-29"
-- Error ReadP Day (1999-02-29) ([])
-- FailT "ReadP Day (1999-02-29)"
--
-- >>> pl @(ReadP TimeOfDay Id) "14:59:20"
-- Present 14:59:20 (ReadP TimeOfDay 14:59:20)
-- PresentT 14:59:20
--
data ReadP (t :: Type) p
type ReadPT (t :: Type) p = ReadP' (Hole t) p

instance P (ReadPT t p) x => P (ReadP t p) x where
  type PP (ReadP t p) x = PP (ReadPT t p) x
  eval _ = eval (Proxy @(ReadPT t p))


-- [] (a,s) (a,[])

-- | Read but returns the Maybe of the value and any remaining unparsed string
--
-- >>> pz @(ReadMaybe Int Id) "123x"
-- PresentT (Just (123,"x"))
--
-- >>> pz @(ReadMaybe Int Id) "123"
-- PresentT (Just (123,""))
--
-- >>> pz @(ReadMaybe Int Id) "x123"
-- PresentT Nothing
--
data ReadMaybe' t p

instance (P p x
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
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right s ->
        let msg1 = msg0 <> " (" <> s <> ")"
            hhs = [hh pp]
        in case reads @(PP t x) s of
           [(b,rest)] -> mkNode opts (PresentT (Just (b,rest))) (lit01 opts msg1 b "" s) hhs
           o -> mkNode opts (PresentT Nothing) (msg1 <> " failed" <> showVerbose opts " " o) hhs

data ReadMaybe (t :: Type) p
type ReadMaybeT (t :: Type) p = ReadMaybe' (Hole t) p

instance P (ReadMaybeT t p) x => P (ReadMaybe t p) x where
  type PP (ReadMaybe t p) x = PP (ReadMaybeT t p) x
  eval _ = eval (Proxy @(ReadMaybeT t p))

-- | uses PrintF (unsafe) to format output for a single value
--
-- >>> pz @(PrintF "value=%03d" Id) 12
-- PresentT "value=012"
--
-- >>> pz @(PrintF "%s" (Fst Id)) ("abc",'x')
-- PresentT "abc"
--
-- >>> pz @(PrintF "%d" (Fst Id)) ("abc",'x')
-- FailT "PrintF (IO e=printf: bad formatting char 'd')"
--
-- >>> pl @(PrintF "someval %d" Id) ("!23"::String)
-- Error PrintF (IO e=printf: bad formatting char 'd') ("!23" s=someval %d)
-- FailT "PrintF (IO e=printf: bad formatting char 'd')"
--
-- >>> pl @(PrintF "%-6s" Id) (1234 :: Int)
-- Error PrintF (IO e=printf: bad formatting char 's') (1234 s=%-6s)
-- FailT "PrintF (IO e=printf: bad formatting char 's')"
--
-- >>> pl @(PrintF "%06x" Id) (1234 :: Int)
-- Present "0004d2" (PrintF [0004d2] | p=1234 | s=%06x)
-- PresentT "0004d2"
--
-- >>> pl @(Msg (PrintF "digits=%d" Len) (Head Id)) [1..4]
-- Present 1 (digits=4 Head 1 | [1,2,3,4])
-- PresentT 1
--
-- >>> pl @(PrintF "ask%%dfas%%kef%05d hey %%" Id) (35 :: Int)
-- Present "ask%dfas%kef00035 hey %" (PrintF [ask%dfas%kef00035 hey %] | p=35 | s=ask%%dfas%%kef%05d hey %%)
-- PresentT "ask%dfas%kef00035 hey %"
--
-- >>> pl @(Fail () (PrintF "someval int=%d" Id)) (45 :: Int)
-- Error someval int=45
-- FailT "someval int=45"
--
data PrintF s p

instance (PrintfArg (PP p x)
        , Show (PP p x)
        , PP s x ~ String
        , P s x
        , P p x
        ) => P (PrintF s p) x where
  type PP (PrintF s p) x = String
  eval _ opts x = do
    let msg0 = "PrintF"
    lrx <- runPQ msg0 (Proxy @s) (Proxy @p) opts x []
    case lrx of
      Left e -> pure e
      Right (s,p,ss,pp) -> do
        lr <- catchitNF @_ @E.SomeException (printf s p)
        pure $ case lr of
          Left e -> mkNode opts (FailT (msg0 <> " (" <> e <> ")")) (showL opts p <> " s=" <> s) [hh ss, hh pp]
          Right ret -> mkNode opts (PresentT ret) (msg0 <> " [" <> litL opts ret <> "]" <> showVerbose opts " | p=" p <> litVerbose opts " | s=" s) [hh ss, hh pp]


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
-- >>> pl @(PrintT "%d %s %s %s" '(Fst Id, Snd Id, Snd Id,Snd Id)) (10,"Asdf")
-- Present "10 Asdf Asdf Asdf" (PrintT [10 Asdf Asdf Asdf] | s=%d %s %s %s)
-- PresentT "10 Asdf Asdf Asdf"
--
-- >>> pl @(PrintT "%c %d %s" Id) ('x', 10,"Asdf")
-- Present "x 10 Asdf" (PrintT [x 10 Asdf] | s=%c %d %s)
-- PresentT "x 10 Asdf"
--
-- >>> pz @(PrintT "fst=%s snd=%03d" Id) ("ab",123)
-- PresentT "fst=ab snd=123"
--
-- >>> pz @(PrintT "fst=%s snd=%03d thd=%s" Id) ("ab",123,"xx")
-- PresentT "fst=ab snd=123 thd=xx"
--
-- >>> pl @(PrintT "%s %d %c %s" '(W "xyz", Fst Id, Snd Id, Thd Id)) (123,'x',"ab")
-- Present "xyz 123 x ab" (PrintT [xyz 123 x ab] | s=%s %d %c %s)
-- PresentT "xyz 123 x ab"
--
-- >>> pl @(PrintT "%d %c %s" Id) (123,'x')
-- Error PrintT(IO e=printf: argument list ended prematurely) (PrintT %d %c %s)
-- FailT "PrintT(IO e=printf: argument list ended prematurely)"
--
-- >>> pl @(PrintT "%d %c %s" Id) (123,'x',"abc",11)
-- Error PrintT(IO e=printf: formatting string ended prematurely) (PrintT %d %c %s)
-- FailT "PrintT(IO e=printf: formatting string ended prematurely)"
--
-- >>> pl @(PrintT "lhs = %d rhs = %s" Id) (123::Int,"asdf"::String)
-- Present "lhs = 123 rhs = asdf" (PrintT [lhs = 123 rhs = asdf] | s=lhs = %d rhs = %s)
-- PresentT "lhs = 123 rhs = asdf"
--
-- >>> pl @(PrintT "d=%03d s=%s" Id) (9::Int,"ab"::String)
-- Present "d=009 s=ab" (PrintT [d=009 s=ab] | s=d=%03d s=%s)
-- PresentT "d=009 s=ab"
--
-- >>> pl @(PrintT "d=%03d s=%s c=%c f=%4.2f" Id) (9::Int,"ab"::String,'x',1.54::Float)
-- Present "d=009 s=ab c=x f=1.54" (PrintT [d=009 s=ab c=x f=1.54] | s=d=%03d s=%s c=%c f=%4.2f)
-- PresentT "d=009 s=ab c=x f=1.54"
--
-- >>> pl @(PrintT "d=%03d s=%s" Id) (9::Int, "ab"::String,'x',1.54::Float)
-- Error PrintT(IO e=printf: formatting string ended prematurely) (PrintT d=%03d s=%s)
-- FailT "PrintT(IO e=printf: formatting string ended prematurely)"
--
-- >>> pl @(PrintT "lhs = %d rhs = %s c=%d" Id) (123::Int,"asdf"::String,'x')
-- Present "lhs = 123 rhs = asdf c=120" (PrintT [lhs = 123 rhs = asdf c=120] | s=lhs = %d rhs = %s c=%d)
-- PresentT "lhs = 123 rhs = asdf c=120"
--
-- >>> pl @(PrintT "hello d=%d %c %s" '(12, Char1 "z", "someval")) ()
-- Present "hello d=12 z someval" (PrintT [hello d=12 z someval] | s=hello d=%d %c %s)
-- PresentT "hello d=12 z someval"
--
-- >>> pl @(PrintT "ipaddress %03d.%03d.%03d.%03d" '(1,2,3,4)) ()
-- Present "ipaddress 001.002.003.004" (PrintT [ipaddress 001.002.003.004] | s=ipaddress %03d.%03d.%03d.%03d)
-- PresentT "ipaddress 001.002.003.004"
--
data PrintT s p
instance (PrintC bs
        , (b,bs) ~ InductTupleP y
        , InductTupleC y
        , PrintfArg b
        , PP s x ~ String
        , PP p x ~ y
        , P s x
        , P p x
        , CheckT (PP p x) ~ 'True
        ) => P (PrintT s p) x where
  type PP (PrintT s p) x = String
  eval _ opts x = do
    let msg0 = "PrintT"
    lrx <- runPQ msg0 (Proxy @s) (Proxy @p) opts x []
    case lrx of
      Left e -> pure e
      Right (s,y,ss,pp) -> do
        let hhs = [hh ss, hh pp]
        lr <- catchitNF @_ @E.SomeException (prtC @bs s (inductTupleC y))
        pure $ case lr of
          Left e -> mkNode opts (FailT (msg0 <> "(" <> e <> ")")) (msg0 <> " " <> s) hhs
          Right ret -> mkNode opts (PresentT ret) (msg0 <> " [" <> litL opts ret <> "] | s=" <> litL opts s) hhs

type family CheckT (tp :: Type) :: Bool where
  CheckT () = GL.TypeError ('GL.Text "Printfn: inductive tuple cannot be empty")
  CheckT o = 'True


-- | print for lists  -- use 'PrintT' as it is safer than 'PrintL'
--
-- >>> pl @(PrintL 4 "%s %s %s %s" '[W "xyz", ShowP (Fst Id), ShowP (Snd Id), Thd Id]) (123,'x',"ab")
-- Present "xyz 123 'x' ab" (PrintL(4) [xyz 123 'x' ab] | s=%s %s %s %s)
-- PresentT "xyz 123 'x' ab"
--
-- >>> pz @(PrintL 1 "%05d" '[Id]) 123  -- tick is required for a one element list (use 'PrintF')
-- PresentT "00123"
--
-- >>> pz @(PrintL 2 "%d %05d" [Fst Id,Snd Id]) (29,123)
-- PresentT "29 00123"
--
-- >>> pl @(PrintL 3 "first=%d second=%d third=%d" Id) [10,11,12]
-- Present "first=10 second=11 third=12" (PrintL(3) [first=10 second=11 third=12] | s=first=%d second=%d third=%d)
-- PresentT "first=10 second=11 third=12"
--
-- >>> pl @(PrintL 2 "first=%d second=%d third=%d" Id) [10,11,12]
-- Error PrintL(2) arg count=3 (wrong length 3)
-- FailT "PrintL(2) arg count=3"
--
-- >>> pl @(PrintL 4 "first=%d second=%d third=%d" Id) [10,11,12]
-- Error PrintL(4) arg count=3 (wrong length 3)
-- FailT "PrintL(4) arg count=3"
--
-- >>> pl @(PrintL 4 "%03d.%03d.%03d.%03d" Id) [1,2,3,4::Int]
-- Present "001.002.003.004" (PrintL(4) [001.002.003.004] | s=%03d.%03d.%03d.%03d)
-- PresentT "001.002.003.004"
--
-- >>> pl @(PrintL 4 "%03d.%03d.%03d.%03d" Id) [1,2,3,4,5::Int]
-- Error PrintL(4) arg count=5 (wrong length 5)
-- FailT "PrintL(4) arg count=5"
--
-- >>> pl @(PrintL 4 "%03d.%03d.%03d.%03d" Id) [1,2,3::Int]
-- Error PrintL(4) arg count=3 (wrong length 3)
-- FailT "PrintL(4) arg count=3"
--
-- >>> pl @(PrintL 4 "%03d.%03d.%03d.%03d" Id) [1,2,3,4::Int]
-- Present "001.002.003.004" (PrintL(4) [001.002.003.004] | s=%03d.%03d.%03d.%03d)
-- PresentT "001.002.003.004"
--
-- >>> pl @(PrintL 4 "%d %4d %-d %03d" Id) [1..4::Int]
-- Present "1    2 3 004" (PrintL(4) [1    2 3 004] | s=%d %4d %-d %03d)
-- PresentT "1    2 3 004"
--

data PrintL (n :: Nat) s p

instance (KnownNat n
        , PrintC bs
        , (b,bs) ~ InductListP n a
        , InductListC n a
        , PrintfArg b
        , PP s x ~ String
        , PP p x ~ [a]
        , P s x
        , P p x
        ) => P (PrintL n s p) x where
  type PP (PrintL n s p) x = String
  eval _ opts x = do
    let msg0 = "PrintL(" ++ show n ++ ")"
        n = nat @n
    lrx <- runPQ msg0 (Proxy @s) (Proxy @p) opts x []
    case lrx of
      Left e -> pure e
      Right (s,p,ss,pp) -> do
        let hhs = [hh ss, hh pp]
        if length p /= n then pure $ mkNode opts (FailT (msg0 <> " arg count=" ++ show (length p))) ("wrong length " ++ show (length p)) hhs
        else do
          lr <- catchitNF @_ @E.SomeException (prtC @bs s (inductListC @n @a p))
          pure $ case lr of
            Left e -> mkNode opts (FailT (msg0 <> "(" <> e <> ")")) ("s=" <> s) hhs
            Right ret -> mkNode opts (PresentT ret) (msg0 <> " [" <> litL opts ret <> "] | s=" <> litL opts s) hhs

