-- very experimental: skip this
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
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedLists #-}
-- |
-- Module      : Extras
-- Description : Very experimental P instances that didn't quite make it
-- Copyright   : (c) Grant Weyburne, 2019
-- License     : BSD-3
-- Maintainer  : gbwey9@gmail.com
--
module Extras where
import UtilP
import Predicate
import GHC.TypeLits (ErrorMessage((:$$:),(:<>:)))
import qualified GHC.TypeLits as GL
import Data.Proxy
import Data.Kind (Type)
import qualified Data.Bifunctor as BI
import Control.Lens hiding (strict,iall)
import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Comonad

data Test1

instance Show a => P Test1 a where
  type PP Test1 a = a
  eval _ opts a =
    let msg = "Test1"
    in pure $ mkNode opts (PresentT a) [msg <> show0 opts " " a, "line2 ......", "line3 ......", "line4 ......"] []

-- Proxyeval is for working with RepeatT where we want to use Proxy instead of type families
data Proxyeval -- use Proxy a for the input cos has more info than ()
instance P a (Proxy a) => P Proxyeval (Proxy a) where
  type PP Proxyeval (Proxy a) = PP a (Proxy a)
  eval _ opts pa = eval pa opts (Proxy @a)

data Fmapx p q

instance (P p (a -> a)
        , P q x
        , Show (t b)
        , ApplyFmapT (PP p (a -> a)) (PP q x) ~ t b
        , Functor t
        , PP p (a -> a) ~ (a -> b)
        , PP q x ~ t a
        ) => P (Fmapx p q) x where
  type PP (Fmapx p q) x = ApplyFmapT (PP p (FmapTX (PP q x))) (PP q x)
  eval _ opts x = do
    let msg0 = "(<$>)"
    pp <- eval (Proxy @p) opts (id @a)
    case getValueLR opts (msg0 <> " p failed") pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @q) opts x
        pure $ case getValueLR opts (msg0 <> " q failed") qq [hh pp] of
          Left e -> e
          Right q ->
            let d = p <$> q
            in mkNode opts (PresentT d) [msg0 <> show0 opts " " d] [hh pp, hh qq]

type family FmapTX (ta :: Type) :: Type where
  FmapTX (t a) = a -> a
  FmapTX ta = GL.TypeError (
       'GL.Text "FmapTX: (t a) but found something else"
       ':$$: 'GL.Text "t a = "
       ':<>: 'GL.ShowType ta)

data Fcurry' t t1 t2
type Fcurry (t :: Type) (t1 :: Type) (t2 :: Type) = Fcurry' (Hole t) (Hole t1) (Hole t2)

instance P (Fcurry' t t1 t2) a where
  type PP (Fcurry' t t1 t2) a = ((PP t a, PP t1 a) -> PP t2 a) -> PP t a -> PP t1 a -> PP t2 a
  eval _ opts _a = pure $ mkNode opts (PresentT curry) ["Fcurry"] []

data Funcurry' t t1 t2
type Funcurry (t :: Type) (t1 :: Type) (t2 :: Type) = Funcurry' (Hole t) (Hole t1) (Hole t2)

instance P (Funcurry' t t1 t2) a where
  type PP (Funcurry' t t1 t2) a = (PP t a -> PP t1 a -> PP t2 a) -> (PP t a, PP t1 a) -> PP t2 a
  eval _ opts _a = pure $ mkNode opts (PresentT uncurry) ["Funcurry"] []


data Fcurryz

instance P Fcurryz ((a,b) -> c) where
  type PP Fcurryz ((a,b) -> c) = a -> b -> c
  eval _ opts f = pure $ mkNode opts (PresentT (curry f)) ["Fcurryz"] []

data Fcurryw p

instance P p (a -> (b,c)) => P (Fcurryw p) (a -> (b,c)) where
  type PP (Fcurryw p) (a -> (b,c)) = PP p (a -> (b,c))
  eval _ opts f = do
    let msg0 = "Fcurryw"
    pp <- eval (Proxy @p) opts f
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p -> mkNode opts (PresentT p) [msg0] [hh pp]

data Funcurryw p

instance P p (a -> b -> c) => P (Funcurryw p) (a -> b -> c) where
  type PP (Funcurryw p) (a -> b -> c) = PP p (a -> b -> c)
  eval _ opts f = do
    let msg0 = "Funcurryw"
    pp <- eval (Proxy @p) opts f
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p -> mkNode opts (PresentT p) [msg0] [hh pp]

data Funcurryz

instance P Funcurryz (a -> b -> c) where
  type PP Funcurryz (a -> b -> c) = (a,b) -> c
  eval _ opts f = pure $ mkNode opts (PresentT (uncurry f)) ["Funcurryz"] []


data Fcurryx' t t1 t2
type Fcurryx (t :: Type) (t1 :: Type) (t2 :: Type) = Fcurryx' (Hole t) (Hole t1) (Hole t2)

instance P (Fcurryx' t t1 t2) a where
  type PP (Fcurryx' t t1 t2) a = ((PP t a, PP t1 a) -> PP t2 a) -> (PP t a -> PP t1 a -> PP t2 a)
  eval _ opts _ = pure $ mkNode opts (PresentT curry) ["Fcurryx"] []

-- normal functions on 2 args
-- comma function
data F_Comma' t t1
type F_Comma (t :: Type) (t1 :: Type) = F_Comma' (Hole t) (Hole t1)

instance P (F_Comma' t t1) a where
  type PP (F_Comma' t t1) a = PP t a -> PP t1 a -> (PP t a, PP t1 a)
  eval _ opts _a = pure $ mkNode opts (PresentT (,)) ["F_Comma"] []

data F_App' t
type F_App (t :: Type) = F_App' (Hole t)

instance Semigroup (PP t a) => P (F_App' t) a where
  type PP (F_App' t) a = PP t a -> PP t a -> PP t a
  eval _ opts _a = pure $ mkNode opts (PresentT (<>)) ["F_App"] []

data F_Alt'  t
type F_Alt (t :: Type) = F_Alt' (Hole t)

instance (Alternative f, PP t a ~ f b, GetAltT (PP t a) ~ f) => P (F_Alt' t) a where
  type PP (F_Alt' t) a = PP t a -> PP t a -> PP t a
  eval _ opts _a = pure $ mkNode opts (PresentT (<|>)) ["F_Alt"] []

type family GetAltT ta where
  GetAltT (t a) = t
  GetAltT ta = GL.TypeError (
      'GL.Text "GetAltT: expected (t a)"
      ':$$: 'GL.Text "t a = "
      ':<>: 'GL.ShowType ta)

data F_K' t t1
type F_K (t :: Type) (t1 :: Type) = F_K' (Hole t) (Hole t1)

instance P (F_K' t t1) a where
  type PP (F_K' t t1) a = PP t a -> PP t1 a -> PP t a
  eval _ opts _a = pure $ mkNode opts (PresentT const) ["F_K"] []

data F_Flipk' t t1
type F_Flipk (t :: Type) (t1 :: Type) = F_Flipk' (Hole t) (Hole t1)

instance P (F_Flipk' t t1) a where
  type PP (F_Flipk' t t1) a = PP t a -> PP t1 a -> PP t1 a
  eval _ opts _a = pure $ mkNode opts (PresentT (flip const)) ["F_Flipk"] []

-- normal functions on 1 arg
data F_Swap' t t1
type F_Swap (t :: Type) (t1 :: Type) = F_Swap' (Hole t) (Hole t1)

instance P (F_Swap' t t1) a where
  type PP (F_Swap' t t1) a = (PP t a, PP t1 a) -> (PP t1 a, PP t a)
  eval _ opts _a = pure $ mkNode opts (PresentT swapt) ["F_Swap"] []

data F_Swapl' (p :: Type -> Type -> Type) t t1 -- (p :: Type -> Type -> Type) is important to lock this down else ambiguity errors
type F_Swapl (p :: Type -> Type -> Type) (t :: Type) (t1 :: Type) = F_Swapl' p (Hole t) (Hole t1)

instance Swapped p => P (F_Swapl' p t t1) a where
  type PP (F_Swapl' p t t1) a = p (PP t a) (PP t1 a) -> p (PP t1 a) (PP t a)
  eval _ opts _a = pure $ mkNode opts (PresentT (\a -> a ^. swapped)) ["F_Swapl"] []


data F_Dup' t
type F_Dup (t :: Type) = F_Dup' (Hole t)

instance P (F_Dup' t) a where
  type PP (F_Dup' t) a = PP t a -> (PP t a, PP t a)
  eval _ opts _a = pure $ mkNode opts (PresentT dup) ["F_Dup"] []

-- curried <>
data F_Appa' t
type F_Appa (t :: Type) = F_Appa' (Hole t)

instance Semigroup (PP t a) => P (F_Appa' t) a where
  type PP (F_Appa' t) a = (PP t a, PP t a) -> PP t a
  eval _ opts _a = pure $ mkNode opts (PresentT (uncurry (<>))) ["F_Appa"] []

-- fst function
data F_1' t t1
type F_1 (t :: Type) (t1 :: Type) = F_1' (Hole t) (Hole t1)

instance P (F_1' t t1) a where
  type PP (F_1' t t1) a = (PP t a, PP t1 a) -> PP t a
  eval _ opts _a = pure $ mkNode opts (PresentT fst) ["F_1"] []

-- snd function
data F_2' t t1
type F_2 (t :: Type) (t1 :: Type) = F_2' (Hole t) (Hole t1)

instance P (F_2' t t1) a where
  type PP (F_2' t t1) a = (PP t a, PP t1 a) -> PP t1 a
  eval _ opts _a = pure $ mkNode opts (PresentT snd) ["F_2"] []

data F_Show' t
type F_Show (t :: Type) = F_Show' (Hole t)

instance Show (PP t a) => P (F_Show' t) a where
  type PP (F_Show' t) a = PP t a -> String
  eval _ opts _a = pure $ mkNode opts (PresentT show) ["F_Show"] []


-- so we can const a value: could use with <$> but <$ is already written and doesnt require 't'
-- const function
data K' t p
type K (t :: Type) p = K' (Hole t) p

-- works but we need an extra 't'
-- want to emulate <$ without a separate P method [same for <*]
-- pe2 @(K _ Fst <$> Snd) (4,Just 'x')
instance P p a => P (K' t p) a where
  type PP (K' t p) a = PP t a -> PP p a
  eval _ opts a = do
    let msg0 = "K"
    pp <- eval (Proxy @p) opts a
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p -> mkNode opts (PresentT (const p)) [msg0] [hh pp]

data Fdupx

instance P Fdupx (a -> b) where
  type PP Fdupx (a -> b) = a -> (b,b)
  eval _ opts ab = pure $ mkNode opts (PresentT (dup . ab)) ["Fdupx"] []

data F1Y

instance (a,b) ~ c => P F1Y ((a,b) -> c) where
  type PP F1Y ((a,b) -> c) = (a,b) -> a
  eval _ opts ab = pure $ mkNode opts (PresentT (fst . ab)) ["F1Y"] []

data F1X

instance P F1X (a -> (b,c)) where
  type PP F1X (a -> (b,c)) = a -> b
  eval _ opts ab = pure $ mkNode opts (PresentT (fst . ab)) ["F1X"] []

data F2X

instance P F2X (a -> (b,c)) where
  type PP F2X (a -> (b,c)) = a -> c
  eval _ opts ab = pure $ mkNode opts (PresentT (snd . ab)) ["F2X"] []

-- has the advantage over Fidp that we can calculate the type without running: once you use >> you are running it
data Fid' t
type Fid (t :: Type) = Fid' (Hole t)

instance P (Fid' t) a where
  type PP (Fid' t) a = PP t a -> PP t a
  eval _ opts _ = pure $ mkNode opts (PresentT id) ["Fid"] []

data Fidp
type FidpP = 'Proxy >> Fidp
type Fidpt (t :: Type) = Proxy t >> Fidp

instance P Fidp (Proxy (a :: Type)) where
  type PP Fidp (Proxy a) = a -> a
  eval _ opts _ = pure $ mkNode opts (PresentT id) ["Fidp"] []

dup :: a -> (a,a)
dup = join (,)

data Fdup' t
type Fdup (t :: Type) = Fdup' (Hole t)

instance P (Fdup' t) a where
  type PP (Fdup' t) a = PP t a -> (PP t a, PP t a)
  eval _ opts _a = pure $ mkNode opts (PresentT dup) ["Fdup"] []

data Fshow

instance Show b => P Fshow (a -> b) where
  type PP Fshow (a -> b) = a -> String
  eval _ opts ab = pure $ mkNode opts (PresentT (show . ab)) ["Fshow"] []

data Fswap

instance P Fswap (a -> (b,c)) where
  type PP Fswap (a -> (b,c)) = a -> (c,b)
  eval _ opts ab = pure $ mkNode opts (PresentT (swapt . ab)) ["Fswap"] []

data Fswapl

instance Swapped p => P Fswapl (a -> p b c) where
  type PP Fswapl (a -> p b c) = a -> p c b
  eval _ opts ab = pure $ mkNode opts (PresentT (\a -> ab a ^. swapped)) ["Fswapl"] []

-- forces input to come from 'a' but works! with F_ functions
data Bimap p q
type Bfirst p = Bimap p I
type Bsecond q = Bimap I q

instance (P p (a -> a)
        , P q (b -> b)
        , PP p (a -> a) ~ (a -> a')
        , PP q (b -> b) ~ (b -> b')
        , BothT (PP p (a -> a)) ~ a'
        , BothT (PP q (b -> b)) ~ b'
        , Bifunctor bi
        ) => P (Bimap p q) (bi a b) where
  type PP (Bimap p q) (bi a b) = bi (BothT (PP p (a -> a))) (BothT (PP q (b -> b)))
  eval _ opts bi = do
    let msg0 = "Bimap"
    pp <- eval (Proxy @p) opts (id @a)
    case getValueLR opts (msg0 <> "p failed") pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @q) opts (id @b)
        pure $ case getValueLR opts (msg0 <> "q failed") qq [hh pp] of
          Left e -> e
          Right q -> mkNode opts (PresentT (BI.bimap p q bi)) [msg0] [hh pp, hh qq]

data Fbimap p q
type Ffirst p = Fbimap p I
type Fsecond q = Fbimap I q

-- same as above but just a Dot difference -- BI.bimap p q . ibi
instance (P p (a -> a), P q (b -> b), PP p (a -> a) ~ (a -> a'), PP q (b -> b) ~ (b -> b'), BothT (PP p (a -> a)) ~ a', BothT (PP q (b -> b)) ~ b', Bifunctor bi) => P (Fbimap p q) (i -> bi a b) where
  type PP (Fbimap p q) (i -> bi a b) = i -> bi (BothT (PP p (a -> a))) (BothT (PP q (b -> b)))
  eval _ opts ibi = do
    let msg0 = "Fbimap"
    pp <- eval (Proxy @p) opts (id @a)
    case getValueLR opts (msg0 <> "p failed") pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @q) opts (id @b)
        pure $ case getValueLR opts (msg0 <> "q failed") qq [hh pp] of
          Left e -> e
          Right q -> mkNode opts (PresentT (BI.bimap p q . ibi)) [msg0] [hh pp, hh qq]

swapt :: (a,b) -> (b,a)
swapt (a,b) = (b,a)

-- normal functions on 2 args

type Bothx p q r = p *** q >> r Fst Snd -- it gets confused with the types for more complex situations
-- also doesnt partially apply a predicate but an Adt!

data Both p q

instance (P p (Proxy b), P q (Proxy c), PP p (Proxy b) ~ (b -> b'), PP q (Proxy c) ~ (c -> c')) => P (Both p q) (a -> (b,c)) where
  type PP (Both p q) (a -> (b,c))  = a -> (BothT (PP p (Proxy b)), BothT (PP q (Proxy c)))
  eval _ opts abc = do
    let msg0 = "Both"
    pp <- eval (Proxy @p) opts (Proxy @b)
    case getValueLR opts (msg0 <> " p failed") pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @q) opts (Proxy @c)
        pure $ case getValueLR opts (msg0 <> " q failed") qq [hh pp] of
          Left e -> e
          Right q -> mkNode opts (PresentT (\a -> (p *** q) (abc a))) [msg0] [hh pp, hh qq]

type family BothT ab where
  BothT (a -> b) = b

-- handled by >> (or <<)
data Dotx p

instance (P p (a -> b), PP p (a -> b) ~ (b' -> c), b ~ b') => P (Dotx p) (a -> b) where
  type PP (Dotx p) (a -> b) = DotT (a -> b) (PP p (a -> b))
  eval _ opts ab = do
    let msg0 = "Dotx"
    pp <- eval (Proxy @p) opts ab
    pure $ case getValueLR opts (msg0 <> " p failed") pp [] of
      Left e -> e
      Right p -> mkNode opts (PresentT (p . ab)) [msg0] [hh pp]

type family DotT ab bc where
  DotT (a -> b) (b -> c) = a -> c

-- we should be able to pass in any functions to Dot
data Dot

instance b ~ b' => P Dot (a -> b, b' -> c) where
  type PP Dot (a -> b, b' -> c) = a -> c
  eval _ opts (ab,bc) = pure $ mkNode opts (PresentT (bc . ab)) ["Dot"] []

data Curry

instance P Curry ((a,b) -> c) where
  type PP Curry ((a,b) -> c) = a -> b -> c
  eval _ opts abc = pure $ mkNode opts (PresentT (curry abc)) ["Curry"] []

data Uncurry

instance P Uncurry (a -> b -> c) where
  type PP Uncurry (a -> b -> c) = (a,b) -> c
  eval _ opts abc = pure $ mkNode opts (PresentT (uncurry abc)) ["Uncurry"] []

data Mappendx
-- b ~ b' is key! else cant do  -- pe @(FidpP >> F_Comma _ _ >> Uncurry >> Mappendx $ '("aa","bb")) 14
instance (Semigroup b, b ~ b') => P Mappendx (a -> (b,b')) where
  type PP Mappendx (a -> (b,b')) = a -> b
  eval _ opts abb = pure $ mkNode opts (PresentT (\a -> uncurry (<>) (abb a))) ["Mappendx"] []

data p <*> q
infixl 4 <*>
type Lifta2 p q r = p <$> q <*> r

type p <**> q = q <*> p
infixl 4 <**>

instance (P p x, P q x, Show (t b), ApplyApplicativeT (PP p x) (PP q x) ~ t b, Applicative t, PP p x ~ t (a -> b), PP q x ~ t a) => P (p <*> q) x where
  type PP (p <*> q) x = ApplyApplicativeT (PP p x) (PP q x)
  eval _ opts x = do
    let msg0 = "(<*>)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <*> q
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " d] [hh pp, hh qq]

type family ApplyApplicativeT (tab :: Type) (ta :: Type) :: Type where
  ApplyApplicativeT (t (a -> b)) (t a) = t b
  ApplyApplicativeT tab ta = GL.TypeError (
       'GL.Text "ApplyApplicativeT: (t (a -> b)) (t a) but found something else"
       ':$$: 'GL.Text "t (a -> b) = "
       ':<>: 'GL.ShowType tab
       ':$$: 'GL.Text "t a = "
       ':<>: 'GL.ShowType ta)

data p <$> q
infixl 4 <$>
type Fmap p q = p <$> q

instance (P p x
        , P q x
        , Show (t b)
        , ApplyFmapT (PP p x) (PP q x) ~ t b
        , Functor t
        , PP p x ~ (a -> b)
        , PP q x ~ t a
        ) => P (p <$> q) x where
  type PP (p <$> q) x = ApplyFmapT (PP p x) (PP q x)
  eval _ opts x = do
    let msg0 = "(<$>)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <$> q
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " d] [hh pp, hh qq]

type family ApplyFmapT (ab :: Type) (ta :: Type) :: Type where
  ApplyFmapT (a -> b) (t a) = t b
  ApplyFmapT ab ta = GL.TypeError (
       'GL.Text "ApplyFmapT: (a -> b) (t a) but found something else"
       ':$$: 'GL.Text "a -> b = "
       ':<>: 'GL.ShowType ab
       ':$$: 'GL.Text "t a = "
       ':<>: 'GL.ShowType ta)

data p >>= q
infixl 4 >>=

type p =<< q = q >>= p
infixr 1 =<<

instance (P p x, P q x, Show (m b), ApplyMonadT (PP p x) (PP q x) ~ m b, Monad m, PP p x ~ m a, PP q x ~ (a -> m b)) => P (p >>= q) x where
  type PP (p >>= q) x = ApplyMonadT (PP p x) (PP q x)
  eval _ opts x = do
    let msg0 = "(>>=)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p >>= q
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " d] [hh pp, hh qq]


type family ApplyMonadT (ma :: Type) (amb :: Type) :: Type where
  ApplyMonadT (m a) (a -> m b) = m b
  ApplyMonadT ma amb = GL.TypeError (
       'GL.Text "ApplyMonadT: (m a) (a -> m b) but found something else"
       ':$$: 'GL.Text "m a = "
       ':<>: 'GL.ShowType ma
       ':$$: 'GL.Text "a -> m b = "
       ':<>: 'GL.ShowType amb)

data Extend p q
type p <<= q = Extend p q
infixl 4 <<=

type p =>> q = q <<= p
infixr 1 =>>

instance (P p x
        , P q x
        , Show (w b)
        , ApplyComonadT (PP p x) (PP q x) ~ w b
        , Comonad w
        , PP p x ~ (w a -> b)
        , PP q x ~ w a
        ) => P (Extend p q) x where
  type PP (Extend p q) x = ApplyComonadT (PP p x) (PP q x)
  eval _ opts x = do
    let msg0 = "(<<=)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = extend p q
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " d] [hh pp, hh qq]

type family ApplyComonadT (wab :: Type) (wa :: Type) :: Type where
  ApplyComonadT (w a -> b) (w a) = w b
  ApplyComonadT wab wa = GL.TypeError (
       'GL.Text "ApplyComonadT: (w a -> b) (w a) but found something else"
       ':$$: 'GL.Text "w a -> b = "
       ':<>: 'GL.ShowType wab
       ':$$: 'GL.Text "w a = "
       ':<>: 'GL.ShowType wa)

data Mark (t :: Type) p

instance P p a => P (Mark t p) a where
  type PP (Mark t p) a = PP p a
  eval _ opts a = do
    let msg0 = "Mark"
    pp <- eval (Proxy @p) opts a
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right _p -> mkNode opts (_tBool pp) [msg0] [hh pp]


type ToProxylist = 'Proxy >> Proxylist

data Proxylist

instance P Proxylist (proxy (a :: Type)) where
  type PP Proxylist (proxy a) = Proxy [a]
  eval _ opts _pa = pure $ mkNode opts (PresentT (Proxy @[a])) ["Proxylist"] []

data Proxyfaa
type Proxyfaa' = 'Proxy >> Proxyfaa

instance P Proxyfaa (proxy (f a)) where
  type PP Proxyfaa (proxy (f a)) = Proxy a
  eval _ opts _ = pure $ mkNode opts (PresentT (Proxy @a)) ["Proxyfaa"] []

data Proxyfaba
type Proxyfaba' = 'Proxy >> Proxyfaba

instance P Proxyfaba (proxy (f a b)) where
  type PP Proxyfaba (proxy (f a b)) = Proxy a
  eval _ opts _ = pure $ mkNode opts (PresentT (Proxy @a)) ["Proxyfaba"] []

data Proxyfabb
type Proxyfabb' = 'Proxy >> Proxyfabb

instance P Proxyfabb (proxy (f a b)) where
  type PP Proxyfabb (proxy (f a b)) = Proxy b
  eval _ opts _ = pure $ mkNode opts (PresentT (Proxy @b)) ["Proxyfabb"] []


type Lefts' = ListOf LeftToMaybe
type Rights' = ListOf RightToMaybe
type PartitionEithers' = '(Lefts', Rights')

type Thiss' = ListOf ThisToMaybe
type Thats' = ListOf ThatToMaybe
type Theses' = ListOf TheseToMaybe
type PartitionThese' = '(Thiss', '(Thats', Theses'))

type ListOf p = Concat << MapF (GDef_PA p (Proxylist >> MemptyProxy) (Pure [] Snd) Id)


data Fmap_SWAP
instance (Swapped p, Functor f) => P Fmap_SWAP (f (p a b)) where
  type PP Fmap_SWAP (f (p a b)) = f (p b a)
  eval _ opts mb = pure $ mkNode opts (PresentT (view swapped <$> mb)) ["Fmap_SWAP"] []

data Fmap_SEMI
instance (Semigroup s, Functor f) => P Fmap_SEMI (f (s,s)) where
  type PP Fmap_SEMI (f (s,s)) = f s
  eval _ opts mb = pure $ mkNode opts (PresentT (uncurry (<>) <$> mb)) ["Fmap_SEMI"] []

data Fmap_ALT
instance (Alternative t, Functor f) => P Fmap_ALT (f (t a,t a)) where
  type PP Fmap_ALT (f (t a,t a)) = f (t a)
  eval _ opts mb = pure $ mkNode opts (PresentT (uncurry (<|>) <$> mb)) ["Fmap_ALT"] []

data Fmap_APP
instance (Applicative t, Functor f) => P Fmap_APP (f (t a,t b)) where
  type PP Fmap_APP (f (t a,t b)) = f (t (a,b))
  eval _ opts mb = pure $ mkNode opts (PresentT (uncurry (liftA2 (,)) <$> mb)) ["Fmap_APP"] []

data Fmap_APPK
instance (Applicative t, Functor f) => P Fmap_APPK (f (t a,t x)) where
  type PP Fmap_APPK (f (t a,t x)) = f (t a)
  eval _ opts mb = pure $ mkNode opts (PresentT (uncurry (<*) <$> mb)) ["Fmap_APPK"] []

data Fmap_CONS
instance (Functor f
        , Cons s s (ConsT s) (ConsT s)
        , ConsT s ~ a
        ) => P Fmap_CONS (f (a, s)) where
  type PP Fmap_CONS (f (a,s)) = f s
  eval _ opts mb = pure $ mkNode opts (PresentT (uncurry cons <$> mb)) ["Fmap_CONS"] []

data Fmap_SNOC
instance (Functor f
        , Snoc s s (ConsT s) (ConsT s)
        , ConsT s ~ a
        ) => P Fmap_SNOC (f (s, a)) where
  type PP Fmap_SNOC (f (s,a)) = f s
  eval _ opts mb = pure $ mkNode opts (PresentT (uncurry snoc <$> mb)) ["Fmap_SNOC"] []

data Fmap_INS p q
instance (Functor f
        , P p x
        , P q x
        , PP p x ~ b
        , PP q x ~ f a
        ) => P (Fmap_INS p q) x where
  type PP (Fmap_INS p q) x = Fmap_InsT (PP q x) (PP p x)
  eval _ opts x = do
    let msg0 = "Fmap_INS"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) -> mkNode opts (PresentT ((p,) <$> q)) ["Fmap_INS"] [hh pp, hh qq]

type family Fmap_InsT fa b where
  Fmap_InsT (f a) b = f (b,a)

