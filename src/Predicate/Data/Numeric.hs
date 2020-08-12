{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-compat #-}
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
     promoted numeric functions
-}
module Predicate.Data.Numeric (

  -- ** numeric expressions
    type (+)
  , type (-)
  , type (*)
  , type (/)
  , Negate
  , Abs
  , Signum
  , FromInteger
  , FromInteger'
  , FromIntegral
  , FromIntegral'
  , Truncate
  , Truncate'
  , Ceiling
  , Ceiling'
  , Floor
  , Floor'
  , Even
  , Odd
  , Div
  , Mod
  , DivMod
  , QuotRem
  , Quot
  , Rem
  , LogBase
  , type (^)
  , type (**)
  , RoundUp

  -- *** rational numbers
  , type (%)
  , type (-%)
  , ToRational
  , FromRational
  , FromRational'

 -- ** read / show expressions
  , ReadBase
  , ReadBase'
  , ShowBase

  , RoundUpX
--  , RoundUpY
 ) where
import Predicate.Core
import Predicate.Util
import Predicate.Data.Ordering (type (==))
import GHC.TypeLits (Nat,KnownNat)
import qualified GHC.TypeLits as GL
import Data.List
import Data.Proxy
import Data.Typeable
import Data.Kind (Type)
import Data.Maybe
import qualified Numeric
import Data.Char
import Data.Ratio
import GHC.Real (Ratio((:%)))
import Control.Lens ((&))
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG
-- >>> import Data.Time

data FromInteger' t n

instance (Num (PP t a)
        , Integral (PP n a)
        , P n a
        , Show (PP t a)
        ) => P (FromInteger' t n) a where
  type PP (FromInteger' t n) a = PP t a
  eval _ opts a = do
    let msg0 = "FromInteger"
    nn <- eval (Proxy @n) opts a
    pure $ case getValueLR opts msg0 nn [] of
      Left e -> e
      Right n ->
        let b = fromInteger (fromIntegral n)
        in mkNode opts (PresentT b) (msg0 <> " " <> showL opts b) [hh nn]

-- | 'fromInteger' function where you need to provide the type \'t\' of the result
--
-- >>> pz @(FromInteger (SG.Sum _) Id) 23
-- PresentT (Sum {getSum = 23})
--
-- >>> pz @(FromInteger Rational 44) 12
-- PresentT (44 % 1)
--
-- >>> pz @(FromInteger Rational Id) 12
-- PresentT (12 % 1)
--
-- >>> pl @((FromInteger _ 12 &&& Id) >> Fst Id + Snd Id) (SG.Min 7)
-- Present Min {getMin = 19} ((>>) Min {getMin = 19} | {getMin = 19})
-- PresentT (Min {getMin = 19})
--
-- >>> pl @((FromInteger _ 12 &&& Id) >> SapA) (SG.Product 7)
-- Present Product {getProduct = 84} ((>>) Product {getProduct = 84} | {getProduct = 84})
-- PresentT (Product {getProduct = 84})
--
-- >>> pl @(FromInteger (SG.Sum _) (Fst Id)) (3,"A")
-- Present Sum {getSum = 3} (FromInteger Sum {getSum = 3})
-- PresentT (Sum {getSum = 3})
--
-- >>> pl @(FromInteger DiffTime 123) 'x'
-- Present 123s (FromInteger 123s)
-- PresentT 123s
--
data FromInteger (t :: Type) p
type FromIntegerT (t :: Type) p = FromInteger' (Hole t) p
--type FromIntegerP n = FromInteger' Unproxy n

instance P (FromIntegerT t p) x => P (FromInteger t p) x where
  type PP (FromInteger t p) x = PP (FromIntegerT t p) x
  eval _ = eval (Proxy @(FromIntegerT t p))

-- | 'fromIntegral' function where you need to provide the type \'t\' of the result
--
-- >>> pz @(FromIntegral (SG.Sum _) Id) 23
-- PresentT (Sum {getSum = 23})
data FromIntegral' t n

instance (Num (PP t a)
        , Integral (PP n a)
        , P n a
        , Show (PP t a)
        , Show (PP n a)
        ) => P (FromIntegral' t n) a where
  type PP (FromIntegral' t n) a = PP t a
  eval _ opts a = do
    let msg0 = "FromIntegral"
    nn <- eval (Proxy @n) opts a
    pure $ case getValueLR opts msg0 nn [] of
      Left e -> e
      Right n ->
        let b = fromIntegral n
        in mkNode opts (PresentT b) (show01 opts msg0 b n) [hh nn]

data FromIntegral (t :: Type) p
type FromIntegralT (t :: Type) p = FromIntegral' (Hole t) p

instance P (FromIntegralT t p) x => P (FromIntegral t p) x where
  type PP (FromIntegral t p) x = PP (FromIntegralT t p) x
  eval _ = eval (Proxy @(FromIntegralT t p))

-- | 'toRational' function
--
-- >>> pz @(ToRational Id) 23.5
-- PresentT (47 % 2)
--
-- >>> pl @((ToRational 123 &&& Id) >> Fst Id + Snd Id) 4.2
-- Present 636 % 5 ((>>) 636 % 5 | {123 % 1 + 21 % 5 = 636 % 5})
-- PresentT (636 % 5)
--
-- >>> pl @(Fst Id >= Snd Id || Snd Id > 23 || 12 -% 5 <= ToRational (Fst Id)) (12,13)
-- True (False || True)
-- TrueT
--
-- >>> pl @(ToRational 14) ()
-- Present 14 % 1 (ToRational 14 % 1 | 14)
-- PresentT (14 % 1)
--
-- >>> pl @(ToRational 5 / ToRational 3) 'x'
-- Present 5 % 3 (5 % 1 / 3 % 1 = 5 % 3)
-- PresentT (5 % 3)
--

data ToRational p

instance (a ~ PP p x
         , Show a
         , Real a
         , P p x)
   => P (ToRational p) x where
  type PP (ToRational p) x = Rational
  eval _ opts x = do
    let msg0 = "ToRational"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right a ->
        let r = toRational a
        in mkNode opts (PresentT r) (show01 opts msg0 r a) [hh pp]

-- | 'fromRational' function where you need to provide the type \'t\' of the result
--
-- >>> pl @(FromRational' (Fst Id) (Snd Id)) (1::Float,2 % 5)
-- Present 0.4 (FromRational 0.4 | 2 % 5)
-- PresentT 0.4
--
data FromRational' t r

instance (P r a
        , PP r a ~ Rational
        , Show (PP t a)
        , Fractional (PP t a)
        ) => P (FromRational' t r) a where
  type PP (FromRational' t r) a = PP t a
  eval _ opts a = do
    let msg0 = "FromRational"
    rr <- eval (Proxy @r) opts a
    pure $ case getValueLR opts msg0 rr [] of
      Left e -> e
      Right r ->
        let b = fromRational @(PP t a) r
        in mkNode opts (PresentT b) (show01 opts msg0 b r) [hh rr]

-- | 'fromRational' function where you need to provide the type \'t\' of the result
--
-- >>> pz @(FromRational Rational Id) 23.5
-- PresentT (47 % 2)
--
-- >>> pl @(FromRational Float (4 % 5)) ()
-- Present 0.8 (FromRational 0.8 | 4 % 5)
-- PresentT 0.8
--
data FromRational (t :: Type) p
type FromRationalT (t :: Type) p = FromRational' (Hole t) p

instance P (FromRationalT t p) x => P (FromRational t p) x where
  type PP (FromRational t p) x = PP (FromRationalT t p) x
  eval _ = eval (Proxy @(FromRationalT t p))

-- | 'truncate' function where you need to provide the type \'t\' of the result
--
-- >>> pz @(Truncate Int Id) (23 % 5)
-- PresentT 4
--
-- >>> pl @(Truncate' (Fst Id >> Unproxy) (Snd Id)) (Proxy @Integer,2.3)
-- Present 2 (Truncate 2 | 2.3)
-- PresentT 2
--
-- >>> pl @(Truncate' (Fst Id) (Snd Id)) (1::Int,2.3)
-- Present 2 (Truncate 2 | 2.3)
-- PresentT 2
--
data Truncate' t p

instance (Show (PP p x)
        , P p x
        , Show (PP t x)
        , RealFrac (PP p x)
        , Integral (PP t x)
        ) => P (Truncate' t p) x where
  type PP (Truncate' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "Truncate"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = truncate p
        in mkNode opts (PresentT b) (show01 opts msg0 b p) [hh pp]

data Truncate (t :: Type) p
type TruncateT (t :: Type) p = Truncate' (Hole t) p

instance P (TruncateT t p) x => P (Truncate t p) x where
  type PP (Truncate t p) x = PP (TruncateT t p) x
  eval _ = eval (Proxy @(TruncateT t p))

-- | 'ceiling' function where you need to provide the type \'t\' of the result
--
-- >>> pz @(Ceiling Int Id) (23 % 5)
-- PresentT 5
data Ceiling' t p

instance (Show (PP p x)
        , P p x
        , Show (PP t x)
        , RealFrac (PP p x)
        , Integral (PP t x)
        ) => P (Ceiling' t p) x where
  type PP (Ceiling' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "Ceiling"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = ceiling p
        in mkNode opts (PresentT b) (show01 opts msg0 b p) [hh pp]

data Ceiling (t :: Type) p
type CeilingT (t :: Type) p = Ceiling' (Hole t) p

instance P (CeilingT t p) x => P (Ceiling t p) x where
  type PP (Ceiling t p) x = PP (CeilingT t p) x
  eval _ = eval (Proxy @(CeilingT t p))

-- | 'floor' function where you need to provide the type \'t\' of the result
--
-- >>> pz @(Floor Int Id) (23 % 5)
-- PresentT 4
data Floor' t p

instance (Show (PP p x)
        , P p x
        , Show (PP t x)
        , RealFrac (PP p x)
        , Integral (PP t x)
        ) => P (Floor' t p) x where
  type PP (Floor' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "Floor"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = floor p
        in mkNode opts (PresentT b) (show01 opts msg0 b p) [hh pp]

data Floor (t :: Type) p
type FloorT (t :: Type) p = Floor' (Hole t) p

instance P (FloorT t p) x => P (Floor t p) x where
  type PP (Floor t p) x = PP (FloorT t p) x
  eval _ = eval (Proxy @(FloorT t p))

data BinOp = BMult | BSub | BAdd deriving (Show,Eq)

data p + q
infixl 6 +

type AddT p q = Bin 'BAdd p q

instance P (AddT p q) x => P (p + q) x where
  type PP (p + q) x = PP (AddT p q) x
  eval _ = eval (Proxy @(AddT p q))

data p - q
infixl 6 -

type SubT p q = Bin 'BSub p q

instance P (SubT p q) x => P (p - q) x where
  type PP (p - q) x = PP (SubT p q) x
  eval _ = eval (Proxy @(SubT p q))

data p * q
infixl 7 *

type MultT p q = Bin 'BMult p q

instance P (MultT p q) x => P (p * q) x where
  type PP (p * q) x = PP (MultT p q) x
  eval _ = eval (Proxy @(MultT p q))

-- | similar to 'GHC.Real.(^)'
--
-- >>> pz @(Fst Id ^ Snd Id) (10,4)
-- PresentT 10000
--
data p ^ q
infixr 8 ^

instance (P p a
        , P q a
        , Show (PP p a)
        , Show (PP q a)
        , Num (PP p a)
        , Integral (PP q a)
        ) => P (p ^ q) a where
  type PP (p ^ q) a = PP p a
  eval _ opts a = do
    let msg0 = "Pow"
    pp <- eval (Proxy @p) opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @q) opts a
        pure $ case getValueLR opts msg0 qq [hh pp] of
          Left e -> e
          Right q ->
                let hhs = [hh pp, hh qq]
                in if q < 0 then mkNode opts (FailT (msg0 <> " negative exponent")) "" hhs
                   else let d = p ^ q
                        in mkNode opts (PresentT d) (showL opts p <> " ^ " <> showL opts q <> " = " <> showL opts d) hhs

-- | similar to 'GHC.Float.(**)'
--
-- >>> pz @(Fst Id ** Snd Id) (10,4)
-- PresentT 10000.0
--
-- >>> pz @'(Prime Id,Id ^ 3,(FromIntegral _ Id) ** (FromRational _ (1 % 2))) 4
-- PresentT (False,64,2.0)
--
data p ** q
infixr 8 **

instance (PP p a ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        , Floating (PP p a)
        , Ord (PP q a)
        ) => P (p ** q) a where
  type PP (p ** q) a = PP p a
  eval _ opts a = do
    let msg0 = "Exp"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
         let hhs = [hh pp, hh qq]
         in if q < 0 then mkNode opts (FailT (msg0 <> " negative exponent")) "" hhs
            else if p == 0 && q == 0 then mkNode opts (FailT (msg0 <> " zero/zero")) "" hhs
            else let d = p ** q
                in mkNode opts (PresentT d) (showL opts p <> " ** " <> showL opts q <> " = " <> showL opts d) hhs

-- | similar to 'logBase'
--
-- >>> pz @(Fst Id `LogBase` Snd Id >> Truncate Int Id) (10,12345)
-- PresentT 4
--
data LogBase p q
instance (PP p a ~ PP q a
        , P p a
        , P q a
        , Show (PP q a)
        , Floating (PP q a)
        , Ord (PP p a)
        ) => P (LogBase p q) a where
  type PP (LogBase p q) a = PP p a
  eval _ opts a = do
    let msg0 = "LogBase"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
         let hhs = [hh pp, hh qq]
         in if p <= 0 then mkNode opts (FailT (msg0 <> " non-positive base")) "" hhs
            else let d = logBase p q
                 in mkNode opts (PresentT d) (msg0 <> " " <> showL opts p <> " " <> showL opts q <> " = " <> showL opts d) hhs

class GetBinOp (k :: BinOp) where
  getBinOp :: (Num a, a ~ b) => (String, a -> b -> a)

instance GetBinOp 'BMult where
  getBinOp = ("*",(*))
instance GetBinOp 'BSub where
  getBinOp = ("-",(-))
instance GetBinOp 'BAdd where
  getBinOp = ("+",(+))

-- | addition, multiplication and subtraction
--
-- >>> pz @(Fst Id * Snd Id) (13,5)
-- PresentT 65
--
-- >>> pz @(Fst Id + 4 * Length (Snd Id) - 4) (3,"hello")
-- PresentT 19
--
data Bin (op :: BinOp) p q

instance (GetBinOp op
        , PP p a ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        , Num (PP p a)
        ) => P (Bin op p q) a where
  type PP (Bin op p q) a = PP p a
  eval _ opts a = do
    let (s,f) = getBinOp @op
    lr <- runPQ s (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p `f` q
        in mkNode opts (PresentT d) (showL opts p <> " " <> s <> " " <> showL opts q <> " = " <> showL opts d) [hh pp, hh qq]

-- | fractional division
--
-- >>> pz @(Fst Id / Snd Id) (13,2)
-- PresentT 6.5
--
-- >>> pz @(ToRational 13 / Id) 0
-- FailT "(/) zero denominator"
--
-- >>> pz @(12 % 7 / 14 % 5 + Id) 12.4
-- PresentT (3188 % 245)
--
data p / q
infixl 7 /

instance (PP p a ~ PP q a
        , Eq (PP q a)
        , P p a
        , P q a
        , Show (PP p a)
        , Fractional (PP p a)
        ) => P (p / q) a where
  type PP (p / q) a = PP p a
  eval _ opts a = do
    let msg0 = "(/)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq)
         | q == 0 -> let msg1 = msg0 <> " zero denominator"
                     in mkNode opts (FailT msg1) "" [hh pp, hh qq]
         | otherwise ->
            let d = p / q
            in mkNode opts (PresentT d) (showL opts p <> " / " <> showL opts q <> " = " <> showL opts d) [hh pp, hh qq]

-- | creates a 'Rational' value
--
-- >>> pz @(Id < 21 % 5) (-3.1)
-- TrueT
--
-- >>> pz @(Id < 21 % 5) 4.5
-- FalseT
--
-- >>> pz @(Fst Id % Snd Id) (13,2)
-- PresentT (13 % 2)
--
-- >>> pz @(13 % Id) 0
-- FailT "(%) zero denominator"
--
-- >>> pz @(4 % 3 + 5 % 7) "asfd"
-- PresentT (43 % 21)
--
-- >>> pz @(4 -% 7 * 5 -% 3) "asfd"
-- PresentT (20 % 21)
--
-- >>> pz @(Negate (14 % 3)) ()
-- PresentT ((-14) % 3)
--
-- >>> pz @(14 % 3) ()
-- PresentT (14 % 3)
--
-- >>> pz @(Negate (14 % 3) ==! FromIntegral _ (Negate 5)) ()
-- PresentT GT
--
-- >>> pz @(14 -% 3 ==! 5 -% 1) "aa"
-- PresentT GT
--
-- >>> pz @(Negate (14 % 3) ==! Negate 5 % 2) ()
-- PresentT LT
--
-- >>> pz @(14 -% 3 * 5 -% 1) ()
-- PresentT (70 % 3)
--
-- >>> pz @(14 % 3 ==! 5 % 1) ()
-- PresentT LT
--
-- >>> pz @(15 % 3 / 4 % 2) ()
-- PresentT (5 % 2)
--
data p % q
infixl 8 %

instance (Integral (PP p x)
        , Integral (PP q x)
        , Eq (PP q x)
        , P p x
        , P q x
        , Show (PP p x)
        , Show (PP q x)
        ) => P (p % q) x where
  type PP (p % q) x = Rational
  eval _ opts x = do
    let msg0 = "(%)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq)
         | q == 0 -> let msg1 = msg0 <> " zero denominator"
                     in mkNode opts (FailT msg1) "" [hh pp, hh qq]
         | otherwise ->
            let z@(p1,q1) = (fromIntegral p, fromIntegral q)
                d@(dn :% dd) = uncurry (%) z
                zz = if dn == p1 && dd == q1 then ""
                     else litVerbose opts " | " (show p <> " % " <> show q)
            in mkNode opts (PresentT d) (showL opts d <> zz) [hh pp, hh qq]

-- | negate a ratio
--
-- >>> pl @'[1 % 1 ,3 -% 2,3 -% 1] ()
-- Present [1 % 1,(-3) % 2,(-3) % 1] ('[1 % 1,(-3) % 2,(-3) % 1] (1 % 1) | ())
-- PresentT [1 % 1,(-3) % 2,(-3) % 1]
--
-- >>> pl @('[1 % 1 ,Negate (33 % 7), 21 % 4,Signum (7 -% 5)] >> Map (Floor _ Id) Id) ()
-- Present [1,-5,5,-1] ((>>) [1,-5,5,-1] | {Map [1,-5,5,-1] | [1 % 1,(-33) % 7,21 % 4,(-1) % 1]})
-- PresentT [1,-5,5,-1]
--
-- >>> pl @('[1 % 1 ,Negate (33 % 7), 21 % 4,Signum (7 -% 5)] >> Map (Ceiling _ Id) Id) ()
-- Present [1,-4,6,-1] ((>>) [1,-4,6,-1] | {Map [1,-4,6,-1] | [1 % 1,(-33) % 7,21 % 4,(-1) % 1]})
-- PresentT [1,-4,6,-1]
--
-- >>> pl @('[1 % 1 ,Negate (33 % 7), 21 % 4,Signum (7 -% 5)] >> Map (Truncate _ Id) Id) ()
-- Present [1,-4,5,-1] ((>>) [1,-4,5,-1] | {Map [1,-4,5,-1] | [1 % 1,(-33) % 7,21 % 4,(-1) % 1]})
-- PresentT [1,-4,5,-1]
--
-- >>> pl @(5 % 1 / 3 -% 1) 'x'
-- Present (-5) % 3 (5 % 1 / (-3) % 1 = (-5) % 3)
-- PresentT ((-5) % 3)
--
-- >>> pl @(5 -% 1 / Fst Id) (3,'x')
-- Present (-5) % 3 ((-5) % 1 / 3 % 1 = (-5) % 3)
-- PresentT ((-5) % 3)
--
data p -% q -- = Negate (p % q)
infixl 8 -%
type NegateRatioT p q = Negate (p % q)

instance P (NegateRatioT p q) x => P (p -% q) x where
  type PP (p -% q) x = PP (NegateRatioT p q) x
  eval _ = eval (Proxy @(NegateRatioT p q))


-- | similar to 'negate'
--
-- >>> pz @(Negate Id) 14
-- PresentT (-14)
--
-- >>> pz @(Negate (Fst Id * Snd Id)) (14,3)
-- PresentT (-42)
--
-- >>> pz @(Negate (15 -% 4)) "abc"
-- PresentT (15 % 4)
--
-- >>> pz @(Negate (15 % 3)) ()
-- PresentT ((-5) % 1)
--
-- >>> pz @(Negate (Fst Id % Snd Id)) (14,3)
-- PresentT ((-14) % 3)
--
data Negate p

instance ( Show (PP p x)
         , Num (PP p x)
         , P p x
         ) => P (Negate p) x where
  type PP (Negate p) x = PP p x
  eval _ opts x = do
    let msg0 = "Negate"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = negate p
        in mkNode opts (PresentT d) (show01 opts msg0 d p) [hh pp]


-- | similar to 'abs'
--
-- >>> pz @(Abs Id) (-14)
-- PresentT 14
--
-- >>> pz @(Abs (Snd Id)) ("xx",14)
-- PresentT 14
--
-- >>> pz @(Abs Id) 0
-- PresentT 0
--
-- >>> pz @(Abs (Negate 44)) "aaa"
-- PresentT 44
--
data Abs p

instance ( Show (PP p x)
         , Num (PP p x)
         , P p x
         ) => P (Abs p) x where
  type PP (Abs p) x = PP p x
  eval _ opts x = do
    let msg0 = "Abs"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = abs p
        in mkNode opts (PresentT d) (show01 opts msg0 d p) [hh pp]

-- | similar to 'div'
--
-- >>> pz @(Div (Fst Id) (Snd Id)) (10,4)
-- PresentT 2
--
-- >>> pz @(Div (Fst Id) (Snd Id)) (10,0)
-- FailT "Div zero denominator"
--
data Div p q
instance (PP p a ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        , Integral (PP p a)
        ) => P (Div p q) a where
  type PP (Div p q) a = PP p a
  eval _ opts a = do
    let msg0 = "Div"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
         let hhs = [hh pp, hh qq]
         in case q of
              0 -> mkNode opts (FailT (msg0 <> " zero denominator")) "" hhs
              _ -> let d = p `div` q
                   in mkNode opts (PresentT d) (showL opts p <> " `div` " <> showL opts q <> " = " <> showL opts d) hhs


-- | similar to 'GHC.Real.mod'
--
-- >>> pz @(Mod (Fst Id) (Snd Id)) (10,3)
-- PresentT 1
--
-- >>> pz @(Mod (Fst Id) (Snd Id)) (10,0)
-- FailT "Mod zero denominator"
--
data Mod p q
instance (PP p a ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        , Integral (PP p a)
        ) => P (Mod p q) a where
  type PP (Mod p q) a = PP p a
  eval _ opts a = do
    let msg0 = "Mod"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
         let hhs = [hh pp, hh qq]
         in case q of
              0 -> mkNode opts (FailT (msg0 <> " zero denominator")) "" hhs
              _ -> let d = p `mod` q
                   in mkNode opts (PresentT d) (showL opts p <> " `mod` " <> showL opts q <> " = " <> showL opts d) hhs

-- | similar to 'divMod'
--
-- >>> pz @(DivMod (Fst Id) (Snd Id)) (10,3)
-- PresentT (3,1)
--
-- >>> pz @(DivMod (Fst Id) (Snd Id)) (10,-3)
-- PresentT (-4,-2)
--
-- >>> pz @(DivMod (Fst Id) (Snd Id)) (-10,3)
-- PresentT (-4,2)
--
-- >>> pz @(DivMod (Fst Id) (Snd Id)) (-10,-3)
-- PresentT (3,-1)
--
-- >>> pz @(DivMod (Fst Id) (Snd Id)) (10,0)
-- FailT "DivMod zero denominator"
--
-- >>> pl @(DivMod (Negate Id) 7) 23
-- Present (-4,5) (-23 `divMod` 7 = (-4,5))
-- PresentT (-4,5)
--
-- >>> pl @(DivMod (Fst Id) (Snd Id)) (10,-3)
-- Present (-4,-2) (10 `divMod` -3 = (-4,-2))
-- PresentT (-4,-2)
--
-- >>> pl @(DivMod (Fst Id) (Snd Id)) (10,0)
-- Error DivMod zero denominator
-- FailT "DivMod zero denominator"
--
-- >>> pl @(DivMod (9 - Fst Id) (Last (Snd Id))) (10,[12,13])
-- Present (-1,12) (-1 `divMod` 13 = (-1,12))
-- PresentT (-1,12)
--

data DivMod p q

instance (PP p a ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        , Integral (PP p a)
        ) => P (DivMod p q) a where
  type PP (DivMod p q) a = (PP p a, PP p a)
  eval _ opts a = do
    let msg0 = "DivMod"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case q of
             0 -> mkNode opts (FailT (msg0 <> " zero denominator")) "" hhs
             _ -> let d = p `divMod` q
                  in mkNode opts (PresentT d) (showL opts p <> " `divMod` " <> showL opts q <> " = " <> showL opts d) hhs

-- | similar to 'quotRem'
--
-- >>> pz @(QuotRem (Fst Id) (Snd Id)) (10,3)
-- PresentT (3,1)
--
-- >>> pz @(QuotRem (Fst Id) (Snd Id)) (10,-3)
-- PresentT (-3,1)
--
-- >>> pz @(QuotRem (Fst Id) (Snd Id)) (-10,-3)
-- PresentT (3,-1)
--
-- >>> pz @(QuotRem (Fst Id) (Snd Id)) (-10,3)
-- PresentT (-3,-1)
--
-- >>> pz @(QuotRem (Fst Id) (Snd Id)) (10,0)
-- FailT "QuotRem zero denominator"
--
-- >>> pl @(QuotRem (Negate Id) 7) 23
-- Present (-3,-2) (-23 `quotRem` 7 = (-3,-2))
-- PresentT (-3,-2)
--
-- >>> pl @(QuotRem (Fst Id) (Snd Id)) (10,-3)
-- Present (-3,1) (10 `quotRem` -3 = (-3,1))
-- PresentT (-3,1)
--

data QuotRem p q

instance (PP p a ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        , Integral (PP p a)
        ) => P (QuotRem p q) a where
  type PP (QuotRem p q) a = (PP p a, PP p a)
  eval _ opts a = do
    let msg0 = "QuotRem"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case q of
             0 -> mkNode opts (FailT (msg0 <> " zero denominator")) "" hhs
             _ -> let d = p `quotRem` q
                  in mkNode opts (PresentT d) (showL opts p <> " `quotRem` " <> showL opts q <> " = " <> showL opts d) hhs

data Quot p q
type QuotT p q = Fst (QuotRem p q)

instance P (QuotT p q) x => P (Quot p q) x where
  type PP (Quot p q) x = PP (QuotT p q) x
  eval _ = eval (Proxy @(QuotT p q))

data Rem p q
type RemT p q = Snd (QuotRem p q)

instance P (RemT p q) x => P (Rem p q) x where
  type PP (Rem p q) x = PP (RemT p q) x
  eval _ = eval (Proxy @(RemT p q))

-- | similar to 'even'
--
-- >>> pz @(Map Even Id) [9,-4,12,1,2,3]
-- PresentT [False,True,True,False,True,False]
--
-- >>> pz @(Map '(Even,Odd) Id) [9,-4,12,1,2,3]
-- PresentT [(False,True),(True,False),(True,False),(False,True),(True,False),(False,True)]
--
data Even
type EvenT = Mod I 2 == 0

instance P EvenT x => P Even x where
  type PP Even x = Bool
  eval _ = evalBool (Proxy @EvenT)

data Odd
type OddT = Mod I 2 == 1

instance P OddT x => P Odd x where
  type PP Odd x = Bool
  eval _ = evalBool (Proxy @OddT)

-- | similar to 'signum'
--
-- >>> pz @(Signum Id) (-14)
-- PresentT (-1)
--
-- >>> pz @(Signum Id) 14
-- PresentT 1
--
-- >>> pz @(Signum Id) 0
-- PresentT 0
--
data Signum p

instance ( Show (PP p x)
         , Num (PP p x)
         , P p x
         ) => P (Signum p) x where
  type PP (Signum p) x = PP p x
  eval _ opts x = do
    let msg0 = "Signum"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = signum p
        in mkNode opts (PresentT d) (show01 opts msg0 d p) [hh pp]

-- supports negative numbers unlike readInt
data ReadBase' t (n :: Nat) p

instance (Typeable (PP t x)
        , ZwischenT 2 36 n
        , Show (PP t x)
        , Num (PP t x)
        , KnownNat n
        , PP p x ~ String
        , P p x
        ) => P (ReadBase' t n p) x where
  type PP (ReadBase' t n p) x = PP t x
  eval _ opts x = do
    let n = nat @n
        xs = getValidBase n
        msg0 = "ReadBase(" <> t <> "," <> show n <> ")"
        t = showT @(PP t x)
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let (ff,p1) = case p of
                        '-':q -> (negate,q)
                        _ -> (id,p)
        in case Numeric.readInt (fromIntegral n)
            ((`elem` xs) . toLower)
            (fromJust . (`elemIndex` xs) . toLower)
            p1 of
             [(b,"")] -> mkNode opts (PresentT (ff b)) (msg0 <> " " <> showL opts (ff b) <> showVerbose opts " | " p) [hh pp]
             o -> mkNode opts (FailT ("invalid base " <> show n)) (msg0 <> " as=" <> p <> " err=" <> showL opts o) [hh pp]

-- | Read a number using base 2 through a maximum of 36
--
-- >>> pz @(ReadBase Int 16 Id) "00feD"
-- PresentT 4077
--
-- >>> pz @(ReadBase Int 16 Id) "-ff"
-- PresentT (-255)
--
-- >>> pz @(ReadBase Int 2 Id) "10010011"
-- PresentT 147
--
-- >>> pz @(ReadBase Int 8 Id) "Abff"
-- FailT "invalid base 8"
--
-- >>> pl @(ReadBase Int 16 Id >> GuardSimple (Id > 0xffff) >> ShowBase 16 Id) "12344"
-- Present "12344" ((>>) "12344" | {ShowBase(16) 12344 | 74564})
-- PresentT "12344"
--
-- >>> :set -XBinaryLiterals
-- >>> pz @(ReadBase Int 16 Id >> GuardSimple (Id > 0b10011111) >> ShowBase 16 Id) "7f"
-- FailT "(127 > 159)"
--
-- >>> pl @(ReadBase Int 16 Id) "fFe0"
-- Present 65504 (ReadBase(Int,16) 65504 | "fFe0")
-- PresentT 65504
--
-- >>> pl @(ReadBase Int 16 Id) "-ff"
-- Present -255 (ReadBase(Int,16) -255 | "-ff")
-- PresentT (-255)
--
-- >>> pl @(ReadBase Int 16 Id) "ff"
-- Present 255 (ReadBase(Int,16) 255 | "ff")
-- PresentT 255
--
-- >>> pl @(ReadBase Int 22 Id) "zzz"
-- Error invalid base 22 (ReadBase(Int,22) as=zzz err=[])
-- FailT "invalid base 22"
--
-- >>> pl @((ReadBase Int 16 Id &&& Id) >> First (ShowBase 16 Id)) "fFe0"
-- Present ("ffe0","fFe0") ((>>) ("ffe0","fFe0") | {(***) ("ffe0","fFe0") | (65504,"fFe0")})
-- PresentT ("ffe0","fFe0")
--
-- >>> pl @(ReadBase Int 2 Id) "101111"
-- Present 47 (ReadBase(Int,2) 47 | "101111")
-- PresentT 47
--
data ReadBase (t :: Type) (n :: Nat) p
type ReadBaseT (t :: Type) (n :: Nat) p = ReadBase' (Hole t) n p

instance P (ReadBaseT t n p) x => P (ReadBase t n p) x where
  type PP (ReadBase t n p) x = PP (ReadBaseT t n p) x
  eval _ = eval (Proxy @(ReadBaseT t n p))

getValidBase :: Int -> String
getValidBase n =
  let xs = ['0'..'9'] <> ['a'..'z']
      len = length xs
  in if n > len || n < 2 then errorInProgram $ "getValidBase: oops invalid base valid is 2 thru " ++ show len ++ " found " ++ show n
     else take n xs

-- | Display a number at base 2 to 36, similar to 'Numeric.showIntAtBase' but supports signed numbers
--
-- >>> pz @(ShowBase 16 Id) 4077
-- PresentT "fed"
--
-- >>> pz @(ShowBase 16 Id) (-255)
-- PresentT "-ff"
--
-- >>> pz @(ShowBase 2 Id) 147
-- PresentT "10010011"
--
-- >>> pz @(ShowBase 2 (Negate 147)) "whatever"
-- PresentT "-10010011"
--
-- >>> pl @(ShowBase 16 Id) (-123)
-- Present "-7b" (ShowBase(16) -7b | -123)
-- PresentT "-7b"
--
-- >>> pl @(ShowBase 16 Id) 123
-- Present "7b" (ShowBase(16) 7b | 123)
-- PresentT "7b"
--
-- >>> pl @(ShowBase 16 Id) 65504
-- Present "ffe0" (ShowBase(16) ffe0 | 65504)
-- PresentT "ffe0"
--

data ShowBase (n :: Nat) p

instance (PP p x ~ a
        , P p x
        , Show a
        , 2 GL.<= n
        , n GL.<= 36
        , KnownNat n
        , Integral a
        ) => P (ShowBase n p) x where
  type PP (ShowBase n p) x = String
  eval _ opts x = do
    let n = nat @n
        xs = getValidBase n
        msg0 = "ShowBase(" <> show n <> ")"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let (ff,a') = if p < 0 then (('-':), abs p) else (id,p)
            b = Numeric.showIntAtBase (fromIntegral n) (xs !!) a' ""
        in mkNode opts (PresentT (ff b)) (msg0 <> " " <> litL opts (ff b) <> showVerbose opts " | " p) [hh pp]

-- | calculate the amount to roundup to next n
--
-- >>> pl @(RoundUp (Fst Id) (Snd Id)) (3,9)
-- Present 0 (RoundUp 3 `mod` 3 = 0)
-- PresentT 0
--
-- >>> pl @(RoundUp (Fst Id) (Snd Id)) (3,10)
-- Present 2 (RoundUp 2 `mod` 3 = 2)
-- PresentT 2
--
-- >>> pl @(RoundUp (Fst Id) (Snd Id)) (3,11)
-- Present 1 (RoundUp 1 `mod` 3 = 1)
-- PresentT 1
--
-- >>> pl @(RoundUp (Fst Id) (Snd Id)) (3,12)
-- Present 0 (RoundUp 3 `mod` 3 = 0)
-- PresentT 0
--
-- >>> pl @(RoundUp 3 0) ()
-- Present 0 (RoundUp 3 `mod` 3 = 0)
-- PresentT 0
--
-- >>> pl @(RoundUp 0 10) ()
-- Error Mod zero denominator (RoundUp Mod)
-- FailT "Mod zero denominator"
--
data RoundUp n p
type RoundUpT n p = (n - p `Mod` n) `Mod` n

instance P (RoundUpT n p) x => P (RoundUp n p) x where
  type PP (RoundUp n p) x = PP (RoundUpT n p) x
  eval _ opts =
    opts & if isVerbose opts
           then eval (Proxy @(MsgI "RoundUp " (RoundUpT n p)))
           else eval (Proxy @(MsgI "RoundUp " (Hide (RoundUpT n p))))

data RoundUpX n p

instance P (RoundUpT n p) x
      => P (RoundUpX n p) x where
  type PP (RoundUpX n p) x = PP (RoundUpT n p) x
  eval _ opts x = do
    let p = Proxy @(Apply1 (MsgI "RoundUpX "))
    let q = Proxy @(RoundUpT n p)
    let q1 = Proxy @(Hide (RoundUpT n p))
    if isVerbose opts
    then eval p opts (q,x)
    else eval p opts (q1,x)
{-
data RoundUpY n p

instance P (RoundUpT n p) x
      => P (RoundUpY n p) x where
  type PP (RoundUpY n p) x = PP (RoundUpT n p) x
  eval _ opts x = do
    let z :: Proxy Apply1X
        z = Proxy
    let p :: Proxy (MsgI "RoundUpY " :: Type -> Type) -- absolutely need kind signatur
        p = Proxy
    let q :: Proxy (RoundUpT n p)
        q = Proxy
    let q1 :: Proxy (Hide (RoundUpT n p))
        q1 = Proxy
    if isVerbose opts
    then eval z opts ((p,q),x)
    else eval z opts ((p,q1),x)
-}
{- need to explicitly set kind signature for first proxy p
C:\\work\predicate-typed\src\Predicate\Data\Numeric.hs:1133:10-45: error:
    • Could not deduce (P Apply1X
                          ((Proxy (MsgI "RoundUpY "), Proxy (RoundUpT n p)), x))
        arising from a use of ‘eval’
      from the context: P (RoundUpT n p) x
        bound by the instance declaration
        at C:\\work\predicate-typed\src\Predicate\Data\Numeric.hs:(1124,10)-(1125,27)
      or from: MonadEval m
        bound by the type signature for:
                   eval :: forall (m :: * -> *) (proxy :: * -> *).
                           MonadEval m =>
                           proxy (RoundUpY n p) -> POpts -> x -> m (TT (PP (RoundUpY n
p) x))
        at C:\\work\predicate-typed\src\Predicate\Data\Numeric.hs:1127:3-6
      The type variable ‘k0’ is ambiguous
      Relevant bindings include
        q1 :: Proxy (Hide (RoundUpT n p))
          (bound at C:\\work\predicate-typed\src\Predicate\Data\Numeric.hs:1131:9)
        q :: Proxy (RoundUpT n p)
          (bound at C:\\work\predicate-typed\src\Predicate\Data\Numeric.hs:1130:9)
        x :: x
          (bound at C:\\work\predicate-typed\src\Predicate\Data\Numeric.hs:1127:15)
        eval :: proxy (RoundUpY n p)
                -> POpts -> x -> m (TT (PP (RoundUpY n p) x))
          (bound at C:\\work\predicate-typed\src\Predicate\Data\Numeric.hs:1127:3)
      These potential instance exist:
        one instance involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In a stmt of a 'do' block:
        z <- eval (Proxy @Apply1X) opts ((p, q), x)
      In the expression:
        do let z = Proxy @Apply1X
           let p = Proxy @(MsgI "RoundUpY ")
           let q = Proxy @(RoundUpT n p)
           let q1 = Proxy @(Hide (RoundUpT n p))
           ....
      In an equation for ‘eval’:
-}