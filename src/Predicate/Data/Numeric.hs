{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
-- |     promoted numeric functions
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
  , ShowBaseN
  , UnShowBaseN
  , Bits

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Predicate.Data.Ordering (type (==))
import GHC.TypeLits (Nat,KnownNat)
import qualified GHC.TypeLits as GL
import Data.List (elemIndex, unfoldr)
import Data.Typeable (Typeable, Proxy(Proxy))
import Data.Kind (Type)
import qualified Numeric
import Data.Char (toLower)
import Data.Ratio ((%))
import GHC.Real (Ratio((:%)))
import qualified Safe (fromJustNote)
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG
-- >>> import Data.Time

data FromInteger' t n deriving Show

instance ( Num (PP t a)
         , Integral (PP n a)
         , P n a
         , Show (PP t a)
         ) => P (FromInteger' t n) a where
  type PP (FromInteger' t n) a = PP t a
  eval _ opts a = do
    let msg0 = "FromInteger"
    nn <- eval (Proxy @n) opts a
    pure $ case getValueLR NoInline opts msg0 nn [] of
      Left e -> e
      Right n ->
        let b = fromInteger (fromIntegral n)
        in mkNode opts (Val b) (msg0 <> " " <> showL opts b) [hh nn]

-- | 'fromInteger' function where you need to provide the type @t@ of the result
--
-- >>> pz @(FromInteger (SG.Sum _)) 23
-- Val (Sum {getSum = 23})
--
-- >>> pz @(44 >> FromInteger Rational) 12
-- Val (44 % 1)
--
-- >>> pz @(FromInteger Rational) 12
-- Val (12 % 1)
--
-- >>> pl @((Lift (FromInteger _) 12 &&& Id) >> Fst + Snd) (SG.Min 7)
-- Present Min {getMin = 19} ((>>) Min {getMin = 19} | {getMin = 19})
-- Val (Min {getMin = 19})
--
-- >>> pl @(Lift (FromInteger _) 12 <> Id) (SG.Product 7)
-- Present Product {getProduct = 84} (Product {getProduct = 12} <> Product {getProduct = 7} = Product {getProduct = 84})
-- Val (Product {getProduct = 84})
--
-- >>> pl @(Fst >> FromInteger (SG.Sum _)) (3,"A")
-- Present Sum {getSum = 3} ((>>) Sum {getSum = 3} | {getSum = 3})
-- Val (Sum {getSum = 3})
--
-- >>> pl @(Lift (FromInteger DiffTime) 123) 'x'
-- Present 123s ((>>) 123s | {FromInteger 123s})
-- Val 123s
--
data FromInteger (t :: Type) deriving Show
type FromIntegerT (t :: Type) = FromInteger' (Hole t) Id
--type FromIntegerP n = FromInteger' Unproxy n

instance P (FromIntegerT t) x => P (FromInteger t) x where
  type PP (FromInteger t) x = PP (FromIntegerT t) x
  eval _ = eval (Proxy @(FromIntegerT t))

-- | 'fromIntegral' function where you need to provide the type @t@ of the result
--
-- >>> pz @(FromIntegral (SG.Sum _)) 23
-- Val (Sum {getSum = 23})
data FromIntegral' t n deriving Show

instance ( Num (PP t a)
         , Integral (PP n a)
         , P n a
         , Show (PP t a)
         , Show (PP n a)
         ) => P (FromIntegral' t n) a where
  type PP (FromIntegral' t n) a = PP t a
  eval _ opts a = do
    let msg0 = "FromIntegral"
    nn <- eval (Proxy @n) opts a
    pure $ case getValueLR NoInline opts msg0 nn [] of
      Left e -> e
      Right n ->
        let b = fromIntegral n
        in mkNode opts (Val b) (show3 opts msg0 b n) [hh nn]

data FromIntegral (t :: Type) deriving Show
type FromIntegralT (t :: Type) = FromIntegral' (Hole t) Id

instance P (FromIntegralT t) x => P (FromIntegral t) x where
  type PP (FromIntegral t) x = PP (FromIntegralT t) x
  eval _ = eval (Proxy @(FromIntegralT t))

-- | 'toRational' function
--
-- >>> pz @(ToRational Id) 23.5
-- Val (47 % 2)
--
-- >>> pl @((ToRational 123 &&& Id) >> Fst + Snd) 4.2
-- Present 636 % 5 ((>>) 636 % 5 | {123 % 1 + 21 % 5 = 636 % 5})
-- Val (636 % 5)
--
-- >>> pl @(Fst >= Snd || Snd > 23 || 12 -% 5 <= ToRational Fst) (12,13)
-- True (False || True)
-- Val True
--
-- >>> pl @(ToRational 14) ()
-- Present 14 % 1 (ToRational 14 % 1 | 14)
-- Val (14 % 1)
--
-- >>> pl @(ToRational 5 / ToRational 3) 'x'
-- Present 5 % 3 (5 % 1 / 3 % 1 = 5 % 3)
-- Val (5 % 3)
--

data ToRational p deriving Show

instance ( a ~ PP p x
         , Show a
         , Real a
         , P p x
         )
   => P (ToRational p) x where
  type PP (ToRational p) x = Rational
  eval _ opts x = do
    let msg0 = "ToRational"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right a ->
        let r = toRational a
        in mkNode opts (Val r) (show3 opts msg0 r a) [hh pp]

-- | 'fromRational' function where you need to provide the type @t@ of the result
--
-- >>> pl @(FromRational' Fst Snd) (1,2 % 5)
-- Present 0.4 (FromRational 0.4 | 2 % 5)
-- Val 0.4
--
data FromRational' t p deriving Show

instance ( P p a
         , PP p a ~ Rational
         , Show (PP t a)
         , Fractional (PP t a)
         ) => P (FromRational' t p) a where
  type PP (FromRational' t p) a = PP t a
  eval _ opts a = do
    let msg0 = "FromRational"
    pp <- eval (Proxy @p) opts a
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = fromRational @(PP t a) p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

-- | 'fromRational' function where you need to provide the type @t@ of the result
--
-- >>> pz @(FromRational Rational) 23.5
-- Val (47 % 2)
--
-- >>> pl @(FromRational Float) (4 % 5)
-- Present 0.8 (FromRational 0.8 | 4 % 5)
-- Val 0.8
--
data FromRational (t :: Type) deriving Show
type FromRationalT (t :: Type) = FromRational' (Hole t) Id

instance P (FromRationalT t) x => P (FromRational t) x where
  type PP (FromRational t) x = PP (FromRationalT t) x
  eval _ = eval (Proxy @(FromRationalT t))

-- | 'truncate' function where you need to provide the type @t@ of the result
--
-- >>> pl @(Truncate' (Fst >> Unproxy) Snd) (Proxy @Integer,2.3)
-- Present 2 (Truncate 2 | 2.3)
-- Val 2
--
-- >>> pl @(Truncate' Fst Snd) (1::Int,2.3)
-- Present 2 (Truncate 2 | 2.3)
-- Val 2
--
data Truncate' t p deriving Show

instance ( P p x
         , RealFrac (PP p x)
         , Integral (PP t x)
         , Show (PP t x)
         , Show (PP p x)
         ) => P (Truncate' t p) x where
  type PP (Truncate' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "Truncate"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = truncate p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

-- | 'truncate' function where you need to provide the type @t@ of the result
--
-- >>> pz @(Truncate Int) (23 % 5)
-- Val 4
--
data Truncate (t :: Type) deriving Show
type TruncateT (t :: Type) = Truncate' (Hole t) Id

instance P (TruncateT t) x => P (Truncate t) x where
  type PP (Truncate t) x = PP (TruncateT t) x
  eval _ = eval (Proxy @(TruncateT t))

-- | 'ceiling' function where you need to provide the type @t@ of the result
data Ceiling' t p deriving Show

instance ( P p x
         , RealFrac (PP p x)
         , Integral (PP t x)
         , Show (PP t x)
         , Show (PP p x)
         ) => P (Ceiling' t p) x where
  type PP (Ceiling' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "Ceiling"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = ceiling p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

-- | 'ceiling' function where you need to provide the type @t@ of the result
--
-- >>> pz @(Ceiling Int) (23 % 5)
-- Val 5
--
data Ceiling (t :: Type) deriving Show
type CeilingT (t :: Type) = Ceiling' (Hole t) Id

instance P (CeilingT t) x => P (Ceiling t) x where
  type PP (Ceiling t) x = PP (CeilingT t) x
  eval _ = eval (Proxy @(CeilingT t))

-- | 'floor' function where you need to provide the type @t@ of the result
data Floor' t p deriving Show

instance ( P p x
         , RealFrac (PP p x)
         , Integral (PP t x)
         , Show (PP t x)
         , Show (PP p x)
         ) => P (Floor' t p) x where
  type PP (Floor' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "Floor"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = floor p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

-- | 'floor' function where you need to provide the type @t@ of the result
--
-- >>> pz @(Floor Int) (23 % 5)
-- Val 4
--
data Floor (t :: Type) deriving Show
type FloorT (t :: Type) = Floor' (Hole t) Id

instance P (FloorT t) x => P (Floor t) x where
  type PP (Floor t) x = PP (FloorT t) x
  eval _ = eval (Proxy @(FloorT t))

data BinOp = BMult | BSub | BAdd
  deriving stock (Read, Show, Eq)

data p + q deriving Show
infixl 6 +

type AddT p q = Bin 'BAdd p q

instance P (AddT p q) x => P (p + q) x where
  type PP (p + q) x = PP (AddT p q) x
  eval _ = eval (Proxy @(AddT p q))

data p - q deriving Show
infixl 6 -

type SubT p q = Bin 'BSub p q

instance P (SubT p q) x => P (p - q) x where
  type PP (p - q) x = PP (SubT p q) x
  eval _ = eval (Proxy @(SubT p q))

data p * q deriving Show
infixl 7 *

type MultT p q = Bin 'BMult p q

instance P (MultT p q) x => P (p * q) x where
  type PP (p * q) x = PP (MultT p q) x
  eval _ = eval (Proxy @(MultT p q))

-- | similar to 'GHC.Real.(^)'
--
-- >>> pz @(Fst ^ Snd) (10,4)
-- Val 10000
--
data p ^ q deriving Show
infixr 8 ^

instance ( P p a
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
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @q) opts a
        pure $ case getValueLR NoInline opts msg0 qq [hh pp] of
          Left e -> e
          Right q ->
                let hhs = [hh pp, hh qq]
                in if q < 0 then mkNode opts (Fail (msg0 <> " negative exponent")) "" hhs
                   else let d = p ^ q
                        in mkNode opts (Val d) (showL opts p <> " ^ " <> showL opts q <> " = " <> showL opts d) hhs

-- | similar to 'GHC.Float.(**)'
--
-- >>> pz @(Fst ** Snd) (10,4)
-- Val 10000.0
--
-- >>> pz @'(IsPrime,Id ^ 3,(FromIntegral _) ** (Lift (FromRational _) (1 % 2))) 4
-- Val (False,64,2.0)
--
data p ** q deriving Show
infixr 8 **

instance ( PP p a ~ PP q a
         , P p a
         , P q a
         , Show (PP p a)
         , Floating (PP p a)
         , Ord (PP q a)
         ) => P (p ** q) a where
  type PP (p ** q) a = PP p a
  eval _ opts a = do
    let msg0 = "Exp"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
         let hhs = [hh pp, hh qq]
         in if q < 0 then mkNode opts (Fail (msg0 <> " negative exponent")) "" hhs
            else if p == 0 && q == 0 then mkNode opts (Fail (msg0 <> " zero/zero")) "" hhs
            else let d = p ** q
                in mkNode opts (Val d) (showL opts p <> " ** " <> showL opts q <> " = " <> showL opts d) hhs

-- | similar to 'logBase'
--
-- >>> pz @(Fst `LogBase` Snd >> Truncate Int) (10,12345)
-- Val 4
--
data LogBase p q deriving Show
instance ( PP p a ~ PP q a
         , P p a
         , P q a
         , Show (PP q a)
         , Floating (PP q a)
         , Ord (PP p a)
         ) => P (LogBase p q) a where
  type PP (LogBase p q) a = PP p a
  eval _ opts a = do
    let msg0 = "LogBase"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
         let hhs = [hh pp, hh qq]
         in if p <= 0 then mkNode opts (Fail (msg0 <> " non-positive base")) "" hhs
            else let d = logBase p q
                 in mkNode opts (Val d) (msg0 <> " " <> showL opts p <> " " <> showL opts q <> " = " <> showL opts d) hhs

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
-- >>> pz @(Fst * Snd) (13,5)
-- Val 65
--
-- >>> pz @(Fst + 4 * Length Snd - 4) (3,"hello")
-- Val 19
--
data Bin (op :: BinOp) p q deriving Show

instance ( GetBinOp op
         , PP p a ~ PP q a
         , P p a
         , P q a
         , Show (PP p a)
         , Num (PP p a)
         ) => P (Bin op p q) a where
  type PP (Bin op p q) a = PP p a
  eval _ opts a = do
    let (s,f) = getBinOp @op
    lr <- runPQ NoInline s (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p `f` q
        in mkNode opts (Val d) (showL opts p <> " " <> s <> " " <> showL opts q <> " = " <> showL opts d) [hh pp, hh qq]

-- | fractional division
--
-- >>> pz @(Fst / Snd) (13,2)
-- Val 6.5
--
-- >>> pz @(ToRational 13 / Id) 0
-- Fail "(/) zero denominator"
--
-- >>> pz @(12 % 7 / 14 % 5 + Id) 12.4
-- Val (3188 % 245)
--
data p / q deriving Show
infixl 7 /

instance ( PP p a ~ PP q a
         , Eq (PP q a)
         , P p a
         , P q a
         , Show (PP p a)
         , Fractional (PP p a)
         ) => P (p / q) a where
  type PP (p / q) a = PP p a
  eval _ opts a = do
    let msg0 = "(/)"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq)
         | q == 0 -> let msg1 = msg0 <> " zero denominator"
                     in mkNode opts (Fail msg1) "" [hh pp, hh qq]
         | otherwise ->
            let d = p / q
            in mkNode opts (Val d) (showL opts p <> " / " <> showL opts q <> " = " <> showL opts d) [hh pp, hh qq]

-- | creates a 'Rational' value
--
-- >>> pz @(Id < 21 % 5) (-3.1)
-- Val True
--
-- >>> pz @(Id < 21 % 5) 4.5
-- Val False
--
-- >>> pz @(Fst % Snd) (13,2)
-- Val (13 % 2)
--
-- >>> pz @(13 % Id) 0
-- Fail "(%) zero denominator"
--
-- >>> pz @(4 % 3 + 5 % 7) "asfd"
-- Val (43 % 21)
--
-- >>> pz @(4 -% 7 * 5 -% 3) "asfd"
-- Val (20 % 21)
--
-- >>> pz @(Negate (14 % 3)) ()
-- Val ((-14) % 3)
--
-- >>> pz @(14 % 3) ()
-- Val (14 % 3)
--
-- >>> pz @(Negate (14 % 3) ==! Lift (FromIntegral _) (Negate 5)) ()
-- Val GT
--
-- >>> pz @(14 -% 3 ==! 5 -% 1) "aa"
-- Val GT
--
-- >>> pz @(Negate (14 % 3) ==! Negate 5 % 2) ()
-- Val LT
--
-- >>> pz @(14 -% 3 * 5 -% 1) ()
-- Val (70 % 3)
--
-- >>> pz @(14 % 3 ==! 5 % 1) ()
-- Val LT
--
-- >>> pz @(15 % 3 / 4 % 2) ()
-- Val (5 % 2)
--
data p % q deriving Show
infixl 8 %

instance ( Integral (PP p x)
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
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq)
         | q == 0 -> let msg1 = msg0 <> " zero denominator"
                     in mkNode opts (Fail msg1) "" [hh pp, hh qq]
         | otherwise ->
            let z@(p1,q1) = (fromIntegral p, fromIntegral q)
                d@(dn :% dd) = uncurry (%) z
                zz = if dn == p1 && dd == q1 then ""
                     else litVerbose opts " | " (show p <> " % " <> show q)
            in mkNode opts (Val d) (showL opts d <> zz) [hh pp, hh qq]

-- | negate a ratio
--
-- >>> pl @'[1 % 1 ,3 -% 2,3 -% 1] ()
-- Present [1 % 1,(-3) % 2,(-3) % 1] ('[1 % 1,(-3) % 2,(-3) % 1] (1 % 1) | ())
-- Val [1 % 1,(-3) % 2,(-3) % 1]
--
-- >>> pl @('[1 % 1 ,Negate (33 % 7), 21 % 4,Signum (7 -% 5)] >> Map (Floor _)) ()
-- Present [1,-5,5,-1] ((>>) [1,-5,5,-1] | {Map [1,-5,5,-1] | [1 % 1,(-33) % 7,21 % 4,(-1) % 1]})
-- Val [1,-5,5,-1]
--
-- >>> pl @('[1 % 1 ,Negate (33 % 7), 21 % 4,Signum (7 -% 5)] >> Map (Ceiling _)) ()
-- Present [1,-4,6,-1] ((>>) [1,-4,6,-1] | {Map [1,-4,6,-1] | [1 % 1,(-33) % 7,21 % 4,(-1) % 1]})
-- Val [1,-4,6,-1]
--
-- >>> pl @('[1 % 1 ,Negate (33 % 7), 21 % 4,Signum (7 -% 5)] >> Map (Truncate _)) ()
-- Present [1,-4,5,-1] ((>>) [1,-4,5,-1] | {Map [1,-4,5,-1] | [1 % 1,(-33) % 7,21 % 4,(-1) % 1]})
-- Val [1,-4,5,-1]
--
-- >>> pl @(5 % 1 / 3 -% 1) 'x'
-- Present (-5) % 3 (5 % 1 / (-3) % 1 = (-5) % 3)
-- Val ((-5) % 3)
--
-- >>> pl @(5 -% 1 / Fst) (3,'x')
-- Present (-5) % 3 ((-5) % 1 / 3 % 1 = (-5) % 3)
-- Val ((-5) % 3)
--
data p -% q deriving Show
infixl 8 -%
type NegateRatioT p q = Negate (p % q)

instance P (NegateRatioT p q) x => P (p -% q) x where
  type PP (p -% q) x = PP (NegateRatioT p q) x
  eval _ = eval (Proxy @(NegateRatioT p q))


-- | similar to 'negate'
--
-- >>> pz @(Negate Id) 14
-- Val (-14)
--
-- >>> pz @(Negate (Fst * Snd)) (14,3)
-- Val (-42)
--
-- >>> pz @(Negate (15 -% 4)) "abc"
-- Val (15 % 4)
--
-- >>> pz @(Negate (15 % 3)) ()
-- Val ((-5) % 1)
--
-- >>> pz @(Negate (Fst % Snd)) (14,3)
-- Val ((-14) % 3)
--
data Negate p deriving Show

instance ( Num (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (Negate p) x where
  type PP (Negate p) x = PP p x
  eval _ opts x = do
    let msg0 = "Negate"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = negate p
        in mkNode opts (Val d) (show3 opts msg0 d p) [hh pp]


-- | similar to 'abs'
--
-- >>> pz @(Abs Id) (-14)
-- Val 14
--
-- >>> pz @(Abs Snd) ("xx",14)
-- Val 14
--
-- >>> pz @(Abs Id) 0
-- Val 0
--
-- >>> pz @(Abs (Negate 44)) "aaa"
-- Val 44
--
data Abs p deriving Show

instance ( Num (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (Abs p) x where
  type PP (Abs p) x = PP p x
  eval _ opts x = do
    let msg0 = "Abs"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = abs p
        in mkNode opts (Val d) (show3 opts msg0 d p) [hh pp]

-- | similar to 'div'
--
-- >>> pz @(Div Fst Snd) (10,4)
-- Val 2
--
-- >>> pz @(Div Fst Snd) (10,0)
-- Fail "Div zero denominator"
--
data Div p q deriving Show
instance ( PP p a ~ PP q a
         , P p a
         , P q a
         , Show (PP p a)
         , Integral (PP p a)
         ) => P (Div p q) a where
  type PP (Div p q) a = PP p a
  eval _ opts a = do
    let msg0 = "Div"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
         let hhs = [hh pp, hh qq]
         in case q of
              0 -> mkNode opts (Fail (msg0 <> " zero denominator")) "" hhs
              _ -> let d = p `div` q
                   in mkNode opts (Val d) (showL opts p <> " `div` " <> showL opts q <> " = " <> showL opts d) hhs


-- | similar to 'GHC.Real.mod'
--
-- >>> pz @(Mod Fst Snd) (10,3)
-- Val 1
--
-- >>> pz @(Mod Fst Snd) (10,0)
-- Fail "Mod zero denominator"
--
data Mod p q deriving Show
instance ( PP p a ~ PP q a
         , P p a
         , P q a
         , Show (PP p a)
         , Integral (PP p a)
         ) => P (Mod p q) a where
  type PP (Mod p q) a = PP p a
  eval _ opts a = do
    let msg0 = "Mod"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
         let hhs = [hh pp, hh qq]
         in case q of
              0 -> mkNode opts (Fail (msg0 <> " zero denominator")) "" hhs
              _ -> let d = p `mod` q
                   in mkNode opts (Val d) (showL opts p <> " `mod` " <> showL opts q <> " = " <> showL opts d) hhs

-- | similar to 'divMod'
--
-- >>> pz @(DivMod Fst Snd) (10,3)
-- Val (3,1)
--
-- >>> pz @(DivMod Fst Snd) (10,-3)
-- Val (-4,-2)
--
-- >>> pz @(DivMod Fst Snd) (-10,3)
-- Val (-4,2)
--
-- >>> pz @(DivMod Fst Snd) (-10,-3)
-- Val (3,-1)
--
-- >>> pz @(DivMod Fst Snd) (10,0)
-- Fail "DivMod zero denominator"
--
-- >>> pl @(DivMod (Negate Id) 7) 23
-- Present (-4,5) (-23 `divMod` 7 = (-4,5))
-- Val (-4,5)
--
-- >>> pl @(DivMod Fst Snd) (10,-3)
-- Present (-4,-2) (10 `divMod` -3 = (-4,-2))
-- Val (-4,-2)
--
-- >>> pl @(DivMod Fst Snd) (10,0)
-- Error DivMod zero denominator
-- Fail "DivMod zero denominator"
--
-- >>> pl @(DivMod (9 - Fst) (Snd >> Last)) (10,[12,13])
-- Present (-1,12) (-1 `divMod` 13 = (-1,12))
-- Val (-1,12)
--

data DivMod p q deriving Show

instance ( PP p a ~ PP q a
         , P p a
         , P q a
         , Show (PP p a)
         , Integral (PP p a)
         ) => P (DivMod p q) a where
  type PP (DivMod p q) a = (PP p a, PP p a)
  eval _ opts a = do
    let msg0 = "DivMod"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case q of
             0 -> mkNode opts (Fail (msg0 <> " zero denominator")) "" hhs
             _ -> let d = p `divMod` q
                  in mkNode opts (Val d) (showL opts p <> " `divMod` " <> showL opts q <> " = " <> showL opts d) hhs

-- | similar to 'quotRem'
--
-- >>> pz @(QuotRem Fst Snd) (10,3)
-- Val (3,1)
--
-- >>> pz @(QuotRem Fst Snd) (10,-3)
-- Val (-3,1)
--
-- >>> pz @(QuotRem Fst Snd) (-10,-3)
-- Val (3,-1)
--
-- >>> pz @(QuotRem Fst Snd) (-10,3)
-- Val (-3,-1)
--
-- >>> pz @(QuotRem Fst Snd) (10,0)
-- Fail "QuotRem zero denominator"
--
-- >>> pl @(QuotRem (Negate Id) 7) 23
-- Present (-3,-2) (-23 `quotRem` 7 = (-3,-2))
-- Val (-3,-2)
--
-- >>> pl @(QuotRem Fst Snd) (10,-3)
-- Present (-3,1) (10 `quotRem` -3 = (-3,1))
-- Val (-3,1)
--

data QuotRem p q deriving Show

instance ( PP p a ~ PP q a
         , P p a
         , P q a
         , Show (PP p a)
         , Integral (PP p a)
         ) => P (QuotRem p q) a where
  type PP (QuotRem p q) a = (PP p a, PP p a)
  eval _ opts a = do
    let msg0 = "QuotRem"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case q of
             0 -> mkNode opts (Fail (msg0 <> " zero denominator")) "" hhs
             _ -> let d = p `quotRem` q
                  in mkNode opts (Val d) (showL opts p <> " `quotRem` " <> showL opts q <> " = " <> showL opts d) hhs

data Quot p q deriving Show
type QuotT p q = QuotRem p q >> Fst

instance P (QuotT p q) x => P (Quot p q) x where
  type PP (Quot p q) x = PP (QuotT p q) x
  eval _ = eval (Proxy @(QuotT p q))

data Rem p q deriving Show
type RemT p q = QuotRem p q >> Snd

instance P (RemT p q) x => P (Rem p q) x where
  type PP (Rem p q) x = PP (RemT p q) x
  eval _ = eval (Proxy @(RemT p q))

-- | similar to 'even'
--
-- >>> pz @(Map Even) [9,-4,12,1,2,3]
-- Val [False,True,True,False,True,False]
--
-- >>> pz @(Map '(Even,Odd)) [9,-4,12,1,2,3]
-- Val [(False,True),(True,False),(True,False),(False,True),(True,False),(False,True)]
--
data Even deriving Show
type EvenT = Mod Id 2 == 0

instance P EvenT x => P Even x where
  type PP Even x = Bool
  eval _ = evalBool (Proxy @EvenT)

data Odd deriving Show
type OddT = Mod Id 2 == 1

instance P OddT x => P Odd x where
  type PP Odd x = Bool
  eval _ = evalBool (Proxy @OddT)

-- | similar to 'signum'
--
-- >>> pz @(Signum Id) (-14)
-- Val (-1)
--
-- >>> pz @(Signum Id) 14
-- Val 1
--
-- >>> pz @(Signum Id) 0
-- Val 0
--
data Signum p deriving Show

instance ( Num (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (Signum p) x where
  type PP (Signum p) x = PP p x
  eval _ opts x = do
    let msg0 = "Signum"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = signum p
        in mkNode opts (Val d) (show3 opts msg0 d p) [hh pp]

-- supports negative numbers unlike readInt
data ReadBase' t (n :: Nat) p deriving Show

instance ( Typeable (PP t x)
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
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let (ff,p1) = case p of
                        '-':q -> (negate,q)
                        _ -> (id,p)
        in case Numeric.readInt (fromIntegral n)
            ((`elem` xs) . toLower)
            (Safe.fromJustNote "ReadBase" . (`elemIndex` xs) . toLower)
            p1 of
             [(b,"")] -> mkNode opts (Val (ff b)) (msg0 <> " " <> showL opts (ff b) <> showVerbose opts " | " p) [hh pp]
             o -> mkNode opts (Fail ("invalid base " <> show n)) (msg0 <> " as=" <> p <> " err=" <> showL opts o) [hh pp]

-- | Read a number using base 2 through a maximum of 36
--
-- >>> pz @(ReadBase Int 16) "00feD"
-- Val 4077
--
-- >>> pz @(ReadBase Int 16) "-ff"
-- Val (-255)
--
-- >>> pz @(ReadBase Int 2) "10010011"
-- Val 147
--
-- >>> pz @(ReadBase Int 8) "Abff"
-- Fail "invalid base 8"
--
-- >>> pl @(ReadBase Int 16 >> GuardSimple (Id > 0xffff) >> ShowBase 16) "12344"
-- Present "12344" ((>>) "12344" | {ShowBase(16) 12344 | 74564})
-- Val "12344"
--
-- >>> :set -XBinaryLiterals
-- >>> pz @(ReadBase Int 16 >> GuardSimple (Id > 0b10011111) >> ShowBase 16) "7f"
-- Fail "(127 > 159)"
--
-- >>> pl @(ReadBase Int 16) "fFe0"
-- Present 65504 (ReadBase(Int,16) 65504 | "fFe0")
-- Val 65504
--
-- >>> pl @(ReadBase Int 16) "-ff"
-- Present -255 (ReadBase(Int,16) -255 | "-ff")
-- Val (-255)
--
-- >>> pl @(ReadBase Int 16) "ff"
-- Present 255 (ReadBase(Int,16) 255 | "ff")
-- Val 255
--
-- >>> pl @(ReadBase Int 22) "zzz"
-- Error invalid base 22 (ReadBase(Int,22) as=zzz err=[])
-- Fail "invalid base 22"
--
-- >>> pl @((ReadBase Int 16 &&& Id) >> First (ShowBase 16)) "fFe0"
-- Present ("ffe0","fFe0") ((>>) ("ffe0","fFe0") | {(***) ("ffe0","fFe0") | (65504,"fFe0")})
-- Val ("ffe0","fFe0")
--
-- >>> pl @(ReadBase Int 2) "101111"
-- Present 47 (ReadBase(Int,2) 47 | "101111")
-- Val 47
--
data ReadBase (t :: Type) (n :: Nat) deriving Show
type ReadBaseT (t :: Type) (n :: Nat) = ReadBase' (Hole t) n Id

instance P (ReadBaseT t n) x => P (ReadBase t n) x where
  type PP (ReadBase t n) x = PP (ReadBaseT t n) x
  eval _ = eval (Proxy @(ReadBaseT t n))

getValidBase :: Int -> String
getValidBase n =
  let xs = ['0'..'9'] <> ['a'..'z']
      len = length xs
  in if n > len || n < 2
     then errorInProgram $ "getValidBase: oops invalid base valid is 2 thru " ++ show len ++ " found " ++ show n
     else take n xs

-- | Display a number at base 2 to 36, similar to 'Numeric.showIntAtBase' but passes the sign through
--
-- >>> pz @(ShowBase 16) 4077
-- Val "fed"
--
-- >>> pz @(ShowBase 16) (-255)
-- Val "-ff"
--
-- >>> pz @(ShowBase 2) 147
-- Val "10010011"
--
-- >>> pz @(Lift (ShowBase 2) (Negate 147)) "whatever"
-- Val "-10010011"
--
-- >>> pl @(ShowBase 16) (-123)
-- Present "-7b" (ShowBase(16) -7b | -123)
-- Val "-7b"
--
-- >>> pl @(ShowBase 16) 123
-- Present "7b" (ShowBase(16) 7b | 123)
-- Val "7b"
--
-- >>> pl @(ShowBase 16) 65504
-- Present "ffe0" (ShowBase(16) ffe0 | 65504)
-- Val "ffe0"
--

data ShowBase (n :: Nat) deriving Show

instance ( 2 GL.<= n
         , n GL.<= 36
         , KnownNat n
         , Integral x
         ) => P (ShowBase n) x where
  type PP (ShowBase n) x = String
  eval _ opts x =
    let n = nat @n
        xs = getValidBase n
        msg0 = "ShowBase(" <> show n <> ")"
        p :: Integer
        p = fromIntegral x
        (ff,a') = if p < 0 then (('-':), abs p) else (id,p)
        b = Numeric.showIntAtBase (fromIntegral n) (xs !!) a' ""
    in pure $ mkNode opts (Val (ff b)) (msg0 <> " " <> litL opts (ff b) <> showVerbose opts " | " p) []

-- | Display a number at base >= 2 but just show as a list of ints: ignores the sign
--
-- >>> pl @(ShowBaseN 16 Id) (256*256*2+256*14+16*7+11)
-- Present [2,0,14,7,11] (ShowBaseN | 16 | 134779)
-- Val [2,0,14,7,11]
--
data ShowBaseN n p deriving Show

instance ( PP p x ~ a
         , P p x
         , PP n x ~ b
         , P n x
         , Integral a
         , Integral b
         ) => P (ShowBaseN n p) x where
  type PP (ShowBaseN n p) x = [Int]
  eval _ opts x = do
    let msg0 = "ShowBaseN"
    lr <- runPQ NoInline msg0 (Proxy @n) (Proxy @p) opts x []
    pure $ case lr of
      Left e -> e
      Right (fromIntegral -> n,fromIntegral -> p,nn,pp) ->
         let hhs = [hh nn, hh pp]
         in if n < 2 then mkNode opts (Fail (msg0 <> " base must be greater than 1")) "" hhs
            else let xs = reverse $ unfoldr (\s -> if s<1 then Nothing else Just (swapC (divMod s n))) (abs p)
                 in mkNode opts (Val xs) (msg0 <> showVerbose opts " | " n <> showVerbose opts " | " p) hhs

-- | convert to bits
--
-- >>> pl @(Bits 123 >> UnShowBaseN 2) ()
-- Present 123 ((>>) 123 | {UnShowBaseN | 2 | [1,1,1,1,0,1,1]})
-- Val 123
--
data Bits p deriving Show
type BitsT p = ShowBaseN 2 p

instance P (BitsT p) x => P (Bits p) x where
  type PP (Bits p) x = PP (BitsT p) x
  eval _ = eval (Proxy @(BitsT p))


-- | reverse 'ShowBaseN'
--
-- >>> pz @(UnShowBaseN 2) [1,0,0,1,0]
-- Val 18
--
-- >>> pz @(UnShowBaseN 2) [1,1,1]
-- Val 7
--
-- >>> pz @(UnShowBaseN 16) [7,0,3,1]
-- Val 28721
--
-- >>> pz @(UnShowBaseN 16) [0]
-- Val 0
--
-- >>> pz @(UnShowBaseN 16) []
-- Val 0
--
data UnShowBaseN n deriving Show

instance ( x ~ [a]
         , PP n x ~ b
         , P n x
         , Integral a
         , Integral b
         ) => P (UnShowBaseN n) x where
  type PP (UnShowBaseN n) x = Integer
  eval _ opts x = do
    let msg0 = "UnShowBaseN"
    nn <- eval (Proxy @n) opts x
    pure $ case getValueLR NoInline opts msg0 nn [] of
      Left e -> e
      Right (fromIntegral -> n) ->
         let xs = map fromIntegral x
             hhs = [hh nn]
         in if n < 2 then mkNode opts (Fail (msg0 <> " base must be greater than 1")) "" hhs
            else let b = snd $ foldr (\a (m,tot) -> (m*n, a*m+tot)) (1,0) xs
                 in mkNode opts (Val b) (msg0 <> showVerbose opts " | " n <> showVerbose opts " | " xs) hhs


-- | calculate the amount to roundup to next n
--
-- >>> pl @(RoundUp Fst Snd) (3,9)
-- Present 0 (RoundUp 3 `mod` 3 = 0)
-- Val 0
--
-- >>> pl @(RoundUp Fst Snd) (3,10)
-- Present 2 (RoundUp 2 `mod` 3 = 2)
-- Val 2
--
-- >>> pl @(RoundUp Fst Snd) (3,11)
-- Present 1 (RoundUp 1 `mod` 3 = 1)
-- Val 1
--
-- >>> pl @(RoundUp Fst Snd) (3,12)
-- Present 0 (RoundUp 3 `mod` 3 = 0)
-- Val 0
--
-- >>> pl @(RoundUp 3 0) ()
-- Present 0 (RoundUp 3 `mod` 3 = 0)
-- Val 0
--
-- >>> pl @(RoundUp 0 10) ()
-- Error Mod zero denominator (RoundUp - | Mod)
-- Fail "Mod zero denominator"
--
data RoundUp n p deriving Show
type RoundUpT n p = (n - p `Mod` n) `Mod` n

instance P (RoundUpT n p) x => P (RoundUp n p) x where
  type PP (RoundUp n p) x = PP (RoundUpT n p) x
  eval _ opts =
    if isVerbose opts
    then eval (Proxy @(MsgI "RoundUp " (RoundUpT n p))) opts
    else eval (Proxy @(MsgI "RoundUp " (Hide (RoundUpT n p)))) opts

