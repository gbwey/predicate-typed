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
     promoted enum functions
-}
module Predicate.Data.Enum (

  -- *** constructors
    type (...)
  , EnumFromTo
  , EnumFromThenTo
  , FromEnum
  , Universe
  , Universe'

  -- ** bounded enums
  , SuccB
  , SuccB'
  , PredB
  , PredB'
  , ToEnumBDef
  , ToEnumBDef'
  , ToEnumBFail

  -- ** unsafe enum expressions
  , Succ
  , SuccN
  , Pred
  , ToEnum
  , ToEnum'

 ) where
import Predicate.Core
import Predicate.Util
import Safe (succMay, predMay, toEnumMay)
import Data.Proxy
import qualified Control.Exception as E
import Data.Kind (Type)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import qualified Data.Text as T
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG
-- >>> import Data.Time

instance (PP q x ~ a
        , P q x
        , P p (Proxy a)
        , PP p (Proxy a) ~ a
        , Show a
        , Eq a
        , Bounded a
        , Enum a
        ) => P (SuccB p q) x where
  type PP (SuccB p q) x = PP q x
  eval _ opts x = do
    let msg0 = "SuccB"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case succMay q of
          Nothing -> do
             let msg1 = msg0 <> " out of range"
             pp <- eval (Proxy @p) opts (Proxy @a)
             pure $ case getValueLR opts msg1 pp [hh qq] of
               Left e -> e
               Right _ -> mkNode opts (_ttBool pp) msg1 [hh qq, hh pp]
          Just n -> pure $ mkNode opts (PresentT n) (show01 opts msg0 n q) [hh qq]

-- | bounded 'succ' function
--
-- >>> pz @(SuccB 'LT Id) GT
-- PresentT LT
--
data SuccB p q

-- | bounded 'succ' function
--
-- >>> pz @(SuccB' Id) GT
-- FailT "Succ bounded"
--
-- >>> pz @(SuccB' Id) (13 :: Int)
-- PresentT 14
--
-- >>> pz @(SuccB' Id) LT
-- PresentT EQ
--
data SuccB' q
type SuccBT' q = SuccB (Failp "Succ bounded") q

instance P (SuccBT' q) x => P (SuccB' q) x where
  type PP (SuccB' q) x = PP (SuccBT' q) x
  eval _ = eval (Proxy @(SuccBT' q))

-- | bounded 'pred' function
--
-- >>> pz @(PredB' Id) (13 :: Int)
-- PresentT 12
--
-- >>> pz @(PredB' Id) LT
-- FailT "Pred bounded"
--
-- >>> pl @(PredB' Id) GT
-- Present EQ (PredB EQ | GT)
-- PresentT EQ
--
-- >>> pl @(PredB' Id) LT
-- Error Pred bounded (PredB out of range)
-- FailT "Pred bounded"
--

data PredB' q
type PredBT' q = PredB (Failp "Pred bounded") q

instance (PP q x ~ a
        , P q x
        , P p (Proxy a)
        , PP p (Proxy a) ~ a
        , Show a
        , Eq a
        , Bounded a
        , Enum a
        ) => P (PredB p q) x where
  type PP (PredB p q) x = PP q x
  eval _ opts x = do
    let msg0 = "PredB"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case predMay q of
          Nothing -> do
             let msg1 = msg0 <> " out of range"
             pp <- eval (Proxy @p) opts (Proxy @a)
             pure $ case getValueLR opts msg1 pp [hh qq] of
               Left e -> e
               Right _ -> mkNode opts (_ttBool pp) msg1 [hh qq, hh pp]
          Just n -> pure $ mkNode opts (PresentT n) (show01 opts msg0 n q) [hh qq]


-- | unbounded 'succ' function
--
-- >>> pz @(Succ Id) 13
-- PresentT 14
--
-- >>> pz @(Succ Id) LT
-- PresentT EQ
--
-- >>> pz @(Succ Id) GT
-- FailT "Succ IO e=Prelude.Enum.Ordering.succ: bad argument"
--
-- >>> pl @(Succ Id) 10
-- Present 11 (Succ 11 | 10)
-- PresentT 11
--
-- >>> pl @(Succ Id) True -- captures the exception
-- Error Succ IO e=Prelude.Enum.Bool.succ: bad argument (True)
-- FailT "Succ IO e=Prelude.Enum.Bool.succ: bad argument"
--
data Succ p

instance (Show a
        , Enum a
        , PP p x ~ a
        , P p x
        ) => P (Succ p) x where
  type PP (Succ p) x = PP p x
  eval _ opts x = do
    let msg0 = "Succ"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        lr <- catchit @_ @E.SomeException (succ p)
        pure $ case lr of
          Left e -> mkNode opts (FailT (msg0 <> " " <> e)) (showL opts p) [hh pp]
          Right n -> mkNode opts (PresentT n) (show01 opts msg0 n p) [hh pp]

-- | SuccN n p (unsafe) increments an enum p by the given integral n
--
-- >>> pz @(ReadP Day Id >> Id ... SuccN 5 Id) "2020-07-27"
-- PresentT [2020-07-27,2020-07-28,2020-07-29,2020-07-30,2020-07-31,2020-08-01]
--
-- >>> pz @(ReadP Day Id >> SuccN (Negate 5) Id) "2020-07-27"
-- PresentT 2020-07-22
--
-- >>> pl @(SuccN 3 'LT) ()
-- Error SuccN IO e=Prelude.Enum.Ordering.toEnum: bad argument (SuccN 3 LT)
-- FailT "SuccN IO e=Prelude.Enum.Ordering.toEnum: bad argument"
--
-- >>> pz @(SuccN 2 'LT) ()
-- PresentT GT
--
data SuccN n p

instance (Show a
        , Enum a
        , Integral (PP n x)
        , P n x
        , PP p x ~ a
        , P p x
        ) => P (SuccN n p) x where
  type PP (SuccN n p) x = PP p x
  eval _ opts x = do
    let msg0 = "SuccN"
    lr <- runPQ msg0 (Proxy @n) (Proxy @p) opts x []
    case lr of
      Left e -> pure e
      Right (n,p,nn,pp) -> do
        lr1 <- catchit @_ @E.SomeException (toEnum (fromEnum p + fromIntegral n))
        pure $ case lr1 of
          Left e -> mkNode opts (FailT (msg0 <> " " <> e)) (litL opts (msg0 <> " " <> show (fromIntegral @_ @Integer n) <> " " <> show p)) [hh nn, hh pp]
          Right r -> mkNode opts (PresentT r) (litL opts (msg0 <> " " <> show (fromIntegral @_ @Integer n) <> " " <> show p)) [hh nn, hh pp]


-- | unbounded 'pred' function
--
-- >>> pz @(Pred Id) 13
-- PresentT 12
--
-- >>> pz @(Pred Id) LT
-- FailT "Pred IO e=Prelude.Enum.Ordering.pred: bad argument"
--
data Pred p

instance (Show a
        , Enum a
        , PP p x ~ a
        , P p x
        ) => P (Pred p) x where
  type PP (Pred p) x = PP p x
  eval _ opts x = do
    let msg0 = "Pred"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        lr <- catchit @_ @E.SomeException (pred p)
        pure $ case lr of
          Left e -> mkNode opts (FailT (msg0 <> " " <> e)) (showL opts p) [hh pp]
          Right n -> mkNode opts (PresentT n) (show01 opts msg0 n p) [hh pp]

-- | bounded 'pred' function
--
-- >>> pl @(PredB 'GT Id) LT
-- Present GT (PredB out of range)
-- PresentT GT
--
-- >>> pl @(PredB 'LT Id) GT
-- Present EQ (PredB EQ | GT)
-- PresentT EQ
--

data PredB p q

instance P (PredBT' q) x => P (PredB' q) x where
  type PP (PredB' q) x = PP (PredBT' q) x
  eval _ = eval (Proxy @(PredBT' q))


-- | 'fromEnum' function
--
-- >>> pz @(FromEnum Id) 'x'
-- PresentT 120
--
-- >>> pl @(FromEnum ("aa" ==! Id) >> Same 1) "aaaa"
-- False ((>>) False | {0 == 1})
-- FalseT
--
-- >>> pl @(FromEnum ("aa" ==! Id) >> ToEnum OrderingP Id) "aaaa"
-- Present CGt ((>>) CGt | {ToEnum CGt | 0})
-- PresentT CGt
--
-- >>> pl @(Map (FromEnum Id) Id >> Map (ToEnum Char Id) Id) ("abcd" :: String)
-- Present "abcd" ((>>) "abcd" | {Map "abcd" | [97,98,99,100]})
-- PresentT "abcd"
--

data FromEnum p

instance (Show a
        , Enum a
        , PP p x ~ a
        , P p x
        ) => P (FromEnum p) x where
  type PP (FromEnum p) x = Int
  eval _ opts x = do
    let msg0 = "FromEnum"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let n = fromEnum p
        in mkNode opts (PresentT n) (show01 opts msg0 n p) [hh pp]

-- | unsafe 'toEnum' function
--
-- >>> pz @(ToEnum Char Id) 120
-- PresentT 'x'
--
-- >>> pl @(Map (FromEnum Id) Id >> Map (Id - 97 >> ToEnum Ordering Id) Id) ("abcde" :: String)
-- Error ToEnum IO e=Prelude.Enum.Ordering.toEnum: bad argument(2) ([97,98,99,100,101])
-- FailT "ToEnum IO e=Prelude.Enum.Ordering.toEnum: bad argument(2)"
--
-- >>> pl @((ToEnum Day Id *** ToEnum Day Id) >> EnumFromTo Fst Snd) (0,5)
-- Present [1858-11-17,1858-11-18,1858-11-19,1858-11-20,1858-11-21,1858-11-22] ((>>) [1858-11-17,1858-11-18,1858-11-19,1858-11-20,1858-11-21,1858-11-22] | {1858-11-17 ... 1858-11-22})
-- PresentT [1858-11-17,1858-11-18,1858-11-19,1858-11-20,1858-11-21,1858-11-22]
--
data ToEnum' t p

instance (PP p x ~ a
        , P p x
        , Show a
        , Enum (PP t x)
        , Show (PP t x)
        , Integral a
        ) => P (ToEnum' t p) x where
  type PP (ToEnum' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "ToEnum"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        lr <- catchit @_ @E.SomeException (toEnum $! fromIntegral p)
        pure $ case lr of
          Left e -> mkNode opts (FailT (msg0 <> " " <> e)) (showL opts p) [hh pp]
          Right n -> mkNode opts (PresentT n) (show01 opts msg0 n p) [hh pp]

data ToEnum (t :: Type) p
type ToEnumT (t :: Type) p = ToEnum' (Hole t) p

instance P (ToEnumT t p) x => P (ToEnum t p) x where
  type PP (ToEnum t p) x = PP (ToEnumT t p) x
  eval _ = eval (Proxy @(ToEnumT t p))
data ToEnumBDef' t def

instance (P def (Proxy (PP t a))
        , PP def (Proxy (PP t a)) ~ PP t a
        , Show a
        , Show (PP t a)
        , Bounded (PP t a)
        , Enum (PP t a)
        , Integral a
        ) => P (ToEnumBDef' t def) a where
  type PP (ToEnumBDef' t def) a = PP t a
  eval _ opts a = do
    let msg0 = "ToEnumBDef"
    case toEnumMay $ fromIntegral a of
      Nothing -> do
         let msg1 = msg0 <> " out of range"
         pp <- eval (Proxy @def) opts (Proxy @(PP t a))
         pure $ case getValueLR opts msg1 pp [] of
           Left e -> e
           Right _ -> mkNode opts (_ttBool pp) msg1 [hh pp]
      Just n -> pure $ mkNode opts (PresentT n) (show01 opts msg0 n a) []

-- | bounded 'toEnum' function
--
-- >>> pz @(ToEnumBDef Ordering LT) 2
-- PresentT GT
--
-- >>> pz @(ToEnumBDef Ordering LT) 6
-- PresentT LT
--
-- >>> pl @(ToEnumBDef Ordering 'LT) 123
-- Present LT (ToEnumBDef out of range)
-- PresentT LT
--
-- >>> pl @(ToEnumBDef Ordering 'GT) 1
-- Present EQ (ToEnumBDef EQ | 1)
-- PresentT EQ
--

data ToEnumBDef (t :: Type) def
type ToEnumBDefT (t :: Type) def = ToEnumBDef' (Hole t) def

instance P (ToEnumBDefT t def) x => P (ToEnumBDef t def) x where
  type PP (ToEnumBDef t def) x = PP (ToEnumBDefT t def) x
  eval _ = eval (Proxy @(ToEnumBDefT t def))

-- | bounded 'toEnum' function
--
-- >>> pz @(ToEnumBFail Ordering) 6
-- FailT "ToEnum bounded"
--
-- >>> pl @(ToEnumBFail Ordering) 1
-- Present EQ (ToEnumBDef EQ | 1)
-- PresentT EQ
--
-- >>> pl @(ToEnumBFail Ordering) 44
-- Error ToEnum bounded (ToEnumBDef out of range)
-- FailT "ToEnum bounded"
--
data ToEnumBFail (t :: Type)
type ToEnumBFailT (t :: Type) = ToEnumBDef' (Hole t) (Failp "ToEnum bounded")

instance P (ToEnumBFailT t) x => P (ToEnumBFail t) x where
  type PP (ToEnumBFail t) x = PP (ToEnumBFailT t) x
  eval _ = eval (Proxy @(ToEnumBFailT t))

-- | similar to 'enumFromTo'
--
-- >>> pz @(EnumFromTo 'GT 'LT) ()
-- PresentT []
--
-- >>> pz @(EnumFromTo (Pred Id) (Succ Id)) (SG.Max 10)
-- PresentT [Max {getMax = 9},Max {getMax = 10},Max {getMax = 11}]
--
-- >>> pz @(EnumFromTo 1 20 >> Map '(Id, (If (Id `Mod` 3 == 0) "Fizz" "" <> If (Id `Mod` 5 == 0) "Buzz" "")) Id) 123
-- PresentT [(1,""),(2,""),(3,"Fizz"),(4,""),(5,"Buzz"),(6,"Fizz"),(7,""),(8,""),(9,"Fizz"),(10,"Buzz"),(11,""),(12,"Fizz"),(13,""),(14,""),(15,"FizzBuzz"),(16,""),(17,""),(18,"Fizz"),(19,""),(20,"Buzz")]
--
-- >>> pl @(EnumFromTo (Pure SG.Min 9) (Pure _ 13)) ()
-- Present [Min {getMin = 9},Min {getMin = 10},Min {getMin = 11},Min {getMin = 12},Min {getMin = 13}] (Min {getMin = 9} ... Min {getMin = 13})
-- PresentT [Min {getMin = 9},Min {getMin = 10},Min {getMin = 11},Min {getMin = 12},Min {getMin = 13}]
--
-- >>> pl @(EnumFromTo (Wrap (SG.Min _) 9) (Wrap _ 13)) ()
-- Present [Min {getMin = 9},Min {getMin = 10},Min {getMin = 11},Min {getMin = 12},Min {getMin = 13}] (Min {getMin = 9} ... Min {getMin = 13})
-- PresentT [Min {getMin = 9},Min {getMin = 10},Min {getMin = 11},Min {getMin = 12},Min {getMin = 13}]
--
data EnumFromTo p q

-- | similar to 'enumFromTo'
--
-- >>> pz @(2 ... 5) ()
-- PresentT [2,3,4,5]
--
-- >>> pz @('LT ... 'GT) ()
-- PresentT [LT,EQ,GT]
--
-- >>> pz @('Just (MkDay '(2020, 1, 2)) ... 'Just (MkDay '(2020, 1, 7))) ()
-- PresentT [2020-01-02,2020-01-03,2020-01-04,2020-01-05,2020-01-06,2020-01-07]
--
data p ... q
infix 7 ...

type EnumFromToT p q = EnumFromTo p q

instance P (EnumFromToT p q) x => P (p ... q) x where
  type PP (p ... q) x = PP (EnumFromToT p q) x
  eval _ = eval (Proxy @(EnumFromToT p q))

instance (P p x
        , P q x
        , PP p x ~ a
        , Show a
        , PP q x ~ a
        , Enum a
        ) => P (EnumFromTo p q) x where
  type PP (EnumFromTo p q) x = [PP p x]
  eval _ opts z = do
    let msg0 = "..."
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts z []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) -> mkNode opts (PresentT (enumFromTo p q)) (showL opts p <> " " <> msg0 <> " " <> showL opts q) [hh pp, hh qq]

-- | similar to 'enumFromThenTo'
--
-- >>> pz @(EnumFromThenTo (ToEnum Day 10) (ToEnum Day 20) (ToEnum Day 70)) ()
-- PresentT [1858-11-27,1858-12-07,1858-12-17,1858-12-27,1859-01-06,1859-01-16,1859-01-26]
--
-- >>> pz @(EnumFromThenTo (ReadP Day "2020-01-12") (ReadP Day "2020-02-12") (ReadP Day "2020-08-12")) ()
-- PresentT [2020-01-12,2020-02-12,2020-03-14,2020-04-14,2020-05-15,2020-06-15,2020-07-16]
--
data EnumFromThenTo p q r

instance (P p x
        , P q x
        , P r x
        , PP p x ~ a
        , Show a
        , PP q x ~ a
        , PP r x ~ a
        , Enum a
        ) => P (EnumFromThenTo p q r) x where
  type PP (EnumFromThenTo p q r) x = [PP p x]
  eval _ opts z = do
    let msg0 = "EnumFromThenTo"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts z []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        rr <- eval (Proxy @r) opts z
        pure $ case getValueLR opts (msg0 ++ " r failed") rr [hh pp, hh qq] of
          Left e -> e
          Right r ->
            mkNode opts (PresentT (enumFromThenTo p q r)) (msg0 <> " [" <> showL opts p <> ", " <> showL opts q <> " .. " <> showL opts r <> "]") [hh pp, hh qq, hh rr]

-- | universe of enum using the type pointed to by \'p\'
--
-- >>> pl @(Universe' Id) LT
-- Present [LT,EQ,GT] (Universe [LT .. GT])
-- PresentT [LT,EQ,GT]
--
data Universe' p

instance ( PP p x ~ a
         , Show a
         , Enum a
         , Bounded a
         ) => P (Universe' p) x where
  type PP (Universe' p) x = [PP p x]
  eval _ opts _z =
    let msg0 = "Universe"
        u = [mn .. mx]
        mn = minBound @a
        mx = maxBound @a
    in pure $ mkNode opts (PresentT u) (msg0 <> " [" <> showL opts mn <> " .. " <> showL opts mx <> "]") []

-- | get universe of an enum of type \'t\'
--
-- >>> pz @(Universe Ordering) ()
-- PresentT [LT,EQ,GT]
--
data Universe (t :: Type)
type UniverseT (t :: Type) = Universe' (Hole t)

instance P (UniverseT t) x => P (Universe t) x where
  type PP (Universe t) x = PP (UniverseT t) x
  eval _ = eval (Proxy @(UniverseT t))


