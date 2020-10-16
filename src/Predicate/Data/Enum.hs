{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
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
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE EmptyDataDeriving #-}
-- |     promoted enum functions
module Predicate.Data.Enum (

  -- *** constructors
    type (...)
  , EnumFromTo
  , EnumFromThenTo
  , FromEnum
  , FromEnum'
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
import Data.Proxy (Proxy(..))
import Data.Kind (Type)
import Data.Tree (Tree)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as T
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG
-- >>> import Data.Time

-- | bounded 'succ' function
--
-- >>> pz @(SuccB 'LT Id) GT
-- Val LT
--
-- >>> pz @(SuccB 'LT 'GT) ()
-- Val LT
--
-- >>> pz @(SuccB 'GT 'LT) ()
-- Val EQ
--
-- >>> pl @(SuccB 'LT Id) GT
-- Present LT (SuccB out of range)
-- Val LT
--
data SuccB p q deriving Show

instance ( PP q x ~ a
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
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case succMay q of
          Nothing -> _enumDefault @p @a opts msg0 (hh qq)
          Just n -> pure $ mkNode opts (Val n) (show3 opts msg0 n q) [hh qq]

_enumDefault :: forall p a m
  . ( MonadEval m
    , P p (Proxy a)
    , PP p (Proxy a) ~ a
    )
  => POpts
  -> String
  -> Tree PE
  -> m (TT a)
_enumDefault opts msg0 hhqq = do
   let msg1 = msg0 <> " out of range"
   pp <- eval (Proxy @p) opts (Proxy @a)
   pure $ case getValueLR NoInline opts msg1 pp [hhqq] of
     Left e -> e
     Right _ -> mkNodeCopy opts pp msg1 [hhqq, hh pp]

-- | bounded 'succ' function
--
-- >>> pz @(SuccB' Id) GT
-- Fail "Succ bounded"
--
-- >>> pz @(SuccB' Id) (13 :: Int)
-- Val 14
--
-- >>> pz @(SuccB' Id) LT
-- Val EQ
--
data SuccB' q deriving Show
type SuccBT' q = SuccB (FailP "Succ bounded") q

instance P (SuccBT' q) x => P (SuccB' q) x where
  type PP (SuccB' q) x = PP (SuccBT' q) x
  eval _ = eval (Proxy @(SuccBT' q))

-- | bounded 'pred' function
--
-- >>> pz @(PredB' Id) (13 :: Int)
-- Val 12
--
-- >>> pz @(PredB' Id) LT
-- Fail "Pred bounded"
--
-- >>> pl @(PredB' Id) GT
-- Present EQ (PredB EQ | GT)
-- Val EQ
--
-- >>> pl @(PredB' Id) LT
-- Error Pred bounded (PredB out of range)
-- Fail "Pred bounded"
--

data PredB' q deriving Show
type PredBT' q = PredB (FailP "Pred bounded") q

instance ( PP q x ~ a
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
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case predMay q of
          Nothing -> _enumDefault @p @a opts msg0 (hh qq)
          Just n -> pure $ mkNode opts (Val n) (show3 opts msg0 n q) [hh qq]


-- | unbounded 'succ' function
--
-- >>> pz @Succ 13
-- Val 14
--
-- >>> pz @Succ LT
-- Val EQ
--
-- >>> pz @Succ GT
-- Fail "Succ IO e=Prelude.Enum.Ordering.succ: bad argument"
--
-- >>> pl @Succ 10
-- Present 11 (Succ 11 | 10)
-- Val 11
--
-- >>> pl @Succ True -- captures the exception
-- Error Succ IO e=Prelude.Enum.Bool.succ: bad argument (True)
-- Fail "Succ IO e=Prelude.Enum.Bool.succ: bad argument"
--
data Succ deriving Show

instance ( Show x
         , Enum x
         ) => P Succ x where
  type PP Succ x = x
  eval _ opts x = do
    let msg0 = "Succ"
    lr <- catchit (succ x)
    pure $ case lr of
      Left e -> mkNode opts (Fail (msg0 <> " " <> e)) (showL opts x) []
      Right n -> mkNode opts (Val n) (show3 opts msg0 n x) []

-- | SuccN n p (unsafe) increments an enum p by the given integral n
--
-- >>> pz @(ReadP Day Id >> Id ... SuccN 5 Id) "2020-07-27"
-- Val [2020-07-27,2020-07-28,2020-07-29,2020-07-30,2020-07-31,2020-08-01]
--
-- >>> pz @(ReadP Day Id >> SuccN (Negate 5) Id) "2020-07-27"
-- Val 2020-07-22
--
-- >>> pl @(SuccN 3 'LT) ()
-- Error SuccN IO e=Prelude.Enum.Ordering.toEnum: bad argument (SuccN 3 LT)
-- Fail "SuccN IO e=Prelude.Enum.Ordering.toEnum: bad argument"
--
-- >>> pz @(SuccN 2 'LT) ()
-- Val GT
--
data SuccN n p deriving Show

instance ( Show a
         , Enum a
         , Integral (PP n x)
         , P n x
         , PP p x ~ a
         , P p x
         ) => P (SuccN n p) x where
  type PP (SuccN n p) x = PP p x
  eval _ opts x = do
    let msg0 = "SuccN"
    lr <- runPQ NoInline msg0 (Proxy @n) (Proxy @p) opts x []
    case lr of
      Left e -> pure e
      Right (fromIntegral -> n :: Int,p,nn,pp) -> do
        lr1 <- catchit (toEnum (fromEnum p + n))
        pure $ case lr1 of
          Left e -> mkNode opts (Fail (msg0 <> " " <> e)) (litL opts (msg0 <> " " <> show n <> " " <> show p)) [hh nn, hh pp]
          Right r -> mkNode opts (Val r) (litL opts (msg0 <> " " <> show n <> " " <> show p)) [hh nn, hh pp]


-- | unbounded 'pred' function
--
-- >>> pz @Pred 13
-- Val 12
--
-- >>> pz @Pred LT
-- Fail "Pred IO e=Prelude.Enum.Ordering.pred: bad argument"
--
data Pred deriving Show

instance ( Show x
         , Enum x
         ) => P Pred x where
  type PP Pred x = x
  eval _ opts x = do
    let msg0 = "Pred"
    lr <- catchit (pred x)
    pure $ case lr of
      Left e -> mkNode opts (Fail (msg0 <> " " <> e)) (showL opts x) []
      Right n -> mkNode opts (Val n) (show3 opts msg0 n x) []

-- | bounded 'pred' function
--
-- >>> pl @(PredB 'GT Id) LT
-- Present GT (PredB out of range)
-- Val GT
--
-- >>> pl @(PredB 'LT Id) GT
-- Present EQ (PredB EQ | GT)
-- Val EQ
--

data PredB p q deriving Show

instance P (PredBT' q) x => P (PredB' q) x where
  type PP (PredB' q) x = PP (PredBT' q) x
  eval _ = eval (Proxy @(PredBT' q))


-- | 'fromEnum' function
--
-- >>> pz @(FromEnum' Id) 'x'
-- Val 120
--
-- >>> pl @(FromEnum' ("aa" ==! Id) >> Same 1) "aaaa"
-- False ((>>) False | {0 == 1})
-- Val False
--
-- >>> pl @(FromEnum' ("aa" ==! Id) >> ToEnum OrderingP) "aaaa"
-- Present CGt ((>>) CGt | {ToEnum CGt | 0})
-- Val CGt
--
-- >>> pl @(Map (FromEnum' Id) >> Map (ToEnum Char)) "abcd"
-- Present "abcd" ((>>) "abcd" | {Map "abcd" | [97,98,99,100]})
-- Val "abcd"
--

data FromEnum' p deriving Show

instance ( Show a
         , Enum a
         , PP p x ~ a
         , P p x
         ) => P (FromEnum' p) x where
  type PP (FromEnum' p) x = Int
  eval _ opts x = do
    let msg0 = "FromEnum"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let n = fromEnum p
        in mkNode opts (Val n) (show3 opts msg0 n p) [hh pp]

-- | 'fromEnum' function
--
-- >>> pz @FromEnum 'x'
-- Val 120
--
data FromEnum deriving Show

instance ( Show x
         , Enum x
         ) => P FromEnum x where
  type PP FromEnum x = Int
  eval _ opts x =
    let msg0 = "FromEnum"
        n = fromEnum x
    in pure $ mkNode opts (Val n) (show3 opts msg0 n x) []



-- | unsafe 'toEnum' function
--
-- >>> pz @(ToEnum Char) 120
-- Val 'x'
--
-- >>> pl @(Map FromEnum >> Map (Id - 97 >> ToEnum Ordering)) "abcde"
-- Error ToEnum IO e=Prelude.Enum.Ordering.toEnum: bad argument(2) (Map(i=3, a=100) excnt=2)
-- Fail "ToEnum IO e=Prelude.Enum.Ordering.toEnum: bad argument(2)"
--
-- >>> pl @((ToEnum Day *** ToEnum Day) >> EnumFromTo Fst Snd) (0,5)
-- Present [1858-11-17,1858-11-18,1858-11-19,1858-11-20,1858-11-21,1858-11-22] ((>>) [1858-11-17,1858-11-18,1858-11-19,1858-11-20,1858-11-21,1858-11-22] | {1858-11-17 ... 1858-11-22})
-- Val [1858-11-17,1858-11-18,1858-11-19,1858-11-20,1858-11-21,1858-11-22]
--
data ToEnum' t p deriving Show

instance ( PP p x ~ a
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
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        lr <- catchit (toEnum $! fromIntegral p)
        pure $ case lr of
          Left e -> mkNode opts (Fail (msg0 <> " " <> e)) (showL opts p) [hh pp]
          Right n -> mkNode opts (Val n) (show3 opts msg0 n p) [hh pp]

data ToEnum (t :: Type) deriving Show
type ToEnumT (t :: Type) = ToEnum' (Hole t) Id

instance P (ToEnumT t) x => P (ToEnum t) x where
  type PP (ToEnum t) x = PP (ToEnumT t) x
  eval _ = eval (Proxy @(ToEnumT t))

data ToEnumBDef' t def deriving Show

instance ( P def (Proxy (PP t a))
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
         pure $ case getValueLR NoInline opts msg1 pp [] of
           Left e -> e
           Right _ -> mkNodeCopy opts pp msg1 [hh pp]
      Just n -> pure $ mkNode opts (Val n) (show3 opts msg0 n a) []

-- | bounded 'toEnum' function
--
-- >>> pz @(ToEnumBDef Ordering LT) 2
-- Val GT
--
-- >>> pz @(ToEnumBDef Ordering LT) 6
-- Val LT
--
-- >>> pl @(ToEnumBDef Ordering 'LT) 123
-- Present LT (ToEnumBDef out of range)
-- Val LT
--
-- >>> pl @(ToEnumBDef Ordering 'GT) 1
-- Present EQ (ToEnumBDef EQ | 1)
-- Val EQ
--

data ToEnumBDef (t :: Type) def deriving Show
type ToEnumBDefT (t :: Type) def = ToEnumBDef' (Hole t) def

instance P (ToEnumBDefT t def) x => P (ToEnumBDef t def) x where
  type PP (ToEnumBDef t def) x = PP (ToEnumBDefT t def) x
  eval _ = eval (Proxy @(ToEnumBDefT t def))

-- | bounded 'toEnum' function
--
-- >>> pz @(ToEnumBFail Ordering) 6
-- Fail "ToEnum bounded"
--
-- >>> pl @(ToEnumBFail Ordering) 1
-- Present EQ (ToEnumBDef EQ | 1)
-- Val EQ
--
-- >>> pl @(ToEnumBFail Ordering) 44
-- Error ToEnum bounded (ToEnumBDef out of range)
-- Fail "ToEnum bounded"
--
data ToEnumBFail (t :: Type) deriving Show
type ToEnumBFailT (t :: Type) = ToEnumBDef' (Hole t) (FailP "ToEnum bounded")

instance P (ToEnumBFailT t) x => P (ToEnumBFail t) x where
  type PP (ToEnumBFail t) x = PP (ToEnumBFailT t) x
  eval _ = eval (Proxy @(ToEnumBFailT t))

-- | similar to 'enumFromTo'
--
-- >>> pz @(EnumFromTo 'GT 'LT) ()
-- Val []
--
-- >>> pz @(EnumFromTo Pred Succ) (SG.Max 10)
-- Val [Max {getMax = 9},Max {getMax = 10},Max {getMax = 11}]
--
-- >>> pz @(EnumFromTo 1 20 >> Map '(Id, (If (Id `Mod` 3 == 0) "Fizz" "" <> If (Id `Mod` 5 == 0) "Buzz" ""))) 123
-- Val [(1,""),(2,""),(3,"Fizz"),(4,""),(5,"Buzz"),(6,"Fizz"),(7,""),(8,""),(9,"Fizz"),(10,"Buzz"),(11,""),(12,"Fizz"),(13,""),(14,""),(15,"FizzBuzz"),(16,""),(17,""),(18,"Fizz"),(19,""),(20,"Buzz")]
--
-- >>> pl @(EnumFromTo (Pure SG.Min 9) (Pure _ 13)) ()
-- Present [Min {getMin = 9},Min {getMin = 10},Min {getMin = 11},Min {getMin = 12},Min {getMin = 13}] (Min {getMin = 9} ... Min {getMin = 13})
-- Val [Min {getMin = 9},Min {getMin = 10},Min {getMin = 11},Min {getMin = 12},Min {getMin = 13}]
--
-- >>> pl @(EnumFromTo (Wrap (SG.Min _) 9) (Wrap _ 13)) ()
-- Present [Min {getMin = 9},Min {getMin = 10},Min {getMin = 11},Min {getMin = 12},Min {getMin = 13}] (Min {getMin = 9} ... Min {getMin = 13})
-- Val [Min {getMin = 9},Min {getMin = 10},Min {getMin = 11},Min {getMin = 12},Min {getMin = 13}]
--
data EnumFromTo p q deriving Show

-- | similar to 'enumFromTo'
--
-- >>> pz @(2 ... 5) ()
-- Val [2,3,4,5]
--
-- >>> pz @('LT ... 'GT) ()
-- Val [LT,EQ,GT]
--
-- >>> pz @('Just (MkDay '(2020, 1, 2)) ... 'Just (MkDay '(2020, 1, 7))) ()
-- Val [2020-01-02,2020-01-03,2020-01-04,2020-01-05,2020-01-06,2020-01-07]
--
data p ... q deriving Show
infix 7 ...

type EnumFromToT p q = EnumFromTo p q

instance P (EnumFromToT p q) x => P (p ... q) x where
  type PP (p ... q) x = PP (EnumFromToT p q) x
  eval _ = eval (Proxy @(EnumFromToT p q))

instance ( P p x
         , P q x
         , PP p x ~ a
         , Show a
         , PP q x ~ a
         , Enum a
         ) => P (EnumFromTo p q) x where
  type PP (EnumFromTo p q) x = [PP p x]
  eval _ opts z = do
    let msg0 = "..."
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts z []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) -> mkNode opts (Val (enumFromTo p q)) (showL opts p <> " " <> msg0 <> " " <> showL opts q) [hh pp, hh qq]

-- | similar to 'enumFromThenTo'
--
-- >>> pz @(EnumFromThenTo (10 >> ToEnum Day) (20 >> ToEnum Day) (70 >> ToEnum Day)) ()
-- Val [1858-11-27,1858-12-07,1858-12-17,1858-12-27,1859-01-06,1859-01-16,1859-01-26]
--
-- >>> pz @(EnumFromThenTo (ReadP Day "2020-01-12") (ReadP Day "2020-02-12") (ReadP Day "2020-08-12")) ()
-- Val [2020-01-12,2020-02-12,2020-03-14,2020-04-14,2020-05-15,2020-06-15,2020-07-16]
--
data EnumFromThenTo p q r deriving Show

instance ( P p x
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
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts z []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        rr <- eval (Proxy @r) opts z
        pure $ case getValueLR NoInline opts (msg0 ++ " r failed") rr [hh pp, hh qq] of
          Left e -> e
          Right r ->
            mkNode opts (Val (enumFromThenTo p q r)) (msg0 <> " [" <> showL opts p <> ", " <> showL opts q <> " .. " <> showL opts r <> "]") [hh pp, hh qq, hh rr]

-- | universe of enum using the type pointed to by @p@
--
-- >>> pl @(Universe' Id) LT
-- Present [LT,EQ,GT] (Universe [LT .. GT])
-- Val [LT,EQ,GT]
--
data Universe' p deriving Show

instance ( PP p x ~ a
         , Show a
         , Enum a
         , Bounded a
         ) => P (Universe' p) x where
  type PP (Universe' p) x = [PP p x]
  eval _ opts _ =
    let msg0 = "Universe"
        u = [mn .. mx]
        mn = minBound @a
        mx = maxBound @a
    in pure $ mkNode opts (Val u) (msg0 <> " [" <> showL opts mn <> " .. " <> showL opts mx <> "]") []

-- | get universe of an enum of type @t@
--
-- >>> pz @(Universe Ordering) ()
-- Val [LT,EQ,GT]
--
data Universe (t :: Type) deriving Show
type UniverseT (t :: Type) = Universe' (Hole t)

instance P (UniverseT t) x => P (Universe t) x where
  type PP (Universe t) x = PP (UniverseT t) x
  eval _ = eval (Proxy @(UniverseT t))


