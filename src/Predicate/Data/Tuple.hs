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
     promoted tuple functions
-}
module Predicate.Data.Tuple (

    Dup
  , First
  , Second
  , type (&&&)
  , type (***)
  , Pairs

  , AndA
  , type (&*)
  , OrA
  , type (|+)

 ) where
import Predicate.Core
import Predicate.Util
import Data.Proxy

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate.Prelude

-- | duplicate a value into a tuple
--
-- >>> pl @Dup 4
-- Present (4,4) (W '(4,4))
-- PresentT (4,4)
--
-- >>> pl @(Dup >> Id) 4
-- Present (4,4) ((>>) (4,4) | {Id (4,4)})
-- PresentT (4,4)
--
-- >>> pl @(Dup << Fst Id * Snd Id) (4,5)
-- Present (20,20) ((>>) (20,20) | {W '(20,20)})
-- PresentT (20,20)
--
-- >>> pl @(Fst Id * Snd Id >> Dup) (4,5)
-- Present (20,20) ((>>) (20,20) | {W '(20,20)})
-- PresentT (20,20)
--
data Dup
type DupT = W '(Id, Id)

instance Show x => P Dup x where
  type PP Dup x = PP DupT x
  eval _ = eval (Proxy @DupT)

-- | creates a list of overlapping pairs of elements. requires two or more elements
--
-- >>> pz @Pairs [1,2,3,4]
-- PresentT [(1,2),(2,3),(3,4)]
--
-- >>> pz @Pairs []
-- FailT "Pairs no data found"
--
-- >>> pz @Pairs [1]
-- FailT "Pairs only one element found"
--
-- >>> pl @Pairs ([] :: [()])
-- Error Pairs no data found (Pairs no data found)
-- FailT "Pairs no data found"
--
-- >>> pl @Pairs [1]
-- Error Pairs only one element found (Pairs only one element found)
-- FailT "Pairs only one element found"
--
-- >>> pl @Pairs [1,2]
-- Present [(1,2)] (Pairs [(1,2)] | [1,2])
-- PresentT [(1,2)]
--
-- >>> pl @Pairs [1,2,3]
-- Present [(1,2),(2,3)] (Pairs [(1,2),(2,3)] | [1,2,3])
-- PresentT [(1,2),(2,3)]
--
-- >>> pl @Pairs [1,2,3,4]
-- Present [(1,2),(2,3),(3,4)] (Pairs [(1,2),(2,3),(3,4)] | [1,2,3,4])
-- PresentT [(1,2),(2,3),(3,4)]
--
data Pairs
instance Show a => P Pairs [a] where
  type PP Pairs [a] = [(a,a)]
  eval _ opts as =
    let msg0 = "Pairs"
        lr = case as of
               [] -> Left (msg0 <> " no data found")
               [_] -> Left (msg0 <> " only one element found")
               _:bs@(_:_) -> Right (zip as bs)
    in pure $ case lr of
         Left e -> mkNode opts (FailT e) e []
         Right zs -> mkNode opts (PresentT zs) (show01 opts msg0 zs as ) []


-- | similar to 'Control.Arrow.&&&'
--
-- >>> pl @(Min &&& Max >> Id >> Fst Id < Snd Id) [10,4,2,12,14]
-- True ((>>) True | {2 < 14})
-- TrueT
--
-- >>> pl @((123 &&& Id) >> Fst Id + Snd Id) 4
-- Present 127 ((>>) 127 | {123 + 4 = 127})
-- PresentT 127
--
-- >>> pl @(4 &&& "sadf" &&& 'LT) ()
-- Present (4,("sadf",LT)) (W '(4,("sadf",LT)))
-- PresentT (4,("sadf",LT))
--
-- >>> pl @(Id &&& '() &&& ()) (Just 10)
-- Present (Just 10,((),())) (W '(Just 10,((),())))
-- PresentT (Just 10,((),()))
--
-- >>> pl @(Fst Id &&& Snd Id &&& Thd Id &&& ()) (1,'x',True)
-- Present (1,('x',(True,()))) (W '(1,('x',(True,()))))
-- PresentT (1,('x',(True,())))
--
-- >>> pl @(Fst Id &&& Snd Id &&& Thd Id &&& ()) (1,'x',True)
-- Present (1,('x',(True,()))) (W '(1,('x',(True,()))))
-- PresentT (1,('x',(True,())))
--
-- >>> pl @(Fst Id &&& Snd Id &&& Thd Id &&& ()) (1,1.4,"aaa")
-- Present (1,(1.4,("aaa",()))) (W '(1,(1.4,("aaa",()))))
-- PresentT (1,(1.4,("aaa",())))
--
data p &&& q
infixr 3 &&&
type WAmpT p q = W '(p, q)

instance P (WAmpT p q) x => P (p &&& q) x where
  type PP (p &&& q) x = PP (WAmpT p q) x
  eval _ = eval (Proxy @(WAmpT p q))

-- | similar to 'Control.Arrow.***'
--
-- >>> pz @(Pred Id *** ShowP Id) (13, True)
-- PresentT (12,"True")
--
-- >>> pl @(FlipT (***) Len (Id * 12)) (99,"cdef")
-- Present (1188,4) ((***) (1188,4) | (99,"cdef"))
-- PresentT (1188,4)
--
-- >>> pl @(4 *** "sadf" *** 'LT) ('x',("abv",[1]))
-- Present (4,("sadf",LT)) ((***) (4,("sadf",LT)) | ('x',("abv",[1])))
-- PresentT (4,("sadf",LT))
--
data p *** q
infixr 3 ***

instance (Show (PP p a)
        , Show (PP q b)
        , P p a
        , P q b
        , Show a
        , Show b
        ) => P (p *** q) (a,b) where
  type PP (p *** q) (a,b) = (PP p a, PP q b)
  eval _ opts (a,b) = do
    let msg0 = "(***)"
    pp <- eval (Proxy @p) opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right a1 -> do
        qq <- eval (Proxy @q) opts b
        pure $ case getValueLR opts msg0 qq [hh pp] of
          Left e -> e
          Right b1 -> mkNode opts (PresentT (a1,b1)) (msg0 <> " " <> showL opts (a1,b1) <> showVerbose opts " | " (a,b)) [hh pp, hh qq]

data First p
type FirstT p = p *** I

instance P (FirstT p) x => P (First p) x where
  type PP (First p) x = PP (FirstT p) x
  eval _ = eval (Proxy @(FirstT p))

data Second q
type SecondT q = I *** q

instance P (SecondT q) x => P (Second q) x where
  type PP (Second q) x = PP (SecondT q) x
  eval _ = eval (Proxy @(SecondT q))

data AndA p q r
instance (PP r x ~ (a,b)
        , PP p a ~ Bool
        , PP q b ~ Bool
        , P p a
        , P q b
        , P r x
        ) => P (AndA p q r) x where
  type PP (AndA p q r) x = Bool
  eval _ opts x = do
    let msg0 = "(&*)"
    rr <- eval (Proxy @r) opts x
    case getValueLR opts msg0 rr [] of
      Left e -> pure e
      Right (r1,r2) -> do
        pp <- evalBool (Proxy @p) opts r1
        case getValueLR opts msg0 pp [hh rr] of
          Left e -> pure e
          Right p -> do
            qq <- evalBool (Proxy @q) opts r2
            pure $ case getValueLR opts msg0 qq [hh rr, hh pp] of
              Left e -> e
              Right q ->
                let zz = case (p,q) of
                          (True, True) -> ""
                          (False, True) -> topMessage pp
                          (True, False) -> topMessage qq
                          (False, False) -> topMessage pp <> " " <> msg0 <> " " <> topMessage qq
                in mkNodeB opts (p&&q) (showL opts p <> " " <> msg0 <> " " <> showL opts q <> (if null zz then zz else " | " <> zz)) [hh rr, hh pp, hh qq]

-- | like 'Predicate.Core.&&' but for a tuple
--
-- >>> pl @(SplitAt 4 "abcdefg" >> Len > 4 &* Len < 5) ()
-- False ((>>) False | {False (&*) True | (4 > 4)})
-- FalseT
--
data p &* q
type AndAT p q = AndA p q Id
infixr 3 &*

instance P (AndAT p q) x => P (p &* q) x where
  type PP (p &* q) x = PP (AndAT p q) x
  eval _ = evalBool (Proxy @(AndAT p q))

data OrA p q r
instance (PP r x ~ (a,b)
        , PP p a ~ Bool
        , PP q b ~ Bool
        , P p a
        , P q b
        , P r x
        ) => P (OrA p q r) x where
  type PP (OrA p q r) x = Bool
  eval _ opts x = do
    let msg0 = "(|+)"
    rr <- eval (Proxy @r) opts x
    case getValueLR opts msg0 rr [] of
      Left e -> pure e
      Right (r1,r2) -> do
        pp <- evalBool (Proxy @p) opts r1
        case getValueLR opts msg0 pp [hh rr] of
          Left e -> pure e
          Right p -> do
            qq <- evalBool (Proxy @q) opts r2
            pure $ case getValueLR opts msg0 qq [hh rr, hh pp] of
              Left e -> e
              Right q ->
                let zz = case (p,q) of
                          (False,False) -> topMessage pp <> " " <> msg0 <> " " <> topMessage qq
                          _ -> ""
                in mkNodeB opts (p||q) (showL opts p <> " " <> msg0 <> " " <> showL opts q <> (if null zz then zz else " | " <> zz)) [hh rr, hh pp, hh qq]

-- | like 'Predicate.Core.||' but for a tuple
--
-- >>> pl @(Sum > 44 |+ Id < 2) ([5,6,7,8,14,44],9)
-- True (True (|+) False)
-- TrueT
--
-- >>> pl @(Sum > 44 |+ Id < 2) ([5,6,7,14],9)
-- False (False (|+) False | (32 > 44) (|+) (9 < 2))
-- FalseT
--
-- >>> pl @(Sum > 44 |+ Id < 2) ([5,6,7,14],1)
-- True (False (|+) True)
-- TrueT
--
data p |+ q
type OrAT p q = OrA p q Id
infixr 3 |+

instance P (OrAT p q) x => P (p |+ q) x where
  type PP (p |+ q) x = PP (OrAT p q) x
  eval _ = evalBool (Proxy @(OrAT p q))


