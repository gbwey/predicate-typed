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
     promoted tuple functions
-}
module Predicate.Data.Tuple (

    Dup
  , First
  , Second
  , type (&&&)
  , type (***)
  , Both
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
-- FailT "Pairs:no data found"
--
-- >>> pz @Pairs [1]
-- FailT "Pairs:only one element found"
--
-- >>> pl @Pairs ([] :: [()])
-- Error Pairs:no data found
-- FailT "Pairs:no data found"
--
-- >>> pl @Pairs [1]
-- Error Pairs:only one element found
-- FailT "Pairs:only one element found"
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
               [] -> Left (msg0 <> ":no data found")
               [_] -> Left (msg0 <> ":only one element found")
               _:bs@(_:_) -> Right (zip as bs)
    in pure $ case lr of
         Left e -> mkNode opts (FailT e) "" []
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

-- | applies a function against the first part of a tuple: similar to 'Control.Arrow.first'
--
-- >>> pz @(First (Succ Id)) (12,True)
-- PresentT (13,True)
--
data First p
type FirstT p = p *** I

instance P (FirstT p) x => P (First p) x where
  type PP (First p) x = PP (FirstT p) x
  eval _ = eval (Proxy @(FirstT p))

-- | applies a function against the second part of a tuple: similar to 'Control.Arrow.second'
--
-- >>> pz @(Second (Succ Id)) (12,False)
-- PresentT (12,True)
--
data Second q
type SecondT q = I *** q

instance P (SecondT q) x => P (Second q) x where
  type PP (Second q) x = PP (SecondT q) x
  eval _ = eval (Proxy @(SecondT q))

-- | applies \'p\' to lhs of the tuple and \'q\' to the rhs and then \'ands\' them together: see '&*'
--
-- >>> pl @(AndA (Gt 3) (Lt 10) Id) (1,2)
-- False (False (&*) True | (1 > 3))
-- FalseT
--
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
                in mkNodeB opts (p&&q) (showL opts p <> " " <> msg0 <> " " <> showL opts q <> nullIf " | " zz) [hh rr, hh pp, hh qq]

-- | applies \'p\' to lhs of the tuple and \'q\' to the rhs and then \'Ands\' them together
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

-- | applies \'p\' to lhs of the tuple and \'q\' to the rhs and then \'ors\' them together: see '|+'
--
-- >>> pl @(OrA (Gt 3) (Lt 10) Id) (1,2)
-- True (False (|+) True)
-- TrueT
--
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
                in mkNodeB opts (p||q) (showL opts p <> " " <> msg0 <> " " <> showL opts q <> nullIf " | " zz) [hh rr, hh pp, hh qq]

-- | applies \'p\' to lhs of the tuple and \'q\' to the rhs and then \'Ors\' them together
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

-- | applies \'p\' to the first and second slot of an n-tuple (similar to '***')
--
-- >>> pl @(Both Len (Fst Id)) (("abc",[10..17],1,2,3),True)
-- Present (3,8) (Both)
-- PresentT (3,8)
--
-- >>> pl @(Both (Pred Id) $ Fst Id) ((12,'z',[10..17]),True)
-- Present (11,'y') (Both)
-- PresentT (11,'y')
--
-- >>> pl @(Both (Succ Id) Id) (4,'a')
-- Present (5,'b') (Both)
-- PresentT (5,'b')
--
-- >>> pl @(Both Len (Fst Id)) (("abc",[10..17]),True)
-- Present (3,8) (Both)
-- PresentT (3,8)
--
-- >>> import Data.Time
-- >>> pl @(Both (ReadP Day Id) Id) ("1999-01-01","2001-02-12")
-- Present (1999-01-01,2001-02-12) (Both)
-- PresentT (1999-01-01,2001-02-12)
--
data Both p q
instance ( ExtractL1C (PP q x)
         , ExtractL2C (PP q x)
         , P p (ExtractL1T (PP q x))
         , P p (ExtractL2T (PP q x))
         , P q x
   ) => P (Both p q) x where
  type PP (Both p q) x = (PP p (ExtractL1T (PP q x)), PP p (ExtractL2T (PP q x)))
  eval _ opts x = do
    let msg0 = "Both"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        let (a,a') = (extractL1C q, extractL2C q)
        pp <- eval (Proxy @p) opts a
        case getValueLR opts msg0 pp [hh qq] of
          Left e -> pure e
          Right b -> do
            pp' <- eval (Proxy @p) opts a'
            pure $ case getValueLR opts msg0 pp' [hh qq, hh pp] of
              Left e -> e
              Right b' ->
                mkNode opts (PresentT (b,b')) msg0 [hh qq, hh pp, hh pp']
