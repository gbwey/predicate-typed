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
  , Tuple
  , Tuple'

  , AndA
  , type (&*)
  , OrA
  , type (|+)

 ) where
import Predicate.Core
import Predicate.Util
import Data.Proxy (Proxy(Proxy))
import GHC.TypeNats (Nat, KnownNat)

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
-- >>> pl @(Dup << Fst * Snd) (4,5)
-- Present (20,20) ((>>) (20,20) | {W '(20,20)})
-- PresentT (20,20)
--
-- >>> pl @(Fst * Snd >> Dup) (4,5)
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
-- PresentT []
--
-- >>> pz @Pairs [1]
-- PresentT []
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
    let zs = case as of
               [] -> []
               _:bs -> zip as bs
    in pure $ mkNode opts (PresentT zs) (show01 opts "Pairs" zs as) []

-- | similar to 'Control.Arrow.&&&'
--
-- >>> pl @(Min &&& Max >> Id >> Fst < Snd) [10,4,2,12,14]
-- Present True ((>>) True | {True:2 < 14})
-- PresentT True
--
-- >>> pl @((123 &&& Id) >> Fst + Snd) 4
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
-- >>> pl @(Fst &&& Snd &&& Thd &&& ()) (1,'x',True)
-- Present (1,('x',(True,()))) (W '(1,('x',(True,()))))
-- PresentT (1,('x',(True,())))
--
-- >>> pl @(Fst &&& Snd &&& Thd &&& ()) (1,'x',True)
-- Present (1,('x',(True,()))) (W '(1,('x',(True,()))))
-- PresentT (1,('x',(True,())))
--
-- >>> pl @(Fst &&& Snd &&& Thd &&& ()) (1,1.4,"aaa")
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
-- >>> pz @(Pred *** ShowP Id) (13, True)
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
-- >>> pz @(First Succ) (12,True)
-- PresentT (13,True)
--
data First p
type FirstT p = p *** Id

instance P (FirstT p) x => P (First p) x where
  type PP (First p) x = PP (FirstT p) x
  eval _ = eval (Proxy @(FirstT p))

-- | applies a function against the second part of a tuple: similar to 'Control.Arrow.second'
--
-- >>> pz @(Second Succ) (12,False)
-- PresentT (12,True)
--
data Second q
type SecondT q = Id *** q

instance P (SecondT q) x => P (Second q) x where
  type PP (Second q) x = PP (SecondT q) x
  eval _ = eval (Proxy @(SecondT q))

-- | applies @p@ to lhs of the tuple and @q@ to the rhs and then @ands@ them together: see '&*'
--
-- >>> pl @(AndA (Gt 3) (Lt 10) Id) (1,2)
-- Present False (False:False (&*) True | (False:1 > 3))
-- PresentT False
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

-- | applies @p@ to lhs of the tuple and @q@ to the rhs and then @Ands@ them together
--
-- >>> pl @(SplitAt 4 "abcdefg" >> Len > 4 &* Len < 5) ()
-- Present False ((>>) False | {False:False (&*) True | (False:4 > 4)})
-- PresentT False
--
data p &* q
type AndAT p q = AndA p q Id
infixr 3 &*

instance P (AndAT p q) x => P (p &* q) x where
  type PP (p &* q) x = PP (AndAT p q) x
  eval _ = evalBool (Proxy @(AndAT p q))

-- | applies @p@ to lhs of the tuple and @q@ to the rhs and then @ors@ them together: see '|+'
--
-- >>> pl @(OrA (Gt 3) (Lt 10) Id) (1,2)
-- Present True (True:False (|+) True)
-- PresentT True
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

-- | applies @p@ to lhs of the tuple and @q@ to the rhs and then @Ors@ them together
--
-- >>> pl @(Sum > 44 |+ Id < 2) ([5,6,7,8,14,44],9)
-- Present True (True:True (|+) False)
-- PresentT True
--
-- >>> pl @(Sum > 44 |+ Id < 2) ([5,6,7,14],9)
-- Present False (False:False (|+) False | (False:32 > 44) (|+) (False:9 < 2))
-- PresentT False
--
-- >>> pl @(Sum > 44 |+ Id < 2) ([5,6,7,14],1)
-- Present True (True:False (|+) True)
-- PresentT True
--
data p |+ q
type OrAT p q = OrA p q Id
infixr 3 |+

instance P (OrAT p q) x => P (p |+ q) x where
  type PP (p |+ q) x = PP (OrAT p q) x
  eval _ = evalBool (Proxy @(OrAT p q))

-- | applies @p@ to the first and second slot of an n-tuple (similar to '***')
--
-- >>> pl @(Both Len Fst) (("abc",[10..17],1,2,3),True)
-- Present (3,8) (Both)
-- PresentT (3,8)
--
-- >>> pl @(Both Pred $ Fst) ((12,'z',[10..17]),True)
-- Present (11,'y') (Both)
-- PresentT (11,'y')
--
-- >>> pl @(Both Succ Id) (4,'a')
-- Present (5,'b') (Both)
-- PresentT (5,'b')
--
-- >>> pl @(Both Len Fst) (("abc",[10..17]),True)
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

-- | create a n tuple from a list
--
-- >>> pz @(Tuple 4) "abcdefg"
-- PresentT ('a','b','c','d')
--
-- >>> pz @(Tuple 4) "abc"
-- FailT "Tuple(4):not enough elements"
--
-- >>> pz @(Fst >> Tuple 3) ([1..5],True)
-- PresentT (1,2,3)
--
-- >>> pz @(Lift (Tuple 3) Fst) ([1..5],True)
-- PresentT (1,2,3)
--
data Tuple (n :: Nat)

instance ( KnownNat n
         , TupleC n a
         , x ~ [a]
         , Show a
         ) => P (Tuple n) x where
  type PP (Tuple n) x = TupleT n (ExtractAFromList x)
  eval _ opts as =
    let msg0 = "Tuple(" ++ show n ++ ")"
        n :: Int
        n = nat @n
    in pure $ case getTupleC @n as of
         Left es -> mkNode opts (FailT (msg0 <> ":not enough elements")) (showVerbose opts " | " es) []
         Right r -> mkNode opts (PresentT r) msg0 []

-- | create a n tuple from a list
--
-- >>> pz @(Tuple' 4) "abcdefg"
-- PresentT (Right ('a','b','c','d'))
--
-- >>> pz @(Tuple' 4) "abc"
-- PresentT (Left "abc")
--
-- >>> pz @(Tuple' 4) []
-- PresentT (Left [])
--
-- >>> :set -XPolyKinds
-- >>> type F n i = ChunksOf' n i Id >> Map (Tuple' n) Id >> PartitionEithers
-- >>> pz @(F 3 1) [1..7]
-- PresentT ([[6,7],[7]],[(1,2,3),(2,3,4),(3,4,5),(4,5,6),(5,6,7)])
--
data Tuple' (n :: Nat)

instance ( KnownNat n
         , TupleC n a
         , x ~ [a]
         ) => P (Tuple' n) x where
  type PP (Tuple' n) x = Either x (TupleT n (ExtractAFromList x))
  eval _ opts as =
    let msg0 = "Tuple'(" ++ show n ++ ")"
        n :: Int
        n = nat @n
    in pure $ mkNode opts (PresentT (getTupleC @n as)) msg0 []
