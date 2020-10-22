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
-- | promoted tuple functions
module Predicate.Data.Tuple (

 -- ** arrows
    Dup
  , First
  , Second
  , type (&&&)
  , type (***)
  , Both

 -- ** flat tuples
  , Tuple
  , Tuple'
  , Pairs

 -- ** boolean
  , AndA
  , type (&*)
  , OrA
  , type (|+)

 -- ** inductive tuples
  , EachITuple
  , ToITuple
  , ReverseITuple
  , ToITupleList

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Data.Proxy (Proxy(Proxy))
import GHC.TypeNats (Nat, KnownNat)
import qualified GHC.TypeLits as GL
import Control.Lens

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG
-- >>> :m + Data.Ratio

-- | duplicate a value into a tuple
--
-- >>> pl @Dup 4
-- Present (4,4) ('(4,4))
-- Val (4,4)
--
-- >>> pl @(Dup >> Id) 4
-- Present (4,4) ((>>) (4,4) | {Id (4,4)})
-- Val (4,4)
--
-- >>> pl @(Dup << Fst * Snd) (4,5)
-- Present (20,20) ((>>) (20,20) | {'(20,20)})
-- Val (20,20)
--
-- >>> pl @(Fst * Snd >> Dup) (4,5)
-- Present (20,20) ((>>) (20,20) | {'(20,20)})
-- Val (20,20)
--
data Dup deriving Show
type DupT = W '(Id, Id)

instance Show x => P Dup x where
  type PP Dup x = PP DupT x
  eval _ = eval (Proxy @DupT)

-- | creates a list of overlapping pairs of elements. requires two or more elements
--
-- >>> pz @Pairs [1,2,3,4]
-- Val [(1,2),(2,3),(3,4)]
--
-- >>> pz @Pairs []
-- Val []
--
-- >>> pz @Pairs [1]
-- Val []
--
-- >>> pl @Pairs [1,2]
-- Present [(1,2)] (Pairs [(1,2)] | [1,2])
-- Val [(1,2)]
--
-- >>> pl @Pairs [1,2,3]
-- Present [(1,2),(2,3)] (Pairs [(1,2),(2,3)] | [1,2,3])
-- Val [(1,2),(2,3)]
--
-- >>> pl @Pairs [1,2,3,4]
-- Present [(1,2),(2,3),(3,4)] (Pairs [(1,2),(2,3),(3,4)] | [1,2,3,4])
-- Val [(1,2),(2,3),(3,4)]
--
-- >>> pan @(Pairs >> Len >> 'True >> 'False >> FailT _ "xyzzy") "abcde"
-- [Error xyzzy] False
-- |
-- +- P Pairs [('a','b'),('b','c'),('c','d'),('d','e')]
-- |
-- +- P Len 4
-- |
-- +- True 'True
-- |
-- +- False 'False
-- |
-- `- [Error xyzzy]
-- Fail "xyzzy"
--
data Pairs deriving Show
instance ([a] ~ x, Show a) => P Pairs x where
  type PP Pairs x = [(ExtractAFromTA x,ExtractAFromTA x)]
  eval _ opts as =
    let zs = case as of
               [] -> []
               _:bs -> zip as bs
    in pure $ mkNode opts (Val zs) (show3 opts "Pairs" zs as) []

-- | similar to 'Control.Arrow.&&&'
--
-- >>> pl @(Min &&& Max >> Id >> Fst < Snd) [10,4,2,12,14]
-- True ((>>) True | {2 < 14})
-- Val True
--
-- >>> pl @((123 &&& Id) >> Fst + Snd) 4
-- Present 127 ((>>) 127 | {123 + 4 = 127})
-- Val 127
--
-- >>> pl @(4 &&& "sadf" &&& 'LT) ()
-- Present (4,("sadf",LT)) ('(4,("sadf",LT)))
-- Val (4,("sadf",LT))
--
-- >>> pl @(Id &&& '() &&& ()) (Just 10)
-- Present (Just 10,((),())) ('(Just 10,((),())))
-- Val (Just 10,((),()))
--
-- >>> pl @(Fst &&& Snd &&& Thd &&& ()) (1,'x',True)
-- Present (1,('x',(True,()))) ('(1,('x',(True,()))))
-- Val (1,('x',(True,())))
--
-- >>> pl @(Fst &&& Snd &&& Thd &&& ()) (1,'x',True)
-- Present (1,('x',(True,()))) ('(1,('x',(True,()))))
-- Val (1,('x',(True,())))
--
-- >>> pl @(Fst &&& Snd &&& Thd &&& ()) (1,1.4,"aaa")
-- Present (1,(1.4,("aaa",()))) ('(1,(1.4,("aaa",()))))
-- Val (1,(1.4,("aaa",())))
--
data p &&& q deriving Show
infixr 3 &&&
type WAmpT p q = W '(p, q)

instance P (WAmpT p q) x => P (p &&& q) x where
  type PP (p &&& q) x = PP (WAmpT p q) x
  eval _ = eval (Proxy @(WAmpT p q))

-- | similar to 'Control.Arrow.***'
--
-- >>> pz @(Pred *** ShowP Id) (13, True)
-- Val (12,"True")
--
-- >>> pl @(FlipT (***) Len (Id * 12)) (99,"cdef")
-- Present (1188,4) ((***) (1188,4) | (99,"cdef"))
-- Val (1188,4)
--
-- >>> pl @(4 *** "sadf" *** 'LT) ('x',("abv",[1]))
-- Present (4,("sadf",LT)) ((***) (4,("sadf",LT)) | ('x',("abv",[1])))
-- Val (4,("sadf",LT))
--
data p *** q deriving Show
infixr 3 ***

instance ( Show (PP p a)
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
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right a1 -> do
        qq <- eval (Proxy @q) opts b
        pure $ case getValueLR NoInline opts msg0 qq [hh pp] of
          Left e -> e
          Right b1 -> mkNode opts (Val (a1,b1)) (msg0 <> " " <> showL opts (a1,b1) <> showVerbose opts " | " (a,b)) [hh pp, hh qq]

-- | applies a function against the first part of a tuple: similar to 'Control.Arrow.first'
--
-- >>> pz @(First Succ) (12,True)
-- Val (13,True)
--
data First p deriving Show
type FirstT p = p *** Id

instance P (FirstT p) x => P (First p) x where
  type PP (First p) x = PP (FirstT p) x
  eval _ = eval (Proxy @(FirstT p))

-- | applies a function against the second part of a tuple: similar to 'Control.Arrow.second'
--
-- >>> pz @(Second Succ) (12,False)
-- Val (12,True)
--
data Second q deriving Show
type SecondT q = Id *** q

instance P (SecondT q) x => P (Second q) x where
  type PP (Second q) x = PP (SecondT q) x
  eval _ = eval (Proxy @(SecondT q))

-- | applies @p@ to lhs of the tuple and @q@ to the rhs and then @ands@ them together: see '&*'
--
-- >>> pl @(AndA (Gt 3) (Lt 10) Id) (1,2)
-- False (False (&*) True | (1 > 3))
-- Val False
--
data AndA p q r deriving Show
instance ( PP r x ~ (a,b)
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
    case getValueLR NoInline opts msg0 rr [] of
      Left e -> pure e
      Right (r1,r2) -> do
        pp <- evalBool (Proxy @p) opts r1
        case getValueLR NoInline opts msg0 pp [hh rr] of
          Left e -> pure e
          Right p -> do
            qq <- evalBool (Proxy @q) opts r2
            pure $ case getValueLR NoInline opts msg0 qq [hh rr, hh pp] of
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
-- False ((>>) False | {False (&*) True | (4 > 4)})
-- Val False
--
data p &* q deriving Show
type AndAT p q = AndA p q Id
infixr 3 &*

instance P (AndAT p q) x => P (p &* q) x where
  type PP (p &* q) x = PP (AndAT p q) x
  eval _ = evalBool (Proxy @(AndAT p q))

-- | applies @p@ to lhs of the tuple and @q@ to the rhs and then @ors@ them together: see '|+'
--
-- >>> pl @(OrA (Gt 3) (Lt 10) Id) (1,2)
-- True (False (|+) True)
-- Val True
--
data OrA p q r deriving Show
instance ( PP r x ~ (a,b)
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
    case getValueLR NoInline opts msg0 rr [] of
      Left e -> pure e
      Right (r1,r2) -> do
        pp <- evalBool (Proxy @p) opts r1
        case getValueLR NoInline opts msg0 pp [hh rr] of
          Left e -> pure e
          Right p -> do
            qq <- evalBool (Proxy @q) opts r2
            pure $ case getValueLR NoInline opts msg0 qq [hh rr, hh pp] of
              Left e -> e
              Right q ->
                let zz = case (p,q) of
                          (False,False) -> topMessage pp <> " " <> msg0 <> " " <> topMessage qq
                          _ -> ""
                in mkNodeB opts (p||q) (showL opts p <> " " <> msg0 <> " " <> showL opts q <> nullIf " | " zz) [hh rr, hh pp, hh qq]

-- | applies @p@ to lhs of the tuple and @q@ to the rhs and then @Ors@ them together
--
-- >>> pl @(Sum > 44 |+ Id < 2) ([5,6,7,8,14,44],9)
-- True (True (|+) False)
-- Val True
--
-- >>> pl @(Sum > 44 |+ Id < 2) ([5,6,7,14],9)
-- False (False (|+) False | (32 > 44) (|+) (9 < 2))
-- Val False
--
-- >>> pl @(Sum > 44 |+ Id < 2) ([5,6,7,14],1)
-- True (False (|+) True)
-- Val True
--
data p |+ q deriving Show
type OrAT p q = OrA p q Id
infixr 3 |+

instance P (OrAT p q) x => P (p |+ q) x where
  type PP (p |+ q) x = PP (OrAT p q) x
  eval _ = evalBool (Proxy @(OrAT p q))

-- | applies @p@ to the first and second slot of an n-tuple (similar to '***')
--
-- >>> pl @(Both Len Fst) (("abc",[10..17],1,2,3),True)
-- Present (3,8) (Both)
-- Val (3,8)
--
-- >>> pl @(Both Pred $ Fst) ((12,'z',[10..17]),True)
-- Present (11,'y') (Both)
-- Val (11,'y')
--
-- >>> pl @(Both Succ Id) (4,'a')
-- Present (5,'b') (Both)
-- Val (5,'b')
--
-- >>> pl @(Both Len Fst) (("abc",[10..17]),True)
-- Present (3,8) (Both)
-- Val (3,8)
--
-- >>> import Data.Time
-- >>> pl @(Both (ReadP Day Id) Id) ("1999-01-01","2001-02-12")
-- Present (1999-01-01,2001-02-12) (Both)
-- Val (1999-01-01,2001-02-12)
--
data Both p q deriving Show
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
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        let (a,a') = (extractL1C q, extractL2C q)
        pp <- eval (Proxy @p) opts a
        case getValueLR NoInline opts msg0 pp [hh qq] of
          Left e -> pure e
          Right b -> do
            pp' <- eval (Proxy @p) opts a'
            pure $ case getValueLR NoInline opts msg0 pp' [hh qq, hh pp] of
              Left e -> e
              Right b' ->
                mkNode opts (Val (b,b')) msg0 [hh qq, hh pp, hh pp']

-- | create a n tuple from a list
--
-- >>> pz @(Tuple 4) "abcdefg"
-- Val ('a','b','c','d')
--
-- >>> pz @(Tuple 4) "abc"
-- Fail "Tuple(4) not enough elements(3)"
--
-- >>> pz @(Fst >> Tuple 3) ([1..5],True)
-- Val (1,2,3)
--
-- >>> pz @(Lift (Tuple 3) Fst) ([1..5],True)
-- Val (1,2,3)
--
data Tuple (n :: Nat) deriving Show

instance ( KnownNat n
         , FailWhenT (n GL.<=? 1)
                  ('GL.Text "Tuple:n cannot be less than two but found n="
                   'GL.:<>: 'GL.ShowType n)
         , TupleC n a
         , x ~ [a]
         , Show a
         ) => P (Tuple n) x where
  type PP (Tuple n) x = TupleT n (ExtractAFromList x)
  eval _ opts as =
    let msg0 = "Tuple(" ++ show n ++ ")"
        n = nat @n @Int
    in pure $ case getTupleC @n as of
         Left es -> mkNode opts (Fail (msg0 <> " not enough elements(" <> show (length as) <> ")")) (showVerbose opts " | " es) []
         Right r -> mkNode opts (Val r) msg0 []

-- | create a n tuple from a list
--
-- >>> pz @(Tuple' 4) "abcdefg"
-- Val (Right ('a','b','c','d'))
--
-- >>> pz @(Tuple' 4) "abc"
-- Val (Left "abc")
--
-- >>> pz @(Tuple' 4) []
-- Val (Left [])
--
-- >>> pl @(Tuple' 4) "abc"
-- Present Left "abc" (Tuple'(4) not enough elements(3))
-- Val (Left "abc")
--
-- >>> :set -XPolyKinds
-- >>> type F n i = ChunksOf' n i Id >> Map (Tuple' n) >> PartitionEithers
-- >>> pz @(F 3 1) [1..7]
-- Val ([[6,7],[7]],[(1,2,3),(2,3,4),(3,4,5),(4,5,6),(5,6,7)])
--
data Tuple' (n :: Nat) deriving Show

instance ( KnownNat n
         , FailWhenT (n GL.<=? 1)
                  ('GL.Text "Tuple':n cannot be less than two but found n="
                   'GL.:<>: 'GL.ShowType n)
         , TupleC n a
         , x ~ [a]
         ) => P (Tuple' n) x where
  type PP (Tuple' n) x = Either x (TupleT n (ExtractAFromList x))
  eval _ opts as =
    let msg0 = "Tuple'(" ++ show n ++ ")"
        n = nat @n @Int
        lr = getTupleC @n as
    in pure $ case lr of
         Left e -> mkNode opts (Val (Left e)) (msg0 <> " not enough elements(" <> show (length as) <> ")") []
         Right ret -> mkNode opts (Val (Right ret)) msg0 []

-- | run @p@ with inductive tuples
--
-- >>> pz @(EachITuple Succ) (False,(2,(LT,('c',()))))
-- Val (True,(3,(EQ,('d',()))))
--
-- >>> pz @(EachITuple (Id + (4 >> FromIntegral _))) (1,(1/4,(5%6,())))
-- Val (5 % 1,(17 % 4,(29 % 6,())))
--
-- >>> pz @(ToITuple >> EachITuple (Id + (4 >> FromIntegral _))) (1000,1/4,5%6)
-- Val (1004 % 1,(17 % 4,(29 % 6,())))
--
-- >>> pz @(ToITuple >> EachITuple ((Id >> FromIntegral _) + (4 >> FromIntegral _))) (1000::Integer,17::Int)
-- Val (1004,(21,()))
--
-- >>> pz @(ToITuple >> EachITuple (Dup >> Fst<>Snd)) (SG.Min 1,SG.First 'x',"abcd")
-- Val (Min {getMin = 1},(First {getFirst = 'x'},("abcdabcd",())))
--
data EachITuple p deriving Show

instance ( P p b
         , P (EachITuple p) bs
         ) => P (EachITuple p) (b,bs) where
  type PP (EachITuple p) (b,bs) = (PP p b, PP (EachITuple p) bs)
  eval _ opts (b,bs) = do
    let msg0 = "EachITuple"
    pp <- eval (Proxy @p) opts b
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @(EachITuple p)) opts bs
        pure $ case getValueLR NoInline opts msg0 qq [] of
          Left e -> e
          Right q ->
            qq & ttVal' . _Val .~ (p,q)
               & ttForest %~ (hh pp:)

instance P (EachITuple p) () where
  type PP (EachITuple p) () = ()
  eval _ opts () = do
    let msg0 = "EachITuple"
    pure $ mkNode opts (Val ()) msg0 []

-- | create inductive tuples from flat tuples
--
-- >>> pz @(ToITuple >> EachITuple Succ) (1,2,False,'x')
-- Val (2,(3,(True,('y',()))))
--
data ToITuple deriving Show

instance ToITupleC x => P ToITuple x where
  type PP ToITuple x = ToITupleP x
  eval _ opts x = do
    let msg0 = "ToITuple"
    pure $ mkNode opts (Val (toITupleC x)) msg0 []

-- | reverse an inductive tuple
--
-- >>> pz @ReverseITuple (1.4,(1,(2,(False,('x',())))))
-- Val ('x',(False,(2,(1,(1.4,())))))
--
data ReverseITuple deriving Show

instance P ReverseITuple () where
  type PP ReverseITuple () = ()
  eval _ opts () = do
    let msg0 = "ReverseITuple"
    pure $ mkNode opts (Val ()) msg0 []

instance ReverseITupleC x xs () => P ReverseITuple (x,xs) where
  type PP ReverseITuple (x,xs) = ReverseITupleT x xs ()
  eval _ opts (x,xs) = do
    let msg0 = "ReverseITuple"
    pure $ mkNode opts (Val (reverseITupleC x xs ())) msg0 []

-- | create inductive tuples from a list of the exact size @n@
--
-- >>> pz @(ToITupleList 4 >> EachITuple Succ) ['a','c','y','B']
-- Val ('b',('d',('z',('C',()))))
--
-- >>> pz @(ToITupleList 4) ['a','c','y','B']
-- Val ('a',('c',('y',('B',()))))
--
-- >>> pz @(Take 10 Id >> ToITupleList 10) ['a'..'z']
-- Val ('a',('b',('c',('d',('e',('f',('g',('h',('i',('j',()))))))))))
--
data ToITupleList (n :: Nat) deriving Show

instance ( KnownNat n
         , FailWhenT (n GL.<=? 0)
                  ('GL.Text "ToITupleList:n cannot be 0")
         , ToITupleListC n a
         , xs ~ [a]
         ) => P (ToITupleList n) xs where
  type PP (ToITupleList n) xs = ToITupleListP n (ExtractAFromTA xs)
  eval _ opts xs =
    let msg0 = "ToITupleList(" <> show n <> ")"
        n = nat @n @Int
    in pure $ case toITupleListC @n @a xs of
         Left e -> mkNode opts (Fail e) (msg0 <> " instead found " <> show (length xs)) []
         Right d -> mkNode opts (Val d) msg0 []
