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
  , On
  , On'
  , Uncurry

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
  , FromITuple
  , ReverseITuple
  , ToITupleList

 -- ** assoc
  , Assoc
  , Unassoc
 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Data.Proxy (Proxy(Proxy))
import GHC.TypeNats (Nat, KnownNat)
import qualified GHC.TypeLits as GL
import Control.Lens
import Data.Kind (Type)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG
-- >>> :m + Data.These
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
-- >>> pl @(Flip (***) Len (Id * 12)) (99,"cdef")
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
                in mkNodeB opts (p&&q) (showL opts p <> " " <> msg0 <> " " <> joinStrings (showL opts q) zz) [hh rr, hh pp, hh qq]

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
                in mkNodeB opts (p||q) (showL opts p <> " " <> msg0 <> " " <> joinStrings (showL opts q) zz) [hh rr, hh pp, hh qq]

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
-- >>> pl @(Fst >> Both Len) (("abc",[10..17]),True)
-- Present (3,8) ((>>) (3,8) | {Both})
-- Val (3,8)
--
-- >>> pl @(Lift (Both Pred) Fst) ((12,'z'),True)
-- Present (11,'y') ((>>) (11,'y') | {Both})
-- Val (11,'y')
--
-- >>> pl @(Both Succ) (4,'a')
-- Present (5,'b') (Both)
-- Val (5,'b')
--
-- >>> import Data.Time
-- >>> pl @(Both (ReadP Day Id)) ("1999-01-01","2001-02-12")
-- Present (1999-01-01,2001-02-12) (Both)
-- Val (1999-01-01,2001-02-12)
--
-- >>> pz @(Both (Id * Id) >> ((Fst + Snd) ** (DivI Double 1 2))) (3,4)
-- Val 5.0
--
data Both p deriving Show
instance ( P p a
         , P p a'
   ) => P (Both p) (a,a') where
  type PP (Both p) (a,a') = (PP p a, PP p a')
  eval _ opts (a,a') = do
    let msg0 = "Both"
    pp <- eval (Proxy @p) opts a
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right b -> do
        pp' <- eval (Proxy @p) opts a'
        pure $ case getValueLR NoInline opts msg0 pp' [hh pp] of
          Left e -> e
          Right b' ->
            mkNode opts (Val (b,b')) msg0 [hh pp, hh pp']

-- | similar to 'Data.Function.on'
--
-- >>> pz @(On' (==!) Len Fst (Reverse << Snd)) ("1ss","x2")
-- Val GT
--
data On' (p :: Type -> Type -> k) q r s deriving Show

instance ( P q (PP r x)
         , P q (PP s x)
         , P r x
         , P s x
         , P (p Fst Snd) (PP q (PP r x), PP q (PP s x))
   ) => P (On' p q r s) x where
  type PP (On' p q r s) x = PP (p Fst Snd) (PP q (PP r x), PP q (PP s x))
  eval _ opts x = do
    let msg0 = "On'"
    lr <- runPQ NoInline msg0 (Proxy @r) (Proxy @s) opts x []
    case lr of
      Left e -> pure e
      Right (r,s,rr,ss) -> do
        let hhs = [hh rr, hh ss]
        qq <- eval (Proxy @q) opts r
        case getValueLR NoInline opts msg0 qq hhs of
          Left e -> pure e
          Right b -> do
            qq' <- eval (Proxy @q) opts s
            case getValueLR NoInline opts msg0 qq' (hhs ++ [hh qq]) of
              Left e -> pure e
              Right b' -> do
                pp <- eval (Proxy @(p Fst Snd)) opts (b,b')
                let hhs1 = hhs ++ [hh qq, hh qq']
                pure $ case getValueLR NoInline opts msg0 pp hhs1 of
                  Left e -> e
                  Right _ -> mkNodeCopy opts pp msg0 hhs1 -- so we can preserve ValP

-- | similar to 'Data.Function.on' for tuples
--
-- >>> pz @(On (==!) Len) ("1ss","x2") -- or use Comparing or Both+Compare
-- Val GT
--
-- >>> pz @('(4,2) >> On (**) (FromIntegral _)) ()
-- Val 16.0
--
-- >>> pz @('(4,2) >> Both (FromIntegral _) >> Fst ** Snd) () -- equivalent to the above
-- Val 16.0
--
-- >>> pz @(On (+) (Id * Id) >> Id ** (1 % 2 >> FromRational _)) (3,4)
-- Val 5.0
--
-- >>> pz @(Both (Id * Id) >> ((Fst + Snd) ** (1 % 2 >> FromRational _))) (3,4) -- equivalent to the above
-- Val 5.0
--
-- >>> pz @(On (<>) (Pure [] Id)) ('x','y')
-- Val "xy"
--
-- >>> pz @(On (Flip (<>)) (Pure [] Id)) ('x','y')
-- Val "yx"
--
-- >>> pz @(On (Flip (<>)) (Pure _ Id) >> '(Len,Head,Last)) ('x','y')
-- Val (2,'y','x')
--
data On (p :: Type -> Type -> k) q deriving Show
type OnT p q = On' p q Fst Snd

instance P (OnT p q) x => P (On p q) x where
  type PP (On p q) x = PP (OnT p q) x
  eval _ = eval (Proxy @(OnT p q))

-- | similar to 'uncurry'
--
-- >>> pl @(Uncurry (==)) ('x','x')
-- True (On')
-- Val True
--
-- >>> pz @(Uncurry (.&.)) (7,15)
-- Val 7
--
-- >>> pz @(Uncurry (+)) (7,15)
-- Val 22
--
-- >>> pz @(Uncurry (*)) (7,15)
-- Val 105
--
-- >>> pz @(Uncurry '(,)) (7,15)
-- Val (7,15)
--
-- >>> pz @(Uncurry ('(,,) 4)) ('x',True)
-- Val (4,'x',True)
--
-- >>> pz @(Uncurry (Flip '(,))) (1,'x')
-- Val ('x',1)
--
-- >>> pz @(Uncurry (<>)) ("ab","def")
-- Val "abdef"
--
-- >>> pz @(Uncurry (<>)) (SG.Sum 12,SG.Sum 99)
-- Val (Sum {getSum = 111})
--
data Uncurry (p :: Type -> Type -> k) deriving Show
type UncurryT p = On p Id

instance P (UncurryT p) x => P (Uncurry p) x where
  type PP (Uncurry p) x = PP (UncurryT p) x
  eval _ = eval (Proxy @(UncurryT p))

-- | create a @n@ tuple from a list or fail
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
  eval _ opts xs' =
    let msg0 = "Tuple(" ++ show n ++ ")"
        n = nat @n @Int
    in pure $ case chkSize opts msg0 xs' [] of
        Left e -> e
        Right (xsLen,xs) ->
          case getTupleC @n xs of
            Nothing -> mkNode opts (Fail (msg0 <> " not enough elements(" <> show xsLen <> ")")) (showVerbose opts " | " xs) []
            Just r -> mkNode opts (Val r) msg0 []

-- | create a @n@ tuple from a list and return as an Either
--
-- >>> pz @(Tuple' 4) "abcdefg"
-- Val (Right ('a','b','c','d'))
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
  eval _ opts xs' =
    let msg0 = "Tuple'(" ++ show n ++ ")"
        n = nat @n @Int
    in pure $ case chkSize opts msg0 xs' [] of
      Left e -> e
      Right (xsLen,xs) ->
        let lr = getTupleC @n xs
        in case lr of
             Nothing -> mkNode opts (Val (Left xs)) (msg0 <> " not enough elements(" <> show xsLen <> ")") []
             Just ret -> mkNode opts (Val (Right ret)) msg0 []

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
            qq & ttVal .~ Val (p,q)
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

-- | create flat tuples from inductive tuples
--
-- >>> pz @FromITuple (2,(3,(True,('y',()))))
-- Val (2,3,True,'y')
--
-- >>> pz @FromITuple ()
-- Val ()
--
-- >>> pz @FromITuple (1,())
-- Val 1
--
data FromITuple deriving Show

instance FromITupleC x => P FromITuple x where
  type PP FromITuple x = FromITupleP x
  eval _ opts x = do
    let msg0 = "FromITuple"
    pure $ mkNode opts (Val (fromITupleC x)) msg0 []

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
  eval _ opts xs' =
    let msg0 = "ToITupleList(" <> show n <> ")"
        n = nat @n @Int
    in pure $ case chkSize opts msg0 xs' [] of
         Left e -> e
         Right (xsLen,xs) ->
           case toITupleListC @n @a xs of
             Left e -> mkNode opts (Fail e) (msg0 <> " instead found " <> show xsLen) []
             Right d -> mkNode opts (Val d) msg0 []

-- | assoc using 'AssocC'
--
-- >>> pz @Assoc (This (These 123 'x'))
-- Val (These 123 (This 'x'))
--
-- >>> pz @Assoc ((99,'a'),True)
-- Val (99,('a',True))
--
-- >>> pz @Assoc ((99,'a'),True)
-- Val (99,('a',True))
--
-- >>> pz @Assoc (Right "Abc" :: Either (Either () ()) String)
-- Val (Right (Right "Abc"))
--
-- >>> pz @Assoc (Left (Left 'x'))
-- Val (Left 'x')
--
-- >>> pl @Assoc ((10,'c'),True)
-- Present (10,('c',True)) (Assoc (10,('c',True)) | ((10,'c'),True))
-- Val (10,('c',True))
--
-- >>> pl @(Assoc >> Unassoc) ((10,'c'),True)
-- Present ((10,'c'),True) ((>>) ((10,'c'),True) | {Unassoc ((10,'c'),True) | (10,('c',True))})
-- Val ((10,'c'),True)
--
data Assoc deriving Show

instance ( AssocC p
         , Show (p (p a b) c)
         , Show (p a (p b c))
         ) => P Assoc (p (p a b) c) where
  type PP Assoc (p (p a b) c) = p a (p b c)
  eval _ opts pabc =
    let msg0 = "Assoc"
        d = assoc pabc
    in pure $ mkNode opts (Val d) (show3 opts msg0 d pabc) []

-- | unassoc using 'AssocC'
--
-- >>> pz @Unassoc (These 123 (This 'x'))
-- Val (This (These 123 'x'))
--
-- >>> pz @Unassoc (99,('a',True))
-- Val ((99,'a'),True)
--
-- >>> pz @Unassoc (This 10 :: These Int (These Bool ()))
-- Val (This (This 10))
--
-- >>> pz @Unassoc (Right (Right 123))
-- Val (Right 123)
--
-- >>> pz @Unassoc (Left 'x' :: Either Char (Either Bool Double))
-- Val (Left (Left 'x'))
--
-- >>> pl @Unassoc (10,('c',True))
-- Present ((10,'c'),True) (Unassoc ((10,'c'),True) | (10,('c',True)))
-- Val ((10,'c'),True)
--
data Unassoc deriving Show

instance ( AssocC p
         , Show (p (p a b) c)
         , Show (p a (p b c))
         ) => P Unassoc (p a (p b c)) where
  type PP Unassoc (p a (p b c)) = p (p a b) c
  eval _ opts pabc =
    let msg0 = "Unassoc"
        d = unassoc pabc
    in pure $ mkNode opts (Val d) (show3 opts msg0 d pabc) []

