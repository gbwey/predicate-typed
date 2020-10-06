{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# OPTIONS -Wunused-type-patterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
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
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
     Utility methods for Predicate / methods for displaying the evaluation tree
-}
module Predicate.Misc (
  -- ** useful type families
    ZwischenT
  , FailWhenT
  , FailUnlessT
  , AndT
  , OrT
  , NotT
  , RepeatT
  , IntersperseT
  , LenT
  , FlipT
  , IfT
  , SumT
  , MapT
  , ConsT
  , type (%%)
  , type (%&)
  , type (<%>)
  , AnyT
  , ExtractAFromList
  , ExtractAFromTA
  , ExtractTFromTA
  , MaybeT
  , LeftT
  , RightT
  , ThisT
  , ThatT
  , TheseT
  , FnT
  , ApplyConstT
  , CheckT
  , JoinT

 -- ** extract values from the type level
  , GetBool(..)
  , GetLen(..)
  , GetThese(..)
  , GetOrdering(..)
  , OrderingP(..)
  , GetOrd(..)
  , InductTupleC(..)
  , InductListC(..)
  , TupleC(..)

 -- ** extract from n-tuple
  , T4_1
  , T4_2
  , T4_3
  , T4_4
  , T5_1
  , T5_2
  , T5_3
  , T5_4
  , T5_5

 -- ** tuple classes
  , ExtractL1C(..)
  , ExtractL2C(..)
  , ExtractL3C(..)
  , ExtractL4C(..)
  , ExtractL5C(..)
  , ExtractL6C(..)

 -- ** primes
  , isPrime
  , primes
  , primeFactors

  -- ** regular expressions
  , ROpt(..)
  , GetROpts(..)
  , RReplace(..)
  , GetReplaceFnSub(..)
  , ReplaceFnSub(..)
  , displayROpts

  -- ** colors
  , SColor(..)
  , GetColor(..)

 -- ** miscellaneous
  , showTK
  , showT
  , showThese
  , prettyOrd
  , nat
  , symb
  , errorInProgram
  , readField
  , unlessNull
  , unlessNullM
  , nullSpace
  , nullIf
  , pureTryTest
  , pureTryTestPred
  , (~>)

  ) where
import qualified GHC.TypeNats as GN
import GHC.TypeLits (Symbol,Nat,KnownSymbol,KnownNat,ErrorMessage((:$$:),(:<>:)))
import qualified GHC.TypeLits as GL
import Data.Typeable (Typeable, Proxy(Proxy), typeRep)
import System.Console.Pretty (Color(..))
import GHC.Exts (Constraint)
import qualified Text.Regex.PCRE.Light as RL
import qualified Data.Text as T
import GHC.Word (Word8)
import Data.Sequence (Seq)
import Control.Applicative (ZipList)
import Data.Kind (Type)
import Data.These (These(..))
import Data.These.Combinators (isThis, isThat, isThese)
import Data.List.NonEmpty (NonEmpty(..))
import Data.ByteString (ByteString)
import GHC.Stack (HasCallStack)
import Data.Containers.ListUtils (nubOrd)
import Control.Arrow (Arrow((***)),ArrowChoice(left))
import Data.List (intercalate)
import qualified Safe (headNote)
import qualified Text.Read.Lex as L
import qualified Text.ParserCombinators.ReadPrec as PCR
import qualified GHC.Read as GR
import Data.Char (isSpace)
import qualified Control.Exception as E
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators

-- | type level Between
type family ZwischenT (a :: Nat) (b :: Nat) (v :: Nat) :: Constraint where
  ZwischenT m n v =
     FailUnlessT (AndT (m GL.<=? v) (v GL.<=? n))
            ('GL.Text "ZwischenT failure"
             ':$$: 'GL.ShowType v
             ':$$: 'GL.Text " is outside of "
             ':$$: 'GL.ShowType m
             ':<>: 'GL.Text " and "
             ':<>: 'GL.ShowType n)

-- | helper method that fails with a msg when True
type family FailWhenT (b :: Bool) (msg :: GL.ErrorMessage) :: Constraint where
  FailWhenT 'False _ = ()
  FailWhenT 'True e = GL.TypeError e

-- | helper method that fails with msg when False
type family FailUnlessT (b :: Bool) (msg :: GL.ErrorMessage) :: Constraint where
  FailUnlessT 'True _ = ()
  FailUnlessT 'False e = GL.TypeError e

-- | typelevel boolean And
type family AndT (b :: Bool) (b1 :: Bool) :: Bool where
  AndT 'False _ = 'False
  AndT 'True b1 = b1

-- | typelevel boolean Or
type family OrT (b :: Bool) (b1 :: Bool) :: Bool where
  OrT 'True _ = 'True
  OrT 'False b1 = b1

-- | typelevel boolean Not
type family NotT (b :: Bool) :: Bool where
  NotT 'True = 'False
  NotT 'False = 'True

-- | get the length of a typelevel container
--
-- >>> getLen @'["abc","def","g"]
-- 3
--
-- >>> getLen @'[]
-- 0
--
-- >>> getLen @(9 ':| '[1,2,3])
-- 4
--
-- >>> getLen @('These 9 "Asfs")
-- 1
--
-- >>> getLen @('This 1)
-- 0
--
class GetLen xs where -- (xs :: [k]) will break it! ghc 8.6.5
  getLen :: Int
instance GetLen '[] where
  getLen = 0
instance GetLen xs => GetLen (x ': xs) where
  getLen = 1 + getLen @xs
instance GetLen ('Just a) where
  getLen = 1
instance GetLen 'Nothing where
  getLen = 0
instance GetLen ('Left a) where
  getLen = 0
instance GetLen ('Right a) where
  getLen = 1
instance GetLen ('This a) where
  getLen = 0
instance GetLen ('That a) where
  getLen = 1
instance GetLen ('These a b) where
  getLen = 1
instance GetLen xs => GetLen (x ':| xs) where
  getLen = 1 + getLen @xs

-- | display constructor name for 'These'
showThese :: These a b -> String
showThese = \case
  This {} -> "This"
  That {} -> "That"
  These {} -> "These"

-- | get 'These' from typelevel
class GetThese th where
  getThese :: (String, These w v -> Bool)
instance GetThese ('This x) where
  getThese = ("This", isThis)
instance GetThese ('That y) where
  getThese = ("That", isThat)
instance GetThese ('These x y) where
  getThese = ("These", isThese)

-- | get ordering from the typelevel
class GetOrdering (cmp :: Ordering) where
  getOrdering :: Ordering
instance GetOrdering 'LT where
  getOrdering = LT
instance GetOrdering 'EQ where
  getOrdering = EQ
instance GetOrdering 'GT where
  getOrdering = GT

-- | all the ways to compare two values
data OrderingP = CGt | CGe | CEq | CLe | CLt | CNe
  deriving stock (Read, Show, Eq, Enum, Bounded)

-- | extract 'OrderingP' from the typelevel
class GetOrd (k :: OrderingP) where
  getOrd :: Ord a => (String, a -> a -> Bool)

instance GetOrd 'CGt where getOrd = (">", (>))
instance GetOrd 'CGe where getOrd = (">=",(>=))
instance GetOrd 'CEq where getOrd = ("==",(==))
instance GetOrd 'CLe where getOrd = ("<=",(<=))
instance GetOrd 'CLt where getOrd = ("<", (<))
instance GetOrd 'CNe where getOrd = ("/=",(/=))

-- | show the type as a string
showT :: forall (t :: Type) . Typeable t => String
showT = show (typeRep (Proxy @t))

-- | Repeat an expression n times
type family RepeatT (n :: Nat) (p :: k) :: [k] where
  RepeatT 0 _p = GL.TypeError ('GL.Text "RepeatT is not defined for zero")
  RepeatT 1 p = p ': '[]
  RepeatT n p = p ': RepeatT (n GN.- 1) p

type s <%> t = GL.AppendSymbol s t
infixr 7 <%>

-- | Intersperse a symbol inside a list of symbols
type family IntersperseT (s :: Symbol) (xs :: [Symbol]) :: Symbol where
  IntersperseT _s '[] = ""
  IntersperseT _s '[x] = x
  IntersperseT s (x ': y ': xs) = x <%> s <%> IntersperseT s (y ': xs)

-- | length of a type level list
type family LenT (xs :: [k]) :: Nat where
  LenT '[] = 0
  LenT (_x ': xs) = 1 GN.+ LenT xs

-- | takes a flat n-tuple and creates a reversed inductive tuple. see 'Predicate.Data.ReadShow.PrintT'
--
-- >>> inductTupleC (123,'x',False,"abc")
-- ("abc",(False,('x',(123,()))))
--
-- >>> inductTupleC (123,'x')
-- ('x',(123,()))
--
class InductTupleC x where
  type InductTupleP x
  inductTupleC :: x -> InductTupleP x
instance (GL.TypeError ('GL.Text "InductTupleC: inductive tuple cannot be empty")) => InductTupleC () where
  type InductTupleP () = ()
  inductTupleC () = ()
instance InductTupleC (a,b) where
  type InductTupleP (a,b) = (b,(a,()))
  inductTupleC (a,b) = (b,(a,()))
instance InductTupleC (a,b,c) where
  type InductTupleP (a,b,c) = (c,(b,(a,())))
  inductTupleC (a,b,c) = (c,(b,(a,())))
instance InductTupleC (a,b,c,d) where
  type InductTupleP (a,b,c,d) = (d,(c,(b,(a,()))))
  inductTupleC (a,b,c,d) = (d,(c,(b,(a,()))))
instance InductTupleC (a,b,c,d,e) where
  type InductTupleP (a,b,c,d,e) = (e,(d,(c,(b,(a,())))))
  inductTupleC (a,b,c,d,e) = (e,(d,(c,(b,(a,())))))
instance InductTupleC (a,b,c,d,e,f) where
  type InductTupleP (a,b,c,d,e,f) = (f,(e,(d,(c,(b,(a,()))))))
  inductTupleC (a,b,c,d,e,f) = (f,(e,(d,(c,(b,(a,()))))))
instance InductTupleC (a,b,c,d,e,f,g) where
  type InductTupleP (a,b,c,d,e,f,g) = (g,(f,(e,(d,(c,(b,(a,())))))))
  inductTupleC (a,b,c,d,e,f,g) = (g,(f,(e,(d,(c,(b,(a,())))))))
instance InductTupleC (a,b,c,d,e,f,g,h) where
  type InductTupleP (a,b,c,d,e,f,g,h) = (h,(g,(f,(e,(d,(c,(b,(a,()))))))))
  inductTupleC (a,b,c,d,e,f,g,h) = (h,(g,(f,(e,(d,(c,(b,(a,()))))))))
instance InductTupleC (a,b,c,d,e,f,g,h,i) where
  type InductTupleP (a,b,c,d,e,f,g,h,i) = (i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))
  inductTupleC (a,b,c,d,e,f,g,h,i) = (i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))
instance InductTupleC (a,b,c,d,e,f,g,h,i,j) where
  type InductTupleP (a,b,c,d,e,f,g,h,i,j) = (j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))
  inductTupleC (a,b,c,d,e,f,g,h,i,j) = (j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))
instance InductTupleC (a,b,c,d,e,f,g,h,i,j,k) where
  type InductTupleP (a,b,c,d,e,f,g,h,i,j,k) = (k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))))
  inductTupleC (a,b,c,d,e,f,g,h,i,j,k) = (k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))))
instance InductTupleC (a,b,c,d,e,f,g,h,i,j,k,l) where
  type InductTupleP (a,b,c,d,e,f,g,h,i,j,k,l) = (l,(k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))))
  inductTupleC (a,b,c,d,e,f,g,h,i,j,k,l) = (l,(k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))))

-- | takes a list and converts to a reversed inductive tuple. see 'Predicate.Data.ReadShow.PrintL'
--
-- >>> inductListC @4 [10,12,13,1]
-- (1,(13,(12,(10,()))))
--
-- >>> inductListC @2 ["ab","cc"]
-- ("cc",("ab",()))
--
class InductListC (n :: Nat) a where
  type InductListP n a
  inductListC :: [a] -> InductListP n a
instance (GL.TypeError ('GL.Text "InductListC: inductive tuple cannot be empty")) => InductListC 0 a where
  type InductListP 0 a = ()
  inductListC _ = errorInProgram "InductListC 0: shouldnt be called"
instance InductListC 1 a where
  type InductListP 1 a = (a,())
  inductListC [a] = (a,())
  inductListC _ = errorInProgram "inductListC: expected 1 value"
instance InductListC 2 a where
  type InductListP 2 a = (a,(a,()))
  inductListC [a,b] = (b,(a,()))
  inductListC _ = errorInProgram "inductListC: expected 2 values"
instance InductListC 3 a where
  type InductListP 3 a = (a,(a,(a,())))
  inductListC [a,b,c] = (c,(b,(a,())))
  inductListC _ = errorInProgram "inductListC: expected 3 values"
instance InductListC 4 a where
  type InductListP 4 a = (a,(a,(a,(a,()))))
  inductListC [a,b,c,d] = (d,(c,(b,(a,()))))
  inductListC _ = errorInProgram "inductListC: expected 4 values"
instance InductListC 5 a where
  type InductListP 5 a = (a,(a,(a,(a,(a,())))))
  inductListC [a,b,c,d,e] = (e,(d,(c,(b,(a,())))))
  inductListC _ = errorInProgram "inductListC: expected 5 values"
instance InductListC 6 a where
  type InductListP 6 a = (a,(a,(a,(a,(a,(a,()))))))
  inductListC [a,b,c,d,e,f] = (f,(e,(d,(c,(b,(a,()))))))
  inductListC _ = errorInProgram "inductListC: expected 6 values"
instance InductListC 7 a where
  type InductListP 7 a = (a,(a,(a,(a,(a,(a,(a,())))))))
  inductListC [a,b,c,d,e,f,g] = (g,(f,(e,(d,(c,(b,(a,())))))))
  inductListC _ = errorInProgram "inductListC: expected 7 values"
instance InductListC 8 a where
  type InductListP 8 a = (a,(a,(a,(a,(a,(a,(a,(a,()))))))))
  inductListC [a,b,c,d,e,f,g,h] = (h,(g,(f,(e,(d,(c,(b,(a,()))))))))
  inductListC _ = errorInProgram "inductListC: expected 8 values"
instance InductListC 9 a where
  type InductListP 9 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,())))))))))
  inductListC [a,b,c,d,e,f,g,h,i] = (i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))
  inductListC _ = errorInProgram "inductListC: expected 9 values"
instance InductListC 10 a where
  type InductListP 10 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,(a,()))))))))))
  inductListC [a,b,c,d,e,f,g,h,i,j] = (j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))
  inductListC _ = errorInProgram "inductListC: expected 10 values"
instance InductListC 11 a where
  type InductListP 11 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,(a,(a,())))))))))))
  inductListC [a,b,c,d,e,f,g,h,i,j,k] = (k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))))
  inductListC _ = errorInProgram "inductListC: expected 11 values"
instance InductListC 12 a where
  type InductListP 12 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,(a,(a,(a,()))))))))))))
  inductListC [a,b,c,d,e,f,g,h,i,j,k,l] = (l,(k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))))
  inductListC _ = errorInProgram "inductListC: expected 12 values"

-- partially apply the 2nd arg to an ADT -- $ and & work with functions only
-- doesnt apply more than once because we need to eval it
type family (p :: k -> k1) %% (q :: k) :: k1 where
  p %% q = p q

infixl 9 %%

type family (p :: k) %& (q :: k -> k1) :: k1 where
  p %& q = q p

infixr 9 %&

-- | 'flip' at the type level
type family FlipT (d :: k1 -> k -> k2) (p :: k) (q :: k1) :: k2 where
  FlipT d p q = d q p

-- | 'if' at the type level
type family IfT (b :: Bool) (t :: k) (f :: k) :: k where
  -- IfT b x x = x -- todo: benefit? now it needs to eval both sides
  IfT 'True t _f = t
  IfT 'False _t f = f

-- | 'sum' at the type level for a list of 'Nat'
type family SumT (ns :: [Nat]) :: Nat where
  SumT '[] = 0
  SumT (n ': ns) = n GL.+ SumT ns

-- only works if you use ADTs not type synonyms
-- | 'map' at the type level
type family MapT (f :: k -> k1) (xs :: [k]) :: [k1] where
  MapT _f '[] = '[]
  MapT f (x ': xs) = f x ': MapT f xs

-- | Extract @a@ from a list-like container
type family ConsT s where
  ConsT [a] = a
  ConsT (ZipList a) = a
  ConsT T.Text = Char
  ConsT ByteString = Word8
  ConsT (Seq a) = a
  ConsT s  = GL.TypeError (
      'GL.Text "invalid ConsT instance"
      ':$$: 'GL.Text "s = "
      ':<>: 'GL.ShowType s)

-- | extract @opts@ part of 4 tuple from the type level for use with 'Predicate.Refined2.Refined2'
type family T4_1 x where
  T4_1 '(opts,_,_,_) = opts
-- | extract @ip@ part of 4 tuple from the type level for use with 'Predicate.Refined2.Refined2'
type family T4_2 x where
  T4_2 '(_,ip,_,_) = ip
-- | extract @op@ part of 4 tuple from the type level for use with 'Predicate.Refined2.Refined2'
type family T4_3 x where
  T4_3 '(_,_,op,_) = op
-- | extract @i@ part of 4 tuple from the type level for use with 'Predicate.Refined2.Refined2'
type family T4_4 x where
  T4_4 '(_,_,_,i) = i

-- | extract @opts@ part of 5 tuple from the type level for use with 'Predicate.Refined3.Refined3'
type family T5_1 x where
  T5_1 '(opts,_,_,_,_) = opts
-- | extract @ip@ part of 5 tuple from the type level for use with 'Predicate.Refined3.Refined3'
type family T5_2 x where
  T5_2 '(_,ip,_,_,_) = ip
-- | extract @op@ part of 5 tuple from the type level for use with 'Predicate.Refined3.Refined3'
type family T5_3 x where
  T5_3 '(_,_,op,_,_) = op
-- | extract @fmt@ part of 5 tuple from the type level for use with 'Predicate.Refined3.Refined3'
type family T5_4 x where
  T5_4 '(_,_,_,fmt,_) = fmt
-- | extract @i@ part of 5 tuple from the type level for use with 'Predicate.Refined3.Refined3'
type family T5_5 x where
  T5_5 '(_,_,_,_,i) = i

-- | represents any kind
type family AnyT :: k where {}

-- | type family to extract @a@ from @t a@
type family ExtractAFromTA (ta :: Type) :: Type where
  ExtractAFromTA (_t a) = a
  ExtractAFromTA z = GL.TypeError (
      'GL.Text "ExtractAFromTA: expected (t a) but found something else"
      ':$$: 'GL.Text "t a = "
      ':<>: 'GL.ShowType z)

-- | type family to extract @t@ from @t a@
type family ExtractTFromTA (ta :: Type) :: (Type -> Type) where
  ExtractTFromTA (t _a) = t
  ExtractTFromTA z = GL.TypeError (
      'GL.Text "ExtractTFromTA: expected (t a) but found something else"
      ':$$: 'GL.Text "t a = "
      ':<>: 'GL.ShowType z)


-- todo: get ExtractAFromList failure to fire if wrong Type
-- | type family to extract @a@ from a list of @a@
type family ExtractAFromList (as :: Type) :: Type where
  ExtractAFromList [a] = a
  ExtractAFromList z = GL.TypeError (
      'GL.Text "ExtractAFromList: expected [a] but found something else"
      ':$$: 'GL.Text "as = "
      ':<>: 'GL.ShowType z)

type family MaybeT mb where
  MaybeT (Maybe a) = a
  MaybeT o = GL.TypeError (
      'GL.Text "MaybeT: expected 'Maybe a' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)


type family LeftT lr where
  LeftT (Either a _) = a
  LeftT o = GL.TypeError (
      'GL.Text "LeftT: expected 'Either a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family RightT lr where
  RightT (Either _a b) = b
  RightT o = GL.TypeError (
      'GL.Text "RightT: expected 'Either a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family ThisT lr where
  ThisT (These a _b) = a
  ThisT o = GL.TypeError (
      'GL.Text "ThisT: expected 'These a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family ThatT lr where
  ThatT (These _a b) = b
  ThatT o = GL.TypeError (
      'GL.Text "ThatT: expected 'These a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family TheseT lr where
  TheseT (These a b) = (a,b)
  TheseT o = GL.TypeError (
      'GL.Text "TheseT: expected 'These a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family FnT ab :: Type where
  FnT (_a -> b) = b
  FnT ab = GL.TypeError (
      'GL.Text "FnT: expected Type -> Type but found a simple Type?"
      ':$$: 'GL.Text "ab = "
      ':<>: 'GL.ShowType ab)

type family JoinT x y where
  JoinT (t a) (t b) = t (a, b)
  JoinT ta tb = GL.TypeError (
       'GL.Text "JoinT: expected (t a) (t b) but found something else"
       ':$$: 'GL.Text "t a = "
       ':<>: 'GL.ShowType ta
       ':$$: 'GL.Text "t b = "
       ':<>: 'GL.ShowType tb)

type family ApplyConstT (ta :: Type) (b :: Type) :: Type where
--type family ApplyConstT ta b where -- less restrictive so allows ('Just Int) Bool through!
  ApplyConstT (t _a) b = t b
  ApplyConstT ta b = GL.TypeError (
       'GL.Text "ApplyConstT: (t a) b but found something else"
       ':$$: 'GL.Text "t a = "
       ':<>: 'GL.ShowType ta
       ':$$: 'GL.Text "b = "
       ':<>: 'GL.ShowType b)

type family CheckT (tp :: Type) :: Bool where
  CheckT () = GL.TypeError ('GL.Text "Printfn: inductive tuple cannot be empty")
  CheckT _o = 'True

errorInProgram :: HasCallStack => String -> x
errorInProgram s = error $ "programmer error:" <> s

-- | boolean implication
--
-- >>> True ~> False
-- False
--
-- >>> True ~> True
-- True
--
-- >>> False ~> False
-- True
--
-- >>> False ~> True
-- True
--
(~>) :: Bool -> Bool -> Bool
p ~> q = not p || q
infixr 1 ~>


class ExtractL1C tp where
  type ExtractL1T tp
  extractL1C :: tp -> ExtractL1T tp
instance ExtractL1C (a,b) where
  type ExtractL1T (a,b) = a
  extractL1C (a,_) = a
instance ExtractL1C (a,b,c) where
  type ExtractL1T (a,b,c) = a
  extractL1C (a,_,_) = a
instance ExtractL1C (a,b,c,d) where
  type ExtractL1T (a,b,c,d) = a
  extractL1C (a,_,_,_) = a
instance ExtractL1C (a,b,c,d,e) where
  type ExtractL1T (a,b,c,d,e) = a
  extractL1C (a,_,_,_,_) = a
instance ExtractL1C (a,b,c,d,e,f) where
  type ExtractL1T (a,b,c,d,e,f) = a
  extractL1C (a,_,_,_,_,_) = a

class ExtractL2C tp where
  type ExtractL2T tp
  extractL2C :: tp -> ExtractL2T tp
instance ExtractL2C (a,b) where
  type ExtractL2T (a,b) = b
  extractL2C (_,b) = b
instance ExtractL2C (a,b,c) where
  type ExtractL2T (a,b,c) = b
  extractL2C (_,b,_) = b
instance ExtractL2C (a,b,c,d) where
  type ExtractL2T (a,b,c,d) = b
  extractL2C (_,b,_,_) = b
instance ExtractL2C (a,b,c,d,e) where
  type ExtractL2T (a,b,c,d,e) = b
  extractL2C (_,b,_,_,_) = b
instance ExtractL2C (a,b,c,d,e,f) where
  type ExtractL2T (a,b,c,d,e,f) = b
  extractL2C (_,b,_,_,_,_) = b
class ExtractL3C tp where
  type ExtractL3T tp
  extractL3C :: tp -> ExtractL3T tp
instance ExtractL3C (a,b) where
  type ExtractL3T (a,b) = GL.TypeError ('GL.Text "Thd doesn't work for 2-tuples")
  extractL3C _ = errorInProgram "Thd doesn't work for 2-tuples"
instance ExtractL3C (a,b,c) where
  type ExtractL3T (a,b,c) = c
  extractL3C (_,_,c) = c
instance ExtractL3C (a,b,c,d) where
  type ExtractL3T (a,b,c,d) = c
  extractL3C (_,_,c,_) = c
instance ExtractL3C (a,b,c,d,e) where
  type ExtractL3T (a,b,c,d,e) = c
  extractL3C (_,_,c,_,_) = c
instance ExtractL3C (a,b,c,d,e,f) where
  type ExtractL3T (a,b,c,d,e,f) = c
  extractL3C (_,_,c,_,_,_) = c

class ExtractL4C tp where
  type ExtractL4T tp
  extractL4C :: tp -> ExtractL4T tp
instance ExtractL4C (a,b) where
  type ExtractL4T (a,b) = GL.TypeError ('GL.Text "L4 doesn't work for 2-tuples")
  extractL4C _ = errorInProgram "L4 doesn't work for 2-tuples"
instance ExtractL4C (a,b,c) where
  type ExtractL4T (a,b,c) = GL.TypeError ('GL.Text "L4 doesn't work for 3-tuples")
  extractL4C _ = errorInProgram "L4 doesn't work for 3-tuples"
instance ExtractL4C (a,b,c,d) where
  type ExtractL4T (a,b,c,d) = d
  extractL4C (_,_,_,d) = d
instance ExtractL4C (a,b,c,d,e) where
  type ExtractL4T (a,b,c,d,e) = d
  extractL4C (_,_,_,d,_) = d
instance ExtractL4C (a,b,c,d,e,f) where
  type ExtractL4T (a,b,c,d,e,f) = d
  extractL4C (_,_,_,d,_,_) = d

class ExtractL5C tp where
  type ExtractL5T tp
  extractL5C :: tp -> ExtractL5T tp
instance ExtractL5C (a,b) where
  type ExtractL5T (a,b) = GL.TypeError ('GL.Text "L5 doesn't work for 2-tuples")
  extractL5C _ = errorInProgram "L5 doesn't work for 2-tuples"
instance ExtractL5C (a,b,c) where
  type ExtractL5T (a,b,c) = GL.TypeError ('GL.Text "L5 doesn't work for 3-tuples")
  extractL5C _ = errorInProgram "L5 doesn't work for 3-tuples"
instance ExtractL5C (a,b,c,d) where
  type ExtractL5T (a,b,c,d) = GL.TypeError ('GL.Text "L5 doesn't work for 4-tuples")
  extractL5C _ = errorInProgram "L5 doesn't work for 4-tuples"
instance ExtractL5C (a,b,c,d,e) where
  type ExtractL5T (a,b,c,d,e) = e
  extractL5C (_,_,_,_,e) = e
instance ExtractL5C (a,b,c,d,e,f) where
  type ExtractL5T (a,b,c,d,e,f) = e
  extractL5C (_,_,_,_,e,_) = e

class ExtractL6C tp where
  type ExtractL6T tp
  extractL6C :: tp -> ExtractL6T tp
instance ExtractL6C (a,b) where
  type ExtractL6T (a,b) = GL.TypeError ('GL.Text "L6 doesn't work for 2-tuples")
  extractL6C _ = errorInProgram "L6 doesn't work for 2-tuples"
instance ExtractL6C (a,b,c) where
  type ExtractL6T (a,b,c) = GL.TypeError ('GL.Text "L6 doesn't work for 3-tuples")
  extractL6C _ = errorInProgram "L6 doesn't work for 3-tuples"
instance ExtractL6C (a,b,c,d) where
  type ExtractL6T (a,b,c,d) = GL.TypeError ('GL.Text "L6 doesn't work for 4-tuples")
  extractL6C _ = errorInProgram "L6 doesn't work for 4-tuples"
instance ExtractL6C (a,b,c,d,e) where
  type ExtractL6T (a,b,c,d,e) = GL.TypeError ('GL.Text "L6 doesn't work for 5-tuples")
  extractL6C _ = errorInProgram "L6 doesn't work for 5-tuples"
instance ExtractL6C (a,b,c,d,e,f) where
  type ExtractL6T (a,b,c,d,e,f) = f
  extractL6C (_,_,_,_,_,f) = f

class TupleC (n :: Nat) a where
  type TupleT n a
  getTupleC :: [a] -> Either [a] (TupleT n a)
instance TupleC 2 a where
  type TupleT 2 a = (a,a)
  getTupleC = \case
                a:b:_ -> Right (a,b)
                o -> Left o
instance TupleC 3 a where
  type TupleT 3 a = (a,a,a)
  getTupleC = \case
                a:b:c:_ -> Right (a,b,c)
                o -> Left o
instance TupleC 4 a where
  type TupleT 4 a = (a,a,a,a)
  getTupleC = \case
                a:b:c:d:_ -> Right (a,b,c,d)
                o -> Left o
instance TupleC 5 a where
  type TupleT 5 a = (a,a,a,a,a)
  getTupleC = \case
                a:b:c:d:e:_ -> Right (a,b,c,d,e)
                o -> Left o
instance TupleC 6 a where
  type TupleT 6 a = (a,a,a,a,a,a)
  getTupleC = \case
                a:b:c:d:e:f:_ -> Right (a,b,c,d,e,f)
                o -> Left o


-- | prime predicate
--
-- >>> isPrime 7
-- True
--
-- >>> isPrime 6
-- False
--
isPrime :: Int -> Bool
isPrime n = n == 2 || n > 2 && all ((> 0) . mod n) (2:[3,5 .. floor . sqrt @Double . fromIntegral $ n+1])

-- | prime factors
--
-- >>> primeFactors 100
-- [2,2,5,5]
--
-- >>> primeFactors 123
-- [3,41]
--
primeFactors :: Integer -> [Integer]
primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` Safe.headNote "primeFactors" factors)
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

-- | primes stream
--
-- >>> take 10 primes
--[2,3,5,7,11,13,17,19,23,29]
--
primes :: [Integer]
primes = 2 : 3 : 5 : primes'
  where
    isPrime' [] _ = errorInProgram "primes is empty"
    isPrime' (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime' ps n
    primes' = 7 : filter (isPrime' primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

-- | pretty print 'Ordering'
prettyOrd :: Ordering -> String
prettyOrd = \case
              LT -> "<"
              EQ -> "="
              GT -> ">"

-- | show the kind as a string
showTK :: forall r . Typeable r => String
showTK = show (typeRep (Proxy @r))

-- | get a Nat from the typelevel
--
-- >>> nat @14
-- 14
--
nat :: forall n a
  . ( KnownNat n
    , Num a
    ) => a
nat = fromIntegral (GL.natVal (Proxy @n))

-- | gets the Symbol from the typelevel
--
-- >>> symb @"abc"
-- "abc"
--
symb :: forall s . KnownSymbol s => String
symb = GL.symbolVal (Proxy @s)

-- | get a list of Nats from the typelevel
--
-- >>> getNats @'[10,12,1]
-- [10,12,1]
class GetNats as where
  getNats :: [Int]
instance GetNats '[] where
  getNats = []
instance ( KnownNat n
         , GetNats ns
         ) => GetNats (n ': ns) where
  getNats = nat @n : getNats @ns

-- | get a list of Symbols from the typelevel
--
-- >>> getSymbs @'["abc","def","g"]
-- ["abc","def","g"]
--
class GetSymbs ns where
  getSymbs :: [String]
instance GetSymbs '[] where
  getSymbs = []
instance ( KnownSymbol s
         , GetSymbs ss
         ) => GetSymbs (s ': ss) where
  getSymbs = symb @s : getSymbs @ss

-- | get 'Bool' from the typelevel
class GetBool (a :: Bool) where
  getBool :: Bool
instance GetBool 'True where
  getBool = True
instance GetBool 'False where
  getBool = False

-- | Regex options for Rescan Resplit Re etc
data ROpt =
    Anchored -- ^ Force pattern anchoring
  | AutoCallout -- ^ Compile automatic callouts
--  | BsrAnycrlf --  \R matches only CR, LF, or CrlF
--  | BsrUnicode -- ^ \R matches all Unicode line endings
  | Caseless -- ^ Do caseless matching
  | DollarEndonly -- ^ dollar not to match newline at end
  | Dotall -- ^ matches anything including NL
  | Dupnames -- ^ Allow duplicate names for subpatterns
  | Extended -- ^ Ignore whitespace and # comments
  | Extra -- ^ PCRE extra features (not much use currently)
  | Firstline -- ^ Force matching to be before newline
  | Multiline -- ^ caret and dollar match newlines within data
--  | NewlineAny -- ^ Recognize any Unicode newline sequence
--  | NewlineAnycrlf -- ^ Recognize CR, LF, and CrlF as newline sequences
  | NewlineCr -- ^ Set CR as the newline sequence
  | NewlineCrlf -- ^ Set CrlF as the newline sequence
  | NewlineLf -- ^ Set LF as the newline sequence
  | NoAutoCapture -- ^ Disable numbered capturing parentheses (named ones available)
  | Ungreedy -- ^ Invert greediness of quantifiers
  | Utf8 -- ^ Run in UTF--8 mode
  | NoUtf8Check -- ^ Do not check the pattern for UTF-8 validity
  deriving stock (Read, Show, Eq, Ord, Enum, Bounded)

-- | extract the regex options from the type level list
class GetROpts (os :: [ROpt]) where
  getROpts :: ([String], [RL.PCREOption])
instance GetROpts '[] where
  getROpts = ([], [])
instance ( Typeable r
         , GetROpt r
         , GetROpts rs
         ) => GetROpts (r ': rs) where
  getROpts = ((showTK @r :) *** (getROpt @r :)) (getROpts @rs)

displayROpts :: [String] -> String
displayROpts xs = "[" <> intercalate ", " (nubOrd xs) <> "]"

-- | convert type level regex option to the value level
class GetROpt (o :: ROpt) where
  getROpt :: RL.PCREOption
instance GetROpt 'Anchored where getROpt = RL.anchored
instance GetROpt 'AutoCallout where getROpt = RL.auto_callout
--instance GetROpt 'BsrAnycrlf where getROpt = RL.bsr_anycrlf
--instance GetROpt 'BsrUnicode where getROpt = RL.bsr_unicode
instance GetROpt 'Caseless where getROpt = RL.caseless
instance GetROpt 'DollarEndonly where getROpt = RL.dollar_endonly
instance GetROpt 'Dotall where getROpt = RL.dotall
instance GetROpt 'Dupnames where getROpt = RL.dupnames
instance GetROpt 'Extended where getROpt = RL.extended
instance GetROpt 'Extra where getROpt = RL.extra
instance GetROpt 'Firstline where getROpt = RL.firstline
instance GetROpt 'Multiline where getROpt = RL.multiline
--instance GetROpt 'NewlineAny where getROpt = RL.newline_any
--instance GetROpt 'NewlineAnycrlf where getROpt = RL.newline_anycrlf
instance GetROpt 'NewlineCr where getROpt = RL.newline_cr
instance GetROpt 'NewlineCrlf where getROpt = RL.newline_crlf
instance GetROpt 'NewlineLf where getROpt = RL.newline_lf
instance GetROpt 'NoAutoCapture where getROpt = RL.no_auto_capture
instance GetROpt 'Ungreedy where getROpt = RL.ungreedy
instance GetROpt 'Utf8 where getROpt = RL.utf8
instance GetROpt 'NoUtf8Check where getROpt = RL.no_utf8_check

-- | simple regex string replacement options
data ReplaceFnSub =
    RPrepend
  | ROverWrite
  | RAppend
  deriving stock (Read, Show, Eq, Bounded, Enum)

-- | extract replacement options from typelevel
class GetReplaceFnSub (k :: ReplaceFnSub) where
  getReplaceFnSub :: ReplaceFnSub
instance GetReplaceFnSub 'RPrepend where getReplaceFnSub = RPrepend
instance GetReplaceFnSub 'ROverWrite where getReplaceFnSub = ROverWrite
instance GetReplaceFnSub 'RAppend where getReplaceFnSub = RAppend

-- | used by 'Predicate.ReplaceImpl' and 'RH.sub' and 'RH.gsub' to allow more flexible replacement
--   These parallel the RegexReplacement (not exported) class in "Text.Regex.PCRE.Heavy" but have overlappable instances which is problematic for this code so I use 'RReplace'
data RReplace =
     RReplace !ReplaceFnSub !String
   | RReplace1 !(String -> [String] -> String)
   | RReplace2 !(String -> String)
   | RReplace3 !([String] -> String)

instance Show RReplace where
  show = \case
           RReplace o s -> "RReplace " ++ show o ++ " " ++ s
           RReplace1 {} -> "RReplace1 <fn>"
           RReplace2 {} -> "RReplace2 <fn>"
           RReplace3 {} -> "RReplace3 <fn>"

-- | wrapper for a Show instance around 'Color'
newtype SColor = SColor Color
  deriving newtype Enum

instance Show SColor where
  show (SColor c) =
    case c of
      Black -> "Black"
      Red -> "Red"
      Green -> "Green"
      Yellow -> "Yellow"
      Blue -> "Blue"
      Magenta -> "Magenta"
      Cyan -> "Cyan"
      White -> "White"
      Default -> "Default"

-- | get 'Color' from the typelevel
class GetColor (a :: Color) where
  getColor :: Color
instance GetColor 'Black where
  getColor = Black
instance GetColor 'Red where
  getColor = Red
instance GetColor 'Green where
  getColor = Green
instance GetColor 'Yellow where
  getColor = Yellow
instance GetColor 'Blue where
  getColor = Blue
instance GetColor 'Magenta where
  getColor = Magenta
instance GetColor 'Cyan where
  getColor = Cyan
instance GetColor 'White where
  getColor = White
instance GetColor 'Default where
  getColor = Default

-- | read a field and value using 'PCR.ReadPrec' parser
readField :: String -> PCR.ReadPrec a -> PCR.ReadPrec a
readField fieldName readVal = do
        GR.expectP (L.Ident fieldName)
        GR.expectP (L.Punc "=")
        readVal

-- | convenience method for optional display
unlessNull :: (Foldable t, Monoid m) => t a -> m -> m
unlessNull t m | null t = mempty
               | otherwise = m

unlessNullM :: (Foldable t, Applicative m) => t a -> (t a -> m ()) -> m ()
unlessNullM t f
  | null t = pure ()
  | otherwise = f t

nullSpace :: String -> String
nullSpace = nullIf " "

nullIf :: String -> String -> String
nullIf s t
  | all isSpace t = ""
  | otherwise = s <> t

pureTryTest :: a -> IO (Either () a)
pureTryTest = fmap (left (const ())) . E.try @E.SomeException . E.evaluate
--pureTryTest = over (mapped . _Left) (const ()) . E.try @E.SomeException . E.evaluate

pureTryTestPred :: (String -> Bool)
                -> a
                -> IO (Either String (Either () a))
pureTryTestPred p a = do
  lr <- left E.displayException <$> E.try @E.SomeException (E.evaluate a)
  return $ case lr of
    Left e | p e -> Right (Left ())
           | otherwise -> Left ("no match found: e=" ++ e)
    Right r -> Right (Right r)

