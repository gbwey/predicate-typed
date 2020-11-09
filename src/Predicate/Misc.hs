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
-- | Utility methods for Predicate / methods for displaying the evaluation tree
module Predicate.Misc (
  -- ** useful type families
    AndT
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
  , JoinT
  , FailWhenT
  , FailUnlessT
  , BetweenT

 -- ** extract values from the type level
  , GetBool(..)
  , GetLen(..)
  , GetThese(..)
  , GetOrdering(..)
  , OrderingP(..)
  , GetOrd(..)
  , nat
  , symb

 -- ** inductive tuples
  , ToITupleC(..)
  , FromITupleC(..)
  , ToITupleListC(..)
  , ReverseITupleC(..)
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
  , ExtractL7C(..)
  , ExtractL8C(..)

 -- ** primes
  , isPrime
  , primeStream
  , primeFactors

  -- ** regular expressions
  , compileRegex
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
  , SwapC(..)
  , showTK
  , showT
  , showThese
  , prettyOrd
  , unlessNull
  , unlessNullM
  , nullSpace
  , nullIf
  , pureTryTest
  , pureTryTestPred
  , (~>)
  , errorInProgram
  , drawTreeU
  , removeAnsi
  , _Id
  , sum'
  , product'
  , foldMapStrict
  , cycle'
  , cmpOf
  , ifM
  , AssocC(..)
  , simpleAlign
  , getValidBase
  ) where
import qualified GHC.TypeNats as GN
import GHC.TypeLits (Symbol,Nat,KnownSymbol,KnownNat,ErrorMessage((:$$:),(:<>:)))
import qualified GHC.TypeLits as GL
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable, typeRep)
import System.Console.Pretty (Color(..))
import GHC.Exts (Constraint)
import qualified Text.Regex.PCRE.Heavy as RH
import qualified Text.Regex.PCRE.Light as RL
import qualified Data.Text.Encoding as TE
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
import Data.List (foldl', intercalate, unfoldr, isPrefixOf, isInfixOf, isSuffixOf)
import qualified Safe (headNote)
import Data.Char (isSpace)
import qualified Control.Exception as E
import Data.Tree (Tree(Node))
import Control.Lens
import qualified Data.Semigroup as SG
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators

-- | type level Between
type family BetweenT (s :: Symbol) (a :: Nat) (b :: Nat) (v :: Nat) :: Constraint where
  BetweenT s m n v =
     FailUnlessT (AndT (m GL.<=? v) (v GL.<=? n))
            ('GL.Text s
             ':<>: 'GL.Text " failed"
             ':$$: 'GL.ShowType v
             ':<>: 'GL.Text " is outside the range ["
             ':<>: 'GL.ShowType m
             ':<>: 'GL.Text ".."
             ':<>: 'GL.ShowType n
             ':<>: 'GL.Text "]")

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
class GetLen xs where
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
class GetThese (th :: These a b) where
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

-- | takes a flat n-tuple and creates an inductive tuple. see 'Predicate.Data.ReadShow.PrintT'
--
-- >>> toITupleC (123,'x',False,"abc")
-- (123,('x',(False,("abc",()))))
--
-- >>> toITupleC (123,'x')
-- (123,('x',()))
--
class ToITupleC x where
  type ToITupleP x
  toITupleC :: x -> ToITupleP x
instance (GL.TypeError ('GL.Text "ToITupleC: invalid empty tuple")) => ToITupleC () where
  type ToITupleP () = ()
  toITupleC () = ()
instance ToITupleC (a,b) where
  type ToITupleP (a,b) = (a,(b,()))
  toITupleC (a,b) = (a,(b,()))
instance ToITupleC (a,b,c) where
  type ToITupleP (a,b,c) = (a,(b,(c,())))
  toITupleC (a,b,c) = (a,(b,(c,())))
instance ToITupleC (a,b,c,d) where
  type ToITupleP (a,b,c,d) = (a,(b,(c,(d,()))))
  toITupleC (a,b,c,d) = (a,(b,(c,(d,()))))
instance ToITupleC (a,b,c,d,e) where
  type ToITupleP (a,b,c,d,e) = (a,(b,(c,(d,(e,())))))
  toITupleC (a,b,c,d,e) = (a,(b,(c,(d,(e,())))))
instance ToITupleC (a,b,c,d,e,f) where
  type ToITupleP (a,b,c,d,e,f) = (a,(b,(c,(d,(e,(f,()))))))
  toITupleC (a,b,c,d,e,f) = (a,(b,(c,(d,(e,(f,()))))))
instance ToITupleC (a,b,c,d,e,f,g) where
  type ToITupleP (a,b,c,d,e,f,g) = (a,(b,(c,(d,(e,(f,(g,())))))))
  toITupleC (a,b,c,d,e,f,g) = (a,(b,(c,(d,(e,(f,(g,())))))))
instance ToITupleC (a,b,c,d,e,f,g,h) where
  type ToITupleP (a,b,c,d,e,f,g,h) = (a,(b,(c,(d,(e,(f,(g,(h,()))))))))
  toITupleC (a,b,c,d,e,f,g,h) = (a,(b,(c,(d,(e,(f,(g,(h,()))))))))
instance ToITupleC (a,b,c,d,e,f,g,h,i) where
  type ToITupleP (a,b,c,d,e,f,g,h,i) = (a,(b,(c,(d,(e,(f,(g,(h,(i,())))))))))
  toITupleC (a,b,c,d,e,f,g,h,i) = (a,(b,(c,(d,(e,(f,(g,(h,(i,())))))))))
instance ToITupleC (a,b,c,d,e,f,g,h,i,j) where
  type ToITupleP (a,b,c,d,e,f,g,h,i,j) = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,()))))))))))
  toITupleC (a,b,c,d,e,f,g,h,i,j) = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,()))))))))))
instance ToITupleC (a,b,c,d,e,f,g,h,i,j,k) where
  type ToITupleP (a,b,c,d,e,f,g,h,i,j,k) = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,())))))))))))
  toITupleC (a,b,c,d,e,f,g,h,i,j,k) = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,())))))))))))
instance ToITupleC (a,b,c,d,e,f,g,h,i,j,k,l) where
  type ToITupleP (a,b,c,d,e,f,g,h,i,j,k,l) = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,()))))))))))))
  toITupleC (a,b,c,d,e,f,g,h,i,j,k,l) = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,()))))))))))))

-- | takes an inductive tuple and creates a flat n-tuple
--
-- >>> fromITupleC (123,('x',(False,("abc",()))))
-- (123,'x',False,"abc")
--
-- >>> fromITupleC (123,('x',()))
-- (123,'x')
--
class FromITupleC x where
  type FromITupleP x
  fromITupleC :: x -> FromITupleP x
instance FromITupleC () where
  type FromITupleP () = ()
  fromITupleC () = ()
instance FromITupleC (a,()) where
  type FromITupleP (a,()) = a
  fromITupleC (a,()) = a
instance FromITupleC (a,(b,())) where
  type FromITupleP (a,(b,())) = (a,b)
  fromITupleC (a,(b,())) = (a,b)
instance FromITupleC (a,(b,(c,()))) where
  type FromITupleP (a,(b,(c,()))) = (a,b,c)
  fromITupleC (a,(b,(c,()))) = (a,b,c)
instance FromITupleC (a,(b,(c,(d,())))) where
  type FromITupleP (a,(b,(c,(d,())))) = (a,b,c,d)
  fromITupleC (a,(b,(c,(d,())))) = (a,b,c,d)
instance FromITupleC (a,(b,(c,(d,(e,()))))) where
  type FromITupleP (a,(b,(c,(d,(e,()))))) = (a,b,c,d,e)
  fromITupleC (a,(b,(c,(d,(e,()))))) = (a,b,c,d,e)
instance FromITupleC (a,(b,(c,(d,(e,(f,())))))) where
  type FromITupleP (a,(b,(c,(d,(e,(f,())))))) = (a,b,c,d,e,f)
  fromITupleC (a,(b,(c,(d,(e,(f,())))))) = (a,b,c,d,e,f)
instance FromITupleC (a,(b,(c,(d,(e,(f,(g,()))))))) where
  type FromITupleP (a,(b,(c,(d,(e,(f,(g,()))))))) = (a,b,c,d,e,f,g)
  fromITupleC (a,(b,(c,(d,(e,(f,(g,()))))))) = (a,b,c,d,e,f,g)
instance FromITupleC (a,(b,(c,(d,(e,(f,(g,(h,())))))))) where
  type FromITupleP (a,(b,(c,(d,(e,(f,(g,(h,())))))))) = (a,b,c,d,e,f,g,h)
  fromITupleC (a,(b,(c,(d,(e,(f,(g,(h,())))))))) = (a,b,c,d,e,f,g,h)
instance FromITupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,()))))))))) where
  type FromITupleP (a,(b,(c,(d,(e,(f,(g,(h,(i,()))))))))) = (a,b,c,d,e,f,g,h,i)
  fromITupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,()))))))))) = (a,b,c,d,e,f,g,h,i)
instance FromITupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,())))))))))) where
  type FromITupleP (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,())))))))))) = (a,b,c,d,e,f,g,h,i,j)
  fromITupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,())))))))))) = (a,b,c,d,e,f,g,h,i,j)
instance FromITupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,()))))))))))) where
  type FromITupleP (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,()))))))))))) = (a,b,c,d,e,f,g,h,i,j,k)
  fromITupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,()))))))))))) = (a,b,c,d,e,f,g,h,i,j,k)
instance FromITupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,())))))))))))) where
  type FromITupleP (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,())))))))))))) = (a,b,c,d,e,f,g,h,i,j,k,l)
  fromITupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,())))))))))))) = (a,b,c,d,e,f,g,h,i,j,k,l)

-- | takes a list of size @n@ and converts it to an inductive tuple. see 'Predicate.Data.ReadShow.PrintL'
--
-- >>> toITupleListC @4 [10,12,13,1]
-- Right (10,(12,(13,(1,()))))
--
-- >>> toITupleListC @2 ["ab","cc"]
-- Right ("ab",("cc",()))
--
-- >>> toITupleListC @10 [10,12,13,1]
-- Left "toITupleListC: expected exactly 10 values"
--
-- >>> toITupleListC @2 [10,12,13,1]
-- Left "toITupleListC: expected exactly 2 values"
--
class ToITupleListC (n :: Nat) (a :: Type) where
  type ToITupleListP n a
  toITupleListC :: [a] -> Either String (ToITupleListP n a)
instance (GL.TypeError ('GL.Text "ToITupleListC: inductive tuple cannot be empty")) => ToITupleListC 0 a where
  type ToITupleListP 0 a = ()
  toITupleListC _ = Left "ToITupleListC 0: shouldnt be called"
instance ToITupleListC 1 a where
  type ToITupleListP 1 a = (a,())
  toITupleListC [a] = Right (a,())
  toITupleListC _ = Left "toITupleListC: expected exactly 1 value"
instance ToITupleListC 2 a where
  type ToITupleListP 2 a = (a,(a,()))
  toITupleListC [a,b] = Right (a,(b,()))
  toITupleListC _ = Left "toITupleListC: expected exactly 2 values"
instance ToITupleListC 3 a where
  type ToITupleListP 3 a = (a,(a,(a,())))
  toITupleListC [a,b,c] = Right (a,(b,(c,())))
  toITupleListC _ = Left "toITupleListC: expected exactly 3 values"
instance ToITupleListC 4 a where
  type ToITupleListP 4 a = (a,(a,(a,(a,()))))
  toITupleListC [a,b,c,d] = Right (a,(b,(c,(d,()))))
  toITupleListC _ = Left "toITupleListC: expected exactly 4 values"
instance ToITupleListC 5 a where
  type ToITupleListP 5 a = (a,(a,(a,(a,(a,())))))
  toITupleListC [a,b,c,d,e] = Right (a,(b,(c,(d,(e,())))))
  toITupleListC _ = Left "toITupleListC: expected exactly 5 values"
instance ToITupleListC 6 a where
  type ToITupleListP 6 a = (a,(a,(a,(a,(a,(a,()))))))
  toITupleListC [a,b,c,d,e,f] = Right (a,(b,(c,(d,(e,(f,()))))))
  toITupleListC _ = Left "toITupleListC: expected exactly 6 values"
instance ToITupleListC 7 a where
  type ToITupleListP 7 a = (a,(a,(a,(a,(a,(a,(a,())))))))
  toITupleListC [a,b,c,d,e,f,g] = Right (a,(b,(c,(d,(e,(f,(g,())))))))
  toITupleListC _ = Left "toITupleListC: expected exactly 7 values"
instance ToITupleListC 8 a where
  type ToITupleListP 8 a = (a,(a,(a,(a,(a,(a,(a,(a,()))))))))
  toITupleListC [a,b,c,d,e,f,g,h] = Right (a,(b,(c,(d,(e,(f,(g,(h,()))))))))
  toITupleListC _ = Left "toITupleListC: expected exactly 8 values"
instance ToITupleListC 9 a where
  type ToITupleListP 9 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,())))))))))
  toITupleListC [a,b,c,d,e,f,g,h,i] = Right (a,(b,(c,(d,(e,(f,(g,(h,(i,())))))))))
  toITupleListC _ = Left "toITupleListC: expected exactly 9 values"
instance ToITupleListC 10 a where
  type ToITupleListP 10 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,(a,()))))))))))
  toITupleListC [a,b,c,d,e,f,g,h,i,j] = Right (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,()))))))))))
  toITupleListC _ = Left "toITupleListC: expected exactly 10 values"
instance ToITupleListC 11 a where
  type ToITupleListP 11 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,(a,(a,())))))))))))
  toITupleListC [a,b,c,d,e,f,g,h,i,j,k] = Right (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,())))))))))))
  toITupleListC _ = Left "toITupleListC: expected exactly 11 values"
instance ToITupleListC 12 a where
  type ToITupleListP 12 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,(a,(a,(a,()))))))))))))
  toITupleListC [a,b,c,d,e,f,g,h,i,j,k,l] = Right (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,()))))))))))))
  toITupleListC _ = Left "toITupleListC: expected exactly 12 values"

class ReverseITupleC (x :: Type) (xs :: Type) (ys :: Type) where
  type ReverseITupleT x xs ys
  reverseITupleC :: x -> xs -> ys -> ReverseITupleT x xs ys
instance ReverseITupleC x () ys  where
  type ReverseITupleT x () ys = (x,ys)
  reverseITupleC x () ys = (x,ys)
instance ReverseITupleC w ws (x, ys) => ReverseITupleC x (w,ws) ys  where
  type ReverseITupleT x (w,ws) ys = (ReverseITupleT w ws (x,ys))
  reverseITupleC x (w,ws) ys = reverseITupleC w ws (x,ys)

-- | type level application: see 'Predicate.Core.$' which works for type level functions
type family (p :: k -> k1) %% (q :: k) :: k1 where
  p %% q = p q

infixl 9 %%

-- | reverse type level application: see 'Predicate.Core.&' which works for type level functions
type family (p :: k) %& (q :: k -> k1) :: k1 where
  p %& q = q p

infixr 9 %&

-- | 'flip' at the type level
type family FlipT (d :: k1 -> k -> k2) (p :: k) (q :: k1) :: k2 where
  FlipT d p q = d q p

-- | 'if' at the type level
type family IfT (b :: Bool) (t :: k) (f :: k) :: k where
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

-- | type family to extract @a@ from a list of @a@
type family ExtractAFromList (as :: Type) :: Type where
  ExtractAFromList [a] = a
  ExtractAFromList z = GL.TypeError (
      'GL.Text "ExtractAFromList: expected [a] but found something else"
      ':$$: 'GL.Text "as = "
      ':<>: 'GL.ShowType z)

-- | extract @a@ from a Maybe container
type family MaybeT mb where
  MaybeT (Maybe a) = a
  MaybeT o = GL.TypeError (
      'GL.Text "MaybeT: expected 'Maybe a' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | extract @a@ from a Either container
type family LeftT lr where
  LeftT (Either a _) = a
  LeftT o = GL.TypeError (
      'GL.Text "LeftT: expected 'Either a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | extract @b@ from a Either container
type family RightT lr where
  RightT (Either _a b) = b
  RightT o = GL.TypeError (
      'GL.Text "RightT: expected 'Either a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | extract @a@ from a These container
type family ThisT lr where
  ThisT (These a _b) = a
  ThisT o = GL.TypeError (
      'GL.Text "ThisT: expected 'These a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | extract @b@ from a These container
type family ThatT lr where
  ThatT (These _a b) = b
  ThatT o = GL.TypeError (
      'GL.Text "ThatT: expected 'These a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | extract @a@ and @b@ from a These container
type family TheseT lr where
  TheseT (These a b) = (a,b)
  TheseT o = GL.TypeError (
      'GL.Text "TheseT: expected 'These a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | extract @b@ from an arrow type
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
  ApplyConstT (t _) b = t b
  ApplyConstT ta b = GL.TypeError (
       'GL.Text "ApplyConstT: (t a) b but found something else"
       ':$$: 'GL.Text "t a = "
       ':<>: 'GL.ShowType ta
       ':$$: 'GL.Text "b = "
       ':<>: 'GL.ShowType b)

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

-- | extract element 1 from a n-tuple
class ExtractL1C (tp :: Type) where
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
instance ExtractL1C (a,b,c,d,e,f,g) where
  type ExtractL1T (a,b,c,d,e,f,g) = a
  extractL1C (a,_,_,_,_,_,_) = a
instance ExtractL1C (a,b,c,d,e,f,g,h) where
  type ExtractL1T (a,b,c,d,e,f,g,h) = a
  extractL1C (a,_,_,_,_,_,_,_) = a

-- | extract element 2 from a n-tuple
class ExtractL2C (tp :: Type) where
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
instance ExtractL2C (a,b,c,d,e,f,g) where
  type ExtractL2T (a,b,c,d,e,f,g) = b
  extractL2C (_,b,_,_,_,_,_) = b
instance ExtractL2C (a,b,c,d,e,f,g,h) where
  type ExtractL2T (a,b,c,d,e,f,g,h) = b
  extractL2C (_,b,_,_,_,_,_,_) = b

-- | extract element 3 from a n-tuple
class ExtractL3C (tp :: Type) where
  type ExtractL3T tp
  extractL3C :: tp -> ExtractL3T tp
instance ExtractL3C (a,b) where
  type ExtractL3T (a,b) = GL.TypeError ('GL.Text "L3 doesn't work for 2-tuples")
  extractL3C _ = errorInProgram "L3 doesn't work for 2-tuples"
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
instance ExtractL3C (a,b,c,d,e,f,g) where
  type ExtractL3T (a,b,c,d,e,f,g) = c
  extractL3C (_,_,c,_,_,_,_) = c
instance ExtractL3C (a,b,c,d,e,f,g,h) where
  type ExtractL3T (a,b,c,d,e,f,g,h) = c
  extractL3C (_,_,c,_,_,_,_,_) = c

-- | extract element 4 from a n-tuple
class ExtractL4C (tp :: Type) where
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
instance ExtractL4C (a,b,c,d,e,f,g) where
  type ExtractL4T (a,b,c,d,e,f,g) = d
  extractL4C (_,_,_,d,_,_,_) = d
instance ExtractL4C (a,b,c,d,e,f,g,h) where
  type ExtractL4T (a,b,c,d,e,f,g,h) = d
  extractL4C (_,_,_,d,_,_,_,_) = d

-- | extract element 5 from a n-tuple
class ExtractL5C (tp :: Type) where
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
instance ExtractL5C (a,b,c,d,e,f,g) where
  type ExtractL5T (a,b,c,d,e,f,g) = e
  extractL5C (_,_,_,_,e,_,_) = e
instance ExtractL5C (a,b,c,d,e,f,g,h) where
  type ExtractL5T (a,b,c,d,e,f,g,h) = e
  extractL5C (_,_,_,_,e,_,_,_) = e

-- | extract element 6 from a n-tuple
class ExtractL6C (tp :: Type) where
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
instance ExtractL6C (a,b,c,d,e,f,g) where
  type ExtractL6T (a,b,c,d,e,f,g) = f
  extractL6C (_,_,_,_,_,f,_) = f
instance ExtractL6C (a,b,c,d,e,f,g,h) where
  type ExtractL6T (a,b,c,d,e,f,g,h) = f
  extractL6C (_,_,_,_,_,f,_,_) = f

-- | extract element 7 from a n-tuple
class ExtractL7C (tp :: Type) where
  type ExtractL7T tp
  extractL7C :: tp -> ExtractL7T tp
instance ExtractL7C (a,b) where
  type ExtractL7T (a,b) = GL.TypeError ('GL.Text "L7 doesn't work for 2-tuples")
  extractL7C _ = errorInProgram "L7 doesn't work for 2-tuples"
instance ExtractL7C (a,b,c) where
  type ExtractL7T (a,b,c) = GL.TypeError ('GL.Text "L7 doesn't work for 3-tuples")
  extractL7C _ = errorInProgram "L7 doesn't work for 3-tuples"
instance ExtractL7C (a,b,c,d) where
  type ExtractL7T (a,b,c,d) = GL.TypeError ('GL.Text "L7 doesn't work for 4-tuples")
  extractL7C _ = errorInProgram "L7 doesn't work for 4-tuples"
instance ExtractL7C (a,b,c,d,e) where
  type ExtractL7T (a,b,c,d,e) = GL.TypeError ('GL.Text "L7 doesn't work for 5-tuples")
  extractL7C _ = errorInProgram "L7 doesn't work for 5-tuples"
instance ExtractL7C (a,b,c,d,e,f) where
  type ExtractL7T (a,b,c,d,e,f) = GL.TypeError ('GL.Text "L7 doesn't work for 6-tuples")
  extractL7C _ = errorInProgram "L7 doesn't work for 6-tuples"
instance ExtractL7C (a,b,c,d,e,f,g) where
  type ExtractL7T (a,b,c,d,e,f,g) = g
  extractL7C (_,_,_,_,_,_,g) = g
instance ExtractL7C (a,b,c,d,e,f,g,h) where
  type ExtractL7T (a,b,c,d,e,f,g,h) = g
  extractL7C (_,_,_,_,_,_,g,_) = g

-- | extract element 8 from a n-tuple
class ExtractL8C (tp :: Type) where
  type ExtractL8T tp
  extractL8C :: tp -> ExtractL8T tp
instance ExtractL8C (a,b) where
  type ExtractL8T (a,b) = GL.TypeError ('GL.Text "L8 doesn't work for 2-tuples")
  extractL8C _ = errorInProgram "L8 doesn't work for 2-tuples"
instance ExtractL8C (a,b,c) where
  type ExtractL8T (a,b,c) = GL.TypeError ('GL.Text "L8 doesn't work for 3-tuples")
  extractL8C _ = errorInProgram "L8 doesn't work for 3-tuples"
instance ExtractL8C (a,b,c,d) where
  type ExtractL8T (a,b,c,d) = GL.TypeError ('GL.Text "L8 doesn't work for 4-tuples")
  extractL8C _ = errorInProgram "L8 doesn't work for 4-tuples"
instance ExtractL8C (a,b,c,d,e) where
  type ExtractL8T (a,b,c,d,e) = GL.TypeError ('GL.Text "L8 doesn't work for 5-tuples")
  extractL8C _ = errorInProgram "L8 doesn't work for 5-tuples"
instance ExtractL8C (a,b,c,d,e,f) where
  type ExtractL8T (a,b,c,d,e,f) = GL.TypeError ('GL.Text "L8 doesn't work for 6-tuples")
  extractL8C _ = errorInProgram "L8 doesn't work for 6-tuples"
instance ExtractL8C (a,b,c,d,e,f,g) where
  type ExtractL8T (a,b,c,d,e,f,g) = GL.TypeError ('GL.Text "L8 doesn't work for 7-tuples")
  extractL8C _ = errorInProgram "L8 doesn't work for 7-tuples"
instance ExtractL8C (a,b,c,d,e,f,g,h) where
  type ExtractL8T (a,b,c,d,e,f,g,h) = h
  extractL8C (_,_,_,_,_,_,_,h) = h

-- | try to convert a list to a n-tuple
class TupleC (n :: Nat) (a :: Type) where
  type TupleT n a
  getTupleC :: [a] -> Maybe (TupleT n a)

-- | convert a list of at least 2 elements to a 2-tuple
instance TupleC 2 a where
  type TupleT 2 a = (a,a)
  getTupleC = \case
                a:b:_ -> Just (a,b)
                _ -> Nothing

-- | convert a list of at least 3 elements to a 3-tuple
instance TupleC 3 a where
  type TupleT 3 a = (a,a,a)
  getTupleC = \case
                a:b:c:_ -> Just (a,b,c)
                _ -> Nothing

-- | convert a list of at least 4 elements to a 4-tuple
instance TupleC 4 a where
  type TupleT 4 a = (a,a,a,a)
  getTupleC = \case
                a:b:c:d:_ -> Just (a,b,c,d)
                _ -> Nothing

-- | convert a list of at least 5 elements to a 5-tuple
instance TupleC 5 a where
  type TupleT 5 a = (a,a,a,a,a)
  getTupleC = \case
                a:b:c:d:e:_ -> Just (a,b,c,d,e)
                _ -> Nothing

-- | convert a list of at least 6 elements to a 6-tuple
instance TupleC 6 a where
  type TupleT 6 a = (a,a,a,a,a,a)
  getTupleC = \case
                a:b:c:d:e:f:_ -> Just (a,b,c,d,e,f)
                _ -> Nothing

-- | convert a list of at least 7 elements to a 7-tuple
instance TupleC 7 a where
  type TupleT 7 a = (a,a,a,a,a,a,a)
  getTupleC = \case
                a:b:c:d:e:f:g:_ -> Just (a,b,c,d,e,f,g)
                _ -> Nothing

-- | convert a list of at least 8 elements to a 8-tuple
instance TupleC 8 a where
  type TupleT 8 a = (a,a,a,a,a,a,a,a)
  getTupleC = \case
                a:b:c:d:e:f:g:h:_ -> Just (a,b,c,d,e,f,g,h)
                _ -> Nothing

-- | convert a list of at least 9 elements to a 9-tuple
instance TupleC 9 a where
  type TupleT 9 a = (a,a,a,a,a,a,a,a,a)
  getTupleC = \case
                a:b:c:d:e:f:g:h:i:_ -> Just (a,b,c,d,e,f,g,h,i)
                _ -> Nothing

-- | convert a list of at least 10 elements to a 10-tuple
instance TupleC 10 a where
  type TupleT 10 a = (a,a,a,a,a,a,a,a,a,a)
  getTupleC = \case
                a:b:c:d:e:f:g:h:i:j:_ -> Just (a,b,c,d,e,f,g,h,i,j)
                _ -> Nothing

-- | convert a list of at least 11 elements to a 11-tuple
instance TupleC 11 a where
  type TupleT 11 a = (a,a,a,a,a,a,a,a,a,a,a)
  getTupleC = \case
                a:b:c:d:e:f:g:h:i:j:k:_ -> Just (a,b,c,d,e,f,g,h,i,j,k)
                _ -> Nothing

-- | convert a list of at least 12 elements to a 12-tuple
instance TupleC 12 a where
  type TupleT 12 a = (a,a,a,a,a,a,a,a,a,a,a,a)
  getTupleC = \case
                a:b:c:d:e:f:g:h:i:j:k:l:_ -> Just (a,b,c,d,e,f,g,h,i,j,k,l)
                _ -> Nothing

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
-- >>> take 10 primeStream
-- [2,3,5,7,11,13,17,19,23,29]
--
primeStream :: [Integer]
primeStream = 2 : 3 : 5 : primes'
  where
    isPrime' [] _ = errorInProgram "primes is empty"
    isPrime' (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime' ps n
    primes' = 7 : filter (isPrime' primes') (scanl (+) 11 $ cycle' [2,4,2,4,6,2,6,4])

-- | similar to 'cycle' but if the list is empty will return an empty list
cycle' :: [a] -> [a]
cycle' [] = []
cycle' xs = xs' where xs' = xs ++ xs'

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
class GetNats (as :: [Nat]) where
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
class GetSymbs (ns :: [Symbol]) where
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

-- | compile a regex using type level options
compileRegex :: forall rs . GetROpts rs
  => String
  -> String
  -> Either (String, String) RH.Regex
compileRegex nm s
  | null s = Left ("Regex cannot be empty",nm)
  | otherwise =
      let rs = getROpts @rs
          mm = nm <> " " <> show rs
          f e = ("Regex failed to compile", mm <> ":" <> e)
      in left f (RH.compileM (TE.encodeUtf8 (T.pack s)) (snd rs))

-- | Regex options for Rescan Resplit Re etc
data ROpt =
    Anchored -- ^ Force pattern anchoring
  | AutoCallout -- ^ Compile automatic callouts
{-
  | BsrAnycrlf -- ^ \R matches only CR, LF, or CrlF
  | BsrUnicode -- ^ \R matches all Unicode line endings
-}
  | Caseless -- ^ Do caseless matching
  | DollarEndonly -- ^ dollar not to match newline at end
  | Dotall -- ^ matches anything including NL
  | Dupnames -- ^ Allow duplicate names for subpatterns
  | Extended -- ^ Ignore whitespace and # comments
  | Extra -- ^ PCRE extra features (not much use currently)
  | Firstline -- ^ Force matching to be before newline
  | Multiline -- ^ caret and dollar match newlines within data
{-
  | NewlineAny -- ^ Recognize any Unicode newline sequence
  | NewlineAnycrlf -- ^ Recognize CR, LF, and CrlF as newline sequences
-}
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

-- | display regex options
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
instance Bounded SColor where
  minBound = SColor Black
  maxBound = SColor Default

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

pureTryTestPred :: (String -> Bool)
                -> a
                -> IO (Either String (Either () a))
pureTryTestPred p a = do
  lr <- left E.displayException <$> E.try @E.SomeException (E.evaluate a)
  return $ case lr of
    Left e | p e -> Right (Left ())
           | otherwise -> Left ("no match found: e=" ++ e)
    Right r -> Right (Right r)

-- https://github.com/haskell/containers/pull/344
-- | draw a tree using unicode
drawTreeU :: Tree String -> String
drawTreeU  = intercalate "\n" . drawU

drawU :: Tree String -> [String]
drawU (Node x ts0) = x : drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        shift "\x2514\x2500" "  " (drawU t)
    drawSubTrees (t:ts) =
        shift "\x251c\x2500" "\x2502 " (drawU t) ++ drawSubTrees ts

    shift one other = zipWith (++) (one : repeat other)

-- | strip ansi characters from a string and print it (for doctests)
removeAnsi :: Show a => Either String a -> IO ()
removeAnsi = putStrLn . removeAnsiImpl

removeAnsiImpl :: Show a => Either String a -> String
removeAnsiImpl =
  \case
     Left e -> let esc = '\x1b'
                   f :: String -> Maybe (String, String)
                   f = \case
                          [] -> Nothing
                          c:cs | c == esc -> case break (=='m') cs of
                                                  (_,'m':s) -> Just ("",s)
                                                  _ -> Nothing
                               | otherwise -> Just $ break (==esc) (c:cs)
               in concat $ unfoldr f e
     Right a -> show a

-- | 'Identity' lens
_Id :: Lens (Identity a) (Identity b) a b
_Id afb (Identity a) = Identity <$> afb a

class Bifunctor p => SwapC p where
  swapC :: p a b -> p b a
instance SwapC Either where
  swapC (Left a) = Right a
  swapC (Right a) = Left a
instance SwapC These where
  swapC (This a) = That a
  swapC (That b) = This b
  swapC (These a b) = These b a
instance SwapC SG.Arg where
  swapC (SG.Arg a b) = SG.Arg b a
instance SwapC (,) where
  swapC (a,b) = (b,a)
instance SwapC ((,,) a) where
  swapC (a,b,c) = (a,c,b)
instance SwapC ((,,,) a b) where
  swapC (a,b,c,d) = (a,b,d,c)
instance SwapC ((,,,,) a b c) where
  swapC (a,b,c,d,e) = (a,b,c,e,d)
instance SwapC ((,,,,,) a b c d) where
  swapC (a,b,c,d,e,f) = (a,b,c,d,f,e)
instance SwapC ((,,,,,,) a b c d e) where
  swapC (a,b,c,d,e,f,g) = (a,b,c,d,e,g,f)

-- | strict version of 'sum'
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldl' (+) 0

-- | strict version of 'product'
product' :: (Foldable t, Num a) => t a -> a
product' = foldl' (*) 1

-- | strict version of 'Data.Foldable.foldMap': replace with Data.Foldable.foldMap' when more generally available
foldMapStrict :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMapStrict f = foldl' (\z a -> z <> f a) mempty

cmpOf :: Eq a => Ordering -> ([a] -> [a] -> Bool, String)
cmpOf = \case
           LT -> (isPrefixOf, "IsPrefix")
           EQ -> (isInfixOf, "IsInfix")
           GT -> (isSuffixOf, "IsSuffix")

-- | lifted if statement
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb mt mf = do
  b <- mb
  if b then mt else mf


class AssocC p where
  assoc :: p (p a b) c -> p a (p b c)
  unassoc :: p a (p b c) -> p (p a b) c
instance AssocC Either where
  assoc (Left (Left a)) = Left a
  assoc (Left (Right b)) = Right (Left b)
  assoc (Right b) = Right (Right b)

  unassoc (Left a) = Left (Left a)
  unassoc (Right (Left b)) = Left (Right b)
  unassoc (Right (Right b)) = Right b
instance AssocC These where
  assoc (This (This a)) = This a
  assoc (This (That b)) = That (This b)
  assoc (This (These a b)) = These a (This b)
  assoc (That c) = That (That c)
  assoc (These (This a) c) = These a (That c)
  assoc (These (That b) c) = That (These b c)
  assoc (These (These a b) c) = These a (These b c)

  unassoc (This a) = This (This a)
  unassoc (That (This b)) = This (That b)
  unassoc (That (That c)) = That c
  unassoc (That (These b c)) = These (That b) c
  unassoc (These a (This b)) = This (These a b)
  unassoc (These a (That c)) = These (This a) c
  unassoc (These a (These b c)) = These (These a b) c

instance AssocC (,) where
  assoc ((a,b),c) = (a,(b,c))
  unassoc (a,(b,c)) = ((a,b),c)

-- | zip two lists using These
--
-- >>> simpleAlign "ab" ""
-- [This 'a',This 'b']
--
-- >>> simpleAlign "" "ab"
-- [That 'a',That 'b']
--
-- >>> simpleAlign [1] "ab"
-- [These 1 'a',That 'b']
--
-- >>> simpleAlign [] []
-- []
--
-- >>> simpleAlign [1,2] "ab"
-- [These 1 'a',These 2 'b']
--
simpleAlign :: [a] -> [b] -> [These a b]
simpleAlign as [] = map This as
simpleAlign [] bs = map That bs
simpleAlign (a:as) (b:bs) = These a b : simpleAlign as bs

-- | get base values for n between 2 and 36
--
-- >>> getValidBase 36
-- "0123456789abcdefghijklmnopqrstuvwxyz"
--
-- >>> getValidBase 2
-- "01"
--
-- >>> getValidBase 8
-- "01234567"
--
getValidBase :: Int -> String
getValidBase n
  | n < 2 = errorInProgram $ "getValidBase: oops invalid base: found n<2 ie " ++ show n
  | n > 36 = errorInProgram $ "getValidBase: oops invalid base: found n>36 ie " ++ show n
  | otherwise = take n (['0'..'9'] <> ['a'..'z'])

