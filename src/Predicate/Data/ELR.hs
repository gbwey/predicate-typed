{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
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
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
-- | ELR related methods
module Predicate.Data.ELR (
 -- definition
    ELR(..)

 -- ** destructors
  , ENone'
  , ELeft'
  , ERight'
  , EBoth'

  , ELRIn
  , ELRId
  , ELRDef
  , ELRDef''
  , PartitionELR

  , ENoneDef
  , ELeftDef
  , ERightDef
  , EBothDef

 -- ** constructors
  , MkENone
  , MkELeft
  , MkERight
  , MkEBoth

  , MkENone'
  , MkELeft'
  , MkERight'

 -- ** boolean predicates
  , IsENone
  , IsELeft
  , IsERight
  , IsEBoth

 -- ** miscellaneous
  , getBifoldInfo
  , showELR
  , GetELR(..)
  , partitionELR
  , fromELR
 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import qualified GHC.TypeLits as GL
import GHC.TypeLits (ErrorMessage((:$$:),(:<>:)))
import Data.Kind (Type)
import Control.Lens
import Data.Proxy (Proxy(..))
import Data.Bitraversable
import Data.Bifoldable
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Monad (ap)
import qualified Language.Haskell.TH.Lift as TH
-- $setup
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG

-- | returns the filled status of a Bifoldable container
getBifoldInfo :: Bifoldable bi => bi a b -> String
getBifoldInfo bi =
  case bifoldMap (const (ELeft ())) (const (ERight ())) bi of
    ENone -> " <skipped>"
    ELeft () -> "(L)"
    ERight () -> "(R)"
    EBoth () () -> "(B)"

-- | similar to 'Data.These' with an additional empty constructor to support a Monoid instance
data ELR a b =
     ENone -- ^ empty constructor
   | ELeft !a  -- ^ similar to 'Data.These.This'
   | ERight !b -- ^ similar to 'Data.These.That'
   | EBoth !a !b -- ^ similar to 'Data.These.These'
   deriving stock (Show,Eq,Ord,Foldable,Functor,Traversable,Generic,TH.Lift)
   deriving anyclass NFData

instance (Semigroup a, Semigroup b) => Semigroup (ELR a b) where
  ENone <> x' = x'
  x <> ENone = x
  ELeft a <> ELeft a' = ELeft (a <> a')
  ELeft a <> ERight b' = EBoth a b'
  ELeft a <> EBoth a' b' = EBoth (a <> a') b'
  ERight b <> ELeft a' = EBoth a' b
  ERight b <> ERight b' = ERight (b <> b')
  ERight b <> EBoth a' b' = EBoth a' (b <> b')
  EBoth a b <> ELeft a' = EBoth (a <> a') b
  EBoth a b <> ERight b' = EBoth a (b <> b')
  EBoth a b <> EBoth a' b' = EBoth (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (ELR a b) where
  mempty = ENone

instance Semigroup x => Applicative (ELR x) where
  pure = ERight
  (<*>) = ap

instance Semigroup x => Monad (ELR x) where
  return = pure
  ENone >>= _ = ENone
  ELeft x >>= _ = ELeft x
  ERight a >>= amb = amb a
  EBoth x a >>= amb =
    case amb a of
      ENone -> ELeft x
      EBoth y b -> EBoth (x <> y) b
      ELeft y -> ELeft (x <> y)
      ERight b -> EBoth x b

instance Bifunctor ELR where
  bimap f g =
    \case
      ENone -> ENone
      ELeft a -> ELeft (f a)
      ERight b -> ERight (g b)
      EBoth a b -> EBoth (f a) (g b)

instance Bifoldable ELR where
  bifoldMap f g =
    \case
      ENone -> mempty
      ELeft a -> f a
      ERight b -> g b
      EBoth a b -> f a <> g b

instance Bitraversable ELR where
  bitraverse f g =
    \case
      ENone -> pure ENone
      ELeft a -> ELeft <$> f a
      ERight b -> ERight <$> g b
      EBoth a b -> EBoth <$> f a <*> g b

instance SwapC ELR where
  swapC =
    \case
      ENone -> ENone
      ELeft a -> ERight a
      ERight b -> ELeft b
      EBoth a b -> EBoth b a

-- | 'ENone' constructor
--
-- >>> pz @(Proxy Int >> MkENone' UnproxyT 10) []
-- Val ENone
--
data MkENone' t t1 deriving Show

instance P (MkENone' t t1) x where
  type PP (MkENone' t t1) x = ELR (PP t x) (PP t1 x)
  eval _ opts _ =
    let msg0 = "MkENone"
        d = ENone
    in pure $ mkNode opts (Val d) msg0 []

-- | 'ENone' constructor
--
-- >>> pl @(MkENone () Id) 'x'
-- Present ENone (MkENone)
-- Val ENone
--
data MkENone (t :: Type) (t1 :: Type) deriving Show
type MkENoneT (t :: Type) (t1 :: Type) = MkENone' (Hole t) (Hole t1)

instance P (MkENone t t1) x where
  type PP (MkENone t t1) x = PP (MkENoneT t t1) x
  eval _ = eval (Proxy @(MkENoneT t t1))

-- | 'ELeft' constructor
--
-- >>> pz @(Proxy Int >> MkELeft' UnproxyT 10) []
-- Val (ELeft 10)
--
data MkELeft' t p deriving Show

instance P p x
      => P (MkELeft' t p) x where
  type PP (MkELeft' t p) x = ELR (PP p x) (PP t x)
  eval _ opts x = do
    let msg0 = "MkELeft"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = ELeft p
        in mkNode opts (Val d) msg0 [hh pp]

-- | 'ELeft' constructor
--
-- >>> pl @(MkELeft () Id) 'x'
-- Present ELeft 'x' (MkELeft)
-- Val (ELeft 'x')
--
-- >>> pl @(MkELeft () Fst) ('x',True)
-- Present ELeft 'x' (MkELeft)
-- Val (ELeft 'x')
--
-- >>> pz @(MkELeft _ Id) 44
-- Val (ELeft 44)
--
data MkELeft (t :: Type) p deriving Show
type MkELeftT (t :: Type) p = MkELeft' (Hole t) p

instance P (MkELeftT t p) x => P (MkELeft t p) x where
  type PP (MkELeft t p) x = PP (MkELeftT t p) x
  eval _ = eval (Proxy @(MkELeftT t p))

-- | similar to 'MkERight' where @t@ references the type
data MkERight' t p deriving Show

instance P p x
      => P (MkERight' t p) x where
  type PP (MkERight' t p) x = ELR (PP t x) (PP p x)
  eval _ opts x = do
    let msg0 = "MkERight"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = ERight p
        in mkNode opts (Val d) msg0 [hh pp]

-- | 'ERight' constructor
--
-- >>> pz @(MkERight _ Id) 44
-- Val (ERight 44)
--
-- >>> pz @(MkERight _ "Abc" <> MkELeft _ '[1,2] <> MkEBoth [3,4] "def") ()
-- Val (EBoth [1,2,3,4] "Abcdef")
--
-- >>> pl @(MkERight () Id) 'x'
-- Present ERight 'x' (MkERight)
-- Val (ERight 'x')
--
data MkERight (t :: Type) p deriving Show
type MkERightT (t :: Type) p = MkERight' (Hole t) p

instance P (MkERightT t p) x => P (MkERight t p) x where
  type PP (MkERight t p) x = PP (MkERightT t p) x
  eval _ = eval (Proxy @(MkERightT t p))

-- | 'EBoth' constructor
--
-- >>> pz @(MkEBoth Fst Snd) (44,'x')
-- Val (EBoth 44 'x')
--
-- >>> pl @(MkEBoth Id 'True) 'x'
-- Present EBoth 'x' True (MkEBoth)
-- Val (EBoth 'x' True)
--
data MkEBoth p q deriving Show
instance ( P p a
         , P q a
         ) => P (MkEBoth p q) a where
  type PP (MkEBoth p q) a = ELR (PP p a) (PP q a)
  eval _ opts a = do
    let msg0 = "MkEBoth"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = EBoth p q
        in mkNode opts (Val d) msg0 [hh pp, hh qq]

data IsELR (th :: ELR x y) deriving Show
-- x y can be anything

-- trying to avoid Show instance because more likely to have ambiguity errors
instance ( x ~ ELR a b
         , Show a
         , Show b
         , GetELR th
         ) => P (IsELR (th :: ELR x1 x2)) x where
  type PP (IsELR th) x = Bool
  eval _ opts x =
    let msg0 = "Is"
        (t,f) = getELR @_ @_ @th
        b = f x
    in pure $ mkNodeB opts b (msg0 <> t <> showVerbose opts " | " x) []

-- | predicate on 'ENone'
--
-- >>> pz @IsENone ENone
-- Val True
--
-- >>> pz @IsENone (EBoth 1 'a')
-- Val False
--
data IsENone deriving Show
type IsENoneT = IsELR 'ENone

instance P IsENoneT x => P IsENone x where
  type PP IsENone x = PP IsENoneT x
  eval _ = evalBool (Proxy @IsENoneT)

-- | predicate on 'ELeft'
--
-- >>> pz @IsELeft (ELeft "aBc")
-- Val True
--
-- >>> pz @IsELeft (EBoth 1 'a')
-- Val False
--
-- >>> pl @IsELeft (ELeft 12)
-- True (IsELeft | ELeft 12)
-- Val True
--
data IsELeft deriving Show
type IsELeftT = IsELR ('ELeft '())

instance P IsELeftT x => P IsELeft x where
  type PP IsELeft x = PP IsELeftT x
  eval _ = evalBool (Proxy @IsELeftT)

-- | predicate on 'ERight'
--
-- >>> pl @IsERight (ELeft 12)
-- False (IsERight | ELeft 12)
-- Val False
--
data IsERight deriving Show
type IsERightT = IsELR ('ERight '())

instance P IsERightT x => P IsERight x where
  type PP IsERight x = PP IsERightT x
  eval _ = evalBool (Proxy @IsERightT)

-- | predicate on 'EBoth'
--
-- >>> pl @IsEBoth (ELeft 12)
-- False (IsEBoth | ELeft 12)
-- Val False
--
-- >>> pz @IsEBoth (EBoth 1 'a')
-- Val True
--
-- >>> pl @IsEBoth (EBoth 'x' 12)
-- True (IsEBoth | EBoth 'x' 12)
-- Val True
--
-- >>> pl @IsEBoth (ERight (SG.Sum 12))
-- False (IsEBoth | ERight (Sum {getSum = 12}))
-- Val False
--
-- >>> pl @IsEBoth (EBoth 1 (SG.Sum 12))
-- True (IsEBoth | EBoth 1 (Sum {getSum = 12}))
-- Val True
--
data IsEBoth deriving Show
type IsEBothT = IsELR ('EBoth '() '())

instance P IsEBothT x => P IsEBoth x where
  type PP IsEBoth x = PP IsEBothT x
  eval _ = evalBool (Proxy @IsEBothT)

-- | extracts the () from type level @ENone@ if the value exists
--
-- >>> pl @'ENone ENone
-- Present () ('ENone)
-- Val ()
--
-- >>> pz @'ENone (ERight "aaa")
-- Fail "'ENone found ERight"
--
instance x ~ ELR a b => P 'ENone x where
  type PP 'ENone x = ()
  eval _ opts x =
    let msg0 = "'ENone"
    in pure $ case x of
      ELeft {} -> mkNode opts (Fail (msg0 <> " found ELeft")) "" []
      ENone -> mkNode opts (Val ()) msg0 []
      ERight {} -> mkNode opts (Fail (msg0 <> " found ERight")) "" []
      EBoth {} -> mkNode opts (Fail (msg0 <> " found EBoth")) "" []

-- | extracts the @a@ from type level @ELeft a@ if the value exists
--
-- >>> pl @('ELeft Id) (ELeft 12)
-- Present 12 ('ELeft)
-- Val 12
--
-- >>> pz @('ELeft Id) (ERight "aaa")
-- Fail "'ELeft found ERight"
--
-- >>> pz @('ELeft Id) (EBoth 999 "aaa")
-- Fail "'ELeft found EBoth"
--
-- >>> pl @('ELeft Id) (ERight 12)
-- Error 'ELeft found ERight
-- Fail "'ELeft found ERight"
--
instance ( PP p x ~ ELR a b
         , P p x
         )
    => P ('ELeft p) x where
  type PP ('ELeft p) x = ELeftT (PP p x)
  eval _ opts x = do
    let msg0 = "'ELeft"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          ENone -> mkNode opts (Fail (msg0 <> " found ENone")) "" [hh pp]
          ELeft a -> mkNode opts (Val a) msg0 [hh pp]
          ERight {} -> mkNode opts (Fail (msg0 <> " found ERight")) "" [hh pp]
          EBoth {} -> mkNode opts (Fail (msg0 <> " found EBoth")) "" [hh pp]

-- | extracts the @b@ from type level @ERight b@ if the value exists
--
-- >>> pz @('ERight Id) (ERight 123)
-- Val 123
--
-- >>> pz @('ERight Id) (ELeft "aaa")
-- Fail "'ERight found ELeft"
--
-- >>> pz @('ERight Id) (EBoth 44 "aaa")
-- Fail "'ERight found EBoth"
--
instance ( PP p x ~ ELR a b
         , P p x
         )
    => P ('ERight p) x where
  type PP ('ERight p) x = ERightT (PP p x)
  eval _ opts x = do
    let msg0 = "'ERight"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          ENone -> mkNode opts (Fail (msg0 <> " found ENone")) "" [hh pp]
          ELeft {} -> mkNode opts (Fail (msg0 <> " found ELeft")) "" [hh pp]
          ERight b -> mkNode opts (Val b) msg0 [hh pp]
          EBoth {} -> mkNode opts (Fail (msg0 <> " found EBoth")) "" [hh pp]

-- | extracts the (a,b) from type level @EBoth a b@ if the value exists
--
-- >>> pz @('EBoth Id Id) (EBoth 123 "abc")
-- Val (123,"abc")
--
-- >>> pz @('EBoth Id 5) (EBoth 123 "abcde")
-- Val (123,5)
--
-- >>> pz @('EBoth Id Id) (ELeft "aaa")
-- Fail "'EBoth found ELeft"
--
-- >>> pz @('EBoth Id Id) (ERight "aaa")
-- Fail "'EBoth found ERight"
--
instance ( Show a
         , Show b
         , P p a
         , P q b
         , Show (PP p a)
         , Show (PP q b)
         ) => P ('EBoth p q) (ELR a b) where
  type PP ('EBoth p q) (ELR a b) = (PP p a, PP q b)
  eval _ opts th = do
    let msg0 = "'EBoth"
    case th of
      EBoth a b -> do
        pp <- eval (Proxy @p) opts a
        case getValueLR NoInline opts msg0 pp [] of
           Left e -> pure e
           Right p -> do
             qq <- eval (Proxy @q) opts b
             pure $ case getValueLR NoInline opts (msg0 <> " q failed p=" <> showL opts p) qq [hh pp] of
                Left e -> e
                Right q ->
                  let ret =(p,q)
                  in  mkNode opts (Val ret) (show3 opts msg0 ret (EBoth a b)) [hh pp, hh qq]
      _ -> pure $ mkNode opts (Fail (msg0 <> " found " <> showELR th)) "" []

-- | display constructor name for 'ELR'
showELR :: ELR a b -> String
showELR = \case
  ENone -> "ENone"
  ELeft {} -> "ELeft"
  ERight {} -> "ERight"
  EBoth {} -> "EBoth"

-- | get 'ELR' from typelevel [type application order is a b then th if explicit kind for th else is first parameter!
class GetELR (th :: ELR a b) where
  getELR :: (String, ELR w v -> Bool)
instance GetELR 'ENone where
  getELR = ("ENone", isENone)
instance GetELR ('ELeft x) where
  getELR = ("ELeft", isELeft)
instance GetELR ('ERight y) where
  getELR = ("ERight", isERight)
instance GetELR ('EBoth x y) where
  getELR = ("EBoth", isEBoth)

isENone, isELeft, isERight, isEBoth :: ELR a b -> Bool
isENone ENone = True
isENone _ = False

isELeft ELeft {} = True
isELeft _ = False

isERight ERight {} = True
isERight _ = False

isEBoth EBoth {} = True
isEBoth _ = False

type family ENoneT lr where
  ENoneT (ELR _ _) = ()
  ENoneT o = GL.TypeError (
      'GL.Text "ENoneT: expected 'ELR a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family ELeftT lr where
  ELeftT (ELR a _) = a
  ELeftT o = GL.TypeError (
      'GL.Text "ELeftT: expected 'ELR a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family ERightT lr where
  ERightT (ELR _ b) = b
  ERightT o = GL.TypeError (
      'GL.Text "ERightT: expected 'ELR a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family EBothT lr where
  EBothT (ELR a b) = (a,b)
  EBothT o = GL.TypeError (
      'GL.Text "EBothT: expected 'ELR a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | tries to extract a () from the 'ENone' constructor
--
-- >>> pz @ENone' ENone
-- Val ()
--
-- >>> pz @ENone' (ERight 'a')
-- Fail "ENone' found ERight"
--
data ENone' deriving Show
instance P ENone' (ELR x y) where
  type PP ENone' (ELR x y) = ()
  eval _ opts lr =
    let msg0 = "ENone'"
    in pure $ case lr of
         ENone -> mkNode opts (Val ()) msg0 []
         EBoth _ _ -> mkNode opts (Fail (msg0 <> " found EBoth")) "" []
         ERight _ -> mkNode opts (Fail (msg0 <> " found ERight")) "" []
         ELeft _ -> mkNode opts (Fail (msg0 <> " found ELeft")) "" []

-- | tries to extract a value from the 'ELeft' constructor
--
-- >>> pz @(ELeft' >> Succ) (ELeft 20)
-- Val 21
--
-- >>> pz @(ELeft' >> Succ) (ERight 'a')
-- Fail "ELeft' found ERight"
--
data ELeft' deriving Show
instance Show a => P ELeft' (ELR a x) where
  type PP ELeft' (ELR a x) = a
  eval _ opts lr =
    let msg0 = "ELeft'"
    in pure $ case lr of
         ENone -> mkNode opts (Fail (msg0 <> " found ENone")) "" []
         EBoth _ _ -> mkNode opts (Fail (msg0 <> " found EBoth")) "" []
         ERight _ -> mkNode opts (Fail (msg0 <> " found ERight")) "" []
         ELeft a -> mkNode opts (Val a) (msg0 <> " " <> showL opts a) []

-- | tries to extract a value from the 'ERight' constructor
--
-- >>> pz @(ERight' >> Succ) (ERight 20)
-- Val 21
--
-- >>> pz @(ERight' >> Succ) (ELeft 'a')
-- Fail "ERight' found ELeft"
--
data ERight' deriving Show
instance Show a => P ERight' (ELR x a) where
  type PP ERight' (ELR x a) = a
  eval _ opts lr =
    let msg0 = "ERight'"
    in pure $ case lr of
         ENone -> mkNode opts (Fail (msg0 <> " found ENone")) "" []
         EBoth _ _ -> mkNode opts (Fail (msg0 <> " found EBoth")) "" []
         ELeft _ -> mkNode opts (Fail (msg0 <> " found ELeft")) "" []
         ERight a -> mkNode opts (Val a) (msg0 <> " " <> showL opts a) []

-- | tries to extract the values from the 'EBoth' constructor
--
-- >>> pz @(EBoth' >> Second Succ) (EBoth 1 'a')
-- Val (1,'b')
--
-- >>> pz @(ERight' >> Succ) (ELeft 'a')
-- Fail "ERight' found ELeft"
--
-- >>> pz @(EBoth' >> Second Succ) (ERight 8)
-- Fail "EBoth' found ERight"
--
data EBoth' deriving Show
instance ( Show a
         , Show b
        ) => P EBoth' (ELR a b) where
  type PP EBoth' (ELR a b) = (a,b)
  eval _ opts lr =
    let msg0 = "EBoth'"
    in pure $ case lr of
         ENone -> mkNode opts (Fail (msg0 <> " found ENone")) "" []
         ELeft _ -> mkNode opts (Fail (msg0 <> " found ELeft")) "" []
         ERight _ -> mkNode opts (Fail (msg0 <> " found ERight")) "" []
         EBoth a b -> mkNode opts (Val (a,b)) (msg0 <> " " <> showL opts (a,b)) []

-- | similar to 'Predicate.Data.These.PartitionThese' for 'ELR'. returns a 4-tuple with the results so use 'Fst' 'Snd' 'Thd' 'L4' to extract
--
-- >>> pz @PartitionELR [ELeft 'a', ENone, ERight 2, ELeft 'c', EBoth 'z' 1, ERight 4, EBoth 'a' 2, ERight 99, ENone]
-- Val ([(),()],"ac",[2,4,99],[('z',1),('a',2)])
--
-- >>> pz @PartitionELR [ELeft 4, ERight 'x', ERight 'y',EBoth 3 'b', ELeft 99, EBoth 5 'x']
-- Val ([],[4,99],"xy",[(3,'b'),(5,'x')])
--
-- >>> pz @PartitionELR [ENone,ELeft 1,ERight 'x',ELeft 4,ERight 'y',EBoth 9 'z',ELeft 10,EBoth 8 'y']
-- Val ([()],[1,4,10],"xy",[(9,'z'),(8,'y')])
--
data PartitionELR deriving Show

instance ( Show a
         , Show b
         ) => P PartitionELR [ELR a b] where
  type PP PartitionELR [ELR a b] = ([()], [a], [b], [(a, b)])
  eval _ opts as =
    let msg0 = "PartitionELR"
        b = partitionELR as
    in pure $ mkNode opts (Val b) (show3 opts msg0 b as) []

-- | partition ELR into 4 lists for each constructor: foldMap (yep ...)
partitionELR :: [ELR a b] -> ([()], [a], [b], [(a,b)])
partitionELR = foldMapStrict $
  \case
    ENone -> ([()],[],[],[])
    ELeft a -> ([],[a],[],[])
    ERight b -> ([],[],[b],[])
    EBoth a b -> ([],[],[],[(a,b)])

-- | convert ELR to a tuple with default values
fromELR :: a -> b -> ELR a b -> (a,b)
fromELR a b =
  \case
    ENone -> (a,b)
    ELeft v -> (v,b)
    ERight w -> (a,w)
    EBoth v w -> (v,w)

-- | destructs an ELR value
--   @n@ @ENone@ receives @(PP s x)@
--   @p@ @ELeft a@ receives @(PP s x,a)@
--   @q@ @ERight b@ receives @(PP s x,b)@
--   @r@ @EBoth a b@ receives @(PP s x,(a,b))@
--   @s@ points to the environment you want to pass in
--   @t@ points to the ELR value
--
-- >>> pz @(ELRIn Id '(Snd,L12) '(L11,Snd) Snd Fst Snd) ((999,'a'), EBoth 12 'x')
-- Val (12,'x')
--
-- >>> pz @(ELRIn Id '(Snd,L12) '(L11,Snd) Snd Fst Snd) ((999,'a'), ENone)
-- Val (999,'a')
--
-- >>> pz @(ELRIn Id '(Snd,L12) '(L11,Snd) Snd Fst Snd) ((999,'a'), ERight 'z')
-- Val (999,'z')
--
-- >>> pz @(ELRIn 999 Snd (Snd >> Len) (Snd >> Fst + Length Snd) () Id) (ELeft 13)
-- Val 13
--
-- >>> pz @(ELRIn 999 Snd (Snd >> Len) (Snd >> Fst + Length Snd) () Id) (ERight "abcdef")
-- Val 6
--
-- >>> pl @(ELRIn "none" "left" "right" "both" () Id) (ELeft (SG.Sum 12))
-- Present "left" (ELRIn "left" | ELeft Sum {getSum = 12})
-- Val "left"
--
-- >>> pl @(ELRIn '("",2) '(Snd,999) '("no value",Snd) Snd () Id) (EBoth "Ab" 13)
-- Present ("Ab",13) (ELRIn ("Ab",13) | EBoth "Ab" 13)
-- Val ("Ab",13)
--
-- >>> pl @(ELRIn '("",2) '(Snd,999) '("no value",Snd) Snd () Id) (ELeft "Ab")
-- Present ("Ab",999) (ELRIn ("Ab",999) | ELeft "Ab")
-- Val ("Ab",999)
--
-- >>> pl @(ELRIn '("",2) '(Snd,999) '("no value",Snd) Snd () Id) ENone
-- Present ("",2) (ELRIn ("",2) | ENone ())
-- Val ("",2)
--
data ELRIn n p q r s t deriving Show

instance ( Show a
         , Show b
         , Show (PP r (y,(a,b)))
         , P n y
         , P p (y,a)
         , P q (y,b)
         , P r (y,(a,b))
         , PP n y ~ PP p (y,a)
         , PP p (y,a) ~ PP q (y,b)
         , PP q (y,b) ~ PP r (y,(a,b))
         , P s x
         , P t x
         , PP t x ~ ELR a b
         , PP s x ~ y
         )  => P (ELRIn n p q r s t) x where
  type PP (ELRIn n p q r s t) x = PP n (PP s x)
  eval _ opts x = do
    let msg0 = "ELRIn"
    lr <- runPQ NoInline msg0 (Proxy @s) (Proxy @t) opts x []
    case lr of
      Left e -> pure e
      Right (s,t,ss,tt) -> do
         let hhs = [hh ss, hh tt]
         case t of
            ENone -> do
              let msg1 = "ENone "
                  msg2 = msg0 <> msg1
              nn <- eval (Proxy @n) opts s
              pure $ case getValueLR NoInline opts (msg2 <> "n failed") nn hhs of
                   Left e -> e
                   Right c -> mkNode opts (Val c) (show3' opts msg0 c msg1 ()) (hhs ++ [hh nn])
            ELeft a -> do
              let msg1 = "ELeft "
                  msg2 = msg0 <> msg1
              pp <- eval (Proxy @p) opts (s,a)
              pure $ case getValueLR NoInline opts (msg2 <> "p failed") pp hhs of
                   Left e -> e
                   Right c -> mkNode opts (Val c) (show3' opts msg0 c msg1 a) (hhs ++ [hh pp])
            ERight b -> do
              let msg1 = "ERight "
                  msg2 = msg0 <> msg1
              qq <- eval (Proxy @q) opts (s,b)
              pure $ case getValueLR NoInline opts (msg2 <> "q failed") qq hhs of
                   Left e -> e
                   Right c -> mkNode opts (Val c) (show3' opts msg0 c msg1 b) (hhs ++ [hh qq])
            EBoth a b -> do
              let msg1 = "EBoth "
                  msg2 = msg0 <> msg1
              rr <- eval (Proxy @r) opts (s,(a,b))
              pure $ case getValueLR NoInline opts (msg2 <> "r failed") rr hhs of
                   Left e -> e
                   Right c -> mkNode opts (Val c) (show3' opts msg0 c "" (EBoth a b)) (hhs ++ [hh rr])

-- | simple version of 'ELRIn' with Id as the ELR value and the environment set to ()
--
-- >>> pz @(ELRId '(999,"oops") '(Id,"fromleft") '(888,Id) Id) (EBoth 222 "ok")
-- Val (222,"ok")
--
-- >>> pz @(ELRId '(999,"oops") '(Id,"fromleft") '(888,Id) Id) (ERight "ok")
-- Val (888,"ok")
--
-- >>> pz @(ELRId '(999,"oops") '(Id,"fromleft") '(888,Id) Id) ENone
-- Val (999,"oops")
--
-- >>> pz @(ELRId '(999,"oops") '(Id,"fromleft") '(888,Id) Id) (ELeft 123)
-- Val (123,"fromleft")
--
data ELRId n p q r deriving Show
type ELRIdT n p q r = ELRIn n (Snd >> p) (Snd >> q) (Snd >> r) () Id

instance P (ELRIdT n p q r) x => P (ELRId n p q r) x where
  type PP (ELRId n p q r) x = PP (ELRIdT n p q r) x
  eval _ = eval (Proxy @(ELRIdT n p q r))

-- | provide the default pair in @s@ and @t@ refers to ELR
--
-- >>> pz @(ELRDef Fst Snd) ((999,"oops"),EBoth 2 "xx")
-- Val (2,"xx")
--
-- >>> pz @(ELRDef Fst Snd) ((999,"oops"),ENone)
-- Val (999,"oops")
--
-- >>> pz @(ELRDef Fst Snd) ((999,"oops"),ERight "ok")
-- Val (999,"ok")
--
data ELRDef s t deriving Show
type ELRDefT s t = ELRIn Id '(Snd,L12) '(L11,Snd) Snd s t

instance P (ELRDefT s t) x => P (ELRDef s t) x where
  type PP (ELRDef s t) x = PP (ELRDefT s t) x
  eval _ = eval (Proxy @(ELRDefT s t))

-- | similar to 'Predicate.Data.These.TheseDef'''
--
-- >>> pz @(ELRDef'' 999 Id Len (Fst + Length Snd)) (ELeft 13)
-- Val 13
--
-- >>> pz @(ELRDef'' 999 Id Len (Fst + Length Snd)) ENone
-- Val 999
--
-- >>> pz @(ELRDef'' 999 Id Len (Fst + Length Snd)) (ERight "this is a long string")
-- Val 21
--
-- >>> pz @(ELRDef'' 999 Id Len (Fst + Length Snd)) (EBoth 20 "somedata")
-- Val 28
--
-- >>> pz @(ELRDef'' (FailT _ "err") (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (ERight "this is a long string")
-- Val (Right "this is a long string")
--
-- >>> pz @(ELRDef'' (FailT _ "err") (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) ENone
-- Fail "err"
--
-- >>> pz @(ELRDef'' (FailT _ "err") (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (EBoth 1 "this is a long string")
-- Val (Right "this is a long string")
--
-- >>> pz @(ELRDef'' (FailT _ "err") (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (EBoth 100 "this is a long string")
-- Val (Left 100)
--
-- >>> pl @(ELRDef'' "none" "left" "right" "both") (ELeft (SG.Sum 12))
-- Present "left" (ELRIn "left" | ELeft Sum {getSum = 12})
-- Val "left"
--
-- >>> pl @(ELRDef'' (FailT _ "err") (Id &&& 999) ("no value" &&& Id) Id) (EBoth "Ab" 13)
-- Present ("Ab",13) (ELRIn ("Ab",13) | EBoth "Ab" 13)
-- Val ("Ab",13)
--
-- >>> pl @(ELRDef'' (FailT _ "err") (Id &&& 999) ("no value" &&& Id) Id) (ELeft "Ab")
-- Present ("Ab",999) (ELRIn ("Ab",999) | ELeft "Ab")
-- Val ("Ab",999)
--
-- >>> pl @(ELRDef'' (FailT _ "err") (Id &&& 999) ("no value" &&& Id) Id) (ERight 13)
-- Present ("no value",13) (ELRIn ("no value",13) | ERight 13)
-- Val ("no value",13)
--
data ELRDef'' n p q r deriving Show
type ELRDefT'' n p q r = ELRIn n (Snd >> p) (Snd >> q) (Snd >> r) () Id

instance P (ELRDefT'' n p q r) x => P (ELRDef'' n p q r) x where
  type PP (ELRDef'' n p q r) x = PP (ELRDefT'' n p q r) x
  eval _ = eval (Proxy @(ELRDefT'' n p q r))

-- | get ENone or run @p@: really only useful when p is set to Fail
--
-- >>> pz @(ENoneDef (FailT _ "not ENone") Id) ENone
-- Val ()
--
-- >>> pz @(ENoneDef (FailT _ "not ENone") Id) (ELeft 1)
-- Fail "not ENone"
--
data ENoneDef p q deriving Show
type ENoneDefT p q = ELRIn Id p p p () q

instance P (ENoneDefT p q) x => P (ENoneDef p q) x where
  type PP (ENoneDef p q) x = PP (ENoneDefT p q) x
  eval _ = eval (Proxy @(ENoneDefT p q))

-- | get ELeft or default value
--
-- >>> pz @(ELeftDef 999 Id) ENone
-- Val 999
--
-- >>> pz @(ELeftDef 999 Id) (ERight "sdf")
-- Val 999
--
-- >>> pz @(ELeftDef 999 Id) (ELeft 1)
-- Val 1
--
data ELeftDef p q deriving Show
type ELeftDefT p q = ELRIn p Snd p p () q

instance P (ELeftDefT p q) x => P (ELeftDef p q) x where
  type PP (ELeftDef p q) x = PP (ELeftDefT p q) x
  eval _ = eval (Proxy @(ELeftDefT p q))

-- | get ERight or default value
--
-- >>> pz @(ERightDef 999 Id) ENone
-- Val 999
--
-- >>> pz @(ERightDef 999 Id) (ELeft "sdf")
-- Val 999
--
-- >>> pz @(ERightDef 999 Id) (ERight 1)
-- Val 1
--
data ERightDef p q deriving Show
type ERightDefT p q = ELRIn p p Snd p () q

instance P (ERightDefT p q) x => P (ERightDef p q) x where
  type PP (ERightDef p q) x = PP (ERightDefT p q) x
  eval _ = eval (Proxy @(ERightDefT p q))

-- | get EBoth or default value
--
-- >>> pz @(EBothDef '(999,"xx") Id) ENone
-- Val (999,"xx")
--
-- >>> pz @(EBothDef '(999,"xx") Id) (ERight "abc")
-- Val (999,"xx")
--
-- >>> pz @(EBothDef '(999,"xx") Id) (ELeft 1)
-- Val (999,"xx")
--
-- >>> pz @(EBothDef '(999,"xx") Id) (EBoth 1 "abc")
-- Val (1,"abc")
--
data EBothDef p q deriving Show
type EBothDefT p q = ELRIn p p p Snd () q

instance P (EBothDefT p q) x => P (EBothDef p q) x where
  type PP (EBothDef p q) x = PP (EBothDefT p q) x
  eval _ = eval (Proxy @(EBothDefT p q))



