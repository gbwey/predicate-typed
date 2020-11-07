{-# LANGUAGE TypeOperators #-}
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
-- | Elr related methods
module Predicate.Data.Elr (

 -- ** destructors
    ENone'
  , ELeft'
  , ERight'
  , EBoth'

  , ElrIn
  , ElrId
  , ElrPair
  , ElrInSimple
  , PartitionElr

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

  , These2Elr
  , Elr2These
 ) where
import Predicate.Core
import Predicate.Util
import Predicate.Elr
import Data.Kind (Type)
import Control.Lens
import Data.Proxy (Proxy(..))
import Data.These (These)
-- $setup
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG
-- >>> :m + Data.These

-- | 'ENone' constructor
--
-- >>> pz @(Proxy Int >> MkENone' UnproxyT 10) []
-- Val ENone
--
data MkENone' t t1 deriving Show

instance P (MkENone' t t1) x where
  type PP (MkENone' t t1) x = Elr (PP t x) (PP t1 x)
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
  type PP (MkELeft' t p) x = Elr (PP p x) (PP t x)
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
  type PP (MkERight' t p) x = Elr (PP t x) (PP p x)
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
-- >>> pz @(MkENone _ _ <> MkELeft _ '[1] <> MkERight _ "abc" <> MkELeft _ '[2] <> MkEBoth '[3,4,5] "def") ()
-- Val (EBoth [1,2,3,4,5] "abcdef")
--
data MkEBoth p q deriving Show
instance ( P p a
         , P q a
         ) => P (MkEBoth p q) a where
  type PP (MkEBoth p q) a = Elr (PP p a) (PP q a)
  eval _ opts a = do
    let msg0 = "MkEBoth"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = EBoth p q
        in mkNode opts (Val d) msg0 [hh pp, hh qq]

data IsElr (th :: Elr x y) deriving Show
-- x y can be anything

-- trying to avoid Show instance because more likely to have ambiguity errors
instance ( x ~ Elr a b
         , Show a
         , Show b
         , GetElr th
         ) => P (IsElr (th :: Elr x1 x2)) x where
  type PP (IsElr th) x = Bool
  eval _ opts x =
    let msg0 = "Is"
        (t,f) = getElr @_ @_ @th
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
type IsENoneT = IsElr 'ENone

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
type IsELeftT = IsElr ('ELeft '())

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
type IsERightT = IsElr ('ERight '())

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
type IsEBothT = IsElr ('EBoth '() '())

instance P IsEBothT x => P IsEBoth x where
  type PP IsEBoth x = PP IsEBothT x
  eval _ = evalBool (Proxy @IsEBothT)

-- | tries to extract a () from the 'ENone' constructor
--
-- >>> pz @ENone' ENone
-- Val ()
--
-- >>> pz @ENone' (ERight 'a')
-- Fail "ENone' found ERight"
--
data ENone' deriving Show

instance P ENone' (Elr x y) where
  type PP ENone' (Elr x y) = ()
  eval _ opts lr =
    let msg0 = "ENone'"
    in pure $ case lr of
         ENone -> mkNode opts (Val ()) msg0 []
         _ -> mkNode opts (Fail (msg0 <> " found " <> showElr lr)) "" []

-- | tries to extract a value from the 'ELeft' constructor
--
-- >>> pz @(ELeft' >> Succ) (ELeft 20)
-- Val 21
--
-- >>> pz @(ELeft' >> Succ) (ERight 'a')
-- Fail "ELeft' found ERight"
--
data ELeft' deriving Show

instance Show a => P ELeft' (Elr a x) where
  type PP ELeft' (Elr a x) = a
  eval _ opts lr =
    let msg0 = "ELeft'"
    in pure $ case lr of
         ELeft a -> mkNode opts (Val a) (msg0 <> " " <> showL opts a) []
         _ -> mkNode opts (Fail (msg0 <> " found " <> showElr lr)) "" []

-- | tries to extract a value from the 'ERight' constructor
--
-- >>> pz @(ERight' >> Succ) (ERight 20)
-- Val 21
--
-- >>> pz @(ERight' >> Succ) (ELeft 'a')
-- Fail "ERight' found ELeft"
--
data ERight' deriving Show

instance Show a => P ERight' (Elr x a) where
  type PP ERight' (Elr x a) = a
  eval _ opts lr =
    let msg0 = "ERight'"
    in pure $ case lr of
         ERight a -> mkNode opts (Val a) (msg0 <> " " <> showL opts a) []
         _ -> mkNode opts (Fail (msg0 <> " found " <> showElr lr)) "" []

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
        ) => P EBoth' (Elr a b) where
  type PP EBoth' (Elr a b) = (a,b)
  eval _ opts lr =
    let msg0 = "EBoth'"
    in pure $ case lr of
         EBoth a b -> mkNode opts (Val (a,b)) (msg0 <> " " <> showL opts (a,b)) []
         _ -> mkNode opts (Fail (msg0 <> " found " <> showElr lr)) "" []

-- | similar to 'Predicate.Data.These.PartitionThese' for 'Elr'. returns a 4-tuple with the results so use 'Fst' 'Snd' 'Thd' 'L4' to extract
--
-- >>> pz @PartitionElr [ELeft 'a', ENone, ERight 2, ELeft 'c', EBoth 'z' 1, ERight 4, EBoth 'a' 2, ERight 99, ENone]
-- Val ([(),()],"ac",[2,4,99],[('z',1),('a',2)])
--
-- >>> pz @PartitionElr [ELeft 4, ERight 'x', ERight 'y',EBoth 3 'b', ELeft 99, EBoth 5 'x']
-- Val ([],[4,99],"xy",[(3,'b'),(5,'x')])
--
-- >>> pz @PartitionElr [ENone,ELeft 1,ERight 'x',ELeft 4,ERight 'y',EBoth 9 'z',ELeft 10,EBoth 8 'y']
-- Val ([()],[1,4,10],"xy",[(9,'z'),(8,'y')])
--
data PartitionElr deriving Show

instance ( Show a
         , Show b
         ) => P PartitionElr [Elr a b] where
  type PP PartitionElr [Elr a b] = ([()], [a], [b], [(a, b)])
  eval _ opts as =
    let msg0 = "PartitionElr"
        b = partitionElr as
    in pure $ mkNode opts (Val b) (show3 opts msg0 b as) []

-- | destructs an Elr value
--   @n@ @ENone@ receives @(PP s x)@
--   @p@ @ELeft a@ receives @(PP s x,a)@
--   @q@ @ERight b@ receives @(PP s x,b)@
--   @r@ @EBoth a b@ receives @(PP s x,(a,b))@
--   @s@ points to the environment you want to pass in
--   @t@ points to the Elr value
--
-- >>> pz @(ElrIn Id '(Snd,L12) '(L11,Snd) Snd Fst Snd) ((999,'a'), EBoth 12 'x')
-- Val (12,'x')
--
-- >>> pz @(ElrIn Id '(Snd,L12) '(L11,Snd) Snd Fst Snd) ((999,'a'), ENone)
-- Val (999,'a')
--
-- >>> pz @(ElrIn Id '(Snd,L12) '(L11,Snd) Snd Fst Snd) ((999,'a'), ERight 'z')
-- Val (999,'z')
--
-- >>> pz @(ElrIn 999 Snd (Snd >> Len) (Snd >> Fst + Length Snd) () Id) (ELeft 13)
-- Val 13
--
-- >>> pz @(ElrIn 999 Snd (Snd >> Len) (Snd >> Fst + Length Snd) () Id) (ERight "abcdef")
-- Val 6
--
-- >>> pl @(ElrIn "none" "left" "right" "both" () Id) (ELeft (SG.Sum 12))
-- Present "left" (ElrIn(ELeft) "left" | Sum {getSum = 12})
-- Val "left"
--
-- >>> pl @(ElrIn '("",2) '(Snd,999) '("no value",Snd) Snd () Id) (EBoth "Ab" 13)
-- Present ("Ab",13) (ElrIn(EBoth) ("Ab",13) | ("Ab",13))
-- Val ("Ab",13)
--
-- >>> pl @(ElrIn '("",2) '(Snd,999) '("no value",Snd) Snd () Id) (ELeft "Ab")
-- Present ("Ab",999) (ElrIn(ELeft) ("Ab",999) | "Ab")
-- Val ("Ab",999)
--
-- >>> pl @(ElrIn '("",2) '(Snd,999) '("no value",Snd) Snd () Id) ENone
-- Present ("",2) (ElrIn(ENone) ("",2) | ())
-- Val ("",2)
--
-- >>> pl @(ElrIn (FailT _ "none found") '(Snd,"fromleft") '(888,Snd) Snd () Id)  ENone
-- Error none found (ElrIn(ENone) n failed)
-- Fail "none found"
--
data ElrIn n p q r s t deriving Show

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
         , PP t x ~ Elr a b
         , PP s x ~ y
         )  => P (ElrIn n p q r s t) x where
  type PP (ElrIn n p q r s t) x = PP n (PP s x)
  eval _ opts x = do
    let msg0 = "ElrIn"
    lr <- runPQ NoInline msg0 (Proxy @s) (Proxy @t) opts x []
    case lr of
      Left e -> pure e
      Right (s,t,ss,tt) -> do
         let hhs = [hh ss, hh tt]
         case t of
            ENone -> do
              let msg1 = msg0 <> "(ENone)"
              nn <- eval (Proxy @n) opts s
              pure $ case getValueLR NoInline opts (msg1 <> " n failed") nn hhs of
                   Left e -> e
                   Right c -> mkNodeCopy opts nn (show3 opts msg1 c ()) hhs
            ELeft a -> do
              let msg1 = msg0 <> "(ELeft)"
              pp <- eval (Proxy @p) opts (s,a)
              pure $ case getValueLR NoInline opts (msg1 <> " p failed") pp hhs of
                   Left e -> e
                   Right c -> mkNodeCopy opts pp (show3 opts msg1 c a) hhs
            ERight b -> do
              let msg1 = msg0 <> "(ERight)"
              qq <- eval (Proxy @q) opts (s,b)
              pure $ case getValueLR NoInline opts (msg1 <> " q failed") qq hhs of
                   Left e -> e
                   Right c -> mkNodeCopy opts qq (show3 opts msg1 c b) hhs
            EBoth a b -> do
              let msg1 = msg0 <> "(EBoth)"
              rr <- eval (Proxy @r) opts (s,(a,b))
              pure $ case getValueLR NoInline opts (msg1 <> " r failed") rr hhs of
                   Left e -> e
                   Right c -> mkNodeCopy opts rr (show3 opts msg1 c (a,b)) hhs

-- | simple version of 'ElrIn' with Id as the Elr value and the environment set to ()
--
-- >>> pz @(ElrId '(999,"oops") '(Id,"fromleft") '(888,Id) Id) (EBoth 222 "ok")
-- Val (222,"ok")
--
-- >>> pz @(ElrId '(999,"oops") '(Id,"fromleft") '(888,Id) Id) (ERight "ok")
-- Val (888,"ok")
--
-- >>> pz @(ElrId '(999,"oops") '(Id,"fromleft") '(888,Id) Id) ENone
-- Val (999,"oops")
--
-- >>> pz @(ElrId '(999,"oops") '(Id,"fromleft") '(888,Id) Id) (ELeft 123)
-- Val (123,"fromleft")
--
-- >>> pl @(ElrId (FailT _ "none found" ) '(Id,"fromleft") '(888,Id) Id) ENone
-- Error none found (ElrIn(ENone) n failed)
-- Fail "none found"
--
data ElrId n p q r deriving Show

type ElrIdT n p q r = ElrIn n (Snd >> p) (Snd >> q) (Snd >> r) () Id

instance P (ElrIdT n p q r) x => P (ElrId n p q r) x where
  type PP (ElrId n p q r) x = PP (ElrIdT n p q r) x
  eval _ = eval (Proxy @(ElrIdT n p q r))

-- | creates a pair where the values are filled in by @s@ and @t@ holds the Elr value
--
-- >>> pz @(ElrPair Fst Snd) ((999,"oops"),EBoth 2 "xx")
-- Val (2,"xx")
--
-- >>> pz @(ElrPair Fst Snd) ((999,"oops"),ENone)
-- Val (999,"oops")
--
-- >>> pz @(ElrPair Fst Snd) ((999,"oops"),ERight "ok")
-- Val (999,"ok")
--
data ElrPair s t deriving Show

type ElrPairT s t = ElrIn Id '(Snd,L12) '(L11,Snd) Snd s t

instance P (ElrPairT s t) x => P (ElrPair s t) x where
  type PP (ElrPair s t) x = PP (ElrPairT s t) x
  eval _ = eval (Proxy @(ElrPairT s t))

-- | similar to 'ElrIn' but without an environment @s@ and uses Id for @t@
--
-- >>> pz @(ElrInSimple 999 Id Len (Fst + Length Snd)) (ELeft 13)
-- Val 13
--
-- >>> pz @(ElrInSimple 999 Id Len (Fst + Length Snd)) ENone
-- Val 999
--
-- >>> pz @(ElrInSimple 999 Id Len (Fst + Length Snd)) (ERight "this is a long string")
-- Val 21
--
-- >>> pz @(ElrInSimple 999 Id Len (Fst + Length Snd)) (EBoth 20 "somedata")
-- Val 28
--
-- >>> pz @(ElrInSimple (FailT _ "err") (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (ERight "this is a long string")
-- Val (Right "this is a long string")
--
-- >>> pz @(ElrInSimple (FailT _ "err") (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) ENone
-- Fail "err"
--
-- >>> pz @(ElrInSimple (FailT _ "err") (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (EBoth 1 "this is a long string")
-- Val (Right "this is a long string")
--
-- >>> pz @(ElrInSimple (FailT _ "err") (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (EBoth 100 "this is a long string")
-- Val (Left 100)
--
-- >>> pl @(ElrInSimple "none" "left" "right" "both") (ELeft (SG.Sum 12))
-- Present "left" (ElrIn(ELeft) "left" | Sum {getSum = 12})
-- Val "left"
--
-- >>> pl @(ElrInSimple (FailT _ "err") (Id &&& 999) ("no value" &&& Id) Id) (EBoth "Ab" 13)
-- Present ("Ab",13) (ElrIn(EBoth) ("Ab",13) | ("Ab",13))
-- Val ("Ab",13)
--
-- >>> pl @(ElrInSimple (FailT _ "err") (Id &&& 999) ("no value" &&& Id) Id) (ELeft "Ab")
-- Present ("Ab",999) (ElrIn(ELeft) ("Ab",999) | "Ab")
-- Val ("Ab",999)
--
-- >>> pl @(ElrInSimple (FailT _ "err") (Id &&& 999) ("no value" &&& Id) Id) (ERight 13)
-- Present ("no value",13) (ElrIn(ERight) ("no value",13) | 13)
-- Val ("no value",13)
--
data ElrInSimple n p q r deriving Show

type ElrInSimpleT n p q r = ElrIn n (Snd >> p) (Snd >> q) (Snd >> r) () Id

instance P (ElrInSimpleT n p q r) x => P (ElrInSimple n p q r) x where
  type PP (ElrInSimple n p q r) x = PP (ElrInSimpleT n p q r) x
  eval _ = eval (Proxy @(ElrInSimpleT n p q r))

-- | get ENone or run @p@: really only useful when p is set to Fail: where @q@ is the environment and @r@ is the Elr value
--
-- >>> pz @(ENoneDef (FailT _ "not ENone") () Id) ENone
-- Val ()
--
-- >>> pz @(ENoneDef (FailT _ "not ENone") () Id) (ELeft 1)
-- Fail "not ENone"
--
-- >>> pz @(ENoneDef (FailT _ Id) Fst Snd) ("not right",EBoth 1 2)
-- Fail "not right"
--
data ENoneDef p q r deriving Show

type ENoneDefT p q r = ElrIn Id (Fst >> p) (Fst >> p) (Fst >> p) q r

instance P (ENoneDefT p q r) x => P (ENoneDef p q r) x where
  type PP (ENoneDef p q r) x = PP (ENoneDefT p q r) x
  eval _ = eval (Proxy @(ENoneDefT p q r))

-- | get ELeft or use the default value @p@: @q@ is the environment and @r@ is the Elr value
--
-- >>> pz @(ELeftDef Id Fst Snd) (999,ENone)
-- Val 999
--
-- >>> pz @(ELeftDef 999 () Id) (ERight "sdf")
-- Val 999
--
-- >>> pz @(ELeftDef 999 () Id) (ELeft 1)
-- Val 1
--
data ELeftDef p q r deriving Show

type ELeftDefT p q r = ElrIn p Snd (Fst >> p) (Fst >> p) q r

instance P (ELeftDefT p q r) x => P (ELeftDef p q r) x where
  type PP (ELeftDef p q r) x = PP (ELeftDefT p q r) x
  eval _ = eval (Proxy @(ELeftDefT p q r))

-- | get ERight or use the default value @p@: @q@ is the environment and @r@ is the Elr value
--
-- >>> pz @(ERightDef 999 () Id) ENone
-- Val 999
--
-- >>> pz @(ERightDef 999 () Id) (ELeft "sdf")
-- Val 999
--
-- >>> pz @(ERightDef 999 Fst Snd) (999,ERight 1)
-- Val 1
--
data ERightDef p q r deriving Show

type ERightDefT p q r = ElrIn p (Fst >> p) Snd (Fst >> p) q r

instance P (ERightDefT p q r) x => P (ERightDef p q r) x where
  type PP (ERightDef p q r) x = PP (ERightDefT p q r) x
  eval _ = eval (Proxy @(ERightDefT p q r))

-- | get EBoth or use the default value @p@: @q@ is the environment and @r@ is the Elr value
--
-- >>> pz @(EBothDef '(999,"xx") () Id) ENone
-- Val (999,"xx")
--
-- >>> pz @(EBothDef '(999,"xx") () Id) (ERight "abc")
-- Val (999,"xx")
--
-- >>> pz @(EBothDef '(999,"xx") () Id) (ELeft 1)
-- Val (999,"xx")
--
-- >>> pz @(EBothDef '(999,"xx") () Id) (EBoth 1 "abc")
-- Val (1,"abc")
--
-- >>> pz @(EBothDef Id Fst Snd) ((999,"xx"),ENone)
-- Val (999,"xx")
--
data EBothDef p q r deriving Show

type EBothDefT p q r = ElrIn p (Fst >> p) (Fst >> p) Snd q r

instance P (EBothDefT p q r) x => P (EBothDef p q r) x where
  type PP (EBothDef p q r) x = PP (EBothDefT p q r) x
  eval _ = eval (Proxy @(EBothDefT p q r))

-- | converts 'Elr' to 'These'
--
-- >>> pz @Elr2These ENone
-- Val Nothing
--
-- >>> pz @Elr2These (ELeft 123)
-- Val (Just (This 123))
--
data Elr2These deriving Show

instance P Elr2These (Elr a b) where
  type PP Elr2These (Elr a b) = Maybe (These a b)
  eval _ opts x =
    let msg0 = "Elr2These"
        b = x ^. _elr2These
    in pure $ mkNode opts (Val b) msg0 []

-- | converts 'These' to 'Elr'
--
-- >>> pz @These2Elr (These 12 'x')
-- Val (EBoth 12 'x')
--
-- >>> pz @These2Elr (This 123)
-- Val (ELeft 123)
--
data These2Elr deriving Show

instance P These2Elr (These a b) where
  type PP These2Elr (These a b) = Elr a b
  eval _ opts x =
    let msg0 = "These2Elr"
        b = _elr2These # Just x
    in pure $ mkNode opts (Val b) msg0 []
