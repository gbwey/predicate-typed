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
-- | promoted 'These' functions
module Predicate.Data.These (
 -- ** boolean predicates
    IsThis
  , IsThat
  , IsThese

 -- ** constructors
  , MkThis
  , MkThis'
  , MkThat
  , MkThat'
  , MkThese

 -- ** destructors
  , This'
  , That'
  , These'
  , ThisDef
  , ThisFail
  , ThatDef
  , ThatFail
  , TheseDef
  , TheseFail
  , Thiss
  , Thats
  , Theses
  , Theres
  , Heres
  , TheseIn
  , TheseId
  , ThesePair
  , ThisDef'
  , ThatDef'
  , TheseDef'
  , TheseInSimple
  , PartitionThese

 -- ** miscellaneous
  , ZipThese

 -- ** type families
  , TheseInT
 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Data.Proxy (Proxy(Proxy))
import Data.Kind (Type)
import Data.These (partitionThese, These(..))
import qualified Data.These.Combinators as TheseC
import qualified GHC.TypeLits as GL
import GHC.TypeLits (ErrorMessage((:$$:),(:<>:)))

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG

-- | similar to 'Data.These.partitionThese'. returns a 3-tuple with the results so use 'Fst' 'Snd' 'Thd' to extract
--
-- >>> pz @PartitionThese [This 'a', That 2, This 'c', These 'z' 1, That 4, These 'a' 2, That 99]
-- Val ("ac",[2,4,99],[('z',1),('a',2)])
--
-- >>> pl @PartitionThese [This 4, That 'x', That 'y',These 3 'b', This 99, These 5 'x']
-- Present ([4,99],"xy",[(3,'b'),(5,'x')]) (PartitionThese ([4,99],"xy",[(3,'b'),(5,'x')]) | [This 4,That 'x',That 'y',These 3 'b',This 99,These 5 'x'])
-- Val ([4,99],"xy",[(3,'b'),(5,'x')])
--
-- >>> pl @PartitionThese [This 1,That 'x',This 4,That 'y',These 9 'z',This 10,These 8 'y']
-- Present ([1,4,10],"xy",[(9,'z'),(8,'y')]) (PartitionThese ([1,4,10],"xy",[(9,'z'),(8,'y')]) | [This 1,That 'x',This 4,That 'y',These 9 'z',This 10,These 8 'y'])
-- Val ([1,4,10],"xy",[(9,'z'),(8,'y')])
--
data PartitionThese deriving Show

instance ( Show a
         , Show b
         ) => P PartitionThese [These a b] where
  type PP PartitionThese [These a b] = ([a], [b], [(a, b)])
  eval _ opts as =
    let msg0 = "PartitionThese"
        b = partitionThese as
    in pure $ mkNode opts (Val b) (show3 opts msg0 b as) []

-- | similar to 'Data.These.Combinators.catThis'
--
-- >>> pz @(Thiss) [That 1, This 'a', These 'b' 33, This 'd', That 4]
-- Val "ad"
--
-- >>> pz @(Thiss) [That 1, This 'a', These 'b' 33]
-- Val "a"
--
-- >>> pz @(Thiss) [That 1, That 9, These 1 33]
-- Val []
--
data Thiss deriving Show
type ThissT = PartitionThese >> Fst

instance P ThissT x => P Thiss x where
  type PP Thiss x = PP ThissT x
  eval _ = eval (Proxy @ThissT)

-- | similar to 'Data.These.Combinators.catThat'
--
-- >>> pl @Thats [This 1, This 10,That 'x', This 99, That 'y']
-- Present "xy" ((>>) "xy" | {Snd "xy" | ([1,10,99],"xy",[])})
-- Val "xy"
--
data Thats deriving Show
type ThatsT = PartitionThese >> Snd

instance P ThatsT x => P Thats x where
  type PP Thats x = PP ThatsT x
  eval _ = eval (Proxy @ThatsT)

-- | similar to 'Data.These.Combinators.catThese'
--
-- >>> pz @(ZipThese Id Tail >> Theses) [1..10]
-- Val [(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10)]
--
data Theses deriving Show
type ThesesT = PartitionThese >> Thd

instance P ThesesT x => P Theses x where
  type PP Theses x = PP ThesesT x
  eval _ = eval (Proxy @ThesesT)

-- | similar to 'Data.These.Combinators.catHere'
--
-- >>> pz @(ZipThese Id Tail >> Heres) [1..10]
-- Val [1,2,3,4,5,6,7,8,9,10]
--
data Heres deriving Show

instance ( Show a
         , Show b
         ) => P Heres [These a b] where
  type PP Heres [These a b] = [a]
  eval _ opts as =
    let msg0 = "Heres"
        b = TheseC.catHere as
    in pure $ mkNode opts (Val b) (show3 opts msg0 b as) []

-- | similar to 'Data.These.Combinators.catThere'
--
-- >>> pz @(ZipThese Id Tail >> Theres) [1..10]
-- Val [2,3,4,5,6,7,8,9,10]
--
data Theres deriving Show

instance ( Show a
         , Show b
         ) => P Theres [These a b] where
  type PP Theres [These a b] = [b]
  eval _ opts as =
    let msg0 = "Theres"
        b = TheseC.catThere as
    in pure $ mkNode opts (Val b) (show3 opts msg0 b as) []

-- | 'Data.These.This' constructor
--
-- >>> pz @(Proxy Int >> MkThis' UnproxyT 10) []
-- Val (This 10)
--
data MkThis' t p deriving Show

instance ( P p x
         , Show (PP p x)
         ) => P (MkThis' t p) x where
  type PP (MkThis' t p) x = These (PP p x) (PP t x)
  eval _ opts x = do
    let msg0 = "MkThis"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = This p
        in mkNode opts (Val d) (msg0 <> " This " <> showL opts p) [hh pp]

-- | 'Data.These.This' constructor
--
-- >>> pl @(MkThis () Id) 'x'
-- Present This 'x' (MkThis This 'x')
-- Val (This 'x')
--
-- >>> pl @(MkThis () Fst) ('x',True)
-- Present This 'x' (MkThis This 'x')
-- Val (This 'x')
--
-- >>> pz @(MkThis _ Id) 44
-- Val (This 44)
--
data MkThis (t :: Type) p deriving Show
type MkThisT (t :: Type) p = MkThis' (Hole t) p

instance P (MkThisT t p) x => P (MkThis t p) x where
  type PP (MkThis t p) x = PP (MkThisT t p) x
  eval _ = eval (Proxy @(MkThisT t p))

-- | similar to 'MkThat' where @t@ references the type
data MkThat' t p deriving Show

instance ( Show (PP p x)
         , P p x
         ) => P (MkThat' t p) x where
  type PP (MkThat' t p) x = These (PP t x) (PP p x)
  eval _ opts x = do
    let msg0 = "MkThat"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = That p
        in mkNode opts (Val d) (msg0 <> " That " <> showL opts p) [hh pp]

-- | 'Data.These.That' constructor
--
-- >>> pz @(MkThat _ Id) 44
-- Val (That 44)
--
-- >>> pz @(MkThat _ "Abc" <> MkThis _ '[1,2] <> MkThese [3,4] "def") ()
-- Val (These [1,2,3,4] "Abcdef")
--
-- >>> pl @(MkThat () Id) 'x'
-- Present That 'x' (MkThat That 'x')
-- Val (That 'x')
--
data MkThat (t :: Type) p deriving Show
type MkThatT (t :: Type) p = MkThat' (Hole t) p

instance P (MkThatT t p) x => P (MkThat t p) x where
  type PP (MkThat t p) x = PP (MkThatT t p) x
  eval _ = eval (Proxy @(MkThatT t p))

-- type MkThat t p = MkThis t p >> Swap
-- type MkThat' (t :: Type) = Pure (These t) Id -- t has to be a semigroup

-- | 'Data.These.These' constructor
--
-- >>> pz @(MkThese Fst Snd) (44,'x')
-- Val (These 44 'x')
--
-- >>> pl @(MkThese Id 'True) 'x'
-- Present These 'x' True (MkThese These 'x' True)
-- Val (These 'x' True)
--
data MkThese p q deriving Show
instance ( P p a
         , P q a
         , Show (PP p a)
         , Show (PP q a)
         ) => P (MkThese p q) a where
  type PP (MkThese p q) a = These (PP p a) (PP q a)
  eval _ opts a = do
    let msg0 = "MkThese"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = These p q
        in mkNode opts (Val d) (msg0 <> " " <> showL opts d) [hh pp, hh qq]

data IsTh (th :: These x y) deriving Show
-- x y can be anything

-- trying to avoid Show instance because more likely to have ambiguity errors
instance ( x ~ These a b
         , Show a
         , Show b
         , GetThese th
         ) => P (IsTh (th :: These x1 x2)) x where
  type PP (IsTh th) x = Bool
  eval _ opts x =
    let msg0 = "Is"
        (t,f) = getThese @_ @_ @th
        b = f x
    in pure $ mkNodeB opts b (msg0 <> t <> showVerbose opts " | " x) []

-- | predicate on 'Data.These.This'
--
-- >>> pz @IsThis (This "aBc")
-- Val True
--
-- >>> pz @IsThis (These 1 'a')
-- Val False
--
-- >>> pl @IsThis (This 12)
-- True (IsThis | This 12)
-- Val True
--
data IsThis deriving Show
type IsThisT = IsTh ('This '())

instance P IsThisT x => P IsThis x where
  type PP IsThis x = PP IsThisT x
  eval _ = evalBool (Proxy @IsThisT)

-- | predicate on 'Data.These.That'
--
-- >>> pl @IsThat (This 12)
-- False (IsThat | This 12)
-- Val False
--
data IsThat deriving Show
type IsThatT = IsTh ('That '())

instance P IsThatT x => P IsThat x where
  type PP IsThat x = PP IsThatT x
  eval _ = evalBool (Proxy @IsThatT)

-- | predicate on 'Data.These.These'
--
-- >>> pl @IsThese (This 12)
-- False (IsThese | This 12)
-- Val False
--
-- >>> pz @IsThese (These 1 'a')
-- Val True
--
-- >>> pl @IsThese (These 'x' 12)
-- True (IsThese | These 'x' 12)
-- Val True
--
-- >>> pl @IsThese (That (SG.Sum 12))
-- False (IsThese | That (Sum {getSum = 12}))
-- Val False
--
-- >>> pl @IsThese (These 1 (SG.Sum 12))
-- True (IsThese | These 1 (Sum {getSum = 12}))
-- Val True
--
data IsThese deriving Show
type IsTheseT = IsTh ('These '() '())

instance P IsTheseT x => P IsThese x where
  type PP IsThese x = PP IsTheseT x
  eval _ = evalBool (Proxy @IsTheseT)

-- | similar to 'Data.Align.align' thats pads with 'Data.These.This' or 'Data.These.That' if one list is shorter than the other
--
-- the key is that all information about both lists are preserved
--
-- >>> pz @(ZipThese Fst Snd) ("aBc", [1..5])
-- Val [These 'a' 1,These 'B' 2,These 'c' 3,That 4,That 5]
--
-- >>> pz @(ZipThese Fst Snd) ("aBcDeF", [1..3])
-- Val [These 'a' 1,These 'B' 2,These 'c' 3,This 'D',This 'e',This 'F']
--
-- >>> pz @(ZipThese Id Reverse) "aBcDeF"
-- Val [These 'a' 'F',These 'B' 'e',These 'c' 'D',These 'D' 'c',These 'e' 'B',These 'F' 'a']
--
-- >>> pz @(ZipThese Id '[]) "aBcDeF"
-- Val [This 'a',This 'B',This 'c',This 'D',This 'e',This 'F']
--
-- >>> pz @(ZipThese '[] Id) "aBcDeF"
-- Val [That 'a',That 'B',That 'c',That 'D',That 'e',That 'F']
--
-- >>> pz @(ZipThese '[] '[]) "aBcDeF"
-- Val []
--
-- >>> pl @(ZipThese Fst Snd >> Map (TheseInSimple Id Id Fst)) (['w'..'y'],['a'..'f'])
-- Present "wxydef" ((>>) "wxydef" | {Map "wxydef" | [These 'w' 'a',These 'x' 'b',These 'y' 'c',That 'd',That 'e',That 'f']})
-- Val "wxydef"
--
-- >>> pl @(("sdf" &&& Id) >> ZipThese Fst Snd >> Map (TheseInSimple (Id &&& 0) (C "x" &&& Id) Id)) [1..5]
-- Present [('s',1),('d',2),('f',3),('x',4),('x',5)] ((>>) [('s',1),('d',2),('f',3),('x',4),('x',5)] | {Map [('s',1),('d',2),('f',3),('x',4),('x',5)] | [These 's' 1,These 'd' 2,These 'f' 3,That 4,That 5]})
-- Val [('s',1),('d',2),('f',3),('x',4),('x',5)]
--
data ZipThese p q deriving Show

instance ( PP p a ~ [x]
        , PP q a ~ [y]
         , P p a
         , P q a
         , Show x
         , Show y
         ) => P (ZipThese p q) a where
  type PP (ZipThese p q) a = [These (ExtractAFromList (PP p a)) (ExtractAFromList (PP q a))]
  eval _ opts a = do
    let msg0 = "ZipThese"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case chkSize2 opts msg0 p q hhs of
          Left e -> e
          Right _ ->
            let d = simpleAlign p q
            in mkNode opts (Val d) (show3' opts msg0 d "p=" p <> showVerbose opts " | q=" q) hhs

-- | extract the This value from an 'These' otherwise use the default value
--   if there is no This value then @p@ is passed the whole context only
--
-- >>> pz @(ThisDef (1 % 4) Id) (This 20.4)
-- Val (102 % 5)
--
-- >>> pz @(ThisDef (1 % 4) Id) (That "aa")
-- Val (1 % 4)
--
-- >>> pz @(ThisDef (1 % 4) Id) (These 2.3 "aa")
-- Val (1 % 4)
--
-- >>> pz @(ThisDef (PrintT "found %s fst=%d" '(ShowP Snd, Fst)) Snd) (123,That "xy")
-- Val "found That \"xy\" fst=123"
--
-- >>> pz @(ThisDef (MEmptyT _) Id) (That 222)
-- Val ()
--
-- >>> pz @(ThisDef (MEmptyT (SG.Sum _)) Id) (These 222 'x')
-- Val (Sum {getSum = 0})
--
-- >>> pl @(ThisDef (MEmptyT _) Id) (This (SG.Sum 12))
-- Present Sum {getSum = 12} (ThisDef This)
-- Val (Sum {getSum = 12})
--
-- >>> pl @(ThisDef (MEmptyT _) Id) (That 12)
-- Present () (ThisDef That)
-- Val ()
--
data ThisDef p q deriving Show

instance ( PP q x ~ These a b
         , PP p x ~ a
         , P q x
         , P p x
    ) => P (ThisDef p q) x where
  type PP (ThisDef p q) x = ThisT (PP q x)
  eval _ opts x = do
    let msg0 = "ThisDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          This a -> pure $ mkNode opts (Val a) (msg0 <> " This") [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR NoInline opts msg0 pp [hh qq] of
              Left e -> e
              Right _ -> mkNodeCopy opts pp (msg0 <> " " <> showThese q) [hh qq]


-- | extract the That value from an 'These' otherwise use the default value
--
-- if there is no That value then @p@ is passed the whole context only
--
-- >>> pz @(ThatDef (1 % 4) Id) (That 20.4)
-- Val (102 % 5)
--
-- >>> pz @(ThatDef (1 % 4) Id) (This "aa")
-- Val (1 % 4)
--
-- >>> pz @(ThatDef (1 % 4) Id) (These "aa" 2.3)
-- Val (1 % 4)
--
-- >>> pz @(ThatDef (PrintT "found %s fst=%d" '(ShowP Snd, Fst)) Snd) (123,This "xy")
-- Val "found This \"xy\" fst=123"
--
-- >>> pz @(ThatDef (MEmptyT _) Id) (This 222)
-- Val ()
--
-- >>> pz @(ThatDef (MEmptyT (SG.Sum _)) Id) (These 'x' 1120)
-- Val (Sum {getSum = 0})
--
data ThatDef p q deriving Show

instance ( PP q x ~ These a b
         , PP p x ~ b
         , P q x
         , P p x
    ) => P (ThatDef p q) x where
  type PP (ThatDef p q) x = ThatT (PP q x)
  eval _ opts x = do
    let msg0 = "ThatDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          That a -> pure $ mkNode opts (Val a) (msg0 <> " That") [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR NoInline opts msg0 pp [hh qq] of
              Left e -> e
              Right _ -> mkNodeCopy opts pp (msg0 <> " " <> showThese q) [hh qq]

-- | extract the These value from an 'These' otherwise use the default value
--
-- if there is no These value then @p@ is passed the whole context only
--
-- >>> pz @(TheseDef '(1 % 4,"zz") Id) (These 20.4 "x")
-- Val (102 % 5,"x")
--
-- >>> pz @(TheseDef '(1 % 4,"zz") Id) (This 20.4)
-- Val (1 % 4,"zz")
--
-- >>> pz @(TheseDef '(1 % 4,"zz") Id) (That "x")
-- Val (1 % 4,"zz")
--
-- >>> pz @(TheseDef '(PrintT "found %s fst=%d" '(ShowP Snd, Fst),999) Snd) (123,This "xy")
-- Val ("found This \"xy\" fst=123",999)
--
-- >>> pz @(TheseDef (MEmptyT (SG.Sum _, String)) Id) (This 222)
-- Val (Sum {getSum = 0},"")
--
-- >>> pz @(TheseDef (MEmptyT _) Id) (These (SG.Sum 222) "aa")
-- Val (Sum {getSum = 222},"aa")
--
-- >>> pl @(TheseDef '("xyz",'True) Id) (This "abc")
-- Present ("xyz",True) (TheseDef This)
-- Val ("xyz",True)
--
-- >>> pl @(TheseDef '("xyz",'True) Id) (That False)
-- Present ("xyz",True) (TheseDef That)
-- Val ("xyz",True)
--
-- >>> pl @(TheseDef '("xyz",'True) Id) (These "abc" False)
-- Present ("abc",False) (TheseDef These)
-- Val ("abc",False)
--
data TheseDef p q deriving Show

instance ( PP q x ~ These a b
         , PP p x ~ (a,b)
         , P q x
         , P p x
    ) => P (TheseDef p q) x where
  type PP (TheseDef p q) x = TheseT (PP q x)
  eval _ opts x = do
    let msg0 = "TheseDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          These a b -> pure $ mkNode opts (Val (a,b)) (msg0 <> " These") [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR NoInline opts msg0 pp [hh qq] of
              Left e -> e
              Right _ -> mkNodeCopy opts pp (msg0 <> " " <> showThese q) [hh qq]


-- | extract the This value from a 'These' otherwise fail with a message
--
-- if there is no This value then @p@ is passed the whole context only
--
-- >>> pz @(ThisFail "oops" Id) (This 20.4)
-- Val 20.4
--
-- >>> pz @(ThisFail "oops" Id) (That "aa")
-- Fail "oops"
--
-- >>> pz @(ThisFail (PrintT "found %s fst=%d" '(ShowP Snd,Fst)) Snd) (123,That "xy")
-- Fail "found That \"xy\" fst=123"
--
-- >>> pz @(ThisFail (MEmptyT _) Id) (That 222)
-- Fail ""
--
-- >>> pl @(ThisFail "sdf" Id) (This (SG.Sum 12))
-- Present Sum {getSum = 12} (This)
-- Val (Sum {getSum = 12})
--
-- >>> pl @(ThisFail "sdf" Id) (That (SG.Sum 12))
-- Error sdf (ThisFail That)
-- Fail "sdf"
--
-- >>> pl @(ThisFail "sdf" Id) (That 12)
-- Error sdf (ThisFail That)
-- Fail "sdf"
--
data ThisFail p q deriving Show

instance ( PP p x ~ String
         , PP q x ~ These a b
         , P p x
         , P q x
         )
    => P (ThisFail p q) x where
  type PP (ThisFail p q) x = ThisT (PP q x)
  eval _ opts x = do
    let msg0 = "ThisFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          This a -> pure $ mkNode opts (Val a) "This" [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR NoInline opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Fail p) (msg0 <> " " <> showThese q) [hh qq, hh pp]


-- | extract the That value from a 'These' otherwise fail with a message
--
-- if there is no That value then @p@ is passed the whole context only
--
-- >>> pz @(ThatFail "oops" Id) (That 20.4)
-- Val 20.4
--
-- >>> pz @(ThatFail "oops" Id) (This "aa")
-- Fail "oops"
--
-- >>> pz @(ThatFail (PrintT "found %s fst=%d" '(ShowP Snd,Fst)) Snd) (123,This "xy")
-- Fail "found This \"xy\" fst=123"
--
-- >>> pz @(ThatFail (MEmptyT _) Id) (This 222)
-- Fail ""
--
data ThatFail p q deriving Show

instance ( PP p x ~ String
         , PP q x ~ These a b
         , P p x
         , P q x
         )
    => P (ThatFail p q) x where
  type PP (ThatFail p q) x = ThatT (PP q x)
  eval _ opts x = do
    let msg0 = "ThatFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          That a -> pure $ mkNode opts (Val a) "That" [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR NoInline opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Fail p) (msg0 <> " " <> showThese q) [hh qq, hh pp]




-- | extract the These value from a 'These' otherwise fail with a message
--
-- if there is no These value then @p@ is passed the whole context only
--
-- >>> pz @(TheseFail "oops" Id) (These "abc" 20.4)
-- Val ("abc",20.4)
--
-- >>> pz @(TheseFail "oops" Id) (That "aa")
-- Fail "oops"
--
-- >>> pz @(TheseFail (PrintT "found %s fst=%d" '(ShowP Snd,Fst)) Snd) (123,That "xy")
-- Fail "found That \"xy\" fst=123"
--
-- >>> pz @(TheseFail (MEmptyT _) Id) (That 222)
-- Fail ""
--
data TheseFail p q deriving Show

instance ( PP p x ~ String
         , PP q x ~ These a b
         , P p x
         , P q x
         )
    => P (TheseFail p q) x where
  type PP (TheseFail p q) x = TheseT (PP q x)
  eval _ opts x = do
    let msg0 = "TheseFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          These a b -> pure $ mkNode opts (Val (a,b)) "These" [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR NoInline opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Fail p) (msg0 <> " " <> showThese q) [hh qq, hh pp]


-- | tries to extract a value from the 'Data.These.This' constructor
--
-- >>> pz @(This' >> Succ) (This 20)
-- Val 21
--
-- >>> pz @(This' >> Succ) (That 'a')
-- Fail "This' found That"
--
data This' deriving Show
instance Show a => P This' (These a x) where
  type PP This' (These a x) = a
  eval _ opts lr =
    let msg0 = "This'"
    in pure $ case lr of
         These _ _ -> mkNode opts (Fail (msg0 <> " found These")) "" []
         That _ -> mkNode opts (Fail (msg0 <> " found That")) "" []
         This a -> mkNode opts (Val a) (msg0 <> " " <> showL opts a) []

-- | tries to extract a value from the 'Data.These.That' constructor
--
-- >>> pz @(That' >> Succ) (That 20)
-- Val 21
--
-- >>> pz @(That' >> Succ) (This 'a')
-- Fail "That' found This"
--
data That' deriving Show
instance Show a => P That' (These x a) where
  type PP That' (These x a) = a
  eval _ opts lr =
    let msg0 = "That'"
    in pure $ case lr of
         These _ _ -> mkNode opts (Fail (msg0 <> " found These")) "" []
         This _ -> mkNode opts (Fail (msg0 <> " found This")) "" []
         That a -> mkNode opts (Val a) (msg0 <> " " <> showL opts a) []

-- | tries to extract the values from the 'Data.These.These' constructor
--
-- >>> pz @(These' >> Second Succ) (These 1 'a')
-- Val (1,'b')
--
-- >>> pz @(That' >> Succ) (This 'a')
-- Fail "That' found This"
--
-- >>> pz @(These' >> Second Succ) (That 8)
-- Fail "These' found That"
--
data These' deriving Show
instance ( Show a
         , Show b
        ) => P These' (These a b) where
  type PP These' (These a b) = (a,b)
  eval _ opts lr =
    let msg0 = "These'"
    in pure $ case lr of
         This _ -> mkNode opts (Fail (msg0 <> " found This")) "" []
         That _ -> mkNode opts (Fail (msg0 <> " found That")) "" []
         These a b -> mkNode opts (Val (a,b)) (msg0 <> " " <> showL opts (a,b)) []

-- | destructor for These (similar to 'Data.These.these' but with an extra environment @s@)
--   @p@ @This a@ receives @(PP t x,a)@
--   @q@ @That b@ receives @(PP t x,b)@
--   @r@ @These a b@ receives @(PP t x,(a,b))@
--   @s@ points to the environment you want to pass in
--   @t@ points to the These value
--
-- >>> pz @(TheseIn Snd Fst L21 Fst Snd) (999,This 123)
-- Val 123
--
-- >>> pz @(TheseIn Snd Fst L21 Fst Snd) (999,That 123)
-- Val 999
--
-- >>> pz @(TheseIn Snd Fst L21 Fst Snd) (999,These 9 11)
-- Val 9
--
-- >>> pz @(TheseIn '(Snd,L12) '(L11,Snd) Snd Fst Snd) ((999,'a'), These 12 'x')
-- Val (12,'x')
--
-- >>> pz @(TheseIn '(Snd,L12) '(L11,Snd) Snd Fst Snd) ((999,'a'), That 'z')
-- Val (999,'z')
--
-- >>> pz @(TheseIn Snd (Snd >> Len) (Snd >> Fst + Length Snd) () Id) (This 13)
-- Val 13
--
-- >>> pz @(TheseIn Snd (Snd >> Len) (Snd >> Fst + Length Snd) () Id) (That "abcdef")
-- Val 6
--
-- >>> pl @(TheseIn "left" "right" "both" () Id) (This (SG.Sum 12))
-- Present "left" (TheseIn "left" | This Sum {getSum = 12})
-- Val "left"
--
-- >>> pl @(TheseIn '(Snd,999) '("no value",Snd) Snd () Id) (These "Ab" 13)
-- Present ("Ab",13) (TheseIn ("Ab",13) | These "Ab" 13)
-- Val ("Ab",13)
--
-- >>> pl @(TheseIn '(Snd,999) '("no value",Snd) Snd () Id) (This "Ab")
-- Present ("Ab",999) (TheseIn ("Ab",999) | This "Ab")
-- Val ("Ab",999)
--
-- >>> pz @(TheseIn ((Fst + Snd) >> ShowP Id) Snd "Xx" Fst Snd) (9,This 123)
-- Val "132"
--
-- >>> pz @(TheseIn '(Snd,"fromthis") '(Negate 99,Snd) Snd () Id) (This 123)
-- Val (123,"fromthis")
--
-- >>> pz @(TheseIn '(Snd,"fromthis") '(Negate 99,Snd) Snd () Id) (That "fromthat")
-- Val (-99,"fromthat")
--
-- >>> pz @(TheseIn '(Snd,"fromthis") '(Negate 99,Snd) Snd () Id) (These 123 "fromthese")
-- Val (123,"fromthese")
--
-- >>> pl @(TheseIn (PrintF "a=%d" (Snd >> Succ)) ("b=" <> Snd) (PrintT "a=%d b=%s" Snd) () Id) (These @Int 9 "rhs")
-- Present "a=9 b=rhs" (TheseIn "a=9 b=rhs" | These 9 "rhs")
-- Val "a=9 b=rhs"
--
-- >>> pl @(TheseIn (PrintF "a=%d" (Snd >> Succ)) ("b=" <> Snd) (PrintT "a=%d b=%s" Snd) () Id) (This @Int 9)
-- Present "a=10" (TheseIn "a=10" | This 9)
-- Val "a=10"
--
-- >>> pl @(TheseIn (PrintF "a=%d" (Snd >> Succ)) ("b=" <> Snd) (PrintT "a=%d b=%s" Snd) () Id) (That @Int "rhs")
-- Present "b=rhs" (TheseIn "b=rhs" | That "rhs")
-- Val "b=rhs"
--
-- >>> pz @(TheseIn ((Fst + Snd) >> ShowP Id) (ShowP Id) L22 Fst Snd) (9,This 123)
-- Val "132"
--
data TheseIn p q r s t deriving Show

instance ( Show a
         , Show b
         , Show (PP r (y,(a,b)))
         , P p (y,a)
         , P q (y,b)
         , P r (y,(a,b))
         , PP p (y,a) ~ PP q (y,b)
         , PP q (y,b) ~ PP r (y,(a,b))
         , P s x
         , P t x
         , PP s x ~ y
         , PP t x ~ These a b
         )  => P (TheseIn p q r s t) x where
  type PP (TheseIn p q r s t) x = TheseInT r (PP s x) (PP t x)
  eval _ opts x = do
    let msg0 = "TheseIn"
    lr <- runPQ NoInline msg0 (Proxy @s) (Proxy @t) opts x []
    case lr of
      Left e -> pure e
      Right (s,t,ss,tt) -> do
         let hhs = [hh ss, hh tt]
         case t of
            This a -> do
              let msg1 = "This "
                  msg2 = msg0 <> msg1
              pp <- eval (Proxy @p) opts (s,a)
              pure $ case getValueLR NoInline opts (msg2 <> "p failed") pp hhs of
                   Left e -> e
                   Right c -> mkNode opts (Val c) (show3' opts msg0 c msg1 a) (hhs ++ [hh pp])
            That b -> do
              let msg1 = "That "
                  msg2 = msg0 <> msg1
              qq <- eval (Proxy @q) opts (s,b)
              pure $ case getValueLR NoInline opts (msg2 <> "q failed") qq hhs of
                   Left e -> e
                   Right c -> mkNode opts (Val c) (show3' opts msg0 c msg1 b) (hhs ++ [hh qq])
            These a b -> do
              let msg1 = "These "
                  msg2 = msg0 <> msg1
              rr <- eval (Proxy @r) opts (s,(a,b))
              pure $ case getValueLR NoInline opts (msg2 <> "r failed") rr hhs of
                   Left e -> e
                   Right c -> mkNode opts (Val c) (show3' opts msg0 c "" (These a b)) (hhs ++ [hh rr])

type family TheseInT r y elr where
  TheseInT r y (These a b) = PP r (y,(a,b))
  TheseInT _ _ o = GL.TypeError (
      'GL.Text "TheseInT: expected 'These a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | simple version of 'TheseIn' with Id as the These value and the environment set to ()
--
-- >>> pz @(TheseId '(Id,"fromleft") '(888,Id) Id) (These 222 "ok")
-- Val (222,"ok")
--
-- >>> pz @(TheseId '(Id,"fromleft") '(888,Id) Id) (That "ok")
-- Val (888,"ok")
--
-- >>> pz @(TheseId '(Id,"fromleft") '(888,Id) Id) (This 123)
-- Val (123,"fromleft")
--
data TheseId p q r deriving Show
type TheseIdT p q r = TheseIn (Snd >> p) (Snd >> q) (Snd >> r) () Id

instance P (TheseIdT p q r) x => P (TheseId p q r) x where
  type PP (TheseId p q r) x = PP (TheseIdT p q r) x
  eval _ = eval (Proxy @(TheseIdT p q r))

-- | provide the default pair in @s@ and @t@ refers to These
--
-- >>> pz @(ThesePair Fst Snd) ((999,"oops"),These 2 "xx")
-- Val (2,"xx")
--
-- >>> pz @(ThesePair Fst Snd) ((999,"oops"),That "ok")
-- Val (999,"ok")
--
data ThesePair s t deriving Show
type ThesePairT s t = TheseIn '(Snd,L12) '(L11,Snd) Snd s t

instance P (ThesePairT s t) x => P (ThesePair s t) x where
  type PP (ThesePair s t) x = PP (ThesePairT s t) x
  eval _ = eval (Proxy @(ThesePairT s t))


-- | similar to 'Data.These.these' but without any environment
--
-- >>> pz @(TheseInSimple Id Len (Fst + Length Snd)) (This 13)
-- Val 13
--
-- >>> pz @(TheseInSimple Id Len (Fst + Length Snd)) (That "this is a long string")
-- Val 21
--
-- >>> pz @(TheseInSimple Id Len (Fst + Length Snd)) (These 20 "somedata")
-- Val 28
--
-- >>> pz @(TheseInSimple (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (That "this is a long string")
-- Val (Right "this is a long string")
--
-- >>> pz @(TheseInSimple (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (These 1 "this is a long string")
-- Val (Right "this is a long string")
--
-- >>> pz @(TheseInSimple (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (These 100 "this is a long string")
-- Val (Left 100)
--
-- >>> pl @(TheseInSimple "this" "that" "these") (This (SG.Sum 12))
-- Present "this" (TheseIn "this" | This Sum {getSum = 12})
-- Val "this"
--
-- >>> pl @(TheseInSimple (Id &&& 999) ("no value" &&& Id) Id) (These "Ab" 13)
-- Present ("Ab",13) (TheseIn ("Ab",13) | These "Ab" 13)
-- Val ("Ab",13)
--
-- >>> pl @(TheseInSimple (Id &&& 999) ("no value" &&& Id) Id) (This "Ab")
-- Present ("Ab",999) (TheseIn ("Ab",999) | This "Ab")
-- Val ("Ab",999)
--
-- >>> pl @(TheseInSimple (Id &&& 999) ("no value" &&& Id) Id) (That 13)
-- Present ("no value",13) (TheseIn ("no value",13) | That 13)
-- Val ("no value",13)
--
data TheseInSimple p q r deriving Show
type TheseInSimpleT p q r = TheseIn (Snd >> p) (Snd >> q) (Snd >> r) () Id

instance P (TheseInSimpleT p q r) x => P (TheseInSimple p q r) x where
  type PP (TheseInSimple p q r) x = PP (TheseInSimpleT p q r) x
  eval _ = eval (Proxy @(TheseInSimpleT p q r))


-- | get This or use the default value @p@: @q@ is the environment and @r@ is the These value
--
-- >>> pz @(ThisDef' Id Fst Snd) (999,These 1 "xx")
-- Val 999
--
-- >>> pz @(ThisDef' 999 () Id) (That "sdf")
-- Val 999
--
-- >>> pz @(ThisDef' 999 () Id) (This 1)
-- Val 1
--
data ThisDef' p q r deriving Show

type ThisDefT' p q r = TheseIn Snd (Fst >> p) (Fst >> p) q r

instance P (ThisDefT' p q r) x => P (ThisDef' p q r) x where
  type PP (ThisDef' p q r) x = PP (ThisDefT' p q r) x
  eval _ = eval (Proxy @(ThisDefT' p q r))

-- | get That or use the default value @p@: @q@ is the environment and @r@ is the These value
--
-- >>> pz @(ThatDef' 999 () Id) (These "x" 1)
-- Val 999
--
-- >>> pz @(ThatDef' 999 () Id) (This "sdf")
-- Val 999
--
-- >>> pz @(ThatDef' 999 Fst Snd) (999,That 1)
-- Val 1
--
data ThatDef' p q r deriving Show

type ThatDefT' p q r = TheseIn (Fst >> p) Snd (Fst >> p) q r

instance P (ThatDefT' p q r) x => P (ThatDef' p q r) x where
  type PP (ThatDef' p q r) x = PP (ThatDefT' p q r) x
  eval _ = eval (Proxy @(ThatDefT' p q r))

-- | get These or use the default value @p@: @q@ is the environment and @r@ is the These value
--
-- >>> pz @(TheseDef' '(999,"xx") () Id) (These 12 "yy")
-- Val (12,"yy")
--
-- >>> pz @(TheseDef' '(999,"xx") () Id) (That "abc")
-- Val (999,"xx")
--
-- >>> pz @(TheseDef' '(999,"xx") () Id) (This 1)
-- Val (999,"xx")
--
-- >>> pz @(TheseDef' '(999,"xx") () Id) (These 1 "abc")
-- Val (1,"abc")
--
-- >>> pz @(TheseDef' Id Fst Snd) ((999,"xx"),That "yy")
-- Val (999,"xx")
--
data TheseDef' p q r deriving Show

type TheseDefT' p q r = TheseIn (Fst >> p) (Fst >> p) Snd q r

instance P (TheseDefT' p q r) x => P (TheseDef' p q r) x where
  type PP (TheseDef' p q r) x = PP (TheseDefT' p q r) x
  eval _ = eval (Proxy @(TheseDefT' p q r))

