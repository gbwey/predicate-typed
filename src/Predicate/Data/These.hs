{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# OPTIONS -Wunused-type-patterns #-}
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
{- |
     promoted 'These' functions
-}
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

 -- ** get rid of These
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
  , PartitionThese
  , TheseX

 -- ** miscellaneous
  , ZipThese
  , Assoc
  , Unassoc

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Data.Proxy (Proxy(Proxy))
import Data.Kind (Type)
import Data.These (partitionThese, These(..))
import qualified Data.These.Combinators as TheseC

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG

-- | similar to 'partitionThese'. returns a 3-tuple with the results so use 'Fst' 'Snd' 'Thd' to extract
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
data PartitionThese

instance ( Show a
         , Show b
         ) => P PartitionThese [These a b] where
  type PP PartitionThese [These a b] = ([a], [b], [(a, b)])
  eval _ opts as =
    let msg0 = "PartitionThese"
        b = partitionThese as
    in pure $ mkNode opts (Val b) (show3 opts msg0 b as) []

-- | similar to 'TheseC.catThis'
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
data Thiss
type ThissT = PartitionThese >> Fst

instance P ThissT x => P Thiss x where
  type PP Thiss x = PP ThissT x
  eval _ = eval (Proxy @ThissT)

-- | similar to 'TheseC.catThat'
--
-- >>> pl @Thats [This 1, This 10,That 'x', This 99, That 'y']
-- Present "xy" ((>>) "xy" | {Snd "xy" | ([1,10,99],"xy",[])})
-- Val "xy"
--
data Thats
type ThatsT = PartitionThese >> Snd

instance P ThatsT x => P Thats x where
  type PP Thats x = PP ThatsT x
  eval _ = eval (Proxy @ThatsT)

-- | similar to 'TheseC.catThese'
--
-- >>> pz @(ZipThese Id Tail >> Theses) [1..10]
-- Val [(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10)]
--
data Theses
type ThesesT = PartitionThese >> Thd

instance P ThesesT x => P Theses x where
  type PP Theses x = PP ThesesT x
  eval _ = eval (Proxy @ThesesT)

-- | similar to 'TheseC.catHere'
--
-- >>> pz @(ZipThese Id Tail >> Heres) [1..10]
-- Val [1,2,3,4,5,6,7,8,9,10]
--
data Heres

instance ( Show a
         , Show b
         ) => P Heres [These a b] where
  type PP Heres [These a b] = [a]
  eval _ opts as =
    let msg0 = "Heres"
        b = TheseC.catHere as
    in pure $ mkNode opts (Val b) (show3 opts msg0 b as) []

-- | similar to 'TheseC.catThere'
--
-- >>> pz @(ZipThese Id Tail >> Theres) [1..10]
-- Val [2,3,4,5,6,7,8,9,10]
--
data Theres

instance ( Show a
         , Show b
         ) => P Theres [These a b] where
  type PP Theres [These a b] = [b]
  eval _ opts as =
    let msg0 = "Theres"
        b = TheseC.catThere as
    in pure $ mkNode opts (Val b) (show3 opts msg0 b as) []

-- | similar to 'Data.These.mergeTheseWith' but additionally provides @p@, @q@ and @r@ the original input as the first element in the tuple
--
-- >>> pz @(TheseX ((L11 + Snd) >> ShowP Id) (ShowP Id) L22 Snd) (9,This 123)
-- Val "132"
--
-- >>> pz @(TheseX '(Snd,"fromthis") '(Negate 99,Snd) Snd Id) (This 123)
-- Val (123,"fromthis")
--
-- >>> pz @(TheseX '(Snd,"fromthis") '(Negate 99,Snd) Snd Id) (That "fromthat")
-- Val (-99,"fromthat")
--
-- >>> pz @(TheseX '(Snd,"fromthis") '(Negate 99,Snd) Snd Id) (These 123 "fromthese")
-- Val (123,"fromthese")
--
-- >>> pl @(TheseX (PrintF "a=%d" (Snd >> Succ)) ("b=" <> Snd) (PrintT "a=%d b=%s" Snd) Id) (These @Int 9 "rhs")
-- Present "a=9 b=rhs" (TheseX(These))
-- Val "a=9 b=rhs"
--
-- >>> pl @(TheseX (PrintF "a=%d" (Snd >> Succ)) ("b=" <> Snd) (PrintT "a=%d b=%s" Snd) Id) (This @Int 9)
-- Present "a=10" (TheseX(This))
-- Val "a=10"
--
-- >>> pl @(TheseX (PrintF "a=%d" (Snd >> Succ)) ("b=" <> Snd) (PrintT "a=%d b=%s" Snd) Id) (That @Int "rhs")
-- Present "b=rhs" (TheseX(That))
-- Val "b=rhs"
--
data TheseX p q r s

instance ( P s x
         , P p (x,a)
         , P q (x,b)
         , P r (x,(a,b))
         , PP s x ~ These a b
         , PP p (x,a) ~ c
         , PP q (x,b) ~ c
         , PP r (x,(a,b)) ~ c
         ) => P (TheseX p q r s) x where
  type PP (TheseX p q r s) x = TheseXT (PP s x) x p
  eval _ opts x = do
    let msg0 = "TheseX"
    ss <- eval (Proxy @s) opts x
    case getValueLR opts msg0 ss [] of
      Left e -> pure e
      Right (This a) -> do
        let msg1 = msg0 <> "(This)"
        pp <- eval (Proxy @p) opts (x,a)
        pure $ case getValueLR opts msg1 pp [hh ss] of
          Left e -> e
          Right _ -> mkNodeCopy opts pp msg1 [hh ss, hh pp]
      Right (That b) -> do
        let msg1 = msg0 <> "(That)"
        qq <- eval (Proxy @q) opts (x,b)
        pure $ case getValueLR opts msg1 qq [hh ss] of
          Left e -> e
          Right _ -> mkNodeCopy opts qq msg1 [hh ss, hh qq]
      Right (These a b) -> do
        let msg1 = msg0 <> "(These)"
        rr <- eval (Proxy @r) opts (x,(a,b))
        pure $ case getValueLR opts msg1 rr [hh ss] of
          Left e -> e
          Right _ -> mkNodeCopy opts rr msg1 [hh ss, hh rr]

type family TheseXT lr x p where
  TheseXT (These a _b) x p = PP p (x,a)

-- | 'Data.These.This' constructor
--
-- >>> pz @(MkThis _ Id) 44
-- Val (This 44)
--
-- >>> pz @(Proxy Int >> MkThis' Unproxy 10) []
-- Val (This 10)
--
data MkThis' t p

instance ( P p x
         , Show (PP p x)
         ) => P (MkThis' t p) x where
  type PP (MkThis' t p) x = These (PP p x) (PP t x)
  eval _ opts x = do
    let msg0 = "MkThis"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
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

data MkThis (t :: Type) p
type MkThisT (t :: Type) p = MkThis' (Hole t) p

instance P (MkThisT t p) x => P (MkThis t p) x where
  type PP (MkThis t p) x = PP (MkThisT t p) x
  eval _ = eval (Proxy @(MkThisT t p))

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
data MkThat' t p

instance ( Show (PP p x)
         , P p x
         ) => P (MkThat' t p) x where
  type PP (MkThat' t p) x = These (PP t x) (PP p x)
  eval _ opts x = do
    let msg0 = "MkThat"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = That p
        in mkNode opts (Val d) (msg0 <> " That " <> showL opts p) [hh pp]

data MkThat (t :: Type) p
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
data MkThese p q
instance ( P p a
         , P q a
         , Show (PP p a)
         , Show (PP q a)
         ) => P (MkThese p q) a where
  type PP (MkThese p q) a = These (PP p a) (PP q a)
  eval _ opts a = do
    let msg0 = "MkThese"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = These p q
        in mkNode opts (Val d) (msg0 <> " " <> showL opts d) [hh pp, hh qq]

data IsTh (th :: These x y) -- x y can be anything

-- trying to avoid Show instance cos of ambiguities
instance ( x ~ These a b
         , Show a
         , Show b
         , GetThese th
         ) => P (IsTh (th :: These x1 x2)) x where
  type PP (IsTh th) x = Bool
  eval _ opts x =
    let msg0 = "Is"
        (t,f) = getThese @th
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
data IsThis
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
data IsThat
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
data IsThese
type IsTheseT = IsTh ('These '() '())

instance P IsTheseT x => P IsThese x where
  type PP IsThese x = PP IsTheseT x
  eval _ = evalBool (Proxy @IsTheseT)

-- | similar to 'Data.These.these'
--
-- >>> pz @(TheseIn Id Len (Fst + Length Snd)) (This 13)
-- Val 13
--
-- >>> pz @(TheseIn Id Len (Fst + Length Snd)) (That "this is a long string")
-- Val 21
--
-- >>> pz @(TheseIn Id Len (Fst + Length Snd)) (These 20 "somedata")
-- Val 28
--
-- >>> pz @(TheseIn (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (That "this is a long string")
-- Val (Right "this is a long string")
--
-- >>> pz @(TheseIn (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (These 1 "this is a long string")
-- Val (Right "this is a long string")
--
-- >>> pz @(TheseIn (MkLeft _ Id) (MkRight _ Id) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (These 100 "this is a long string")
-- Val (Left 100)
--
-- >>> pl @(TheseIn "this" "that" "these") (This (SG.Sum 12))
-- Present "this" (TheseIn "this" | This Sum {getSum = 12})
-- Val "this"
--
-- >>> pl @(TheseIn (Id &&& 999) ("no value" &&& Id) Id) (These "Ab" 13)
-- Present ("Ab",13) (TheseIn ("Ab",13) | These "Ab" 13)
-- Val ("Ab",13)
--
-- >>> pl @(TheseIn (Id &&& 999) ("no value" &&& Id) Id) (This "Ab")
-- Present ("Ab",999) (TheseIn ("Ab",999) | This "Ab")
-- Val ("Ab",999)
--
-- >>> pl @(TheseIn (Id &&& 999) ("no value" &&& Id) Id) (That 13)
-- Present ("no value",13) (TheseIn ("no value",13) | That 13)
-- Val ("no value",13)
--

data TheseIn p q r

instance ( Show a
         , Show b
         , Show (PP p a)
         , P p a
         , P q b
         , P r (a,b)
         , PP p a ~ PP q b
         , PP p a ~ PP r (a,b)
         , PP q b ~ PP r (a,b)
         )  => P (TheseIn p q r) (These a b) where
  type PP (TheseIn p q r) (These a b) = PP p a
  eval _ opts th = do
     let msg0 = "TheseIn"
     case th of
        This a -> do
          let msg1 = "This "
              msg2 = msg0 <> msg1
          pp <- eval (Proxy @p) opts a
          pure $ case getValueLR opts (msg2 <> "p failed") pp [] of
               Left e -> e
               Right c -> mkNode opts (Val c) (show3' opts msg0 c msg1 a) [hh pp]
        That b -> do
          let msg1 = "That "
              msg2 = msg0 <> msg1
          qq <- eval (Proxy @q) opts b
          pure $ case getValueLR opts (msg2 <> "q failed") qq [] of
               Left e -> e
               Right c -> mkNode opts (Val c) (show3' opts msg0 c msg1 b) [hh qq]
        These a b -> do
          let msg1 = "These "
              msg2 = msg0 <> msg1
          rr <- eval (Proxy @r) opts (a,b)
          pure $ case getValueLR opts (msg2 <> "r failed") rr [] of
               Left e -> e
               Right c -> mkNode opts (Val c) (show3 opts msg0 c (These a b)) [hh rr]

-- | TheseId: given a 'These' returns a tuple but you need to provide default values for both sides
--
-- >>> pl @(TheseId "xyz" 'True ) (This "abc")
-- Present ("abc",True) (TheseIn ("abc",True) | This "abc")
-- Val ("abc",True)
--
-- >>> pl @(TheseId "xyz" 'True) (That False)
-- Present ("xyz",False) (TheseIn ("xyz",False) | That False)
-- Val ("xyz",False)
--
-- >>> pl @(TheseId "xyz" 'True) (These "abc" False)
-- Present ("abc",False) (TheseIn ("abc",False) | These "abc" False)
-- Val ("abc",False)
--
data TheseId p q
type TheseIdT p q = TheseIn '(Id, q) '(p, Id) Id

instance P (TheseIdT p q) x => P (TheseId p q) x where
  type PP (TheseId p q) x = PP (TheseIdT p q) x
  eval _ = eval (Proxy @(TheseIdT p q))

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
-- >>> pl @(ZipThese Fst Snd >> Map (TheseIn Id Id Fst)) (['w'..'y'],['a'..'f'])
-- Present "wxydef" ((>>) "wxydef" | {Map "wxydef" | [These 'w' 'a',These 'x' 'b',These 'y' 'c',That 'd',That 'e',That 'f']})
-- Val "wxydef"
--
-- >>> pl @(("sdf" &&& Id) >> ZipThese Fst Snd >> Map (TheseIn (Id &&& 0) (("x" >> Head) &&& Id) Id)) [1..5]
-- Present [('s',1),('d',2),('f',3),('x',4),('x',5)] ((>>) [('s',1),('d',2),('f',3),('x',4),('x',5)] | {Map [('s',1),('d',2),('f',3),('x',4),('x',5)] | [These 's' 1,These 'd' 2,These 'f' 3,That 4,That 5]})
-- Val [('s',1),('d',2),('f',3),('x',4),('x',5)]
--

data ZipThese p q

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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case chkSize2 opts msg0 p q hhs of
          Left e -> e
          Right _ ->
            let d = simpleAlign p q
            in mkNode opts (Val d) (show3' opts msg0 d "p=" p <> showVerbose opts " | q=" q) hhs


simpleAlign :: [a] -> [b] -> [These a b]
simpleAlign as [] = map This as
simpleAlign [] bs = map That bs
simpleAlign (a:as) (b:bs) = These a b : simpleAlign as bs


-- | extract the This value from an 'These' otherwise use the default value
--
-- if there is no This value then \p\ is passed the whole context only
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

data ThisDef p q

instance ( PP q x ~ These a b
         , PP p x ~ a
         , P q x
         , P p x
    ) => P (ThisDef p q) x where
  type PP (ThisDef p q) x = ThisT (PP q x)
  eval _ opts x = do
    let msg0 = "ThisDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          This a -> pure $ mkNode opts (Val a) (msg0 <> " This") [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Val p) (msg0 <> " " <> showThese q) [hh qq, hh pp]


-- | extract the That value from an 'These' otherwise use the default value
--
-- if there is no That value then \p\ is passed the whole context only
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
data ThatDef p q

instance ( PP q x ~ These a b
         , PP p x ~ b
         , P q x
         , P p x
    ) => P (ThatDef p q) x where
  type PP (ThatDef p q) x = ThatT (PP q x)
  eval _ opts x = do
    let msg0 = "ThatDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          That a -> pure $ mkNode opts (Val a) (msg0 <> " That") [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Val p) (msg0 <> " " <> showThese q) [hh qq, hh pp]

-- | extract the These value from an 'These' otherwise use the default value
--
-- if there is no These value then \p\ is passed the whole context only
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
-- >>> pz @(TheseDef (MEmptyT _) Id) (These (222 :: SG.Sum Int) "aa")
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
data TheseDef p q

instance ( PP q x ~ These a b
         , PP p x ~ (a,b)
         , P q x
         , P p x
    ) => P (TheseDef p q) x where
  type PP (TheseDef p q) x = TheseT (PP q x)
  eval _ opts x = do
    let msg0 = "TheseDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          These a b -> pure $ mkNode opts (Val (a,b)) (msg0 <> " These") [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Val p) (msg0 <> " " <> showThese q) [hh qq, hh pp]


-- | extract the This value from a 'These' otherwise fail with a message
--
-- if there is no This value then \p\ is passed the whole context only
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
data ThisFail p q

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
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          This a -> pure $ mkNode opts (Val a) "This" [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Fail p) (msg0 <> " " <> showThese q) [hh qq, hh pp]


-- | extract the That value from a 'These' otherwise fail with a message
--
-- if there is no That value then \p\ is passed the whole context only
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
data ThatFail p q

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
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          That a -> pure $ mkNode opts (Val a) "That" [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Fail p) (msg0 <> " " <> showThese q) [hh qq, hh pp]




-- | extract the These value from a 'These' otherwise fail with a message
--
-- if there is no These value then \p\ is passed the whole context only
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
data TheseFail p q

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
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          These a b -> pure $ mkNode opts (Val (a,b)) "These" [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Fail p) (msg0 <> " " <> showThese q) [hh qq, hh pp]


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
data Assoc

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
  assoc (That b) = That (That b)
  assoc (These (This a) c) = These a (That c)
  assoc (These (That b) c) = That (These b c)
  assoc (These (These a b) c) = These a (These b c)
  assoc (This (These a b)) = These a (This b)
  unassoc (This a) = This (This a)
  unassoc (That (This b)) = This (That b)
  unassoc (That (That b)) = That b
  unassoc (These a (That c)) = These (This a) c
  unassoc (That (These b c)) = These (That b) c
  unassoc (These a (These b c)) = These (These a b) c
  unassoc (These a (This b)) = This (These a b)

instance AssocC (,) where
  assoc ((a,b),c) = (a,(b,c))
  unassoc (a,(b,c)) = ((a,b),c)

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
data Unassoc

instance ( AssocC p
         , Show (p (p a b) c)
         , Show (p a (p b c))
         ) => P Unassoc (p a (p b c)) where
  type PP Unassoc (p a (p b c)) = p (p a b) c
  eval _ opts pabc =
    let msg0 = "Unassoc"
        d = unassoc pabc
    in pure $ mkNode opts (Val d) (show3 opts msg0 d pabc) []


-- | tries to extract a value from the 'Data.These.This' constructor
--
-- >>> pz @(This' >> Succ) (This 20)
-- Val 21
--
-- >>> pz @(This' >> Succ) (That 'a')
-- Fail "This' found That"
--
data This'
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
data That'
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
data These'
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


