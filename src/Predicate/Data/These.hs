{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-compat #-}
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
     promoted 'These' functions
-}
module Predicate.Data.These (
 -- ** these expressions
    PartitionThese
  , Thiss
  , Thats
  , Theses
  , Theres
  , Heres
  , IsThis
  , IsThat
  , IsThese
  , MkThis
  , MkThis'
  , MkThat
  , MkThat'
  , MkThese
  , ThisDef
  , ThisFail
  , ThatDef
  , ThatFail
  , TheseDef
  , TheseFail
  , TheseIn
  , TheseId
  , TheseX
  , ZipThese

 ) where
import Predicate.Core
import Predicate.Util
import Data.Proxy
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
-- PresentT ("ac",[2,4,99],[('z',1),('a',2)])
--
-- >>> pl @PartitionThese [This 4, That 'x', That 'y',These 3 'b', This 99, These 5 'x']
-- Present ([4,99],"xy",[(3,'b'),(5,'x')]) (PartitionThese ([4,99],"xy",[(3,'b'),(5,'x')]) | [This 4,That 'x',That 'y',These 3 'b',This 99,These 5 'x'])
-- PresentT ([4,99],"xy",[(3,'b'),(5,'x')])
--
-- >>> pl @PartitionThese [This 1,That 'x',This 4,That 'y',These 9 'z',This 10,These 8 'y']
-- Present ([1,4,10],"xy",[(9,'z'),(8,'y')]) (PartitionThese ([1,4,10],"xy",[(9,'z'),(8,'y')]) | [This 1,That 'x',This 4,That 'y',These 9 'z',This 10,These 8 'y'])
-- PresentT ([1,4,10],"xy",[(9,'z'),(8,'y')])
--
data PartitionThese

instance ( Show a
         , Show b
         ) => P PartitionThese [These a b] where
  type PP PartitionThese [These a b] = ([a], [b], [(a, b)])
  eval _ opts as =
    let msg0 = "PartitionThese"
        b = partitionThese as
    in pure $ mkNode opts (PresentT b) (show01 opts msg0 b as) []

-- | similar to 'TheseC.catThis'
--
-- >>> pz @(Thiss) [That 1, This 'a', These 'b' 33, This 'd', That 4]
-- PresentT "ad"
--
-- >>> pz @(Thiss) [That 1, This 'a', These 'b' 33]
-- PresentT "a"
--
-- >>> pz @(Thiss) [That 1, That 9, These 1 33]
-- PresentT []
--
data Thiss
type ThissT = Fst PartitionThese

instance P ThissT x => P Thiss x where
  type PP Thiss x = PP ThissT x
  eval _ = eval (Proxy @ThissT)

-- | similar to 'TheseC.catThat'
--
-- >>> pl @Thats [This 1, This 10,That 'x', This 99, That 'y']
-- Present "xy" (Snd "xy" | ([1,10,99],"xy",[]))
-- PresentT "xy"
--
data Thats
type ThatsT = Snd PartitionThese

instance P ThatsT x => P Thats x where
  type PP Thats x = PP ThatsT x
  eval _ = eval (Proxy @ThatsT)

-- | similar to 'TheseC.catThese'
--
-- >>> pz @(ZipThese Id (Tail Id) >> Theses) [1..10]
-- PresentT [(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10)]
--
data Theses
type ThesesT = Thd PartitionThese

instance P ThesesT x => P Theses x where
  type PP Theses x = PP ThesesT x
  eval _ = eval (Proxy @ThesesT)

-- | similar to 'TheseC.catHere'
--
-- >>> pz @(ZipThese Id (Tail Id) >> Heres) [1..10]
-- PresentT [1,2,3,4,5,6,7,8,9,10]
--
data Heres

instance ( Show a
         , Show b
         ) => P Heres [These a b] where
  type PP Heres [These a b] = [a]
  eval _ opts as =
    let msg0 = "Heres"
        b = TheseC.catHere as
    in pure $ mkNode opts (PresentT b) (show01 opts msg0 b as) []

-- | similar to 'TheseC.catThere'
--
-- >>> pz @(ZipThese Id (Tail Id) >> Theres) [1..10]
-- PresentT [2,3,4,5,6,7,8,9,10]
--
data Theres

instance ( Show a
         , Show b
         ) => P Theres [These a b] where
  type PP Theres [These a b] = [b]
  eval _ opts as =
    let msg0 = "Theres"
        b = TheseC.catThere as
    in pure $ mkNode opts (PresentT b) (show01 opts msg0 b as) []

-- | similar to 'Data.These.mergeTheseWith' but additionally provides \'p\', '\q'\ and \'r\' the original input as the first element in the tuple
--
-- >>> pz @(TheseX ((Fst (Fst Id) + Snd Id) >> ShowP Id) (ShowP Id) (Snd (Snd Id)) (Snd Id)) (9,This 123)
-- PresentT "132"
--
-- >>> pz @(TheseX '(Snd Id,"fromthis") '(Negate 99,Snd Id) (Snd Id) Id) (This 123)
-- PresentT (123,"fromthis")
--
-- >>> pz @(TheseX '(Snd Id,"fromthis") '(Negate 99,Snd Id) (Snd Id) Id) (That "fromthat")
-- PresentT (-99,"fromthat")
--
-- >>> pz @(TheseX '(Snd Id,"fromthis") '(Negate 99,Snd Id) (Snd Id) Id) (These 123 "fromthese")
-- PresentT (123,"fromthese")
--
-- >>> pl @(TheseX (PrintF "a=%d" (Succ (Snd Id))) ("b=" <> Snd Id) (PrintT "a=%d b=%s" (Snd Id)) Id) (These @Int 9 "rhs")
-- Present "a=9 b=rhs" (TheseX(These))
-- PresentT "a=9 b=rhs"
--
-- >>> pl @(TheseX (PrintF "a=%d" (Succ (Snd Id))) ("b=" <> Snd Id) (PrintT "a=%d b=%s" (Snd Id)) Id) (This @Int 9)
-- Present "a=10" (TheseX(This))
-- PresentT "a=10"
--
-- >>> pl @(TheseX (PrintF "a=%d" (Succ (Snd Id))) ("b=" <> Snd Id) (PrintT "a=%d b=%s" (Snd Id)) Id) (That @Int "rhs")
-- Present "b=rhs" (TheseX(That))
-- PresentT "b=rhs"
--
data TheseX p q r s

instance (P s x
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
          Right _ -> mkNode opts (_tBool pp) msg1 [hh ss, hh pp]
      Right (That b) -> do
        let msg1 = msg0 <> "(That)"
        qq <- eval (Proxy @q) opts (x,b)
        pure $ case getValueLR opts msg1 qq [hh ss] of
          Left e -> e
          Right _ -> mkNode opts (_tBool qq) msg1 [hh ss, hh qq]
      Right (These a b) -> do
        let msg1 = msg0 <> "(These)"
        rr <- eval (Proxy @r) opts (x,(a,b))
        pure $ case getValueLR opts msg1 rr [hh ss] of
          Left e -> e
          Right _ -> mkNode opts (_tBool rr) msg1 [hh ss, hh rr]

type family TheseXT lr x p where
  TheseXT (These a b) x p = PP p (x,a)

-- | 'Data.These.This' constructor
--
-- >>> pz @(MkThis _ Id) 44
-- PresentT (This 44)
--
-- >>> pz @(Proxy Int >> MkThis' Unproxy 10) []
-- PresentT (This 10)
--
data MkThis' t p

instance ( Show (PP p x)
         , P p x
         ) => P (MkThis' t p) x where
  type PP (MkThis' t p) x = These (PP p x) (PP t x)
  eval _ opts x = do
    let msg0 = "MkThis"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = This p
        in mkNode opts (PresentT d) (msg0 <> " This " <> showL opts p) [hh pp]

-- | MkThis
--
-- >>> pl @(MkThis () Id) 'x'
-- Present This 'x' (MkThis This 'x')
-- PresentT (This 'x')
--
-- >>> pl @(MkThis () (Fst Id)) ('x',True)
-- Present This 'x' (MkThis This 'x')
-- PresentT (This 'x')
--

data MkThis (t :: Type) p
type MkThisT (t :: Type) p = MkThis' (Hole t) p

instance P (MkThisT t p) x => P (MkThis t p) x where
  type PP (MkThis t p) x = PP (MkThisT t p) x
  eval _ = eval (Proxy @(MkThisT t p))

-- | 'Data.These.That' constructor
--
-- >>> pz @(MkThat _ Id) 44
-- PresentT (That 44)
--
-- >>> pz @(MkThat _ "Abc" <> MkThis _ '[1,2] <> MkThese [3,4] "def") ()
-- PresentT (These [1,2,3,4] "Abcdef")
--
-- >>> pl @(MkThat () Id) 'x'
-- Present That 'x' (MkThat That 'x')
-- PresentT (That 'x')
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
        in mkNode opts (PresentT d) (msg0 <> " That " <> showL opts p) [hh pp]

data MkThat (t :: Type) p
type MkThatT (t :: Type) p = MkThat' (Hole t) p

instance P (MkThatT t p) x => P (MkThat t p) x where
  type PP (MkThat t p) x = PP (MkThatT t p) x
  eval _ = eval (Proxy @(MkThatT t p))

-- type MkThat t p = MkThis t p >> Swap
-- type MkThat' (t :: Type) = Pure (These t) Id -- t has to be a semigroup

-- | 'Data.These.These' constructor
--
-- >>> pz @(MkThese (Fst Id) (Snd Id)) (44,'x')
-- PresentT (These 44 'x')
--
-- >>> pl @(MkThese Id 'True) 'x'
-- Present These 'x' True (MkThese These 'x' True)
-- PresentT (These 'x' True)
--
data MkThese p q
instance (P p a
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
        in mkNode opts (PresentT d) (msg0 <> " " <> showL opts d) [hh pp, hh qq]

-- | predicate on 'These'
--
-- >>> pz @(IsThis Id) (This "aBc")
-- TrueT
--
-- >>> pz @(IsThis Id) (These 1 'a')
-- FalseT
--
-- >>> pz @(IsThese Id) (These 1 'a')
-- TrueT
--
-- >>> pl @(IsThat Id) (This 12)
-- False (IsThat | This 12)
-- FalseT
--
-- >>> pl @(IsThis Id) (This 12)
-- True (IsThis | This 12)
-- TrueT
--
-- >>> pl @(IsThese Id) (This 12)
-- False (IsThese | This 12)
-- FalseT
--
-- >>> pl @(IsThese Id) (These 'x' 12)
-- True (IsThese | These 'x' 12)
-- TrueT
--
-- >>> pl @(IsThese Id) (That (SG.Sum 12))
-- False (IsThese | That (Sum {getSum = 12}))
-- FalseT
--
-- >>> pl @(IsThese Id) (These 1 (SG.Sum 12))
-- True (IsThese | These 1 (Sum {getSum = 12}))
-- TrueT
--

data IsTh (th :: These x y) p -- x y can be anything

-- trying to avoid show instance cos of ambiguities
instance (PP p x ~ These a b
        , P p x
        , Show a
        , Show b
        , GetThese th
        ) => P (IsTh (th :: These x1 x2) p) x where
  type PP (IsTh th p) x = Bool
  eval _ opts x = do
    let msg0 = "Is"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let (t,f) = getThese @th
            b = f p
        in mkNodeB opts b (msg0 <> t <> showVerbose opts " | " p) [hh pp]

data IsThis p
type IsThisT p = IsTh ('This '()) p

instance P (IsThisT p) x => P (IsThis p) x where
  type PP (IsThis p) x = PP (IsThisT p) x
  eval _ = evalBool (Proxy @(IsThisT p))

data IsThat p
type IsThatT p = IsTh ('That '()) p

instance P (IsThatT p) x => P (IsThat p) x where
  type PP (IsThat p) x = PP (IsThatT p) x
  eval _ = evalBool (Proxy @(IsThatT p))

data IsThese p
type IsTheseT p = IsTh ('These '() '()) p

instance P (IsTheseT p) x => P (IsThese p) x where
  type PP (IsThese p) x = PP (IsTheseT p) x
  eval _ = evalBool (Proxy @(IsTheseT p))

-- | similar to 'Data.These.these'
--
-- >>> pz @(TheseIn Id Len (Fst Id + Length (Snd Id))) (This 13)
-- PresentT 13
--
-- >>> pz @(TheseIn Id Len (Fst Id + Length (Snd Id))) (That "this is a long string")
-- PresentT 21
--
-- >>> pz @(TheseIn Id Len (Fst Id + Length (Snd Id))) (These 20 "somedata")
-- PresentT 28
--
-- >>> pz @(TheseIn (MkLeft _ Id) (MkRight _ Id) (If (Fst Id > Length (Snd Id)) (MkLeft _ (Fst Id)) (MkRight _ (Snd Id)))) (That "this is a long string")
-- PresentT (Right "this is a long string")
--
-- >>> pz @(TheseIn (MkLeft _ Id) (MkRight _ Id) (If (Fst Id > Length (Snd Id)) (MkLeft _ (Fst Id)) (MkRight _ (Snd Id)))) (These 1 "this is a long string")
-- PresentT (Right "this is a long string")
--
-- >>> pz @(TheseIn (MkLeft _ Id) (MkRight _ Id) (If (Fst Id > Length (Snd Id)) (MkLeft _ (Fst Id)) (MkRight _ (Snd Id)))) (These 100 "this is a long string")
-- PresentT (Left 100)
--
-- >>> pl @(TheseIn "this" "that" "these") (This (SG.Sum 12))
-- Present "this" (TheseIn "this" | This Sum {getSum = 12})
-- PresentT "this"
--
-- >>> pl @(TheseIn (Id &&& 999) ("no value" &&& Id) Id) (These "Ab" 13)
-- Present ("Ab",13) (TheseIn ("Ab",13) | These "Ab" 13)
-- PresentT ("Ab",13)
--
-- >>> pl @(TheseIn (Id &&& 999) ("no value" &&& Id) Id) (This "Ab")
-- Present ("Ab",999) (TheseIn ("Ab",999) | This "Ab")
-- PresentT ("Ab",999)
--
-- >>> pl @(TheseIn (Id &&& 999) ("no value" &&& Id) Id) (That 13)
-- Present ("no value",13) (TheseIn ("no value",13) | That 13)
-- PresentT ("no value",13)
--

data TheseIn p q r

instance (Show a
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
               Right c -> mkNode opts (PresentT c) (show01' opts msg0 c msg1 a) [hh pp]
        That b -> do
          let msg1 = "That "
              msg2 = msg0 <> msg1
          qq <- eval (Proxy @q) opts b
          pure $ case getValueLR opts (msg2 <> "q failed") qq [] of
               Left e -> e
               Right c -> mkNode opts (PresentT c) (show01' opts msg0 c msg1 b) [hh qq]
        These a b -> do
          let msg1 = "These "
              msg2 = msg0 <> msg1
          rr <- eval (Proxy @r) opts (a,b)
          pure $ case getValueLR opts (msg2 <> "r failed") rr [] of
               Left e -> e
               Right c -> mkNode opts (PresentT c) (show01 opts msg0 c (These a b)) [hh rr]

-- | TheseId
--
-- >>> pl @(TheseId 'True "xyz") (This "abc")
-- Present ("abc",True) (TheseIn ("abc",True) | This "abc")
-- PresentT ("abc",True)
--
-- >>> pl @(TheseId 'True "xyz") (That False)
-- Present ("xyz",False) (TheseIn ("xyz",False) | That False)
-- PresentT ("xyz",False)
--
-- >>> pl @(TheseId 'True "xyz") (These "abc" False)
-- Present ("abc",False) (TheseIn ("abc",False) | These "abc" False)
-- PresentT ("abc",False)
--
data TheseId p q
type TheseIdT p q = TheseIn '(I, p) '(q, I) I

instance P (TheseIdT p q) x => P (TheseId p q) x where
  type PP (TheseId p q) x = PP (TheseIdT p q) x
  eval _ = eval (Proxy @(TheseIdT p q))

-- | similar to 'Data.Align.align' thats pads with 'Data.These.This' or 'Data.These.That' if one list is shorter than the other
--
-- the key is that all information about both lists are preserved
--
-- >>> pz @(ZipThese (Fst Id) (Snd Id)) ("aBc", [1..5])
-- PresentT [These 'a' 1,These 'B' 2,These 'c' 3,That 4,That 5]
--
-- >>> pz @(ZipThese (Fst Id) (Snd Id)) ("aBcDeF", [1..3])
-- PresentT [These 'a' 1,These 'B' 2,These 'c' 3,This 'D',This 'e',This 'F']
--
-- >>> pz @(ZipThese Id Reverse) "aBcDeF"
-- PresentT [These 'a' 'F',These 'B' 'e',These 'c' 'D',These 'D' 'c',These 'e' 'B',These 'F' 'a']
--
-- >>> pz @(ZipThese Id '[]) "aBcDeF"
-- PresentT [This 'a',This 'B',This 'c',This 'D',This 'e',This 'F']
--
-- >>> pz @(ZipThese '[] Id) "aBcDeF"
-- PresentT [That 'a',That 'B',That 'c',That 'D',That 'e',That 'F']
--
-- >>> pz @(ZipThese '[] '[]) "aBcDeF"
-- PresentT []
--
-- >>> pl @(ZipThese (Fst Id) (Snd Id) >> Map (TheseIn Id Id (Fst Id)) Id) (['w'..'y'],['a'..'f'])
-- Present "wxydef" ((>>) "wxydef" | {Map "wxydef" | [These 'w' 'a',These 'x' 'b',These 'y' 'c',That 'd',That 'e',That 'f']})
-- PresentT "wxydef"
--
-- >>> pl @(("sdf" &&& Id) >> ZipThese (Fst Id) (Snd Id) >> Map (TheseIn (Id &&& 0) (Head "x" &&& Id) Id) Id) [1..5]
-- Present [('s',1),('d',2),('f',3),('x',4),('x',5)] ((>>) [('s',1),('d',2),('f',3),('x',4),('x',5)] | {Map [('s',1),('d',2),('f',3),('x',4),('x',5)] | [These 's' 1,These 'd' 2,These 'f' 3,That 4,That 5]})
-- PresentT [('s',1),('d',2),('f',3),('x',4),('x',5)]
--

data ZipThese p q

instance (PP p a ~ [x]
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
        in case chkSize opts msg0 p hhs <* chkSize opts msg0 q hhs of
          Left e -> e
          Right () ->
            let d = simpleAlign p q
            in mkNode opts (PresentT d) (show01' opts msg0 d "p=" p <> showVerbose opts " | q=" q) hhs


simpleAlign :: [a] -> [b] -> [These a b]
simpleAlign as [] = map This as
simpleAlign [] bs = map That bs
simpleAlign (a:as) (b:bs) = These a b : simpleAlign as bs


-- | extract the This value from an 'These' otherwise use the default value
--
-- if there is no This value then \p\ is passed the whole context only
--
-- >>> pz @(ThisDef (1 % 4) Id) (This 20.4)
-- PresentT (102 % 5)
--
-- >>> pz @(ThisDef (1 % 4) Id) (That "aa")
-- PresentT (1 % 4)
--
-- >>> pz @(ThisDef (1 % 4) Id) (These 2.3 "aa")
-- PresentT (1 % 4)
--
-- >>> pz @(ThisDef (PrintT "found %s fst=%d" '(ShowP (Snd Id), Fst Id)) (Snd Id)) (123,That "xy")
-- PresentT "found That \"xy\" fst=123"
--
-- >>> pz @(ThisDef (MEmptyT _) Id) (That 222)
-- PresentT ()
--
-- >>> pz @(ThisDef (MEmptyT (SG.Sum _)) Id) (These 222 'x')
-- PresentT (Sum {getSum = 0})
--
-- >>> pl @(ThisDef (MEmptyT _) Id) (This (SG.Sum 12))
-- Present Sum {getSum = 12} (ThisDef This)
-- PresentT (Sum {getSum = 12})
--
-- >>> pl @(ThisDef (MEmptyT _) Id) (That 12)
-- Present () (ThisDef That)
-- PresentT ()
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
          This a -> pure $ mkNode opts (PresentT a) (msg0 <> " This") [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (PresentT p) (msg0 <> " " <> showThese q) [hh qq, hh pp]


-- | extract the That value from an 'These' otherwise use the default value
--
-- if there is no That value then \p\ is passed the whole context only
--
-- >>> pz @(ThatDef (1 % 4) Id) (That 20.4)
-- PresentT (102 % 5)
--
-- >>> pz @(ThatDef (1 % 4) Id) (This "aa")
-- PresentT (1 % 4)
--
-- >>> pz @(ThatDef (1 % 4) Id) (These "aa" 2.3)
-- PresentT (1 % 4)
--
-- >>> pz @(ThatDef (PrintT "found %s fst=%d" '(ShowP (Snd Id), Fst Id)) (Snd Id)) (123,This "xy")
-- PresentT "found This \"xy\" fst=123"
--
-- >>> pz @(ThatDef (MEmptyT _) Id) (This 222)
-- PresentT ()
--
-- >>> pz @(ThatDef (MEmptyT (SG.Sum _)) Id) (These 'x' 1120)
-- PresentT (Sum {getSum = 0})
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
          That a -> pure $ mkNode opts (PresentT a) (msg0 <> " That") [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (PresentT p) (msg0 <> " " <> showThese q) [hh qq, hh pp]

-- | extract the These value from an 'These' otherwise use the default value
--
-- if there is no These value then \p\ is passed the whole context only
--
-- >>> pz @(TheseDef '(1 % 4,"zz") Id) (These 20.4 "x")
-- PresentT (102 % 5,"x")
--
-- >>> pz @(TheseDef '(1 % 4,"zz") Id) (This 20.4)
-- PresentT (1 % 4,"zz")
--
-- >>> pz @(TheseDef '(1 % 4,"zz") Id) (That "x")
-- PresentT (1 % 4,"zz")
--
-- >>> pz @(TheseDef '(PrintT "found %s fst=%d" '(ShowP (Snd Id), Fst Id),999) (Snd Id)) (123,This "xy")
-- PresentT ("found This \"xy\" fst=123",999)
--
-- >>> pz @(TheseDef (MEmptyT (SG.Sum _, String)) Id) (This 222)
-- PresentT (Sum {getSum = 0},"")
--
-- >>> pz @(TheseDef (MEmptyT _) Id) (These (222 :: SG.Sum Int) "aa")
-- PresentT (Sum {getSum = 222},"aa")
--
-- >>> pl @(TheseDef '("xyz",'True) Id) (This "abc")
-- Present ("xyz",True) (TheseDef This)
-- PresentT ("xyz",True)
--
-- >>> pl @(TheseDef '("xyz",'True) Id) (That False)
-- Present ("xyz",True) (TheseDef That)
-- PresentT ("xyz",True)
--
-- >>> pl @(TheseDef '("xyz",'True) Id) (These "abc" False)
-- Present ("abc",False) (TheseDef These)
-- PresentT ("abc",False)
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
          These a b -> pure $ mkNode opts (PresentT (a,b)) (msg0 <> " These") [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (PresentT p) (msg0 <> " " <> showThese q) [hh qq, hh pp]


-- | extract the This value from a 'These' otherwise fail with a message
--
-- if there is no This value then \p\ is passed the whole context only
--
-- >>> pz @(ThisFail "oops" Id) (This 20.4)
-- PresentT 20.4
--
-- >>> pz @(ThisFail "oops" Id) (That "aa")
-- FailT "oops"
--
-- >>> pz @(ThisFail (PrintT "found %s fst=%d" '(ShowP (Snd Id),Fst Id)) (Snd Id)) (123,That "xy")
-- FailT "found That \"xy\" fst=123"
--
-- >>> pz @(ThisFail (MEmptyT _) Id) (That 222)
-- FailT ""
--
-- >>> pl @(ThisFail "sdf" Id) (This (SG.Sum 12))
-- Present Sum {getSum = 12} (This)
-- PresentT (Sum {getSum = 12})
--
-- >>> pl @(ThisFail "sdf" Id) (That (SG.Sum 12))
-- Error sdf (ThisFail That)
-- FailT "sdf"
--
-- >>> pl @(ThisFail "sdf" Id) (That 12)
-- Error sdf (ThisFail That)
-- FailT "sdf"
--
data ThisFail p q

instance ( PP p x ~ String
         , PP q x ~ These a b
         , P p x
         , P q x)
    => P (ThisFail p q) x where
  type PP (ThisFail p q) x = ThisT (PP q x)
  eval _ opts x = do
    let msg0 = "ThisFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          This a -> pure $ mkNode opts (PresentT a) "This" [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (FailT p) (msg0 <> " " <> showThese q) [hh qq, hh pp]


-- | extract the That value from a 'These' otherwise fail with a message
--
-- if there is no That value then \p\ is passed the whole context only
--
-- >>> pz @(ThatFail "oops" Id) (That 20.4)
-- PresentT 20.4
--
-- >>> pz @(ThatFail "oops" Id) (This "aa")
-- FailT "oops"
--
-- >>> pz @(ThatFail (PrintT "found %s fst=%d" '(ShowP (Snd Id),Fst Id)) (Snd Id)) (123,This "xy")
-- FailT "found This \"xy\" fst=123"
--
-- >>> pz @(ThatFail (MEmptyT _) Id) (This 222)
-- FailT ""
--
data ThatFail p q

instance ( PP p x ~ String
         , PP q x ~ These a b
         , P p x
         , P q x)
    => P (ThatFail p q) x where
  type PP (ThatFail p q) x = ThatT (PP q x)
  eval _ opts x = do
    let msg0 = "ThatFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          That a -> pure $ mkNode opts (PresentT a) "That" [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (FailT p) (msg0 <> " " <> showThese q) [hh qq, hh pp]




-- | extract the These value from a 'These' otherwise fail with a message
--
-- if there is no These value then \p\ is passed the whole context only
--
-- >>> pz @(TheseFail "oops" Id) (These "abc" 20.4)
-- PresentT ("abc",20.4)
--
-- >>> pz @(TheseFail "oops" Id) (That "aa")
-- FailT "oops"
--
-- >>> pz @(TheseFail (PrintT "found %s fst=%d" '(ShowP (Snd Id),Fst Id)) (Snd Id)) (123,That "xy")
-- FailT "found That \"xy\" fst=123"
--
-- >>> pz @(TheseFail (MEmptyT _) Id) (That 222)
-- FailT ""
--
data TheseFail p q

instance ( PP p x ~ String
         , PP q x ~ These a b
         , P p x
         , P q x)
    => P (TheseFail p q) x where
  type PP (TheseFail p q) x = TheseT (PP q x)
  eval _ opts x = do
    let msg0 = "TheseFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          These a b -> pure $ mkNode opts (PresentT (a,b)) "These" [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (FailT p) (msg0 <> " " <> showThese q) [hh qq, hh pp]

{-
data These' p
type TheseT' p = TheseFail "expected These" p

instance P (TheseT' p) x => P (These' p) x where
  type PP (These' p) x = PP (TheseT' p) x
  eval _ = eval (Proxy @(TheseT' p))
-}