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
{-# LANGUAGE ViewPatterns #-}
{- |
     promoted foldable functions
-}
module Predicate.Data.Foldable (
    Concat
  , ConcatMap
  , Cycle
  , FoldMap

  , ToListExt
  , FromList
  , FromListExt

  , ToList

  , IToList
  , IToList'

  , ToNEList

  , Null
  , Null'
  , IsEmpty

  , Ands
  , Ors

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Predicate.Data.Monoid (MConcat)
import Control.Lens
import Data.Typeable (Typeable, Proxy(Proxy))
import Data.Kind (Type)
import Data.Foldable (Foldable(toList))
import qualified Data.List.NonEmpty as N
import Data.List.NonEmpty (NonEmpty(..))
import qualified GHC.Exts as GE
import Data.List (findIndex)
import qualified Safe (cycleNote)
-- $setup
-- >>> import Predicate.Prelude
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XAllowAmbiguousTypes
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> :set -XFlexibleContexts
-- >>> import qualified Data.Map.Strict as M
-- >>> import qualified Data.Set as Set
-- >>> import qualified Data.Text as T
-- >>> import qualified Data.Semigroup as SG
-- >>> import Data.These
-- >>> import Data.Time

-- | create a 'NonEmpty' list from a 'Foldable'
--
-- >>> pz @ToNEList []
-- Fail "empty list"
--
-- >>> pz @ToNEList [1,2,3,4,5]
-- Val (1 :| [2,3,4,5])
--
data ToNEList
instance ( Show (t a)
         , Foldable t
         ) => P ToNEList (t a) where
  type PP ToNEList (t a) = NonEmpty a
  eval _ opts as =
    let msg0 = "ToNEList"
    in pure $ case toList as of
         [] -> mkNode opts (Fail "empty list") msg0 []
         x:xs -> mkNode opts (Val (x N.:| xs)) (msg0 <> showVerbose opts " " as) []


-- cant directly create a singleton type using '[] since the type of '[] is unknown. instead use 'Singleton' or 'EmptyT'

-- | similar to 'null' using 'AsEmpty'
--
-- >>> pz @IsEmpty [1,2,3,4]
-- Val False
--
-- >>> pz @IsEmpty []
-- Val True
--
-- >>> pz @IsEmpty LT
-- Val False
--
-- >>> pz @IsEmpty EQ
-- Val True
--
-- >>> pl @IsEmpty ("failed11" :: T.Text)
-- False (IsEmpty | "failed11")
-- Val False
--
-- >>> pl @IsEmpty ("" :: T.Text)
-- True (IsEmpty | "")
-- Val True
--
data IsEmpty

instance ( Show as
         , AsEmpty as
         ) => P IsEmpty as where
  type PP IsEmpty as = Bool
  eval _ opts as =
    let b = has _Empty as
    in pure $ mkNodeB opts b ("IsEmpty" <> showVerbose opts " | " as) []

data IToList' t

instance ( Show (f a)
         , Typeable (PP t x)
         , Show (PP t x)
         , FoldableWithIndex (PP t x) f
         , x ~ f a
         , Show a
         ) => P (IToList' t) x where
  type PP (IToList' t) x = [(PP t x, ExtractAFromTA x)]
  eval _ opts x =
    let msg0 = "IToList"
        b = itoList x
        t = showT @(PP t x)
    in pure $ mkNode opts (Val b) (msg0 <> "(" <> t <> ") " <> showL opts b <> showVerbose opts " | " x) []

-- | similar to 'Control.Lens.itoList'
--
-- >>> pz @(IToList _) ("aBc" :: String)
-- Val [(0,'a'),(1,'B'),(2,'c')]
--
-- >>> pl @(IToList _) ("abcd" :: String)
-- Present [(0,'a'),(1,'b'),(2,'c'),(3,'d')] (IToList(Int) [(0,'a'),(1,'b'),(2,'c'),(3,'d')] | "abcd")
-- Val [(0,'a'),(1,'b'),(2,'c'),(3,'d')]
--
-- >>> pl @(IToList _) (M.fromList $ itoList ("abcd" :: String))
-- Present [(0,'a'),(1,'b'),(2,'c'),(3,'d')] (IToList(Int) [(0,'a'),(1,'b'),(2,'c'),(3,'d')] | fromList [(0,'a'),(1,'b'),(2,'c'),(3,'d')])
-- Val [(0,'a'),(1,'b'),(2,'c'),(3,'d')]
--
-- >>> pl @(IToList _) [9,2,7,4]
-- Present [(0,9),(1,2),(2,7),(3,4)] (IToList(Int) [(0,9),(1,2),(2,7),(3,4)] | [9,2,7,4])
-- Val [(0,9),(1,2),(2,7),(3,4)]
--
-- >>> pl @(IToList _) (M.fromList (zip ['a'..] [9,2,7,4]))
-- Present [('a',9),('b',2),('c',7),('d',4)] (IToList(Char) [('a',9),('b',2),('c',7),('d',4)] | fromList [('a',9),('b',2),('c',7),('d',4)])
-- Val [('a',9),('b',2),('c',7),('d',4)]
--
-- >>> pl @(IToList _) (Just 234)
-- Present [((),234)] (IToList(()) [((),234)] | Just 234)
-- Val [((),234)]
--
-- >>> pl @(IToList _) (Nothing @Double)
-- Present [] (IToList(()) [] | Nothing)
-- Val []
--
-- >>> pl @(IToList _) [1..5]
-- Present [(0,1),(1,2),(2,3),(3,4),(4,5)] (IToList(Int) [(0,1),(1,2),(2,3),(3,4),(4,5)] | [1,2,3,4,5])
-- Val [(0,1),(1,2),(2,3),(3,4),(4,5)]
--
-- >>> pl @(IToList _) ['a','b','c']
-- Present [(0,'a'),(1,'b'),(2,'c')] (IToList(Int) [(0,'a'),(1,'b'),(2,'c')] | "abc")
-- Val [(0,'a'),(1,'b'),(2,'c')]
--
data IToList (t :: Type)
type IToListT (t :: Type) = IToList' (Hole t)

instance P (IToListT t) x => P (IToList t) x where
  type PP (IToList t) x = PP (IToListT t) x
  eval _ = eval (Proxy @(IToListT t))

-- | invokes 'GE.toList'
--
-- >>> pz @ToListExt (M.fromList [(1,'x'),(4,'y')])
-- Val [(1,'x'),(4,'y')]
--
-- >>> pz @ToListExt (T.pack "abc")
-- Val "abc"
--
data ToListExt

instance ( Show l
         , GE.IsList l
         , Show (GE.Item l)
         ) => P ToListExt l where
  type PP ToListExt l = [GE.Item l]
  eval _ opts as =
    let msg0 = "ToListExt"
        z = GE.toList as
    in pure $ mkNode opts (Val z) (show3 opts msg0 z as) []

-- | invokes 'GE.fromList'
--
-- >>> run @('OMsg "Fred" ':# 'OLite ':# 'OColorOff) @(FromList (Set.Set Int) << '[2,1,5,5,2,5,2]) ()
-- Fred >>> Present fromList [1,2,5] ((>>) fromList [1,2,5] | {FromList fromList [1,2,5]})
-- Val (fromList [1,2,5])
--
-- >>> pl @(FromList (M.Map _ _) >> Id !! Char1 "y") [('x',True),('y',False)]
-- Present False ((>>) False | {IxL('y') False | p=fromList [('x',True),('y',False)] | q='y'})
-- Val False
--
-- >>> pl @(FromList (M.Map _ _) >> Id !! Char1 "z") [('x',True),('y',False)]
-- Error (!!) index not found (IxL('z'))
-- Fail "(!!) index not found"
--

data FromList (t :: Type) -- doesnt work with OverloadedLists unless you cast to [a] explicitly

instance ( a ~ GE.Item t
         , Show t
         , GE.IsList t
         , [a] ~ x
         ) => P (FromList t) x where
  type PP (FromList t) x = t
  eval _ opts as =
    let msg0 = "FromList"
        z = GE.fromList (as :: [GE.Item t]) :: t
    in pure $ mkNode opts (Val z) (msg0 <> " " <> showL opts z) []

-- | invokes 'GE.fromList'
--
-- requires the OverloadedLists extension
--
-- >>> :set -XOverloadedLists
-- >>> pz @(FromListExt (M.Map _ _)) [(4,"x"),(5,"dd")]
-- Val (fromList [(4,"x"),(5,"dd")])
--
data FromListExt (t :: Type)
-- l ~ l' is key
instance ( Show l
         , GE.IsList l
         , l ~ l'
         ) => P (FromListExt l') l where
  type PP (FromListExt l') l = l'
  eval _ opts as =
    let msg0 = "FromListExt"
        z = GE.fromList (GE.toList @l as)
    in pure $ mkNode opts (Val z) (msg0 <> " " <> showL opts z) []

-- | similar to 'concat'
--
-- >>> pz @Concat ["abc","D","eF","","G"]
-- Val "abcDeFG"
--
-- >>> pz @(Lift Concat Snd) ('x',["abc","D","eF","","G"])
-- Val "abcDeFG"
--
data Concat

instance ( Show a
         , Show (t [a])
         , x ~ t [a]
         , Foldable t
         ) => P Concat x where
  type PP Concat x = ExtractAFromTA x
  eval _ opts x =
    let msg0 = "Concat"
        b = concat x
    in pure $ mkNode opts (Val b) (show3 opts msg0 b x) []

-- | similar to 'concatMap'
data ConcatMap p q
type ConcatMapT p q = Map p q >> Concat

instance P (ConcatMapT p q) x => P (ConcatMap p q) x where
  type PP (ConcatMap p q) x = PP (ConcatMapT p q) x
  eval _ = eval (Proxy @(ConcatMapT p q))


-- | similar to 'cycle' but for a fixed number @n@
--
-- >>> pz @(Cycle 5 Id) [1,2]
-- Val [1,2,1,2,1]
--
data Cycle n p

instance ( Show a
         , Show (t a)
         , PP p x ~ t a
         , P p x
         , Integral (PP n x)
         , P n x
         , Foldable t
         ) => P (Cycle n p) x where
  type PP (Cycle n p) x = [ExtractAFromTA (PP p x)]
  eval _ opts x = do
    let msg0 = "Cycle"
    lr <- runPQ msg0 (Proxy @n) (Proxy @p) opts x []
    pure $ case lr of
      Left e -> e
      Right (fromIntegral -> n,p,nn,pp) ->
        let hhs = [hh nn, hh pp]
        in case chkSize opts msg0 p hhs of
            Left e ->  e
            Right _ ->
              let msg1 = msg0 <> "(" <> show n <> ")"
                  d = take n (Safe.cycleNote msg0 (toList p))
              in mkNode opts (Val d) (show3 opts msg1 d p) hhs


-- | similar to 'toList'
--
-- >>> pz @ToList "aBc"
-- Val "aBc"
--
-- >>> pz @ToList (Just 14)
-- Val [14]
--
-- >>> pz @ToList Nothing
-- Val []
--
-- >>> pz @ToList (Left "xx")
-- Val []
--
-- >>> pz @ToList (These 12 "xx")
-- Val ["xx"]
--
-- >>> pl @ToList (M.fromList $ zip [0..] "abcd")
-- Present "abcd" (ToList fromList [(0,'a'),(1,'b'),(2,'c'),(3,'d')])
-- Val "abcd"
--
-- >>> pl @ToList (Just 123)
-- Present [123] (ToList Just 123)
-- Val [123]
--
-- >>> pl @ToList (M.fromList (zip ['a'..] [9,2,7,4]))
-- Present [9,2,7,4] (ToList fromList [('a',9),('b',2),('c',7),('d',4)])
-- Val [9,2,7,4]
--

data ToList
instance ( Show (t a)
         , Foldable t
         ) => P ToList (t a) where
  type PP ToList (t a) = [a]
  eval _ opts as =
    let msg0 = "ToList"
        z = toList as
    in pure $ mkNode opts (Val z) (msg0 <> showVerbose opts " " as) []

data Null' p

instance ( Show (t a)
         , Foldable t
         , t a ~ PP p x
         , P p x
         ) => P (Null' p) x where
  type PP (Null' p) x = Bool
  eval _ opts x = do
    let msg0 = "Null"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = null p
        in mkNodeB opts b ("Null" <> showVerbose opts " | " p) [hh pp]

-- | similar to 'null' using 'Foldable'
--
-- >>> pz @Null [1,2,3,4]
-- Val False
--
-- >>> pz @Null []
-- Val True
--
-- >>> pz @Null Nothing
-- Val True
--
data Null
type NullT = Null' Id
instance P NullT a => P Null a where
  type PP Null a = Bool
  eval _ = evalBool (Proxy @NullT)

-- | similar to a limited form of 'foldMap'
--
-- >>> pz @(FoldMap (SG.Sum _) Id) [44, 12, 3]
-- Val 59
--
-- >>> pz @(FoldMap (SG.Product _) Id) [44, 12, 3]
-- Val 1584
--
-- >>> type Ands' p = FoldMap SG.All p
-- >>> pz @(Ands' Id) [True,False,True,True]
-- Val False
--
-- >>> pz @(Ands' Id) [True,True,True]
-- Val True
--
-- >>> pz @(Ands' Id) []
-- Val True
--
-- >>> type Ors' p = FoldMap SG.Any p
-- >>> pz @(Ors' Id) [False,False,False]
-- Val False
--
-- >>> pz @(Ors' Id) []
-- Val False
--
-- >>> pz @(Ors' Id) [False,False,False,True]
-- Val True
--
-- >>> type AllPositive' = FoldMap SG.All (Map Positive Id)
-- >>> pz @AllPositive' [3,1,-5,10,2,3]
-- Val False
--
-- >>> type AllNegative' = FoldMap SG.All (Map Negative Id)
-- >>> pz @AllNegative' [-1,-5,-10,-2,-3]
-- Val True
--
-- >>> :set -XKindSignatures
-- >>> type Max' (t :: Type) = FoldMap (SG.Max t) Id -- requires t be Bounded for monoid instance
-- >>> pz @(Max' Int) [10,4,5,12,3,4]
-- Val 12
--
-- >>> pl @(FoldMap (SG.Sum _) Id) [14,8,17,13]
-- Present 52 ((>>) 52 | {getSum = 52})
-- Val 52
--
-- >>> pl @(FoldMap (SG.Max _) Id) [14 :: Int,8,17,13] -- cos Bounded!
-- Present 17 ((>>) 17 | {getMax = 17})
-- Val 17
--
-- >>> pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) || (FoldMap (SG.Sum _) Id >> Gt 200)) [1..20]
-- True (False || True)
-- Val True
--
-- >>> pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) || (FoldMap (SG.Sum _) Id >> Gt 200)) [1..19]
-- False (False || False | ((>>) False | {1 == 0})}) || ((>>) False | {190 > 200}))
-- Val False
--
-- >>> pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) || (FoldMap (SG.Sum _) Id >> Gt 200)) []
-- True (True || False)
-- Val True
--
-- >>> pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) &&& FoldMap (SG.Sum _) Id) [1..20]
-- Present (False,210) (W '(False,210))
-- Val (False,210)
--
-- >>> pl @(FoldMap SG.Any Id) [False,False,True,False]
-- Present True ((>>) True | {getAny = True})
-- Val True
--
-- >>> pl @(FoldMap SG.All Id) [False,False,True,False]
-- Present False ((>>) False | {getAll = False})
-- Val False
--
-- >>> pl @(FoldMap (SG.Sum _) Id) (Just 13)
-- Present 13 ((>>) 13 | {getSum = 13})
-- Val 13
--
-- >>> pl @(FoldMap (SG.Sum _) Id) [1..10]
-- Present 55 ((>>) 55 | {getSum = 55})
-- Val 55
--

data FoldMap (t :: Type) p
type FoldMapT (t :: Type) p = Map (Wrap t Id) p >> MConcat Id >> Unwrap

instance P (FoldMapT t p) x => P (FoldMap t p) x where
  type PP (FoldMap t p) x = PP (FoldMapT t p) x
  eval _ = eval (Proxy @(FoldMapT t p))

-- | similar to 'Data.Foldable.and'
--
-- >>> pz @Ands [True,True,True]
-- Val True
--
-- >>> pl @Ands [True,True,True,False]
-- False (Ands(4) i=3 | [True,True,True,False])
-- Val False
--
-- >>> pz @Ands []
-- Val True
--
data Ands

instance ( x ~ t a
         , Show (t a)
         , Foldable t
         , a ~ Bool
         ) => P Ands x where
  type PP Ands x = Bool
  eval _ opts x =
    let msg0 = "Ands"
        msg1 = msg0 ++ "(" ++ show (length x) ++ ")"
        w = case findIndex not (toList x) of
              Nothing -> ""
              Just i -> " i="++show i
    in pure $ mkNodeB opts (and x) (msg1 <> w <> showVerbose opts " | " x) []

-- | similar to 'Data.Foldable.or'
--
-- >>> pz @Ors [False,False,False]
-- Val False
--
-- >>> pl @Ors [True,True,True,False]
-- True (Ors(4) i=0 | [True,True,True,False])
-- Val True
--
-- >>> pl @Ors []
-- False (Ors(0) | [])
-- Val False
--
data Ors

instance ( x ~ t a
         , Show (t a)
         , Foldable t
         , a ~ Bool
         ) => P Ors x where
  type PP Ors x = Bool
  eval _ opts x =
    let msg0 = "Ors"
        msg1 = msg0 ++ "(" ++ show (length x) ++ ")"
        w = case findIndex id (toList x) of
              Nothing -> ""
              Just i -> " i=" ++ show i
    in pure $ mkNodeB opts (or x) (msg1 <> w <> showVerbose opts " | " x) []

