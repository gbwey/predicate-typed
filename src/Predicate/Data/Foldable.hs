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
{-# LANGUAGE ViewPatterns #-}
-- | promoted foldable functions
module Predicate.Data.Foldable (
    Concat
  , ConcatMap
  , Cycle
  , FoldAla
  , FoldMap

  , ToListExt
  , FromList
  , FromListExt

  , ToList

  , IToList
  , IToList'

  , ToNEList

  , OneP

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
import Data.Foldable (Foldable(toList,fold))
import qualified Data.List.NonEmpty as N
import Data.List.NonEmpty (NonEmpty(..))
import qualified GHC.Exts as GE
import Data.List (findIndex)
-- $setup
-- >>> import Predicate
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
data ToNEList deriving Show
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
data IsEmpty deriving Show

instance ( Show as
         , AsEmpty as
         ) => P IsEmpty as where
  type PP IsEmpty as = Bool
  eval _ opts as =
    let b = has _Empty as
    in pure $ mkNodeB opts b ("IsEmpty" <> showVerbose opts " | " as) []

-- | explicit version of 'IToList' with an extra parameter @p@ to point to the value
--
-- >>> pz @(IToList' (Hole _) Snd) (True,"aBc" :: String)
-- Val [(0,'a'),(1,'B'),(2,'c')]
--
data IToList' t p deriving Show

instance ( Typeable (PP t x)
         , Show (PP t x)
         , FoldableWithIndex (PP t x) f
         , PP p x ~ f a
         , P p x
         , Show x
         , Show a
         ) => P (IToList' t p) x where
  type PP (IToList' t p) x = [(PP t x, ExtractAFromTA (PP p x))]
  eval _ opts x = do
    let msg0 = "IToList"
        t = showT @(PP t x)
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = itoList p
        in mkNode opts (Val b) (msg0 <> "(" <> t <> ") " <> showL opts b <> showVerbose opts " | " x) [hh pp]

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
data IToList (t :: Type) deriving Show
type IToListT (t :: Type) = IToList' (Hole t) Id

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
data ToListExt deriving Show

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
-- >>> pl @(FromList (M.Map _ _) >> Id !! C "y") [('x',True),('y',False)]
-- Present False ((>>) False | {IxL('y') False | p=fromList [('x',True),('y',False)] | q='y'})
-- Val False
--
-- >>> pl @(FromList (M.Map _ _) >> Id !! C "z") [('x',True),('y',False)]
-- Error (!!) index not found (IxL('z') | fromList [('x',True),('y',False)])
-- Fail "(!!) index not found"
--
-- >>> pl @(FromList (M.Map _ _)) [(4,"x"),(5,"dd")]
-- Present fromList [(4,"x"),(5,"dd")] (FromList fromList [(4,"x"),(5,"dd")])
-- Val (fromList [(4,"x"),(5,"dd")])
--
data FromList (t :: Type) deriving Show
-- doesnt work with OverloadedLists unless you cast to [a] explicitly

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
data FromListExt (t :: Type) deriving Show
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
-- >>> pz @Concat (Just "abc")
-- Val "abc"
--
-- >>> pz @Concat (Left 123)
-- Val []
--
-- >>> pz @Concat ["abc","D","eF","","G"]
-- Val "abcDeFG"
--
-- >>> pz @(Snd >> Concat) ('x',["abc","D","eF","","G"])
-- Val "abcDeFG"
--
data Concat deriving Show

instance ( Show x
         , x ~ t [a]
         , Show a
         , Foldable t
         ) => P Concat x where
  type PP Concat x = ExtractAFromTA x
  eval _ opts x =
    let msg0 = "Concat"
        b = concat x
    in pure $ mkNode opts (Val b) (show3 opts msg0 b x) []

-- | similar to 'concatMap'
data ConcatMap p q deriving Show
type ConcatMapT p q = Map' p q >> Concat

instance P (ConcatMapT p q) x => P (ConcatMap p q) x where
  type PP (ConcatMap p q) x = PP (ConcatMapT p q) x
  eval _ = eval (Proxy @(ConcatMapT p q))


-- | similar to 'cycle' but for a fixed number @n@: for an empty list it just returns an empty list
--
-- >>> pz @(Cycle 5 Id) [1,2]
-- Val [1,2,1,2,1]
--
-- >>> pz @(Cycle 5 Id) []
-- Val []
--
data Cycle n p deriving Show

instance ( PP p x ~ t a
         , Show x
         , P p x
         , Integral (PP n x)
         , P n x
         , Foldable t
         ) => P (Cycle n p) x where
  type PP (Cycle n p) x = [ExtractAFromTA (PP p x)]
  eval _ opts x = do
    let msg0 = "Cycle"
    lr <- runPQ NoInline msg0 (Proxy @n) (Proxy @p) opts x []
    pure $ case lr of
      Left e -> e
      Right (fromIntegral -> n,p,nn,pp) ->
        let hhs = [hh nn, hh pp]
        in case chkSize opts msg0 p hhs of
            Left e ->  e
            Right (_,ps) ->
              let msg1 = msg0 <> "(" <> show n <> ")"
                  d = take n (cycle' ps)
              in mkNode opts (Val d) (showVerbose opts msg1 x) hhs

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
data ToList deriving Show

instance ( Show (t a)
         , Foldable t
         ) => P ToList (t a) where
  type PP ToList (t a) = [a]
  eval _ opts as =
    let msg0 = "ToList"
        z = toList as
    in pure $ mkNode opts (Val z) (msg0 <> showVerbose opts " " as) []

-- | explicit version of 'Null' with an extra parameter @p@ to point to the value
data Null' p deriving Show

instance ( Show (t a)
         , Foldable t
         , t a ~ PP p x
         , P p x
         ) => P (Null' p) x where
  type PP (Null' p) x = Bool
  eval _ opts x = do
    let msg0 = "Null"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
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
data Null deriving Show
type NullT = Null' Id

instance P NullT a => P Null a where
  type PP Null a = Bool
  eval _ = evalBool (Proxy @NullT)

-- | similar to 'Data.Foldable.foldMap'
--
-- >>> pl @(FoldMap (Wrap (SG.Sum _) Id)) (Left "x")
-- Present Sum {getSum = 0} (FoldMap <skipped>)
-- Val (Sum {getSum = 0})
--
-- >>> pz @(FoldMap (Wrap (SG.Sum _) Id)) [1..5]
-- Val (Sum {getSum = 15})
--
-- >>> pl @(FoldMap (Wrap (SG.Sum _) Id)) (Right 123)
-- Present Sum {getSum = 123} (FoldMap Wrap Sum {getSum = 123} | 123)
-- Val (Sum {getSum = 123})
--
-- >>> pl @(FoldMap (Map Len)) (Just ["abc","defg","h"])
-- Present [3,4,1] (FoldMap Map [3,4,1] | ["abc","defg","h"])
-- Val [3,4,1]
--
-- >>> pz @(FoldMap (Map Len)) (Just ["abc","defg","h"])
-- Val [3,4,1]
--
-- >>> pz @(FoldMap (Wrap (SG.Sum _) Len)) ["abc","defg","h"]
-- Val (Sum {getSum = 8})
--
-- >>> pz @(FoldMap (FoldMap (Wrap (SG.Sum _) Id))) (Just [1..10])
-- Val (Sum {getSum = 55})
--
data FoldMap p deriving Show

instance ( Traversable n
         , Monoid t
         , PP p a ~ t
         , P p a
         ) => P (FoldMap p) (n a) where
  type PP (FoldMap p) (n a) = PP p a
  eval _ opts na = do
    let msg0 = "FoldMap"
    nttb <- traverse
              (fmap (\tt -> tt & ttString %~ litL opts
                               & ttForest .~ [hh tt]) . eval (Proxy @p) opts)
              na
    let ttnb = sequenceA nttb
    pure $ case getValueLR Inline opts "" ttnb [] of
      Left e -> e
      Right ret ->
        let ind = case foldMap pure ret of
                    [] -> " <skipped>"
                    _:_ -> ""
            d = fold ret
        in ttnb & ttVal .~ Val d
                & ttString %~ (msg0 <>) . (ind <>) . nullIf " "

-- | wraps each item in the foldable container and then unwraps the mconcatenated result: uses 'Control.Lens.Wrapped.Wrapped'
--
-- >>> pz @(FoldAla (SG.Sum _)) [44, 12, 3]
-- Val 59
--
-- >>> pz @(FoldAla (SG.Product _)) [44, 12, 3]
-- Val 1584
--
-- >>> type Ands' = FoldAla SG.All
-- >>> pz @Ands' [True,False,True,True]
-- Val False
--
-- >>> pz @Ands' [True,True,True]
-- Val True
--
-- >>> pz @Ands' []
-- Val True
--
-- >>> type Ors' = FoldAla SG.Any
-- >>> pz @Ors' [False,False,False]
-- Val False
--
-- >>> pz @Ors' []
-- Val False
--
-- >>> pz @Ors' [False,False,False,True]
-- Val True
--
-- >>> type AllPositive' = Map Positive >> FoldAla SG.All
-- >>> pz @AllPositive' [3,1,-5,10,2,3]
-- Val False
--
-- >>> type AllNegative' = Map Negative >> FoldAla SG.All
-- >>> pz @AllNegative' [-1,-5,-10,-2,-3]
-- Val True
--
-- >>> :set -XKindSignatures
-- >>> type Max' (t :: Type) = FoldAla (SG.Max t) -- requires t be Bounded for monoid instance
-- >>> pz @(Max' Int) [10,4,5,12,3,4]
-- Val 12
--
-- >>> pl @(FoldAla (SG.Sum _)) [14,8,17,13]
-- Present 52 ((>>) 52 | {getSum = 52})
-- Val 52
--
-- >>> pl @(FoldAla (SG.Max _)) [14 :: Int,8,17,13] -- allowed as the values are Bounded!
-- Present 17 ((>>) 17 | {getMax = 17})
-- Val 17
--
-- >>> pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) || (FoldAla (SG.Sum _) >> Gt 200)) [1..20]
-- True (False || True)
-- Val True
--
-- >>> pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) || (FoldAla (SG.Sum _) >> Gt 200)) [1..19]
-- False (False || False | ((>>) False | {1 == 0}) || ((>>) False | {190 > 200}))
-- Val False
--
-- >>> pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) || (FoldAla (SG.Sum _) >> Gt 200)) []
-- True (True || False)
-- Val True
--
-- >>> pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) &&& FoldAla (SG.Sum _)) [1..20]
-- Present (False,210) ('(False,210))
-- Val (False,210)
--
-- >>> pl @(FoldAla SG.Any) [False,False,True,False]
-- Present True ((>>) True | {getAny = True})
-- Val True
--
-- >>> pl @(FoldAla SG.All) [False,False,True,False]
-- Present False ((>>) False | {getAll = False})
-- Val False
--
-- >>> pl @(FoldAla (SG.Sum _)) (Just 13)
-- Present 13 ((>>) 13 | {getSum = 13})
-- Val 13
--
-- >>> pl @(FoldAla (SG.Sum _)) [1..10]
-- Present 55 ((>>) 55 | {getSum = 55})
-- Val 55
--
data FoldAla (t :: Type) deriving Show
type FoldAlaT (t :: Type) = Map' (Wrap t Id) Id >> MConcat >> Unwrap

instance P (FoldAlaT t) x => P (FoldAla t) x where
  type PP (FoldAla t) x = PP (FoldAlaT t) x
  eval _ = eval (Proxy @(FoldAlaT t))

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
data Ands deriving Show

instance ( x ~ t a
         , Foldable t
         , a ~ Bool
         ) => P Ands x where
  type PP Ands x = Bool
  eval _ opts x' =
    let msg0 = "Ands"
    in pure $ case chkSize opts msg0 x' [] of
         Left e -> e
         Right (xLen,x) ->
           let msg1 = msg0 ++ "(" ++ show xLen ++ ")"
               w = case findIndex not x of
                     Nothing -> ""
                     Just i -> " i="++show i
           in mkNodeB opts (and x) (msg1 <> w <> showVerbose opts " | " x) []

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
data Ors deriving Show

instance ( x ~ t a
         , Foldable t
         , a ~ Bool
         ) => P Ors x where
  type PP Ors x = Bool
  eval _ opts x' =
    let msg0 = "Ors"
    in pure $ case chkSize opts msg0 x' [] of
         Left e -> e
         Right (xLen,x) ->
           let msg1 = msg0 ++ "(" ++ show xLen ++ ")"
               w = case findIndex id x of
                     Nothing -> ""
                     Just i -> " i="++show i
           in mkNodeB opts (or x) (msg1 <> w <> showVerbose opts " | " x) []

-- | gets the singleton value from a foldable
--
-- >>> pl @OneP [10..15]
-- Error OneP:expected one element(6)
-- Fail "OneP:expected one element(6)"
--
-- >>> pl @OneP [10]
-- Present 10 (OneP)
-- Val 10
--
-- >>> pl @OneP []
-- Error OneP:expected one element(empty)
-- Fail "OneP:expected one element(empty)"
--
-- >>> pl @OneP (Just 10)
-- Present 10 (OneP)
-- Val 10
--
-- >>> pl @OneP Nothing
-- Error OneP:expected one element(empty)
-- Fail "OneP:expected one element(empty)"
--
data OneP deriving Show

instance ( Foldable t
         , x ~ t a
         ) => P OneP x where
  type PP OneP x = ExtractAFromTA x
  eval _ opts x = do
    let msg0 = "OneP"
    pure $ case toList x of
      [] -> mkNode opts (Fail (msg0 <> ":expected one element(empty)")) "" []
      [a] -> mkNode opts (Val a) msg0 []
      as' -> case chkSize opts msg0 as' [] of
               Left e -> e
               Right (asLen,_) ->
                 mkNode opts (Fail (msg0 <> ":expected one element(" <> show asLen <> ")")) "" []

--type OneP = Guard "expected list of length 1" (Len == 1) >> Head
--type OneP = Guard (PrintF "expected list of length 1 but found length=%d" Len) (Len == 1) >> Head

