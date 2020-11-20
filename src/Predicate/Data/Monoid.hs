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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE EmptyDataDeriving #-}
-- | promoted 'Semigroup' and 'Monoid' functions
module Predicate.Data.Monoid (
    type (<>)
  , MConcat
  , SConcat
  , STimes
  , Sap
  , type S
  , MEmptyT
  , MEmptyT'
  , MEmptyP
 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Data.Proxy (Proxy(Proxy))
import Data.Kind (Type)
import qualified Data.Semigroup as SG
import Data.List.NonEmpty (NonEmpty(..))

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate
-- >>> import qualified Data.Semigroup as SG
-- >>> import Data.Functor.Identity

-- | similar to 'SG.<>'
--
-- >>> pz @(Fst <> Snd) ("abc","def")
-- Val "abcdef"
--
-- >>> pz @("abcd" <> "ef" <> Id) "ghi"
-- Val "abcdefghi"
--
-- >>> pz @("abcd" <> "ef" <> Id) "ghi"
-- Val "abcdefghi"
--
-- >>> pz @(Wrap (SG.Sum _) Id <> (10 >> FromInteger _)) 13
-- Val (Sum {getSum = 23})
--
-- >>> pz @(Wrap (SG.Product _) Id <> Lift (FromInteger _) 10) 13
-- Val (Product {getProduct = 130})
--
-- >>> pz @('(10 >> FromInteger _,"def") <> Id) (SG.Sum 12, "_XYZ")
-- Val (Sum {getSum = 22},"def_XYZ")
--
data p <> q deriving Show
infixr 6 <>

instance ( Semigroup (PP p x)
         , PP p x ~ PP q x
         , P p x
         , Show (PP q x)
         , P q x
         ) => P (p <> q) x where
  type PP (p <> q) x = PP p x
  eval _ opts x = do
    let msg0 = "<>"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <> q
        in mkNode opts (Val d) (showL opts p <> " <> " <> showL opts q <> " = " <> showL opts d) [hh pp, hh qq]

-- | synonym for wrapping a monoid
type S a = SG.WrappedMonoid a

-- | semigroup append both sides of a tuple (ie uncurry (<>)) using 'Wrap' and then unwraps the final result
--
-- >>> pz @(Sap (SG.Sum _)) (4,5)
-- Val 9
--
-- >>> pz @(Sap (SG.Sum _)) (13,44)
-- Val 57
--
-- >>> pz @(Sap SG.Any) (True,False)
-- Val True
--
-- >>> pz @(Sap SG.All) (True,False)
-- Val False
--
-- >>> pz @(Sap (SG.Max _)) (10,12)
-- Val 12
--
-- >>> pz @(Sap (SG.Sum _)) (10,12)
-- Val 22
--
-- >>> pz @(Sap (S _)) ("abc","def")
-- Val "abcdef"
--
-- >>> pz @(Fst <> Snd) ("abc","def") -- same as above but more direct
-- Val "abcdef"
--
data Sap (t :: Type) deriving Show
type SapT (t :: Type) = Wrap t Fst <> Wrap t Snd >> Unwrap

instance P (SapT t) x => P (Sap t) x where
  type PP (Sap t) x = PP (SapT t) x
  eval _ = eval (Proxy @(SapT t))

-- | similar to 'mconcat'
--
-- >>> pz @MConcat [SG.Sum 44, SG.Sum 12, SG.Sum 3]
-- Val (Sum {getSum = 59})
--
-- >>> pz @(Map '(Pure SG.Sum Id, Pure SG.Max Id) >> MConcat) [7 :: Int,6,1,3,5] -- monoid so need eg Int
-- Val (Sum {getSum = 22},Max {getMax = 7})
--
data MConcat deriving Show

instance ( x ~ [a]
         , Show a
         , Monoid a
         ) => P MConcat x where
  type PP MConcat x = ExtractAFromList x
  eval _ opts x =
    let msg0 = "MConcat"
        b = mconcat x
    in pure $ mkNode opts (Val b) (show3 opts msg0 b x) []

-- | similar to 'SG.sconcat'
--
-- >>> pz @(ToNEList >> SConcat Id) [SG.Sum 44, SG.Sum 12, SG.Sum 3]
-- Val (Sum {getSum = 59})
--
-- >>> pz @(Map '(Pure SG.Sum Id, Pure SG.Max Id) >> ToNEList >> SConcat Id) [7,6,1,3,5]
-- Val (Sum {getSum = 22},Max {getMax = 7})
--
data SConcat p deriving Show

instance ( PP p x ~ NonEmpty a
         , P p x
         , Show a
         , Semigroup a
         ) => P (SConcat p) x where
  type PP (SConcat p) x = ExtractAFromTA (PP p x)
  eval _ opts x = do
    let msg0 = "SConcat"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = SG.sconcat p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

-- | similar to 'mempty'
--
-- >>> pl @(MEmptyT' Id) (Just (SG.Sum 12))
-- Present Nothing (MEmptyT Nothing)
-- Val Nothing
--
data MEmptyT' t deriving Show -- no Monoid for Maybe a unless a is also a monoid but can use empty!
instance ( Show (PP t a)
         , Monoid (PP t a)
         ) => P (MEmptyT' t) a where
  type PP (MEmptyT' t) a = PP t a
  eval _ opts _ =
    let msg0 = "MEmptyT"
        b = mempty @(PP t a)
    in pure $ mkNode opts (Val b) (msg0 <> " " <> showL opts b) []

-- | similar to 'mempty'
--
-- >>> pz @(MEmptyT (SG.Sum Int)) ()
-- Val (Sum {getSum = 0})
--
-- >>> pl @(MEmptyT _ ||| Ones) (Right "abc")
-- Present ["a","b","c"] ((|||) Right ["a","b","c"] | "abc")
-- Val ["a","b","c"]
--
-- >>> pl @(MEmptyT _ ||| Ones) (Left ["ab"])
-- Present [] ((|||) Left [] | ["ab"])
-- Val []
--
-- >>> pl @(MEmptyT (Maybe ())) 'x'
-- Present Nothing (MEmptyT Nothing)
-- Val Nothing
--
-- >>> pl @(MEmptyT (SG.Sum _) >> Unwrap >> Id + 4) ()
-- Present 4 ((>>) 4 | {0 + 4 = 4})
-- Val 4
--
-- >>> pz @(FMap (MEmptyT (SG.Product Int))) [Identity (-13), Identity 4, Identity 99]
-- Val [Product {getProduct = 1},Product {getProduct = 1},Product {getProduct = 1}]
--
-- >>> pl @(FMap (MEmptyT (SG.Sum _))) (Just ())
-- Present Just (Sum {getSum = 0}) (FMap MEmptyT Sum {getSum = 0})
-- Val (Just (Sum {getSum = 0}))
--
data MEmptyT (t :: Type) deriving Show
type MEmptyTT (t :: Type) = MEmptyT' (Hole t)

instance P (MEmptyTT t) x => P (MEmptyT t) x where
  type PP (MEmptyT t) x = PP (MEmptyTT t) x
  eval _ = eval (Proxy @(MEmptyTT t))

-- | creates a mempty value for the proxy
--
-- >>> pl @('Proxy >> MEmptyP) "abc"
-- Present "" ((>>) "" | {MEmptyT ""})
-- Val ""
--
data MEmptyP deriving Show
type MEmptyPT = MEmptyT' UnproxyT -- expects a proxy: so only some things work with this: eg MaybeIn

instance P MEmptyPT x => P MEmptyP x where
  type PP MEmptyP x = PP MEmptyPT x
  eval _ = eval (Proxy @MEmptyPT)

-- | similar to 'SG.stimes'
--
-- >>> pz @(STimes 4 Id) (SG.Sum 3)
-- Val (Sum {getSum = 12})
--
-- >>> pz @(STimes 4 Id) "ab"
-- Val "abababab"
--
-- >>> pl @(STimes 4 Id) (SG.Sum 13)
-- Present Sum {getSum = 52} (STimes 4 p=Sum {getSum = 13} Sum {getSum = 52} | n=4 | Sum {getSum = 13})
-- Val (Sum {getSum = 52})
--
-- >>> pl @(STimes Fst Snd) (4,['x','y'])
-- Present "xyxyxyxy" (STimes 4 p="xy" "xyxyxyxy" | n=4 | "xy")
-- Val "xyxyxyxy"
--
-- >>> pl @(STimes Fst Snd) (4,"abc")
-- Present "abcabcabcabc" (STimes 4 p="abc" "abcabcabcabc" | n=4 | "abc")
-- Val "abcabcabcabc"
--
-- >>> pl @(STimes 4 Id) "abc"
-- Present "abcabcabcabc" (STimes 4 p="abc" "abcabcabcabc" | n=4 | "abc")
-- Val "abcabcabcabc"
--
data STimes n p deriving Show
instance ( P n a
         , Integral (PP n a)
         , Semigroup (PP p a)
         , P p a
         , Show (PP p a)
         ) => P (STimes n p) a where
  type PP (STimes n p) a = PP p a
  eval _ opts a = do
    let msg0 = "STimes"
    lr <- runPQ NoInline msg0 (Proxy @n) (Proxy @p) opts a []
    pure $ case lr of
      Left e -> e
      Right (fromIntegral -> n::Int,p,pp,qq) ->
        let msg1 = msg0 <> " " <> showL opts n <> " p=" <> showL opts p
            b = SG.stimes n p
            in mkNode opts (Val b) (show3' opts msg1 b "n=" n <> showVerbose opts " | " p) [hh pp, hh qq]
