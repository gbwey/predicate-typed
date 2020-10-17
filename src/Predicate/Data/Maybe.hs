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
-- |     promoted 'Maybe' functions
module Predicate.Data.Maybe (

 -- ** boolean predicates
    IsNothing
  , IsJust

 -- ** constructors
  , MkNothing
  , MkNothing'
  , MkJust

 -- ** get rid of Maybe
  , Just'
  , JustDef
  , JustFail
  , MapMaybe
  , CatMaybes
  , MaybeIn
  , MaybeBool
  , EmptyBool

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Predicate.Data.Foldable (ConcatMap)
import Predicate.Data.Monoid (MEmptyP)
import Data.Proxy (Proxy(..))
import Data.Kind (Type)
import Data.Maybe (isJust, isNothing)
import Control.Applicative (Alternative(empty))

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Map.Strict as M
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG

-- | similar to 'Data.Maybe.fromJust'
--
-- >>> pz @(Just' >> Succ) (Just 20)
-- Val 21
--
-- >>> pz @(Just' >> Succ) Nothing
-- Fail "Just' found Nothing"
--
data Just' deriving Show
instance Show a => P Just' (Maybe a) where
  type PP Just' (Maybe a) = a
  eval _ opts lr =
    let msg0 = "Just'"
    in pure $ case lr of
         Nothing -> mkNode opts (Fail (msg0 <> " found Nothing")) "" []
         Just a -> mkNode opts (Val a) (msg0 <> " " <> showL opts a) []

-- | constructs a Nothing for a given type
data MkNothing' t deriving Show
-- works always! MaybeBool is a good alternative and then dont need the extra 't'

-- for this to be useful has to have 't' else we end up with tons of problems
instance P (MkNothing' t) a where
  type PP (MkNothing' t) a = Maybe (PP t a)
  eval _ opts _ =
    let msg0 = "MkNothing"
    in pure $ mkNode opts (Val Nothing) msg0 []

-- | constructs a Nothing for a given type
data MkNothing (t :: Type) deriving Show
type MkNothingT (t :: Type) = MkNothing' (Hole t)

instance P (MkNothing t) x where
  type PP (MkNothing t) x = PP (MkNothingT t) x
  eval _ = eval (Proxy @(MkNothingT t))

-- | 'GHC.Maybe.Just' constructor
--
-- >>> pz @(MkJust Id) 44
-- Val (Just 44)
--
data MkJust p deriving Show
instance ( PP p x ~ a
         , P p x
         , Show a
         ) => P (MkJust p) x where
  type PP (MkJust p) x = Maybe (PP p x)
  eval _ opts x = do
    let msg0 = "MkJust"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = Just p
        in mkNode opts (Val d) (msg0 <> " Just " <> showL opts p) [hh pp]

-- | similar to 'Data.Maybe.maybe'
--
-- provides a Proxy to the result of @q@ but does not provide the surrounding context
--
-- >>> pz @(MaybeIn "foundnothing" (ShowP Pred)) (Just 20)
-- Val "19"
--
-- >>> pz @(MaybeIn "found nothing" (ShowP Pred)) Nothing
-- Val "found nothing"
--
-- >>> pl @(MaybeIn 'True Id) (Nothing @Bool) -- need @() else breaks
-- True (MaybeIn(Nothing) True | Proxy)
-- Val True
--
-- >>> pl @(MaybeIn (FailT _ "failed4") Id) (Just 10)
-- Present 10 (MaybeIn(Just) 10 | 10)
-- Val 10
--
-- >>> pl @(MaybeIn 'False Id) (Nothing @Bool) -- breaks otherwise
-- False (MaybeIn(Nothing) False | Proxy)
-- Val False
--
-- >>> pl @(MaybeIn MEmptyP Id) (Just [1,2,3])
-- Present [1,2,3] (MaybeIn(Just) [1,2,3] | [1,2,3])
-- Val [1,2,3]
--
-- >>> pl @(MaybeIn MEmptyP Id) (Nothing @[Int])
-- Present [] (MaybeIn(Nothing) [] | Proxy)
-- Val []
--
-- >>> pl @(MaybeIn (FailP "err") Succ) (Just 116)
-- Present 117 (MaybeIn(Just) 117 | 116)
-- Val 117
--
-- >>> pl @(MaybeIn 99 Succ) (Nothing @Int)
-- Present 99 (MaybeIn(Nothing) 99 | Proxy)
-- Val 99
--
-- >>> pl @(MaybeIn (FailP "someval") Succ) (Nothing @())
-- Error someval (MaybeIn(Nothing))
-- Fail "someval"
--
-- >>> pl @(MaybeIn 'True 'False) (Nothing @())
-- True (MaybeIn(Nothing) True | Proxy)
-- Val True
--
-- >>> pl @(MaybeIn 'True 'False) (Just "aa")
-- False (MaybeIn(Just) False | "aa")
-- Val False
--
-- >>> pl @(MaybeIn MEmptyP (Fst ==! Snd)) (Just ('x','z'))
-- Present LT (MaybeIn(Just) LT | ('x','z'))
-- Val LT
--
-- >>> pl @(MaybeIn MEmptyP (Fst ==! Snd)) (Nothing @(Char,Char))
-- Present EQ (MaybeIn(Nothing) EQ | Proxy)
-- Val EQ
--
-- >>> pl @(MaybeIn (FailP "failed20") 'False) (Nothing @Int)
-- Error failed20 (MaybeIn(Nothing))
-- Fail "failed20"
--
-- >>> pl @(MaybeIn ('False >> FailS "failed21") 'False) (Nothing @Double)
-- Error failed21 (False | MaybeIn(Nothing))
-- Fail "failed21"
--
-- >>> pl @(MaybeIn (FailP "err") Id) (Nothing @Int)
-- Error err (MaybeIn(Nothing))
-- Fail "err"
--
-- >>> pl @(MaybeIn (FailP "err") Id) (Nothing @())
-- Error err (MaybeIn(Nothing))
-- Fail "err"
--
-- >>> pl @(MaybeIn MEmptyP Id) (Just (M.fromList [(1,'a')]))
-- Present fromList [(1,'a')] (MaybeIn(Just) fromList [(1,'a')] | fromList [(1,'a')])
-- Val (fromList [(1,'a')])
--
-- >>> pl @(MaybeIn MEmptyP Id) (Nothing @(M.Map () ()))
-- Present fromList [] (MaybeIn(Nothing) fromList [] | Proxy)
-- Val (fromList [])
--
-- >>> pl @(MaybeIn MEmptyP Ones) (Just @String "abc")
-- Present ["a","b","c"] (MaybeIn(Just) ["a","b","c"] | "abc")
-- Val ["a","b","c"]
--
-- >>> pl @(MaybeIn 99 Id) (Just 12)
-- Present 12 (MaybeIn(Just) 12 | 12)
-- Val 12
--
-- >>> pl @(MaybeIn 99 Id) Nothing
-- Present 99 (MaybeIn(Nothing) 99 | Proxy)
-- Val 99
--
-- >>> pl @(MaybeIn (99 -% 1) Id) Nothing
-- Present (-99) % 1 (MaybeIn(Nothing) (-99) % 1 | Proxy)
-- Val ((-99) % 1)
--
-- >>> pl @(MaybeIn 123 Id) (Nothing @Int)
-- Present 123 (MaybeIn(Nothing) 123 | Proxy)
-- Val 123
--
-- >>> pl @(MaybeIn 123 Id) (Just 9)
-- Present 9 (MaybeIn(Just) 9 | 9)
-- Val 9
--
-- >>> pl @(Uncons >> MaybeIn '(1,MEmptyT _) Id) []
-- Present (1,[]) ((>>) (1,[]) | {MaybeIn(Nothing) (1,[]) | Proxy})
-- Val (1,[])
--
-- >>> pl @(MaybeIn MEmptyP (ShowP Id >> Ones)) (Just 123)
-- Present ["1","2","3"] (MaybeIn(Just) ["1","2","3"] | 123)
-- Val ["1","2","3"]
--
-- >>> pl @(MaybeIn MEmptyP (ShowP Id >> Ones)) (Nothing @String)
-- Present [] (MaybeIn(Nothing) [] | Proxy)
-- Val []
--
-- >>> pl @(MaybeIn MEmptyP Ones) (Just @String "ab")
-- Present ["a","b"] (MaybeIn(Just) ["a","b"] | "ab")
-- Val ["a","b"]
--
-- >>> pl @(MaybeIn MEmptyP Ones) (Nothing @String)
-- Present [] (MaybeIn(Nothing) [] | Proxy)
-- Val []
--
data MaybeIn p q deriving Show

-- tricky: the nothing case is the proxy of PP q a: ie proxy of the final result
instance ( P q a
         , Show a
         , Show (PP q a)
         , PP p (Proxy (PP q a)) ~ PP q a
         , P p (Proxy (PP q a))
         ) => P (MaybeIn p q) (Maybe a) where
  type PP (MaybeIn p q) (Maybe a) = PP q a
  eval _ opts ma = do
    let msg0 = "MaybeIn"
    case ma of
      Nothing -> do
        let msg1 = msg0 <> "(Nothing)"
        pp <- eval (Proxy @p) opts (Proxy @(PP q a))
        pure $ case getValueLR NoInline opts msg1 pp [] of
          Left e -> e
          Right b -> mkNodeCopy opts pp (msg1 <> " " <> showL opts b <> " | Proxy") [hh pp]
      Just a -> do
        let msg1 = msg0 <> "(Just)"
        qq <- eval (Proxy @q) opts a
        pure $ case getValueLR NoInline opts msg1 qq [] of
          Left e -> e
          Right b -> mkNodeCopy opts qq (show3 opts msg1 b a) [hh qq]

-- | similar to 'Data.Maybe.isJust'
--
-- >>> pz @IsJust Nothing
-- Val False
--
-- >>> pz @IsJust (Just 'a')
-- Val True
--
data IsJust deriving Show

instance x ~ Maybe a
         => P IsJust x where
  type PP IsJust x = Bool
  eval _ opts x = pure $ mkNodeB opts (isJust x) "IsJust" []

-- | similar to 'Data.Maybe.isNothing'
--
-- >>> pz @IsNothing (Just 123)
-- Val False
--
-- >>> pz @IsNothing Nothing
-- Val True
--
-- >>> pl @(Not IsNothing &&& ('Just Id >> Id + 12)) (Just 1)
-- Present (True,13) ('(True,13))
-- Val (True,13)
--
-- >>> pl @(Not IsNothing &&& ('Just Id >> Id + 12)) Nothing
-- Error 'Just(empty) ('(,))
-- Fail "'Just(empty)"
--
data IsNothing deriving Show

instance x ~ Maybe a
         => P IsNothing x where
  type PP IsNothing x = Bool
  eval _ opts x = pure $ mkNodeB opts (isNothing x) "IsNothing" []

-- | like 'Data.Maybe.mapMaybe'
--
-- >>> pl @(MapMaybe (MaybeBool (Le 3) Id) Id) [1..5]
-- Present [1,2,3] ((>>) [1,2,3] | {Concat [1,2,3] | [[1],[2],[3],[],[]]})
-- Val [1,2,3]
--
-- >>> pl @(MapMaybe (MaybeBool (Gt 3) Id) Id) [1..5]
-- Present [4,5] ((>>) [4,5] | {Concat [4,5] | [[],[],[],[4],[5]]})
-- Val [4,5]
--
data MapMaybe p q deriving Show
type MapMaybeT p q = ConcatMap (p >> MaybeIn MEmptyP '[Id]) q

instance P (MapMaybeT p q) x => P (MapMaybe p q) x where
  type PP (MapMaybe p q) x = PP (MapMaybeT p q) x
  eval _ = eval (Proxy @(MapMaybeT p q))

-- | similar to 'Data.Maybe.catMaybes'
--
-- >>> pl @CatMaybes [Just 'a',Nothing,Just 'c',Just 'd',Nothing]
-- Present "acd" ((>>) "acd" | {Concat "acd" | ["a","","c","d",""]})
-- Val "acd"
--
data CatMaybes deriving Show
type CatMaybesT = MapMaybe Id Id

instance P CatMaybesT x => P CatMaybes x where
  type PP CatMaybes x = PP CatMaybesT x
  eval _ = eval (Proxy @CatMaybesT)

-- | Convenient method to convert a value @p@ to a 'Maybe' based on a predicate @b@
-- if @b@ then Just @p@ else Nothing
--
-- >>> pz @(MaybeBool (Id > 4) Id) 24
-- Val (Just 24)
--
-- >>> pz @(MaybeBool (Id > 4) Id) (-5)
-- Val Nothing
--
-- >>> pz @(MaybeBool 'True 10) ()
-- Val (Just 10)
--

--    pu @(If 'True (MkJust 10) (EmptyT Maybe)) ()  -- doesnt work
--    <interactive>:211:1: error:
--       * Couldn't match type 'Int' with '()' arising from a use of 'pu'

data MaybeBool b p deriving Show

type MaybeBoolT b p = EmptyBool Maybe b p

instance P (MaybeBoolT b p) x => P (MaybeBool b p) x where
  type PP (MaybeBool b p) x = PP (MaybeBoolT b p) x
  eval _ = eval (Proxy @(MaybeBoolT b p))

-- | Convenient method to convert a value @p@ to an Alternative based on a predicate @b@
-- if @b@ then pure @p@ else empty
--
-- >>> pz @(EmptyBool [] (Id > 4) 'True) 24
-- Val [True]
--
-- >>> pz @(EmptyBool [] (Id > 4) 'True) 1
-- Val []
--
data EmptyBool t b p deriving Show

instance ( Show (PP p a)
         , P b a
         , P p a
         , PP b a ~ Bool
         , Alternative t
         ) => P (EmptyBool t b p) a where
  type PP (EmptyBool t b p) a = t (PP p a)
  eval _ opts z = do
    let msg0 = "EmptyBool"
    bb <- evalBool (Proxy @b) opts z
    case getValueLR NoInline opts (msg0 <> " b failed") bb [] of
      Left e -> pure e
      Right True -> do
        pp <- eval (Proxy @p) opts z
        pure $ case getValueLR NoInline opts (msg0 <> " p failed") pp [hh bb] of
          Left e -> e
          Right p -> mkNode opts (Val (pure p)) (msg0 <> "(False) Just " <> showL opts p) [hh bb, hh pp]
      Right False -> pure $ mkNode opts (Val empty) (msg0 <> "(True)") [hh bb]

-- | extract the value from a 'Maybe' otherwise use the default value: similar to 'Data.Maybe.fromMaybe'
--
-- >>> pz @(JustDef (1 % 4) Id) (Just 20.4)
-- Val (102 % 5)
--
-- >>> pz @(JustDef (1 % 4) Id) Nothing
-- Val (1 % 4)
--
-- >>> pz @(JustDef (MEmptyT _) Id) (Just "xy")
-- Val "xy"
--
-- >>> pz @(JustDef (MEmptyT _) Id) Nothing
-- Val ()
--
-- >>> pz @(JustDef (MEmptyT (SG.Sum _)) Id) Nothing
-- Val (Sum {getSum = 0})
--
-- >>> pl @(JustDef 0 Id) (Just 123)
-- Present 123 (JustDef Just)
-- Val 123
--
-- >>> pl @(JustDef 0 Id) Nothing
-- Present 0 (JustDef Nothing)
-- Val 0
--
-- >>> pl @(JustDef 99 Id) (Just 12)
-- Present 12 (JustDef Just)
-- Val 12
--
-- >>> pl @(JustDef 99 Id) Nothing
-- Present 99 (JustDef Nothing)
-- Val 99
--
-- >>> pl @(JustDef (99 -% 1) Id) Nothing
-- Present (-99) % 1 (JustDef Nothing)
-- Val ((-99) % 1)
--
-- >>> pl @(JustDef (MEmptyT _) Id) (Just (SG.Sum 123))
-- Present Sum {getSum = 123} (JustDef Just)
-- Val (Sum {getSum = 123})
--
-- >>> pl @(JustDef (MEmptyT _) Id) (Nothing @(SG.Sum _))
-- Present Sum {getSum = 0} (JustDef Nothing)
-- Val (Sum {getSum = 0})
--
data JustDef p q deriving Show

instance ( PP p x ~ a
         , PP q x ~ Maybe a
         , P p x
         , P q x
         )
    => P (JustDef p q) x where
  type PP (JustDef p q) x = MaybeT (PP q x)
  eval _ opts x = do
    let msg0 = "JustDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          Just b -> pure $ mkNode opts (Val b) (msg0 <> " Just") [hh qq]
          Nothing -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR NoInline opts msg0 pp [hh qq] of
              Left e -> e
              Right b -> mkNode opts (Val b) (msg0 <> " Nothing") [hh qq, hh pp]


-- | extract the value from a 'Maybe' or fail with the given message
--
-- >>> pz @(JustFail "nope" Id) (Just 99)
-- Val 99
--
-- >>> pz @(JustFail "nope" Id) Nothing
-- Fail "nope"
--
-- >>> pz @(JustFail (PrintF "oops=%d" Snd) Fst) (Nothing, 123)
-- Fail "oops=123"
--
-- >>> pz @(JustFail (PrintF "oops=%d" Snd) Fst) (Just 'x', 123)
-- Val 'x'
--
data JustFail p q deriving Show

instance ( PP p x ~ String
         , PP q x ~ Maybe a
         , P p x
         , P q x
         )
    => P (JustFail p q) x where
  type PP (JustFail p q) x = MaybeT (PP q x)
  eval _ opts x = do
    let msg0 = "JustFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          Just b -> pure $ mkNode opts (Val b) (msg0 <> " Just") [hh qq]
          Nothing -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR NoInline opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Fail p) (msg0 <> " Nothing") [hh qq, hh pp]
