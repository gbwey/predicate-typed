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
-- | promoted 'Maybe' functions
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
  , MaybeBool
  , MaybeIn
  , MaybeId

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Predicate.Data.Foldable (ConcatMap)
import Predicate.Data.Monoid (MEmptyP)
import Predicate.Data.Lifted (EmptyBool)
import Data.Proxy (Proxy(..))
import Data.Kind (Type)
import Data.Maybe (isJust, isNothing)
import GHC.TypeLits (ErrorMessage((:$$:),(:<>:)))
import qualified GHC.TypeLits as GL

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
type MapMaybeT p q = ConcatMap (p >> MaybeId MEmptyP '[Id]) q

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
data MaybeBool b p deriving Show

type MaybeBoolT b p = EmptyBool Maybe b p

instance P (MaybeBoolT b p) x => P (MaybeBool b p) x where
  type PP (MaybeBool b p) x = PP (MaybeBoolT b p) x
  eval _ = eval (Proxy @(MaybeBoolT b p))

-- | extract the value from a 'Maybe' otherwise use the default value: similar to 'Data.Maybe.fromMaybe'
--
-- >>> pl @(JustDef 'True Id) Nothing -- preserves TrueP/FalseP in the default case
-- True (JustDef Nothing)
-- Val True
--
-- >>> pl @(JustDef (Fst > 12) Snd) (3,Just False) -- ValP for normal case
-- Present False (JustDef Just)
-- Val False
--
-- >>> pl @(JustDef Fst Snd) (True,Nothing)
-- Present True (JustDef Nothing)
-- Val True
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
              Right _ -> mkNodeCopy opts pp (msg0 <> " Nothing") [hh qq]


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

-- | destructs an Maybe value
--   @n@ @Nothing@ receives @(PP s x,Proxy result)@ (you can use the proxy with MEmptyP)
--   @p@ @Just a@ receives @(PP s x,a)@
--   @s@ points to the environment you want to pass in
--   @t@ points to the Maybe value
--
-- >>> pz @(MaybeIn Fst Snd Fst Snd) ('a', Just 'x')
-- Val 'x'
--
-- >>> pz @(MaybeIn Fst Snd Fst Snd) ('a', Nothing)
-- Val 'a'
--
-- >>> pl @(MaybeIn "none" "just"() Id) (Just (SG.Sum 12))
-- Present "just" (MaybeIn(Just) "just" | Sum {getSum = 12})
-- Val "just"
--
-- >>> pl @(MaybeIn (Snd >> FailP "oops") Snd Fst Snd) ("abc", Nothing)
-- Error oops (Proxy | MaybeIn(Nothing) n failed)
-- Fail "oops"
--
-- >>> pl @(MaybeIn (Snd >> MEmptyP) Snd Fst Snd) ("abc", Nothing)
-- Present () (MaybeIn(Nothing) () | ())
-- Val ()
--
data MaybeIn n p s t deriving Show

instance ( Show a
         , Show (PP p (y,a))
         , P n (y,Proxy z)
         , P p (y,a)
         , PP n (y,Proxy z) ~ PP p (y,a)
         , z ~ PP p (y,a)
         , P s x
         , P t x
         , PP t x ~ Maybe a
         , PP s x ~ y
         )  => P (MaybeIn n p s t) x where
  type PP (MaybeIn n p s t) x = MaybeInT p (PP s x) (PP t x)
  eval _ opts x = do
    let msg0 = "MaybeIn"
    lr <- runPQ NoInline msg0 (Proxy @s) (Proxy @t) opts x []
    case lr of
      Left e -> pure e
      Right (s,t,ss,tt) -> do
         let hhs = [hh ss, hh tt]
         case t of
            Nothing -> do
              let msg1 = msg0 <> "(Nothing)"
              nn <- eval (Proxy @n) opts (s,Proxy @z)
              pure $ case getValueLR NoInline opts (msg1 <> " n failed") nn hhs of
                   Left e -> e
                   Right c -> mkNodeCopy opts nn (show3 opts msg1 c ()) hhs
            Just a -> do
              let msg1 = msg0 <> "(Just)"
              pp <- eval (Proxy @p) opts (s,a)
              pure $ case getValueLR NoInline opts (msg1 <> " p failed") pp hhs of
                   Left e -> e
                   Right c -> mkNodeCopy opts pp (show3 opts msg1 c a) hhs

type family MaybeInT p y ma where
  MaybeInT p y (Maybe a) = PP p (y,a)
  MaybeInT _ _ o = GL.TypeError (
      'GL.Text "MaybeInT: expected 'Maybe a' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | simple version of 'MaybeIn' with Id as the Maybe value and the environment set to ()
--
-- >>> pz @(MaybeId '("x","oops") '(Id,"fromjust")) (Just "ok")
-- Val ("ok","fromjust")
--
-- >>> pz @(MaybeId '("x","oops") '(Id,"fromjust")) Nothing
-- Val ("x","oops")
--
-- >>> pz @(MaybeId "found nothing" (ShowP Pred)) (Just 20)
-- Val "19"
--
-- >>> pz @(MaybeId "found nothing" (ShowP Pred)) Nothing
-- Val "found nothing"
--
-- >>> pl @(MaybeId 'True Id) Nothing
-- True (MaybeIn(Nothing) True | ())
-- Val True
--
-- >>> pl @(MaybeId 'True IdBool) (Just False)
-- False (MaybeIn(Just) False | False)
-- Val False
--
-- >>> pl @(MaybeId (FailT _ "failed4") Id) (Just 10)
-- Present 10 (MaybeIn(Just) 10 | 10)
-- Val 10
--
-- >>> pl @(MaybeId 'False Id) Nothing
-- False (MaybeIn(Nothing) False | ())
-- Val False
--
-- >>> pl @(MaybeId (FailT _ "err") Id) Nothing
-- Error err (Proxy | MaybeIn(Nothing) n failed)
-- Fail "err"
--
-- >>> pz @(MaybeId 99 Id) (Just 12)
-- Val 12
--
-- >>> pz @(MaybeId 99 Id) Nothing
-- Val 99
--
-- >>> pl @(MaybeId MEmptyP Ones) (Just "ab")
-- Present ["a","b"] (MaybeIn(Just) ["a","b"] | "ab")
-- Val ["a","b"]
--
-- >>> pl @(MaybeId MEmptyP Ones) Nothing
-- Present [] (MaybeIn(Nothing) [] | ())
-- Val []
--
-- >>> pl @(MaybeId MEmptyP (Fst ==! Snd)) (Just ('x','z'))
-- Present LT (MaybeIn(Just) LT | ('x','z'))
-- Val LT
--
-- >>> pl @(MaybeId MEmptyP (Fst ==! Snd)) (Nothing @(Char,Char))
-- Present EQ (MaybeIn(Nothing) EQ | ())
-- Val EQ
--
data MaybeId n p deriving Show

type MaybeIdT n p = MaybeIn (Snd >> n) (Snd >> p) () Id

instance P (MaybeIdT n p) x => P (MaybeId n p) x where
  type PP (MaybeId n p) x = PP (MaybeIdT n p) x
  eval _ = eval (Proxy @(MaybeIdT n p))

