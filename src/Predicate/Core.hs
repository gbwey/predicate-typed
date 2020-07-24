{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     Dsl for evaluating and displaying type level expressions
-}
module Predicate.Core (

 -- ** basic types
    I
  , Id
  , IdT
  , W
  , Msg
  , Hide

  -- ** display evaluation tree
  , pan
  , panv
  , pa
  , pu
  , pab
  , pub
  , pav
  , puv
  , pl
  , pz
  , run
  , runs

  , P(..)

  -- ** evaluation methods
  , runPQ
  , runPQBool
  , evalBool
  , evalBoolHide
  , evalHide
  , evalQuick
  , prtTree
  ) where
import Predicate.Util
import GHC.TypeLits (Symbol,Nat,KnownSymbol,KnownNat)
import Control.Lens ((&), (^.), (.~))
import Data.Proxy
import Data.Typeable
import Data.Kind (Type)
import Data.These (These(..))
import Data.Functor.Identity
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators

-- | This is the core class. Each instance of this class can be combined into a dsl using 'Predicate.Prelude.>>'
class P p a where
  type PP (p :: k) a :: Type -- PP is the output type
  eval :: MonadEval m
     => proxy p -- ^ proxy for the expression
     -> POpts  -- ^ display options
     -> a      -- ^ value
     -> m (TT (PP p a)) -- ^ returns a tree of results

-- | A specialised form of 'eval' that works only on predicates
evalBool :: ( MonadEval m
            , P p a
            , PP p a ~ Bool
            ) => proxy p
              -> POpts
              -> a
              -> m (TT (PP p a))
evalBool p opts a = fixBoolT <$> eval p opts a

evalQuick :: forall p i . P p i => i -> Either String (PP p i)
evalQuick i = getValLRFromTT (runIdentity (eval (Proxy @p) (getOptT @OL) i))

-- | identity function
--
-- >>> pz @I 23
-- PresentT 23
data I
instance P I a where
  type PP I a = a
  eval _ opts a =
    let msg0 = "I"
    in pure $ mkNode opts (PresentT a) msg0 []


-- | identity function that displays the input unlike 'I'
--
-- even more constraints than 'I' so we might need to add explicit type signatures
--
-- >>> pz @Id 23
-- PresentT 23
data Id
instance Show a => P Id a where
  type PP Id a = a
  eval _ opts a =
    let msg0 = "Id"
    in pure $ mkNode opts (PresentT a) (msg0 <> " " <> showL opts a) []


-- even more constraints than 'Id' so we might need to explicitly add types (Typeable)
-- | identity function that also displays the type information for debugging
--
-- >>> pz @IdT 23
-- PresentT 23
data IdT
instance ( Typeable a
         , Show a
         ) => P IdT a where
  type PP IdT a = a
  eval _ opts a =
    let msg0 = "IdT(" <> t <> ")"
        t = showT @a
    in pure $ mkNode opts (PresentT a) (msg0 <> " " <> showL opts a) []

-- | transparent predicate wrapper to make k of kind 'Type' so it can be in a promoted list (cant mix kinds) see 'Predicate.Core.Do'
--
-- >>> pz @'[W 123, Id] 99
-- PresentT [123,99]
--
-- >>> pz @'[W "abc", W "def", Id, Id] "ghi"
-- PresentT ["abc","def","ghi","ghi"]
--
data W (p :: k)
instance P p a => P (W p) a where
  type PP (W p) a = PP p a
  eval _ = eval (Proxy @(Msg "W " p))

-- | add a message to give more context to the evaluation tree
--
-- >>> pan @(Msg "[somemessage] " Id) 999
-- P [somemessage] Id 999
-- PresentT 999
--
-- >>> pan @(Msg Id 999) "info message:"
-- P info message:'999
-- PresentT 999
--
data Msg prt p

instance (P prt a
        , PP prt a ~ String
        , P p a
        ) => P (Msg prt p) a where
  type PP (Msg prt p) a = PP p a
  eval _ opts a = do
    pp <- eval (Proxy @prt) opts a
    case getValueLR opts "Msg" pp [] of
         Left e -> pure e
         Right msg -> prefixMsg msg <$> eval (Proxy @p) opts a

-- | run the expression \'p\' but remove the subtrees
data Hide p
-- type H p = Hide p -- doesnt work with %   -- unsaturated!

instance P p x => P (Hide p) x where
  type PP (Hide p) x = PP p x
  eval _ opts x = do
      tt <- eval (Proxy @p) opts x
      pure $ tt & tForest .~ []

-- | 'const' () function
--
-- >>> pz @() "Asf"
-- PresentT ()
--
instance P () a where
  type PP () a = ()
  eval _ opts _ =
    let msg0 = "()"
    in pure $ mkNode opts (PresentT ()) msg0 []

instance P (Proxy t) a where
  type PP (Proxy t) a = Proxy t
  eval _ opts _ =
    let msg0 = "Proxy"
    in pure $ mkNode opts (PresentT Proxy) msg0 []

-- Start non-Type kinds
-----------------------

-- | pulls the type level 'Bool' to the value level
--
-- >>> pz @'True "not used"
-- TrueT
--
-- >>> pz @'False ()
-- FalseT
instance GetBool b => P (b :: Bool) a where
  type PP b a = Bool
  eval _ opts _ =
    let b = getBool @b
    in pure $ mkNodeB opts b ("'" <> show b) []

-- | pulls the type level 'Symbol' to the value level as a 'GHC.Base.String'
--
-- >>> pz @"hello world" ()
-- PresentT "hello world"
instance KnownSymbol s => P (s :: Symbol) a where
  type PP s a = String
  eval _ opts _ =
    let s = symb @s
    in pure $ mkNode opts (PresentT s) ("'" <> litL opts ("\"" <> s <> "\"")) []

-- | run the predicates in a promoted 2-tuple; similar to 'Control.Arrow.&&&'
--
-- >>> pz @'(Id, 4) "hello"
-- PresentT ("hello",4)
--
instance ( P p a
         , P q a
--         , Show (PP p a)
--         , Show (PP q a)
         ) => P '(p,q) a where
  type PP '(p,q) a = (PP p a, PP q a)
  eval _ opts a = do
    let msg = "'(,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
       Left e -> e
       Right (p,q,pp,qq) ->
         mkNode opts (PresentT (p,q)) msg [hh pp, hh qq]
--         mkNode opts (PresentT (p,q)) ("'(" <> showL opts p <> ", " <> showL opts q <> ")") [hh pp, hh qq]

-- | run the predicates in a promoted 3-tuple
--
-- >>> pz @'(4, Id, "goodbye") "hello"
-- PresentT (4,"hello","goodbye")
--
-- >>> pan @'( 'True, 'False, 123) True
-- P '(,,)
-- |
-- +- True 'True
-- |
-- +- False 'False
-- |
-- `- P '123
-- PresentT (True,False,123)
--
instance (P p a
        , P q a
        , P r a
        ) => P '(p,q,r) a where
  type PP '(p,q,r) a = (PP p a, PP q a, PP r a)
  eval _ opts a = do
    let msg = "'(,,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
         let hhs0 = [hh pp, hh qq]
         rr <- eval (Proxy @r) opts a
         pure $ case getValueLR opts msg rr hhs0 of
           Left e -> e
           Right r ->
             let hhs1 = hhs0 <> [hh rr]
             in mkNode opts (PresentT (p,q,r)) msg hhs1

-- | run the predicates in a promoted 4-tuple
--
-- >>> pz @'(4, Id, "inj", 999) "hello"
-- PresentT (4,"hello","inj",999)
--
instance (P p a
        , P q a
        , P r a
        , P s a
        ) => P '(p,q,r,s) a where
  type PP '(p,q,r,s) a = (PP p a, PP q a, PP r a, PP s a)
  eval _ opts a = do
    let msg = "'(,,,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs0 = [hh pp, hh qq]
        lr1 <- runPQ msg (Proxy @r) (Proxy @s) opts a hhs0
        pure $ case lr1 of
          Left e -> e
          Right (r,s,rr,ss) ->
            let hhs1 = hhs0 ++ [hh rr, hh ss]
            in mkNode opts (PresentT (p,q,r,s)) msg hhs1

-- | run the predicates in a promoted 5-tuple
--
-- >>> pz @'(4, Id, "inj", 999, 'LT) "hello"
-- PresentT (4,"hello","inj",999,LT)
--
instance (P p a
        , P q a
        , P r a
        , P s a
        , P t a
        ) => P '(p,q,r,s,t) a where
  type PP '(p,q,r,s,t) a = (PP p a, PP q a, PP r a, PP s a, PP t a)
  eval _ opts a = do
    let msg = "'(,,,,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs0 = [hh pp, hh qq]
        lr1 <- runPQ msg (Proxy @r) (Proxy @s) opts a hhs0
        case lr1 of
          Left e -> pure e
          Right (r,s,rr,ss) -> do
            let hhs1 = hhs0 ++ [hh rr, hh ss]
            tt <- eval (Proxy @t) opts a
            pure $ case getValueLR opts msg tt hhs1 of
              Left e -> e
              Right t ->
                let hhs2 = hhs1 <> [hh tt]
                in mkNode opts (PresentT (p,q,r,s,t)) msg hhs2

-- | run the predicates in a promoted 6-tuple
--
-- >>> pz @'(4, Id, "inj", 999, 'LT, 1) "hello"
-- PresentT (4,"hello","inj",999,LT,1)
--
instance (P p a
        , P q a
        , P r a
        , P s a
        , P t a
        , P u a
        ) => P '(p,q,r,s,t,u) a where
  type PP '(p,q,r,s,t,u) a = (PP p a, PP q a, PP r a, PP s a, PP t a, PP u a)
  eval _ opts a = do
    let msg = "'(,,,,,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs0 = [hh pp, hh qq]
        lr1 <- runPQ msg (Proxy @r) (Proxy @s) opts a hhs0
        case lr1 of
          Left e -> pure e
          Right (r,s,rr,ss) -> do
            let hhs1 = hhs0 ++ [hh rr, hh ss]
            lr2 <- runPQ msg (Proxy @t) (Proxy @u) opts a hhs1
            pure $ case lr2 of
              Left e -> e
              Right (t,u,tt,uu) ->
                let hhs2 = hhs1 ++ [hh tt, hh uu]
                in mkNode opts (PresentT (p,q,r,s,t,u)) msg hhs2

-- | run the predicates in a promoted 7-tuple
--
-- >>> pz @'(4, Id, "inj", 999, 'LT, 1, 2) "hello"
-- PresentT (4,"hello","inj",999,LT,1,2)
--
instance (P p a
        , P q a
        , P r a
        , P s a
        , P t a
        , P u a
        , P v a
        ) => P '(p,q,r,s,t,u,v) a where
  type PP '(p,q,r,s,t,u,v) a = (PP p a, PP q a, PP r a, PP s a, PP t a, PP u a, PP v a)
  eval _ opts a = do
    let msg = "'(,,,,,,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs0 = [hh pp, hh qq]
        lr1 <- runPQ msg (Proxy @r) (Proxy @s) opts a hhs0
        case lr1 of
          Left e -> pure e
          Right (r,s,rr,ss) -> do
            let hhs1 = hhs0 ++ [hh rr, hh ss]
            lr2 <- runPQ msg (Proxy @t) (Proxy @u) opts a hhs1
            case lr2 of
              Left e -> pure e
              Right (t,u,tt,uu) -> do
                vv <- eval (Proxy @v) opts a
                let hhs2 = hhs1 ++ [hh tt, hh uu]
                pure $ case getValueLR opts msg vv hhs2 of
                  Left e -> e
                  Right v ->
                    let hhs3 = hhs2 ++ [hh vv]
                    in mkNode opts (PresentT (p,q,r,s,t,u,v)) msg hhs3

-- | run the predicates in a promoted 8-tuple
--
-- >>> pz @'(4, Id, "inj", 999, 'LT, 1, 2, 3) "hello"
-- PresentT (4,"hello","inj",999,LT,1,2,3)
--
instance (P p a
        , P q a
        , P r a
        , P s a
        , P t a
        , P u a
        , P v a
        , P w a
        ) => P '(p,q,r,s,t,u,v,w) a where
  type PP '(p,q,r,s,t,u,v,w) a = (PP p a, PP q a, PP r a, PP s a, PP t a, PP u a, PP v a, PP w a)
  eval _ opts a = do
    let msg = "'(,,,,,,,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs0 = [hh pp, hh qq]
        lr1 <- runPQ msg (Proxy @r) (Proxy @s) opts a hhs0
        case lr1 of
          Left e -> pure e
          Right (r,s,rr,ss) -> do
            let hhs1 = hhs0 ++ [hh rr, hh ss]
            lr2 <- runPQ msg (Proxy @t) (Proxy @u) opts a hhs1
            case lr2 of
              Left e -> pure e
              Right (t,u,tt,uu) -> do
                let hhs2 = hhs1 ++ [hh tt, hh uu]
                lr3 <- runPQ msg (Proxy @v) (Proxy @w) opts a hhs2
                pure $ case lr3 of
                  Left e -> e
                  Right (v,w,vv,ww) ->
                     let hhs3 = hhs2 ++ [hh vv, hh ww]
                     in mkNode opts (PresentT (p,q,r,s,t,u,v,w)) msg hhs3


-- | extracts the value level representation of the promoted 'Ordering'
--
-- >>> pz @'LT "not used"
-- PresentT LT
--
-- >>> pz @'EQ ()
-- PresentT EQ
instance GetOrdering cmp => P (cmp :: Ordering) a where
  type PP cmp a = Ordering
  eval _ opts _a =
    let cmp = getOrdering @cmp
        msg = "'" <> show cmp
    in pure $ mkNode opts (PresentT cmp) msg []

-- | extracts the value level representation of the type level 'Nat'
--
-- >>> pz @123 ()
-- PresentT 123
instance KnownNat n => P (n :: Nat) a where
  type PP n a = Int
  eval _ opts _ =
    let n = nat @n
    in pure $ mkNode opts (PresentT n) ("'" <> show n) []

-- | extracts the value level representation of the type level '()
--
-- >>> pz @'() ()
-- PresentT ()
instance P '() a where
  type PP '() a = ()
  eval _ opts _ = pure $ mkNode opts (PresentT ()) "'()" []

-- the type has to be [a] so we still need type PP '[p] a = [PP p a] to keep the types in line

-- | extracts the value level representation of the type level '[]
--
-- >>> pz @'[] False
-- PresentT []
instance P ('[] :: [k]) a where
  type PP ('[] :: [k]) a = [a]
  eval _ opts _ = pure $ mkNode opts (PresentT mempty) "'[]" []

-- | runs each predicate in turn from the promoted list
--
-- >>> pz @'[1, 2, 3] 999
-- PresentT [1,2,3]
--
-- >>> pz @'[W 1, W 2, W 3, Id] 999
-- PresentT [1,2,3,999]
--
instance ( Show (PP p a)
         , Show a
         , P p a
         ) => P '[p] a where
  type PP '[p] a = [PP p a]
  eval _ opts a = do
    pp <- eval (Proxy @p) opts a
    let msg0 = ""
    pure $ case getValueLR opts msg0 pp [] of
       Left e -> e
       Right b -> mkNode opts (PresentT [b]) ("'" <> showL opts [b] <> showVerbose opts " | " a) [hh pp]

instance (Show (PP p a)
        , Show a
        , P (p1 ': ps) a
        , PP (p1 ': ps) a ~ [PP p1 a]
        , P p a
        , PP p a ~ PP p1 a
        ) => P (p ': p1 ': ps) a where
  type PP (p ': p1 ': ps) a = [PP p a]
  eval _ opts a = do
    let msg0 = "'(p':q)"
    pp <- eval (Proxy @p) opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @(p1 ': ps)) opts a
        pure $ case getValueLR opts msg0 qq [hh pp] of
          Left e -> e
          Right q ->
            let ret = p:q
            -- no gap between ' and ret!
            in mkNode opts (PresentT ret) ("'" <> showL opts ret <> litVerbose opts " " (topMessage pp) <> showVerbose opts " | " a) ([hh pp | isVerbose opts] <> [hh qq])

-- | extracts the \'a\' from type level \'Maybe a\' if the value exists
--
-- >>> pz @('Just Id) (Just 123)
-- PresentT 123
--
-- >>> pz @('Just Id) (Just True)
-- PresentT True
--
-- >>> pz @('Just Id) Nothing
-- FailT "'Just found Nothing"
--
instance (Show (PP p a)
        , P p a
        , Show a
        ) => P ('Just p) (Maybe a) where
  type PP ('Just p) (Maybe a) = PP p a
  eval _ opts ma = do
    let msg0 = "'Just"
    case ma of
      Just a -> do
        pp <- eval (Proxy @p) opts a
        pure $ case getValueLR opts msg0 pp [] of
          Left e -> e
          Right b -> mkNode opts (PresentT b) (show01 opts msg0 b ma) [hh pp]
      Nothing -> pure $ mkNode opts (FailT (msg0 <> " found Nothing")) "" []

-- | expects Nothing otherwise it fails
-- if the value is Nothing then it returns \'Proxy a\' as this provides type information
--
-- >>> pz @'Nothing Nothing
-- PresentT Proxy
--
-- >>> pz @'Nothing (Just True)
-- FailT "'Nothing found Just"
--
instance P 'Nothing (Maybe a) where
  type PP 'Nothing (Maybe a) = Proxy a -- () gives us less information
  eval _ opts ma =
    let msg0 = "'Nothing"
    in pure $ case ma of
         Nothing -> mkNode opts (PresentT Proxy) msg0 []
         Just _ -> mkNode opts (FailT (msg0 <> " found Just")) "" []

-- omitted Show x so we can have less ambiguity
-- | extracts the \'a\' from type level \'Either a b\' if the value exists
--
-- >>> pz @('Left Id) (Left 123)
-- PresentT 123
--
-- >>> pz @('Left Id) (Right "aaa")
-- FailT "'Left found Right"
--
instance (Show a
        , Show (PP p a)
        , P p a
        ) => P ('Left p) (Either a x) where
  type PP ('Left p) (Either a x) = PP p a
  eval _ opts lr =
    let msg0 = "'Left"
    in case lr of
         Right _ -> pure $ mkNode opts (FailT (msg0 <> " found Right")) "" []
         Left a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg0 pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) (show01' opts msg0 b "Left " a) [hh pp]

-- | extracts the \'b\' from type level \'Either a b\' if the value exists
--
-- >>> pz @('Right Id) (Right 123)
-- PresentT 123
--
-- >>> pz @('Right Id) (Left "aaa")
-- FailT "'Right found Left"
--
instance (Show a
        , Show (PP p a)
        , P p a
        ) => P ('Right p) (Either x a) where
  type PP ('Right p) (Either x a) = PP p a
  eval _ opts lr = do
    let msg0 = "'Right"
    case lr of
         Left _ -> pure $ mkNode opts (FailT (msg0 <> " found Left")) "" []
         Right a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg0 pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) (show01' opts msg0 b "Right " a) [hh pp]

-- removed Show x: else ambiguity errors in TestPredicate

-- | extracts the \'a\' from type level \'These a b\' if the value exists
--
-- >>> pz @('This Id) (This 123)
-- PresentT 123
--
-- >>> pz @('This Id) (That "aaa")
-- FailT "'This found That"
--
-- >>> pz @('This Id) (These 999 "aaa")
-- FailT "'This found These"
--
instance (Show a
        , Show (PP p a)
        , P p a
        ) => P ('This p) (These a x) where
  type PP ('This p) (These a x) = PP p a
  eval _ opts th = do
    let msg0 = "'This"
    case th of
         This a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg0 pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) (show01' opts msg0 b "This " a) [hh pp]
         _ -> pure $ mkNode opts (FailT (msg0 <> " found " <> showThese th)) "" []

-- | extracts the \'b\' from type level \'These a b\' if the value exists
--
-- >>> pz @('That Id) (That 123)
-- PresentT 123
--
-- >>> pz @('That Id) (This "aaa")
-- FailT "'That found This"
--
-- >>> pz @('That Id) (These 44 "aaa")
-- FailT "'That found These"
--
instance (Show a
        , Show (PP p a)
        , P p a
        ) => P ('That p) (These x a) where
  type PP ('That p) (These x a) = PP p a
  eval _ opts th = do
    let msg0 = "'That"
    case th of
         That a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg0 pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) (show01' opts msg0 b "That " a) [hh pp]
         _ -> pure $ mkNode opts (FailT (msg0 <> " found " <> showThese th)) "" []


-- | extracts the (a,b) from type level 'These a b' if the value exists
--
-- >>> pz @('These Id Id) (These 123 "abc")
-- PresentT (123,"abc")
--
-- >>> pz @('These Id 5) (These 123 "abcde")
-- PresentT (123,5)
--
-- >>> pz @('These Id Id) (This "aaa")
-- FailT "'These found This"
--
-- >>> pz @('These Id Id) (That "aaa")
-- FailT "'These found That"
--
instance (Show a
        , Show b
        , P p a
        , P q b
        , Show (PP p a)
        , Show (PP q b)
        ) => P ('These p q) (These a b) where
  type PP ('These p q) (These a b) = (PP p a, PP q b)
  eval _ opts th = do
    let msg0 = "'These"
    case th of
         These a b -> do
            pp <- eval (Proxy @p) opts a
            case getValueLR opts msg0 pp [] of
               Left e -> pure e
               Right p -> do
                 qq <- eval (Proxy @q) opts b
                 pure $ case getValueLR opts (msg0 <> " q failed p=" <> showL opts p) qq [hh pp] of
                    Left e -> e
                    Right q ->
                      let ret =(p,q)
                      in  mkNode opts (PresentT ret) (show01 opts msg0 ret (These a b)) [hh pp, hh qq]
         _ -> pure $ mkNode opts (FailT (msg0 <> " found " <> showThese th)) "" []

-- | converts the value to the corresponding 'Proxy'
--
-- >>> pz @'Proxy 'x'
-- PresentT Proxy
--
instance Show a => P 'Proxy a where
  type PP 'Proxy a = Proxy a
  eval _ opts a =
    let b = Proxy @a
    in pure $ mkNode opts (PresentT b) ("'Proxy" <> showVerbose opts " | " a) []

-- | typelevel 'BoolT'
--
-- >>> pz @'TrueT ()
-- TrueT
--
-- >>> pz @'FalseT ()
-- FalseT
--
-- >>> pz @('PresentT 123) ()
-- PresentT False
--
-- >>> pz @('FailT '[]) ()
-- FailT "'FailT _"
--
instance GetBoolT x b => P (b :: BoolT x) a where
  type PP b a = Bool
  eval _ opts _ = do
    let ret = getBoolT @x @b
    pure $ case ret of
      Left b -> mkNodeB opts b (if b then "'TrueT" else "'FalseT") []
      Right True -> mkNode opts (PresentT False) "'PresentT _" []
      Right False -> mkNode opts (FailT "'FailT _") "BoolT" []

pan, panv, pa, pu, pl, pz, pab, pub, pav, puv
  :: forall p a
  . ( Show (PP p a)
    , P p a
    ) => a
      -> IO (BoolT (PP p a))
-- | skips the evaluation tree and just displays the end result
pz = run @OZ @p
-- | same as 'pz' but adds context to the end result
pl = run @OL @p
-- | displays the evaluation tree in plain text without colors
pan = run @OAN @p
-- | displays the evaluation tree in plain text without colors and verbose
panv = run @OANV @p
-- | displays the evaluation tree using colors without background colors
pa = run @OA @p
-- | displays the evaluation tree using background colors
pab = run @OAB @p
-- | 'pa' and verbose
pav = run @OAV @p
-- | display the evaluation tree using unicode and colors
-- @
--   pu @'(Id, "abc", 123) [1..4]
-- @
pu = run @OU @p
-- | displays the evaluation tree using unicode and colors with background colors
pub = run @OUB @p
-- | 'pu' and verbose
puv = run @OUV @p

-- | evaluate a typelevel expression (use type applications to pass in the options and the expression)
--
-- >>> run @OZ @Id 123
-- PresentT 123
--
-- >>> run @('OMsg "field1" ':# OL) @('Left Id) (Right 123)
-- field1 >>> Error 'Left found Right
-- FailT "'Left found Right"
--
-- >>> run @(OptTT '[ 'OMsg "test", OU, 'OEmpty, OL, 'OMsg "field2"]) @('FailT '[]) ()
-- test | field2 >>> Error 'FailT _ (BoolT)
-- FailT "'FailT _"
--
run :: forall opts p a
        . ( OptTC opts
          , Show (PP p a)
          , P p a)
        => a
        -> IO (BoolT (PP p a))
run a = do
  let opts = getOptT @opts
  pp <- eval (Proxy @p) opts a
  let r = pp ^. tBool
  putStr $ prtTree opts pp
  return r

-- | run expression with multiple options in a list
--
-- >>> runs @'[ OL, 'OMsg "field2"] @'( 'True, 'False) ()
-- field2 >>> Present (True,False) ('(,))
-- PresentT (True,False)
--
-- >>> runs @'[ 'OMsg "test", OU, 'OEmpty, OL, 'OMsg "field2"] @('FailT '[]) ()
-- test | field2 >>> Error 'FailT _ (BoolT)
-- FailT "'FailT _"
--
runs :: forall optss p a
        . ( OptTC (OptTT optss)
          , Show (PP p a)
          , P p a)
        => a
        -> IO (BoolT (PP p a))
runs = run @(OptTT optss) @p


prtTree :: Show x => POpts -> TT x -> String
prtTree opts pp =
  let r = pp ^. tBool
  in case oDebug opts of
       DZero -> ""
       DLite ->
             formatOMsg opts " >>> "
          <> colorBoolT opts r
          <> " "
          <> topMessage pp
          <> "\n"
       _ -> formatOMsg opts "\n"
         <> prtTreePure opts (fromTT pp)

runPQ :: ( P p a
         , P q a
         , MonadEval m)
   => String
   -> proxy1 p
   -> proxy2 q
   -> POpts
   -> a
   -> [Holder]
   -> m (Either (TT x) (PP p a, PP q a, TT (PP p a), TT (PP q a)))
runPQ msg0 proxyp proxyq opts a hhs = do
    pp <- eval proxyp opts a
    case getValueLR opts msg0 pp hhs of
      Left e -> pure $ Left e
      Right p -> do
         qq <- eval proxyq opts a
         pure $ case getValueLR opts msg0 qq (hhs <> [hh pp]) of
           Left e -> Left e
           Right q -> Right (p, q, pp, qq)

runPQBool :: ( P p a
             , PP p a ~ Bool
             , P q a
             , PP q a ~ Bool, MonadEval m)
   => String
   -> proxy1 p
   -> proxy2 q
   -> POpts
   -> a
   -> [Holder]
   -> m (Either (TT x) (PP p a, PP q a, TT (PP p a), TT (PP q a)))
runPQBool msg0 proxyp proxyq opts a hhs = do
    pp <- evalBool proxyp opts a
    case getValueLR opts msg0 pp hhs of
      Left e -> pure $ Left e
      Right p -> do
         qq <- evalBool proxyq opts a
         pure $ case getValueLR opts msg0 qq (hhs <> [hh pp]) of
           Left e -> Left e
           Right q -> Right (p, q, pp, qq)

evalBoolHide :: forall m p a proxy
  . (MonadEval m, P p a, PP p a ~ Bool)
  => proxy p
  -> POpts
  -> a
  -> m (TT (PP p a))
evalBoolHide _ opts =
  if isVerbose opts then evalBool (Proxy @p) opts
  else evalBool (Proxy @(Hide p)) opts

evalHide :: forall m p a proxy
  . (MonadEval m, P p a)
  => proxy p
  -> POpts
  -> a
  -> m (TT (PP p a))
evalHide _ opts =
  if isVerbose opts then eval (Proxy @p) opts
  else eval (Proxy @(Hide p)) opts