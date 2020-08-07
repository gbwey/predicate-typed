{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
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
    Id
  , IdT
  , I
  , W
  , Msg
  , MsgI
  , Hide
  , Width
  , Hole
  , Unproxy
  , Len
  , Length
  , Map
  , Do
  , Pure
  , Coerce
  , OneP
  , type (>>)

  -- ** tree evaluation
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

 -- ** wrap, unwrap expressions
  , Unwrap
  , Wrap
  , Wrap'

 -- ** failure expressions
  , Fail
  , Failp
  , Failt
  , FailS

 -- ** tuple expressions
  , Fst
  , Snd
  , Thd
  , L1
  , L2
  , L3
  , L4
  , L5
  , L6

  -- ** boolean expressions
  , type (&&)
  , type (&&~)
  , type (||)
  , type (||~)
  , type (~>)
  , Not
  , Between
  , All
  , Any
  , IdBool

 -- ** miscellaneous
  , type (<..>)
  , type (<<)
  , Swap
  , SwapC(..)
  , type ($)
  , type (&)

  ) where
import Predicate.Util
import qualified GHC.TypeLits as GL
import GHC.TypeLits (Symbol,Nat,KnownSymbol,KnownNat)
import Control.Lens -- ((&), (^.), (.~))
import Data.Foldable (toList)
import Data.Proxy
import Data.Typeable
import Data.Kind (Type)
import Data.These (These(..))
import Control.Monad
import Data.List
import Data.Coerce
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG
-- >>> import Data.Time

-- | This is the core class. Each instance of this class can be combined into a dsl using 'Predicate.Core.>>'
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

-- | identity function without show instance of 'Id'
--
-- >>> pz @I 23
-- PresentT 23
--
data I
instance P I a where
  type PP I a = a
  eval _ opts a =
    let msg0 = "I"
    in pure $ mkNode opts (PresentT a) msg0 []


-- | identity function
--
-- >>> pz @Id 23
-- PresentT 23
--
data Id
instance Show a => P Id a where
  type PP Id a = a
  eval _ opts a =
    let msg0 = "Id"
    in pure $ mkNode opts (PresentT a) (msg0 <> " " <> showL opts a) []

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
  eval _ = eval (Proxy @(MsgI "W " p))

-- | add a message to give more context to the evaluation tree
--
-- >>> pan @(Msg "[somemessage]" Id) 999
-- P [somemessage] Id 999
-- PresentT 999
--
-- >>> pan @(Msg Id 999) "info message:"
-- P info message: '999
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
         Right msg -> prefixMsg (setOtherEffects opts msg <> " ") <$> eval (Proxy @p) opts a

-- | add a message to give more context to the evaluation tree
--
-- >>> pan @(MsgI "[somemessage] " Id) 999
-- P [somemessage] Id 999
-- PresentT 999
--
-- >>> pan @(MsgI Id 999) "info message:"
-- P info message:'999
-- PresentT 999
--
data MsgI prt p

instance (P prt a
        , PP prt a ~ String
        , P p a
        ) => P (MsgI prt p) a where
  type PP (MsgI prt p) a = PP p a
  eval _ opts a = do
    pp <- eval (Proxy @prt) opts a
    case getValueLR opts "MsgI" pp [] of
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

data Hole (t :: Type)

-- | Acts as a proxy in this dsl where you can explicitly set the Type.
--
--  It is passed around as an argument to help the type checker when needed.
--
instance Typeable t => P (Hole t) a where
  type PP (Hole t) a = t -- can only be Type not Type -> Type (can use Proxy but then we go down the rabbithole)
  eval _ opts _a =
    let msg0 = "Hole(" <> showT @t <> ")"
    in pure $ mkNode opts (FailT msg0) "you probably meant to get access to the type of PP only and not evaluate" []

-- | override the display width for the expression \'p\'
data Width (n :: Nat) p

instance (KnownNat n
        , P p a
        ) => P (Width n p) a where
  type PP (Width n p) a = PP p a
  eval _ opts a = do
    let opts' = opts { oWidth = nat @n }
    eval (Proxy @p) opts' a

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

-- | pulls the type level 'GHC.TypeLits.Symbol' to the value level as a 'GHC.Base.String'
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
         , Show (PP p a)
         , Show (PP q a)
         ) => P '(p,q) a where
  type PP '(p,q) a = (PP p a, PP q a)
  eval _ opts a = do
    let msg = "'(,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
       Left e -> e
       Right (p,q,pp,qq) ->
--         mkNode opts (PresentT (p,q)) msg [hh pp, hh qq]
         mkNode opts (PresentT (p,q)) ("'(" <> showL opts p <> "," <> showL opts q <> ")") [hh pp, hh qq]

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
--
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

-- | tries to extract @a@ from @Maybe a@ otherwise it fails: similar to 'Data.Maybe.fromJust'
--
-- >>> pz @('Just Id) (Just "abc")
-- PresentT "abc"
--
-- >>> pl @('Just Id >> Id) (Just 123)
-- Present 123 ((>>) 123 | {Id 123})
-- PresentT 123
--
-- >>> pl @('Just Id) (Just [1,2,3])
-- Present [1,2,3] ('Just [1,2,3] | Just [1,2,3])
-- PresentT [1,2,3]
--
-- >>> pl @('Just Id) (Just 10)
-- Present 10 ('Just 10 | Just 10)
-- PresentT 10
--
-- >>> pl @('Just Id) Nothing
-- Error 'Just(empty)
-- FailT "'Just(empty)"
--
-- >>> pz @('Just (Fst Id)) (Just 123,'x')
-- PresentT 123
--
instance (Show a
        , PP p x ~ Maybe a
        , P p x
        ) => P ('Just p) x where
  type PP ('Just p) x = MaybeT (PP p x)
  eval _ opts x = do
    let msg0 = "'Just"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          Nothing -> mkNode opts (FailT (msg0 <> "(empty)")) "" [hh pp]
          Just d -> mkNode opts (PresentT d) (show01 opts msg0 d p) [hh pp]

-- | expects Nothing otherwise it fails
--   if the value is Nothing then it returns \'Proxy a\' as this provides type information
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
-- >>> pz @('Left (Snd Id)) ('x', Left 123)
-- PresentT 123
--
-- >>> pz @('Left Id) (Right "aaa")
-- FailT "'Left found Right"
--
-- >>> pl @('Left Id) (Left 123)
-- Present 123 (Left)
-- PresentT 123
--
-- >>> pl @('Left Id) (Right 123)
-- Error 'Left found Right
-- FailT "'Left found Right"
--

instance ( PP p x ~ Either a b
         , P p x)
    => P ('Left p) x where
  type PP ('Left p) x = LeftT (PP p x)
  eval _ opts x = do
    let msg0 = "'Left"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          Left a -> mkNode opts (PresentT a) "Left" [hh pp]
          Right _b -> mkNode opts (FailT (msg0 <> " found Right")) "" [hh pp]

-- | extracts the \'b\' from type level \'Either a b\' if the value exists
--
-- >>> pl @('Right Id) (Right 123)
-- Present 123 (Right)
-- PresentT 123
--
-- >>> pz @('Right Id >> Snd Id) (Right ('x',123))
-- PresentT 123
--
-- >>> pz @('Right Id) (Left "aaa")
-- FailT "'Right found Left"
--
-- >>> pl @('Right Id) (Left 123)
-- Error 'Right found Left
-- FailT "'Right found Left"
--
instance ( PP p x ~ Either a b
         , P p x)
    => P ('Right p) x where
  type PP ('Right p) x = RightT (PP p x)
  eval _ opts x = do
    let msg0 = "'Right"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          Left _a -> mkNode opts (FailT (msg0 <> " found Left")) "" [hh pp]
          Right b -> mkNode opts (PresentT b) "Right" [hh pp]


-- removed Show x: else ambiguity errors in TestPredicate

-- | extracts the \'a\' from type level \'These a b\' if the value exists
--
-- >>> pl @('This Id) (This 12)
-- Present 12 (This)
-- PresentT 12
--
-- >>> pz @('This Id) (That "aaa")
-- FailT "'This found That"
--
-- >>> pz @('This Id) (These 999 "aaa")
-- FailT "'This found These"
--
-- >>> pl @('This Id) (That 12)
-- Error 'This found That
-- FailT "'This found That"
--

instance ( PP p x ~ These a b
         , P p x)
    => P ('This p) x where
  type PP ('This p) x = ThisT (PP p x)
  eval _ opts x = do
    let msg0 = "'This"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          This a -> mkNode opts (PresentT a) "This" [hh pp]
          That _b -> mkNode opts (FailT (msg0 <> " found That")) "" [hh pp]
          These _a _b -> mkNode opts (FailT (msg0 <> " found These")) "" [hh pp]

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

instance ( PP p x ~ These a b
         , P p x)
    => P ('That p) x where
  type PP ('That p) x = ThatT (PP p x)
  eval _ opts x = do
    let msg0 = "'That"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          This _a -> mkNode opts (FailT (msg0 <> " found This")) "" [hh pp]
          That b -> mkNode opts (PresentT b) "That" [hh pp]
          These _a _b -> mkNode opts (FailT (msg0 <> " found These")) "" [hh pp]


-- | extracts the (a,b) from type level \'These a b\' if the value exists
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
-- field2 >>> Present (True,False) ('(True,False))
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

-- | convenience method to evaluate two expressions using the same input and return the results
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

-- | convenience method to evaluate two boolean expressions using the same input and return the results
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

-- | evaluate a boolean expressions but hide the results unless verbose
evalBoolHide :: forall p a m
  . (MonadEval m, P p a, PP p a ~ Bool)
  => POpts
  -> a
  -> m (TT (PP p a))
evalBoolHide opts =
  if isVerbose opts then evalBool (Proxy @p) opts
  else evalBool (Proxy @(Hide p)) opts

-- | evaluate a expressions but hide the results unless verbose
evalHide :: forall p a m
  . (MonadEval m, P p a)
  => POpts
  -> a
  -> m (TT (PP p a))
evalHide opts =
  if isVerbose opts then eval (Proxy @p) opts
  else eval (Proxy @(Hide p)) opts


-- advantage of (>>) over 'Do [k] is we can use different kinds for (>>) without having to wrap with 'W'

-- | compose expressions
--
-- >>> pz @(Fst Id >> Snd Id) ((11,12),'x')
-- PresentT 12
--
data p >> q
infixr 1 >>

instance (Show (PP p a)
        , Show (PP q (PP p a))
        , P p a
        , P q (PP p a)
        ) => P (p >> q) a where
  type PP (p >> q) a = PP q (PP p a)
  eval _ opts a = do
    let msg0 = "(>>)"
    pp <- eval (Proxy @p) opts a
    case getValueLR opts "(>>) lhs failed" pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @q) opts p
        pure $ case getValueLR opts (show p <> " (>>) rhs failed") qq [hh pp] of
          Left e -> e
          Right q -> mkNode opts (_tBool qq) (lit01 opts msg0 q "" (topMessageEgregious qq)) [hh pp, hh qq]

-- | flipped version of 'Predicate.Core.>>'
data p << q
type LeftArrowsT p q = q >> p
infixr 1 <<

instance P (LeftArrowsT p q) x => P (p << q) x where
  type PP (p << q) x = PP (LeftArrowsT p q) x
  eval _ = eval (Proxy @(LeftArrowsT p q))

-- bearbeiten! only used by >>
topMessageEgregious :: TT a -> String
topMessageEgregious pp = innermost (pp ^. tString)
  where innermost = ('{':) . reverse . ('}':) . takeWhile (/='{') . dropWhile (=='}') . reverse

-- | unwraps a value (see '_Wrapped'')
--
-- >>> pz @(Unwrap Id) (SG.Sum (-13))
-- PresentT (-13)
--
-- >>> pl @(Unwrap Id >> '(Id, 'True)) (SG.Sum 13)
-- Present (13,True) ((>>) (13,True) | {'(13,True)})
-- PresentT (13,True)
--
data Unwrap p

instance (PP p x ~ s
        , P p x
        , Show s
        , Show (Unwrapped s)
        , Wrapped s
        ) => P (Unwrap p) x where
  type PP (Unwrap p) x = Unwrapped (PP p x)
  eval _ opts x = do
    let msg0 = "Unwrap"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = p ^. _Wrapped'
        in mkNode opts (PresentT d) (show01 opts msg0 d p) [hh pp]

data Wrap' t p

instance (Show (PP p x)
        , P p x
        , Unwrapped (PP s x) ~ PP p x
        , Wrapped (PP s x)
        , Show (PP s x)
        ) => P (Wrap' s p) x where
  type PP (Wrap' s p) x = PP s x
  eval _ opts x = do
    let msg0 = "Wrap"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = p ^. _Unwrapped'
        in mkNode opts (PresentT d) (show01 opts msg0 d p) [hh pp]

-- | wraps a value (see '_Wrapped'' and '_Unwrapped'')
--
-- >>> pz @(Wrap (SG.Sum _) Id) (-13)
-- PresentT (Sum {getSum = -13})
--
-- >>> pz @(Wrap SG.Any (Ge 4)) 13
-- PresentT (Any {getAny = True})
--
-- >>> import Data.List.NonEmpty (NonEmpty(..))
-- >>> pz @(Wrap (NonEmpty _) (Uncons >> 'Just Id)) "abcd"
-- PresentT ('a' :| "bcd")
--
-- >>> pl @(Wrap (SG.Sum _) Id) 13
-- Present Sum {getSum = 13} (Wrap Sum {getSum = 13} | 13)
-- PresentT (Sum {getSum = 13})
--
-- >>> pl @(Wrap (SG.Sum _) Id >> STimes 4 Id) 13
-- Present Sum {getSum = 52} ((>>) Sum {getSum = 52} | {getSum = 13})
-- PresentT (Sum {getSum = 52})
--
-- >>> pl @(Wrap _ 13 <> Id) (SG.Sum @Int 12)
-- Present Sum {getSum = 25} (Sum {getSum = 13} <> Sum {getSum = 12} = Sum {getSum = 25})
-- PresentT (Sum {getSum = 25})
--

data Wrap (t :: Type) p
type WrapT (t :: Type) p = Wrap' (Hole t) p

instance P (WrapT t p) x => P (Wrap t p) x where
  type PP (Wrap t p) x = PP (WrapT t p) x
  eval _ = eval (Proxy @(WrapT t p))


-- | used for type inference
data Unproxy

instance Typeable a => P Unproxy (Proxy (a :: Type)) where
  type PP Unproxy (Proxy a) = a
  eval _ opts _a =
    let msg0 = "Unproxy(" <> showT @a <> ")"
    in pure $ mkNode opts (FailT msg0) "you probably meant to get access to the type of PP only and not evaluate" []

-- | similar to 'length'
--
-- >>> pz @Len [10,4,5,12,3,4]
-- PresentT 6
--
-- >>> pz @Len []
-- PresentT 0
--
data Len
instance ( Show a
         , as ~ [a]
         ) => P Len as where
  type PP Len as = Int
  eval _ opts as =
    let msg0 = "Len"
        n = length as
    in pure $ mkNode opts (PresentT n) (show01 opts msg0 n as) []

-- | similar to 'length' for 'Foldable' instances
--
-- >>> pz @(Length Id) (Left "aa")
-- PresentT 0
--
-- >>> pz @(Length Id) (Right "aa")
-- PresentT 1
--
-- >>> pz @(Length Right') (Right "abcd")
-- PresentT 4
--
-- >>> pz @(Length (Thd (Snd Id))) (True,(23,'x',[10,9,1,3,4,2]))
-- PresentT 6
--
data Length p

instance (PP p x ~ t a
        , P p x
        , Show (t a)
        , Foldable t) => P (Length p) x where
  type PP (Length p) x = Int
  eval _ opts x = do
    let msg0 = "Length"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
            let n = length p
            in mkNode opts (PresentT n) (show01 opts msg0 n p) [hh pp]

-- | 'not' function
--
-- >>> pz @(Not Id) False
-- TrueT
--
-- >>> pz @(Not Id) True
-- FalseT
--
-- >>> pz @(Not (Fst Id)) (True,22)
-- FalseT
--
-- >>> pl @(Not (Lt 3)) 13
-- True (Not (13 < 3))
-- TrueT
--
-- >>> pl @(Not 'True) ()
-- False (Not ('True))
-- FalseT
--
data Not p

instance ( PP p x ~ Bool
         , P p x
         ) => P (Not p) x where
  type PP (Not p) x = Bool
  eval _ opts x = do
    let msg0 = "Not"
    pp <- evalBool (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = not p
        in mkNodeB opts b (msg0 <> litVerbose opts " " (topMessage pp)) [hh pp]

-- | 'id' function on a boolean
--
-- >>> pz @(IdBool Id) False
-- FalseT
--
-- >>> pz @(IdBool Id) True
-- TrueT
--
-- >>> pz @(IdBool (Fst Id)) (True,22)
-- TrueT
--
-- >>> pl @(IdBool (Lt 3)) 13
-- False (IdBool (13 < 3))
-- FalseT
--
data IdBool p

instance ( PP p x ~ Bool
         , P p x
         ) => P (IdBool p) x where
  type PP (IdBool p) x = Bool
  eval _ opts x = do
    let msg0 = "IdBool"
    pp <- evalBool (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = p
        in mkNodeB opts b (msg0 <> litVerbose opts " " (topMessage pp)) [hh pp]

-- | Fails the computation with a message but allows you to set the output type
--
-- >>> pz @(Failt Int (PrintF "value=%03d" Id)) 99
-- FailT "value=099"
--
-- >>> pz @('False || (Fail 'True "failed")) (99,"somedata")
-- FailT "failed"
--
-- >>> pz @('False || (Fail (Hole Bool) "failed")) (99,"somedata")
-- FailT "failed"
--
-- >>> pz @('False || (Fail (Hole _) "failed")) (99,"somedata")
-- FailT "failed"
--
data Fail t prt

instance (P prt a
        , PP prt a ~ String
        ) => P (Fail t prt) a where
  type PP (Fail t prt) a = PP t a
  eval _ opts a = do
    let msg0 = "Fail"
    pp <- eval (Proxy @prt) opts a
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right s -> mkNode opts (FailT s) (msg0 <> " " <> s) [hh pp | isVerbose opts]

-- | Fails the computation with a message for simple failures: doesnt preserve types
--
-- >>> pz @(FailS (PrintT "value=%03d string=%s" Id)) (99,"somedata")
-- FailT "value=099 string=somedata"
--
data FailS p
instance P (Fail I p) x => P (FailS p) x where
  type PP (FailS p) x = PP (Fail I p) x
  eval _ = eval (Proxy @(Fail I p))

-- | Fails the computation with a message (wraps the type in 'Hole')
--
-- >>> pz @(Failt Int (PrintF "value=%03d" Id)) 99
-- FailT "value=099"
--
data Failt (t :: Type) p
instance P (Fail (Hole t) p) x => P (Failt t p) x where
  type PP (Failt t p) x = PP (Fail (Hole t) p) x
  eval _ = eval (Proxy @(Fail (Hole t) p))

-- | Fails the computation with a message where the input value is a Proxy
--
-- >>> pz @(Ix 3 (Failp "oops")) "abcd"
-- PresentT 'd'
--
-- >>> pz @(Ix 3 (Failp "oops")) "abc"
-- FailT "oops"
--
data Failp p
instance P (Fail Unproxy p) x => P (Failp p) x where
  type PP (Failp p) x = PP (Fail Unproxy p) x
  eval _ = eval (Proxy @(Fail Unproxy p))

-- | gets the singleton value from a foldable
--
-- >>> pl @(OneP Id) [10..15]
-- Error OneP 6 elements (expected one element)
-- FailT "OneP 6 elements"
--
-- >>> pl @(OneP Id) [10]
-- Present 10 (OneP)
-- PresentT 10
--
-- >>> pl @(OneP Id) []
-- Error OneP empty (expected one element)
-- FailT "OneP empty"
--
-- >>> pl @(OneP Id) (Just 10)
-- Present 10 (OneP)
-- PresentT 10
--
-- >>> pl @(OneP Id) Nothing
-- Error OneP empty (expected one element)
-- FailT "OneP empty"
--
-- >>> pl @(OneP Id) [12]
-- Present 12 (OneP)
-- PresentT 12
--
-- >>> pl @(OneP Id) [1..5]
-- Error OneP 5 elements (expected one element)
-- FailT "OneP 5 elements"
--
-- >>> pl @(OneP Id) ([] ::[()])
-- Error OneP empty (expected one element)
-- FailT "OneP empty"
--

data OneP p
instance (Foldable t
        , PP p x ~ t a
        , P p x
        ) => P (OneP p) x where
  type PP (OneP p) x = ExtractAFromTA (PP p x)
  eval _ opts x = do
    let msg0 = "OneP"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p -> case toList p of
                   [] -> mkNode opts (FailT (msg0 <> " empty")) "expected one element" [hh pp]
                   [a] -> mkNode opts (PresentT a) msg0 [hh pp]
                   as -> let n = length as
                         in mkNode opts (FailT (msg0 <> " " <> show n <> " elements")) "expected one element" [hh pp]

--type OneP = Guard "expected list of length 1" (Len == 1) >> Head Id
--type OneP = Guard (PrintF "expected list of length 1 but found length=%d" Len) (Len == 1) >> Head Id

-- | A predicate that determines if the value is between \'p\' and \'q\'
--
-- >>> pz @(Between 5 8 Len) [1,2,3,4,5,5,7]
-- TrueT
--
-- >>> pl @(Between 5 8 Id) 9
-- False (9 <= 8)
-- FalseT
--
-- >>> pl @(Between (Fst Id >> Fst Id) (Fst Id >> Snd Id) (Snd Id)) ((1,4),3)
-- True (1 <= 3 <= 4)
-- TrueT
--
-- >>> pl @(Between (Fst Id >> Fst Id) (Fst Id >> Snd Id) (Snd Id)) ((1,4),10)
-- False (10 <= 4)
-- FalseT
--
data Between p q r -- reify as it is used a lot! nicer specific messages at the top level!

instance (Ord (PP p x)
       , Show (PP p x)
       , PP r x ~ PP p x
       , PP r x ~ PP q x
       , P p x
       , P q x
       , P r x
       ) => P (Between p q r) x where
  type PP (Between p q r) x = Bool
  eval _ opts x = do
    let msg0 = "Between"
    rr <- eval (Proxy @r) opts x
    case getValueLR opts msg0 rr [] of
      Left e -> pure e
      Right r -> do
        lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x [hh rr]
        pure $ case lr of
          Left e -> e
          Right (p,q,pp,qq) ->
            let hhs = [hh rr, hh pp, hh qq]
            in if p <= r && r <= q then mkNodeB opts True (showL opts p <> " <= " <> showL opts r <> " <= " <> showL opts q) hhs
               else if p > r then mkNodeB opts False (showL opts p <> " <= " <> showL opts r) hhs
               else mkNodeB opts False (showL opts r <> " <= " <> showL opts q) hhs


-- | A operator predicate that determines if the value is between \'p\' and \'q\'
--
-- >>> pz @(5 <..> 8) 6
-- TrueT
--
-- >>> pz @(10 % 4 <..> 40 % 5) 4
-- TrueT
--
-- >>> pz @(10 % 4 <..> 40 % 5) 33
-- FalseT
--
data p <..> q
infix 4 <..>

type BetweenT p q = Between p q Id

instance P (BetweenT p q) x => P (p <..> q) x where
  type PP (p <..> q) x = PP (BetweenT p q) x
  eval _ = evalBool (Proxy @(BetweenT p q))

-- | similar to 'all'
--
-- >>> pl @(All (Between 1 8 Id) Id) [7,3,4,1,2,9,0,1]
-- False (All(8) i=5 (9 <= 8))
-- FalseT
--
-- >>> pz @(All Odd Id) [1,5,11,5,3]
-- TrueT
--
-- >>> pz @(All Odd Id) []
-- TrueT
--
-- >>> run @'OANV @(All Even Id) [1,5,11,5,3]
-- False All(5) i=0 (1 == 0)
-- |
-- +- P Id [1,5,11,5,3]
-- |
-- +- False i=0: 1 == 0
-- |  |
-- |  +- P 1 `mod` 2 = 1
-- |  |  |
-- |  |  +- P I
-- |  |  |
-- |  |  `- P '2
-- |  |
-- |  `- P '0
-- |
-- +- False i=1: 1 == 0
-- |  |
-- |  +- P 5 `mod` 2 = 1
-- |  |  |
-- |  |  +- P I
-- |  |  |
-- |  |  `- P '2
-- |  |
-- |  `- P '0
-- |
-- +- False i=2: 1 == 0
-- |  |
-- |  +- P 11 `mod` 2 = 1
-- |  |  |
-- |  |  +- P I
-- |  |  |
-- |  |  `- P '2
-- |  |
-- |  `- P '0
-- |
-- +- False i=3: 1 == 0
-- |  |
-- |  +- P 5 `mod` 2 = 1
-- |  |  |
-- |  |  +- P I
-- |  |  |
-- |  |  `- P '2
-- |  |
-- |  `- P '0
-- |
-- `- False i=4: 1 == 0
--    |
--    +- P 3 `mod` 2 = 1
--    |  |
--    |  +- P I
--    |  |
--    |  `- P '2
--    |
--    `- P '0
-- FalseT
--
-- >>> pl @(All (Gt 3) (Fst Id)) ([10,12,3,5],"ss")
-- False (All(4) i=2 (3 > 3))
-- FalseT
--
-- >>> pl @(All (Lt 3) Id) [1::Int .. 10]
-- False (All(10) i=2 (3 < 3))
-- FalseT
--
data All p q

instance (P p a
        , PP p a ~ Bool
        , PP q x ~ f a
        , P q x
        , Show a
        , Foldable f
        ) => P (All p q) x where
  type PP (All p q) x = Bool
  eval _ opts x = do
    let msg0 = "All"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case chkSize opts msg0 q [hh qq] of
          Left e -> pure e
          Right () -> do
            ts <- zipWithM (\i a -> ((i, a),) <$> evalBoolHide @p opts a) [0::Int ..] (toList q)
            pure $ case splitAndAlign opts msg0 ts of
                 Left e -> e
                 Right abcs ->
                   let hhs = hh qq : map (hh . fixit) ts
                       msg1 = msg0 ++ "(" ++ show (length q) ++ ")"
                   in case find (not . view _1) abcs of
                        Nothing -> mkNodeB opts True msg1 hhs
                        Just (_,(i,_),tt) ->
                          mkNodeB opts False (msg1 <> " i=" ++ showIndex i ++ " " <> topMessage tt) hhs

-- | similar to 'any'
--
-- >>> pl @(Any Even Id) [1,5,11,5,3]
-- False (Any(5))
-- FalseT
--
-- >>> pl @(Any Even Id) [1,5,112,5,3]
-- True (Any(5) i=2 (0 == 0))
-- TrueT
--
-- >>> pz @(Any Even Id) []
-- FalseT
--
-- >>> pl @(Any (Gt 3) (Fst Id)) ([10,12,3,5],"ss")
-- True (Any(4) i=0 (10 > 3))
-- TrueT
--
-- >>> pl @(Any (Same 2) Id) [1,4,5]
-- False (Any(3))
-- FalseT
--
-- >>> pl @(Any (Same 2) Id) [1,4,5,2,1]
-- True (Any(5) i=3 (2 == 2))
-- TrueT
--
data Any p q

instance (P p a
        , PP p a ~ Bool
        , PP q x ~ f a
        , P q x
        , Show a
        , Foldable f
        ) => P (Any p q) x where
  type PP (Any p q) x = Bool
  eval _ opts x = do
    let msg0 = "Any"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case chkSize opts msg0 q [hh qq] of
          Left e -> pure e
          Right () -> do
            ts <- zipWithM (\i a -> ((i, a),) <$> evalBoolHide @p opts a) [0::Int ..] (toList q)
            pure $ case splitAndAlign opts msg0 ts of
                 Left e -> e
                 Right abcs ->
                   let hhs = hh qq : map (hh . fixit) ts
                       msg1 = msg0 ++ "(" ++ show (length q) ++ ")"
                   in case find (view _1) abcs of
                        Nothing -> mkNodeB opts False msg1 hhs
                        Just (_,(i,_),tt) ->
                          mkNodeB opts True (msg1 <> " i=" ++ showIndex i ++ " " <> topMessage tt) hhs

-- | similar to 'fst'
--
-- >>> pz @(Fst Id) (10,"Abc")
-- PresentT 10
--
-- >>> pz @(Fst Id) (10,"Abc",'x')
-- PresentT 10
--
-- >>> pz @(Fst Id) (10,"Abc",'x',False)
-- PresentT 10
--
-- >>> pl @(Fst Id) (99,'a',False,1.3)
-- Present 99 (Fst 99 | (99,'a',False,1.3))
-- PresentT 99
--
data Fst p

instance (Show (ExtractL1T (PP p x))
        , ExtractL1C (PP p x)
        , P p x
        , Show (PP p x)
        ) => P (Fst p) x where
  type PP (Fst p) x = ExtractL1T (PP p x)
  eval _ opts x = do
    let msg0 = "Fst"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL1C p
        in mkNode opts (PresentT b) (show01 opts msg0 b p) [hh pp]

data L1 p
type L1T p = Fst p

instance P (L1T p) x => P (L1 p) x where
  type PP (L1 p) x = PP (L1T p) x
  eval _ = eval (Proxy @(L1T p))

class ExtractL1C tp where
  type ExtractL1T tp
  extractL1C :: tp -> ExtractL1T tp
instance ExtractL1C (a,b) where
  type ExtractL1T (a,b) = a
  extractL1C (a,_) = a
instance ExtractL1C (a,b,c) where
  type ExtractL1T (a,b,c) = a
  extractL1C (a,_,_) = a
instance ExtractL1C (a,b,c,d) where
  type ExtractL1T (a,b,c,d) = a
  extractL1C (a,_,_,_) = a
instance ExtractL1C (a,b,c,d,e) where
  type ExtractL1T (a,b,c,d,e) = a
  extractL1C (a,_,_,_,_) = a
instance ExtractL1C (a,b,c,d,e,f) where
  type ExtractL1T (a,b,c,d,e,f) = a
  extractL1C (a,_,_,_,_,_) = a

-- | similar to 'snd'
--
-- >>> pz @(Snd Id) (10,"Abc")
-- PresentT "Abc"
--
-- >>> pz @(Snd Id) (10,"Abc",True)
-- PresentT "Abc"
--
-- >>> pl @(Snd Id) (99,'a',False,1.3)
-- Present 'a' (Snd 'a' | (99,'a',False,1.3))
-- PresentT 'a'
--
data Snd p

instance (Show (ExtractL2T (PP p x))
        , ExtractL2C (PP p x)
        , P p x
        , Show (PP p x)
        ) => P (Snd p) x where
  type PP (Snd p) x = ExtractL2T (PP p x)
  eval _ opts x = do
    let msg0 = "Snd"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL2C p
        in mkNode opts (PresentT b) (show01 opts msg0 b p) [hh pp]

data L2 p
type L2T p = Snd p

instance P (L2T p) x => P (L2 p) x where
  type PP (L2 p) x = PP (L2T p) x
  eval _ = eval (Proxy @(L2T p))

class ExtractL2C tp where
  type ExtractL2T tp
  extractL2C :: tp -> ExtractL2T tp
instance ExtractL2C (a,b) where
  type ExtractL2T (a,b) = b
  extractL2C (_,b) = b
instance ExtractL2C (a,b,c) where
  type ExtractL2T (a,b,c) = b
  extractL2C (_,b,_) = b
instance ExtractL2C (a,b,c,d) where
  type ExtractL2T (a,b,c,d) = b
  extractL2C (_,b,_,_) = b
instance ExtractL2C (a,b,c,d,e) where
  type ExtractL2T (a,b,c,d,e) = b
  extractL2C (_,b,_,_,_) = b
instance ExtractL2C (a,b,c,d,e,f) where
  type ExtractL2T (a,b,c,d,e,f) = b
  extractL2C (_,b,_,_,_,_) = b

-- | similar to 3rd element in a n-tuple
--
-- >>> pz @(Thd Id) (10,"Abc",133)
-- PresentT 133
--
-- >>> pz @(Thd Id) (10,"Abc",133,True)
-- PresentT 133
--
-- >>> pl @(Thd Id) (99,'a',False,1.3)
-- Present False (Thd False | (99,'a',False,1.3))
-- PresentT False
--
data Thd p

instance (Show (ExtractL3T (PP p x))
        , ExtractL3C (PP p x)
        , P p x
        , Show (PP p x)
        ) => P (Thd p) x where
  type PP (Thd p) x = ExtractL3T (PP p x)
  eval _ opts x = do
    let msg0 = "Thd"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL3C p
        in mkNode opts (PresentT b) (show01 opts msg0 b p) [hh pp]

data L3 p
type L3T p = Thd p

instance P (L3T p) x => P (L3 p) x where
  type PP (L3 p) x = PP (L3T p) x
  eval _ = eval (Proxy @(L3T p))

class ExtractL3C tp where
  type ExtractL3T tp
  extractL3C :: tp -> ExtractL3T tp
instance ExtractL3C (a,b) where
  type ExtractL3T (a,b) = GL.TypeError ('GL.Text "Thd doesn't work for 2-tuples")
  extractL3C _ = errorInProgram "Thd doesn't work for 2-tuples"
instance ExtractL3C (a,b,c) where
  type ExtractL3T (a,b,c) = c
  extractL3C (_,_,c) = c
instance ExtractL3C (a,b,c,d) where
  type ExtractL3T (a,b,c,d) = c
  extractL3C (_,_,c,_) = c
instance ExtractL3C (a,b,c,d,e) where
  type ExtractL3T (a,b,c,d,e) = c
  extractL3C (_,_,c,_,_) = c
instance ExtractL3C (a,b,c,d,e,f) where
  type ExtractL3T (a,b,c,d,e,f) = c
  extractL3C (_,_,c,_,_,_) = c

-- | similar to 4th element in a n-tuple
--
-- >>> pz @(L4 Id) (10,"Abc",'x',True)
-- PresentT True
--
-- >>> pz @(L4 (Fst (Snd Id))) ('x',((10,"Abc",'x',999),"aa",1),9)
-- PresentT 999
--
-- >>> pl @(L4 Id) (99,'a',False,"someval")
-- Present "someval" (L4 "someval" | (99,'a',False,"someval"))
-- PresentT "someval"
--
data L4 p

instance (Show (ExtractL4T (PP p x))
        , ExtractL4C (PP p x)
        , P p x
        , Show (PP p x)
        ) => P (L4 p) x where
  type PP (L4 p) x = ExtractL4T (PP p x)
  eval _ opts x = do
    let msg0 = "L4"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL4C p
        in mkNode opts (PresentT b) (show01 opts msg0 b p) [hh pp]

class ExtractL4C tp where
  type ExtractL4T tp
  extractL4C :: tp -> ExtractL4T tp
instance ExtractL4C (a,b) where
  type ExtractL4T (a,b) = GL.TypeError ('GL.Text "L4 doesn't work for 2-tuples")
  extractL4C _ = errorInProgram "L4 doesn't work for 2-tuples"
instance ExtractL4C (a,b,c) where
  type ExtractL4T (a,b,c) = GL.TypeError ('GL.Text "L4 doesn't work for 3-tuples")
  extractL4C _ = errorInProgram "L4 doesn't work for 3-tuples"
instance ExtractL4C (a,b,c,d) where
  type ExtractL4T (a,b,c,d) = d
  extractL4C (_,_,_,d) = d
instance ExtractL4C (a,b,c,d,e) where
  type ExtractL4T (a,b,c,d,e) = d
  extractL4C (_,_,_,d,_) = d
instance ExtractL4C (a,b,c,d,e,f) where
  type ExtractL4T (a,b,c,d,e,f) = d
  extractL4C (_,_,_,d,_,_) = d

-- | similar to 5th element in a n-tuple
--
-- >>> pz @(L5 Id) (10,"Abc",'x',True,1)
-- PresentT 1
--
data L5 p

instance (Show (ExtractL5T (PP p x))
        , ExtractL5C (PP p x)
        , P p x
        , Show (PP p x)
        ) => P (L5 p) x where
  type PP (L5 p) x = ExtractL5T (PP p x)
  eval _ opts x = do
    let msg0 = "L5"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL5C p
        in mkNode opts (PresentT b) (show01 opts msg0 b p) [hh pp]

class ExtractL5C tp where
  type ExtractL5T tp
  extractL5C :: tp -> ExtractL5T tp
instance ExtractL5C (a,b) where
  type ExtractL5T (a,b) = GL.TypeError ('GL.Text "L5 doesn't work for 2-tuples")
  extractL5C _ = errorInProgram "L5 doesn't work for 2-tuples"
instance ExtractL5C (a,b,c) where
  type ExtractL5T (a,b,c) = GL.TypeError ('GL.Text "L5 doesn't work for 3-tuples")
  extractL5C _ = errorInProgram "L5 doesn't work for 3-tuples"
instance ExtractL5C (a,b,c,d) where
  type ExtractL5T (a,b,c,d) = GL.TypeError ('GL.Text "L5 doesn't work for 4-tuples")
  extractL5C _ = errorInProgram "L5 doesn't work for 4-tuples"
instance ExtractL5C (a,b,c,d,e) where
  type ExtractL5T (a,b,c,d,e) = e
  extractL5C (_,_,_,_,e) = e
instance ExtractL5C (a,b,c,d,e,f) where
  type ExtractL5T (a,b,c,d,e,f) = e
  extractL5C (_,_,_,_,e,_) = e


-- | similar to 6th element in a n-tuple
--
-- >>> pz @(L6 Id) (10,"Abc",'x',True,1,99)
-- PresentT 99
--
data L6 p

instance (Show (ExtractL6T (PP p x))
        , ExtractL6C (PP p x)
        , P p x
        , Show (PP p x)
        ) => P (L6 p) x where
  type PP (L6 p) x = ExtractL6T (PP p x)
  eval _ opts x = do
    let msg0 = "L6"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL6C p
        in mkNode opts (PresentT b) (show01 opts msg0 b p) [hh pp]

class ExtractL6C tp where
  type ExtractL6T tp
  extractL6C :: tp -> ExtractL6T tp
instance ExtractL6C (a,b) where
  type ExtractL6T (a,b) = GL.TypeError ('GL.Text "L6 doesn't work for 2-tuples")
  extractL6C _ = errorInProgram "L6 doesn't work for 2-tuples"
instance ExtractL6C (a,b,c) where
  type ExtractL6T (a,b,c) = GL.TypeError ('GL.Text "L6 doesn't work for 3-tuples")
  extractL6C _ = errorInProgram "L6 doesn't work for 3-tuples"
instance ExtractL6C (a,b,c,d) where
  type ExtractL6T (a,b,c,d) = GL.TypeError ('GL.Text "L6 doesn't work for 4-tuples")
  extractL6C _ = errorInProgram "L6 doesn't work for 4-tuples"
instance ExtractL6C (a,b,c,d,e) where
  type ExtractL6T (a,b,c,d,e) = GL.TypeError ('GL.Text "L6 doesn't work for 5-tuples")
  extractL6C _ = errorInProgram "L6 doesn't work for 5-tuples"
instance ExtractL6C (a,b,c,d,e,f) where
  type ExtractL6T (a,b,c,d,e,f) = f
  extractL6C (_,_,_,_,_,f) = f

-- | applies \'p\' to the first and second slot of an n-tuple
--
-- >>> pl @(Both Len (Fst Id)) (("abc",[10..17],1,2,3),True)
-- Present (3,8) (Both)
-- PresentT (3,8)
--
-- >>> pl @(Both (Pred Id) $ Fst Id) ((12,'z',[10..17]),True)
-- Present (11,'y') (Both)
-- PresentT (11,'y')
--
-- >>> pl @(Both (Succ Id) Id) (4,'a')
-- Present (5,'b') (Both)
-- PresentT (5,'b')
--
-- >>> pl @(Both Len (Fst Id)) (("abc",[10..17]),True)
-- Present (3,8) (Both)
-- PresentT (3,8)
--
-- >>> pl @(Both (ReadP Day Id) Id) ("1999-01-01","2001-02-12")
-- Present (1999-01-01,2001-02-12) (Both)
-- PresentT (1999-01-01,2001-02-12)
--
data Both p q
instance ( ExtractL1C (PP q x)
         , ExtractL2C (PP q x)
         , P p (ExtractL1T (PP q x))
         , P p (ExtractL2T (PP q x))
         , P q x
   ) => P (Both p q) x where
  type PP (Both p q) x = (PP p (ExtractL1T (PP q x)), PP p (ExtractL2T (PP q x)))
  eval _ opts x = do
    let msg0 = "Both"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        let (a,a') = (extractL1C q, extractL2C q)
        pp <- eval (Proxy @p) opts a
        case getValueLR opts msg0 pp [hh qq] of
          Left e -> pure e
          Right b -> do
            pp' <- eval (Proxy @p) opts a'
            pure $ case getValueLR opts msg0 pp' [hh qq, hh pp] of
              Left e -> e
              Right b' ->
                mkNode opts (PresentT (b,b')) msg0 [hh qq, hh pp, hh pp']

-- | similar to 'map'
--
-- >>> pz @(Map (Pred Id) Id) [1..5]
-- PresentT [0,1,2,3,4]
--
data Map p q

instance (Show (PP p a)
        , P p a
        , PP q x ~ f a
        , P q x
        , Show a
        , Show (f a)
        , Foldable f
        ) => P (Map p q) x where
  type PP (Map p q) x = [PP p (ExtractAFromTA (PP q x))]
  eval _ opts x = do
    let msg0 = "Map"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        ts <- zipWithM (\i a -> ((i, a),) <$> evalHide @p opts a) [0::Int ..] (toList q)
        pure $ case splitAndAlign opts msg0 ts of
             Left e -> e
             Right abcs ->
               let vals = map (view _1) abcs
               in mkNode opts (PresentT vals) (show01 opts msg0 vals q) (hh qq : map (hh . fixit) ts)

-- | processes a type level list predicates running each in sequence: see 'Predicate.>>'
--
-- >>> pz @(Do [Pred Id, ShowP Id, Id &&& Len]) 9876543
-- PresentT ("9876542",7)
--
-- >>> pz @(Do '[W 123, W "xyz", Len &&& Id, Pred Id *** Id<>Id]) ()
-- PresentT (2,"xyzxyz")
--
-- >>> pl @(Do '[Succ Id,Id,ShowP Id,Ones Id,Map (ReadBase Int 8 Id) Id]) 1239
-- Present [1,2,4,0] ((>>) [1,2,4,0] | {Map [1,2,4,0] | ["1","2","4","0"]})
-- PresentT [1,2,4,0]
--
-- >>> pl @(Do '[Pred Id,Id,ShowP Id,Ones Id,Map (ReadBase Int 8 Id) Id]) 1239
-- Error invalid base 8 (1238 (>>) rhs failed)
-- FailT "invalid base 8"
--
-- >>> pl @(Do '[4,5,6]) ()
-- Present 6 ((>>) 6 | {'6})
-- PresentT 6
--
-- >>> pl @(Do '["abc", "Def", "ggg", "hhhhh"]) ()
-- Present "hhhhh" ((>>) "hhhhh" | {'"hhhhh"})
-- PresentT "hhhhh"
--
-- >>> pl @(Do '[ 'LT, 'EQ, 'GT ]) ()
-- Present GT ((>>) GT | {'GT})
-- PresentT GT
--
-- >>> pl @(Do '[4 % 4,22 % 1 ,12 -% 4]) ()
-- Present (-3) % 1 ((>>) (-3) % 1 | {Negate (-3) % 1 | 3 % 1})
-- PresentT ((-3) % 1)
--
-- >>> pl @(Do '[ W ('PresentT I), W 'FalseT, Not Id]) False
-- True ((>>) True | {Not (Id False)})
-- TrueT
--
-- >>> pl @(Do '[W ('PresentT Id), W 'FalseT]) True -- have to wrap them cos BoolT a vs BoolT Bool ie different types
-- False ((>>) False | {W 'FalseT})
-- FalseT
--
-- >>> pl @(Do '[1,2,3]) ()
-- Present 3 ((>>) 3 | {'3})
-- PresentT 3
--

data Do (ps :: [k])

instance (P (DoExpandT ps) a) => P (Do ps) a where
  type PP (Do ps) a = PP (DoExpandT ps) a
  eval _ = eval (Proxy @(DoExpandT ps))

type family DoExpandT (ps :: [k]) :: Type where
  DoExpandT '[] = GL.TypeError ('GL.Text "'[] invalid: requires at least one predicate in the list")
  DoExpandT '[p] = Id >> p -- need this else fails cos 1 is nat and would mean that the result is nat not Type!
  -- if p >> Id then turns TrueT to PresentT True
  DoExpandT (p ': p1 ': ps) = p >> DoExpandT (p1 ': ps)

-- | similar to 'Prelude.&&'
--
-- >>> pz @(Fst Id && Snd Id) (True, True)
-- TrueT
--
-- >>> pz @(Id > 15 && Id < 17) 16
-- TrueT
--
-- >>> pz @(Id > 15 && Id < 17) 30
-- FalseT
--
-- >>> pz @(Fst Id && (Length (Snd Id) >= 4)) (True,[11,12,13,14])
-- TrueT
--
-- >>> pz @(Fst Id && (Length (Snd Id) == 4)) (True,[12,11,12,13,14])
-- FalseT
--
data p && q
infixr 3 &&

instance (P p a
        , P q a
        , PP p a ~ Bool
        , PP q a ~ Bool
        ) => P (p && q) a where
  type PP (p && q) a = Bool
  eval _ opts a = do
    let msg0 = "&&"
    lr <- runPQBool msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let zz = case (p,q) of
                  (True, True) -> ""
                  (False, True) -> topMessage pp
                  (True, False) -> topMessage qq
                  (False, False) -> topMessage pp <> " " <> msg0 <> " " <> topMessage qq
        in mkNodeB opts (p&&q) (showL opts p <> " " <> msg0 <> " " <> showL opts q <> (if null zz then zz else " | " <> zz)) [hh pp, hh qq]

-- | short circuit version of boolean And
--
-- >>> pl @(Id > 10 &&~ Failt _ "ss") 9
-- False (False &&~ _ | (9 > 10))
-- FalseT
--
-- >>> pl @(Id > 10 &&~ Id == 12) 11
-- False (True &&~ False | (11 == 12))
-- FalseT
--
-- >>> pl @(Id > 10 &&~ Id == 11) 11
-- True (True &&~ True)
-- TrueT
--
data p &&~ q
infixr 3 &&~

instance (P p a
        , P q a
        , PP p a ~ Bool
        , PP q a ~ Bool
        ) => P (p &&~ q) a where
  type PP (p &&~ q) a = Bool
  eval _ opts a = do
    let msg0 = "&&~"
    pp <- eval (Proxy @p) opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right False ->
        pure $ mkNodeB opts False ("False " <> msg0 <> " _" <> litVerbose opts " | " (topMessage pp)) [hh pp]
      Right True -> do
        qq <- eval (Proxy @q) opts a
        pure $ case getValueLR opts msg0 qq [hh pp] of
          Left e -> e
          Right q ->
            let zz = if q then ""
                     else " | " <> topMessage qq
            in mkNodeB opts q ("True " <> msg0 <> " " <> showL opts q <> litVerbose opts "" zz) [hh pp, hh qq]

-- | similar to 'Prelude.||'
--
-- >>> pz @(Fst Id || (Length (Snd Id) >= 4)) (False,[11,12,13,14])
-- TrueT
--
-- >>> pz @(Not (Fst Id) || (Length (Snd Id) == 4)) (True,[12,11,12,13,14])
-- FalseT
--
data p || q
infixr 2 ||

instance (P p a
        , P q a
        , PP p a ~ Bool
        , PP q a ~ Bool
        ) => P (p || q) a where
  type PP (p || q) a = Bool
  eval _ opts a = do
    let msg0 = "||"
    lr <- runPQBool msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let zz = case (p,q) of
                  (False,False) -> " | " <> topMessage pp <> " " <> msg0 <> " " <> topMessage qq
                  _ -> ""
        in mkNodeB opts (p||q) (showL opts p <> " " <> msg0 <> " " <> showL opts q <> zz) [hh pp, hh qq]

-- | short circuit version of boolean Or
--
-- >>> pl @(Id > 10 ||~ Failt _ "ss") 11
-- True (True ||~ _ | (11 > 10))
-- TrueT
--
-- >>> pz @(Id > 10 ||~ Id == 9) 9
-- TrueT
--
-- >>> pl @(Id > 10 ||~ Id > 9) 9
-- False (False ||~ False | (9 > 10) ||~ (9 > 9))
-- FalseT
--
data p ||~ q
infixr 2 ||~

instance (P p a
        , P q a
        , PP p a ~ Bool
        , PP q a ~ Bool
        ) => P (p ||~ q) a where
  type PP (p ||~ q) a = Bool
  eval _ opts a = do
    let msg0 = "||~"
    pp <- eval (Proxy @p) opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right False -> do
        qq <- eval (Proxy @q) opts a
        pure $ case getValueLR opts msg0 qq [hh pp] of
          Left e -> e
          Right q ->
            let zz = if q then ""
                     else " | " <> topMessage pp <> " " <> msg0 <> " " <> topMessage qq
            in mkNodeB opts q ("False " <> msg0 <> " " <> showL opts q <> litVerbose opts "" zz) [hh pp, hh qq]
      Right True ->
        pure $ mkNodeB opts True ("True " <> msg0 <> " _" <> litVerbose opts " | " (topMessage pp)) [hh pp]

-- | boolean implication
--
-- >>> pz @(Fst Id ~> (Length (Snd Id) >= 4)) (True,[11,12,13,14])
-- TrueT
--
-- >>> pz @(Fst Id ~> (Length (Snd Id) == 4)) (True,[12,11,12,13,14])
-- FalseT
--
-- >>> pz @(Fst Id ~> (Length (Snd Id) == 4)) (False,[12,11,12,13,14])
-- TrueT
--
-- >>> pz @(Fst Id ~> (Length (Snd Id) >= 4)) (False,[11,12,13,14])
-- TrueT
--
data p ~> q
infixr 1 ~>

instance (P p a
        , P q a
        , PP p a ~ Bool
        , PP q a ~ Bool
        ) => P (p ~> q) a where
  type PP (p ~> q) a = Bool
  eval _ opts a = do
    let msg0 = "~>"
    lr <- runPQBool msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let zz = case (p,q) of
                  (True,False) -> topMessage pp <> " " <> msg0 <> " " <> topMessage qq
                  _ -> ""
        in mkNodeB opts (p~>q) (showL opts p <> " " <> msg0 <> " " <> showL opts q <> (if null zz then zz else " | " <> zz)) [hh pp, hh qq]


-- | swaps using 'SwapC'
--
-- >>> pz @Swap (Left 123)
-- PresentT (Right 123)
--
-- >>> pz @Swap (Right 123)
-- PresentT (Left 123)
--
-- >>> pz @Swap (These 'x' 123)
-- PresentT (These 123 'x')
--
-- >>> pz @Swap (This 'x')
-- PresentT (That 'x')
--
-- >>> pz @Swap (That 123)
-- PresentT (This 123)
--
-- >>> pz @Swap (123,'x')
-- PresentT ('x',123)
--
-- >>> pz @Swap (Left "abc")
-- PresentT (Right "abc")
--
-- >>> pz @Swap (Right 123)
-- PresentT (Left 123)
--
-- >>> pl @Swap (Right "asfd")
-- Present Left "asfd" (Swap Left "asfd" | Right "asfd")
-- PresentT (Left "asfd")
--
-- >>> pl @Swap (12,"asfd")
-- Present ("asfd",12) (Swap ("asfd",12) | (12,"asfd"))
-- PresentT ("asfd",12)
--

data Swap

class Bifunctor p => SwapC p where -- (p :: Type -> Type -> Type) where
  swapC :: p a b -> p b a
instance SwapC Either where
  swapC (Left a) = Right a
  swapC (Right a) = Left a
instance SwapC These where
  swapC (This a) = That a
  swapC (That b) = This b
  swapC (These a b) = These b a
instance SwapC (,) where
  swapC (a,b) = (b,a)

instance (Show (p a b)
        , SwapC p
        , Show (p b a)
        ) => P Swap (p a b) where
  type PP Swap (p a b) = p b a
  eval _ opts pabx =
    let msg0 = "Swap"
        d = swapC pabx
    in pure $ mkNode opts (PresentT d) (show01 opts msg0 d pabx) []

-- | like 'GHC.Base.$' for expressions
--
-- >>> pl @(Fst $ Snd $ Id) ((1,2),(3,4))
-- Present 3 (Fst 3 | (3,4))
-- PresentT 3
--
-- >>> pl @((<=) 4 $ Fst $ Snd $ Id) ((1,2),(3,4))
-- False (4 <= 3)
-- FalseT
--
data (p :: k -> k1) $ (q :: k)
infixr 0 $

instance P (p q) a => P (p $ q) a where
  type PP (p $ q) a = PP (p q) a
  eval _  = eval (Proxy @(p q))

-- | similar to 'Control.Lens.&'
--
-- >>> pl @(Id & Fst & Singleton & Length) (13,"xyzw")
-- Present 1 (Length 1 | [13])
-- PresentT 1
--
-- >>> pl @(2 & (&&&) "abc") ()
-- Present ("abc",2) (W '("abc",2))
-- PresentT ("abc",2)
--
-- >>> pl @(2 & '(,) "abc") ()
-- Present ("abc",2) ('("abc",2))
-- PresentT ("abc",2)
--
-- >>> pl @('(,) 4 $ '(,) 7 $ "aa") ()
-- Present (4,(7,"aa")) ('(4,(7,"aa")))
-- PresentT (4,(7,"aa"))
--
-- >>> pl @(Thd $ Snd $ Fst Id) ((1,("W",9,'a')),(3,4))
-- Present 'a' (Thd 'a' | ("W",9,'a'))
-- PresentT 'a'
--
data (q :: k) & (p :: k -> k1)
infixl 1 &

instance P (p q) a => P (q & p) a where
  type PP (q & p) a = PP (p q) a
  eval _ = eval (Proxy @(p q))

-- | similar to 'pure'
--
-- >>> pz @(Pure Maybe Id) 4
-- PresentT (Just 4)
--
-- >>> pz @(Pure [] Id) 4
-- PresentT [4]
--
-- >>> pz @(Pure (Either String) (Fst Id)) (13,True)
-- PresentT (Right 13)
--
-- >>> pl @(Pure Maybe Id) 'x'
-- Present Just 'x' (Pure Just 'x' | 'x')
-- PresentT (Just 'x')
--
-- >>> pl @(Pure (Either _) Id) 'x'
-- Present Right 'x' (Pure Right 'x' | 'x')
-- PresentT (Right 'x')
--
-- >>> pl @(Pure (Either _) Id >> Swap) 'x'
-- Present Left 'x' ((>>) Left 'x' | {Swap Left 'x' | Right 'x'})
-- PresentT (Left 'x')
--
-- >>> pl @(Pure (Either ()) Id >> Swap) 'x'
-- Present Left 'x' ((>>) Left 'x' | {Swap Left 'x' | Right 'x'})
-- PresentT (Left 'x')
--
-- >>> pl @(Pure (Either String) Id >> Swap) 123
-- Present Left 123 ((>>) Left 123 | {Swap Left 123 | Right 123})
-- PresentT (Left 123)
--
data Pure (t :: Type -> Type) p
instance (P p x
        , Show (PP p x)
        , Show (t (PP p x))
        , Applicative t
        ) => P (Pure t p) x where
  type PP (Pure t p) x = t (PP p x)
  eval _ opts x = do
    let msg0 = "Pure"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right a ->
        let b = pure a
        in mkNode opts (PresentT b) (show01 opts msg0 b a) [hh pp]

-- | similar to 'coerce'
--
-- >>> pz @(Coerce (SG.Sum Integer)) (Identity (-13))
-- PresentT (Sum {getSum = -13})
--
-- >>> pl @(Coerce SG.Any) True
-- Present Any {getAny = True} (Coerce Any {getAny = True} | True)
-- PresentT (Any {getAny = True})
--
-- >>> pl @(Coerce Bool) (SG.Any True)
-- Present True (Coerce True | Any {getAny = True})
-- PresentT True
--
data Coerce (t :: k)

instance (Show a
        , Show t
        , Coercible t a
        ) => P (Coerce t) a where
  type PP (Coerce t) a = t
  eval _ opts a =
    let msg0 = "Coerce"
        d = a ^. coerced
    in pure $ mkNode opts (PresentT d) (show01 opts msg0 d a) []


