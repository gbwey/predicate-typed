{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# OPTIONS -Wunused-type-patterns #-}
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
  , run
  , runs

  , pz

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
  , L11
  , L12
  , L13
  , L21
  , L22
  , L23
  , L31
  , L32
  , L33

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
import Predicate.Misc
import Predicate.Util
import qualified GHC.TypeLits as GL
import GHC.TypeLits (Symbol,Nat,KnownSymbol,KnownNat)
import Control.Lens
import Data.Foldable (toList)
import Data.Typeable (Typeable, Proxy(..))
import Data.Kind (Type)
import Data.These (These(..))
import Control.Monad (zipWithM)
import Data.List (find)
import Data.Tree (Tree)
import Data.Coerce (Coercible)
import qualified Data.Semigroup as SG
import Data.Tree.Lens (root)
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate.Prelude
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
evalBool p = (fmap fixTTValP .) . eval p

evalQuick :: forall opts p i
  . ( OptC opts
    , P p i
    )
    => i
    -> Either String (PP p i)
evalQuick = getValLRFromTT . runIdentity . eval @_ (Proxy @p) (getOpt @opts)

-- | identity function
--
-- >>> pz @Id 23
-- Val 23
--
data Id
instance Show a => P Id a where
  type PP Id a = a
  eval _ opts a =
    let msg0 = "Id"
    in pure $ mkNode opts (Val a) (msg0 <> " " <> showL opts a) []

-- | identity function that also displays the type information for debugging
--
-- >>> pz @IdT 23
-- Val 23
data IdT
instance ( Typeable a
         , Show a
         ) => P IdT a where
  type PP IdT a = a
  eval _ opts a =
    let msg0 = "IdT(" <> t <> ")"
        t = showT @a
    in pure $ mkNode opts (Val a) (msg0 <> " " <> showL opts a) []

-- | transparent predicate wrapper to make k of kind 'Type' so it can be in a promoted list (cant mix kinds) see 'Predicate.Core.Do'
--
-- >>> pz @'[W 123, Id] 99
-- Val [123,99]
--
-- >>> pz @'[W "abc", W "def", Id, Id] "ghi"
-- Val ["abc","def","ghi","ghi"]
--
data W (p :: k)
instance P p a => P (W p) a where
  type PP (W p) a = PP p a
  eval _ = eval (Proxy @(MsgI "W " p))

-- | add a message to give more context to the evaluation tree
--
-- >>> pan @(Msg "[somemessage]" Id) 999
-- P [somemessage] Id 999
-- Val 999
--
-- >>> pan @(Msg Id 999) "info message:"
-- P info message: '999
-- Val 999
--
data Msg prt p

instance ( P prt a
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
-- Val 999
--
-- >>> pan @(MsgI Id 999) "info message:"
-- P info message:'999
-- Val 999
--
data MsgI prt p

instance ( P prt a
         , PP prt a ~ String
         , P p a
         ) => P (MsgI prt p) a where
  type PP (MsgI prt p) a = PP p a
  eval _ opts a = do
    pp <- eval (Proxy @prt) opts a
    case getValueLR opts "MsgI" pp [] of
         Left e -> pure e
         Right msg -> prefixMsg msg <$> eval (Proxy @p) opts a

-- | run the expression @p@ but remove the subtrees
data Hide p
-- type H p = Hide p -- doesnt work with %   -- unsaturated!

instance P p x => P (Hide p) x where
  type PP (Hide p) x = PP p x
  eval _ opts x = do
      tt <- eval (Proxy @p) opts x
      pure $ tt & ttForest .~ []

data Hole (t :: Type)

-- | Acts as a proxy in this dsl where you can explicitly set the Type.
--
--  It is passed around as an argument to help the type checker when needed.
--
instance Typeable t => P (Hole t) a where
  type PP (Hole t) a = t -- can only be Type not Type -> Type (can use Proxy but then we go down the rabbithole)
  eval _ opts _a =
    let msg0 = "Hole(" <> showT @t <> ")"
    in pure $ mkNode opts (Fail msg0) "you probably meant to get access to the type of PP only and not evaluate" []

-- | override the display width for the expression @p@
data Width (n :: Nat) p

instance ( KnownNat n
         , P p a
         ) => P (Width n p) a where
  type PP (Width n p) a = PP p a
  eval _ opts a = do
    let opts' = opts { oWidth = nat @n }
    eval (Proxy @p) opts' a

-- | 'const' () function
--
-- >>> pz @() "Asf"
-- Val ()
--
instance P () a where
  type PP () a = ()
  eval _ opts _ =
    let msg0 = "()"
    in pure $ mkNode opts (Val ()) msg0 []

-- | 'const' [] function
--
-- >>> pz @[] "Asf"
-- Val []
--
instance P [] a where
  type PP [] a = [a]
  eval _ opts _ =
    let msg0 = "[]"
    in pure $ mkNode opts (Val []) msg0 []

instance P (Proxy t) a where
  type PP (Proxy t) a = Proxy t
  eval _ opts _ =
    let msg0 = "Proxy"
    in pure $ mkNode opts (Val Proxy) msg0 []

-- Start non-Type kinds
-----------------------

-- | pulls the type level 'Bool' to the value level
--
-- >>> pz @'True "not used"
-- Val True
--
-- >>> pz @'False ()
-- Val False
instance GetBool b => P (b :: Bool) a where
  type PP b a = Bool
  eval _ opts _ =
    let b = getBool @b
    in pure $ mkNodeB opts b ("'" <> showL opts b) []

-- | pulls the type level 'GHC.TypeLits.Symbol' to the value level as a 'GHC.Base.String'
--
-- >>> pz @"hello world" ()
-- Val "hello world"
instance KnownSymbol s => P (s :: Symbol) a where
  type PP s a = String
  eval _ opts _ =
    let s = symb @s
    in pure $ mkNode opts (Val s) ("'" <> litL opts ("\"" <> s <> "\"")) []

-- | run the predicates in a promoted 2-tuple; similar to 'Control.Arrow.&&&'
--
-- >>> pz @'(Id, 4) "hello"
-- Val ("hello",4)
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
--         mkNode opts (Val (p,q)) msg [hh pp, hh qq]
         mkNode opts (Val (p,q)) ("'(" <> showL opts p <> "," <> showL opts q <> ")") [hh pp, hh qq]

-- | run the predicates in a promoted 3-tuple
--
-- >>> pz @'(4, Id, "goodbye") "hello"
-- Val (4,"hello","goodbye")
--
-- >>> pan @'( 'True, 'False, 123) True
-- P '(,,)
-- |
-- +- True 'True
-- |
-- +- False 'False
-- |
-- `- P '123
-- Val (True,False,123)
--
instance ( P p a
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
             in mkNode opts (Val (p,q,r)) msg hhs1

-- | run the predicates in a promoted 4-tuple
--
-- >>> pz @'(4, Id, "inj", 999) "hello"
-- Val (4,"hello","inj",999)
--
instance ( P p a
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
            in mkNode opts (Val (p,q,r,s)) msg hhs1

-- | run the predicates in a promoted 5-tuple
--
-- >>> pz @'(4, Id, "inj", 999, 'LT) "hello"
-- Val (4,"hello","inj",999,LT)
--
instance ( P p a
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
                in mkNode opts (Val (p,q,r,s,t)) msg hhs2

-- | run the predicates in a promoted 6-tuple
--
-- >>> pz @'(4, Id, "inj", 999, 'LT, 1) "hello"
-- Val (4,"hello","inj",999,LT,1)
--
instance ( P p a
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
                in mkNode opts (Val (p,q,r,s,t,u)) msg hhs2

-- | run the predicates in a promoted 7-tuple
--
-- >>> pz @'(4, Id, "inj", 999, 'LT, 1, 2) "hello"
-- Val (4,"hello","inj",999,LT,1,2)
--
instance ( P p a
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
                    in mkNode opts (Val (p,q,r,s,t,u,v)) msg hhs3

-- | run the predicates in a promoted 8-tuple
--
-- >>> pz @'(4, Id, "inj", 999, 'LT, 1, 2, 3) "hello"
-- Val (4,"hello","inj",999,LT,1,2,3)
--
instance ( P p a
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
                     in mkNode opts (Val (p,q,r,s,t,u,v,w)) msg hhs3


-- | extracts the value level representation of the promoted 'Ordering'
--
-- >>> pz @'LT "not used"
-- Val LT
--
-- >>> pz @'EQ ()
-- Val EQ
instance GetOrdering cmp => P (cmp :: Ordering) a where
  type PP cmp a = Ordering
  eval _ opts _a =
    let cmp = getOrdering @cmp
        msg = "'" <> showL opts cmp
    in pure $ mkNode opts (Val cmp) msg []

-- | extracts the value level representation of the type level 'Nat'
--
-- >>> pz @123 ()
-- Val 123
--
instance KnownNat n => P (n :: Nat) a where
  type PP n a = Int
  eval _ opts _ =
    let n = nat @n
    in pure $ mkNode opts (Val n) ("'" <> showL opts n) []

-- | extracts the value level representation of the type level '()
--
-- >>> pz @'() ()
-- Val ()
instance P '() a where
  type PP '() a = ()
  eval _ opts _ = pure $ mkNode opts (Val ()) "'()" []

-- the type has to be [a] so we still need type PP '[p] a = [PP p a] to keep the types in line

-- | extracts the value level representation of the type level '[]
--
-- >>> pz @'[] False
-- Val []
instance P ('[] :: [k]) a where
  type PP ('[] :: [k]) a = [a]
  eval _ opts _ = pure $ mkNode opts (Val mempty) "'[]" []

-- | runs each predicate in turn from the promoted list
--
-- >>> pz @'[1, 2, 3] 999
-- Val [1,2,3]
--
-- >>> pz @'[W 1, W 2, W 3, Id] 999
-- Val [1,2,3,999]
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
       Right b -> mkNode opts (Val [b]) ("'" <> showL opts ([b] :: [PP p a]) <> showVerbose opts " | " a) [hh pp]

instance ( Show (PP p a)
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
        pure $ case getValueLR opts (_ttString qq) qq [hh pp] of
          Left e -> e
          Right q ->
            let ret = p:q
            -- no gap between ' and ret!
            in mkNode opts (Val ret) ("'" <> showL opts ret <> litVerbose opts " " (topMessage pp) <> showVerbose opts " | " a) (verboseList opts pp <> [hh qq])

-- | tries to extract @a@ from @Maybe a@ otherwise it fails: similar to 'Data.Maybe.fromJust'
--
-- >>> pz @('Just Id) (Just "abc")
-- Val "abc"
--
-- >>> pl @('Just Id >> Id) (Just 123)
-- Present 123 ((>>) 123 | {Id 123})
-- Val 123
--
-- >>> pl @('Just Id) (Just [1,2,3])
-- Present [1,2,3] ('Just [1,2,3] | Just [1,2,3])
-- Val [1,2,3]
--
-- >>> pl @('Just Id) (Just 10)
-- Present 10 ('Just 10 | Just 10)
-- Val 10
--
-- >>> pl @('Just Id) Nothing
-- Error 'Just(empty)
-- Fail "'Just(empty)"
--
-- >>> pz @('Just Fst) (Just 123,'x')
-- Val 123
--
instance ( Show a
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
          Nothing -> mkNode opts (Fail (msg0 <> "(empty)")) "" [hh pp]
          Just d -> mkNode opts (Val d) (show3 opts msg0 d p) [hh pp]

-- | expects Nothing otherwise it fails
--   if the value is Nothing then it returns @Proxy a@ as this provides type information
--
-- >>> pz @'Nothing Nothing
-- Val Proxy
--
-- >>> pz @'Nothing (Just True)
-- Fail "'Nothing found Just"
--
instance P 'Nothing (Maybe a) where
  type PP 'Nothing (Maybe a) = Proxy a -- () gives us less information
  eval _ opts ma =
    let msg0 = "'Nothing"
    in pure $ case ma of
         Nothing -> mkNode opts (Val Proxy) msg0 []
         Just _ -> mkNode opts (Fail (msg0 <> " found Just")) "" []

-- omitted Show x so we can have less ambiguity
-- | extracts the @a@ from type level @Either a b@ if the value exists
--
-- >>> pz @('Left Id) (Left 123)
-- Val 123
--
-- >>> pz @('Left Snd) ('x', Left 123)
-- Val 123
--
-- >>> pz @('Left Id) (Right "aaa")
-- Fail "'Left found Right"
--
-- >>> pl @('Left Id) (Left 123)
-- Present 123 (Left)
-- Val 123
--
-- >>> pl @('Left Id) (Right 123)
-- Error 'Left found Right
-- Fail "'Left found Right"
--

instance ( PP p x ~ Either a b
         , P p x
         )
    => P ('Left p) x where
  type PP ('Left p) x = LeftT (PP p x)
  eval _ opts x = do
    let msg0 = "'Left"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          Left a -> mkNode opts (Val a) "Left" [hh pp]
          Right _b -> mkNode opts (Fail (msg0 <> " found Right")) "" [hh pp]

-- | extracts the @b@ from type level @Either a b@ if the value exists
--
-- >>> pl @('Right Id) (Right 123)
-- Present 123 (Right)
-- Val 123
--
-- >>> pz @('Right Id >> Snd) (Right ('x',123))
-- Val 123
--
-- >>> pz @('Right Id) (Left "aaa")
-- Fail "'Right found Left"
--
-- >>> pl @('Right Id) (Left 123)
-- Error 'Right found Left
-- Fail "'Right found Left"
--
instance ( PP p x ~ Either a b
         , P p x
         )
    => P ('Right p) x where
  type PP ('Right p) x = RightT (PP p x)
  eval _ opts x = do
    let msg0 = "'Right"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          Left _a -> mkNode opts (Fail (msg0 <> " found Left")) "" [hh pp]
          Right b -> mkNode opts (Val b) "Right" [hh pp]


-- removed Show x: else ambiguity errors in TestPredicate

-- | extracts the @a@ from type level @These a b@ if the value exists
--
-- >>> pl @('This Id) (This 12)
-- Present 12 (This)
-- Val 12
--
-- >>> pz @('This Id) (That "aaa")
-- Fail "'This found That"
--
-- >>> pz @('This Id) (These 999 "aaa")
-- Fail "'This found These"
--
-- >>> pl @('This Id) (That 12)
-- Error 'This found That
-- Fail "'This found That"
--

instance ( PP p x ~ These a b
         , P p x
         )
    => P ('This p) x where
  type PP ('This p) x = ThisT (PP p x)
  eval _ opts x = do
    let msg0 = "'This"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          This a -> mkNode opts (Val a) "This" [hh pp]
          That _b -> mkNode opts (Fail (msg0 <> " found That")) "" [hh pp]
          These _a _b -> mkNode opts (Fail (msg0 <> " found These")) "" [hh pp]

-- | extracts the @b@ from type level @These a b@ if the value exists
--
-- >>> pz @('That Id) (That 123)
-- Val 123
--
-- >>> pz @('That Id) (This "aaa")
-- Fail "'That found This"
--
-- >>> pz @('That Id) (These 44 "aaa")
-- Fail "'That found These"
--

instance ( PP p x ~ These a b
         , P p x
         )
    => P ('That p) x where
  type PP ('That p) x = ThatT (PP p x)
  eval _ opts x = do
    let msg0 = "'That"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          This _a -> mkNode opts (Fail (msg0 <> " found This")) "" [hh pp]
          That b -> mkNode opts (Val b) "That" [hh pp]
          These _a _b -> mkNode opts (Fail (msg0 <> " found These")) "" [hh pp]


-- | extracts the (a,b) from type level @These a b@ if the value exists
--
-- >>> pz @('These Id Id) (These 123 "abc")
-- Val (123,"abc")
--
-- >>> pz @('These Id 5) (These 123 "abcde")
-- Val (123,5)
--
-- >>> pz @('These Id Id) (This "aaa")
-- Fail "'These found This"
--
-- >>> pz @('These Id Id) (That "aaa")
-- Fail "'These found That"
--
instance ( Show a
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
                      in  mkNode opts (Val ret) (show3 opts msg0 ret (These a b)) [hh pp, hh qq]
         _ -> pure $ mkNode opts (Fail (msg0 <> " found " <> showThese th)) "" []

-- | converts the value to the corresponding 'Proxy'
--
-- >>> pz @'Proxy 'x'
-- Val Proxy
--
instance Show a => P 'Proxy a where
  type PP 'Proxy a = Proxy a
  eval _ opts a =
    let b = Proxy @a
    in pure $ mkNode opts (Val b) ("'Proxy" <> showVerbose opts " | " a) []

pu, pl, pa, pan, panv, pab, pub, pav, puv, pz
  :: forall p a
  . ( Show (PP p a)
    , P p a
    ) => a
      -> IO (Val (PP p a))
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
-- Val 123
--
-- >>> run @('OMsg "field1" ':# OL) @('Left Id) (Right 123)
-- field1 >>> Error 'Left found Right
-- Fail "'Left found Right"
--
-- >>> run @(OptT '[ 'OMsg "test", OU, 'OEmpty, OL, 'OMsg "field2"]) @(Failt _ "oops") ()
-- test | field2 >>> Error oops
-- Fail "oops"
--
run :: forall opts p a
        . ( OptC opts
          , Show (PP p a)
          , P p a
          )
        => a
        -> IO (Val (PP p a))
run a = do
  let opts = getOpt @opts
  pp <- eval (Proxy @p) opts a
  case oDebug opts of
    DZero -> pure ()
    DLite -> unlessNullM (prtTree opts pp) putStrLn
    _ -> unlessNullM (prtTree opts pp) putStrLn
  return (_ttVal pp)

-- | run expression with multiple options in a list
--
-- >>> runs @'[OL, 'OMsg "field2"] @'( 'True, 'False) ()
-- field2 >>> Present (True,False) ('(True,False))
-- Val (True,False)
--
-- >>> runs @'[ 'OMsg "test", OU, 'OEmpty, OL, 'OMsg "field2"] @(Failt _ "oops") ()
-- test | field2 >>> Error oops
-- Fail "oops"
--
runs :: forall optss p a
        . ( OptC (OptT optss)
          , Show (PP p a)
          , P p a
          )
        => a
        -> IO (Val (PP p a))
runs = run @(OptT optss) @p

-- | convenience method to evaluate two expressions using the same input and return the results
runPQ :: ( P p a
         , P q a
         , MonadEval m)
   => String
   -> proxy1 p
   -> proxy2 q
   -> POpts
   -> a
   -> [Tree PE]
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
   -> [Tree PE]
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
evalBoolHide opts
  | isVerbose opts = evalBool (Proxy @p) opts
  | otherwise = evalBool (Proxy @(Hide p)) opts

-- | evaluate a expressions but hide the results unless verbose
evalHide :: forall p a m
  . ( MonadEval m
    , P p a
    )
  => POpts
  -> a
  -> m (TT (PP p a))
evalHide opts
  | isVerbose opts = eval (Proxy @p) opts
  | otherwise = eval (Proxy @(Hide p)) opts


-- advantage of (>>) over 'Do [k] is we can use different kinds for (>>) without having to wrap with 'W'

-- | compose expressions
--
-- >>> pz @L12 ((11,12),'x')
-- Val 12
--
data p >> q
infixr 1 >>

instance ( P p a
         , P q (PP p a)
         , Show (PP p a)
         , Show (PP q (PP p a))
         ) => P (p >> q) a where
  type PP (p >> q) a = PP q (PP p a)
  eval _ opts a = do
    let msg0 = "(>>)"
    pp <- eval (Proxy @p) opts a
    case getValueLR opts "" pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @q) opts p
        pure $ case getValueLR opts (showL opts p) qq [hh pp] of
        -- need to look inside to see if there is already an exception in ttForest
          Left e | isVerbose opts -> e
                 | otherwise ->
                    if anyOf (ttForest . traverse . root . peValP) (has _FailP) qq
                    then qq & ttForest %~ (hh pp:) -- we still need pp for context
                    else e
          Right q -> mkNodeCopy opts qq (lit3 opts msg0 q "" (topMessageEgregious qq)) [hh pp, hh qq]

-- | flipped version of 'Predicate.Core.>>'
data p << q
type LeftArrowsT p q = q >> p
infixr 1 <<

instance P (LeftArrowsT p q) x => P (p << q) x where
  type PP (p << q) x = PP (LeftArrowsT p q) x
  eval _ = eval (Proxy @(LeftArrowsT p q))

-- bearbeiten! only used by >>
topMessageEgregious :: TT a -> String
topMessageEgregious pp = innermost (_ttString pp)
  where innermost = ('{':) . reverse . ('}':) . takeWhile (/='{') . dropWhile (=='}') . reverse

-- | unwraps a value (see '_Wrapped'')
--
-- >>> pz @Unwrap (SG.Sum (-13))
-- Val (-13)
--
-- >>> pl @(Unwrap >> '(Id, 'True)) (SG.Sum 13)
-- Present (13,True) ((>>) (13,True) | {'(13,True)})
-- Val (13,True)
--
data Unwrap

instance ( Show x
         , Show (Unwrapped x)
         , Wrapped x
         ) => P Unwrap x where
  type PP Unwrap x = Unwrapped x
  eval _ opts x =
    let msg0 = "Unwrap"
        d = x ^. _Wrapped'
    in pure $ mkNode opts (Val d) (show3 opts msg0 d x) []

data Wrap' t p

instance ( Show (PP p x)
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
        in mkNode opts (Val d) (show3 opts msg0 d p) [hh pp]

-- | wraps a value (see '_Wrapped'' and '_Unwrapped'')
--
-- >>> pz @(Wrap (SG.Sum _) Id) (-13)
-- Val (Sum {getSum = -13})
--
-- >>> pz @(Wrap SG.Any (Ge 4)) 13
-- Val (Any {getAny = True})
--
-- >>> import Data.List.NonEmpty (NonEmpty(..))
-- >>> pz @(Wrap (NonEmpty _) (Uncons >> 'Just Id)) "abcd"
-- Val ('a' :| "bcd")
--
-- >>> pl @(Wrap (SG.Sum _) Id) 13
-- Present Sum {getSum = 13} (Wrap Sum {getSum = 13} | 13)
-- Val (Sum {getSum = 13})
--
-- >>> pl @(Wrap (SG.Sum _) Id >> STimes 4 Id) 13
-- Present Sum {getSum = 52} ((>>) Sum {getSum = 52} | {getSum = 13})
-- Val (Sum {getSum = 52})
--
-- >>> pl @(Wrap _ 13 <> Id) (SG.Sum @Int 12)
-- Present Sum {getSum = 25} (Sum {getSum = 13} <> Sum {getSum = 12} = Sum {getSum = 25})
-- Val (Sum {getSum = 25})
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
    in pure $ mkNode opts (Fail msg0) "you probably meant to get access to the type of PP only and not evaluate" []

-- | similar to 'length'
--
-- >>> pz @Len [10,4,5,12,3,4]
-- Val 6
--
-- >>> pz @Len []
-- Val 0
--
data Len
instance ( Show a
         , as ~ [a]
         ) => P Len as where
  type PP Len as = Int
  eval _ opts as =
    let msg0 = "Len"
        n = length as
    in pure $ mkNode opts (Val n) (show3 opts msg0 n as) []

-- | similar to 'length' for 'Foldable' instances
--
-- >>> pz @(Length Id) (Left "aa")
-- Val 0
--
-- >>> pz @(Length Id) (Right "aa")
-- Val 1
--
-- >>> pz @(Length Right') (Right "abcd")
-- Val 4
--
-- >>> pz @(Length L23) (True,(23,'x',[10,9,1,3,4,2]))
-- Val 6
--
data Length p

instance ( PP p x ~ t a
         , P p x
         , Show (t a)
         , Foldable t
         ) => P (Length p) x where
  type PP (Length p) x = Int
  eval _ opts x = do
    let msg0 = "Length"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
            let n = length p
            in mkNode opts (Val n) (show3 opts msg0 n p) [hh pp]

-- | 'not' function
--
-- >>> pz @(Not Id) False
-- Val True
--
-- >>> pz @(Not Id) True
-- Val False
--
-- >>> pz @(Not Fst) (True,22)
-- Val False
--
-- >>> pl @(Not (Lt 3)) 13
-- True (Not (13 < 3))
-- Val True
--
-- >>> pl @(Not 'True) ()
-- False (Not ('True))
-- Val False
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
-- >>> pz @('[ 'True] >> Head >> IdBool) ()
-- Val True
--
-- >>> pz @(Fst >> IdBool) (False,22)
-- Val False
--
-- >>> pl @(Head >> IdBool) [True]
-- True ((>>) True | {IdBool})
-- Val True
--
-- >>> pan @(Head >> Id) [True]
-- P (>>) True
-- |
-- +- P Head True
-- |
-- `- P Id True
-- Val True
--
-- >>> pan @(Head >> IdBool) [True]
-- True (>>) True
-- |
-- +- P Head True
-- |
-- `- True IdBool
-- Val True
--

data IdBool

instance x ~ Bool
        => P IdBool x where
  type PP IdBool x = Bool
  eval _ opts x =
    let msg0 = "IdBool"
    in pure $ mkNodeB opts x msg0 []

-- | Fails the computation with a message but allows you to set the output type
--
-- >>> pz @(Failt Int (PrintF "value=%03d" Id)) 99
-- Fail "value=099"
--
-- >>> pz @('False || (Fail 'True "failed")) (99,"somedata")
-- Fail "failed"
--
-- >>> pz @('False || (Fail (Hole Bool) "failed")) (99,"somedata")
-- Fail "failed"
--
-- >>> pz @('False || (Fail (Hole _) "failed")) (99,"somedata")
-- Fail "failed"
--
data Fail t prt

instance ( P prt a
         , PP prt a ~ String
         ) => P (Fail t prt) a where
  type PP (Fail t prt) a = PP t a
  eval _ opts a = do
    let msg0 = "Fail"
    pp <- eval (Proxy @prt) opts a
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right s -> mkNode opts (Fail s) "" (verboseList opts pp)

-- | Fails the computation with a message for simple failures: doesnt preserve types
--
-- >>> pz @(FailS (PrintT "value=%03d string=%s" Id)) (99,"somedata")
-- Fail "value=099 string=somedata"
--
data FailS p
instance P (Fail Id p) x => P (FailS p) x where
  type PP (FailS p) x = PP (Fail Id p) x
  eval _ = eval (Proxy @(Fail Id p))

-- | Fails the computation with a message (wraps the type in 'Hole')
--
-- >>> pz @(Failt Int (PrintF "value=%03d" Id)) 99
-- Fail "value=099"
--
data Failt (t :: Type) p
instance P (Fail (Hole t) p) x => P (Failt t p) x where
  type PP (Failt t p) x = PP (Fail (Hole t) p) x
  eval _ = eval (Proxy @(Fail (Hole t) p))

-- | Fails the computation with a message where the input value is a Proxy
--
-- >>> pz @(Ix 3 (Failp "oops")) "abcd"
-- Val 'd'
--
-- >>> pz @(Ix 3 (Failp "oops")) "abc"
-- Fail "oops"
--
data Failp p
instance P (Fail Unproxy p) x => P (Failp p) x where
  type PP (Failp p) x = PP (Fail Unproxy p) x
  eval _ = eval (Proxy @(Fail Unproxy p))

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
-- >>> pl @OneP [12]
-- Present 12 (OneP)
-- Val 12
--
-- >>> pl @OneP [1..5]
-- Error OneP:expected one element(5)
-- Fail "OneP:expected one element(5)"
--
-- >>> pl @OneP ([] ::[()])
-- Error OneP:expected one element(empty)
-- Fail "OneP:expected one element(empty)"
--
data OneP
instance ( Foldable t
         , x ~ t a
         ) => P OneP x where
  type PP OneP x = ExtractAFromTA x
  eval _ opts x = do
    let msg0 = "OneP"
    pure $ case toList x of
      [] -> mkNode opts (Fail (msg0 <> ":expected one element(empty)")) "" []
      [a] -> mkNode opts (Val a) msg0 []
      as -> let n = length as
            in mkNode opts (Fail (msg0 <> ":expected one element(" <> show n <> ")")) "" []

--type OneP = Guard "expected list of length 1" (Len == 1) >> Head
--type OneP = Guard (PrintF "expected list of length 1 but found length=%d" Len) (Len == 1) >> Head

-- | A predicate that determines if the value is between @p@ and @q@
--
-- >>> pz @(Between 5 8 Len) [1,2,3,4,5,5,7]
-- Val True
--
-- >>> pl @(Between 5 8 Id) 9
-- False (9 <= 8)
-- Val False
--
-- >>> pl @(Between L11 L12 Snd) ((1,4),3)
-- True (1 <= 3 <= 4)
-- Val True
--
-- >>> pl @(Between L11 L12 Snd) ((1,4),10)
-- False (10 <= 4)
-- Val False
--
data Between p q r -- reify as it is used a lot! nicer specific messages at the top level!

instance ( Ord (PP p x)
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


-- | A operator predicate that determines if the value is between @p@ and @q@
--
-- >>> pz @(5 <..> 8) 6
-- Val True
--
-- >>> pz @(10 % 4 <..> 40 % 5) 4
-- Val True
--
-- >>> pz @(10 % 4 <..> 40 % 5) 33
-- Val False
--
data p <..> q
infix 4 <..>

type BetweenT p q = Between p q Id

instance P (BetweenT p q) x => P (p <..> q) x where
  type PP (p <..> q) x = PP (BetweenT p q) x
  eval _ = evalBool (Proxy @(BetweenT p q))

-- | similar to 'all'
--
-- >>> pl @(All (Between 1 8 Id)) [7,3,4,1,2,9,0,1]
-- False (All(8) i=5 (9 <= 8))
-- Val False
--
-- >>> pz @(All Odd) [1,5,11,5,3]
-- Val True
--
-- >>> pz @(All Odd) []
-- Val True
--
-- >>> run @OANV @(All Even) [1,5,11,5,3]
-- False All(5) i=0 (1 == 0)
-- |
-- +- False i=0: 1 == 0
-- |  |
-- |  +- P 1 `mod` 2 = 1
-- |  |  |
-- |  |  +- P Id 1
-- |  |  |
-- |  |  `- P '2
-- |  |
-- |  `- P '0
-- |
-- +- False i=1: 1 == 0
-- |  |
-- |  +- P 5 `mod` 2 = 1
-- |  |  |
-- |  |  +- P Id 5
-- |  |  |
-- |  |  `- P '2
-- |  |
-- |  `- P '0
-- |
-- +- False i=2: 1 == 0
-- |  |
-- |  +- P 11 `mod` 2 = 1
-- |  |  |
-- |  |  +- P Id 11
-- |  |  |
-- |  |  `- P '2
-- |  |
-- |  `- P '0
-- |
-- +- False i=3: 1 == 0
-- |  |
-- |  +- P 5 `mod` 2 = 1
-- |  |  |
-- |  |  +- P Id 5
-- |  |  |
-- |  |  `- P '2
-- |  |
-- |  `- P '0
-- |
-- `- False i=4: 1 == 0
--    |
--    +- P 3 `mod` 2 = 1
--    |  |
--    |  +- P Id 3
--    |  |
--    |  `- P '2
--    |
--    `- P '0
-- Val False
--
-- >>> pl @(Fst >> All (Gt 3)) ([10,12,3,5],"ss")
-- False ((>>) False | {All(4) i=2 (3 > 3)})
-- Val False
--
-- >>> pl @(All (Lt 3)) [1::Int .. 10]
-- False (All(10) i=2 (3 < 3))
-- Val False
--
data All p

instance ( P p a
         , PP p a ~ Bool
         , x ~ f a
         , Show a
         , Foldable f
         ) => P (All p) x where
  type PP (All p) x = Bool
  eval _ opts x = do
    let msg0 = "All"
    case chkSize opts msg0 x [] of
      Left e -> pure e
      Right xs -> do
        ts <- zipWithM (\i a -> ((i, a),) <$> evalBoolHide @p opts a) [0::Int ..] xs
        pure $ case splitAndAlign opts msg0 ts of
             Left e -> e
             Right abcs ->
               let hhs = map (hh . prefixNumberToTT) ts
                   msg1 = msg0 ++ "(" ++ showL opts (length x) ++ ")"
               in case find (not . view _1) abcs of
                    Nothing -> mkNodeB opts True msg1 hhs
                    Just (_,(i,_),tt) ->
                      mkNodeB opts False (msg1 <> " i=" ++ showIndex i ++ " " <> topMessage tt) hhs

-- | similar to 'any'
--
-- >>> pl @(Any Even) [1,5,11,5,3]
-- False (Any(5))
-- Val False
--
-- >>> pl @(Any Even) [1,5,112,5,3]
-- True (Any(5) i=2 (0 == 0))
-- Val True
--
-- >>> pz @(Any Even) []
-- Val False
--
-- >>> pl @(Fst >> Any (Gt 3)) ([10,12,3,5],"ss")
-- True ((>>) True | {Any(4) i=0 (10 > 3)})
-- Val True
--
-- >>> pl @(Any (Same 2)) [1,4,5]
-- False (Any(3))
-- Val False
--
-- >>> pl @(Any (Same 2)) [1,4,5,2,1]
-- True (Any(5) i=3 (2 == 2))
-- Val True
--
data Any p

instance ( P p a
         , PP p a ~ Bool
         , x ~ f a
         , Show a
         , Foldable f
         ) => P (Any p) x where
  type PP (Any p) x = Bool
  eval _ opts x = do
    let msg0 = "Any"
    case chkSize opts msg0 x [] of
      Left e -> pure e
      Right xs -> do
        ts <- zipWithM (\i a -> ((i, a),) <$> evalBoolHide @p opts a) [0::Int ..] xs
        pure $ case splitAndAlign opts msg0 ts of
             Left e -> e
             Right abcs ->
               let hhs = map (hh . prefixNumberToTT) ts
                   msg1 = msg0 ++ "(" ++ showL opts (length xs) ++ ")"
               in case find (view _1) abcs of
                    Nothing -> mkNodeB opts False msg1 hhs
                    Just (_,(i,_),tt) ->
                      mkNodeB opts True (msg1 <> " i=" ++ showIndex i ++ " " <> topMessage tt) hhs

-- | similar to 'fst'
--
-- >>> pz @Fst (10,"Abc")
-- Val 10
--
-- >>> pz @Fst (10,"Abc",'x')
-- Val 10
--
-- >>> pz @Fst (10,"Abc",'x',False)
-- Val 10
--
-- >>> pl @Fst (99,'a',False,1.3)
-- Present 99 (Fst 99 | (99,'a',False,1.3))
-- Val 99
--
data L1 p

instance ( Show (ExtractL1T (PP p x))
         , ExtractL1C (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (L1 p) x where
  type PP (L1 p) x = ExtractL1T (PP p x)
  eval _ opts x = do
    let msg0 = "Fst"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL1C p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

data Fst
type FstT = L1 Id

instance P FstT x => P Fst x where
  type PP Fst x = PP FstT x
  eval _ = eval (Proxy @FstT)

-- | similar to 'snd'
--
-- >>> pz @Snd (10,"Abc")
-- Val "Abc"
--
-- >>> pz @Snd (10,"Abc",True)
-- Val "Abc"
--
-- >>> pl @Snd (99,'a',False,1.3)
-- Present 'a' (Snd 'a' | (99,'a',False,1.3))
-- Val 'a'
--
data L2 p

instance ( Show (ExtractL2T (PP p x))
         , ExtractL2C (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (L2 p) x where
  type PP (L2 p) x = ExtractL2T (PP p x)
  eval _ opts x = do
    let msg0 = "Snd"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL2C p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

data Snd
type SndT = L2 Id

instance P SndT x => P Snd x where
  type PP Snd x = PP SndT x
  eval _ = eval (Proxy @SndT)

-- | similar to 3rd element in a n-tuple
--
-- >>> pz @Thd (10,"Abc",133)
-- Val 133
--
-- >>> pz @Thd (10,"Abc",133,True)
-- Val 133
--
-- >>> pl @Thd (99,'a',False,1.3)
-- Present False (Thd False | (99,'a',False,1.3))
-- Val False
--
data L3 p

instance ( Show (ExtractL3T (PP p x))
         , ExtractL3C (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (L3 p) x where
  type PP (L3 p) x = ExtractL3T (PP p x)
  eval _ opts x = do
    let msg0 = "Thd"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL3C p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

data Thd
type ThdT = L3 Id

instance P ThdT x => P Thd x where
  type PP Thd x = PP ThdT x
  eval _ = eval (Proxy @ThdT)

-- | similar to 4th element in a n-tuple
--
-- >>> pz @(L4 Id) (10,"Abc",'x',True)
-- Val True
--
-- >>> pz @(L4 L21) ('x',((10,"Abc",'x',999),"aa",1),9)
-- Val 999
--
-- >>> pl @(L4 Id) (99,'a',False,"someval")
-- Present "someval" (L4 "someval" | (99,'a',False,"someval"))
-- Val "someval"
--
data L4 p

instance ( Show (ExtractL4T (PP p x))
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
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

-- | similar to 5th element in a n-tuple
--
-- >>> pz @(L5 Id) (10,"Abc",'x',True,1)
-- Val 1
--
data L5 p

instance ( Show (ExtractL5T (PP p x))
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
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]


-- | similar to 6th element in a n-tuple
--
-- >>> pz @(L6 Id) (10,"Abc",'x',True,1,99)
-- Val 99
--
data L6 p

instance ( Show (ExtractL6T (PP p x))
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
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

-- | similar to 'map'
--
-- >>> pz @(Map Pred Id) [1..5]
-- Val [0,1,2,3,4]
--
data Map p q

instance ( Show (PP p a)
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
        case chkSize opts msg0 (toList q) [hh qq] of
          Left e -> pure e
          Right xs -> do
            ts <- zipWithM (\i a -> ((i, a),) <$> evalHide @p opts a) [0::Int ..] xs
            pure $ case splitAndAlign opts msg0 ts of
                 Left e -> e
                 Right abcs ->
                   let vals = map (view _1) abcs
                   in mkNode opts (Val vals) (show3 opts msg0 vals q) (hh qq : map (hh . prefixNumberToTT) ts)

-- | processes a type level list predicates running each in sequence: see 'Predicate.>>'
--
-- >>> pz @(Do [Pred, ShowP Id, Id &&& Len]) 9876543
-- Val ("9876542",7)
--
-- >>> pz @(Do '[W 123, W "xyz", Len &&& Id, Pred *** Id<>Id]) ()
-- Val (2,"xyzxyz")
--
-- >>> pl @(Do '[Succ,Id,ShowP Id,Ones,Map (ReadBase Int 8) Id]) 1239
-- Present [1,2,4,0] ((>>) [1,2,4,0] | {Map [1,2,4,0] | ["1","2","4","0"]})
-- Val [1,2,4,0]
--
-- >>> pl @(Do '[Pred,Id,ShowP Id,Ones,Map (ReadBase Int 8) Id]) 1239
-- Error invalid base 8 (Map(i=3, a="8") excnt=1)
-- Fail "invalid base 8"
--
-- >>> pl @(Do '[4,5,6]) ()
-- Present 6 ((>>) 6 | {'6})
-- Val 6
--
-- >>> pl @(Do '["abc", "Def", "ggg", "hhhhh"]) ()
-- Present "hhhhh" ((>>) "hhhhh" | {'"hhhhh"})
-- Val "hhhhh"
--
-- >>> pl @(Do '[ 'LT, 'EQ, 'GT ]) ()
-- Present GT ((>>) GT | {'GT})
-- Val GT
--
-- >>> pl @(Do '[4 % 4,22 % 1 ,12 -% 4]) ()
-- Present (-3) % 1 ((>>) (-3) % 1 | {Negate (-3) % 1 | 3 % 1})
-- Val ((-3) % 1)
--
-- >>> pl @(Do '[1,2,3]) ()
-- Present 3 ((>>) 3 | {'3})
-- Val 3
--

data Do (ps :: [k])

instance (P (DoExpandT ps) a) => P (Do ps) a where
  type PP (Do ps) a = PP (DoExpandT ps) a
  eval _ = eval (Proxy @(DoExpandT ps))

type family DoExpandT (ps :: [k]) :: Type where
  DoExpandT '[] = GL.TypeError ('GL.Text "'[] invalid: requires at least one predicate in the list")
  DoExpandT '[p] = Id >> p -- need this else fails cos 1 is nat and would mean that the result is nat not Type!
  -- if p >> Id then turns Val True to Val True
  DoExpandT (p ': p1 ': ps) = p >> DoExpandT (p1 ': ps)

-- | similar to 'Prelude.&&'
--
-- >>> pz @(Fst && Snd) (True, True)
-- Val True
--
-- >>> pz @(Id > 15 && Id < 17) 16
-- Val True
--
-- >>> pz @(Id > 15 && Id < 17) 30
-- Val False
--
-- >>> pz @(Fst && (Length Snd >= 4)) (True,[11,12,13,14])
-- Val True
--
-- >>> pz @(Fst && (Length Snd == 4)) (True,[12,11,12,13,14])
-- Val False
--
data p && q
infixr 3 &&

instance ( P p a
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
-- Val False
--
-- >>> pl @(Id > 10 &&~ Id == 12) 11
-- False (True &&~ False | (11 == 12))
-- Val False
--
-- >>> pl @(Id > 10 &&~ Id == 11) 11
-- True (True &&~ True)
-- Val True
--
data p &&~ q
infixr 3 &&~

instance ( P p a
         , P q a
         , PP p a ~ Bool
         , PP q a ~ Bool
         ) => P (p &&~ q) a where
  type PP (p &&~ q) a = Bool
  eval _ opts a = do
    let msg0 = "&&~"
    pp <- evalBool (Proxy @p) opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right False ->
        pure $ mkNodeB opts False ("False " <> msg0 <> " _" <> litVerbose opts " | " (topMessage pp)) [hh pp]
      Right True -> do
        qq <- evalBool (Proxy @q) opts a
        pure $ case getValueLR opts msg0 qq [hh pp] of
          Left e -> e
          Right q ->
            let zz = if q then ""
                     else " | " <> topMessage qq
            in mkNodeB opts q ("True " <> msg0 <> " " <> showL opts q <> litVerbose opts "" zz) [hh pp, hh qq]

-- | similar to 'Prelude.||'
--
-- >>> pz @(Fst || (Length Snd >= 4)) (False,[11,12,13,14])
-- Val True
--
-- >>> pz @(Not Fst || (Length Snd == 4)) (True,[12,11,12,13,14])
-- Val False
--
data p || q
infixr 2 ||

instance ( P p a
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
-- Val True
--
-- >>> pz @(Id > 10 ||~ Id == 9) 9
-- Val True
--
-- >>> pl @(Id > 10 ||~ Id > 9) 9
-- False (False ||~ False | (9 > 10) ||~ (9 > 9))
-- Val False
--
data p ||~ q
infixr 2 ||~

instance ( P p a
         , P q a
         , PP p a ~ Bool
         , PP q a ~ Bool
         ) => P (p ||~ q) a where
  type PP (p ||~ q) a = Bool
  eval _ opts a = do
    let msg0 = "||~"
    pp <- evalBool (Proxy @p) opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right False -> do
        qq <- evalBool (Proxy @q) opts a
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
-- >>> pz @(Fst ~> (Length Snd >= 4)) (True,[11,12,13,14])
-- Val True
--
-- >>> pz @(Fst ~> (Length Snd == 4)) (True,[12,11,12,13,14])
-- Val False
--
-- >>> pz @(Fst ~> (Length Snd == 4)) (False,[12,11,12,13,14])
-- Val True
--
-- >>> pz @(Fst ~> (Length Snd >= 4)) (False,[11,12,13,14])
-- Val True
--
data p ~> q
infixr 1 ~>

instance ( P p a
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
-- Val (Right 123)
--
-- >>> pz @Swap (Right 123)
-- Val (Left 123)
--
-- >>> pz @Swap (These 'x' 123)
-- Val (These 123 'x')
--
-- >>> pz @Swap (This 'x')
-- Val (That 'x')
--
-- >>> pz @Swap (That 123)
-- Val (This 123)
--
-- >>> pz @Swap (123,'x')
-- Val ('x',123)
--
-- >>> pz @Swap (Left "abc")
-- Val (Right "abc")
--
-- >>> pz @Swap (Right 123)
-- Val (Left 123)
--
-- >>> pl @Swap (Right "asfd")
-- Present Left "asfd" (Swap Left "asfd" | Right "asfd")
-- Val (Left "asfd")
--
-- >>> pl @Swap (12,"asfd")
-- Present ("asfd",12) (Swap ("asfd",12) | (12,"asfd"))
-- Val ("asfd",12)
--
-- >>> pz @Swap (True,12,"asfd")
-- Val (True,"asfd",12)
--
data Swap

class Bifunctor p => SwapC p where
  swapC :: p a b -> p b a
instance SwapC Either where
  swapC (Left a) = Right a
  swapC (Right a) = Left a
instance SwapC These where
  swapC (This a) = That a
  swapC (That b) = This b
  swapC (These a b) = These b a
instance SwapC SG.Arg where
  swapC (SG.Arg a b) = SG.Arg b a
instance SwapC (,) where
  swapC (a,b) = (b,a)
instance SwapC ((,,) a) where
  swapC (a,b,c) = (a,c,b)
instance SwapC ((,,,) a b) where
  swapC (a,b,c,d) = (a,b,d,c)
instance SwapC ((,,,,) a b c) where
  swapC (a,b,c,d,e) = (a,b,c,e,d)
instance SwapC ((,,,,,) a b c d) where
  swapC (a,b,c,d,e,f) = (a,b,c,d,f,e)

instance ( Show (p a b)
         , SwapC p
         , Show (p b a)
         ) => P Swap (p a b) where
  type PP Swap (p a b) = p b a
  eval _ opts pabx =
    let msg0 = "Swap"
        d = swapC pabx
    in pure $ mkNode opts (Val d) (show3 opts msg0 d pabx) []

-- | like 'GHC.Base.$' for expressions
--
-- >>> pl @(L1 $ L2 $ Id) ((1,2),(3,4))
-- Present 3 (Fst 3 | (3,4))
-- Val 3
--
-- >>> pl @((<=) 4 $ L1 $ L2 $ Id) ((1,2),(3,4))
-- False (4 <= 3)
-- Val False
--
data (p :: k -> k1) $ (q :: k)
infixr 0 $

instance P (p q) a => P (p $ q) a where
  type PP (p $ q) a = PP (p q) a
  eval _  = eval (Proxy @(p q))

-- | similar to 'Control.Lens.&'
--
-- >>> pl @(Id & L1 & Singleton & Length) (13,"xyzw")
-- Present 1 (Length 1 | [13])
-- Val 1
--
-- >>> pl @(2 & (&&&) "abc") ()
-- Present ("abc",2) (W '("abc",2))
-- Val ("abc",2)
--
-- >>> pl @(2 & '(,) "abc") ()
-- Present ("abc",2) ('("abc",2))
-- Val ("abc",2)
--
-- >>> pl @('(,) 4 $ '(,) 7 $ "aa") ()
-- Present (4,(7,"aa")) ('(4,(7,"aa")))
-- Val (4,(7,"aa"))
--
-- >>> pl @(L3 $ L2 $ Fst) ((1,("W",9,'a')),(3,4))
-- Present 'a' (Thd 'a' | ("W",9,'a'))
-- Val 'a'
--
data (q :: k) & (p :: k -> k1)
infixl 1 &

instance P (p q) a => P (q & p) a where
  type PP (q & p) a = PP (p q) a
  eval _ = eval (Proxy @(p q))

-- | similar to 'pure'
--
-- >>> pz @(Pure Maybe Id) 4
-- Val (Just 4)
--
-- >>> pz @(Pure [] Id) 4
-- Val [4]
--
-- >>> pz @(Pure (Either String) Fst) (13,True)
-- Val (Right 13)
--
-- >>> pl @(Pure Maybe Id) 'x'
-- Present Just 'x' (Pure Just 'x' | 'x')
-- Val (Just 'x')
--
-- >>> pl @(Pure (Either _) Id) 'x'
-- Present Right 'x' (Pure Right 'x' | 'x')
-- Val (Right 'x')
--
-- >>> pl @(Pure (Either _) Id >> Swap) 'x'
-- Present Left 'x' ((>>) Left 'x' | {Swap Left 'x' | Right 'x'})
-- Val (Left 'x')
--
-- >>> pl @(Pure (Either ()) Id >> Swap) 'x'
-- Present Left 'x' ((>>) Left 'x' | {Swap Left 'x' | Right 'x'})
-- Val (Left 'x')
--
-- >>> pl @(Pure (Either String) Id >> Swap) 123
-- Present Left 123 ((>>) Left 123 | {Swap Left 123 | Right 123})
-- Val (Left 123)
--
data Pure (t :: Type -> Type) p
instance ( P p x
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
        in mkNode opts (Val b) (show3 opts msg0 b a) [hh pp]

-- | similar to 'Data.Coerce.coerce'
--
-- >>> pz @(Coerce (SG.Sum Integer)) (Identity (-13))
-- Val (Sum {getSum = -13})
--
-- >>> pl @(Coerce SG.Any) True
-- Present Any {getAny = True} (Coerce Any {getAny = True} | True)
-- Val (Any {getAny = True})
--
-- >>> pl @(Coerce Bool) (SG.Any True)
-- Present True (Coerce True | Any {getAny = True})
-- Val True
--
data Coerce (t :: k)

instance ( Show a
         , Show t
         , Coercible t a
         ) => P (Coerce t) a where
  type PP (Coerce t) a = t
  eval _ opts a =
    let msg0 = "Coerce"
        d = a ^. coerced
    in pure $ mkNode opts (Val d) (show3 opts msg0 d a) []

{-
 -- | extracts the value level representation of the promoted 'DayOfWeek'
 --
 -- >>> pz @'Monday ()
 -- Val Monday
 --
 -- >>> pz @'Sunday ()
 -- Val Sunday
 --
instance GetWeekDay dy => P (dy :: DayOfWeek) a where
  type PP dy a = DayOfWeek
  eval _ opts _a =
    let dy = getWeekDay @dy
        msg = "'" <> showL opts dy
    in pure $ mkNode opts (Val dy) msg []

-- | get weekday from the typelevel
class GetWeekDay (dy :: DayOfWeek) where
  getWeekDay :: DayOfWeek
instance GetWeekDay 'Sunday where
  getWeekDay = Sunday
instance GetWeekDay 'Monday where
  getWeekDay = Monday
instance GetWeekDay 'Tuesday where
  getWeekDay = Tuesday
instance GetWeekDay 'Wednesday where
  getWeekDay = Wednesday
instance GetWeekDay 'Thursday where
  getWeekDay = Thursday
instance GetWeekDay 'Friday where
  getWeekDay = Friday
instance GetWeekDay 'Saturday where
  getWeekDay = Saturday
-}

-- | first element in a tuple followed by the first element
--
-- >>> pz @L11 ((10,"ss"),2)
-- Val 10
--
data L11
type L11T = L1 (L1 Id)

instance P L11T x => P L11 x where
  type PP L11 x = PP L11T x
  eval _ = eval (Proxy @L11T)

-- | first element in a tuple followed by the second element
--
-- >>> pz @L12 ((10,"ss"),2)
-- Val "ss"
--
data L12
type L12T = L2 (L1 Id)

instance P L12T x => P L12 x where
  type PP L12 x = PP L12T x
  eval _ = eval (Proxy @L12T)

-- | first element in a tuple followed by the third element
--
-- >>> pz @L13 ((10,"ss",4.5),2)
-- Val 4.5
--
data L13
type L13T = L3 (L1 Id)

instance P L13T x => P L13 x where
  type PP L13 x = PP L13T x
  eval _ = eval (Proxy @L13T)

-- | second element in a tuple followed by the first element
--
-- >>> pz @L21 ('x',(10,"ss",4.5),2)
-- Val 10
--
data L21
type L21T = L1 (L2 Id)

instance P L21T x => P L21 x where
  type PP L21 x = PP L21T x
  eval _ = eval (Proxy @L21T)

-- | second element in a tuple followed by the second element
--
-- >>> pz @L22 ('z',(10,"ss",4.5),2)
-- Val "ss"
--
data L22
type L22T = L2 (L2 Id)

instance P L22T x => P L22 x where
  type PP L22 x = PP L22T x
  eval _ = eval (Proxy @L22T)

-- | second element in a tuple followed by the third element
--
-- >>> pz @L23 ('x',(10,"ss",4.5),2)
-- Val 4.5
--
data L23
type L23T = L3 (L2 Id)

instance P L23T x => P L23 x where
  type PP L23 x = PP L23T x
  eval _ = eval (Proxy @L23T)

-- | third element in a tuple followed by the first element
--
-- >>> pz @L31 (1,2,('c',4))
-- Val 'c'
--
data L31
type L31T = L1 (L3 Id)

instance P L31T x => P L31 x where
  type PP L31 x = PP L31T x
  eval _ = eval (Proxy @L31T)

-- | third element in a tuple followed by the second element
--
-- >>> pz @L32 (1,2,('c',4))
-- Val 4
--
data L32
type L32T = L2 (L3 Id)

instance P L32T x => P L32 x where
  type PP L32 x = PP L32T x
  eval _ = eval (Proxy @L32T)

-- | third element in a tuple followed by the third element
--
-- >>> pz @L33 (1,2,('c',4,False))
-- Val False
--
data L33
type L33T = L3 (L3 Id)

instance P L33T x => P L33 x where
  type PP L33 x = PP L33T x
  eval _ = eval (Proxy @L33T)

