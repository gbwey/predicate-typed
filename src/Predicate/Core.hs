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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE NoStarIsType #-}
-- | a dsl for evaluating and displaying type level expressions
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
  , UnproxyT
  , Len
  , Length
  , Map'
  , Map
  , Do
  , Swap
  , Arg'

  -- ** impure evaluation
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
  , unsafeEval

  -- ** pure evaluation
  , runP
  , runPQ
  , runPQBool
  , evalBool
  , evalBoolHide
  , evalHide
  , evalQuick
  , evalEither

 -- ** wrap, unwrap
  , Wrap
  , Wrap'
  , Unwrap

 -- ** failure
  , Fail
  , FailP
  , FailT
  , FailS

 -- ** tuple
  , Fst
  , Snd
  , Thd
  , L1
  , L2
  , L3
  , L4
  , L5
  , L6
  , L7
  , L8
  , L11
  , L12
  , L13
  , L21
  , L22
  , L23
  , L31
  , L32
  , L33

  -- ** boolean
  , type (&&)
  , type (&&~)
  , type (||)
  , type (||~)
  , type (~>)
  , Not
  , Between
  , type (<..>)
  , All
  , Any
  , IdBool

 -- ** type application
  , type (>>)
  , type (>>>)
  , type (<<)
  , type ($)
  , type (&)
  , DoL

 -- ** core class
  , P(..)

 -- ** type families
  , DoExpandT
  , DoExpandLT
  , ArgT
  ) where
import Predicate.Misc
import Predicate.Util
import Predicate.Elr
import qualified GHC.TypeLits as GL
import GHC.TypeLits (Symbol,Nat,KnownSymbol,KnownNat,ErrorMessage((:$$:),(:<>:)))
import Control.Lens
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Data.Kind (Type)
import Data.These (These(..))
import Control.Monad (zipWithM)
import Control.Arrow (right)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Tree (Tree)
import Data.Tree.Lens (root)
import qualified Text.Regex.PCRE.Heavy as RH
import GHC.Stack (HasCallStack)
import qualified Data.Semigroup as SG
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate
-- >>> import Data.Time
-- >>> :m + Control.Lens
-- >>> :m + Control.Lens.Action
-- >>> :m + Data.Typeable
-- >>> :m + Text.Show.Functions
-- >>> :m + Data.Ratio
-- >>> import qualified Data.Semigroup as SG

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
evalBool p opts = fmap fixTTBool . eval p opts

-- | A specialised form of 'eval' that returns the result or the error string on failure
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
data Id deriving Show
instance Show a => P Id a where
  type PP Id a = a
  eval _ opts a =
    let msg0 = "Id"
    in pure $ mkNode opts (Val a) (msg0 <> " " <> showL opts a) []

-- | identity function that also displays the type information for debugging
--
-- >>> pz @IdT 23
-- Val 23
data IdT deriving Show
instance ( Typeable a
         , Show a
         ) => P IdT a where
  type PP IdT a = a
  eval _ opts a =
    let msg0 = "IdT(" <> t <> ")"
        t = showT @a
    in pure $ mkNode opts (Val a) (msg0 <> " " <> showL opts a) []

-- | transparent wrapper to turn kind k into kind 'Type'
--   eg useful for putting in a promoted list (cant mix kinds) see 'Predicate.Core.Do'
--
-- >>> pz @'[W 123, Id] 99
-- Val [123,99]
--
-- >>> pz @'[W "abc", W "def", Id, Id] "ghi"
-- Val ["abc","def","ghi","ghi"]
--
data W (p :: k) deriving Show
instance P p a => P (W p) a where
  type PP (W p) a = PP p a
  eval _ opts | isVerbose opts = eval (Proxy @(MsgI "W " p)) opts
              | otherwise = eval (Proxy @p) opts

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
data Msg prt p deriving Show

instance ( P prt a
         , PP prt a ~ String
         , P p a
         ) => P (Msg prt p) a where
  type PP (Msg prt p) a = PP p a
  eval _ opts a = do
    pp <- eval (Proxy @prt) opts a
    case getValueLR NoInline opts "Msg" pp [] of
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
data MsgI prt p deriving Show

instance ( P prt a
         , PP prt a ~ String
         , P p a
         ) => P (MsgI prt p) a where
  type PP (MsgI prt p) a = PP p a
  eval _ opts a = do
    pp <- eval (Proxy @prt) opts a
    case getValueLR NoInline opts "MsgI" pp [] of
      Left e -> pure e
      Right msg -> prefixMsg msg <$> eval (Proxy @p) opts a

-- | run the expression @p@ but remove the subtrees
data Hide p deriving Show

instance P p x => P (Hide p) x where
  type PP (Hide p) x = PP p x
  eval _ opts x = do
    tt <- eval (Proxy @p) opts x
    pure $ tt & ttForest .~ []


-- | Acts as a proxy for a Type.
data Hole (t :: Type) deriving Show

instance Typeable t => P (Hole t) a where
  type PP (Hole t) a = t -- can only be Type not Type -> Type (can use Proxy but then we go down the rabbithole)
  eval _ opts _ =
    let msg0 = "Hole(" <> showT @t <> ")"
    in pure $ mkNode opts (Fail msg0) "you probably meant to get access to the type of PP only and not evaluate" []

-- | override the display width for the expression @p@
data Width (n :: Nat) p deriving Show

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

-- | create a Proxy for a kind @t@
--
-- >>> pz @(Proxy 4) ()
-- Val Proxy
--
-- >>> pz @(Proxy Int) ()
-- Val Proxy
--
-- >>> pz @(Proxy "abc" >> Pop0 Id ()) ()
-- Val "abc"
--
instance P (Proxy t) a where
  type PP (Proxy t) a = Proxy t
  eval _ opts _ =
    let msg0 = "Proxy"
    in pure $ mkNode opts (Val Proxy) msg0 []

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
    lr <- runPQ NoInline msg (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
       Left e -> e
       Right (p,q,pp,qq) ->
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
    lr <- runPQ NoInline msg (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
         let hhs0 = [hh pp, hh qq]
         rr <- eval (Proxy @r) opts a
         pure $ case getValueLR NoInline opts msg rr hhs0 of
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
    lr <- runPQ NoInline msg (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs0 = [hh pp, hh qq]
        lr1 <- runPQ NoInline msg (Proxy @r) (Proxy @s) opts a hhs0
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
    lr <- runPQ NoInline msg (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs0 = [hh pp, hh qq]
        lr1 <- runPQ NoInline msg (Proxy @r) (Proxy @s) opts a hhs0
        case lr1 of
          Left e -> pure e
          Right (r,s,rr,ss) -> do
            let hhs1 = hhs0 ++ [hh rr, hh ss]
            tt <- eval (Proxy @t) opts a
            pure $ case getValueLR NoInline opts msg tt hhs1 of
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
    lr <- runPQ NoInline msg (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs0 = [hh pp, hh qq]
        lr1 <- runPQ NoInline msg (Proxy @r) (Proxy @s) opts a hhs0
        case lr1 of
          Left e -> pure e
          Right (r,s,rr,ss) -> do
            let hhs1 = hhs0 ++ [hh rr, hh ss]
            lr2 <- runPQ NoInline msg (Proxy @t) (Proxy @u) opts a hhs1
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
    lr <- runPQ NoInline msg (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs0 = [hh pp, hh qq]
        lr1 <- runPQ NoInline msg (Proxy @r) (Proxy @s) opts a hhs0
        case lr1 of
          Left e -> pure e
          Right (r,s,rr,ss) -> do
            let hhs1 = hhs0 ++ [hh rr, hh ss]
            lr2 <- runPQ NoInline msg (Proxy @t) (Proxy @u) opts a hhs1
            case lr2 of
              Left e -> pure e
              Right (t,u,tt,uu) -> do
                vv <- eval (Proxy @v) opts a
                let hhs2 = hhs1 ++ [hh tt, hh uu]
                pure $ case getValueLR NoInline opts msg vv hhs2 of
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
    lr <- runPQ NoInline msg (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs0 = [hh pp, hh qq]
        lr1 <- runPQ NoInline msg (Proxy @r) (Proxy @s) opts a hhs0
        case lr1 of
          Left e -> pure e
          Right (r,s,rr,ss) -> do
            let hhs1 = hhs0 ++ [hh rr, hh ss]
            lr2 <- runPQ NoInline msg (Proxy @t) (Proxy @u) opts a hhs1
            case lr2 of
              Left e -> pure e
              Right (t,u,tt,uu) -> do
                let hhs2 = hhs1 ++ [hh tt, hh uu]
                lr3 <- runPQ NoInline msg (Proxy @v) (Proxy @w) opts a hhs2
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
  eval _ opts _ =
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
    pure $ case getValueLR NoInline opts msg0 pp [] of
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
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @(p1 ': ps)) opts a
        pure $ case getValueLR Inline opts "" qq [hh pp] of
          Left e -> e
          Right q ->
            let ret = p:q
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
    pure $ case getValueLR NoInline opts msg0 pp [] of
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
    pure $ case getValueLR NoInline opts msg0 pp [] of
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
    pure $ case getValueLR NoInline opts msg0 pp [] of
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
    pure $ case getValueLR NoInline opts msg0 pp [] of
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
    pure $ case getValueLR NoInline opts msg0 pp [] of
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
            case getValueLR NoInline opts msg0 pp [] of
               Left e -> pure e
               Right p -> do
                 qq <- eval (Proxy @q) opts b
                 pure $ case getValueLR NoInline opts (msg0 <> " q failed p=" <> showL opts p) qq [hh pp] of
                    Left e -> e
                    Right q ->
                      let ret =(p,q)
                      in  mkNode opts (Val ret) (show3 opts msg0 ret (These a b)) [hh pp, hh qq]
         _ -> pure $ mkNode opts (Fail (msg0 <> " found " <> showThese th)) "" []

-- | converts the type to the corresponding 'Proxy'
--
-- >>> pz @'Proxy 'x' ^!? acts . _Val . to typeRep
-- Just Char
--
-- >>> pz @'Proxy 'x' >>= return . preview (_Val . to typeRep)
-- Just Char
--
-- >>> pz @'Proxy 45 ^!? acts . _Val . to typeRep
-- Just Integer
--
-- >>> pz @'Proxy "abc" ^!? acts . _Val . to typeRep
-- Just [Char]
--
-- >>> pz @(Pop1' (Proxy ToEnum) 'Proxy 2) LT
-- Val GT
--
instance P 'Proxy t where
  type PP 'Proxy t = Proxy t
  eval _ opts _ =
    let b = Proxy @t
    in pure $ mkNode opts (Val b) "'Proxy" []

-- | evaluate the type level expression in IO
--
-- >>> pl @(Between 4 10 Id) 7 & mapped . _Val %~ not
-- True (4 <= 7 <= 10)
-- Val False
--
-- >>> eval (Proxy @'True) defOpts 7 & mapped . ttValBool . _Val %~ not
-- TT {_ttValP = FalseP, _ttVal = Val False, _ttString = "'True", _ttForest = []}
--

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
--   pu @'(Id, "abc", 'True) [1..4]
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
-- >>> run @(OptT '[ 'OMsg "test", OU, 'OEmpty, OL, 'OMsg "field2"]) @(FailT _ "oops") ()
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
    _ -> unlessNullM (prtTree opts pp) putStrLn
  return (pp ^. ttVal)

-- | run expression with multiple options in a list
--
-- >>> runs @'[OL, 'OMsg "field2"] @'( 'True, 'False) ()
-- field2 >>> Present (True,False) ('(True,False))
-- Val (True,False)
--
-- >>> runs @'[ 'OMsg "test", OU, 'OEmpty, OL, 'OMsg "field2"] @(FailT _ "oops") ()
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

-- | convenience method to evaluate one expression
runP ::  ( P p a
         , MonadEval m)
   => Inline
   -> String
   -> proxy p
   -> POpts
   -> a
   -> [Tree PE]
   -> m (Either (TT x) (PP p a, TT (PP p a)))
runP inline msg0 proxyp opts a hhs = do
    pp <- eval proxyp opts a
    return $ right (,pp) $ getValueLR inline opts msg0 pp hhs

-- | convenience method to evaluate two expressions using the same input and return the results
runPQ :: ( P p a
         , P q a
         , MonadEval m)
   => Inline
   -> String
   -> proxy1 p
   -> proxy2 q
   -> POpts
   -> a
   -> [Tree PE]
   -> m (Either (TT x) (PP p a, PP q a, TT (PP p a), TT (PP q a)))
runPQ inline msg0 proxyp proxyq opts a hhs = do
    pp <- eval proxyp opts a
    case getValueLR inline opts msg0 pp hhs of
      Left e -> pure $ Left e
      Right p -> do
         qq <- eval proxyq opts a
         pure $ case getValueLR inline opts msg0 qq (hhs <> [hh pp]) of
           Left e -> Left e
           Right q -> Right (p, q, pp, qq)

-- | convenience method to evaluate two boolean expressions using the same input and return the results
runPQBool :: ( P p a
             , PP p a ~ Bool
             , P q a
             , PP q a ~ Bool, MonadEval m)
   => Inline
   -> String
   -> proxy1 p
   -> proxy2 q
   -> POpts
   -> a
   -> [Tree PE]
   -> m (Either (TT x) (PP p a, PP q a, TT (PP p a), TT (PP q a)))
runPQBool inline msg0 proxyp proxyq opts a hhs = do
    pp <- evalBool proxyp opts a
    case getValueLR inline opts msg0 pp hhs of
      Left e -> pure $ Left e
      Right p -> do
         qq <- evalBool proxyq opts a
         pure $ case getValueLR inline opts msg0 qq (hhs <> [hh pp]) of
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
-- >>> pz @(L11 >> Not Id) ((True,12),'x')
-- Val False
--
-- >>> pz @(L12 >> Succ >> Dup) ((True,12),'x')
-- Val (13,13)
--
-- >>> pz @(10 >> '(Id,"abc") >> Second Len) ()
-- Val (10,3)
--
data p >> q deriving Show
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
    case getValueLR NoInline opts "" pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @q) opts p
        pure $ case getValueLR NoInline opts (showL opts p) qq [hh pp] of
        -- need to look inside to see if there is already an exception in ttForest
          Left e | isVerbose opts -> e
                 | otherwise ->
                    if anyOf (ttForest . folded . root . peValP) (has _FailP) qq
                    then qq & ttForest %~ (hh pp:) -- we still need pp for context
                    else e
          Right q -> mkNodeCopy opts qq (lit3 opts msg0 q "" (topMessageEgregious (qq ^. ttString))) [hh pp]

-- | infixl version of 'Predicate.Core.>>'
data p >>> q deriving Show
type RightArrowsLeftInfixT p q = p >> q
infixl 1 >>>

instance P (RightArrowsLeftInfixT p q) x => P (p >>> q) x where
  type PP (p >>> q) x = PP (RightArrowsLeftInfixT p q) x
  eval _ = eval (Proxy @(RightArrowsLeftInfixT p q))


-- | flipped version of 'Predicate.Core.>>'
data p << q deriving Show
type LeftArrowsT p q = q >> p
infixr 1 <<

instance P (LeftArrowsT p q) x => P (p << q) x where
  type PP (p << q) x = PP (LeftArrowsT p q) x
  eval _ = eval (Proxy @(LeftArrowsT p q))

topMessageEgregious :: String -> String
topMessageEgregious s =
  let ret = fromMaybe "" (RH.scan topMessageExtractRe s ^? _last . _2 . _last)
  in '{' : (if null ret then s else ret) <> "}"

topMessageExtractRe :: RH.Regex
topMessageExtractRe = [RH.re|^.*\{([^}]+)\}.*?|]

-- | unwraps a value (see '_Wrapped'')
--
-- >>> pz @Unwrap (SG.Sum (-13))
-- Val (-13)
--
-- >>> pl @(Unwrap >> '(Id, 'True)) (SG.Sum 13)
-- Present (13,True) ((>>) (13,True) | {'(13,True)})
-- Val (13,True)
--
data Unwrap deriving Show

instance ( Show x
         , Show (Unwrapped x)
         , Wrapped x
         ) => P Unwrap x where
  type PP Unwrap x = Unwrapped x
  eval _ opts x =
    let msg0 = "Unwrap"
        d = x ^. _Wrapped'
    in pure $ mkNode opts (Val d) (show3 opts msg0 d x) []

-- | similar to 'Wrap' where @t@ points to the type
data Wrap' t p deriving Show

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
    pure $ case getValueLR NoInline opts msg0 pp [] of
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
data Wrap (t :: Type) p deriving Show
type WrapT (t :: Type) p = Wrap' (Hole t) p

instance P (WrapT t p) x => P (Wrap t p) x where
  type PP (Wrap t p) x = PP (WrapT t p) x
  eval _ = eval (Proxy @(WrapT t p))

-- | used internally for type inference
--
-- >>> pz @(FromIntegral' (Proxy (SG.Sum _) >> UnproxyT) 23) ()
-- Val (Sum {getSum = 23})
--
-- >>> pz @(FromIntegral' (Hole (SG.Sum _)) 23) () -- equivalent to Proxy UnproxyT above
-- Val (Sum {getSum = 23})
--
data UnproxyT deriving Show

instance Typeable t => P UnproxyT (Proxy (t :: Type)) where
  type PP UnproxyT (Proxy t) = t
  eval _ opts _ =
    let msg0 = "UnproxyT(" <> showT @t <> ")"
    in pure $ mkNode opts (Fail msg0) "you probably meant to get access to the type of PP only and not evaluate (see Pop0)" []

-- | similar to 'Length' but displays the input value and works only for lists
--
-- >>> pl @Len "abcd"
-- Present 4 (Len 4 | "abcd")
-- Val 4
--
-- >>> pl @Len [1..3000]
-- Present 3000 (Len 3000 | [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,7...)
-- Val 3000
--
-- >>> pz @Len [10,4,5,12,3,4]
-- Val 6
--
-- >>> pz @Len []
-- Val 0
--
-- >>> pz @(Pairs >> Len > 2) "abcdef"
-- Val True
--
data Len deriving Show

instance ( x ~ [a]
         , Show a
         ) => P Len x where
  type PP Len x = Int
  eval _ opts as' =
    let msg0 = "Len"
    in pure $ case chkSize opts { oLarge = True } msg0 as' [] of
         Left e -> e
         Right (asLen,_) ->
           mkNode opts (Val asLen) (show3 opts msg0 asLen as') []

-- | similar to 'length' for 'Foldable' instances
--
-- >>> pz @(Length Snd) (123,"abcdefg") -- if this breaks then get rid of Show a!
-- Val 7
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
data Length p deriving Show

instance ( PP p x ~ t a
         , P p x
         , Foldable t
         ) => P (Length p) x where
  type PP (Length p) x = Int
  eval _ opts x = do
    let msg0 = "Length"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p' ->
        case chkSize opts { oLarge = True } msg0 p' [] of
          Left e -> e
          Right (pLen,_) ->
            mkNode opts (Val pLen) (msg0 <> " " <> show pLen) [hh pp]

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
data Not p deriving Show

instance ( PP p x ~ Bool
         , P p x
         ) => P (Not p) x where
  type PP (Not p) x = Bool
  eval _ opts x = do
    let msg0 = "Not"
    pp <- evalBool (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
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

data IdBool deriving Show

instance x ~ Bool
        => P IdBool x where
  type PP IdBool x = Bool
  eval _ opts x =
    let msg0 = "IdBool"
    in pure $ mkNodeB opts x msg0 []

-- | Fails the computation with a message but allows you to set the output type
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
data Fail t prt deriving Show

instance ( P prt a
         , PP prt a ~ String
         ) => P (Fail t prt) a where
  type PP (Fail t prt) a = PP t a
  eval _ opts a = do
    let msg0 = "Fail"
    pp <- eval (Proxy @prt) opts a
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right s -> mkNode opts (Fail s) "" (verboseList opts pp)

-- | Fails the computation with a message for simple failures: doesnt preserve types
--
-- >>> pz @(FailS (PrintT "value=%03d string=%s" Id)) (99,"somedata")
-- Fail "value=099 string=somedata"
--
data FailS p deriving Show
instance P (Fail Id p) x => P (FailS p) x where
  type PP (FailS p) x = PP (Fail Id p) x
  eval _ = eval (Proxy @(Fail Id p))

-- | Fails the computation with a message (wraps the type in 'Hole')
--
-- >>> pz @(FailT Int (PrintF "value=%03d" Id)) 99
-- Fail "value=099"
--
data FailT (t :: Type) p deriving Show
instance P (Fail (Hole t) p) x => P (FailT t p) x where
  type PP (FailT t p) x = PP (Fail (Hole t) p) x
  eval _ = eval (Proxy @(Fail (Hole t) p))

-- | Fails the computation with a message where the input value is a Proxy
--
-- >>> pz @(Ix 3 (FailP "oops")) "abcd"
-- Val 'd'
--
-- >>> pz @(Ix 3 (FailP "oops")) "abc"
-- Fail "oops"
--
data FailP p deriving Show
instance P (Fail UnproxyT p) x => P (FailP p) x where
  type PP (FailP p) x = PP (Fail UnproxyT p) x
  eval _ = eval (Proxy @(Fail UnproxyT p))

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
data Between p q r deriving Show

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
    case getValueLR NoInline opts msg0 rr [] of
      Left e -> pure e
      Right r -> do
        lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x [hh rr]
        pure $ case lr of
          Left e -> e
          Right (p,q,pp,qq) ->
            let hhs = [hh rr, hh pp, hh qq]
            in case (compare r p, compare r q) of
                 (LT,_) -> mkNodeB opts False (showL opts p <> " <= " <> showL opts r) hhs
                 (_,GT) -> mkNodeB opts False (showL opts r <> " <= " <> showL opts q) hhs
                 _ -> mkNodeB opts True (showL opts p <> " <= " <> showL opts r <> " <= " <> showL opts q) hhs


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
-- >>> pl @(Negate 7 <..> 20) (-4)
-- True (-7 <= -4 <= 20)
-- Val True
--
-- >>> pl @(Negate 7 <..> 20) 21
-- False (21 <= 20)
-- Val False
--
data p <..> q deriving Show
infix 4 <..>

type BetweenOpT p q = Between p q Id

instance P (BetweenOpT p q) x => P (p <..> q) x where
  type PP (p <..> q) x = PP (BetweenOpT p q) x
  eval _ = evalBool (Proxy @(BetweenOpT p q))

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
-- >>> pl @(All (Lt 3)) [1 .. 10]
-- False (All(10) i=2 (3 < 3))
-- Val False
--
data All p deriving Show

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
      Right (xsLen,xs) -> do
        ts <- zipWithM (\i a -> ((i, a),) <$> evalBoolHide @p opts a) [0::Int ..] xs
        pure $ case splitAndAlign opts msg0 ts of
             Left e -> e
             Right abcs ->
               let hhs = map (hh . prefixNumberToTT) ts
                   msg1 = msg0 ++ "(" ++ showL opts xsLen ++ ")"
               in case find (not . view _1) abcs of
                    Nothing -> mkNodeB opts True msg1 hhs
                    Just (_,(i,_),tt) ->
                      mkNodeB opts False (msg1 <> " i=" ++ show i ++ " " <> topMessage tt) hhs

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
data Any p deriving Show
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
      Right (xsLen,xs) -> do
        ts <- zipWithM (\i a -> ((i, a),) <$> evalBoolHide @p opts a) [0::Int ..] xs
        pure $ case splitAndAlign opts msg0 ts of
             Left e -> e
             Right abcs ->
               let hhs = map (hh . prefixNumberToTT) ts
                   msg1 = msg0 ++ "(" ++ showL opts xsLen ++ ")"
               in case find (view _1) abcs of
                    Nothing -> mkNodeB opts False msg1 hhs
                    Just (_,(i,_),tt) ->
                      mkNodeB opts True (msg1 <> " i=" ++ show i ++ " " <> topMessage tt) hhs

-- | similar to 'fst'
data L1 p deriving Show

instance ( Show (ExtractL1T (PP p x))
         , ExtractL1C (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (L1 p) x where
  type PP (L1 p) x = ExtractL1T (PP p x)
  eval _ opts x = do
    let msg0 = "Fst"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL1C p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

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
data Fst deriving Show
type FstT = L1 Id

instance P FstT x => P Fst x where
  type PP Fst x = PP FstT x
  eval _ = eval (Proxy @FstT)

-- | similar to 'snd'
data L2 p deriving Show

instance ( Show (ExtractL2T (PP p x))
         , ExtractL2C (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (L2 p) x where
  type PP (L2 p) x = ExtractL2T (PP p x)
  eval _ opts x = do
    let msg0 = "Snd"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL2C p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

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
data Snd deriving Show

type SndT = L2 Id

instance P SndT x => P Snd x where
  type PP Snd x = PP SndT x
  eval _ = eval (Proxy @SndT)

-- | similar to 3rd element in a n-tuple
data L3 p deriving Show

instance ( Show (ExtractL3T (PP p x))
         , ExtractL3C (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (L3 p) x where
  type PP (L3 p) x = ExtractL3T (PP p x)
  eval _ opts x = do
    let msg0 = "Thd"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL3C p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

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
data Thd deriving Show
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
data L4 p deriving Show

instance ( Show (ExtractL4T (PP p x))
         , ExtractL4C (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (L4 p) x where
  type PP (L4 p) x = ExtractL4T (PP p x)
  eval _ opts x = do
    let msg0 = "L4"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL4C p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

-- | similar to 5th element in a n-tuple
--
-- >>> pz @(L5 Id) (10,"Abc",'x',True,1)
-- Val 1
--
data L5 p deriving Show

instance ( Show (ExtractL5T (PP p x))
         , ExtractL5C (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (L5 p) x where
  type PP (L5 p) x = ExtractL5T (PP p x)
  eval _ opts x = do
    let msg0 = "L5"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL5C p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]


-- | similar to 6th element in a n-tuple
--
-- >>> pz @(L6 Id) (10,"Abc",'x',True,1,99)
-- Val 99
--
data L6 p deriving Show

instance ( Show (ExtractL6T (PP p x))
         , ExtractL6C (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (L6 p) x where
  type PP (L6 p) x = ExtractL6T (PP p x)
  eval _ opts x = do
    let msg0 = "L6"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL6C p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

-- | similar to 7th element in a n-tuple
--
-- >>> pz @(L7 Id) (10,"Abc",'x',True,1,99,'a')
-- Val 'a'
--
data L7 p deriving Show

instance ( Show (ExtractL7T (PP p x))
         , ExtractL7C (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (L7 p) x where
  type PP (L7 p) x = ExtractL7T (PP p x)
  eval _ opts x = do
    let msg0 = "L7"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL7C p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

-- | similar to 8th element in a n-tuple
--
-- >>> pz @(L8 Id) (10,"Abc",'x',True,1,99,True,'a')
-- Val 'a'
--
data L8 p deriving Show

instance ( Show (ExtractL8T (PP p x))
         , ExtractL8C (PP p x)
         , P p x
         , Show (PP p x)
         ) => P (L8 p) x where
  type PP (L8 p) x = ExtractL8T (PP p x)
  eval _ opts x = do
    let msg0 = "L8"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL8C p
        in mkNode opts (Val b) (show3 opts msg0 b p) [hh pp]

-- | similar to 'map' for Foldable types
--
-- >>> pz @(Map' Pred Id) [1..5]
-- Val [0,1,2,3,4]
--
data Map' p q deriving Show

instance ( Show (PP p a)
         , P p a
         , PP q x ~ f a
         , P q x
         , Show a
         , Show (f a)
         , Foldable f
         ) => P (Map' p q) x where
  type PP (Map' p q) x = [PP p (ExtractAFromTA (PP q x))]
  eval _ opts x = do
    let msg0 = "Map"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case chkSize opts msg0 q [hh qq] of
          Left e -> pure e
          Right (_,xs) -> do
            ts <- zipWithM (\i a -> ((i, a),) <$> evalHide @p opts a) [0::Int ..] xs
            pure $ case splitAndAlign opts msg0 ts of
                 Left e -> e
                 Right abcs ->
                   let vals = map (view _1) abcs
                   in mkNode opts (Val vals) (show3 opts msg0 vals q) (hh qq : map (hh . prefixNumberToTT) ts)

-- | similar to 'map'
--
-- >>> pz @(Map Pred) [1..5]
-- Val [0,1,2,3,4]
--
data Map p deriving Show

instance ( Show (PP p a)
         , P p a
         , x ~ [a]
         , Show a
         ) => P (Map p) x where
  type PP (Map p) x = [PP p (ExtractAFromTA x)]
  eval _ opts x = do
    let msg0 = "Map"
    case chkSize opts msg0 x [] of
      Left e -> pure e
      Right (_,xs) -> do
        ts <- zipWithM (\i a -> ((i, a),) <$> evalHide @p opts a) [0::Int ..] xs
        pure $ case splitAndAlign opts msg0 ts of
             Left e -> e
             Right abcs ->
               let vals = map (view _1) abcs
               in mkNode opts (Val vals) (show3 opts msg0 vals x) (map (hh . prefixNumberToTT) ts)

-- | processes a type level list predicates running each in sequence with infixr: see 'Predicate.>>'
--
-- >>> pz @(Do [Pred, ShowP Id, Id &&& Len]) 9876543
-- Val ("9876542",7)
--
-- >>> pz @(Do '[W 123, W "xyz", Len &&& Id, Pred *** Id<>Id]) ()
-- Val (2,"xyzxyz")
--
-- >>> pl @(Do '[Succ,Id,ShowP Id,Ones,Map (ReadBase Int 8)]) 1239
-- Present [1,2,4,0] ((>>) [1,2,4,0] | {Map [1,2,4,0] | ["1","2","4","0"]})
-- Val [1,2,4,0]
--
-- >>> pl @(Do '[Pred,Id,ShowP Id,Ones,Map (ReadBase Int 8)]) 1239
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
data Do (ps :: [k]) deriving Show
-- infixr same as >>

instance (P (DoExpandT ps) a) => P (Do ps) a where
  type PP (Do ps) a = PP (DoExpandT ps) a
  eval _ = eval (Proxy @(DoExpandT ps))

-- need both :: Type and (Id >> p or W)
-- | expand out a type level list of commands using 'Predicate.Core.>>' (associates to the right)
type family DoExpandT (ps :: [k]) :: Type where -- need Type not k else No instance for GN.KnownNat: pl @(Do '[4,5,6]) ()
  DoExpandT '[] = GL.TypeError ('GL.Text "DoExpandT '[] invalid: requires at least one predicate in the list")
  DoExpandT '[p] = W p -- need W or Id >> p else will fail with No instance for Show: pl @(Do '[4,5,6]) ()
  DoExpandT (p ': p1 ': ps) = p >> DoExpandT (p1 ': ps)

-- | processes a type level list predicates running each in sequence with infixl: see 'Predicate.>>'
--
-- >>> pz @(DoL [Pred, ShowP Id, Id &&& Len]) 9876543
-- Val ("9876542",7)
--
-- >>> pz @(DoL [2,3,4]) ()
-- Val 4
--
-- >>> pl @(DoL '[4,5,6]) ()
-- Present 6 ((>>) 6 | {'6})
-- Val 6
--
data DoL (ps :: [k]) deriving Show
-- infixl unlike >>

instance (P (DoExpandLT ps) a) => P (DoL ps) a where
  type PP (DoL ps) a = PP (DoExpandLT ps) a
  eval _ = eval (Proxy @(DoExpandLT ps))

-- | like 'DoExpandT' but associates to the left
type family DoExpandLT (ps :: [k]) :: Type where
  DoExpandLT '[] = GL.TypeError ('GL.Text "DoExpandT '[] invalid: requires at least one predicate in the list")
  DoExpandLT '[p] = W p
  DoExpandLT (p ': p1 ': '[]) = p >> p1
  DoExpandLT (p ': p1 ': p2 ': ps) = (p >> p1) >> DoExpandLT (p2 ': ps)

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
-- >>> pz @(Uncurry (+:)) ([2..5],1)
-- Val [2,3,4,5,1]
--
-- >>> pz @(Uncurry (==!)) ('x','y')
-- Val LT
--
data p && q deriving Show
infixr 3 &&

instance ( P p a
         , P q a
         , PP p a ~ Bool
         , PP q a ~ Bool
         ) => P (p && q) a where
  type PP (p && q) a = Bool
  eval _ opts a = do
    let msg0 = "&&"
    lr <- runPQBool NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let zz = case (p,q) of
                  (True, True) -> ""
                  (False, True) -> topMessage pp
                  (True, False) -> topMessage qq
                  (False, False) -> topMessage pp <> " " <> msg0 <> " " <> topMessage qq
        in mkNodeB opts (p&&q) (showL opts p <> " " <> msg0 <> " " <> joinStrings (showL opts q) zz) [hh pp, hh qq]

-- | short circuit version of boolean And
--
-- >>> pl @(Id > 10 &&~ FailT _ "ss") 9
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
data p &&~ q deriving Show
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
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right False ->
        pure $ mkNodeB opts False ("False " <> msg0 <> " _" <> litVerbose opts " | " (topMessage pp)) [hh pp]
      Right True -> do
        qq <- evalBool (Proxy @q) opts a
        pure $ case getValueLR NoInline opts msg0 qq [hh pp] of
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
data p || q deriving Show
infixr 2 ||

instance ( P p a
         , P q a
         , PP p a ~ Bool
         , PP q a ~ Bool
         ) => P (p || q) a where
  type PP (p || q) a = Bool
  eval _ opts a = do
    let msg0 = "||"
    lr <- runPQBool NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let zz = case (p,q) of
                  (False,False) -> " | " <> topMessage pp <> " " <> msg0 <> " " <> topMessage qq
                  _ -> ""
        in mkNodeB opts (p||q) (showL opts p <> " " <> msg0 <> " " <> showL opts q <> zz) [hh pp, hh qq]

-- | short circuit version of boolean Or
--
-- >>> pl @(Id > 10 ||~ FailT _ "ss") 11
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
data p ||~ q deriving Show
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
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right False -> do
        qq <- evalBool (Proxy @q) opts a
        pure $ case getValueLR NoInline opts msg0 qq [hh pp] of
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
data p ~> q deriving Show
infixr 1 ~>

instance ( P p a
         , P q a
         , PP p a ~ Bool
         , PP q a ~ Bool
         ) => P (p ~> q) a where
  type PP (p ~> q) a = Bool
  eval _ opts a = do
    let msg0 = "~>"
    lr <- runPQBool NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let zz = case (p,q) of
                  (True,False) -> topMessage pp <> " " <> msg0 <> " " <> topMessage qq
                  _ -> ""
        in mkNodeB opts (p~>q) (showL opts p <> " " <> msg0 <> " " <> joinStrings (showL opts q) zz) [hh pp, hh qq]


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
data Swap deriving Show

instance ( Show (p a b)
         , SwapC p
         , Show (p b a)
         ) => P Swap (p a b) where
  type PP Swap (p a b) = p b a
  eval _ opts pabx =
    let msg0 = "Swap"
        d = swapC pabx
    in pure $ mkNode opts (Val d) (show3 opts msg0 d pabx) []

-- | like 'GHC.Base.$' for expressions taking exactly on argument (similar is 'Predicate.Misc.%%')
-- ie this doesnt work: pz @('(,) $ 4 $ 'True) ()
--
-- >>> pl @(L1 $ L2 $ Id) ((1,2),(3,4))
-- Present 3 (Fst 3 | (3,4))
-- Val 3
--
-- >>> pl @((<=) 4 $ L1 $ L2 $ Id) ((1,2),(3,4))
-- False (4 <= 3)
-- Val False
--
-- >>> pz @('(,) 4 $ 'True) ()
-- Val (4,True)
--
-- >>> pz @('(,) %% 'True %% 'False) () -- cant do this with $
-- Val (True,False)
--
data (p :: k -> k1) $ (q :: k) deriving Show
infixr 0 $

instance P (p q) a => P (p $ q) a where
  type PP (p $ q) a = PP (p q) a
  eval _  = eval (Proxy @(p q))

-- | similar to 'Control.Lens.&' for expressions taking exactly on argument
--
-- >>> pl @(Id & L1 & Singleton & Length) (13,"xyzw")
-- Present 1 (Length 1)
-- Val 1
--
-- >>> pl @(2 & (&&&) "abc") ()
-- Present ("abc",2) ('("abc",2))
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
-- >>> pl @(L3 $ L2 $ Fst) ((1,("X",9,'a')),(3,4))
-- Present 'a' (Thd 'a' | ("X",9,'a'))
-- Val 'a'
--
-- >>> pz @('True %& 'False %& '(,)) ()
-- Val (False,True)
--
data (q :: k) & (p :: k -> k1) deriving Show
infixl 1 &

instance P (p q) a => P (q & p) a where
  type PP (q & p) a = PP (p q) a
  eval _ = eval (Proxy @(p q))

-- | first element in a tuple followed by the first element
--
-- >>> pz @L11 ((10,"ss"),2)
-- Val 10
--
data L11 deriving Show
type L11T = MsgI "L11:" (L1 (L1 Id))

instance P L11T x => P L11 x where
  type PP L11 x = PP L11T x
  eval _ = eval (Proxy @L11T)

-- | first element in a tuple followed by the second element
--
-- >>> pz @L12 ((10,"ss"),2)
-- Val "ss"
--
data L12 deriving Show
type L12T = MsgI "L12:" (L2 (L1 Id))

instance P L12T x => P L12 x where
  type PP L12 x = PP L12T x
  eval _ = eval (Proxy @L12T)

-- | first element in a tuple followed by the third element
--
-- >>> pz @L13 ((10,"ss",4.5),2)
-- Val 4.5
--
data L13 deriving Show
type L13T = MsgI "L13:" (L3 (L1 Id))

instance P L13T x => P L13 x where
  type PP L13 x = PP L13T x
  eval _ = eval (Proxy @L13T)

-- | second element in a tuple followed by the first element
--
-- >>> pz @L21 ('x',(10,"ss",4.5),2)
-- Val 10
--
data L21 deriving Show
type L21T = MsgI "L21:" (L1 (L2 Id))

instance P L21T x => P L21 x where
  type PP L21 x = PP L21T x
  eval _ = eval (Proxy @L21T)

-- | second element in a tuple followed by the second element
--
-- >>> pz @L22 ('z',(10,"ss",4.5),2)
-- Val "ss"
--
data L22 deriving Show
type L22T = MsgI "L22:" (L2 (L2 Id))

instance P L22T x => P L22 x where
  type PP L22 x = PP L22T x
  eval _ = eval (Proxy @L22T)

-- | second element in a tuple followed by the third element
--
-- >>> pz @L23 ('x',(10,"ss",4.5),2)
-- Val 4.5
--
data L23 deriving Show
type L23T = MsgI "L23:" (L3 (L2 Id))

instance P L23T x => P L23 x where
  type PP L23 x = PP L23T x
  eval _ = eval (Proxy @L23T)

-- | third element in a tuple followed by the first element
--
-- >>> pz @L31 (1,2,('c',4))
-- Val 'c'
--
data L31 deriving Show
type L31T = MsgI "L31:" (L1 (L3 Id))

instance P L31T x => P L31 x where
  type PP L31 x = PP L31T x
  eval _ = eval (Proxy @L31T)

-- | third element in a tuple followed by the second element
--
-- >>> pz @L32 (1,2,('c',4))
-- Val 4
--
data L32 deriving Show
type L32T = MsgI "L32:" (L2 (L3 Id))

instance P L32T x => P L32 x where
  type PP L32 x = PP L32T x
  eval _ = eval (Proxy @L32T)

-- | third element in a tuple followed by the third element
--
-- >>> pz @L33 (1,2,('c',4,False))
-- Val False
--
data L33 deriving Show
type L33T = MsgI "L33:" (L3 (L3 Id))

instance P L33T x => P L33 x where
  type PP L33 x = PP L33T x
  eval _ = eval (Proxy @L33T)

-- | for use with TH.Lift in a splice. returns a pure value or fails with a tree
unsafeEval :: forall opts p a
        . ( HasCallStack
          , OptC opts
          , Show (PP p a)
          , P p a
          )
        => a
        -> PP p a
unsafeEval = either (error . ("\n" <>)) id . evalEither @opts @p

-- | run a type level computation and returns the value or a tree with the error
evalEither :: forall opts p a
        . ( OptC opts
          , Show (PP p a)
          , P p a
          )
        => a
        -> Either String (PP p a)
evalEither a =
  let opts = getOpt @opts
      pp = runIdentity $ eval (Proxy @p) opts a
  in case pp ^. ttVal of
       Val r -> Right r
       Fail {} -> Left $ prtTree opts pp

-- | creates a 'Data.Semigroup.Arg' value using @p@ and @q@
--
-- >>> pz @('SG.Arg (C "S") 10) ()
-- Val (Arg 'S' 10)
--
instance ( P p x
         , P q x
         , Show (PP p x)
         , Show (PP q x)
         ) => P ('SG.Arg p q) x where
  type PP ('SG.Arg p q) x = SG.Arg (PP p x) (PP q x)
  eval _ opts x = do
    let msg0 = "'Arg"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
            ret = SG.Arg p q
        in mkNode opts (Val ret) (msg0 <> " " <> showL opts p <> " " <> showL opts q) hhs

-- | extracts a tuple from 'Data.Semigroup.Arg'
--
-- >>> pz @('SG.Arg (C "S") 10 >> Arg') ()
-- Val ('S',10)
--
-- >>> pz @Arg' (SG.Arg 'S' 10)
-- Val ('S',10)
--
data Arg' deriving Show

instance x ~ SG.Arg a b => P Arg' x where
  type PP Arg' x = ArgT x
  eval _ opts (SG.Arg a b) =
    let msg0 = "Arg'"
        ret = (a,b)
    in pure $ mkNode opts (Val ret) msg0 []

-- | calculates the return type for 'Arg''
type family ArgT (x :: Type) where
  ArgT (SG.Arg a b) = (a,b)
  ArgT o = GL.TypeError (
      'GL.Text "ArgT: expected 'SG.Arg a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | extracts the () from type level @ENone@ if the value exists
--
-- >>> pl @'ENone ENone
-- Present () ('ENone)
-- Val ()
--
-- >>> pz @'ENone (ERight "aaa")
-- Fail "'ENone found ERight"
--
instance x ~ Elr a b => P 'ENone x where
  type PP 'ENone x = ()
  eval _ opts x =
    let msg0 = "'ENone"
    in pure $ case x of
      ELeft {} -> mkNode opts (Fail (msg0 <> " found ELeft")) "" []
      ENone -> mkNode opts (Val ()) msg0 []
      ERight {} -> mkNode opts (Fail (msg0 <> " found ERight")) "" []
      EBoth {} -> mkNode opts (Fail (msg0 <> " found EBoth")) "" []

-- | extracts the @a@ from type level @ELeft a@ if the value exists
--
-- >>> pl @('ELeft Id) (ELeft 12)
-- Present 12 ('ELeft)
-- Val 12
--
-- >>> pz @('ELeft Id) (ERight "aaa")
-- Fail "'ELeft found ERight"
--
-- >>> pz @('ELeft Id) (EBoth 999 "aaa")
-- Fail "'ELeft found EBoth"
--
-- >>> pl @('ELeft Id) (ERight 12)
-- Error 'ELeft found ERight
-- Fail "'ELeft found ERight"
--
instance ( PP p x ~ Elr a b
         , P p x
         )
    => P ('ELeft p) x where
  type PP ('ELeft p) x = ELeftT (PP p x)
  eval _ opts x = do
    let msg0 = "'ELeft"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          ENone -> mkNode opts (Fail (msg0 <> " found ENone")) "" [hh pp]
          ELeft a -> mkNode opts (Val a) msg0 [hh pp]
          ERight {} -> mkNode opts (Fail (msg0 <> " found ERight")) "" [hh pp]
          EBoth {} -> mkNode opts (Fail (msg0 <> " found EBoth")) "" [hh pp]

-- | extracts the @b@ from type level @ERight b@ if the value exists
--
-- >>> pz @('ERight Id) (ERight 123)
-- Val 123
--
-- >>> pz @('ERight Id) (ELeft "aaa")
-- Fail "'ERight found ELeft"
--
-- >>> pz @('ERight Id) (EBoth 44 "aaa")
-- Fail "'ERight found EBoth"
--
instance ( PP p x ~ Elr a b
         , P p x
         )
    => P ('ERight p) x where
  type PP ('ERight p) x = ERightT (PP p x)
  eval _ opts x = do
    let msg0 = "'ERight"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          ENone -> mkNode opts (Fail (msg0 <> " found ENone")) "" [hh pp]
          ELeft {} -> mkNode opts (Fail (msg0 <> " found ELeft")) "" [hh pp]
          ERight b -> mkNode opts (Val b) msg0 [hh pp]
          EBoth {} -> mkNode opts (Fail (msg0 <> " found EBoth")) "" [hh pp]

-- | extracts the (a,b) from type level @EBoth a b@ if the value exists
--
-- >>> pz @('EBoth Id Id) (EBoth 123 "abc")
-- Val (123,"abc")
--
-- >>> pz @('EBoth Id 5) (EBoth 123 "abcde")
-- Val (123,5)
--
-- >>> pz @('EBoth Id Id) (ELeft "aaa")
-- Fail "'EBoth found ELeft"
--
-- >>> pz @('EBoth Id Id) (ERight "aaa")
-- Fail "'EBoth found ERight"
--
instance ( Show a
         , Show b
         , P p a
         , P q b
         , Show (PP p a)
         , Show (PP q b)
         ) => P ('EBoth p q) (Elr a b) where
  type PP ('EBoth p q) (Elr a b) = (PP p a, PP q b)
  eval _ opts th = do
    let msg0 = "'EBoth"
    case th of
      EBoth a b -> do
        pp <- eval (Proxy @p) opts a
        case getValueLR NoInline opts msg0 pp [] of
           Left e -> pure e
           Right p -> do
             qq <- eval (Proxy @q) opts b
             pure $ case getValueLR NoInline opts (msg0 <> " q failed p=" <> showL opts p) qq [hh pp] of
                Left e -> e
                Right q ->
                  let ret = (p,q)
                  in  mkNode opts (Val ret) (show3 opts msg0 ret (EBoth a b)) [hh pp, hh qq]
      _ -> pure $ mkNode opts (Fail (msg0 <> " found " <> showElr th)) "" []

