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
{- |
     Dsl for evaluating and displaying type level expressions
-}
module Predicate.Core (
    P(..)

 -- ** basic types
  , I
  , Id
  , IdT
  , W
  , Msg

  -- ** display evaluation tree
  , pe
  , pe2
  , pe2n
  , pu
  , pun
  , pe3
  , pl
  , plc

  -- ** evaluation methods
  , runPQ
  , evalBool
  , evalQuick
  ) where
import Predicate.Util
import GHC.TypeLits (Symbol,Nat,KnownSymbol,KnownNat)
import Control.Lens ((^.))
import Data.Proxy
import Data.Typeable
import Data.Kind (Type)
import Data.These (These(..))
import Data.Functor.Identity
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XNoStarIsType

-- | This is the core class. Each instance of this class can be combined into a dsl using 'Main.>>'
class P p a where
  type PP (p :: k) a :: Type -- PP is the output type
  eval :: MonadEval m => Proxy p -> POpts -> a -> m (TT (PP p a)) -- ^ returns a tree of results

-- | A specialised form of 'eval' that works only on predicates
evalBool :: (MonadEval m, P p a, PP p a ~ Bool) => Proxy p -> POpts -> a -> m (TT (PP p a))
evalBool p opts a = fixBoolT <$> eval p opts a

evalQuick :: forall p i . P p i => i -> Either String (PP p i)
evalQuick i = getValLRFromTT (runIdentity (eval (Proxy @p) o0 i))


-- | identity function
--
-- >>> pl @I 23
-- Present 23
-- PresentT 23
data I
instance P I a where
  type PP I a = a
  eval _ opts a =
    let msg0 = "I"
    in pure $ mkNode opts (PresentT a) [msg0] []


-- | identity function that displays the input unlike 'I'
--
-- even more constraints than 'I' so we might need to add explicit type signatures
--
-- >>> pl @Id 23
-- Present 23
-- PresentT 23
data Id
instance Show a => P Id a where
  type PP Id a = a
  eval _ opts a =
    let msg0 = "Id"
    in pure $ mkNode opts (PresentT a) [msg0 <> show0 opts " " a] []


-- even more constraints than 'Id' so we might need to explicitly add types (Typeable)
-- | identity function that also displays the type information for debugging
--
-- >>> pl @IdT 23
-- Present 23
-- PresentT 23
data IdT
instance (Typeable a, Show a) => P IdT a where
  type PP IdT a = a
  eval _ opts a =
    let msg0 = "IdT(" <> t <> ")"
        t = showT @a
    in pure $ mkNode opts (PresentT a) [msg0 <> show0 opts " " a] []

-- | transparent predicate wrapper to make k of kind 'Type' so it can be in a promoted list (cant mix kinds) see 'Predicate.Do'
--
-- >>> pl @'[W 123, Id] 99
-- Present [123,99]
-- PresentT [123,99]
--
-- >>> pl @'[W "abc", W "def", Id, Id] "ghi"
-- Present ["abc","def","ghi","ghi"]
-- PresentT ["abc","def","ghi","ghi"]
--
data W (p :: k)
instance P p a => P (W p) a where
  type PP (W p) a = PP p a
  eval _ = eval (Proxy @(Msg "W" p))

-- | add a message to give more context to the evaluation tree
--
-- >>> pe @(Msg "[somemessage] " Id) 999
-- P [somemessage] Id 999
-- PresentT 999
--
-- >>> pe @(Msg Id 999) "info message:"
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

-- | 'const' () function
--
-- >>> pl @() "Asf"
-- Present ()
-- PresentT ()
--
instance P () a where
  type PP () a = ()
  eval _ opts _ =
    let msg0 = "()"
    in pure $ mkNode opts (PresentT ()) [msg0] []

instance P (Proxy t) a where
  type PP (Proxy t) a = Proxy t
  eval _ opts _ =
    let msg0 = "Proxy"
    in pure $ mkNode opts (PresentT Proxy) [msg0] []

-- Start non-Type kinds
-----------------------

-- | pulls the type level 'Bool' to the value level
--
-- >>> pl @'True "not used"
-- True
-- TrueT
--
-- >>> pl @'False ()
-- False
-- FalseT
instance GetBool b => P (b :: Bool) a where
  type PP b a = Bool
  eval _ opts _ =
    let b = getBool @b
    in pure $ mkNodeB opts b ["'" <> show b] []

-- | pulls the type level 'Symbol' to the value level as a 'GHC.Base.String'
--
-- >>> pl @"hello world" ()
-- Present "hello world"
-- PresentT "hello world"
instance KnownSymbol s => P (s :: Symbol) a where
  type PP s a = String
  eval _ opts _ =
    let s = symb @s
    in pure $ mkNode opts (PresentT s) ["'" <> showLit0 opts "" s] []

-- | run the predicates in a promoted 2-tuple; similar to 'Control.Arrow.&&&'
--
-- >>> pl @'(Id, 4) "hello"
-- Present ("hello",4)
-- PresentT ("hello",4)
--
instance (P p a, P q a) => P '(p,q) a where
  type PP '(p,q) a = (PP p a, PP q a)
  eval _ opts a = do
    let msg = "'(,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
       Left e -> e
       Right (p,q,pp,qq) ->
         mkNode opts (PresentT (p,q)) [msg] [hh pp, hh qq]

-- | run the predicates in a promoted 3-tuple
--
-- >>> pl @'(4, Id, "goodbye") "hello"
-- Present (4,"hello","goodbye")
-- PresentT (4,"hello","goodbye")
--
instance (P p a
        , P q a
        , P r a
        ) => P '(p,q,r) a where
  type PP '(p,q,r) a = (PP p a, PP q a, PP r a)
  eval _ opts a = do
    let msg = "'(,,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
         let hhs = [hh pp, hh qq]
         rr <- eval (Proxy @r) opts a
         pure $ case getValueLR opts msg rr hhs of
           Left e -> e
           Right r -> mkNode opts (PresentT (p,q,r)) [msg] (hhs <> [hh rr])

-- | run the predicates in a promoted 4-tuple
--
-- >>> pl @'(4, Id, "inj", 999) "hello"
-- Present (4,"hello","inj",999)
-- PresentT (4,"hello","inj",999)
--
instance (P p a
        , P q a
        , P r a
        , P s a
        ) => P '(p,q,r,s) a where
  type PP '(p,q,r,s) a = (PP p a, PP q a, PP r a, PP s a)
  eval _ opts a = do
    let msg = "'(,,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        lr1 <- runPQ msg (Proxy @r) (Proxy @s) opts a
        pure $ case lr1 of
          Left e -> e
          Right (r,s,rr,ss) ->
            mkNode opts (PresentT (p,q,r,s)) [msg] [hh pp, hh qq, hh rr, hh ss]

-- | extracts the value level representation of the promoted 'Ordering'
--
-- >>> pl @'LT "not used"
-- Present LT
-- PresentT LT
--
-- >>> pl @'EQ ()
-- Present EQ
-- PresentT EQ
instance GetOrdering cmp => P (cmp :: Ordering) a where
  type PP cmp a = Ordering
  eval _ opts _a =
    let cmp = getOrdering @cmp
        msg = "'" <> show cmp
    in pure $ mkNode opts (PresentT cmp) [msg] []

-- | extracts the value level representation of the type level 'Nat'
--
-- >>> pl @123 ()
-- Present 123
-- PresentT 123
instance KnownNat n => P (n :: Nat) a where
  type PP n a = Int
  eval _ opts _ =
    let n = nat @n
    in pure $ mkNode opts (PresentT n) ["'" <> show n] []

-- | extracts the value level representation of the type level \'()
--
-- >>> pl @'() ()
-- Present ()
-- PresentT ()
instance P '() a where
  type PP '() a = ()
  eval _ opts _ = pure $ mkNode opts (PresentT ()) ["'()"] []

-- todo: the type has to be [a] so we still need type PP '[p] a = [PP p a] to keep the types in line

-- | extracts the value level representation of the type level \'[]
--
-- >>> pl @'[] False
-- Present []
-- PresentT []
instance P ('[] :: [k]) a where
  type PP ('[] :: [k]) a = [a]
  eval _ opts _ = pure $ mkNode opts (PresentT mempty) ["'[]"] []

-- | runs each predicate in turn from the promoted list
--
-- >>> pl @'[1, 2, 3] 999
-- Present [1,2,3]
-- PresentT [1,2,3]
--
-- >>> pl @'[W 1, W 2, W 3, Id] 999
-- Present [1,2,3,999]
-- PresentT [1,2,3,999]
--
instance (Show (PP p a), Show a, P p a) => P '[p] a where
  type PP '[p] a = [PP p a]
  eval _ opts a = do
    pp <- eval (Proxy @p) opts a
    let msg0 = "" -- "'[](end)"
    pure $ case getValueLR opts msg0 pp [] of
       Left e -> e
       Right b -> mkNode opts (PresentT [b]) [show01 opts msg0 b a] [hh pp] --  <> show0 opts " " a <> show1 opts " b=" b]) [hh pp]

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
    lr <- runPQ msg0 (Proxy @p) (Proxy @(p1 ': ps)) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let ret = p:q
        -- no gap between ' and ret!
        in mkNode opts (PresentT ret) ["'" <> show0 opts "" ret <> show1 opts " | " a] [hh pp, hh qq]

-- | extracts the \'a\' from type level \'Maybe a\' if the value exists
--
-- >>> pl @('Just Id) (Just 123)
-- Present 123
-- PresentT 123
--
-- >>> pl @('Just Id) (Just True)
-- Present True
-- PresentT True
--
-- >>> pl @('Just Id) Nothing
-- Error 'Just found Nothing
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
          Right b -> mkNode opts (PresentT b) [show01 opts msg0 b ma] [hh pp]
      Nothing -> pure $ mkNode opts (FailT (msg0 <> " found Nothing")) [msg0 <> " found Nothing"] []

-- | expects Nothing otherwise it fails
-- if the value is Nothing then it returns \'Proxy a\' as this provides more information than '()'
--
-- >>> pl @'Nothing Nothing
-- Present Proxy
-- PresentT Proxy
--
-- >>> pl @'Nothing (Just True)
-- Error 'Nothing found Just
-- FailT "'Nothing found Just"
--
instance P 'Nothing (Maybe a) where
  type PP 'Nothing (Maybe a) = Proxy a -- () gives us less information
  eval _ opts ma =
    let msg0 = "'Nothing"
    in pure $ case ma of
         Nothing -> mkNode opts (PresentT Proxy) [msg0] []
         Just _ -> mkNode opts (FailT (msg0 <> " found Just")) [msg0 <> " found Just"] []

-- omitted Show x so we can have less ambiguity
-- | extracts the \'a\' from type level \'Either a b\' if the value exists
--
-- >>> pl @('Left Id) (Left 123)
-- Present 123
-- PresentT 123
--
-- >>> pl @('Left Id) (Right "aaa")
-- Error 'Left found Right
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
         Right _ -> pure $ mkNode opts (FailT (msg0 <> " found Right")) [msg0 <> " found Right"] []
         Left a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg0 pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) [show01' opts msg0 b "Left " a] [hh pp]

-- | extracts the \'b\' from type level \'Either a b\' if the value exists
--
-- >>> pl @('Right Id) (Right 123)
-- Present 123
-- PresentT 123
--
-- >>> pl @('Right Id) (Left "aaa")
-- Error 'Right found Left
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
         Left _ -> pure $ mkNode opts (FailT (msg0 <> " found Left")) [msg0 <> " found Left"] []
         Right a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg0 pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) [show01' opts msg0 b "Right " a] [hh pp]

-- removed Show x: else ambiguity errors in TestPredicate

-- | extracts the \'a\' from type level \'These a b\' if the value exists
--
-- >>> pl @('This Id) (This 123)
-- Present 123
-- PresentT 123
--
-- >>> pl @('This Id) (That "aaa")
-- Error 'This found That
-- FailT "'This found That"
--
-- >>> pl @('This Id) (These 999 "aaa")
-- Error 'This found These
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
                 Right b -> mkNode opts (_tBool pp) [show01' opts msg0 b "This " a] [hh pp]
         _ -> pure $ mkNode opts (FailT (msg0 <> " found " <> showThese th)) [msg0 <> " found " <> showThese th] []

-- | extracts the \'b\' from type level \'These a b\' if the value exists
--
-- >>> pl @('That Id) (That 123)
-- Present 123
-- PresentT 123
--
-- >>> pl @('That Id) (This "aaa")
-- Error 'That found This
-- FailT "'That found This"
--
-- >>> pl @('That Id) (These 44 "aaa")
-- Error 'That found These
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
                 Right b -> mkNode opts (_tBool pp) [show01' opts msg0 b "That " a] [hh pp]
         _ -> pure $ mkNode opts (FailT (msg0 <> " found " <> showThese th)) [msg0 <> " found " <> showThese th] []


-- | extracts the (a,b) from type level 'These a b' if the value exists
--
-- >>> pl @('These Id Id) (These 123 "abc")
-- Present (123,"abc")
-- PresentT (123,"abc")
--
-- >>> pl @('These Id 5) (These 123 "abcde")
-- Present (123,5)
-- PresentT (123,5)
--
-- >>> pl @('These Id Id) (This "aaa")
-- Error 'These found This
-- FailT "'These found This"
--
-- >>> pl @('These Id Id) (That "aaa")
-- Error 'These found That
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
                 pure $ case getValueLR opts (msg0 <> " q failed p=" <> show p) qq [hh pp] of
                    Left e -> e
                    Right q ->
                      let ret =(p,q)
                      in  mkNode opts (PresentT ret) [show01 opts msg0 ret (These a b)] [hh pp, hh qq]
         _ -> pure $ mkNode opts (FailT (msg0 <> " found " <> showThese th)) [msg0 <> " found " <> showThese th] []

-- | converts the value to the corresponding 'Proxy'
--
-- >>> pl @'Proxy 'x'
-- Present Proxy
-- PresentT Proxy
--
instance Show a => P 'Proxy a where
  type PP 'Proxy a = Proxy a
  eval _ opts a =
    let b = Proxy @a
    in pure $ mkNode opts (PresentT b) ["'Proxy" <> show1 opts " | " a] []

-- End non-Type kinds
-----------------------

pe, pe2, pe2n, pu, pun, pe3, pl, plc :: forall p a . (Show (PP p a), P p a) => a -> IO (BoolT (PP p a))
-- | displays the evaluation tree in plain text without colors
pe  = peWith @p o0
-- | displays the evaluation tree using colors
pe2 = peWith @p o2
-- | same as 'pe2' but truncates the display tree horizontally: see 'o2n'
pe2n = peWith @p o2n
-- | same as 'pe2' but wider display
pe3 = peWith @p o3
-- | skips the evaluation tree and just displays the end result
pl = peWith @p ol
-- | same as 'pl' but with colors
plc = peWith @p olc
-- | display the evaluation tree using unicode and colors
-- @
--   pu @'(Id, "abc", 123) [1..4]
-- @
pu = peWith @p ou
-- | same as 'pu' but truncates the display tree horizontally: see 'ou'
pun = peWith @p oun

peWith :: forall p a
        . (Show (PP p a), P p a)
        => POpts
        -> a
        -> IO (BoolT (PP p a))
peWith opts a = do
  pp <- eval (Proxy @p) opts a
  let r = pp ^. tBool
  if oLite opts then
    let f = colorMe opts (r ^. boolT2P)
    in putStrLn $ case r of
         FailT e -> f "Error" <> " " <> e
         TrueT -> f "True"
         FalseT -> f "False"
         PresentT x -> f "Present" <> " " <> show x
  else prtTree opts (fromTT pp)
  return r

runPQ :: (P p a, P q a, MonadEval m)
   => String
   -> Proxy p
   -> Proxy q
   -> POpts
   -> a
   -> m (Either (TT x) (PP p a, PP q a, TT (PP p a), TT (PP q a)))
runPQ msg0 proxyp proxyq opts a = do
    pp <- eval proxyp opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure $ Left e
      Right p -> do
         qq <- eval proxyq opts a
         pure $ case getValueLR opts msg0 qq [hh pp] of
           Left e -> Left e
           Right q -> Right (p, q, pp, qq)


