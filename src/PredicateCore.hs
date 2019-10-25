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
Module      : PredicateCore
Description : Dsl for evaluating and displaying type level expressions
Copyright   : (c) Grant Weyburne, 2019
License     : BSD-3
Maintainer  : gbwey9@gmail.com

class P is the main class. Contains a minimal set of instances of P to prevent orphans
-}
module PredicateCore where
import UtilP
import GHC.TypeLits (Symbol,Nat,KnownSymbol,KnownNat)
import Control.Lens ((^.))
import Data.Proxy
import Data.Typeable
import Data.Kind (Type)
import Data.These (These(..))

-- | This is the core class. Each instance of this class can be combined into a dsl using 'Main.>>'
class P p a where
  type PP (p :: k) a :: Type -- PP is the output type
  eval :: MonadEval m => Proxy p -> POpts -> a -> m (TT (PP p a)) -- ^ returns a tree of results

-- | A specialised form of 'eval' that works only on predicates
evalBool :: (MonadEval m, P p a, PP p a ~ Bool) => Proxy p -> POpts -> a -> m (TT (PP p a))
evalBool p opts a = fixBoolT <$> eval p opts a

-- | identity function
--
-- >>> pl @I 23
-- Present 23
-- PresentT 23
data I
instance P I a where
  type PP I a = a
  eval _ opts a =
    pure $ mkNode opts (PresentT a) ["I"] []


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
  eval _ opts a = pure $ mkNode opts (PresentT a) ["Id" <> show0 opts " " a] []


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
    let t = showT @a
    in pure $ mkNode opts (PresentT a) ["IdT(" <> t <> ")" <> show0 opts " " a] []

-- | transparent predicate wrapper to make k of kind 'Type' so it can be in a promoted list (cant mix kinds) see 'Predicate.Do'
--
-- >>> pl @'[W 123, Id] 99
-- Present [123,99]
-- PresentT [123,99]
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
instance Show a => P () a where
  type PP () a = ()
  eval _ opts a = pure $ mkNode opts (PresentT ()) ["()" <> show0 opts " " a] []

instance P (Proxy t) a where
  type PP (Proxy t) a = Proxy t
  eval _ opts _ =
    pure $ mkNode opts (PresentT Proxy) ["Proxy"] []

-- Start non-Type kinds
-----------------------
-----------------------
-----------------------

-- | pulls the type level 'Bool' to the value level
--
-- >>> pl @'True "ignore this"
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

-- | pulls the type level 'Symbol' to the value level
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
  eval _ opts _ = pure $ mkNode opts mempty ["'[]"] []

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
    let msg = "" -- "'[](end)"
    pure $ case getValueLR opts msg pp [] of
       Left e -> e
       Right b -> mkNode opts (PresentT [b]) [msg <> show0 opts " " b <> showA opts " | " a] [hh pp] --  <> show0 opts " " a <> showA opts " b=" b]) [hh pp]

instance (Show (PP p a)
        , Show a
        , P (p1 ': ps) a
        , PP (p1 ': ps) a ~ [PP p1 a]
        , P p a
        , PP p a ~ PP p1 a
        ) => P (p ': p1 ': ps) a where
  type PP (p ': p1 ': ps) a = [PP p a]
  eval _ opts a = do
    let msg = "'"
        -- len = 2 + getLen @ps
    lr <- runPQ msg (Proxy @p) (Proxy @(p1 ': ps)) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        mkNode opts (PresentT (p:q)) [msg <> show0 opts "" (p:q) <> showA opts " | " a] [hh pp, hh qq]

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
    let msg = "'Just"
    case ma of
      Just a -> do
        pp <- eval (Proxy @p) opts a
        pure $ case getValueLR opts msg pp [] of
          Left e -> e
          Right b -> mkNode opts (PresentT b) [msg <> show0 opts " " b <> showA opts " | " ma] [hh pp]
      Nothing -> pure $ mkNode opts (FailT (msg <> " found Nothing")) [msg <> " found Nothing"] []

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
    let msg = "'Nothing"
    in pure $ case ma of
         Nothing -> mkNode opts (PresentT Proxy) [msg] []
         Just _ -> mkNode opts (FailT (msg <> " found Just")) [msg <> " found Just"] []

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
    let msg = "'Left"
    in case lr of
         Right _ -> pure $ mkNode opts (FailT (msg <> " found Right")) [msg <> " found Right"] []
         Left a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) [msg <> show0 opts " " b <> showA opts " | Left " a] [hh pp]

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
    let msg = "'Right"
    case lr of
         Left _ -> pure $ mkNode opts (FailT (msg <> " found Left")) [msg <> " found Left"] []
         Right a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) [msg <> show0 opts " " b <> showA opts " | Right " a] [hh pp]

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
    let msg = "'This"
    case th of
         This a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) [msg <> show0 opts " " b <> showA opts " | This " a] [hh pp]
         _ -> pure $ mkNode opts (FailT (msg <> " found " <> showThese th)) [msg <> " found " <> showThese th] []

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
    let msg = "'That"
    case th of
         That a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) [msg <> show0 opts " " b <> showA opts " | That " a] [hh pp]
         _ -> pure $ mkNode opts (FailT (msg <> " found " <> showThese th)) [msg <> " found " <> showThese th] []


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
    let msg = "'These"
    case th of
         These a b -> do
            pp <- eval (Proxy @p) opts a
            case getValueLR opts msg pp [] of
               Left e -> pure e
               Right p -> do
                 qq <- eval (Proxy @q) opts b
                 pure $ case getValueLR opts (msg <> " q failed p=" <> show p) qq [hh pp] of
                    Left e -> e
                    Right q -> mkNode opts (PresentT (p,q)) [msg <> show0 opts " " (p,q) <> showA opts " | " (These a b)] [hh pp, hh qq]
         _ -> pure $ mkNode opts (FailT (msg <> " found " <> showThese th)) [msg <> " found " <> showThese th] []

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
    in pure $ mkNode opts (PresentT b) ["'Proxy" <> showA opts " | " a] []

-- End non-Type kinds
-----------------------
-----------------------
-----------------------

pe0, pe, pe1, pe2, pu, pex, pe3, pl, plc :: forall p a . (Show (PP p a), P p a) => a -> IO (BoolT (PP p a))
pe0  = peWith @p o0
pe  = peWith @p o02
pex  = peWith @p o03
pe1 = peWith @p o1
pe2 = peWith @p o2
pe3 = peWith @p o3
pl = peWith @p ol
plc = peWith @p olc
pu = peWith @p o2 { oDisp = Unicode }

peWith :: forall p a . (Show (PP p a), P p a) =>  -- Typeable (Proxy p),
     POpts -> a -> IO (BoolT (PP p a))
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


