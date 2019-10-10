-- experimental: use Refined/Refined3 instead
-- dispenses with 'fmt' and just emulates Refined3 using Refined
-- the whole thing is driven by proxy '(ip,op,i)
-- since we dont store the input then the input conversion has nothing to do with Refined and is instead part of a separate conversion process
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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStarIsType #-}
module RefinedE where
import Refined
import Predicate
import UtilP
import Control.Lens hiding (strict,iall)
import Data.Tree
import Data.Proxy
import Control.Monad.Except
import Control.Monad.Writer
import Data.Bitraversable
import qualified Data.Bifunctor as Bi

mkProxyE :: forall ip op i . Proxy '(ip,op,i)
mkProxyE = Proxy

type RefinedEC ip op i = (P ip i, P op (PP ip i), PP op (PP ip i) ~ Bool)

type MakeRT m p = RefinedT m (MakeRE p)

-- convert from '(ip,op,fmt,i) to RefinedE signature
type family MakeRE p where
  MakeRE '(ip,op,i) = Refined op (PP ip i)

withRefinedETP :: forall ip op m i b proxy . (Monad m, RefinedEC ip op i, Show (PP ip i))
  => proxy '(ip,op,i)
  -> POpts
  -> i
  -> (Refined op (PP ip i) -> RefinedT m b)
  -> RefinedT m b
withRefinedETP p opts = (>>=) . newRefinedEP p opts

newRefinedE :: forall ip op m i
  . ( RefinedEC ip op i
    , Monad m
    , Show (PP ip i))
   => POpts
   -> i
   -> RefinedT m (Refined op (PP ip i))
newRefinedE = newRefinedEP (Proxy @'(ip,op,i))

newRefinedEP :: forall ip op m i proxy
  . ( RefinedEC ip op i
    , Monad m
    , Show (PP ip i))
  => proxy '(ip,op,i)
  -> POpts
  -> i
  -> RefinedT m (Refined op (PP ip i))
newRefinedEP p opts i =
  case evalEP p opts i of
    Left ret -> do
         tell [prtELPure opts ret]
         throwError $ msgRResults ret
    Right r -> return r

data Results a =
       XF String        -- Left e
     | XTF a String     -- Right a + Left e
     | XTFalse a        -- Right a + Right False
     deriving (Eq,Show)

data RResults a =
       RF String (Tree PE)        -- Left e
     | RTF a (Tree PE) String (Tree PE)    -- Right a + Left e
     | RTFalse a (Tree PE) (Tree PE)        -- Right a + Right False
     deriving Show

evalEP :: forall ip op i proxy . RefinedEC ip op i
  => proxy '(ip,op,i)
  -> POpts
  -> i
  -> Either (RResults (PP ip i)) (Refined op (PP ip i))
evalEP _ = evalE @ip @op

evalE :: forall ip op i . RefinedEC ip op i
  => POpts
  -> i
  -> Either (RResults (PP ip i)) (Refined op (PP ip i))
evalE opts i = runIdentity $ do
  ll@(fromTT -> t1) <- eval (Proxy @ip) opts i
  case getValLR (_tBool ll) of
       Right a -> do
         rr@(fromTT -> t2) <- evalBool (Proxy @op) opts a
         pure $ case getValLR (_tBool rr) of
              Right True -> Right (Refined a)
              Right False -> Left (RTFalse a t1 t2)
              Left e -> Left (RTF a t1 e t2)
       Left e -> pure $ Left (RF e t1)

prtE :: (Bitraversable t, Show a) => POpts -> t (RResults a) d -> t String d
prtE opts = Bi.first (prtELPure opts)

prtELPure :: Show a
   => POpts
   -> RResults a
   -> String
prtELPure opts v =
  let outmsg msg = "\n***" <> msg <> "***\n"
      msg1 a = outmsg ("Step 1. Success Initial Conversion(ip) " ++ show a)
  in case v of
       RF e t1 ->
         let (m,n) = ("Step 1. Initial Conversion(ip) Failed", e)
             r = outmsg m <> " = " <> n
              <> prtTreePure opts t1
         in r
       RTF a t1 e t2 ->
         let (m,n) = ("Step 2. Failed Boolean Check(op)", e)
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg (m <> " = " <> n)
              <> prtTreePure opts t2
         in r
       RTFalse a t1 t2 ->
         let (m,n) = ("Step 2. False Boolean Check(op)", "FalseP")
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg (m <> " = " <> n)
              <> prtTreePure opts t2
         in r

msgRResults :: RResults a -> String
msgRResults = \case
   RF e _ -> "step1:" <> e
   RTF _ _ e _ -> "step2:" <> e
   RTFalse {} -> "boolean check false"

-- convert from RefinedV style Proxy: now you can use evalEP with this
proxyEToV ::  forall ip op fmt i proxy . proxy '(ip, op, fmt, i) -> Proxy '(ip &&& (ip >> fmt),Fst >> op,i)
proxyEToV _ = Proxy

-- same as mkProxy3 but doesnt have the requirement that i ~ PP fmt (PP ip i)
mkProxy3E :: forall ip op fmt i . (PP op (PP ip i) ~ Bool, P ip i, P op (PP ip i), P fmt (PP ip i)) => Proxy '(ip,op,fmt,i)
mkProxy3E = Proxy
