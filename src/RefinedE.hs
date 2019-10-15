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
{- |
Module      : RefinedE
Description : Refinement type that fakes Refined3 using only Refined
Copyright   : (c) Grant Weyburne, 2019
License     : BSD-3
Maintainer  : gbwey9@gmail.com

This is a hybrid between "Refined" and "Refined3"
It dispenses with \'fmt\' from 'Refined3.Refined3' and emulates 'Refined3.Refined3' by packing parts values 'Refined'
The whole thing is driven by 3-tuple proxy '(ip,op,i)
Since we dont store the input then the input conversion has nothing to do with Refined and is instead part of a separate conversion process
-}
module RefinedE (
    mkProxyE
  , withRefinedETP
  , newRefinedE
  , newRefinedEP
  , evalEP
  , evalE
  , prtE
  , prtELPure
  , msgRResults
  , proxyEToV
  , mkProxy3E
  , Results (..)
  , RResults (..)
  , RefinedEC
  , MakeRT
  ) where
import Refined
import Predicate
import UtilP
import Control.Lens hiding (strict,iall)
import Data.Tree
import Data.Proxy
import Control.Monad.Except
import Control.Monad.Writer (tell)
import Data.Bitraversable
import qualified Data.Bifunctor as Bi

mkProxyE :: forall ip op i . Proxy '(ip,op,i)
mkProxyE = Proxy

-- | the class constraints required for this refinement type
type RefinedEC ip op i = (P ip i, P op (PP ip i), PP op (PP ip i) ~ Bool)

type MakeRT m p = RefinedT m (MakeRE p)

-- | convert from \'(ip,op,fmt,i) to RefinedE signature
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
              Right True -> Right $ unsafeRefined a
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
  let outmsg msg = "\n*** " <> msg <> " ***\n"
      msg1 a = outmsg ("Step 1. Success Initial Conversion(ip) [" ++ show a ++ "]")
  in case v of
       RF e t1 ->
         let (m,_n) = ("Step 1. Initial Conversion(ip) Failed", e)
             r = outmsg m
              <> prtTreePure opts t1
         in r
       RTF a t1 e t2 ->
         let (m,_n) = ("Step 2. Failed Boolean Check(op)", e)
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg m
              <> prtTreePure opts t2
         in r
       RTFalse a t1 t2 ->
         let m = "Step 2. False Boolean Check(op)"
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg m
              <> prtTreePure opts t2
         in r

msgRResults :: RResults a -> String
msgRResults = \case
   RF e _ -> "step1:" <> e
   RTF _ _ e _ -> "step2:" <> e
   RTFalse {} -> "boolean check false"

-- | convert from 'Refined3.Refined3' Proxy: now you can use 'evalEP' with this
proxyEToV ::  forall ip op fmt i proxy . proxy '(ip, op, fmt, i) -> Proxy '(ip &&& (ip >> fmt),Fst >> op,i)
proxyEToV _ = Proxy

-- | same as 'Refined3.mkProxy3' but doesnt have the requirement that \'i ~ PP fmt (PP ip i)\'
mkProxy3E :: forall ip op fmt i . (PP op (PP ip i) ~ Bool, P ip i, P op (PP ip i), P fmt (PP ip i)) => Proxy '(ip,op,fmt,i)
mkProxy3E = Proxy
