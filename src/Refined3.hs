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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
-- |
-- Module      : Refined3
-- Description : Refinement type allowing the external type to differ from the internal type
-- Copyright   : (c) Grant Weyburne, 2019
-- License     : BSD-3
-- Maintainer  : gbwey9@gmail.com
--
-- see 'Refined3'
-- contains Json / Arbitrary and Read instances
module Refined3 (
    Refined3
  , unsafeRefined3
  , Refined3C
  , r3in
  , r3out
  , arbitraryR3P
  , arbitraryR3PFun
  , mkProxy3
  , mkProxy3P
  , type MkProxyT
  , withRefined3TIO
  , withRefined3T
  , withRefined3TP
  , newRefined3T
  , newRefined3TP
  , newRefined3TPIO
  , newRefined3TPSkipIPImpl
  , convertRefined3T
  , convertRefined3TP
  , rapply3
  , rapply3P
  , prtEval3P
  , eval3P
  , eval3
  , eval3M
  , eval3MSkip
  , eval3MQuickIdentity
  , eval3MQuick
  , eval3PX
  , eval3X
  , prt3IO
  , prt3
  , Msg3 (..)
  , prt3Impl
  , type MakeR3
  , Results (..)
  , RResults (..)
 ) where
import Refined
import Predicate
import UtilP
import Control.Lens hiding (strict,iall)
import Data.Tree
import Data.Proxy
import Control.Monad.Except
import Control.Monad.Writer
import Data.Aeson
import qualified Language.Haskell.TH.Syntax as TH
import System.Console.Pretty
import Test.QuickCheck

-- | Refinement type that differentiates input type from output type
-- 'i' is the input type
-- 'ip' converts i to PP ip i which is the internal type
-- 'op' validates that internal type using PP op (PP ip i) ~ Bool
-- 'fmt' outputs the internal type PP fmt (PP ip i) ~ i
-- PP fmt (PP ip i) should be valid input to Refined3
data Refined3 ip op fmt i = Refined3 { in3 :: PP ip i, out3 :: PP fmt (PP ip i) }

unsafeRefined3 :: forall ip op fmt i . PP ip i -> PP fmt (PP ip i) -> Refined3 ip op fmt i
unsafeRefined3 = Refined3

-- | Provides the constraints on Refined3
type Refined3C ip op fmt i = (P ip i, P op (PP ip i), PP op (PP ip i) ~ Bool, P fmt (PP ip i), PP fmt (PP ip i) ~ i)

-- | Getters on Refined3
r3in :: Getter (Refined3 ip op fmt i) (PP ip i)
r3in afb (Refined3 a x) = flip Refined3 x <$> afb a

r3out :: Getter (Refined3 ip op fmt i) (PP fmt (PP ip i))
r3out afb (Refined3 x a) = Refined3 x <$> afb a

deriving instance (Show i, Show (PP ip i), Show (PP fmt (PP ip i))) => Show (Refined3 ip op fmt i)
deriving instance (Eq i, Eq (PP ip i), Eq (PP fmt (PP ip i))) => Eq (Refined3 ip op fmt i)
deriving instance (TH.Lift (PP ip i), TH.Lift (PP fmt (PP ip i))) => TH.Lift (Refined3 ip op fmt i)

-- | Read instance for Refined3
instance (Show i, Show (PP ip i), Refined3C ip op fmt i, Read i) => Read (Refined3 ip op fmt i) where
  readsPrec n s = do
    (a,x) <- readsPrec @i n s
    let (_ret,mr) = eval3 @ip @op @fmt o2 a
    case mr of
      Nothing -> [] -- error $ show (prt3Impl _ret)
      Just r -> [(r,x)]

instance ToJSON (PP fmt (PP ip i)) => ToJSON (Refined3 ip op fmt i) where
  toJSON = toJSON . out3

-- reads external value
instance (Show (PP fmt (PP ip i)), Show (PP ip i), Refined3C ip op fmt i, FromJSON i) => FromJSON (Refined3 ip op fmt i) where
  parseJSON z = do
                  i <- parseJSON @i z
                  let (ret,mr) = eval3 @ip @op @fmt o2 i
                  case mr of
                    Nothing -> fail $ "Refined3:" ++ show (prt3Impl o2 ret)
                    Just r -> return r

-- need something simpler
{-
instance (Arbitrary (PP ip i)
        , Show (PP ip i)
        , Show i
        , Refined3C ip op fmt i
        ) => Arbitrary (Refined3 ip op fmt i) where
  arbitrary = suchThatMap (arbitrary @(PP ip i)) $ eval3MQuickIdentity @ip @op @fmt o2
-}
arbitraryR3P :: forall ip op fmt i .
   ( Arbitrary (PP ip i)
   , Show (PP ip i)
   , Show i
   , Refined3C ip op fmt i
   ) => Proxy '(ip,op,fmt,i) -> Gen (Refined3 ip op fmt i)
arbitraryR3P _ = suchThatMap (arbitrary @(PP ip i)) $ eval3MQuickIdentity @ip @op @fmt o2

-- help things along a little
arbitraryR3PFun ::
    forall ip op fmt i
  . (Arbitrary (PP ip i)
   , Show (PP ip i)
   , Show i
   , Refined3C ip op fmt i)
  => Proxy '(ip,op,fmt,i)
  -> POpts
  -> (PP ip i -> PP ip i)
  -> Gen (Refined3 ip op fmt i)
arbitraryR3PFun _ opts f =
  suchThatMap (f <$> arbitrary @(PP ip i)) $ eval3MQuickIdentity @ip @op @fmt opts

mkProxy3 :: forall ip op fmt i . Refined3C ip op fmt i => Proxy '(ip,op,fmt,i)
mkProxy3 = Proxy

-- checks to make sure the proxy is consistent with Refined3C: you can pass in a promoted 4 tuple
mkProxy3P :: forall z ip op fmt i . (z ~ '(ip,op,fmt,i), Refined3C ip op fmt i) => Proxy '(ip,op,fmt,i)
mkProxy3P = Proxy

-- convert from '(ip,op,fmt,i) to Refined3 signature
type family MakeR3 p where
  MakeR3 '(ip,op,fmt,i) = Refined3 ip op fmt i

type family MkProxyT p where
  MkProxyT '(ip,op,fmt,i) = Proxy '(ip,op,fmt,i)

withRefined3TIO :: forall ip op fmt i m b
  . (MonadIO m, Refined3C ip op fmt i, Show (PP ip i), Show i)
  => POpts
  -> i
  -> (Refined3 ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined3TIO opts = (>>=) . newRefined3TPIO (Proxy @'(ip,op,fmt,i)) opts

withRefined3T :: forall ip op fmt i m b
  . (Monad m, Refined3C ip op fmt i, Show (PP ip i), Show i)
  => POpts
  -> i
  -> (Refined3 ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined3T opts = (>>=) . newRefined3TP (Proxy @'(ip,op,fmt,i)) opts

withRefined3TP :: forall ip op fmt i m b proxy
  . (Monad m, Refined3C ip op fmt i, Show (PP ip i), Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> (Refined3 ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined3TP p opts = (>>=) . newRefined3TP p opts

newRefined3T :: forall m ip op fmt i . (Refined3C ip op fmt i, Monad m, Show (PP ip i), Show i)
   => POpts
   -> i
   -> RefinedT m (Refined3 ip op fmt i)
newRefined3T = newRefined3TP (Proxy @'(ip,op,fmt,i))

newRefined3TP :: forall m ip op fmt i proxy
   . (Refined3C ip op fmt i, Monad m, Show (PP ip i), Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> RefinedT m (Refined3 ip op fmt i)
newRefined3TP = newRefined3TPImpl (return . runIdentity)

newRefined3TPIO :: forall m ip op fmt i proxy
   . (Refined3C ip op fmt i, MonadIO m, Show (PP ip i), Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> RefinedT m (Refined3 ip op fmt i)
newRefined3TPIO = newRefined3TPImpl liftIO

-- we call this cos we need to do the bool check and get fmt value
-- eval (PP op i) ~ True and eval (PP fmt i) to get the other value
-- input is set to @Id meaning that PP fmt (PP ip i) /= i doesnt hold
newRefined3TPImpl :: forall n m ip op fmt i proxy
   . (Refined3C ip op fmt i, Monad m, MonadEval n, Show (PP ip i), Show (PP fmt (PP ip i)))
  => (forall x . n x -> RefinedT m x)
   -> proxy '(ip,op,fmt,i)
   -> POpts
   -> i
   -> RefinedT m (Refined3 ip op fmt i)
newRefined3TPImpl f _ opts i = do
  (ret,mr) <- f $ eval3M opts i
  let m3 = prt3Impl opts ret
  tell [m3Long m3]
  case mr of
    Nothing -> throwError $ m3Desc m3 <> " | " <> m3Short m3
    Just r -> return r

newRefined3TPSkipIPImpl :: forall n m ip op fmt i proxy
   . (Refined3C ip op fmt i
    , Monad m
    , MonadEval n
    , Show (PP ip i)
    , Show (PP fmt (PP ip i)))
  => (forall x . n x -> RefinedT m x)
   -> proxy '(ip,op,fmt,i)
   -> POpts
   -> PP ip i
   -> RefinedT m (Refined3 ip op fmt i)
newRefined3TPSkipIPImpl f _ opts a = do
  (ret,mr) <- f $ eval3MSkip opts a
  let m3 = prt3Impl opts ret
  tell [m3Long m3]
  case mr of
    Nothing -> throwError $ m3Desc m3 <> " | " <> m3Short m3
    Just r -> return r

-- optional Refined3C ip op fmt i [not required!]
convertRefined3T :: forall m ip op fmt i ip1 op1 fmt1 i1 .
  ( Refined3C ip1 op1 fmt1 i1
  , Monad m
  , Show (PP ip i)
  , PP ip i ~ PP ip1 i1
  , Show i1)
  => POpts
  -> RefinedT m (Refined3 ip op fmt i)
  -> RefinedT m (Refined3 ip1 op1 fmt1 i1)
convertRefined3T opts ma = do
  Refined3 x _ <- ma
  -- we skip the input value @Id and go straight to the internal value so PP fmt (PP ip i) /= i for this call
  Refined3 a b <- newRefined3TPSkipIPImpl (return . runIdentity) (Proxy @'(ip1, op1, fmt1, i1)) opts x
  return (Refined3 a b)

convertRefined3TP :: forall m ip op fmt i ip1 op1 fmt1 i1 .
  ( Refined3C ip1 op1 fmt1 i1
  , Monad m
  , Show (PP ip i)
  , PP ip i ~ PP ip1 i1
  , Show i1)
  => Proxy '(ip, op, fmt, i)
  -> Proxy '(ip1, op1, fmt1, i1)
  -> POpts
  -> RefinedT m (Refined3 ip op fmt i)
  -> RefinedT m (Refined3 ip1 op1 fmt1 i1)
convertRefined3TP _ _ opts ma = do
  Refined3 x _ <- ma
  -- we skip the input value @Id and go straight to the internal value so PP fmt (PP ip i) /= i for this call
  Refined3 a b <- newRefined3TPSkipIPImpl (return . runIdentity) (Proxy @'(ip1, op1, fmt1, i1)) opts x
  return (Refined3 a b)

rapply3 :: forall m ip op fmt i .
  ( Refined3C ip op fmt i
  , Monad m
  , Show (PP ip i)
  , Show i)
  => POpts
  -> (PP ip i -> PP ip i -> PP ip i)
  -> RefinedT m (Refined3 ip op fmt i)
  -> RefinedT m (Refined3 ip op fmt i)
  -> RefinedT m (Refined3 ip op fmt i)
rapply3 = rapply3P (Proxy @'(ip,op,fmt,i))

-- this is the most generic
-- prtRefinedT $ rapply3P base16 (+) (newRefined3TP Proxy "ff") (newRefined3TP Proxy "22")
rapply3P :: forall m ip op fmt i proxy .
  ( Refined3C ip op fmt i
  , Monad m
  , Show (PP ip i)
  , Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> (PP ip i -> PP ip i -> PP ip i)
  -> RefinedT m (Refined3 ip op fmt i)
  -> RefinedT m (Refined3 ip op fmt i)
  -> RefinedT m (Refined3 ip op fmt i)
rapply3P p opts f ma mb = do
  tell [bgColor Blue "=== a ==="]
  Refined3 x _ <- ma
  tell [bgColor Blue "=== b ==="]
  Refined3 y _ <- mb
  -- we skip the input value @Id and go straight to the internal value so PP fmt (PP ip i) /= i for this call
  tell [bgColor Blue "=== a `op` b ==="]
  Refined3 a b <- newRefined3TPSkipIPImpl (return . runIdentity) p opts (f x y)
  return (Refined3 a b)

data Results a b =
       XF String        -- Left e
     | XTF a String     -- Right a + Left e
     | XTFalse a        -- Right a + Right False
     | XTTrueF a String -- Right a + Right True + Left e
     | XTTrueT a b      -- Right a + Right True + Right b
     deriving (Show,Eq)

data RResults a b =
       RF String (Tree PE)        -- Left e
     | RTF a (Tree PE) String (Tree PE)    -- Right a + Left e
     | RTFalse a (Tree PE) (Tree PE)        -- Right a + Right False
     | RTTrueF a (Tree PE) (Tree PE) String (Tree PE) -- Right a + Right True + Left e
     | RTTrueT a (Tree PE) (Tree PE) b (Tree PE)      -- Right a + Right True + Right b
     deriving Show

prtEval3P :: forall ip op fmt i proxy
  . ( Refined3C ip op fmt i
    , Show (PP ip i)
    , Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> IO (Either String (Refined3 ip op fmt i))
prtEval3P _ opts i = do
  x <- eval3M opts i
  prt3IO opts x

-- pass in a proxy (use mkProxy to package all the types together as a 4-tuple)
-- ip converts input 'i' to format used for op and fmt
-- op is a boolean predicate [has to be True to continue] (uses P ip i as input)
-- fmt formats the output (can be anything ie not just String) (uses P ip i as input)
eval3P :: forall ip op fmt i proxy . Refined3C ip op fmt i
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> (RResults (PP ip i) (PP fmt (PP ip i)), Maybe (Refined3 ip op fmt i))
eval3P _ opts = runIdentity . eval3M opts

-- same as eval3P but can just pass in ip op fmt separately
-- with eval3P we have to specify 'i' but in eval3 we dont cos gets it from context
eval3 :: forall ip op fmt i . Refined3C ip op fmt i
  => POpts
  -> i
  -> (RResults (PP ip i) (PP fmt (PP ip i)), Maybe (Refined3 ip op fmt i))
eval3 opts = runIdentity . eval3M opts

eval3M :: forall m ip op fmt i . (MonadEval m, Refined3C ip op fmt i)
  => POpts
  -> i
  -> m (RResults (PP ip i) (PP fmt (PP ip i)), Maybe (Refined3 ip op fmt i))
eval3M opts i = do
  ll@(fromTT -> t1) <- eval (Proxy @ip) opts i
  case getValLR (_tBool ll) of
       Right a -> do
         rr@(fromTT -> t2) <- evalBool (Proxy @op) opts a
         case getValLR (_tBool rr) of
              Right True -> do
                ss@(fromTT -> t3) <- eval (Proxy @fmt) opts a
                pure $ case getValLR (_tBool ss) of
                     Right b -> (RTTrueT a t1 t2 b t3, Just (Refined3 a b))
                     Left e -> (RTTrueF a t1 t2 e t3, Nothing)
              Right False -> pure (RTFalse a t1 t2, Nothing)
              Left e -> pure (RTF a t1 e t2, Nothing)
       Left e -> pure (RF e t1, Nothing)

-- skip ip conversion: ie uses internal value
eval3MSkip :: forall m ip op fmt i . (MonadEval m, Refined3C ip op fmt i)
   => POpts
   -> PP ip i
   -> m (RResults (PP ip i) (PP fmt (PP ip i)), Maybe (Refined3 ip op fmt i))
eval3MSkip opts a = do
   let t1 = Node (PE (TrueP) ["skipped PP ip i = Id"]) []
   rr@(fromTT -> t2) <- evalBool (Proxy @op) opts a
   case getValLR (_tBool rr) of
        Right True -> do
          ss@(fromTT -> t3) <- eval (Proxy @fmt) opts a
          pure $ case getValLR (_tBool ss) of
               Right b -> (RTTrueT a t1 t2 b t3, Just (Refined3 a b))
               Left e -> (RTTrueF a t1 t2 e t3, Nothing)
        Right False -> pure (RTFalse a t1 t2, Nothing)
        Left e -> pure (RTF a t1 e t2, Nothing)

-- calculates from internal value
eval3MQuickIdentity :: forall ip op fmt i . Refined3C ip op fmt i
   => POpts
   -> PP ip i
   -> Maybe (Refined3 ip op fmt i)
eval3MQuickIdentity opts = runIdentity . eval3MQuick opts

-- from PP ip i
eval3MQuick :: forall m ip op fmt i . (MonadEval m, Refined3C ip op fmt i)
   => POpts
   -> PP ip i
   -> m (Maybe (Refined3 ip op fmt i))
eval3MQuick opts a = do
  rr <- evalBool (Proxy @op) opts a
  case getValLR (_tBool rr) of
    Right True -> do
      ss <- eval (Proxy @fmt) opts a
      pure $ case getValLR (_tBool ss) of
        Right b -> Just (Refined3 a b)
        _ -> Nothing
    _ -> pure Nothing

-- reuses the mkProxy3 but returns Refined vs Refined3
-- using plain Refined to emulate Refined3 sort of
-- we just output fmt instead of embedding it in Refined3
-- so 'ip' predicate gets us started: we store that (PP ip i) in Refined
-- then we run the boolean predicate 'op' which is successful if true
-- then we run 'fmt' against (PP ip i) and output both the Refined (PP p i) and the PP fmt (PP (ip i)) ie fmt run against PP ip i
--       if any of the three steps fails the process stops immediately and dumps out RResults
eval3PX :: forall ip op fmt i proxy . Refined3C ip op fmt i
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> (RResults (PP ip i) (PP fmt (PP ip i)), Maybe (Refined op (PP ip i), PP fmt (PP ip i)))
eval3PX _ = eval3X @ip @op @fmt

eval3X :: forall ip op fmt i . Refined3C ip op fmt i
  => POpts
  -> i
  -> (RResults (PP ip i) (PP fmt (PP ip i)), Maybe (Refined op (PP ip i), PP fmt (PP ip i)))
eval3X opts i = runIdentity $ do
  ll@(fromTT -> t1) <- eval (Proxy @ip) opts i
  case getValLR (_tBool ll) of
       Right a -> do
         rr@(fromTT -> t2) <- evalBool (Proxy @op) opts a
         case getValLR (_tBool rr) of
              Right True -> do
                ss@(fromTT -> t3) <- eval (Proxy @fmt) opts a
                pure $ case getValLR (_tBool ss) of
                     Right b -> (RTTrueT a t1 t2 b t3, Just (unsafeRefined a, b))
                     Left e -> (RTTrueF a t1 t2 e t3, Nothing)
              Right False -> pure (RTFalse a t1 t2, Nothing)
              Left e -> pure (RTF a t1 e t2, Nothing)
       Left e -> pure (RF e t1, Nothing)

prt3IO :: (Show a, Show b) => POpts -> (RResults a b, Maybe r) -> IO (Either String r)
prt3IO opts (ret,mr) = do
  let m3 = prt3Impl opts ret
  putStrLn $ m3Long m3
  return $ maybe (Left (m3Desc m3 <> " | " <> m3Short m3)) Right mr

prt3 :: (Show a, Show b) => POpts -> (RResults a b, Maybe r) -> Either Msg3 r
prt3 opts (ret,mr) = maybe (Left $ prt3Impl opts ret) Right mr

data Msg3 = Msg3 { m3Desc :: String, m3Short :: String, m3Long :: String } deriving Eq
instance Show Msg3 where
  show (Msg3 a b c) = a <> " | " <> b <> "\n" <> c

prt3Impl :: (Show a, Show b)
  => POpts
  -> RResults a b
  -> Msg3
prt3Impl opts v =
  let outmsg msg = "\n***" <> msg <> " ***\n\n"
      msg1 a = outmsg ("Step 1. Success Initial Conversion(ip) = " ++ show a)
  in case v of
       RF e t1 ->
         let (m,n) = ("Step 1. Initial Conversion(ip) Failed", e)
             r = outmsg m <> " = " <> n
              <> prtTreePure opts t1
         in Msg3 m n r
       RTF a t1 e t2 ->
         let (m,n) = ("Step 2. Failed Boolean Check(op)", e)
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg (m <> " = " <> n)
              <> prtTreePure opts t2
         in Msg3 m n r
       RTFalse a t1 t2 ->
         let (m,n) = ("Step 2. False Boolean Check(op)", "FalseP")
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg (m <> " = " <> n)
              <> prtTreePure opts t2
         in Msg3 m n r
       RTTrueF a t1 t2 e t3 ->
         let (m,n) = ("Step 3. Failed Output Conversion(fmt)", e)
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg "Step 2. Success Boolean Check(op)"
              <> prtTreePure opts t2
              <> outmsg (m <> " = " <> n)
              <> prtTreePure opts t3
         in Msg3 m n r
       RTTrueT a t1 t2 b t3 ->
         let (m,n) = ("Step 3. Success Output Conversion(fmt)", show b)
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg "Step 2. Success Boolean Check(op)"
              <> prtTreePure opts t2
              <> outmsg (m <> " = " <> n)
              <> fixLite opts b t3
         in Msg3 m n r

