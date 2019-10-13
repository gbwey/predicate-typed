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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |
-- Module      : Refined
-- Description : Traditional refinement type with only one type
-- Copyright   : (c) Grant Weyburne, 2019
-- License     : BSD-3
-- Maintainer  : gbwey9@gmail.com
--
module Refined (
    Refined
  , unsafeRefined
  , rval
  , RefinedC
  , arbitraryR
  , rapply
  , rapply0
  , rapply1
  , convertRefinedT
  , withRefinedT
  , withRefinedTIO
  , newRefined
  , newRefinedTImpl
  , newRefinedT
  , newRefinedTIO
  , RefinedT(..)
  , unRavelT
  , prtRefinedTImpl
  , prtRefinedTIO
  , prtRefinedT
 ) where
import Predicate
import UtilP
import Control.Lens hiding (strict,iall)
import Data.Proxy
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Aeson
import GHC.Generics (Generic)
import qualified Language.Haskell.TH.Syntax as TH
import System.Console.Pretty
import Test.QuickCheck

newtype Refined p a = Refined { unRefined :: a } deriving (Show, Eq, Generic, TH.Lift)

unsafeRefined :: forall p a . a -> Refined p a
unsafeRefined = Refined

instance (RefinedC p a, Read a) => Read (Refined p a) where
  readsPrec n s = do
    (a,x) <- readsPrec @a n s
    let ((_bp,_e),mr) = runIdentity $ newRefined @p o2 a
    case mr of
       Nothing -> []
       Just r -> [(r,x)]

rval :: Getter (Refined p a) a
rval afb (Refined a) = Refined <$> afb a

type RefinedC p a = (PP p a ~ Bool, P p a) -- we only care about real predicates!

instance ToJSON a => ToJSON (Refined p a) where
  toJSON = toJSON . unRefined

instance (RefinedC p a, FromJSON a) => FromJSON (Refined p a) where
  parseJSON z = do
                  a <- parseJSON z
                  let ((bp,e),mr) = runIdentity $ newRefined @p o2 a
                  case mr of
                    Nothing -> fail $ "Refined:" ++ show bp ++ "\n" ++ e
                    Just r -> return r
{-
-- need something simpler
instance (Arbitrary a, RefinedC p a) => Arbitrary (Refined p a) where
--  arbitrary = Refined <$> suchThat (arbitrary @a) (isJust . snd . runIdentity . newRefined @p)
  arbitrary = suchThatMap (arbitrary @a) (snd . runIdentity . newRefined @p o2)
-}
arbitraryR :: forall p a.
   ( Arbitrary a
   , RefinedC p a
   ) => Gen (Refined p a)
arbitraryR = suchThatMap (arbitrary @a) (snd . runIdentity . newRefined @p o2)


rapply :: forall m p a . (RefinedC p a, Monad m)
  => POpts
  -> (a -> a -> a)
  -> RefinedT m (Refined p a)
  -> RefinedT m (Refined p a)
  -> RefinedT m (Refined p a)
rapply opts f ma mb = do
  tell [bgColor Blue "=== a ==="]
  Refined x <- ma
  tell [bgColor Blue "=== b ==="]
  Refined y <- mb
  tell [bgColor Blue "=== a `op` b ==="]
  newRefinedT opts (f x y)

-- raw values
rapply0 :: forall p a m . (RefinedC p a, Monad m)
  => POpts
  -> (a -> a -> a)
  -> a
  -> a
  -> RefinedT m (Refined p a)
rapply0 opts f a b = rapply opts f (newRefinedT opts a) (newRefinedT opts b)

-- Refined
rapply1 :: forall m p a . (RefinedC p a, Monad m)
  => POpts
  -> (a -> a -> a)
  -> Refined p a
  -> Refined p a
  -> RefinedT m (Refined p a)
rapply1 opts f (Refined a) (Refined b) = newRefinedT opts (f a b)

convertRefinedT :: forall m p p1 i
  . ( RefinedC p1 i
    , Monad m)
  => POpts
  -> RefinedT m (Refined p i)
  -> RefinedT m (Refined p1 i)
convertRefinedT opts ma = do
  Refined a <- ma -- you already got a refined in there so no need to check RefinedC
  newRefinedT @p1 opts a

withRefinedT :: forall p m a b
     . (Monad m, RefinedC p a)
  => POpts
  -> a
  -> (Refined p a -> RefinedT m b)
  -> RefinedT m b
withRefinedT opts a k = newRefinedT @p opts a >>= k

withRefinedTIO :: forall p m a b
     . (MonadIO m, RefinedC p a)
  => POpts
  -> a
  -> (Refined p a -> RefinedT m b)
  -> RefinedT m b
withRefinedTIO opts a k = newRefinedTIO @p opts a >>= k

newRefined :: forall p a m . (MonadEval m, RefinedC p a)
   => POpts
   -> a
   -> m ((BoolP, String), Maybe (Refined p a))
newRefined opts a = do
  tt <- evalBool (Proxy @p) opts a
  let msg = (_tBool tt ^. boolT2P, prtTreePure opts (fromTT tt))
  pure $ (msg,) $ case getValueLR opts "" tt [] of
       Right True -> Just (Refined a)
       _ -> Nothing

newRefinedTImpl :: forall p a n m . (RefinedC p a, Monad m, MonadEval n)
  => (forall x . n x -> RefinedT m x)
  -> POpts
  -> a
  -> RefinedT m (Refined p a)
newRefinedTImpl f opts a = do
  tt <- f $ evalBool (Proxy @p) opts a
  let msg = prtTreePure opts (fromTT tt)
  tell [msg]
  case getValueLR opts "" tt [] of
    Right True -> return (Refined a) -- FalseP is also a failure!
    _ -> let rc = show (_tBool tt ^. boolT2P)
         in throwError rc -- RefinedT $ ExceptT $ WriterT $ return (Left rc, [])

newRefinedT :: forall p a m
  . ( RefinedC p a
    , Monad m)
  => POpts
  -> a
  -> RefinedT m (Refined p a)
newRefinedT = newRefinedTImpl (return . runIdentity)

newRefinedTIO :: forall p a m
  . ( RefinedC p a
    , MonadIO m)
  => POpts
  -> a
  -> RefinedT m (Refined p a)
newRefinedTIO = newRefinedTImpl liftIO

newtype RefinedT m a = RefinedT { unRefinedT :: ExceptT String (WriterT [String] m) a }
  deriving (Functor, Applicative, Monad, MonadCont, MonadWriter [String], Show, MonadIO)
  deriving MonadTrans via RefinedT

instance Monad m => MonadError String (RefinedT m) where
  throwError e = RefinedT $ ExceptT $ WriterT $ return (Left e,[])
  catchError (RefinedT (ExceptT (WriterT ma))) ema =
    RefinedT $ ExceptT $ WriterT $ do
      (lr,ss) <- ma
      case lr of
        Left e -> unRavelT (tell ss >> ema e) -- keep the old messages??
        Right _ -> ma

unRavelT :: RefinedT m a -> m (Either String a, [String])
unRavelT = runWriterT . runExceptT . unRefinedT

prtRefinedTImpl :: forall n m a . (MonadIO n, Show a) => (forall x . m x -> n x) -> RefinedT m a -> n ()
prtRefinedTImpl f rt = do
  (lr,ws) <-  f $ unRavelT rt
  liftIO $ do
    forM_ (zip [1::Int ..] ws) $ \(_,y) -> putStrLn y
    case lr of
      Left e -> putStrLn $ "failure msg[" <> e <> "]"
      Right a -> print a

prtRefinedTIO :: (MonadIO m, Show a) => RefinedT m a -> m ()
prtRefinedTIO = prtRefinedTImpl id

prtRefinedT :: (MonadIO m, Show a) => RefinedT Identity a -> m ()
prtRefinedT = prtRefinedTImpl (return . runIdentity)

