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
    Refined(unRefined)
  , unsafeRefined
  , RefinedC
  , arbRefined
  , rapply
  , rapply0
  , rapply1
  , convertRefinedT
  , withRefinedT
  , withRefinedTIO
  , newRefined
  , prtRefinedIO
  , newRefinedT
  , newRefinedTIO
  , RefinedT(..)
  , prtRefinedTIO
  , prtRefinedT
 ) where
import Predicate
import UtilP
import Control.Lens hiding (strict,iall)
import Data.Proxy
import Control.Monad.Except
import Control.Monad.Writer (WriterT(..), runWriterT, MonadWriter, tell)
import Control.Monad.Cont
import Data.Aeson
import GHC.Generics (Generic)
import qualified Language.Haskell.TH.Syntax as TH
import System.Console.Pretty
import Test.QuickCheck

-- | a simple refinement type that ensures the predicate \'p\' holds for the type \'a\'
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> prtRefinedIO @(Between 10 14) ol 13
-- Right (Refined {unRefined = 13})
--
-- >>> prtRefinedIO @(Between 10 14) ol 99
-- Left FalseP
--
-- >>> prtRefinedIO @(Last >> Len == 4) ol ["one","two","three","four"]
-- Right (Refined {unRefined = ["one","two","three","four"]})
--
-- >>> prtRefinedIO @(Re "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$") ol "141.213.1.99"
-- Right (Refined {unRefined = "141.213.1.99"})
--
-- >>> prtRefinedIO @(Re "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$") ol "141.213.1"
-- Left FalseP
--
-- >>> prtRefinedIO @(Resplit "\\." >> Map (ReadP Int) >> Guard (Len >> Printf "bad length: found %d") (Len == 4) >> 'True) ol "141.213.1"
-- Left (FailP "bad length: found 3")
--
-- >>> prtRefinedIO @(Resplit "\\." >> Map (ReadP Int) >> Guard (Len >> Printf "bad length: found %d") (Len == 4) >> GuardsQuick (Printf2 "octet %d out of range %d") (RepeatT 4 (Between 0 255)) >> 'True) ol "141.213.1.444"
-- Left (FailP "octet 3 out of range 444")
--
-- >>> prtRefinedIO @(Resplit "\\." >> Map (ReadP Int) >> Guard (Len >> Printf "bad length: found %d") (Len == 4) >> GuardsQuick (Printf2 "octet %d out of range %d") (RepeatT 4 (Between 0 255)) >> 'True) ol "141.213.1x34.444"
-- Left (FailP "ReadP Int (1x34) failed")
--
-- >>> prtRefinedIO @(Map ('[Id] >> ReadP Int) >> Luhn) ol "12344"
-- Right (Refined {unRefined = "12344"})
--
-- >>> prtRefinedIO @(Map ('[Id] >> ReadP Int) >> Luhn) ol "12340"
-- Left FalseP
--
-- >>> prtRefinedIO @(Any Prime) ol [11,13,17,18]
-- Right (Refined {unRefined = [11,13,17,18]})
--
-- >>> prtRefinedIO @(All Prime) ol [11,13,17,18]
-- Left FalseP
--
-- >>> prtRefinedIO @(Snd !! Fst >> Len > 5) ol (2,["abc","defghij","xyzxyazsfd"])
-- Right (Refined {unRefined = (2,["abc","defghij","xyzxyazsfd"])})
--
-- >>> prtRefinedIO @(Snd !! Fst >> Len > 5) ol (27,["abc","defghij","xyzxyazsfd"])
-- Left (FailP "(!!) index not found")
--
-- >>> prtRefinedIO @(Snd !! Fst >> Len <= 5) ol (2,["abc","defghij","xyzxyazsfd"])
-- Left FalseP
newtype Refined p a = Refined { unRefined :: a } deriving (Show, Eq, Generic, TH.Lift)

-- | 'Read' instance for 'Refined'
instance (RefinedC p a, Read a) => Read (Refined p a) where
  readsPrec n s = do
    (a,x) <- readsPrec @a n s
    let ((_bp,_e),mr) = runIdentity $ newRefined @p o2 a
    case mr of
       Nothing -> []
       Just r -> [(r,x)]

-- | the constraints that 'Refined' must adhere to
type RefinedC p a = (PP p a ~ Bool, P p a)

-- | 'ToJSON' instance for 'Refined'
instance ToJSON a => ToJSON (Refined p a) where
  toJSON = toJSON . unRefined

-- | 'FromJSON' instance for 'Refined'
instance (RefinedC p a, FromJSON a) => FromJSON (Refined p a) where
  parseJSON z = do
                  a <- parseJSON z
                  let ((bp,e),mr) = runIdentity $ newRefined @p o2 a
                  case mr of
                    Nothing -> fail $ "Refined:" ++ show bp ++ "\n" ++ e
                    Just r -> return r

-- | 'arbitrary' value for 'Refined'
arbRefined :: forall p a.
   ( Arbitrary a
   , RefinedC p a
   ) => POpts -> Gen (Refined p a)
arbRefined opts = suchThatMap (arbitrary @a) (snd . runIdentity . newRefined @p opts)


-- | binary operation applied to two 'RefinedT' values
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

-- | takes two values and lifts them into 'RefinedT' and then applies the binary operation
rapply0 :: forall p a m . (RefinedC p a, Monad m)
  => POpts
  -> (a -> a -> a)
  -> a
  -> a
  -> RefinedT m (Refined p a)
rapply0 opts f a b = rapply opts f (newRefinedT opts a) (newRefinedT opts b)

-- | same as 'rapply' except we already have valid 'Refined' values as input
rapply1 :: forall m p a . (RefinedC p a, Monad m)
  => POpts
  -> (a -> a -> a)
  -> Refined p a
  -> Refined p a
  -> RefinedT m (Refined p a)
rapply1 opts f (Refined a) (Refined b) = newRefinedT opts (f a b)

-- | attempts to lift a refinement type to another refinement type by way of transformation function
--   you can control both the predicate and the type
convertRefinedT :: forall m p p1 a a1
  . ( RefinedC p1 a1
    , Monad m)
  => POpts
  -> (a -> a1)
  -> RefinedT m (Refined p a)
  -> RefinedT m (Refined p1 a1)
convertRefinedT opts f ma = do
  Refined a <- ma -- you already got a refined in there so no need to check RefinedC
  newRefinedT @p1 opts (f a)

-- | invokes the callback with the 'Refined' value if \'a\' is valid for the predicate \'p\'
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

-- | same as 'newRefined' but prints the results
prtRefinedIO :: forall p a
   . RefinedC p a
   => POpts
   -> a
   -> IO (Either BoolP (Refined p a))
prtRefinedIO opts a = do
  tt <- evalBool (Proxy @p) opts a
  let msg = (_tBool tt ^. boolT2P, prtTreePure opts (fromTT tt))
  unless (oLite opts) $ putStrLn $ snd msg
  pure $ case getValueLR opts "" tt [] of
    Right True -> Right (Refined a)
    _ -> Left (fst msg)

-- | returns a 'Refined' value if \'a\' is valid for the predicate \'p\'
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

-- | returns a wrapper 'RefinedT' around a possible 'Refined' value if \'a\' is valid for the predicate \'p\'
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

-- | a way to unsafely create a 'Refined' value
unsafeRefined :: forall p a . a -> Refined p a
unsafeRefined = Refined
