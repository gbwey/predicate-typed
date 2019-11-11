{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveLift #-}
{- |
     Simple refinement type with only one type and a predicate
-}
module Predicate.Refined (
  -- ** Refined
    Refined(unRefined)
  , RefinedC
  , newRefined
  , RefinedT(..)

  -- ** print methods
  , prtRefinedIO
  , prtRefinedTIO
  , prtRefinedT

  -- ** create methods
  , withRefinedT
  , withRefinedTIO
  , newRefinedT
  , newRefinedTIO

  -- ** QuickCheck method
  , arbRefined

  -- ** manipulate RefinedT values
  , convertRefinedT
  , unRavelT
  , unRavelTIO
  , rapply
  , rapply0
  , rapply1

  -- ** unsafe create methods
  , unsafeRefined
  , unsafeRefined'
 ) where
import Predicate.Core
import Predicate.Util
import Control.Lens ((^.))
import Data.Functor.Identity (Identity(..))
import Data.Proxy
import Control.Monad.Except
import Control.Monad.Writer (WriterT(..), runWriterT, MonadWriter, tell)
import Control.Monad.Cont
import Data.Aeson (ToJSON(..), FromJSON(..))
import GHC.Generics (Generic)
import qualified Language.Haskell.TH.Syntax as TH
import System.Console.Pretty
import Test.QuickCheck
import qualified GHC.Read as GR
import qualified Text.ParserCombinators.ReadPrec as PCR
import qualified Text.Read.Lex as RL
import qualified Data.Binary as B
import Data.Binary (Binary)
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XNoStarIsType
-- >>> :set -XOverloadedStrings
-- >>> :m + Predicate.Prelude

-- | a simple refinement type that ensures the predicate \'p\' holds for the type \'a\'
--
-- >>> prtRefinedIO @(Between 10 14) oz 13
-- Right (Refined {unRefined = 13})
--
-- >>> prtRefinedIO @(Between 10 14) oz 99
-- Left FalseP
--
-- >>> prtRefinedIO @(Last Id >> Len == 4) oz ["one","two","three","four"]
-- Right (Refined {unRefined = ["one","two","three","four"]})
--
-- >>> prtRefinedIO @(Re "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$" Id) oz "141.213.1.99"
-- Right (Refined {unRefined = "141.213.1.99"})
--
-- >>> prtRefinedIO @(Re "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$" Id) oz "141.213.1"
-- Left FalseP
--
-- >>> prtRefinedIO @(Map (ReadP Int Id) (Resplit "\\." Id) >> Guard (PrintF "bad length: found %d" Len) (Len == 4) >> 'True) oz "141.213.1"
-- Left (FailP "bad length: found 3")
--
-- >>> prtRefinedIO @(Map (ReadP Int Id) (Resplit "\\." Id) >> Guard (PrintF "bad length: found %d" Len) (Len == 4) >> GuardsN (PrintT "octet %d out of range %d" Id) 4 (Between 0 255) >> 'True) oz "141.213.1.444"
-- Left (FailP "octet 3 out of range 444")
--
-- >>> prtRefinedIO @(Map (ReadP Int Id) (Resplit "\\." Id) >> Guard (PrintF "bad length: found %d" Len) (Len == 4) >> GuardsN (PrintT "octet %d out of range %d" Id) 4 (Between 0 255) >> 'True) oz "141.213.1x34.444"
-- Left (FailP "ReadP Int (1x34) failed")
--
-- >>> prtRefinedIO @(Map ('[Id] >> ReadP Int Id) Id >> Luhn Id) oz "12344"
-- Right (Refined {unRefined = "12344"})
--
-- >>> prtRefinedIO @(Map ('[Id] >> ReadP Int Id) Id >> Luhn Id) oz "12340"
-- Left FalseP
--
-- >>> prtRefinedIO @(Any (Prime Id) Id) oz [11,13,17,18]
-- Right (Refined {unRefined = [11,13,17,18]})
--
-- >>> prtRefinedIO @(All (Prime Id) Id) oz [11,13,17,18]
-- Left FalseP
--
-- >>> prtRefinedIO @(Snd Id !! Fst Id >> Len > 5) oz (2,["abc","defghij","xyzxyazsfd"])
-- Right (Refined {unRefined = (2,["abc","defghij","xyzxyazsfd"])})
--
-- >>> prtRefinedIO @(Snd Id !! Fst Id >> Len > 5) oz (27,["abc","defghij","xyzxyazsfd"])
-- Left (FailP "(!!) index not found")
--
-- >>> prtRefinedIO @(Snd Id !! Fst Id >> Len <= 5) oz (2,["abc","defghij","xyzxyazsfd"])
-- Left FalseP
newtype Refined p a = Refined { unRefined :: a } deriving (Show, Eq, Generic, TH.Lift)

-- | 'Read' instance for 'Refined'
--
-- >>> reads @(Refined (Between 0 255) Int) "Refined {unRefined = 254}"
-- [(Refined {unRefined = 254},"")]
--
-- >>> reads @(Refined (Between 0 255) Int) "Refined {unRefined = 300}"
-- []
--
instance (RefinedC p a, Read a) => Read (Refined p a) where
  readPrec
    = GR.parens
        (PCR.prec
           11
           (do GR.expectP (RL.Ident "Refined")
               GR.expectP (RL.Punc "{")
               fld0 <- GR.readField
                             "unRefined"
                             (PCR.reset GR.readPrec)
               GR.expectP (RL.Punc "}")
               let (_,mr) = runIdentity $ newRefined @p oz fld0 -- since we cant display the failure message ...
               case mr of
                 Nothing -> fail ""
                 Just _r -> pure (Refined fld0)
           ))
  readList = GR.readListDefault
  readListPrec = GR.readListPrecDefault

-- | the constraints that 'Refined' must adhere to
type RefinedC p a = (PP p a ~ Bool, P p a)

-- | 'ToJSON' instance for 'Refined'
instance ToJSON a => ToJSON (Refined p a) where
  toJSON = toJSON . unRefined

-- | 'FromJSON' instance for 'Refined'
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode' @(Refined (Between 10 14) Int) "13"
-- Right (Refined {unRefined = 13})
--
-- >>> removeAnsi $ A.eitherDecode' @(Refined (Between 10 14) Int) "16"
-- Error in $: Refined:FalseP
-- False 16 <= 14
-- |
-- +- P Id 16
-- |
-- +- P '10
-- |
-- `- P '14
-- <BLANKLINE>
--
instance (RefinedC p a, FromJSON a) => FromJSON (Refined p a) where
  parseJSON z = do
                  a <- parseJSON z
                  let ((bp,(e,_top)),mr) = runIdentity $ newRefined @p o2 a
                  case mr of
                    Nothing -> fail $ "Refined:" ++ show bp ++ "\n" ++ e
                    Just r -> return r

-- | 'Binary' instance for 'Refined'
--
-- >>> import Data.Time
-- >>> import Control.Lens
-- >>> import Control.Arrow ((+++))
-- >>> type K1 = Refined (ReadP Day Id >> 'True) String
-- >>> type K2 = Refined (ReadP Day Id >> Between (ReadP Day "2019-03-30") (ReadP Day "2019-06-01")) String
-- >>> type K3 = Refined (ReadP Day Id >> Between (ReadP Day "2019-05-30") (ReadP Day "2019-06-01")) String
-- >>> r = unsafeRefined' oz "2019-04-23" :: K1
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K1 (B.encode r)
-- Refined {unRefined = "2019-04-23"}
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K2 (B.encode r)
-- Refined {unRefined = "2019-04-23"}
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K3 (B.encode r)
-- Refined:FalseP
-- False (>>) False | {2019-05-30 <= 2019-04-23}
-- |
-- +- P ReadP Day (2019-04-23) 2019-04-23 | 2019-04-23
-- |  |
-- |  `- P Id "2019-04-23"
-- |
-- `- False 2019-05-30 <= 2019-04-23
--    |
--    +- P Id 2019-04-23
--    |
--    +- P ReadP Day (2019-05-30) 2019-05-30 | 2019-05-30
--    |  |
--    |  `- P '2019-05-30
--    |
--    `- P ReadP Day (2019-06-01) 2019-06-01 | 2019-06-01
--       |
--       `- P '2019-06-01
-- <BLANKLINE>
--
instance (RefinedC p a, Binary a) => Binary (Refined p a) where
  get = do
          fld0 <- B.get @a
          let ((bp,(e,_top)),mr) = runIdentity $ newRefined @p o2 fld0
          case mr of
            Nothing -> fail $ "Refined:" ++ show bp ++ "\n" ++ e
            Just r -> return r
  put (Refined r) = B.put @a r

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
  case oDebug opts of
     OZero -> pure ()  -- putStrLn $ showBoolP opts (fst msg) <> " " <> topMessage tt
     OLite -> putStrLn $ showBoolP opts (fst msg) <> " " <> topMessage tt
     _ -> putStrLn $ snd msg
  pure $ case getValueLR opts "" tt [] of
    Right True -> Right (Refined a)
    _ -> Left (fst msg)

-- | returns a 'Refined' value if \'a\' is valid for the predicate \'p\'
newRefined :: forall p a m . (MonadEval m, RefinedC p a)
   => POpts
   -> a
   -> m ((BoolP, (String, String)), Maybe (Refined p a))
newRefined opts a = do
  tt <- evalBool (Proxy @p) opts a
  let rc = _tBool tt ^. boolT2P
      ss = case oDebug opts of
             OZero -> ("","")
             OLite -> ("",topMessage tt)
             _ -> (prtTreePure opts (fromTT tt),topMessage tt)
  pure $ ((rc,ss),) $ case getValueLR opts "" tt [] of
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

unRavelTIO :: RefinedT IO a -> IO (Either String a, [String])
unRavelTIO = runWriterT . runExceptT . unRefinedT

prtRefinedTImpl :: forall n m a . (MonadIO n, Show a) => (forall x . m x -> n x) -> RefinedT m a -> n ()
prtRefinedTImpl f rt = do
  (lr,ws) <-  f $ unRavelT rt
  liftIO $ do
    forM_ (zip [1::Int ..] ws) $ \(_,y) -> unless (null y) $ putStrLn y
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

-- | a way to unsafely create a 'Refined' value but run the predicate
unsafeRefined' :: forall p a . RefinedC p a => POpts -> a -> Refined p a
unsafeRefined' opts a =
  let tt = runIdentity $ evalBool (Proxy @p) opts a
  in case getValueLR opts "" tt [] of
       Right True -> Refined a
       _ -> error $ prtTreePure opts (fromTT tt)
