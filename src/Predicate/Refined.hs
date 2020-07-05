{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}
{- |
     Simple refinement type with only one type and a predicate
-}
module Predicate.Refined (
  -- ** Refined
    Refined
  , unRefined
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
  , rapplyLift

  -- ** unsafe create methods
  , unsafeRefined
  , unsafeRefined'

  , type ReplaceOptT
  , type AppendOptT

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
import Test.QuickCheck
import qualified GHC.Read as GR
import qualified Text.ParserCombinators.ReadPrec as PCR
import qualified Text.Read.Lex as RL
import qualified Data.Binary as B
import Data.Binary (Binary)
import Data.String
import Data.Maybe
import Data.Hashable (Hashable(..))
import GHC.Stack
import Data.Tree (Tree)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :m + Predicate.Prelude

-- | a simple refinement type that ensures the predicate \'p\' holds for the type \'a\'
--
-- >>> prtRefinedIO @'OZ @(Between 10 14 Id) 13
-- Right (Refined 13)
--
-- >>> prtRefinedIO @'OZ @(Between 10 14 Id) 99
-- Left FalseP
--
-- >>> prtRefinedIO @'OZ @(Last Id >> Len == 4) ["one","two","three","four"]
-- Right (Refined ["one","two","three","four"])
--
-- >>> prtRefinedIO @'OZ @(Re "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$" Id) "141.213.1.99"
-- Right (Refined "141.213.1.99")
--
-- >>> prtRefinedIO @'OZ @(Re "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$" Id) "141.213.1"
-- Left FalseP
--
-- >>> prtRefinedIO @'OZ @(Map (ReadP Int Id) (Resplit "\\." Id) >> Guard (PrintF "bad length: found %d" Len) (Len == 4) >> 'True) "141.213.1"
-- Left (FailP "bad length: found 3")
--
-- >>> prtRefinedIO @'OZ @(Map (ReadP Int Id) (Resplit "\\." Id) >> Guard (PrintF "bad length: found %d" Len) (Len == 4) >> GuardsN (PrintT "octet %d out of range %d" Id) 4 (Between 0 255 Id) >> 'True) "141.213.1.444"
-- Left (FailP "octet 3 out of range 444")
--
-- >>> prtRefinedIO @'OZ @(Map (ReadP Int Id) (Resplit "\\." Id) >> Guard (PrintF "bad length: found %d" Len) (Len == 4) >> GuardsN (PrintT "octet %d out of range %d" Id) 4 (Between 0 255 Id) >> 'True) "141.213.1x34.444"
-- Left (FailP "ReadP Int (1x34)")
--
-- >>> prtRefinedIO @'OZ @(Map ('[Id] >> ReadP Int Id) Id >> Luhn Id) "12344"
-- Right (Refined "12344")
--
-- >>> prtRefinedIO @'OZ @(Map ('[Id] >> ReadP Int Id) Id >> Luhn Id) "12340"
-- Left FalseP
--
-- >>> prtRefinedIO @'OZ @(Any (Prime Id) Id) [11,13,17,18]
-- Right (Refined [11,13,17,18])
--
-- >>> prtRefinedIO @'OZ @(All (Prime Id) Id) [11,13,17,18]
-- Left FalseP
--
-- >>> prtRefinedIO @'OZ @(Snd Id !! Fst Id >> Len > 5) (2,["abc","defghij","xyzxyazsfd"])
-- Right (Refined (2,["abc","defghij","xyzxyazsfd"]))
--
-- >>> prtRefinedIO @'OZ @(Snd Id !! Fst Id >> Len > 5) (27,["abc","defghij","xyzxyazsfd"])
-- Left (FailP "(!!) index not found")
--
-- >>> prtRefinedIO @'OZ @(Snd Id !! Fst Id >> Len <= 5) (2,["abc","defghij","xyzxyazsfd"])
-- Left FalseP
newtype Refined (opts :: OptT) p a = Refined a deriving (Show, Eq, Generic, TH.Lift)

unRefined :: forall k (opts :: OptT) (p :: k) a. Refined opts p a -> a
unRefined (Refined a) = a

type role Refined nominal nominal nominal

instance RefinedC opts p String => IsString (Refined opts p String) where
  fromString s =
    let ((bp,(e,_top)),mr) = runIdentity $ newRefined @opts @p s
    in fromMaybe (error $ "Refined(fromString):" ++ show bp ++ "\n" ++ e) mr

-- | 'Read' instance for 'Refined'
--
-- >>> reads @(Refined 'OZ (Between 0 255 Id) Int) "Refined 254"
-- [(Refined 254,"")]
--
-- >>> reads @(Refined 'OZ (Between 0 255 Id) Int) "Refined 300"
-- []
--
-- >>> reads @(Refined 'OZ 'True Int) "Refined (-123)xyz"
-- [(Refined (-123),"xyz")]
--

instance (RefinedC opts p a, Read a) => Read (Refined opts p a) where
  readPrec
    = GR.parens
        (PCR.prec
           11
           (do GR.expectP (RL.Ident "Refined")
               fld0 <- PCR.reset GR.readPrec
               let ((_bp,(_e,_top)),mr) = runIdentity $ newRefined @opts @p fld0 -- since we cant display the failure message
               case mr of
                 Nothing -> fail ""   -- (show _bp ++ "\n" ++ _e)
                 Just _r -> pure (Refined fld0)
           ))
  readList = GR.readListDefault
  readListPrec = GR.readListPrecDefault

-- | the constraints that 'Refined' must adhere to
type RefinedC opts p a = (OptTC opts, PP p a ~ Bool, P p a)

-- | 'ToJSON' instance for 'Refined'
instance ToJSON a => ToJSON (Refined opts p a) where
  toJSON = toJSON . unRefined

-- | 'FromJSON' instance for 'Refined'
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode' @(Refined 'OZ (Between 10 14 Id) Int) "13"
-- Right (Refined 13)
--
-- >>> removeAnsi $ A.eitherDecode' @(Refined 'OAN (Between 10 14 Id) Int) "16"
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
instance (RefinedC opts p a, FromJSON a) => FromJSON (Refined opts p a) where
  parseJSON z = do
                  a <- parseJSON z
                  let ((bp,(e,_top)),mr) = runIdentity $ newRefined @opts @p a
                  case mr of
                    Nothing -> fail $ "Refined:" ++ show bp ++ "\n" ++ e
                    Just r -> return r

-- | 'Binary' instance for 'Refined'
--
-- >>> import Data.Time
-- >>> import Control.Lens
-- >>> import Control.Arrow ((+++))
-- >>> type K1 = Refined 'OZ (ReadP Day Id >> 'True) String
-- >>> type K2 = Refined 'OAN (Between (ReadP Day "2019-05-30") (ReadP Day "2019-06-01") (ReadP Day Id)) String
-- >>> r = unsafeRefined' @'OZ "2019-04-23" :: K1
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K1 (B.encode r)
-- Refined "2019-04-23"
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K2 (B.encode r)
-- Refined:FalseP
-- False 2019-05-30 <= 2019-04-23
-- |
-- +- P ReadP Day 2019-04-23
-- |  |
-- |  `- P Id "2019-04-23"
-- |
-- +- P ReadP Day 2019-05-30
-- |  |
-- |  `- P '2019-05-30
-- |
-- `- P ReadP Day 2019-06-01
--    |
--    `- P '2019-06-01
-- <BLANKLINE>
--
instance (RefinedC opts p a, Binary a) => Binary (Refined opts p a) where
  get = do
          fld0 <- B.get @a
          let ((bp,(e,_top)),mr) = runIdentity $ newRefined @opts @p fld0
          case mr of
            Nothing -> fail $ "Refined:" ++ show bp ++ "\n" ++ e
            Just r -> return r
  put (Refined r) = B.put @a r

-- | 'Hashable' instance for 'Refined'
instance ( RefinedC opts p a
         , Hashable a
         ) => Hashable (Refined opts p a) where
  hashWithSalt s (Refined a) = s + hash a

-- | 'arbitrary' value for 'Refined'
arbRefined :: forall opts p a.
   ( Arbitrary a
   , RefinedC opts p a
   ) => Gen (Refined opts p a)
arbRefined = suchThatMap (arbitrary @a) (snd . runIdentity . newRefined @opts @p)

-- | binary operation applied to two 'RefinedT' values
rapply :: forall m opts p a . (RefinedC opts p a, Monad m)
  => (a -> a -> a)
  -> RefinedT m (Refined opts p a)
  -> RefinedT m (Refined opts p a)
  -> RefinedT m (Refined opts p a)
rapply f ma mb = do
  let opts = getOptT @opts
  tell [markBoundary opts "=== a ==="]
  Refined x <- ma
  tell [markBoundary opts "=== b ==="]
  Refined y <- mb
  tell [markBoundary opts "=== a `op` b ==="]
  newRefinedT @m @opts @p (f x y)

-- | takes two values and lifts them into 'RefinedT' and then applies the binary operation
rapply0 :: forall opts p a m . (RefinedC opts p a, Monad m)
  => (a -> a -> a)
  -> a
  -> a
  -> RefinedT m (Refined opts p a)
rapply0 f a b = rapply f (newRefinedT a) (newRefinedT b)

-- | same as 'rapply' except we already have valid 'Refined' values as input
rapplyLift :: forall m opts p a . (RefinedC opts p a, Monad m)
  => (a -> a -> a)
  -> Refined opts p a
  -> Refined opts p a
  -> RefinedT m (Refined opts p a)
rapplyLift f (Refined a) (Refined b) = newRefinedT (f a b)

-- | attempts to lift a refinement type to another refinement type by way of transformation function
--   you can control both the predicate and the type
convertRefinedT :: forall m opts p a p1 a1
  . ( RefinedC opts p1 a1
    , Monad m)
  => (a -> a1)
  -> RefinedT m (Refined opts p a)
  -> RefinedT m (Refined opts p1 a1)
convertRefinedT f ma = do
  Refined a <- ma -- you already got a refined in there so no need to check RefinedC
  newRefinedT @m @opts @p1 (f a)

-- | invokes the callback with the 'Refined' value if \'a\' is valid for the predicate \'p\'
withRefinedT :: forall opts p m a b
     . ( Monad m
       , RefinedC opts p a
       )
  => a
  -> (Refined opts p a -> RefinedT m b)
  -> RefinedT m b
withRefinedT a k = newRefinedT @m @opts @p a >>= k

withRefinedTIO :: forall opts p m a b
     . ( MonadIO m
       , RefinedC opts p a
       )
  => a
  -> (Refined opts p a -> RefinedT m b)
  -> RefinedT m b
withRefinedTIO a k = newRefinedTIO @opts @p a >>= k

-- | same as 'newRefined' but prints the results
prtRefinedIO :: forall opts p a
   . ( RefinedC opts p a
     )
   => a
   -> IO (Either BoolP (Refined opts p a))
prtRefinedIO a = do
  let o = getOptT @opts
  tt <- evalBool (Proxy @p) o a
  let msg = (_tBool tt ^. boolT2P, prtImpl o (fromTT tt))
  case oDebug o of
     DZero -> pure ()  -- putStrLn $ showBoolP opts (fst msg) <> " " <> topMessage tt
     DLite -> putStrLn $ showBoolP o (fst msg) <> " " <> topMessage tt
     _ -> putStrLn $ snd msg
  pure $ case getValueLR o "" tt [] of
    Right True -> Right (Refined a)
    _ -> Left (fst msg)

-- | returns a 'Refined' value if \'a\' is valid for the predicate \'p\'
newRefined :: forall opts p a m
   . ( MonadEval m
     , RefinedC opts p a
     )
   => a
   -> m ((BoolP, (String, String)), Maybe (Refined opts p a))
newRefined a = do
  let o = getOptT @opts
  tt <- evalBool (Proxy @p) o a
  let rc = _tBool tt ^. boolT2P
      ss = case oDebug o of
             DZero -> ("","")
             DLite -> ("",topMessage tt)
             _ -> (prtImpl o (fromTT tt),topMessage tt)
  pure $ ((rc,ss),) $ case getValueLR o "" tt [] of
       Right True -> Just (Refined a)
       _ -> Nothing

newRefinedTImpl :: forall opts p a n m
  . ( RefinedC opts p a
    , Monad m
    , MonadEval n
    )
  => (forall x . n x -> RefinedT m x)
  -> a
  -> RefinedT m (Refined opts p a)
newRefinedTImpl f a = do
  let o = getOptT @opts
  tt <- f $ evalBool (Proxy @p) o a
  let msg = prtImpl o (fromTT tt)
  tell [msg]
  case getValueLR o "" tt [] of
    Right True -> return (Refined a) -- FalseP is also a failure!
    _ -> let rc = show (_tBool tt ^. boolT2P)
         in throwError rc -- RefinedT $ ExceptT $ WriterT $ return (Left rc, [])

-- | returns a wrapper 'RefinedT' around a possible 'Refined' value if \'a\' is valid for the predicate \'p\'
newRefinedT :: forall m opts p a
  . ( RefinedC opts p a
    , Monad m)
  => a
  -> RefinedT m (Refined opts p a)
newRefinedT = newRefinedTImpl (return . runIdentity)

newRefinedTIO :: forall opts p a m
  . ( RefinedC opts p a
    , MonadIO m)
  => a
  -> RefinedT m (Refined opts p a)
newRefinedTIO = newRefinedTImpl liftIO

newtype RefinedT m a = RefinedT { unRefinedT :: ExceptT String (WriterT [String] m) a }
  deriving (Functor, Applicative, Monad, MonadCont, MonadWriter [String], Show, MonadIO)

instance MonadTrans RefinedT where
  lift ma = RefinedT $ ExceptT $ WriterT $ do
              a <- ma
              return (Right a, [])

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
unsafeRefined :: forall opts p a . a -> Refined opts p a
unsafeRefined = Refined

-- | a way to unsafely create a 'Refined' value but run the predicate
unsafeRefined' :: forall opts p a
  . ( RefinedC opts p a
    , HasCallStack
    ) => a -> Refined opts p a
unsafeRefined' a =
  let o = getOptT @opts
      tt = runIdentity $ evalBool (Proxy @p) o a
  in case getValueLR o "" tt [] of
       Right True -> Refined a
       _ -> error $ prtImpl o (fromTT tt)

prtImpl :: POpts -> Tree PE -> String
prtImpl o tt =
  formatOMessage o "\n" <> prtTreePure o tt

type family ReplaceOptT (o :: OptT) t where
  ReplaceOptT o (Refined _ p a) = Refined o p a

type family AppendOptT (o :: OptT) t where
  AppendOptT o (Refined o' p a) = Refined (o' ':# o) p a
