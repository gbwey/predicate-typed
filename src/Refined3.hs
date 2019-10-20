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
{- |
Module      : Refined3
Description : Refinement type allowing the external type to differ from the internal type
Copyright   : (c) Grant Weyburne, 2019
License     : BSD-3
Maintainer  : gbwey9@gmail.com

see 'Refined3'
contains Json and Read instances and arbitrary functions
-}
module Refined3 (
    Refined3(r3In,r3Out)
  , Refined3C
  , mkProxy3
  , mkProxy3P
  , MkProxy3T
  , withRefined3TIO
  , withRefined3T
  , withRefined3TP
  , newRefined3T
  , newRefined3TP
  , newRefined3TPIO
  , convertRefined3T
  , convertRefined3TP
  , rapply3
  , rapply3P
  , prtEval3P
  , prtEval3PIO
  , prtEval3
  , eval3P
  , eval3
  , eval3M
  , eval3PX
  , eval3X
  , prt3IO
  , prt3
  , arbRefined3
  , arbRefined3With
  , Msg3 (..)
  , prt3Impl
  , MakeR3
  , Results (..)
  , RResults (..)
  , unsafeRefined3
  , unsafeRefined3'
 ) where
import Refined
import Predicate
import UtilP
import Control.Lens hiding (strict,iall)
import Data.Tree
import Data.Proxy
import Control.Monad.Except
import Control.Monad.Writer (tell)
import Data.Aeson
import qualified Language.Haskell.TH.Syntax as TH
import System.Console.Pretty
import Test.QuickCheck
import qualified GHC.Read as GR
import qualified Text.ParserCombinators.ReadPrec as PCR
import qualified Text.Read.Lex as RL
import qualified Data.Binary as B
import Data.Binary (Binary)
-- | Refinement type that differentiates the input type from output type
--
-- @
-- \'i\' is the input type
-- \'ip\' converts i to PP ip i which is the internal type
-- \'op\' validates that internal type using PP op (PP ip i) ~ Bool
-- \'fmt\' outputs the internal type PP fmt (PP ip i) ~ i
-- PP fmt (PP ip i) should be valid input to Refined3
-- @
--
-- If we fix the input type to String then it looks similar to:
-- 1. read into an internal type
-- 2. validate internal type with a predicate function
-- 3. show/format the  internal type
--
-- Although the most common scenario is String as input, you are free to choose any input type you like
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> prtEval3 @(ReadBase Int 16) @(Lt 255) @(Printf "%x" Id) ol "00fe"
-- Right (Refined3 {r3In = 254, r3Out = "fe"})
--
-- >>> prtEval3 @(ReadBase Int 16) @(Lt 253) @(Printf "%x" Id) ol "00fe"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> prtEval3 @(ReadBase Int 16) @(Lt 255) @(Printf "%x" Id) ol "00fg"
-- Left Step 1. Initial Conversion(ip) Failed | invalid base 16
--
-- >>> prtEval3 @(Map (ReadP Int) (Resplit "\\.")) @(Guard (Printf "found length=%d" Len) (Len >> Id == 4) >> 'True) @(Printfnt 4 "%03d.%03d.%03d.%03d") ol "198.162.3.1.5"
-- Left Step 2. Failed Boolean Check(op) | found length=5
--
-- >>> prtEval3 @(Map (ReadP Int) (Resplit "\\.")) @(Guard (Printf "found length=%d" Len) (Len >> Id == 4) >> 'True) @(Printfnt 4 "%03d.%03d.%03d.%03d") ol "198.162.3.1"
-- Right (Refined3 {r3In = [198,162,3,1], r3Out = "198.162.003.001"})
--
-- >>> prtEval3 @(MkDay Fst (Snd >> Fst) (Snd >> Snd) >> 'Just Id) @(Guard "expected a Sunday" (Snd >> Snd == 7) >> 'True) @(Fst >> UnMkDay) ol (2019,(10,13))
-- Right (Refined3 {r3In = (2019-10-13,(41,7)), r3Out = (2019,(10,13))})
--
-- >>> prtEval3 @(MkDay Fst (Snd >> Fst) (Snd >> Snd) >> 'Just Id) @(Guard "expected a Sunday" (Snd >> Snd == 7) >> 'True) @(Fst >> UnMkDay) ol (2019,(10,12))
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
-- >>> type T4 k = '(MkDay Fst (Snd >> Fst) (Snd >> Snd) >> 'Just Id, Guard "expected a Sunday" (Snd >> Snd == 7) >> 'True, Fst >> UnMkDay, k)
-- >>> prtEval3P (Proxy @(T4 _)) ol (2019,(10,12))
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
-- >>> prtEval3P (Proxy @(T4 _)) ol (2019,(10,13))
-- Right (Refined3 {r3In = (2019-10-13,(41,7)), r3Out = (2019,(10,13))})
--
data Refined3 ip op fmt i = Refined3 { r3In :: PP ip i, r3Out :: PP fmt (PP ip i) }

-- | directly load values into 'Refined3'. It still checks to see that those values are valid
unsafeRefined3' :: forall ip op fmt i
                . (Show i, Show (PP ip i), Refined3C ip op fmt i)
                => POpts
                -> i
                -> Refined3 ip op fmt i
unsafeRefined3' opts i =
  let (ret,mr) = eval3 @ip @op @fmt opts i
  in case mr of
  Nothing -> error $ show (prt3Impl opts ret)
  Just r -> r
-- | directly load values into 'Refined3' without any checking
unsafeRefined3 :: forall ip op fmt i . PP ip i -> PP fmt (PP ip i) -> Refined3 ip op fmt i
unsafeRefined3 = Refined3


-- | Provides the constraints on Refined3
type Refined3C ip op fmt i =
       ( P ip i
       , P op (PP ip i)
       , PP op (PP ip i) ~ Bool   -- the internal value needs to pass the predicate check
       , P fmt (PP ip i)
       , PP fmt (PP ip i) ~ i  -- the output type must match the original input type
       )

deriving instance (Show i, Show (PP ip i), Show (PP fmt (PP ip i))) => Show (Refined3 ip op fmt i)
deriving instance (Eq i, Eq (PP ip i), Eq (PP fmt (PP ip i))) => Eq (Refined3 ip op fmt i)
deriving instance (TH.Lift (PP ip i), TH.Lift (PP fmt (PP ip i))) => TH.Lift (Refined3 ip op fmt i)

-- read instance from -ddump-deriv
-- | 'Read' instance for 'Refined3'
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedStrings
-- >>> reads @(Refined3 (ReadBase Int 16) (Between 0 255) (ShowBase 16) String) "Refined3 {r3In = 254, r3Out = \"fe\"}"
-- [(Refined3 {r3In = 254, r3Out = "fe"},"")]
--
-- >>> reads @(Refined3 (ReadBase Int 16) (Between 0 255) (ShowBase 16) String) "Refined3 {r3In = 300, r3Out = \"12c\"}"
-- []
--
-- >>> reads @(Refined3 (ReadBase Int 16) (Id < 0) (ShowBase 16) String) "Refined3 {r3In = -1234, r3Out = \"-4d2\"}"
-- [(Refined3 {r3In = -1234, r3Out = "-4d2"},"")]
--
-- >>> reads @(Refined3 (Map (ReadP Int) (Resplit "\\.")) (Guard "len/=4" (Len == 4) >> 'True) (Printfnt 4 "%d.%d.%d.%d") String) "Refined3 {r3In = [192,168,0,1], r3Out = \"192.168.0.1\"}"
-- [(Refined3 {r3In = [192,168,0,1], r3Out = "192.168.0.1"},"")]
--
instance (Eq i, Show i, Show (PP ip i), Refined3C ip op fmt i, Read (PP ip i), Read (PP fmt (PP ip i))) => Read (Refined3 ip op fmt i) where
    readPrec
      = GR.parens
          (PCR.prec
             11
             (do GR.expectP (RL.Ident "Refined3")
                 GR.expectP (RL.Punc "{")
                 fld1 <- GR.readField
                               "r3In" (PCR.reset GR.readPrec)
                 GR.expectP (RL.Punc ",")
                 fld2 <- GR.readField
                               "r3Out" (PCR.reset GR.readPrec)
                 GR.expectP (RL.Punc "}")

                 let (_ret,mr) = runIdentity $ eval3MSkip @_ @ip @op @fmt o2 fld1
                 case mr of
                   Nothing -> fail "" --   show (prt3Impl o2 _ret)
                   Just (Refined3 _r1 r2) | r2 == fld2 -> pure (Refined3 fld1 fld2)
                                         | otherwise -> fail "" -- "mismatch on r3Out fmt: found (" ++ show fld2 ++ ") but expected(" ++ show r2 ++ ")"
             ))
    readList = GR.readListDefault
    readListPrec = GR.readListPrecDefault

-- | 'ToJSON' instance for 'Refined3'
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedStrings
-- >>> encode (unsafeRefined3 @(ReadBase Int 16) @(Between 0 255) @(ShowBase 16) 254 "fe")
-- "\"fe\""
--
-- >>> encode (unsafeRefined3 @Id @'True @Id 123 123)
-- "123"
--
instance ToJSON (PP fmt (PP ip i)) => ToJSON (Refined3 ip op fmt i) where
  toJSON = toJSON . r3Out


-- | 'FromJSON' instance for 'Refined3'
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedStrings
-- >>> eitherDecode' @(Refined3 (ReadBase Int 16) (Id > 10 && Id < 256) (ShowBase 16) String) "\"00fe\""
-- Right (Refined3 {r3In = 254, r3Out = "fe"})
--
-- >>> removeAnsiForDocTest $ eitherDecode' @(Refined3 (ReadBase Int 16) (Id > 10 && Id < 256) (ShowBase 16) String) "\"00fe443a\""
-- Error in $: Refined3:Step 2. False Boolean Check(op) | FalseP
-- <BLANKLINE>
-- *** Step 1. Success Initial Conversion(ip) [16663610] ***
-- <BLANKLINE>
-- P ReadBase(Int,16) 16663610 | "00fe443a"
-- |
-- `- P Id "00fe443a"
-- <BLANKLINE>
-- *** Step 2. False Boolean Check(op) ***
-- <BLANKLINE>
-- False True && False
-- |
-- +- True  16663610 > 10
-- |  |
-- |  +- P Id 16663610
-- |  |
-- |  `- P '10
-- |
-- `- False 16663610 < 256
--    |
--    +- P Id 16663610
--    |
--    `- P '256
-- <BLANKLINE>
--
instance (Show (PP fmt (PP ip i)), Show (PP ip i), Refined3C ip op fmt i, FromJSON i) => FromJSON (Refined3 ip op fmt i) where
  parseJSON z = do
                  i <- parseJSON @i z
                  let (ret,mr) = eval3 @ip @op @fmt o2 i
                  case mr of
                    Nothing -> fail $ "Refined3:" ++ show (prt3Impl o2 ret)
                    Just r -> return r

{-
instance (Arbitrary (PP ip i)
        , Show (PP ip i)
        , Show i
        , Refined3C ip op fmt i
        ) => Arbitrary (Refined3 ip op fmt i) where
  arbitrary = suchThatMap (arbitrary @(PP ip i)) $ eval3MQuickIdentity @ip @op @fmt o2
-}
arbRefined3 :: forall ip op fmt i .
   ( Arbitrary (PP ip i)
   , Refined3C ip op fmt i
   ) => Proxy '(ip,op,fmt,i) -> POpts -> Gen (Refined3 ip op fmt i)
arbRefined3 _ = suchThatMap (arbitrary @(PP ip i)) . eval3MQuickIdentity @ip @op @fmt

-- help things along a little
arbRefined3With ::
    forall ip op fmt i
  . (Arbitrary (PP ip i)
   , Refined3C ip op fmt i)
  => Proxy '(ip,op,fmt,i)
  -> POpts
  -> (PP ip i -> PP ip i)
  -> Gen (Refined3 ip op fmt i)
arbRefined3With _ opts f =
  suchThatMap (f <$> arbitrary @(PP ip i)) $ eval3MQuickIdentity @ip @op @fmt opts

-- | 'Binary' instance for 'Refined3'
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> import Control.Arrow ((+++))
-- >>> import Data.Time
-- >>> type K1 = MakeR3 '(ReadP Day, 'True, ShowP, String)
-- >>> type K2 = MakeR3 '(ReadP Day, Between (ReadP' Day "2019-03-30") (ReadP' Day "2019-06-01"), ShowP, String)
-- >>> type K3 = MakeR3 '(ReadP Day, Between (ReadP' Day "2019-05-30") (ReadP' Day "2019-06-01"), ShowP, String)
-- >>> r = unsafeRefined3' ol "2019-04-23" :: K1
-- >>> removeAnsiForDocTest $ (view _3 +++ view _3) $ B.decodeOrFail @K1 (B.encode r)
-- Refined3 {r3In = 2019-04-23, r3Out = "2019-04-23"}
--
-- >>> removeAnsiForDocTest $ (view _3 +++ view _3) $ B.decodeOrFail @K2 (B.encode r)
-- Refined3 {r3In = 2019-04-23, r3Out = "2019-04-23"}
--
-- >>> removeAnsiForDocTest $ (view _3 +++ view _3) $ B.decodeOrFail @K3 (B.encode r)
-- Refined3:Step 2. False Boolean Check(op) | FalseP
-- <BLANKLINE>
-- *** Step 1. Success Initial Conversion(ip) [2019-04-23] ***
-- <BLANKLINE>
-- P ReadP Day (2019-04-23) 2019-04-23 | 2019-04-23
-- |
-- `- P Id "2019-04-23"
-- <BLANKLINE>
-- *** Step 2. False Boolean Check(op) ***
-- <BLANKLINE>
-- False False && True
-- |
-- +- False 2019-04-23 >= 2019-05-30
-- |  |
-- |  +- P I
-- |  |
-- |  `- P ReadP Day (2019-05-30) 2019-05-30 | 2019-05-30
-- |     |
-- |     `- P '2019-05-30
-- |
-- `- True  2019-04-23 <= 2019-06-01
--    |
--    +- P I
--    |
--    `- P ReadP Day (2019-06-01) 2019-06-01 | 2019-06-01
--       |
--       `- P '2019-06-01
-- <BLANKLINE>
--
instance (Show (PP fmt (PP ip i)), Show (PP ip i), Refined3C ip op fmt i, Binary i) => Binary (Refined3 ip op fmt i) where
  get = do
          i <- B.get @i
          let (ret,mr) = eval3 @ip @op @fmt o2 i
          case mr of
            Nothing -> fail $ "Refined3:" ++ show (prt3Impl o2 ret)
            Just r -> return r
  put (Refined3 _ r) = B.put @i r

-- | wraps the parameters for 'Refined3' in a 4-tuple for use with methods such as 'withRefined3TP' and 'newRefined3TP'
mkProxy3 :: forall ip op fmt i . Refined3C ip op fmt i => Proxy '(ip,op,fmt,i)
mkProxy3 = Proxy

-- | use type application to set the parameters then it will be wrapped into a 4-tuple
--   checks to make sure the proxy is consistent with Refined3C
-- use for passing into eval3P you can pass in a promoted 4 tuple to other methods
mkProxy3P :: forall z ip op fmt i . (z ~ '(ip,op,fmt,i), Refined3C ip op fmt i) => Proxy '(ip,op,fmt,i)
mkProxy3P = Proxy

-- | convenience type family for converting from a 4-tuple '(ip,op,fmt,i) to a 'Refined3' signature
type family MakeR3 p where
  MakeR3 '(ip,op,fmt,i) = Refined3 ip op fmt i

-- | convenience type family for converting from a 4-tuple '(ip,op,fmt,i) to a Proxy
type family MkProxy3T p where
  MkProxy3T '(ip,op,fmt,i) = Proxy '(ip,op,fmt,i)

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

prtEval3PIO :: forall ip op fmt i proxy
  . ( Refined3C ip op fmt i
    , Show (PP ip i)
    , Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> IO (Either String (Refined3 ip op fmt i))
prtEval3PIO _ opts i = do
  x <- eval3M opts i
  prt3IO opts x

prtEval3 :: forall ip op fmt i
  . ( Refined3C ip op fmt i
    , Show (PP ip i)
    , Show i)
  => POpts
  -> i
  -> Either Msg3 (Refined3 ip op fmt i)
prtEval3 opts i =
  let x = eval3 opts i
  in prt3 opts x

prtEval3P :: forall ip op fmt i proxy
  . ( Refined3C ip op fmt i
    , Show (PP ip i)
    , Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> Either Msg3 (Refined3 ip op fmt i)
prtEval3P _ opts i =
  let x = eval3 opts i
  in prt3 opts x


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

prt3IO :: (Show a, Show b) => POpts -> (RResults a b, Maybe r) -> IO (Either String r)
prt3IO opts (ret,mr) = do
  let m3 = prt3Impl opts ret
  unless (oLite opts) $ putStrLn $ m3Long m3
  return $ maybe (Left (m3Desc m3 <> " | " <> m3Short m3)) Right mr

prt3 :: (Show a, Show b) => POpts -> (RResults a b, Maybe r) -> Either Msg3 r
prt3 opts (ret,mr) = maybe (Left $ prt3Impl opts ret) Right mr

data Msg3 = Msg3 { m3Desc :: String, m3Short :: String, m3Long :: String } deriving Eq
instance Show Msg3 where
  show (Msg3 a b c) = a <> " | " <> b <> (if null c then "" else "\n" <> c)

prt3Impl :: (Show a, Show b)
  => POpts
  -> RResults a b
  -> Msg3
prt3Impl opts v =
  let outmsg msg = "\n*** " <> msg <> " ***\n\n"
      msg1 a = outmsg ("Step 1. Success Initial Conversion(ip) [" ++ show a ++ "]")
      mkMsg3 m n r | oLite opts = Msg3 m n ""
                   | otherwise = Msg3 m n r
  in case v of
       RF e t1 ->
         let (m,n) = ("Step 1. Initial Conversion(ip) Failed", e)
             r = outmsg m
              <> prtTreePure opts t1
         in mkMsg3 m n r
       RTF a t1 e t2 ->
         let (m,n) = ("Step 2. Failed Boolean Check(op)", e)
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg m
              <> prtTreePure opts t2
         in mkMsg3 m n r
       RTFalse a t1 t2 ->
         let (m,n) = ("Step 2. False Boolean Check(op)", "FalseP")
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg m
              <> prtTreePure opts t2
         in mkMsg3 m n r
       RTTrueF a t1 t2 e t3 ->
         let (m,n) = ("Step 3. Failed Output Conversion(fmt)", e)
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg "Step 2. Success Boolean Check(op)"
              <> prtTreePure opts t2
              <> outmsg m
              <> prtTreePure opts t3
         in mkMsg3 m n r
       RTTrueT a t1 t2 b t3 ->
         let (m,n) = ("Step 3. Success Output Conversion(fmt)", show b)
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg "Step 2. Success Boolean Check(op)"
              <> prtTreePure opts t2
              <> outmsg m
              <> fixLite opts b t3
         in mkMsg3 m n r


-- | emulates 'Refined3' but uses 'Refined'
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
