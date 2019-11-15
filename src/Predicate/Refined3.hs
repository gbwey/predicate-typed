{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wno-redundant-constraints #-}
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE RoleAnnotations #-}
{- |
     Refinement type allowing the external type to differ from the internal type
     see 'Refined3'
-}
module Predicate.Refined3 (
  -- ** Refined3
    Refined3(r3In,r3Out)
  , Refined3C

 -- ** display results
  , prtEval3P
  , prtEval3PIO
  , prtEval3
  , prt3IO
  , prt3
  , prt3Impl
  , Msg3 (..)
  , Results (..)
  , RResults (..)

  -- ** proxy methods
  , mkProxy3
  , mkProxy3'
  , MakeR3

  -- ** evaluation methods
  , eval3P
  , eval3

  -- ** create a wrapped Refined3 value
  , withRefined3TIO
  , withRefined3T
  , withRefined3TP
  , newRefined3T
  , newRefined3TP
  , newRefined3TPIO
  , convertRefined3TP
  , rapply3
  , rapply3P

  -- ** QuickCheck methods
  , arbRefined3
  , arbRefined3With

  -- ** unsafe methods for creating Refined3
  , unsafeRefined3
  , unsafeRefined3'

  -- ** emulate Refined3 using Refined
  , RefinedEmulate
  , eval3PX
  , eval3X

 ) where
import Predicate.Refined
import Predicate.Core
import Predicate.Util
import Data.Functor.Identity (Identity(..))
import Data.Tree
import Data.Proxy
import Control.Monad.Except
import Control.Monad.Writer (tell)
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Language.Haskell.TH.Syntax as TH
import System.Console.Pretty
import Test.QuickCheck
import qualified GHC.Read as GR
import qualified Text.ParserCombinators.ReadPrec as PCR
import qualified Text.Read.Lex as RL
import qualified Data.Binary as B
import Data.Binary (Binary)
import Data.Maybe (fromMaybe)
import Control.Lens ((^?),ix)
import Data.Tree.Lens (root)
import Data.Char (isSpace)
import Data.Semigroup ((<>))
import Data.String

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :m + Predicate.Prelude

-- | Refinement type that differentiates the input from output
--
--   * __i__ is the input type
--   * __ip__ converts @i@ to @PP ip i@ which is the internal type
--   * __op__ validates that internal type using @PP op (PP ip i) ~ Bool@
--   * __fmt__ outputs the internal type @PP fmt (PP ip i) ~ i@
--   * __PP fmt (PP ip i)__ should be valid as input for Refined3
--
-- Setting __ip__ to @Id@ and __fmt__ to @Id@ makes it equivalent to 'Refined.Refined': see 'RefinedEmulate'
--
-- Setting the input type __i__ to 'GHC.Base.String' resembles the corresponding Read/Show instances but with an additional predicate on the read value
--
--   * __read__ a string using /ip/ into an internal type and store in 'r3In'
--   * __validate__ 'r3In' using the predicate /op/
--   * __show__ 'r3In' using /fmt/ and store that formatted result in 'r3Out'
--
-- Although a common scenario is String as input, you are free to choose any input type you like
--
-- >>> prtEval3 @(ReadBase Int 16 Id) @(Lt 255) @(PrintF "%x" Id) oz "00fe"
-- Right (Refined3 {r3In = 254, r3Out = "fe"})
--
-- >>> prtEval3 @(ReadBase Int 16 Id) @(Lt 253) @(PrintF "%x" Id) oz "00fe"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> prtEval3 @(ReadBase Int 16 Id) @(Lt 255) @(PrintF "%x" Id) oz "00fg"
-- Left Step 1. Initial Conversion(ip) Failed | invalid base 16
--
-- >>> prtEval3 @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Msg "length invalid:" (Len == 4)) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) ol "198.162.3.1.5"
-- Left Step 2. False Boolean Check(op) | {length invalid:5 == 4}
--
-- >>> prtEval3 @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Guard (PrintF "found length=%d" Len) (Len == 4) >> 'True) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) oz "198.162.3.1.5"
-- Left Step 2. Failed Boolean Check(op) | found length=5
--
-- >>> prtEval3 @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Guard (PrintF "found length=%d" Len) (Len == 4) >> 'True) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) oz "198.162.3.1"
-- Right (Refined3 {r3In = [198,162,3,1], r3Out = "198.162.003.001"})
--
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> prtEval3 @(MkDay >> 'Just Id) @(Guard "expected a Sunday" (Thd Id == 7) >> 'True) @(UnMkDay (Fst Id)) oz (2019,10,13)
-- Right (Refined3 {r3In = (2019-10-13,41,7), r3Out = (2019,10,13)})
--
-- >>> prtEval3 @(MkDay >> 'Just Id) @(Msg "expected a Sunday:" (Thd Id == 7)) @(UnMkDay (Fst Id)) ol (2019,10,12)
-- Left Step 2. False Boolean Check(op) | {expected a Sunday:6 == 7}
--
-- >>> prtEval3 @(MkDay' (Fst Id) (Snd Id) (Thd Id) >> 'Just Id) @(Guard "expected a Sunday" (Thd Id == 7) >> 'True) @(UnMkDay (Fst Id)) oz (2019,10,12)
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
-- >>> type T4 k = '(MkDay >> 'Just Id, Guard "expected a Sunday" (Thd Id == 7) >> 'True, UnMkDay (Fst Id), k)
-- >>> prtEval3P (Proxy @(T4 _)) oz (2019,10,12)
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
-- >>> prtEval3P (Proxy @(T4 _)) oz (2019,10,13)
-- Right (Refined3 {r3In = (2019-10-13,41,7), r3Out = (2019,10,13)})
--
data Refined3 ip op fmt i = Refined3 { r3In :: PP ip i, r3Out :: PP fmt (PP ip i) }

type role Refined3 nominal nominal nominal nominal

-- | directly load values into 'Refined3'. It still checks to see that those values are valid
unsafeRefined3' :: forall ip op fmt i
                . (Show i, Show (PP ip i), Refined3C ip op fmt i)
                => POpts
                -> i
                -> Refined3 ip op fmt i
unsafeRefined3' opts i =
  let (ret,mr) = eval3 @ip @op @fmt opts i
  in fromMaybe (error $ show (prt3Impl opts ret)) mr

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

instance (Refined3C ip op fmt String, Show (PP ip String)) => IsString (Refined3 ip op fmt String) where
  fromString s =
    let (ret,mr) = eval3 @ip @op @fmt o2 s
    in case mr of
         Nothing -> error $ "Refined3(fromString):" ++ show (prt3Impl o2 ret)
         Just r -> r

-- read instance from -ddump-deriv
-- | 'Read' instance for 'Refined3'
--
-- >>> reads @(Refined3 (ReadBase Int 16 Id) (Between 0 255) (ShowBase 16 Id) String) "Refined3 {r3In = 254, r3Out = \"fe\"}"
-- [(Refined3 {r3In = 254, r3Out = "fe"},"")]
--
-- >>> reads @(Refined3 (ReadBase Int 16 Id) (Between 0 255) (ShowBase 16 Id) String) "Refined3 {r3In = 300, r3Out = \"12c\"}"
-- []
--
-- >>> reads @(Refined3 (ReadBase Int 16 Id) (Id < 0) (ShowBase 16 Id) String) "Refined3 {r3In = -1234, r3Out = \"-4d2\"}"
-- [(Refined3 {r3In = -1234, r3Out = "-4d2"},"")]
--
-- >>> reads @(Refined3 (Map (ReadP Int Id) (Resplit "\\." Id)) (Guard "len/=4" (Len == 4) >> 'True) (PrintL 4 "%d.%d.%d.%d" Id) String) "Refined3 {r3In = [192,168,0,1], r3Out = \"192.168.0.1\"}"
-- [(Refined3 {r3In = [192,168,0,1], r3Out = "192.168.0.1"},"")]
--
instance ( Eq i
         , Show i
         , Show (PP ip i)
         , Refined3C ip op fmt i
         , Read (PP ip i)
         , Read (PP fmt (PP ip i))
         ) => Read (Refined3 ip op fmt i) where
    readPrec
      = GR.parens
          (PCR.prec
             11
             (do GR.expectP (RL.Ident "Refined3")
                 GR.expectP (RL.Punc "{")
                 fld1 <- readField
                               "r3In" (PCR.reset GR.readPrec)
                 GR.expectP (RL.Punc ",")
                 fld2 <- readField
                               "r3Out" (PCR.reset GR.readPrec)
                 GR.expectP (RL.Punc "}")

                 let (_ret,mr) = runIdentity $ eval3MSkip @_ @ip @op @fmt oz fld1
                 case mr of
                   Nothing -> fail ""
                   Just (Refined3 _r1 r2)
                     | r2 == fld2 -> pure (Refined3 fld1 fld2)
                     | otherwise -> fail "" -- cant display a decent failure message
             ))
    readList = GR.readListDefault
    readListPrec = GR.readListPrecDefault

-- | 'ToJSON' instance for 'Refined3'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.encode (unsafeRefined3 @(ReadBase Int 16 Id) @(Between 0 255) @(ShowBase 16 Id) 254 "fe")
-- "\"fe\""
--
-- >>> A.encode (unsafeRefined3 @Id @'True @Id 123 123)
-- "123"
--
instance ToJSON (PP fmt (PP ip i)) => ToJSON (Refined3 ip op fmt i) where
  toJSON = toJSON . r3Out


-- | 'FromJSON' instance for 'Refined3'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode' @(Refined3 (ReadBase Int 16 Id) (Id > 10 && Id < 256) (ShowBase 16 Id) String) "\"00fe\""
-- Right (Refined3 {r3In = 254, r3Out = "fe"})
--
-- >>> removeAnsi $ A.eitherDecode' @(Refined3 (ReadBase Int 16 Id) (Id > 10 && Id < 256) (ShowBase 16 Id) String) "\"00fe443a\""
-- Error in $: Refined3:Step 2. False Boolean Check(op) | {True && False | (16663610 < 256)}
-- <BLANKLINE>
-- *** Step 1. Success Initial Conversion(ip) [16663610] ***
-- <BLANKLINE>
-- P ReadBase(Int,16) 16663610 | "00fe443a"
-- |
-- `- P Id "00fe443a"
-- <BLANKLINE>
-- *** Step 2. False Boolean Check(op) ***
-- <BLANKLINE>
-- False True && False | (16663610 < 256)
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
instance (Show ( PP fmt (PP ip i))
        , Show (PP ip i)
        , Refined3C ip op fmt i
        , FromJSON i
        ) => FromJSON (Refined3 ip op fmt i) where
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
  arbitrary = suchThatMap (arbitrary @(PP ip i)) $ eval3MQuickIdentity @ip @op @fmt
-}
arbRefined3 :: forall ip op fmt i .
   ( Arbitrary (PP ip i)
   , Refined3C ip op fmt i
   ) => Proxy '(ip,op,fmt,i)
     -> Gen (Refined3 ip op fmt i)
arbRefined3 _ = suchThatMap (arbitrary @(PP ip i)) $ eval3MQuickIdentity @ip @op @fmt

-- help things along a little
arbRefined3With ::
    forall ip op fmt i
  . (Arbitrary (PP ip i)
   , Refined3C ip op fmt i)
  => Proxy '(ip,op,fmt,i)
  -> (PP ip i -> PP ip i)
  -> Gen (Refined3 ip op fmt i)
arbRefined3With _ f =
  suchThatMap (f <$> arbitrary @(PP ip i)) $ eval3MQuickIdentity @ip @op @fmt

-- | 'Binary' instance for 'Refined3'
--
-- >>> import Control.Arrow ((+++))
-- >>> import Control.Lens
-- >>> import Data.Time
-- >>> type K1 = MakeR3 '(ReadP Day Id, 'True, ShowP Id, String)
-- >>> type K2 = MakeR3 '(ReadP Day Id, Between (ReadP Day "2019-03-30") (ReadP Day "2019-06-01"), ShowP Id, String)
-- >>> type K3 = MakeR3 '(ReadP Day Id, Between (ReadP Day "2019-05-30") (ReadP Day "2019-06-01"), ShowP Id, String)
-- >>> r = unsafeRefined3' oz "2019-04-23" :: K1
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K1 (B.encode r)
-- Refined3 {r3In = 2019-04-23, r3Out = "2019-04-23"}
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K2 (B.encode r)
-- Refined3 {r3In = 2019-04-23, r3Out = "2019-04-23"}
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K3 (B.encode r)
-- Refined3:Step 2. False Boolean Check(op) | {2019-05-30 <= 2019-04-23}
-- <BLANKLINE>
-- *** Step 1. Success Initial Conversion(ip) [2019-04-23] ***
-- <BLANKLINE>
-- P ReadP Day (2019-04-23) 2019-04-23 | 2019-04-23
-- |
-- `- P Id "2019-04-23"
-- <BLANKLINE>
-- *** Step 2. False Boolean Check(op) ***
-- <BLANKLINE>
-- False 2019-05-30 <= 2019-04-23
-- |
-- +- P Id 2019-04-23
-- |
-- +- P ReadP Day (2019-05-30) 2019-05-30 | 2019-05-30
-- |  |
-- |  `- P '2019-05-30
-- |
-- `- P ReadP Day (2019-06-01) 2019-06-01 | 2019-06-01
--    |
--    `- P '2019-06-01
-- <BLANKLINE>
--
instance ( Show (PP fmt (PP ip i))
         , Show (PP ip i)
         , Refined3C ip op fmt i
         , Binary i
         ) => Binary (Refined3 ip op fmt i) where
  get = do
          i <- B.get @i
          let (ret,mr) = eval3 @ip @op @fmt o2 i
          case mr of
            Nothing -> fail $ "Refined3:" ++ show (prt3Impl o2 ret)
            Just r -> return r
  put (Refined3 _ r) = B.put @i r

-- | creates a 4-tuple proxy (see 'withRefined3TP' 'newRefined3TP' 'eval3P' 'prtEval3P')
--
-- use type application to set the 4-tuple or skip that and set the individual parameters directly
--
-- set the 4-tuple directly
--
-- >>> eg1 = mkProxy3 @'(ReadP Int Id, Gt 10, ShowP Id, String)
-- >>> prtEval3P eg1 ol "24"
-- Right (Refined3 {r3In = 24, r3Out = "24"})
--
-- skip the 4-tuple and set each parameter individually using type application
--
-- >>> eg2 = mkProxy3 @_ @(ReadP Int Id) @(Gt 10) @(ShowP Id)
-- >>> prtEval3P eg2 ol "24"
-- Right (Refined3 {r3In = 24, r3Out = "24"})
--
mkProxy3 :: forall z ip op fmt i . z ~ '(ip,op,fmt,i) => Proxy '(ip,op,fmt,i)
mkProxy3 = Proxy

-- | same as 'mkProxy3' but checks to make sure the proxy is consistent with the 'Refined3C' constraint
mkProxy3' :: forall z ip op fmt i . (z ~ '(ip,op,fmt,i), Refined3C ip op fmt i) => Proxy '(ip,op,fmt,i)
mkProxy3' = Proxy

-- | convenience type family for converting from a 4-tuple '(ip,op,fmt,i) to a 'Refined3' signature
type family MakeR3 p where
  MakeR3 '(ip,op,fmt,i) = Refined3 ip op fmt i

withRefined3TIO :: forall ip op fmt i m b
  . (MonadIO m, Refined3C ip op fmt i, Show (PP ip i), Show i)
  => POpts
  -> i
  -> (Refined3 ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined3TIO opts = (>>=) . newRefined3TPIO (Proxy @'(ip,op,fmt,i)) opts

-- | create a 'Refined3' value and pass it to a continuation to be processed
--
-- This first example reads a hex string and makes sure it is between 100 and 200 and then
-- reads a binary string and adds the values together
--
-- >>> :set -XPolyKinds
-- >>> b16 = Proxy @'(ReadBase Int 16 Id, Between 100 200, ShowBase 16 Id, String)
-- >>> b2 = Proxy @'(ReadBase Int 2 Id, 'True, ShowBase 2 Id, String)
-- >>> prtRefinedTIO $ withRefined3TP b16 oz "a3" $ \x -> withRefined3TP b2 oz "1001110111" $ \y -> pure (r3In x + r3In y)
-- 794
--
-- this example fails as the the hex value is out of range
--
-- >>> prtRefinedTIO $ withRefined3TP b16 o0 "a388" $ \x -> withRefined3TP b2 o0 "1001110111" $ \y -> pure (x,y)
-- <BLANKLINE>
-- *** Step 1. Success Initial Conversion(ip) [41864] ***
-- <BLANKLINE>
-- P ReadBase(Int,16) 41864 | "a388"
-- |
-- `- P Id "a388"
-- <BLANKLINE>
-- *** Step 2. False Boolean Check(op) ***
-- <BLANKLINE>
-- False 41864 <= 200
-- |
-- +- P Id 41864
-- |
-- +- P '100
-- |
-- `- P '200
-- <BLANKLINE>
-- failure msg[Step 2. False Boolean Check(op) | {41864 <= 200}]
--
withRefined3T :: forall ip op fmt i m b
  . (Monad m, Refined3C ip op fmt i, Show (PP ip i), Show i)
  => POpts
  -> i
  -> (Refined3 ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined3T opts = (>>=) . newRefined3TP (Proxy @'(ip,op,fmt,i)) opts

withRefined3TP :: forall m ip op fmt i b proxy
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

-- | create a wrapped 'Refined3' type
--
-- >>> prtRefinedTIO $ newRefined3TP (Proxy @'(MkDay >> Just Id, GuardSimple (Thd Id == 5) >> 'True, UnMkDay (Fst Id), (Int,Int,Int))) oz (2019,11,1)
-- Refined3 {r3In = (2019-11-01,44,5), r3Out = (2019,11,1)}
--
-- >>> prtRefinedTIO $ newRefined3TP (Proxy @'(MkDay >> Just Id, Thd Id == 5, UnMkDay (Fst Id), (Int,Int,Int))) ol (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {6 == 5}]
--
-- >>> prtRefinedTIO $ newRefined3TP (Proxy @'(MkDay >> Just Id, Msg "wrong day:" (Thd Id == 5), UnMkDay (Fst Id), (Int,Int,Int))) ol (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {wrong day:6 == 5}]
--
newRefined3TP :: forall m ip op fmt i proxy
   . (Refined3C ip op fmt i, Monad m, Show (PP ip i), Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> RefinedT m (Refined3 ip op fmt i)
newRefined3TP = newRefined3TPImpl (return . runIdentity)

newRefined3TPIO :: forall m ip op fmt i proxy
   . (Refined3C ip op fmt i
    , MonadIO m
    , Show (PP ip i)
    , Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> RefinedT m (Refined3 ip op fmt i)
newRefined3TPIO = newRefined3TPImpl liftIO

newRefined3TPImpl :: forall n m ip op fmt i proxy
   . (Refined3C ip op fmt i
    , Monad m
    , MonadEval n
    , Show (PP ip i)
    , Show (PP fmt (PP ip i)))
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

-- | attempts to cast a wrapped 'Refined3' to another 'Refined3' with different predicates
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

-- | applies a binary operation to two wrapped 'Refined3' parameters
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

-- prtRefinedT $ rapply3P base16 (+) (newRefined3TP Proxy "ff") (newRefined3TP Proxy "22")

-- | same as 'rapply3' but uses a 4-tuple proxy instead
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

-- | An ADT that summarises the results of evaluating Refined3 representing all possible states
data RResults a b =
       RF String (Tree PE)        -- Left e
     | RTF a (Tree PE) String (Tree PE)    -- Right a + Left e
     | RTFalse a (Tree PE) (Tree PE)        -- Right a + Right False
     | RTTrueF a (Tree PE) (Tree PE) String (Tree PE) -- Right a + Right True + Left e
     | RTTrueT a (Tree PE) (Tree PE) b (Tree PE)      -- Right a + Right True + Right b
     deriving Show

-- | same as 'prtEval3P' but runs in IO
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

-- | same as 'prtEval3P' but skips the proxy and allows you to set each parameter individually using type application
prtEval3 :: forall ip op fmt i
  . ( Refined3C ip op fmt i
    , Show (PP ip i)
    , Show i)
  => POpts
  -> i
  -> Either Msg3 (Refined3 ip op fmt i)
prtEval3 = prtEval3P Proxy

-- | create a Refined3 using a 4-tuple proxy and aggregate the results on failure
prtEval3P :: forall ip op fmt i proxy
  . ( Refined3C ip op fmt i
    , Show (PP ip i)
    , Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> Either Msg3 (Refined3 ip op fmt i)
prtEval3P _ opts = prt3 opts . eval3 opts

-- | create a Refined3 value using a 4-tuple proxy (see 'mkProxy3')
--
-- use 'mkProxy3' to package all the types together as a 4-tuple
--
eval3P :: forall ip op fmt i proxy . Refined3C ip op fmt i
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> (RResults (PP ip i) (PP fmt (PP ip i)), Maybe (Refined3 ip op fmt i))
eval3P _ opts = runIdentity . eval3M opts

-- | same as 'eval3P' but can pass the parameters individually using type application
eval3 :: forall ip op fmt i . Refined3C ip op fmt i
  => POpts
  -> i
  -> (RResults (PP ip i) (PP fmt (PP ip i)), Maybe (Refined3 ip op fmt i))
eval3 = eval3P Proxy

eval3M :: forall m ip op fmt i . (MonadEval m, Refined3C ip op fmt i)
  => POpts
  -> i
  -> m (RResults (PP ip i) (PP fmt (PP ip i)), Maybe (Refined3 ip op fmt i))
eval3M opts i = do
  ll <- eval (Proxy @ip) opts i
  case getValAndPE ll of
   (Right a, t1) -> do
     rr <- evalBool (Proxy @op) opts a
     case getValAndPE rr of
      (Right True,t2) -> do
        ss <- eval (Proxy @fmt) opts a
        pure $ case getValAndPE ss of
         (Right b,t3) -> (RTTrueT a t1 t2 b t3, Just (Refined3 a b))
         (Left e,t3) -> (RTTrueF a t1 t2 e t3, Nothing)
      (Right False,t2) -> pure (RTFalse a t1 t2, Nothing)
      (Left e,t2) -> pure (RTF a t1 e t2, Nothing)
   (Left e,t1) -> pure (RF e t1, Nothing)

-- | creates Refined3 value but skips the initial conversion
eval3MSkip :: forall m ip op fmt i . (MonadEval m, Refined3C ip op fmt i)
   => POpts
   -> PP ip i
   -> m (RResults (PP ip i) (PP fmt (PP ip i)), Maybe (Refined3 ip op fmt i))
eval3MSkip opts a = do
   rr <- evalBool (Proxy @op) opts a
   case getValAndPE rr of
    (Right True,t2) -> do
      ss <- eval (Proxy @fmt) opts a
      pure $ case getValAndPE ss of
       (Right b,t3) -> (RTTrueT a mkNodeSkipP t2 b t3, Just (Refined3 a b))
       (Left e,t3) -> (RTTrueF a mkNodeSkipP t2 e t3, Nothing)
    (Right False,t2) -> pure (RTFalse a mkNodeSkipP t2, Nothing)
    (Left e,t2) -> pure (RTF a mkNodeSkipP e t2, Nothing)

-- | calculates from internal value
eval3MQuickIdentity :: forall ip op fmt i . Refined3C ip op fmt i
   => PP ip i
   -> Maybe (Refined3 ip op fmt i)
eval3MQuickIdentity = runIdentity . eval3MQuick

-- from PP ip i
eval3MQuick :: forall m ip op fmt i . (MonadEval m, Refined3C ip op fmt i)
   => PP ip i
   -> m (Maybe (Refined3 ip op fmt i))
eval3MQuick a = do
  let opts = oz
  rr <- evalBool (Proxy @op) opts a
  case getValLRFromTT rr of
    Right True -> do
      ss <- eval (Proxy @fmt) opts a
      pure $ case getValLRFromTT ss of
        Right b -> Just (Refined3 a b)
        _ -> Nothing
    _ -> pure Nothing

prt3IO :: (Show a, Show b) => POpts -> (RResults a b, Maybe r) -> IO (Either String r)
prt3IO opts (ret,mr) = do
  let m3 = prt3Impl opts ret
  unless (hasNoTree opts) $ putStrLn $ m3Long m3
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
      mkMsg3 m n r | hasNoTree opts = Msg3 m n ""
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
         let (m,n) = ("Step 2. False Boolean Check(op)", z)
             z = case t2 ^? root . pStrings . ix 0 of
                   Just w -> if null (dropWhile isSpace w) then "FalseP" else "{" <> w <> "}"
                   Nothing -> "FalseP"
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

-- | similar to 'eval3P' but it emulates 'Refined3' but using 'Refined'
--
-- takes a 4-tuple proxy as input but outputs the Refined value and the result separately
--
-- * initial conversion using \'ip\' and stores that in 'Refined'
-- * runs the boolean predicate \'op\' to make sure to validate the converted value from 1.
-- * runs \'fmt\' against the converted value from 1.
-- * returns both the 'Refined' and the output from 3.
-- * if any of the above steps fail the process stops it and dumps out 'RResults'
--
eval3PX :: forall ip op fmt i proxy . Refined3C ip op fmt i
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> (RResults (PP ip i) (PP fmt (PP ip i)), Maybe (Refined op (PP ip i), PP fmt (PP ip i)))
eval3PX _ opts i = runIdentity $ do
  ll <- eval (Proxy @ip) opts i
  case getValAndPE ll of
    (Right a,t1) -> do
      rr <- evalBool (Proxy @op) opts a
      case getValAndPE rr of
        (Right True,t2) -> do
          ss <- eval (Proxy @fmt) opts a
          pure $ case getValAndPE ss of
            (Right b,t3) -> (RTTrueT a t1 t2 b t3, Just (unsafeRefined a, b))
            (Left e,t3) -> (RTTrueF a t1 t2 e t3, Nothing)
        (Right False,t2) -> pure (RTFalse a t1 t2, Nothing)
        (Left e,t2) -> pure (RTF a t1 e t2, Nothing)
    (Left e,t1) -> pure (RF e t1, Nothing)

-- | same as 'eval3PX' but allows you to set the parameters individually using type application
eval3X :: forall ip op fmt i . Refined3C ip op fmt i
  => POpts
  -> i
  -> (RResults (PP ip i) (PP fmt (PP ip i)), Maybe (Refined op (PP ip i), PP fmt (PP ip i)))
eval3X = eval3PX (Proxy @'(ip,op,fmt,i))

-- | emulates 'Refined' using 'Refined3' by setting the input conversion and output formatting as noops
type RefinedEmulate p a = Refined3 Id p Id a

