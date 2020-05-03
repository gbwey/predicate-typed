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
-- |
-- Refinement type allowing the external type to differ from the internal type
-- see 'Refined1'
--
-- @
-- similar to 'Predicate.Refined2.Refined2' but also provides:
-- * quickCheck methods
-- * ability to combine refinement types
-- * a canonical output value using the \'fmt\' parameter
-- @
--
module Predicate.Refined1 (

  -- ** Refined1
    Refined1(unRefined1)
  , Refined1C

 -- ** display results
  , prtEval1
  , prtEval1P
  , prtEval1PIO
  , prt1IO
  , prt1
  , prt1Impl
  , Msg1 (..)
  , RResults1 (..)

  -- ** evaluation methods
  , eval1
  , eval1P

  -- ** create a wrapped Refined1 value
  , newRefined1T
  , newRefined1TP
  , newRefined1TPIO
  , withRefined1T
  , withRefined1TIO
  , withRefined1TP

  -- ** proxy methods
  , mkProxy1
  , mkProxy1'
  , MakeR1

  -- ** unsafe methods for creating Refined1
  , unsafeRefined1
  , unsafeRefined1'

  -- ** combine Refined1 values
  , convertRefined1TP
  , rapply1
  , rapply1P

  -- ** QuickCheck methods
  , arbRefined1
  , arbRefined1With

  -- ** emulate Refined1 using Refined
  , RefinedEmulate
  , eval1PX
  , eval1X

  -- ** extract from 4-tuple
  , T4_1
  , T4_2
  , T4_3
  , T4_4

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
import Data.Hashable (Hashable(..))
import GHC.Stack

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :m + Predicate.Prelude

-- | Refinement type that differentiates the input from output: similar to 'Refined3' but only creates the output value as needed.
--
--   * @i@ is the input type
--   * @ip@ converts @i@ to @PP ip i@ which is the internal type and stored in 'unRefined1'
--   * @op@ validates that internal type using @PP op (PP ip i) ~ Bool@
--   * @fmt@ outputs the internal type @PP fmt (PP ip i) ~ i@ (not stored anywhere but created on demand)
--   * @PP fmt (PP ip i)@ should be valid as input for Refined1
--
-- Setting @ip@ to @Id@ and @fmt@ to @Id@ makes it equivalent to 'Refined.Refined': see 'RefinedEmulate'
--
-- Setting the input type @i@ to 'GHC.Base.String' resembles the corresponding Read/Show instances but with an additional predicate on the read value
--
--   * __read__ a string using /ip/ into an internal type and store in 'unRefined1'
--   * __validate__ 'unRefined1' using the predicate /op/
--   * __show__ 'unRefined1' using /fmt/ and store that formatted result in 'r3Out'
--
-- Although a common scenario is String as input, you are free to choose any input type you like
--
-- >>> prtEval1 @(ReadBase Int 16 Id) @(Lt 255) @(PrintF "%x" Id) oz "00fe"
-- Right (Refined1 {unRefined1 = 254})
--
-- >>> prtEval1 @(ReadBase Int 16 Id) @(Lt 253) @(PrintF "%x" Id) oz "00fe"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> prtEval1 @(ReadBase Int 16 Id) @(Lt 255) @(PrintF "%x" Id) oz "00fg"
-- Left Step 1. Initial Conversion(ip) Failed | invalid base 16
--
-- >>> prtEval1 @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Msg "length invalid:" (Len == 4)) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) ol "198.162.3.1.5"
-- Left Step 2. False Boolean Check(op) | {length invalid:5 == 4}
--
-- >>> prtEval1 @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Guard (PrintF "found length=%d" Len) (Len == 4) >> 'True) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) oz "198.162.3.1.5"
-- Left Step 2. Failed Boolean Check(op) | found length=5
--
-- >>> prtEval1 @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Guard (PrintF "found length=%d" Len) (Len == 4) >> 'True) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) oz "198.162.3.1"
-- Right (Refined1 {unRefined1 = [198,162,3,1]})
--
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> prtEval1 @(MkDay >> 'Just Id) @(Guard "expected a Sunday" (Thd Id == 7) >> 'True) @(UnMkDay (Fst Id)) oz (2019,10,13)
-- Right (Refined1 {unRefined1 = (2019-10-13,41,7)})
--
-- >>> prtEval1 @(MkDay >> 'Just Id) @(Msg "expected a Sunday:" (Thd Id == 7)) @(UnMkDay (Fst Id)) ol (2019,10,12)
-- Left Step 2. False Boolean Check(op) | {expected a Sunday:6 == 7}
--
-- >>> prtEval1 @(MkDay' (Fst Id) (Snd Id) (Thd Id) >> 'Just Id) @(Guard "expected a Sunday" (Thd Id == 7) >> 'True) @(UnMkDay (Fst Id)) oz (2019,10,12)
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
-- >>> type T4 k = '(MkDay >> 'Just Id, Guard "expected a Sunday" (Thd Id == 7) >> 'True, UnMkDay (Fst Id), k)
-- >>> prtEval1P (Proxy @(T4 _)) oz (2019,10,12)
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
-- >>> prtEval1P (Proxy @(T4 _)) oz (2019,10,13)
-- Right (Refined1 {unRefined1 = (2019-10-13,41,7)})
--
newtype Refined1 ip op fmt i = Refined1 { unRefined1 :: PP ip i }

type role Refined1 nominal nominal nominal nominal

-- | directly load values into 'Refined1'. It still checks to see that those values are valid
unsafeRefined1' :: forall ip op fmt i
                . (HasCallStack, Show i, Show (PP ip i), Refined1C ip op fmt i)
                => POpts
                -> i
                -> Refined1 ip op fmt i
unsafeRefined1' opts i =
  let (ret,mr) = eval1 @ip @op @fmt opts i
  in fromMaybe (error $ show (prt1Impl opts ret)) mr

-- | directly load values into 'Refined1' without any checking
unsafeRefined1 :: forall ip op fmt i . PP ip i -> Refined1 ip op fmt i
unsafeRefined1 = Refined1


-- | Provides the constraints on Refined1
type Refined1C ip op fmt i =
       ( P ip i
       , P op (PP ip i)
       , PP op (PP ip i) ~ Bool   -- the internal value needs to pass the predicate check
       , P fmt (PP ip i)
       , PP fmt (PP ip i) ~ i  -- the output type must match the original input type
       )

deriving instance (Show i, Show (PP ip i), Show (PP fmt (PP ip i))) => Show (Refined1 ip op fmt i)
deriving instance (Eq i, Eq (PP ip i), Eq (PP fmt (PP ip i))) => Eq (Refined1 ip op fmt i)
deriving instance (TH.Lift (PP ip i), TH.Lift (PP fmt (PP ip i))) => TH.Lift (Refined1 ip op fmt i)

instance (Refined1C ip op fmt String, Show (PP ip String)) => IsString (Refined1 ip op fmt String) where
  fromString s =
    let (ret,mr) = eval1 @ip @op @fmt o2 s
    in fromMaybe (error $ "Refined1(fromString):" ++ show (prt1Impl o2 ret)) mr

-- read instance from -ddump-deriv
-- | 'Read' instance for 'Refined1'
--
-- >>> reads @(Refined1 (ReadBase Int 16 Id) (Between 0 255 Id) (ShowBase 16 Id) String) "Refined1 {unRefined1 = 254}"
-- [(Refined1 {unRefined1 = 254},"")]
--
-- >>> reads @(Refined1 (ReadBase Int 16 Id) (Between 0 255 Id) (ShowBase 16 Id) String) "Refined1 {unRefined1 = 300}"
-- []
--
-- >>> reads @(Refined1 (ReadBase Int 16 Id) (Id < 0) (ShowBase 16 Id) String) "Refined1 {unRefined1 = -1234}"
-- [(Refined1 {unRefined1 = -1234},"")]
--
-- >>> reads @(Refined1 (Map (ReadP Int Id) (Resplit "\\." Id)) (Guard "len/=4" (Len == 4) >> 'True) (PrintL 4 "%d.%d.%d.%d" Id) String) "Refined1 {unRefined1 = [192,168,0,1]}"
-- [(Refined1 {unRefined1 = [192,168,0,1]},"")]
--
instance ( Eq i
         , Show i
         , Eq (PP ip i)
         , Show (PP ip i)
         , Refined1C ip op fmt i
         , Read (PP ip i)
         , Read (PP fmt (PP ip i))
         ) => Read (Refined1 ip op fmt i) where
    readPrec
      = GR.parens
          (PCR.prec
             11
             (do GR.expectP (RL.Ident "Refined1")
                 GR.expectP (RL.Punc "{")
                 fld1 <- readField
                               "unRefined1" (PCR.reset GR.readPrec)
                 GR.expectP (RL.Punc "}")

                 let (_ret,mr) = runIdentity $ eval1MSkip @_ @ip @op @fmt oz fld1
                 case mr of
                   Nothing -> fail ""
                   Just (Refined1 r1)
                     | r1 == fld1 -> pure (Refined1 r1)
                     | otherwise -> fail "" -- cant display a decent failure message
             ))
    readList = GR.readListDefault
    readListPrec = GR.readListPrecDefault

-- | 'ToJSON' instance for 'Refined1'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.encode (unsafeRefined1 @(ReadBase Int 16 Id) @(Between 0 255 Id) @(ShowBase 16 Id) 254)
-- "\"fe\""
--
-- >>> A.encode (unsafeRefined1 @Id @'True @Id 123)
-- "123"
--
instance (Show (PP fmt (PP ip i)), ToJSON (PP fmt (PP ip i)), P fmt (PP ip i)) => ToJSON (Refined1 ip op fmt i) where
  toJSON (Refined1 x) =
      let ss = runIdentity $ eval (Proxy @fmt) o2 x
      in case getValAndPE ss of
           (Right b,_) -> toJSON b
           (Left e,t3) -> error $ "oops tojson failed " ++ show e ++ " t3=" ++ show t3


-- | 'FromJSON' instance for 'Refined1'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode' @(Refined1 (ReadBase Int 16 Id) (Id > 10 && Id < 256) (ShowBase 16 Id) String) "\"00fe\""
-- Right (Refined1 {unRefined1 = 254})
--
-- >>> removeAnsi $ A.eitherDecode' @(Refined1 (ReadBase Int 16 Id) (Id > 10 && Id < 256) (ShowBase 16 Id) String) "\"00fe443a\""
-- Error in $: Refined1:Step 2. False Boolean Check(op) | {True && False | (16663610 < 256)}
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
-- +- True 16663610 > 10
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
        , Refined1C ip op fmt i
        , FromJSON i
        ) => FromJSON (Refined1 ip op fmt i) where
  parseJSON z = do
                  i <- parseJSON @i z
                  let (ret,mr) = eval1 @ip @op @fmt o2 i
                  case mr of
                    Nothing -> fail $ "Refined1:" ++ show (prt1Impl o2 ret)
                    Just r -> return r

{-
instance (Arbitrary (PP ip i)
        , Show (PP ip i)
        , Show i
        , Refined1C ip op fmt i
        ) => Arbitrary (Refined1 ip op fmt i) where
  arbitrary = suchThatMap (arbitrary @(PP ip i)) $ eval1MQuickIdentity @ip @op @fmt
-}
arbRefined1 :: forall ip op fmt i .
   ( Arbitrary (PP ip i)
   , Refined1C ip op fmt i
   ) => Proxy '(ip,op,fmt,i)
     -> Gen (Refined1 ip op fmt i)
arbRefined1 = flip arbRefined1With id

-- | uses arbitrary to generate the internal 'unRefined1' and then uses \'fmt\' to fill in the 'r3Out' value
arbRefined1With ::
    forall ip op fmt i
  . (Arbitrary (PP ip i)
   , Refined1C ip op fmt i)
  => Proxy '(ip,op,fmt,i)
  -> (PP ip i -> PP ip i)
  -> Gen (Refined1 ip op fmt i)
arbRefined1With _ f =
  suchThatMap (f <$> arbitrary @(PP ip i)) $ eval1MQuickIdentity @ip @op @fmt

-- | 'Binary' instance for 'Refined1'
--
-- >>> import Control.Arrow ((+++))
-- >>> import Control.Lens
-- >>> import Data.Time
-- >>> type K1 = MakeR1 '(ReadP Day Id, 'True, ShowP Id, String)
-- >>> type K2 = MakeR1 '(ReadP Day Id, Between (ReadP Day "2019-05-30") (ReadP Day "2019-06-01") Id, ShowP Id, String)
-- >>> r = unsafeRefined1' oz "2019-04-23" :: K1
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K1 (B.encode r)
-- Refined1 {unRefined1 = 2019-04-23}
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K2 (B.encode r)
-- Refined1:Step 2. False Boolean Check(op) | {2019-05-30 <= 2019-04-23}
-- <BLANKLINE>
-- *** Step 1. Success Initial Conversion(ip) [2019-04-23] ***
-- <BLANKLINE>
-- P ReadP Day 2019-04-23
-- |
-- `- P Id "2019-04-23"
-- <BLANKLINE>
-- *** Step 2. False Boolean Check(op) ***
-- <BLANKLINE>
-- False 2019-05-30 <= 2019-04-23
-- |
-- +- P Id 2019-04-23
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

instance ( Show (PP fmt (PP ip i))
         , Show (PP ip i)
         , Refined1C ip op fmt i
         , Binary i
         ) => Binary (Refined1 ip op fmt i) where
  get = do
          i <- B.get @i
          let (ret,mr) = eval1 @ip @op @fmt o2 i
          case mr of
            Nothing -> fail $ "Refined1:" ++ show (prt1Impl o2 ret)
            Just r -> return r
  put (Refined1 x) =
      let ss = runIdentity $ eval (Proxy @fmt) o2 x
      in case getValAndPE ss of
           (Right b,_) -> B.put @i b
           (Left e,t3) -> error $ "oops tojson failed " ++ show e ++ " t3=" ++ show t3

-- | 'Hashable' instance for 'Refined1'
instance (Refined1C ip op fmt i
        , Hashable (PP ip i)
        ) => Hashable (Refined1 ip op fmt i) where
  hashWithSalt s (Refined1 a) = s + hash a

-- | creates a 4-tuple proxy (see 'withRefined1TP' 'newRefined1TP' 'eval1P' 'prtEval1P')
--
-- use type application to set the 4-tuple or set the individual parameters directly
--
-- set the 4-tuple directly
--
-- >>> eg1 = mkProxy1 @'(ReadP Int Id, Gt 10, ShowP Id, String)
-- >>> prtEval1P eg1 ol "24"
-- Right (Refined1 {unRefined1 = 24})
--
-- skip the 4-tuple and set each parameter individually using type application
--
-- >>> eg2 = mkProxy1 @_ @(ReadP Int Id) @(Gt 10) @(ShowP Id)
-- >>> prtEval1P eg2 ol "24"
-- Right (Refined1 {unRefined1 = 24})
--
mkProxy1 :: forall z ip op fmt i . z ~ '(ip,op,fmt,i) => Proxy '(ip,op,fmt,i)
mkProxy1 = Proxy

-- | same as 'mkProxy1' but checks to make sure the proxy is consistent with the 'Refined1C' constraint
mkProxy1' :: forall z ip op fmt i . (z ~ '(ip,op,fmt,i), Refined1C ip op fmt i) => Proxy '(ip,op,fmt,i)
mkProxy1' = Proxy

-- | type family for converting from a 4-tuple '(ip,op,fmt,i) to a 'Refined1' type
type family MakeR1 p where
  MakeR1 '(ip,op,fmt,i) = Refined1 ip op fmt i

withRefined1TIO :: forall ip op fmt i m b
  . (MonadIO m, Refined1C ip op fmt i, Show (PP ip i), Show i)
  => POpts
  -> i
  -> (Refined1 ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined1TIO opts = (>>=) . newRefined1TPIO (Proxy @'(ip,op,fmt,i)) opts

-- | create a 'Refined1' value using a continuation
--
-- This first example reads a hex string and makes sure it is between 100 and 200 and then
-- reads a binary string and adds the values together
--
-- >>> :set -XPolyKinds
-- >>> b16 = Proxy @'(ReadBase Int 16 Id, Between 100 200 Id, ShowBase 16 Id, String)
-- >>> b2 = Proxy @'(ReadBase Int 2 Id, 'True, ShowBase 2 Id, String)
-- >>> prtRefinedTIO $ withRefined1TP b16 oz "a3" $ \x -> withRefined1TP b2 oz "1001110111" $ \y -> pure (unRefined1 x + unRefined1 y)
-- 794
--
-- this example fails as the the hex value is out of range
--
-- >>> prtRefinedTIO $ withRefined1TP b16 o0 "a388" $ \x -> withRefined1TP b2 o0 "1001110111" $ \y -> pure (x,y)
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
withRefined1T :: forall ip op fmt i m b
  . (Monad m, Refined1C ip op fmt i, Show (PP ip i), Show i)
  => POpts
  -> i
  -> (Refined1 ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined1T opts = (>>=) . newRefined1TP (Proxy @'(ip,op,fmt,i)) opts

withRefined1TP :: forall m ip op fmt i b proxy
  . (Monad m, Refined1C ip op fmt i, Show (PP ip i), Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> (Refined1 ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined1TP p opts = (>>=) . newRefined1TP p opts

newRefined1T :: forall m ip op fmt i . (Refined1C ip op fmt i, Monad m, Show (PP ip i), Show i)
   => POpts
   -> i
   -> RefinedT m (Refined1 ip op fmt i)
newRefined1T = newRefined1TP (Proxy @'(ip,op,fmt,i))

-- | create a wrapped 'Refined1' type
--
-- >>> prtRefinedTIO $ newRefined1TP (Proxy @'(MkDay >> Just Id, GuardSimple (Thd Id == 5) >> 'True, UnMkDay (Fst Id), (Int,Int,Int))) oz (2019,11,1)
-- Refined1 {unRefined1 = (2019-11-01,44,5)}
--
-- >>> prtRefinedTIO $ newRefined1TP (Proxy @'(MkDay >> Just Id, Thd Id == 5, UnMkDay (Fst Id), (Int,Int,Int))) ol (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {6 == 5}]
--
-- >>> prtRefinedTIO $ newRefined1TP (Proxy @'(MkDay >> Just Id, Msg "wrong day:" (Thd Id == 5), UnMkDay (Fst Id), (Int,Int,Int))) ol (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {wrong day:6 == 5}]
--
newRefined1TP :: forall m ip op fmt i proxy
   . (Refined1C ip op fmt i, Monad m, Show (PP ip i), Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> RefinedT m (Refined1 ip op fmt i)
newRefined1TP = newRefined1TPImpl (return . runIdentity)

newRefined1TPIO :: forall m ip op fmt i proxy
   . (Refined1C ip op fmt i
    , MonadIO m
    , Show (PP ip i)
    , Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> RefinedT m (Refined1 ip op fmt i)
newRefined1TPIO = newRefined1TPImpl liftIO

newRefined1TPImpl :: forall n m ip op fmt i proxy
   . (Refined1C ip op fmt i
    , Monad m
    , MonadEval n
    , Show (PP ip i)
    , Show (PP fmt (PP ip i)))
  => (forall x . n x -> RefinedT m x)
   -> proxy '(ip,op,fmt,i)
   -> POpts
   -> i
   -> RefinedT m (Refined1 ip op fmt i)
newRefined1TPImpl f _ opts i = do
  (ret,mr) <- f $ eval1M opts i
  let m3 = prt1Impl opts ret
  tell [m3Long m3]
  case mr of
    Nothing -> throwError $ m3Desc m3 <> " | " <> m3Short m3
    Just r -> return r

newRefined1TPSkipIPImpl :: forall n m ip op fmt i proxy
   . (Refined1C ip op fmt i
    , Monad m
    , MonadEval n
    , Show (PP ip i)
    , Show (PP fmt (PP ip i)))
  => (forall x . n x -> RefinedT m x)
   -> proxy '(ip,op,fmt,i)
   -> POpts
   -> PP ip i
   -> RefinedT m (Refined1 ip op fmt i)
newRefined1TPSkipIPImpl f _ opts a = do
  (ret,mr) <- f $ eval1MSkip opts a
  let m3 = prt1Impl opts ret
  tell [m3Long m3]
  case mr of
    Nothing -> throwError $ m3Desc m3 <> " | " <> m3Short m3
    Just r -> return r

-- | attempts to cast a wrapped 'Refined1' to another 'Refined1' with different predicates
convertRefined1TP :: forall m ip op fmt i ip1 op1 fmt1 i1 .
  ( Refined1C ip1 op1 fmt1 i1
  , Monad m
  , Show (PP ip i)
  , PP ip i ~ PP ip1 i1
  , Show i1)
  => Proxy '(ip, op, fmt, i)
  -> Proxy '(ip1, op1, fmt1, i1)
  -> POpts
  -> RefinedT m (Refined1 ip op fmt i)
  -> RefinedT m (Refined1 ip1 op1 fmt1 i1)
convertRefined1TP _ _ opts ma = do
  Refined1 x <- ma
  -- we skip the input value @Id and go straight to the internal value so PP fmt (PP ip i) /= i for this call
  Refined1 a <- newRefined1TPSkipIPImpl (return . runIdentity) (Proxy @'(ip1, op1, fmt1, i1)) opts x
  return (Refined1 a)

-- | applies a binary operation to two wrapped 'Refined1' parameters
rapply1 :: forall m ip op fmt i .
  ( Refined1C ip op fmt i
  , Monad m
  , Show (PP ip i)
  , Show i)
  => POpts
  -> (PP ip i -> PP ip i -> PP ip i)
  -> RefinedT m (Refined1 ip op fmt i)
  -> RefinedT m (Refined1 ip op fmt i)
  -> RefinedT m (Refined1 ip op fmt i)
rapply1 = rapply1P (Proxy @'(ip,op,fmt,i))

-- prtRefinedT $ rapply1P base16 (+) (newRefined1TP Proxy "ff") (newRefined1TP Proxy "22")

-- | same as 'rapply1' but uses a 4-tuple proxy instead
rapply1P :: forall m ip op fmt i proxy .
  ( Refined1C ip op fmt i
  , Monad m
  , Show (PP ip i)
  , Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> (PP ip i -> PP ip i -> PP ip i)
  -> RefinedT m (Refined1 ip op fmt i)
  -> RefinedT m (Refined1 ip op fmt i)
  -> RefinedT m (Refined1 ip op fmt i)
rapply1P p opts f ma mb = do
  tell [bgColor Blue "=== a ==="]
  Refined1 x <- ma
  tell [bgColor Blue "=== b ==="]
  Refined1 y <- mb
  -- we skip the input value @Id and go straight to the internal value so PP fmt (PP ip i) /= i for this call
  tell [bgColor Blue "=== a `op` b ==="]
  Refined1 a <- newRefined1TPSkipIPImpl (return . runIdentity) p opts (f x y)
  return (Refined1 a)

-- | An ADT that summarises the results of evaluating Refined1 representing all possible states
data RResults1 a b =
       RF String (Tree PE)        -- Left e
     | RTF a (Tree PE) String (Tree PE)    -- Right a + Left e
     | RTFalse a (Tree PE) (Tree PE)        -- Right a + Right False
     | RTTrueF a (Tree PE) (Tree PE) String (Tree PE) -- Right a + Right True + Left e
     | RTTrueT a (Tree PE) (Tree PE) b (Tree PE)      -- Right a + Right True + Right b
     deriving Show

-- | same as 'prtEval1P' but runs in IO
prtEval1PIO :: forall ip op fmt i proxy
  . ( Refined1C ip op fmt i
    , Show (PP ip i)
    , Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> IO (Either String (Refined1 ip op fmt i))
prtEval1PIO _ opts i = do
  x <- eval1M opts i
  prt1IO opts x

-- | same as 'prtEval1P' but skips the proxy and allows you to set each parameter individually using type application
prtEval1 :: forall ip op fmt i
  . ( Refined1C ip op fmt i
    , Show (PP ip i)
    , Show i)
  => POpts
  -> i
  -> Either Msg1 (Refined1 ip op fmt i)
prtEval1 = prtEval1P Proxy

-- | create a Refined1 using a 4-tuple proxy and aggregate the results on failure
prtEval1P :: forall ip op fmt i proxy
  . ( Refined1C ip op fmt i
    , Show (PP ip i)
    , Show i)
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> Either Msg1 (Refined1 ip op fmt i)
prtEval1P _ opts = prt1 opts . eval1 opts

-- | create a Refined1 value using a 4-tuple proxy (see 'mkProxy1')
--
-- use 'mkProxy1' to package all the types together as a 4-tuple
--
eval1P :: forall ip op fmt i proxy . Refined1C ip op fmt i
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> (RResults1 (PP ip i) (PP fmt (PP ip i)), Maybe (Refined1 ip op fmt i))
eval1P _ opts = runIdentity . eval1M opts

-- | same as 'eval1P' but can pass the parameters individually using type application
eval1 :: forall ip op fmt i . Refined1C ip op fmt i
  => POpts
  -> i
  -> (RResults1 (PP ip i) (PP fmt (PP ip i)), Maybe (Refined1 ip op fmt i))
eval1 = eval1P Proxy

eval1M :: forall m ip op fmt i . (MonadEval m, Refined1C ip op fmt i)
  => POpts
  -> i
  -> m (RResults1 (PP ip i) (PP fmt (PP ip i)), Maybe (Refined1 ip op fmt i))
eval1M opts i = do
  ll <- eval (Proxy @ip) opts i
  case getValAndPE ll of
   (Right a, t1) -> do
     rr <- evalBool (Proxy @op) opts a
     case getValAndPE rr of
      (Right True,t2) -> do
        ss <- eval (Proxy @fmt) opts a
        pure $ case getValAndPE ss of
         (Right b,t3) -> (RTTrueT a t1 t2 b t3, Just (Refined1 a))
         (Left e,t3) -> (RTTrueF a t1 t2 e t3, Nothing)
      (Right False,t2) -> pure (RTFalse a t1 t2, Nothing)
      (Left e,t2) -> pure (RTF a t1 e t2, Nothing)
   (Left e,t1) -> pure (RF e t1, Nothing)

-- | creates Refined1 value but skips the initial conversion
eval1MSkip :: forall m ip op fmt i . (MonadEval m, Refined1C ip op fmt i)
   => POpts
   -> PP ip i
   -> m (RResults1 (PP ip i) (PP fmt (PP ip i)), Maybe (Refined1 ip op fmt i))
eval1MSkip opts a = do
   rr <- evalBool (Proxy @op) opts a
   case getValAndPE rr of
    (Right True,t2) -> do
      ss <- eval (Proxy @fmt) opts a
      pure $ case getValAndPE ss of
       (Right b,t3) -> (RTTrueT a mkNodeSkipP t2 b t3, Just (Refined1 a))
       (Left e,t3) -> (RTTrueF a mkNodeSkipP t2 e t3, Nothing)
    (Right False,t2) -> pure (RTFalse a mkNodeSkipP t2, Nothing)
    (Left e,t2) -> pure (RTF a mkNodeSkipP e t2, Nothing)

-- | calculates from internal value
eval1MQuickIdentity :: forall ip op fmt i . Refined1C ip op fmt i
   => PP ip i
   -> Maybe (Refined1 ip op fmt i)
eval1MQuickIdentity = runIdentity . eval1MQuick

-- from PP ip i
eval1MQuick :: forall m ip op fmt i . (MonadEval m, Refined1C ip op fmt i)
   => PP ip i
   -> m (Maybe (Refined1 ip op fmt i))
eval1MQuick a = do
  let opts = oz
  rr <- evalBool (Proxy @op) opts a
  case getValLRFromTT rr of
    Right True -> do
      ss <- eval (Proxy @fmt) opts a
      pure $ case getValLRFromTT ss of
        Right _ -> Just (Refined1 a)
        _ -> Nothing
    _ -> pure Nothing

prt1IO :: (Show a, Show b) => POpts -> (RResults1 a b, Maybe r) -> IO (Either String r)
prt1IO opts (ret,mr) = do
  let m3 = prt1Impl opts ret
  unless (hasNoTree opts) $ putStrLn $ m3Long m3
  return $ maybe (Left (m3Desc m3 <> " | " <> m3Short m3)) Right mr

prt1 :: (Show a, Show b) => POpts -> (RResults1 a b, Maybe r) -> Either Msg1 r
prt1 opts (ret,mr) = maybe (Left $ prt1Impl opts ret) Right mr

data Msg1 = Msg1 { m3Desc :: !String
                 , m3Short :: !String
                 , m3Long :: !String
                 } deriving Eq

instance Show Msg1 where
  show (Msg1 a b c) = a <> " | " <> b <> (if null c then "" else "\n" <> c)

prt1Impl :: (Show a, Show b)
  => POpts
  -> RResults1 a b
  -> Msg1
prt1Impl opts v =
  let outmsg msg = "\n*** " <> msg <> " ***\n\n"
      msg1 a = outmsg ("Step 1. Success Initial Conversion(ip) [" ++ show a ++ "]")
      mkMsg1 m n r | hasNoTree opts = Msg1 m n ""
                   | otherwise = Msg1 m n r
  in case v of
       RF e t1 ->
         let (m,n) = ("Step 1. Initial Conversion(ip) Failed", e)
             r = outmsg m
              <> prtTreePure opts t1
         in mkMsg1 m n r
       RTF a t1 e t2 ->
         let (m,n) = ("Step 2. Failed Boolean Check(op)", e)
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg m
              <> prtTreePure opts t2
         in mkMsg1 m n r
       RTFalse a t1 t2 ->
         let (m,n) = ("Step 2. False Boolean Check(op)", z)
             z = case t2 ^? root . pStrings . ix 0 of
                   Just w -> if null (dropWhile isSpace w) then "FalseP" else "{" <> w <> "}"
                   Nothing -> "FalseP"
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg m
              <> prtTreePure opts t2
         in mkMsg1 m n r
       RTTrueF a t1 t2 e t3 ->
         let (m,n) = ("Step 3. Failed Output Conversion(fmt)", e)
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg "Step 2. Success Boolean Check(op)"
              <> prtTreePure opts t2
              <> outmsg m
              <> prtTreePure opts t3
         in mkMsg1 m n r
       RTTrueT a t1 t2 b t3 ->
         let (m,n) = ("Step 3. Success Output Conversion(fmt)", show b)
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg "Step 2. Success Boolean Check(op)"
              <> prtTreePure opts t2
              <> outmsg m
              <> fixLite opts b t3
         in mkMsg1 m n r

-- | similar to 'eval1P' but it emulates 'Refined1' but using 'Refined'
--
-- takes a 4-tuple proxy as input but outputs the Refined value and the result separately
--
-- * initial conversion using \'ip\' and stores that in 'Refined'
-- * runs the boolean predicate \'op\' to make sure to validate the converted value from 1.
-- * runs \'fmt\' against the converted value from 1.
-- * returns both the 'Refined' and the output from 3.
-- * if any of the above steps fail the process stops it and dumps out 'RResults1'
--
eval1PX :: forall ip op fmt i proxy . Refined1C ip op fmt i
  => proxy '(ip,op,fmt,i)
  -> POpts
  -> i
  -> (RResults1 (PP ip i) (PP fmt (PP ip i)), Maybe (Refined op (PP ip i), PP fmt (PP ip i)))
eval1PX _ opts i = runIdentity $ do
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

-- | same as 'eval1PX' but allows you to set the parameters individually using type application
eval1X :: forall ip op fmt i . Refined1C ip op fmt i
  => POpts
  -> i
  -> (RResults1 (PP ip i) (PP fmt (PP ip i)), Maybe (Refined op (PP ip i), PP fmt (PP ip i)))
eval1X = eval1PX (Proxy @'(ip,op,fmt,i))

-- | emulates 'Refined' using 'Refined1' by setting the input conversion and output formatting as noops
type RefinedEmulate p a = Refined1 Id p Id a

-- | used by 'Refined1' to extract \'ip\' from a promoted 4-tuple
--
-- >>> pl @(T4_1 Predicate.Examples.Refined3.Ip4) "1.2.3.4"
-- Present [1,2,3,4] (Map [1,2,3,4] | ["1","2","3","4"])
-- PresentT [1,2,3,4]
--
type family T4_1 x where
  T4_1 '(a,b,c,d) = a

-- | used by 'Refined1' for extracting the boolean predicate \'op\' from a promoted 4-tuple
--
-- >>> pl @(T4_2 Predicate.Examples.Refined3.Ip4) [141,213,308,4]
-- Error octet 2 out of range 0-255 found 308
-- FailT "octet 2 out of range 0-255 found 308"
--
-- >>> pl @(T4_2 Predicate.Examples.Refined3.Ip4) [141,213,308,4,8]
-- Error Guards:invalid length(5) expected 4
-- FailT "Guards:invalid length(5) expected 4"
--
type family T4_2 x where
  T4_2 '(a,b,c,d) = b

-- | used by 'Refined1' for extracting \'fmt\' from a promoted 4-tuple
--
-- >>> pl @(T4_3 Predicate.Examples.Refined3.Ip4) [141,513,9,4]
-- Present "141.513.009.004" (PrintL(4) [141.513.009.004] | s=%03d.%03d.%03d.%03d)
-- PresentT "141.513.009.004"
--
type family T4_3 x where
  T4_3 '(a,b,c,d) = c

-- | used by 'Refined1' for extracting the input type \'i\' from a promoted 4-tuple
type family T4_4 x where
  T4_4 '(a,b,c,d) = d
