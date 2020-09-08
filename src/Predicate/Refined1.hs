{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE NoStarIsType #-}
-- |
-- Refinement type allowing the external type to differ from the internal type
-- doesnt store the output value but runs on demand but has calculate each time and could fail later
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
    Refined1
  , unRefined1
  , Refined1C

 -- ** display results
  , Msg1 (..)
  , RResults1 (..)

  -- ** evaluation methods
  , eval1
  , eval1P
  , eval1M
  , newRefined1
  , newRefined1P
  , newRefined1'
  , newRefined1P'

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
  , genRefined1
  , genRefined1P

  -- ** miscellaneous
  , replaceOpt1
  , appendOpt1

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
import Test.QuickCheck
import qualified GHC.Read as GR
import qualified Text.ParserCombinators.ReadPrec as PCR
import qualified Text.Read.Lex as RL
import qualified Data.Binary as B
import Data.Binary (Binary)
import Data.Maybe (fromMaybe)
import Control.Lens ((^.))
import Data.Tree.Lens (root)
import Data.Char (isSpace)
import Data.String
import Data.Hashable (Hashable(..))
import GHC.Stack
import Data.Coerce

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :m + Predicate.Prelude
-- >>> :m + Data.Time

-- | Refinement type that differentiates the input from output: similar to 'Predicate.Refined3.Refined3' but only creates the output value as needed.
--
--   * @opts@ are the display options
--   * @ip@ converts @i@ to @PP ip i@ which is the internal type and stored in 'unRefined1'
--   * @op@ validates that internal type using @PP op (PP ip i) ~ Bool@
--   * @fmt@ outputs the internal type @PP fmt (PP ip i) ~ i@ (not stored anywhere but created on demand)
--   * @i@ is the input type
--
--   * @PP fmt (PP ip i)@ should be valid as input for Refined1
--
-- Setting @ip@ to @Id@ and @fmt@ to @Id@ makes it equivalent to 'Refined.Refined'
--
-- Setting the input type @i@ to 'GHC.Base.String' resembles the corresponding Read/Show instances but with an additional predicate on the read value
--
--   * __read__ a string using /ip/ into an internal type and store in 'unRefined1'
--   * __validate__ 'unRefined1' using the predicate /op/
--   * __show__ 'unRefined1' using /fmt/ (does not store the formatted result unlike 'Predicate.Refined3.Refined3')
--
-- Although a common scenario is String as input, you are free to choose any input type you like
--
newtype Refined1 (opts :: Opt) ip op fmt i = Refined1 (PP ip i)

unRefined1 :: forall (opts :: Opt) ip op fmt i. Refined1 opts ip op fmt i -> PP ip i
unRefined1 (Refined1 a) = a

type role Refined1 phantom nominal nominal nominal nominal

-- | directly load values into 'Refined1'. It still checks to see that those values are valid
unsafeRefined1' :: forall opts ip op fmt i
                . ( HasCallStack
                  , Show (PP ip i)
                  , Refined1C opts ip op fmt i
                  )
                => i
                -> Refined1 opts ip op fmt i
unsafeRefined1' i =
  case newRefined1 @opts @ip @op @fmt i of
    Left e -> error $ show e
    Right r -> r

-- | directly load values into 'Refined1' without any checking
unsafeRefined1 :: forall opts ip op fmt i . PP ip i -> Refined1 opts ip op fmt i
unsafeRefined1 = Refined1

-- | Provides the constraints on Refined1
type Refined1C opts ip op fmt i =
       ( OptC opts
       , P ip i
       , P op (PP ip i)
       , PP op (PP ip i) ~ Bool   -- the internal value needs to pass the predicate check
       , P fmt (PP ip i)
       , PP fmt (PP ip i) ~ i  -- the output type must match the original input type
       )

deriving instance Show (PP ip i) => Show (Refined1 opts ip op fmt i)
deriving instance  Eq (PP ip i) => Eq (Refined1 opts ip op fmt i)
deriving instance TH.Lift (PP ip i) => TH.Lift (Refined1 opts ip op fmt i)

-- | 'IsString' instance for Refined1
--
-- >>> pureTryTest $ fromString @(Refined1 OL (ReadP Int Id) (Id > 12) (ShowP Id) String) "523"
-- Right (Refined1 523)
--
-- >>> pureTryTest $ fromString @(Refined1 OL (ReadP Int Id) (Id > 12) (ShowP Id) String) "2"
-- Left ()
--
instance (Refined1C opts ip op fmt String, Show (PP ip String)) => IsString (Refined1 opts ip op fmt String) where
  fromString s =
    let (ret,mr) = eval1 @opts @ip @op @fmt s
    in fromMaybe (error $ "Refined1(fromString):" ++ show (prt1Impl (getOpt @opts) ret)) mr

-- read instance from -ddump-deriv
-- | 'Read' instance for 'Refined1'
--
-- >>> reads @(Refined1 OZ (ReadBase Int 16 Id) (Between 0 255 Id) (ShowBase 16 Id) String) "Refined1 254"
-- [(Refined1 254,"")]
--
-- >>> reads @(Refined1 OZ (ReadBase Int 16 Id) (Between 0 255 Id) (ShowBase 16 Id) String) "Refined1 300"
-- []
--
-- >>> reads @(Refined1 OZ (ReadBase Int 16 Id) (Id < 0) (ShowBase 16 Id) String) "Refined1 (-1234)"
-- [(Refined1 (-1234),"")]
--
-- >>> reads @(Refined1 OZ (Map (ReadP Int Id) (Resplit "\\." Id)) (GuardBool "len/=4" (Len == 4)) (PrintL 4 "%d.%d.%d.%d" Id) String) "Refined1 [192,168,0,1]"
-- [(Refined1 [192,168,0,1],"")]
--
-- >>> reads @(Refined1 OZ Id 'True Id Int) "Refined1 (-123)xyz"
-- [(Refined1 (-123),"xyz")]
--
instance ( Eq (PP ip i)
         , Refined1C opts ip op fmt i
         , Read (PP ip i)
         ) => Read (Refined1 opts ip op fmt i) where
    readPrec
      = GR.parens
          (PCR.prec
             11
             (do GR.expectP (RL.Ident "Refined1")
                 fld1 <- PCR.reset GR.readPrec

                 let (_ret,mr) = runIdentity $ eval1MSkip @opts @ip @op @fmt fld1
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
-- >>> A.encode (unsafeRefined1 @OZ @(ReadBase Int 16 Id) @(Between 0 255 Id) @(ShowBase 16 Id) 254)
-- "\"fe\""
--
-- >>> A.encode (unsafeRefined1 @OZ @Id @'True @Id 123)
-- "123"
--
instance ( OptC opts
         , ToJSON (PP fmt (PP ip i))
         , P fmt (PP ip i)
         ) => ToJSON (Refined1 opts ip op fmt i) where
  toJSON (Refined1 x) =
      let ss = runIdentity $ eval (Proxy @fmt) (getOpt @opts) x
      in case getValAndPE ss of
           (Right b,_) -> toJSON b
           (Left e,t3) -> error $ "oops tojson failed " ++ show e ++ " t3=" ++ show t3


-- | 'FromJSON' instance for 'Refined1'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode' @(Refined1 OZ (ReadBase Int 16 Id) (Id > 10 && Id < 256) (ShowBase 16 Id) String) "\"00fe\""
-- Right (Refined1 254)
--
-- >>> removeAnsi $ A.eitherDecode' @(Refined1 OAN (ReadBase Int 16 Id) (Id > 10 && Id < 256) (ShowBase 16 Id) String) "\"00fe443a\""
-- Error in $: Refined1:Step 2. False Boolean Check(op) | {True && False | (16663610 < 256)}
-- *** Step 1. Success Initial Conversion(ip) (16663610) ***
-- P ReadBase(Int,16) 16663610
-- |
-- `- P Id "00fe443a"
-- *** Step 2. False Boolean Check(op) ***
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
instance (Show (PP ip i)
        , Refined1C opts ip op fmt i
        , FromJSON i
        ) => FromJSON (Refined1 opts ip op fmt i) where
  parseJSON z = do
                  i <- parseJSON @i z
                  let (ret,mr) = eval1 @opts @ip @op @fmt i
                  case mr of
                    Nothing -> fail $ "Refined1:" ++ show (prt1Impl (getOpt @opts) ret)
                    Just r -> return r
-- | 'Arbitrary' instance for 'Refined1'
--
-- >>> xs <- generate (vectorOf 10 (arbitrary @(Refined1 OAN (ReadP Int Id) (1 <..> 120 && Even) (ShowP Id) String)))
-- >>> all ((/=0) . unRefined1) xs
-- True
--
-- >>> xs <- generate (vectorOf 10 (arbitrary @(Refined1 OAN Id IsPrime Id Int)))
-- >>> all (isPrime . unRefined1) xs
-- True
--
instance (Arbitrary (PP ip i)
        , Refined1C opts ip op fmt i
        ) => Arbitrary (Refined1 opts ip op fmt i) where
  arbitrary = genRefined1 arbitrary

-- | create a 'Refined1' generator
--
-- >>> g = genRefined1 @OAN @(ReadP Int Id) @(Between 10 100 Id && Even) @(ShowP Id) (choose (10,100))
-- >>> xs <- generate (vectorOf 10 g)
-- >>> all (\x -> let y = unRefined1 x in y >= 0 && y <= 100 && even y) xs
-- True
--
genRefined1 ::
    forall opts ip op fmt i
  . Refined1C opts ip op fmt i
  => Gen (PP ip i)
  -> Gen (Refined1 opts ip op fmt i)
genRefined1 = genRefined1P Proxy

-- | create a 'Refined1' generator with a Proxy
genRefined1P ::
    forall opts ip op fmt i
  . Refined1C opts ip op fmt i
  => Proxy '(opts,ip,op,fmt,i)
  -> Gen (PP ip i)
  -> Gen (Refined1 opts ip op fmt i)
genRefined1P _ g =
  let o = getOpt @opts
      f !cnt = do
        mppi <- suchThatMaybe g $ \a -> evalQuick @op o a == Right True
        case mppi of
          Nothing ->
             if cnt >= oRecursion o
             then error $ setOtherEffects o ("genRefined1 recursion exceeded(" ++ show (oRecursion o) ++ ")")
             else f (cnt+1)
          Just ppi ->
             pure $ unsafeRefined1 ppi
  in f 0

-- | 'Binary' instance for 'Refined1'
--
-- >>> import Control.Arrow ((+++))
-- >>> import Control.Lens
-- >>> import Data.Time
-- >>> type K1 = MakeR1 '(OAN, ReadP Day Id, 'True, ShowP Id, String)
-- >>> type K2 = MakeR1 '(OAN, ReadP Day Id, Between (ReadP Day "2019-05-30") (ReadP Day "2019-06-01") Id, ShowP Id, String)
-- >>> r = unsafeRefined1' "2019-04-23" :: K1
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K1 (B.encode r)
-- Refined1 2019-04-23
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K2 (B.encode r)
-- Refined1:Step 2. False Boolean Check(op) | {2019-05-30 <= 2019-04-23}
-- *** Step 1. Success Initial Conversion(ip) (2019-04-23) ***
-- P ReadP Day 2019-04-23
-- |
-- `- P Id "2019-04-23"
-- *** Step 2. False Boolean Check(op) ***
-- False 2019-05-30 <= 2019-04-23
-- |
-- +- P Id 2019-04-23
-- |
-- +- P ReadP Day 2019-05-30
-- |  |
-- |  `- P '"2019-05-30"
-- |
-- `- P ReadP Day 2019-06-01
--    |
--    `- P '"2019-06-01"
-- <BLANKLINE>
--

instance ( Show (PP ip i)
         , Refined1C opts ip op fmt i
         , Binary i
         ) => Binary (Refined1 opts ip op fmt i) where
  get = do
          i <- B.get @i
          let (ret,mr) = eval1 @opts @ip @op @fmt i
          case mr of
            Nothing -> fail $ "Refined1:" ++ show (prt1Impl (getOpt @opts) ret)
            Just r -> return r
  put (Refined1 x) =
      let ss = runIdentity $ eval (Proxy @fmt) (getOpt @opts) x
      in case getValAndPE ss of
           (Right b,_) -> B.put @i b
           (Left e,t3) -> error $ "oops tojson failed " ++ show e ++ " t3=" ++ show t3

-- | 'Hashable' instance for 'Refined1'
instance (Refined1C opts ip op fmt i
        , Hashable (PP ip i)
        ) => Hashable (Refined1 opts ip op fmt i) where
  hashWithSalt s (Refined1 a) = s + hash a

-- | creates a 5-tuple proxy (see 'withRefined1TP' 'newRefined1TP' 'eval1P' 'newRefined1P')
--
-- use type application to set the 5-tuple or set the individual parameters directly
--
-- set the 5-tuple directly
--
-- >>> eg1 = mkProxy1 @'(OL, ReadP Int Id, Gt 10, ShowP Id, String)
-- >>> newRefined1P eg1 "24"
-- Right (Refined1 24)
--
-- skip the 5-tuple and set each parameter individually using type application
--
-- >>> eg2 = mkProxy1 @_ @OL @(ReadP Int Id) @(Gt 10) @(ShowP Id)
-- >>> newRefined1P eg2 "24"
-- Right (Refined1 24)
--
mkProxy1 ::
  forall z opts ip op fmt i
       . z ~ '(opts,ip,op,fmt,i)
       => Proxy '(opts,ip,op,fmt,i)
mkProxy1 = Proxy

-- | same as 'mkProxy1' but checks to make sure the proxy is consistent with the 'Refined1C' constraint
mkProxy1' :: forall z opts ip op fmt i
  . ( z ~ '(opts,ip,op,fmt,i)
    , Refined1C opts ip op fmt i
    ) => Proxy '(opts,ip,op,fmt,i)
mkProxy1' = Proxy

-- | type family for converting from a 5-tuple '(ip,op,fmt,i) to a 'Refined1' type
type family MakeR1 p where
  MakeR1 '(opts,ip,op,fmt,i) = Refined1 opts ip op fmt i

withRefined1TIO :: forall opts ip op fmt i m b
  . ( MonadIO m
    , Refined1C opts ip op fmt i
    , Show (PP ip i)
    )
  => i
  -> (Refined1 opts ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined1TIO = (>>=) . newRefined1TPIO (Proxy @'(opts,ip,op,fmt,i))

-- | create a 'Refined1' value using a continuation
--
-- This first example reads a hex string and makes sure it is between 100 and 200 and then
-- reads a binary string and adds the values together
--
-- >>> :set -XPolyKinds
-- >>> :set -XRankNTypes
-- >>> b16 :: forall opts . Proxy '( opts, ReadBase Int 16 Id, Between 100 200 Id, ShowBase 16 Id, String); b16 = Proxy
-- >>> b2 :: forall opts . Proxy '( opts, ReadBase Int 2 Id, 'True, ShowBase 2 Id, String); b2 = Proxy
-- >>> prtRefinedTIO $ withRefined1TP (b16 @OZ) "a3" $ \x -> withRefined1TP (b2 @OZ) "1001110111" $ \y -> pure (unRefined1 x + unRefined1 y)
-- 794
--
-- this example fails as the the hex value is out of range
--
-- >>> prtRefinedTIO $ withRefined1TP (b16 @OAN) "a388" $ \x -> withRefined1TP (b2 @OAN) "1001110111" $ \y -> pure (x,y)
-- *** Step 1. Success Initial Conversion(ip) (41864) ***
-- P ReadBase(Int,16) 41864
-- |
-- `- P Id "a388"
-- *** Step 2. False Boolean Check(op) ***
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
withRefined1T :: forall opts ip op fmt i m b
  . ( Monad m
    , Refined1C opts ip op fmt i
    , Show (PP ip i)
    )
  => i
  -> (Refined1 opts ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined1T = (>>=) . newRefined1TP (Proxy @'(opts,ip,op,fmt,i))

withRefined1TP :: forall opts ip op fmt i b proxy m
  . ( Monad m
    , Refined1C opts ip op fmt i
    , Show (PP ip i)
    )
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> (Refined1 opts ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined1TP p = (>>=) . newRefined1TP p


newRefined1T :: forall opts ip op fmt i m
  . ( Refined1C opts ip op fmt i
    , Monad m
    , Show (PP ip i)
    )
   => i
   -> RefinedT m (Refined1 opts ip op fmt i)
newRefined1T = newRefined1TP (Proxy @'(opts,ip,op,fmt,i))

-- | create a wrapped 'Refined1' type
--
-- >>> prtRefinedTIO $ newRefined1TP (Proxy @'(OZ, MkDayExtra Id >> 'Just Id, GuardSimple (Thd Id == 5) >> 'True, UnMkDay (Fst Id), (Int,Int,Int))) (2019,11,1)
-- Refined1 (2019-11-01,44,5)
--
-- >>> prtRefinedTIO $ newRefined1TP (Proxy @'(OL, MkDayExtra Id >> 'Just Id, Thd Id == 5, UnMkDay (Fst Id), (Int,Int,Int))) (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {6 == 5}]
--
-- >>> prtRefinedTIO $ newRefined1TP (Proxy @'(OL, MkDayExtra Id >> 'Just Id, Msg "wrong day:" (Thd Id == 5), UnMkDay (Fst Id), (Int,Int,Int))) (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {wrong day: 6 == 5}]
--
newRefined1TP :: forall opts ip op fmt i proxy m
   . ( Refined1C opts ip op fmt i
     , Monad m
     , Show (PP ip i)
     )
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> RefinedT m (Refined1 opts ip op fmt i)
newRefined1TP = newRefined1TPImpl (return . runIdentity)

newRefined1TPIO :: forall opts ip op fmt i proxy m
   . ( Refined1C opts ip op fmt i
     , MonadIO m
     , Show (PP ip i)
     )
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> RefinedT m (Refined1 opts ip op fmt i)
newRefined1TPIO = newRefined1TPImpl liftIO

newRefined1TPImpl :: forall n m opts ip op fmt i proxy
   . ( Refined1C opts ip op fmt i
     , Monad m
     , MonadEval n
     , Show (PP ip i)
     )
  => (forall x . n x -> RefinedT m x)
   -> proxy '(opts,ip,op,fmt,i)
   -> i
   -> RefinedT m (Refined1 opts ip op fmt i)
newRefined1TPImpl f _ i = do
  (ret,mr) <- f $ eval1M i
  let m1 = prt1Impl (getOpt @opts) ret
  tell [m1Long m1]
  case mr of
    Nothing -> throwError $ m1Desc m1 <> nullIf " | " (m1Short m1)
    Just r -> return r

newRefined1TPSkipIPImpl :: forall n m opts ip op fmt i proxy
   . ( Refined1C opts ip op fmt i
     , Monad m
     , MonadEval n
     , Show (PP ip i)
     )
  => (forall x . n x -> RefinedT m x)
   -> proxy '(opts,ip,op,fmt,i)
   -> PP ip i
   -> RefinedT m (Refined1 opts ip op fmt i)
newRefined1TPSkipIPImpl f _ a = do
  (ret,mr) <- f $ eval1MSkip a
  let m1 = prt1Impl (getOpt @opts) ret
  tell [m1Long m1]
  case mr of
    Nothing -> throwError $ m1Desc m1 <> nullIf " | " (m1Short m1)
    Just r -> return r

-- | attempts to cast a wrapped 'Refined1' to another 'Refined1' with different predicates
convertRefined1TP :: forall opts ip op fmt i ip1 op1 fmt1 i1 m .
  ( Refined1C opts ip1 op1 fmt1 i1
  , Monad m
  , Show (PP ip i)
  , PP ip i ~ PP ip1 i1
  )
  => Proxy '(opts, ip, op, fmt, i)
  -> Proxy '(opts, ip1, op1, fmt1, i1)
  -> RefinedT m (Refined1 opts ip op fmt i)
  -> RefinedT m (Refined1 opts ip1 op1 fmt1 i1)
convertRefined1TP _ _ ma = do
  Refined1 x <- ma
  -- we skip the input value @Id and go straight to the internal value so PP fmt (PP ip i) /= i for this call
  Refined1 a <- newRefined1TPSkipIPImpl (return . runIdentity) (Proxy @'(opts, ip1, op1, fmt1, i1)) x
  return (Refined1 a)

-- | applies a binary operation to two wrapped 'Refined1' parameters
rapply1 :: forall opts ip op fmt i m .
  ( Refined1C opts ip op fmt i
  , Monad m
  , Show (PP ip i)
  )
  => (PP ip i -> PP ip i -> PP ip i)
  -> RefinedT m (Refined1 opts ip op fmt i)
  -> RefinedT m (Refined1 opts ip op fmt i)
  -> RefinedT m (Refined1 opts ip op fmt i)
rapply1 = rapply1P (Proxy @'(opts,ip,op,fmt,i))

-- prtRefinedTIO $ rapply1P base16 (+) (newRefined1TP Proxy "ff") (newRefined1TP Proxy "22")

-- | same as 'rapply1' but uses a 5-tuple proxy instead
rapply1P :: forall opts ip op fmt i proxy m .
  ( Refined1C opts ip op fmt i
  , Monad m
  , Show (PP ip i)
  )
  => proxy '(opts,ip,op,fmt,i)
  -> (PP ip i -> PP ip i -> PP ip i)
  -> RefinedT m (Refined1 opts ip op fmt i)
  -> RefinedT m (Refined1 opts ip op fmt i)
  -> RefinedT m (Refined1 opts ip op fmt i)
rapply1P p f ma mb = do
  let opts = getOpt @opts
  tell [setOtherEffects opts "=== a ==="]
  Refined1 x <- ma
  tell [setOtherEffects opts "=== b ==="]
  Refined1 y <- mb
  -- we skip the input value @Id and go straight to the internal value so PP fmt (PP ip i) /= i for this call
  tell [setOtherEffects opts "=== a `op` b ==="]
  newRefined1TPSkipIPImpl (return . runIdentity) p (f x y)

-- | An ADT that summarises the results of evaluating Refined1 representing all possible states
data RResults1 a =
       RF !String !(Tree PE)        -- Left e
     | RTF !a !(Tree PE) !String !(Tree PE)    -- Right a + Left e
     | RTFalse !a !(Tree PE) !(Tree PE)        -- Right a + Right False
     | RTTrueF !a !(Tree PE) !(Tree PE) !String !(Tree PE) -- Right a + Right True + Left e
     | RTTrueT !a !(Tree PE) !(Tree PE) !(Tree PE)      -- Right a + Right True + Right b
     deriving Show


-- | same as 'newRefined3P'' but passes in the proxy
newRefined1' :: forall opts ip op fmt i m
  . ( MonadEval m
    , Refined1C opts ip op fmt i
    , Show (PP ip i)
    )
  => i
  -> m (Either Msg1 (Refined1 opts ip op fmt i))
newRefined1' = newRefined1P' Proxy

-- | same as 'newRefined1P' but runs in IO
newRefined1P' :: forall opts ip op fmt i proxy m
  . ( MonadEval m
    , Refined1C opts ip op fmt i
    , Show (PP ip i)
    )
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> m (Either Msg1 (Refined1 opts ip op fmt i))
newRefined1P' _ i = do
  (ret,mr)<- eval1M i
  return $ maybe (Left $ prt1Impl (getOpt @opts) ret) Right mr

-- | same as 'newRefined1P' but skips the proxy and allows you to set each parameter individually using type application
-- | pure version for extracting Refined1
--
-- >>> newRefined1 @OZ @(ReadBase Int 16 Id) @(Lt 255) @(PrintF "%x" Id) "00fe"
-- Right (Refined1 254)
--
-- >>> newRefined1 @OZ @(ReadBase Int 16 Id) @(Lt 253) @(PrintF "%x" Id) "00fe"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> newRefined1 @OZ @(ReadBase Int 16 Id) @(Lt 255) @(PrintF "%x" Id) "00fg"
-- Left Step 1. Initial Conversion(ip) Failed | invalid base 16
--
-- >>> newRefined1 @OL @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Msg "length invalid:" (Len == 4)) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) "198.162.3.1.5"
-- Left Step 2. False Boolean Check(op) | {length invalid: 5 == 4}
--
-- >>> newRefined1 @OZ @(Map (ReadP Int Id) (Resplit "\\." Id)) @(GuardBool (PrintF "found length=%d" Len) (Len == 4)) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) "198.162.3.1.5"
-- Left Step 2. Failed Boolean Check(op) | found length=5
--
-- >>> newRefined1 @OZ @(Map (ReadP Int Id) (Resplit "\\." Id)) @(GuardBool (PrintF "found length=%d" Len) (Len == 4)) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) "198.162.3.1"
-- Right (Refined1 [198,162,3,1])
--
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> newRefined1 @OZ @(MkDayExtra Id >> 'Just Id) @(GuardBool "expected a Sunday" (Thd Id == 7)) @(UnMkDay (Fst Id)) (2019,10,13)
-- Right (Refined1 (2019-10-13,41,7))
--
-- >>> newRefined1 @OL @(MkDayExtra Id >> 'Just Id) @(Msg "expected a Sunday:" (Thd Id == 7)) @(UnMkDay (Fst Id)) (2019,10,12)
-- Left Step 2. False Boolean Check(op) | {expected a Sunday: 6 == 7}
--
-- >>> newRefined1 @OZ @(MkDayExtra' (Fst Id) (Snd Id) (Thd Id) >> 'Just Id) @(GuardBool "expected a Sunday" (Thd Id == 7)) @(UnMkDay (Fst Id)) (2019,10,12)
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
-- >>> newRefined1 @OL @(ParseTimeP TimeOfDay "%-H:%-M:%-S" Id) @'True @(FormatTimeP "%H:%M:%S" Id) "1:15:7"
-- Right (Refined1 01:15:07)
--
-- >>> newRefined1 @OL @(ParseTimeP TimeOfDay "%-H:%-M:%-S" Id) @'True @(FormatTimeP "%H:%M:%S" Id) "1:2:x"
-- Left Step 1. Initial Conversion(ip) Failed | ParseTimeP TimeOfDay (%-H:%-M:%-S) failed to parse
--
-- >>> newRefined1 @OL @(Rescan "^(\\d{1,2}):(\\d{1,2}):(\\d{1,2})$" Id >> Snd (Head Id) >> Map (ReadP Int Id) Id) @(All (0 <..> 59) Id && Len == 3) @(PrintL 3 "%02d:%02d:%02d" Id) "1:2:3"
-- Right (Refined1 [1,2,3])
--
newRefined1 :: forall opts ip op fmt i
  . ( Refined1C opts ip op fmt i
    , Show (PP ip i)
    )
  => i
  -> Either Msg1 (Refined1 opts ip op fmt i)
newRefined1 = newRefined1P Proxy

-- | create a Refined1 using a 5-tuple proxy and aggregate the results on failure
--
-- >>> type T4 k = '(OZ, MkDayExtra Id >> 'Just Id, GuardBool "expected a Sunday" (Thd Id == 7), UnMkDay (Fst Id), k)
-- >>> newRefined1P (Proxy @(T4 _)) (2019,10,12)
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
-- >>> newRefined1P (Proxy @(T4 _)) (2019,10,13)
-- Right (Refined1 (2019-10-13,41,7))
--
newRefined1P :: forall opts ip op fmt i proxy
  . ( Refined1C opts ip op fmt i
    , Show (PP ip i)
    )
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> Either Msg1 (Refined1 opts ip op fmt i)
newRefined1P _ i =
  let (ret,mr) = eval1 i
  in maybe (Left $ prt1Impl (getOpt @opts) ret) Right mr

-- | create a Refined1 value using a 5-tuple proxy (see 'mkProxy1')
--
-- use 'mkProxy1' to package all the types together as a 5-tuple
--
eval1P :: forall opts ip op fmt i proxy . Refined1C opts ip op fmt i
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> (RResults1 (PP ip i), Maybe (Refined1 opts ip op fmt i))
eval1P _ = runIdentity . eval1M

-- | same as 'eval1P' but can pass the parameters individually using type application
eval1 :: forall opts ip op fmt i . Refined1C opts ip op fmt i
  => i
  -> (RResults1 (PP ip i), Maybe (Refined1 opts ip op fmt i))
eval1 = eval1P Proxy

eval1M :: forall opts ip op fmt i m . (MonadEval m, Refined1C opts ip op fmt i)
  => i
  -> m (RResults1 (PP ip i), Maybe (Refined1 opts ip op fmt i))
eval1M i = do
  let o = getOpt @opts
  ll <- eval (Proxy @ip) o i
  case getValAndPE ll of
   (Right a, t1) -> do
     rr <- evalBool (Proxy @op) o a
     case getValAndPE rr of
      (Right True,t2) -> do
        ss <- eval (Proxy @fmt) o a
        pure $ case getValAndPE ss of
         (Right _b,t3) -> (RTTrueT a t1 t2 t3, Just (Refined1 a))
         (Left e,t3) -> (RTTrueF a t1 t2 e t3, Nothing)
      (Right False,t2) -> pure (RTFalse a t1 t2, Nothing)
      (Left e,t2) -> pure (RTF a t1 e t2, Nothing)
   (Left e,t1) -> pure (RF e t1, Nothing)

-- | creates Refined1 value but skips the initial conversion
eval1MSkip :: forall opts ip op fmt i m . (MonadEval m, Refined1C opts ip op fmt i)
   => PP ip i
   -> m (RResults1 (PP ip i), Maybe (Refined1 opts ip op fmt i))
eval1MSkip a = do
   let o = getOpt @opts
   rr <- evalBool (Proxy @op) o a
   case getValAndPE rr of
    (Right True,t2) -> do
      ss <- eval (Proxy @fmt) o a
      pure $ case getValAndPE ss of
       (Right _b,t3) -> (RTTrueT a mkNodeSkipP t2 t3, Just (Refined1 a))
       (Left e,t3) -> (RTTrueF a mkNodeSkipP t2 e t3, Nothing)
    (Right False,t2) -> pure (RTFalse a mkNodeSkipP t2, Nothing)
    (Left e,t2) -> pure (RTF a mkNodeSkipP e t2, Nothing)

data Msg1 = Msg1 { m1Desc :: !String
                 , m1Short :: !String
                 , m1Long :: !String
                 } deriving Eq

instance Show Msg1 where
  show (Msg1 a b c) = a <> nullIf " | " b <> nullIf "\n" c

prt1Impl :: forall a . Show a
  => POpts
  -> RResults1 a
  -> Msg1
prt1Impl opts v =
  let outmsg msg = "*** " <> formatOMsg opts " " <> msg <> " ***\n"
      msg1 a = outmsg ("Step 1. Success Initial Conversion(ip) (" ++ showL opts a ++ ")")
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
             z = let w = t2 ^. root . pString
                 in if all isSpace w then "FalseP" else "{" <> w <> "}"
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
       RTTrueT a t1 t2 t3 ->
         let (m,n) = ("Step 3. Success Output Conversion(fmt)", "")
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg "Step 2. Success Boolean Check(op)"
              <> prtTreePure opts t2
              <> outmsg m
              <> fixLite opts () t3
         in mkMsg1 m n r

replaceOpt1 :: forall (opt :: Opt) opt0 ip op fmt i . Refined1 opt0 ip op fmt i -> Refined1 opt ip op fmt i
replaceOpt1 = coerce

appendOpt1 :: forall (opt :: Opt) opt0 ip op fmt i . Refined1 opt0 ip op fmt i -> Refined1 (opt0 ':# opt) ip op fmt i
appendOpt1 = coerce
