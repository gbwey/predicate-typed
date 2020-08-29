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
{- |
     Refinement type allowing the external type to differ from the internal type
     see 'Refined2'
-}
module Predicate.Refined2 (

  -- ** Refined2
    Refined2(r2In,r2Out)
  , Refined2C

 -- ** display results
  , prtEval2
  , prtEval2P
  , prtEval2IO
  , prtEval2PIO
  , prt2IO
  , prt2Impl
  , Msg2 (..)
  , RResults2 (..)

  -- ** evaluation methods
  , eval2
  , eval2P
  , eval2M
  , newRefined2
  , newRefined2P

  -- ** create a wrapped Refined2 value
  , newRefined2T
  , newRefined2TP
  , newRefined2TIO
  , withRefined2T
  , withRefined2TP
  , withRefined2TIO

  -- ** proxy methods
  , MakeR2
  , mkProxy2
  , mkProxy2'

  -- ** QuickCheck methods
  , genRefined2
  , genRefined2P

  -- ** unsafe methods for creating Refined2
  , unsafeRefined2
  , unsafeRefined2'

  , type ReplaceOptT2
  , type AppendOptT2

 ) where
import Predicate.Refined
import Predicate.Core
import Predicate.Util
import Data.Tree
import Data.Proxy
import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.Writer (tell)
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Language.Haskell.TH.Syntax as TH
import qualified GHC.Read as GR
import qualified Text.ParserCombinators.ReadPrec as PCR
import qualified Text.Read.Lex as RL
import qualified Data.Binary as B
import Data.Binary (Binary)
import Data.Maybe (fromMaybe, isJust)
import Control.Lens
import Data.Tree.Lens (root)
import Data.Char (isSpace)
import Data.String
import Data.Hashable (Hashable(..))
import GHC.Stack
import Test.QuickCheck

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :m + Predicate.Prelude
-- >>> :m + Data.Time

-- | Refinement type for specifying an input type that is different from the output type
--
--   * @opts@ are the display options
--   * @ip@ converts @i@ to @PP ip i@ which is the internal type in 'r2In'
--   * @op@ validates that internal type using @PP op (PP ip i) ~ Bool@
--   * @i@ is the input type which is stored in 'r2Out'
--
-- Although a common scenario is String as input, you are free to choose any input type you like
--
-- >>> newRefined2 @OZ @(ReadBase Int 16 Id) @(Lt 255) "00fe"
-- Right (Refined2 {r2In = 254, r2Out = "00fe"})
--
-- >>> newRefined2 @OZ @(ReadBase Int 16 Id) @(Lt 253) "00fe"
-- Left "Step 2. False Boolean Check(op) | FalseP"
--
-- >>> newRefined2 @OZ @(ReadBase Int 16 Id) @(Lt 255) "00fg"
-- Left "Step 1. Initial Conversion(ip) Failed | invalid base 16"
--
-- >>> newRefined2 @OL @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Msg "length invalid:" (Len == 4)) "198.162.3.1.5"
-- Left "Step 2. False Boolean Check(op) | {length invalid: 5 == 4}"
--
-- >>> newRefined2 @OZ @(Map (ReadP Int Id) (Resplit "\\." Id)) @(GuardBool (PrintF "found length=%d" Len) (Len == 4)) "198.162.3.1.5"
-- Left "Step 2. Failed Boolean Check(op) | found length=5"
--
-- >>> newRefined2 @OZ @(Map (ReadP Int Id) (Resplit "\\." Id)) @(GuardBool (PrintF "found length=%d" Len) (Len == 4)) "198.162.3.1"
-- Right (Refined2 {r2In = [198,162,3,1], r2Out = "198.162.3.1"})
--
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> newRefined2 @OZ @(MkDayExtra Id >> 'Just Id) @(GuardBool "expected a Sunday" (Thd Id == 7)) (2019,10,13)
-- Right (Refined2 {r2In = (2019-10-13,41,7), r2Out = (2019,10,13)})
--
-- >>> newRefined2 @OL @(MkDayExtra Id >> 'Just Id) @(Msg "expected a Sunday:" (Thd Id == 7)) (2019,10,12)
-- Left "Step 2. False Boolean Check(op) | {expected a Sunday: 6 == 7}"
--
-- >>> newRefined2 @OZ @(MkDayExtra' (Fst Id) (Snd Id) (Thd Id) >> 'Just Id) @(GuardBool "expected a Sunday" (Thd Id == 7)) (2019,10,12)
-- Left "Step 2. Failed Boolean Check(op) | expected a Sunday"
--
data Refined2 (opts :: Opt) ip op i = Refined2 { r2In :: !(PP ip i), r2Out :: !i }

type role Refined2 nominal nominal nominal nominal

-- | directly load values into 'Refined2'. It still checks to see that those values are valid
unsafeRefined2' :: forall opts ip op i
                . ( Show (PP ip i)
                  , Refined2C opts ip op i
                  , HasCallStack
                  )
                => i
                -> Refined2 opts ip op i
unsafeRefined2' i =
  let (ret,mr) = eval2 @opts @ip @op i
  in fromMaybe (error $ show (prt2Impl (getOpt @opts) ret)) mr

-- | directly load values into 'Refined2' without any checking
unsafeRefined2 :: forall opts ip op i
   . PP ip i
  -> i
  -> Refined2 opts ip op i
unsafeRefined2 = Refined2

-- | Provides the constraints on Refined2
type Refined2C opts ip op i =
       ( OptC opts
       , P ip i
       , P op (PP ip i)
       , PP op (PP ip i) ~ Bool   -- the internal value needs to pass the predicate check
       )

deriving instance (Show i, Show (PP ip i)) => Show (Refined2 opts ip op i)
deriving instance (Eq i, Eq (PP ip i)) => Eq (Refined2 opts ip op i)
deriving instance (TH.Lift (PP ip i), TH.Lift i) => TH.Lift (Refined2 opts ip op i)

-- | 'IsString' instance for Refined2
--
-- >>> pureTryTest $ fromString @(Refined2 OL (ReadP Int Id) (Id > 12) String) "523"
-- Right (Refined2 {r2In = 523, r2Out = "523"})
--
-- >>> pureTryTest $ fromString @(Refined2 OL (ReadP Int Id) (Id > 12) String) "2"
-- Left ()
--
instance ( s ~ String
         , Refined2C opts ip op s
         , Show (PP ip s)
         ) => IsString (Refined2 opts ip op s) where
  fromString s =
    let (ret,mr) = eval2 @opts @ip @op s
    in fromMaybe (error $ "Refined2(fromString):" ++ show (prt2Impl (getOpt @opts) ret)) mr

-- read instance from -ddump-deriv
-- | 'Read' instance for 'Refined2'
--
-- >>> reads @(Refined2 OZ (ReadBase Int 16 Id) (Between 0 255 Id) String) "Refined2 {r2In = 254, r2Out = \"fe\"}"
-- [(Refined2 {r2In = 254, r2Out = "fe"},"")]
--
-- >>> reads @(Refined2 OZ (ReadBase Int 16 Id) (Between 0 255 Id) String) "Refined2 {r2In = 300, r2Out = \"12c\"}"
-- []
--
-- >>> reads @(Refined2 OZ (ReadBase Int 16 Id) (Id < 0) String) "Refined2 {r2In = -1234, r2Out = \"-4d2\"}"
-- [(Refined2 {r2In = -1234, r2Out = "-4d2"},"")]
--
-- >>> reads @(Refined2 OZ (Map (ReadP Int Id) (Resplit "\\." Id)) (GuardBool "len/=4" (Len == 4)) String) "Refined2 {r2In = [192,168,0,1], r2Out = \"192.168.0.1\"}"
-- [(Refined2 {r2In = [192,168,0,1], r2Out = "192.168.0.1"},"")]
--
instance ( Eq i
         , Show i
         , Show (PP ip i)
         , Refined2C opts ip op i
         , Read (PP ip i)
         , Read i
         ) => Read (Refined2 opts ip op i) where
    readPrec
      = GR.parens
          (PCR.prec
             11
             (do GR.expectP (RL.Ident "Refined2")
                 GR.expectP (RL.Punc "{")
                 fld1 <- readField
                               "r2In" (PCR.reset GR.readPrec)
                 GR.expectP (RL.Punc ",")
                 fld2 <- readField
                               "r2Out" (PCR.reset GR.readPrec)
                 GR.expectP (RL.Punc "}")

                 let lr = evalQuick @op (getOpt @opts) fld1

                 case lr of
                   Left {} -> fail ""
                   Right True -> pure (Refined2 fld1 fld2)
                   Right False -> fail ""
             ))
    readList = GR.readListDefault
    readListPrec = GR.readListPrecDefault

-- | 'ToJSON' instance for 'Refined2'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.encode (unsafeRefined2 @OZ @(ReadBase Int 16 Id) @(Between 0 255 Id) 254 "fe")
-- "\"fe\""
--
-- >>> A.encode (unsafeRefined2 @OZ @Id @'True @Int 123 123)
-- "123"
--
instance ToJSON i => ToJSON (Refined2 opts ip op i) where
  toJSON = toJSON . r2Out


-- | 'FromJSON' instance for 'Refined2'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode' @(Refined2 OZ (ReadBase Int 16 Id) (Id > 10 && Id < 256) String) "\"00fe\""
-- Right (Refined2 {r2In = 254, r2Out = "00fe"})
--
-- >>> removeAnsi $ A.eitherDecode' @(Refined2 OAN (ReadBase Int 16 Id) (Id > 10 && Id < 256) String) "\"00fe443a\""
-- Error in $: Refined2:Step 2. False Boolean Check(op) | {True && False | (16663610 < 256)}
-- <BLANKLINE>
-- *** Step 1. Success Initial Conversion(ip) (16663610) ***
-- <BLANKLINE>
-- P ReadBase(Int,16) 16663610
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
instance ( Show i
         , Show (PP ip i)
         , Refined2C opts ip op i
         , FromJSON i
         ) => FromJSON (Refined2 opts ip op i) where
  parseJSON z = do
                  i <- parseJSON @i z
                  let (ret,mr) = eval2 @opts @ip @op i
                  case mr of
                    Nothing -> fail $ "Refined2:" ++ show (prt2Impl (getOpt @opts) ret)
                    Just r -> return r

-- | 'Arbitrary' instance for 'Refined2'
--
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> xs <- generate (vectorOf 10 (arbitrary @(Refined2 OU (ToEnum Day Id) (Snd (ToWeekDate Id) == "Tuesday") Int)))
-- >>> all (\x -> let y = toEnum @Day (r2Out x) in view _3 (toWeekDate y) == 2 && r2In x == y) xs
-- True
--
instance ( Arbitrary i
         , Refined2C opts ip op i
         , Show (PP ip i)
         ) => Arbitrary (Refined2 opts ip op i) where
  arbitrary = genRefined2 arbitrary

-- | create a 'Refined2' generator using a generator to restrict the values (so it completes)
--
-- >>> g = genRefined2 @OU @(ToEnum Day Id) @(UnMkDay Id >> Snd Id == 10) arbitrary
-- >>> xs <- generate (vectorOf 10 g)
-- >>> all (\x -> let y = toEnum @Day (fromIntegral (r2Out x)) in view _2 (toGregorian y) == 10 && y == r2In x) xs
-- True
--
genRefined2 ::
    forall opts ip op i
  . ( Refined2C opts ip op i
    , Show (PP ip i)
    )
  => Gen i
  -> Gen (Refined2 opts ip op i)
genRefined2 = genRefined2P Proxy

-- generates the external value unlike Refined3 as we dont have a way to recreate the output from the internal value
-- | create a 'Refined2' generator using a proxy
genRefined2P ::
    forall opts ip op i
  . ( Refined2C opts ip op i
    , Show (PP ip i)
    )
  => Proxy '(opts,ip,op,i)
  -> Gen i
  -> Gen (Refined2 opts ip op i)
genRefined2P _ g =
  let o = getOpt @opts
      f !cnt = do
        mi <- suchThatMaybe g (isJust . snd . eval2 @opts @ip @op)
        case mi of
          Nothing ->
             if cnt >= oRecursion o
             then error $ setOtherEffects o ("genRefined2P recursion exceeded(" ++ show (oRecursion o) ++ ")")
             else f (cnt+1)
          Just i -> do
             let lr = newRefined2 @opts @ip @op i
             case lr of
               Left e -> error $ "conversion failed: programming error failed!! " ++ e
               Right r -> pure r
  in f 0

-- | 'Binary' instance for 'Refined2'
--
-- >>> import Control.Arrow ((+++))
-- >>> import Data.Time
-- >>> type K1 = Refined2 OAN (ReadP Day Id) 'True String
-- >>> type K2 = Refined2 OAN (ReadP Day Id) (Between (ReadP Day "2019-05-30") (ReadP Day "2019-06-01") Id) String
-- >>> r = unsafeRefined2' "2019-04-23" :: K1
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K1 (B.encode r)
-- Refined2 {r2In = 2019-04-23, r2Out = "2019-04-23"}
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K2 (B.encode r)
-- Refined2:Step 2. False Boolean Check(op) | {2019-05-30 <= 2019-04-23}
-- <BLANKLINE>
-- *** Step 1. Success Initial Conversion(ip) (2019-04-23) ***
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
-- |  `- P '"2019-05-30"
-- |
-- `- P ReadP Day 2019-06-01
--    |
--    `- P '"2019-06-01"
-- <BLANKLINE>
--
instance ( Refined2C opts ip op i
         , Show i
         , Show (PP ip i)
         , Binary i
         ) => Binary (Refined2 opts ip op i) where
  get = do
          i <- B.get @i
          let (ret,mr) = eval2 @opts @ip @op i
          case mr of
            Nothing -> fail $ "Refined2:" ++ show (prt2Impl (getOpt @opts) ret)
            Just r -> return r
  put (Refined2 _ r) = B.put @i r

-- | 'Hashable' instance for 'Refined2'
instance (Refined2C opts ip op i
        , Hashable i
        ) => Hashable (Refined2 opts ip op i) where
  hashWithSalt s (Refined2 _ b) = s + hash b

-- | same as 'withRefined2T' for IO
withRefined2TIO :: forall opts ip op i m b
  . ( MonadIO m
    , Refined2C opts ip op i
    , Show (PP ip i)
    )
  => i
  -> (Refined2 opts ip op i -> RefinedT m b)
  -> RefinedT m b
withRefined2TIO = (>>=) . newRefined2TIO @opts @ip @op @i

-- | create a 'Refined2' value using a continuation
--
-- This first example reads a hex string and makes sure it is between 100 and 200 and then
-- reads a binary string and adds the values together
--
-- >>> :set -XPolyKinds
-- >>> prtRefinedTIO $ withRefined2T @OZ @(ReadBase Int 16 Id) @(Between 100 200 Id) "a3" $ \x -> withRefined2T @OZ @(ReadBase Int 2 Id) @'True "1001110111" $ \y -> pure (r2In x + r2In y)
-- 794
--
-- this example fails as the the hex value is out of range
--
-- >>> prtRefinedTIO $ withRefined2T @OAN @(ReadBase Int 16 Id) @(Between 100 200 Id) "a388" $ \x -> withRefined2T @OAN @(ReadBase Int 2 Id) @'True "1001110111" $ \y -> pure (x,y)
-- <BLANKLINE>
-- *** Step 1. Success Initial Conversion(ip) (41864) ***
-- <BLANKLINE>
-- P ReadBase(Int,16) 41864
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
withRefined2T :: forall opts ip op i m b
  . ( Monad m
    , Refined2C opts ip op i
    , Show (PP ip i)
    )
  => i
  -> (Refined2 opts ip op i -> RefinedT m b)
  -> RefinedT m b
withRefined2T = (>>=) . newRefined2TP (Proxy @'(opts,ip,op,i))

withRefined2TP :: forall opts ip op i b proxy m
  . ( Monad m
    , Refined2C opts ip op i
    , Show (PP ip i)
    )
  => proxy '(opts,ip,op,i)
  -> i
  -> (Refined2 opts ip op i -> RefinedT m b)
  -> RefinedT m b
withRefined2TP p = (>>=) . newRefined2TP p

-- | pure version for extracting Refined2
--
-- >>> newRefined2 @OL @Id @'True 22
-- Right (Refined2 {r2In = 22, r2Out = 22})
--
-- >>> newRefined2 @OL @(ReadP UTCTime Id) @(Between (MkDay '(2020,5,2)) (MkDay '(2020,5,7)) (MkJust (ToDay Id))) "2020-05-04 12:13:14Z"
-- Right (Refined2 {r2In = 2020-05-04 12:13:14 UTC, r2Out = "2020-05-04 12:13:14Z"})
--
-- >>> newRefined2 @OL @(ReadP UTCTime Id) @(Between (MkDay '(2020,5,2)) (MkDay '(2020,5,7)) (MkJust (ToDay Id))) "2020-05-08 12:13:14Z"
-- Left "Step 2. False Boolean Check(op) | {Just 2020-05-08 <= Just 2020-05-07}"
--
newRefined2 :: forall opts ip op i
   . ( Refined2C opts ip op i
     , Show (PP ip i)
    ) => i
      -> Either String (Refined2 opts ip op i)
newRefined2 = newRefined2P Proxy

newRefined2P :: forall opts ip op i proxy
   . ( Refined2C opts ip op i
     , Show (PP ip i)
    ) => proxy '(opts,ip,op,i)
      -> i
      -> Either String (Refined2 opts ip op i)
newRefined2P _ x =
  let (lr,xs) = runIdentity $ unRavelT $ newRefined2T @opts @ip @op x
  in left (\e -> e ++ (if all null xs then "" else "\n" ++ unlines xs)) lr

-- | create a wrapped 'Refined2' type
--
-- >>> prtRefinedTIO $ newRefined2T @OL @(MkDayExtra Id >> 'Just Id) @(Thd Id == 5) (2019,11,1)
-- Refined2 {r2In = (2019-11-01,44,5), r2Out = (2019,11,1)}
--
-- >>> prtRefinedTIO $ newRefined2T @OL @(MkDayExtra Id >> 'Just Id) @(Thd Id == 5) (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {6 == 5}]
--
-- >>> prtRefinedTIO $ newRefined2T @OL @(MkDayExtra Id >> 'Just Id) @(Msg "wrong day:" (Thd Id == 5)) (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {wrong day: 6 == 5}]
--
-- >>> prtRefinedTIO $ newRefined2TIO @OL @(Hide (Rescan "(\\d+)" Id >> ConcatMap (Snd Id) Id) >> Map (ReadP Int Id) Id) @(Len > 0 && All (0 <..> 0xff) Id) "|23|99|255|254.911."
-- failure msg[Step 2. False Boolean Check(op) | {True && False | (All(5) i=4 (911 <= 255))}]
--
newRefined2T :: forall opts ip op i m
   . ( Refined2C opts ip op i
     , Monad m
     , Show (PP ip i)
    ) => i
      -> RefinedT m (Refined2 opts ip op i)
newRefined2T = newRefined2TImpl (return . runIdentity)

-- | create a wrapped 'Refined2' type with an explicit proxy
newRefined2TP :: forall opts ip op i proxy m
   . ( Refined2C opts ip op i
     , Monad m
     , Show (PP ip i)
   ) => proxy '(opts,ip,op,i)
  -> i
  -> RefinedT m (Refined2 opts ip op i)
newRefined2TP _ = newRefined2TImpl (return . runIdentity)

-- | create a wrapped 'Refined2' type in IO
newRefined2TIO :: forall opts ip op i m
   . ( Refined2C opts ip op i
     , MonadIO m
     , Show (PP ip i)
    ) => i
      -> RefinedT m (Refined2 opts ip op i)
newRefined2TIO = newRefined2TImpl @IO @m liftIO

newRefined2TImpl :: forall n m opts ip op i
   . ( Refined2C opts ip op i
     , Monad m
     , MonadEval n
     , Show (PP ip i)
   ) => (forall x . n x -> RefinedT m x)
   -> i
   -> RefinedT m (Refined2 opts ip op i)
newRefined2TImpl f i = do
  (ret,mr) <- f $ eval2M i
  let m2 = prt2Impl (getOpt @opts) ret
  tell [m2Long m2]
  case mr of
    Nothing -> throwError $ m2Desc m2 <> " | " <> m2Short m2
    Just r -> return r

-- | An ADT that summarises the results of evaluating Refined2 representing all possible states
data RResults2 a =
       RF !String !(Tree PE)        -- fails initial conversion
     | RTF !a !(Tree PE) !String !(Tree PE)    -- op fails
     | RTFalse !a !(Tree PE) !(Tree PE)        -- op false
     | RTTrue !a !(Tree PE) !(Tree PE) -- op true
     deriving Show

-- | same as 'prtEval2PIO' without a proxy for used with TypeApplications
prtEval2IO :: forall opts ip op i
  . ( Refined2C opts ip op i
    , Show (PP ip i)
    ) => i
  -> IO (Either String (Refined2 opts ip op i))
prtEval2IO = prtEval2PIO Proxy

-- | same as 'prtEval2P' but runs in IO
prtEval2PIO :: forall opts ip op i proxy
  . ( Refined2C opts ip op i
    , Show (PP ip i)
    ) => proxy '(opts,ip,op,i)
  -> i
  -> IO (Either String (Refined2 opts ip op i))
prtEval2PIO _ i = do
  x <- eval2M @opts @ip @op i
  prt2IO @opts x

prtEval2 :: forall opts ip op i
  . ( Refined2C opts ip op i
    , Show (PP ip i)
  ) => i
    -> Either Msg2 (Refined2 opts ip op i)
prtEval2 = prtEval2P Proxy

prtEval2P :: forall opts ip op i
  . ( Refined2C opts ip op i
    , Show (PP ip i)
  ) => Proxy '(opts,ip,op,i)
    -> i
    -> Either Msg2 (Refined2 opts ip op i)
prtEval2P _ i =
  let (ret,mr) = eval2 i
  in maybe (Left $ prt2Impl (getOpt @opts) ret) Right mr

eval2P :: forall opts ip op i
  . ( Refined2C opts ip op i
    )
  => Proxy '(opts,ip,op,i)
  -> i
  -> (RResults2 (PP ip i), Maybe (Refined2 opts ip op i))
eval2P _ = runIdentity . eval2M

eval2 :: forall opts ip op i
   . ( Refined2C opts ip op i
     )
  => i
  -> (RResults2 (PP ip i), Maybe (Refined2 opts ip op i))
eval2 = runIdentity . eval2M

eval2M :: forall opts ip op i m
  . ( MonadEval m
    , Refined2C opts ip op i
    )
  => i
  -> m (RResults2 (PP ip i), Maybe (Refined2 opts ip op i))
eval2M i = do
  let o = getOpt @opts
  ll <- eval (Proxy @ip) o i
  case getValAndPE ll of
   (Right a, t1) -> do
     rr <- evalBool (Proxy @op) o a
     pure $ case getValAndPE rr of
      (Right True,t2) -> (RTTrue a t1 t2, Just (Refined2 a i))
      (Right False,t2) -> (RTFalse a t1 t2, Nothing)
      (Left e,t2) -> (RTF a t1 e t2, Nothing)
   (Left e,t1) -> pure (RF e t1, Nothing)

prt2IO :: forall opts a r . (OptC opts, Show a) => (RResults2 a, Maybe r) -> IO (Either String r)
prt2IO (ret,mr) = do
  let m2 = prt2Impl o ret
      o = getOpt @opts
  unless (hasNoTree o) $ putStrLn $ m2Long m2
  return $ maybe (Left (m2Desc m2 <> " | " <> m2Short m2)) Right mr

data Msg2 = Msg2 { m2Desc :: !String
                 , m2Short :: !String
                 , m2Long :: !String
                 } deriving Eq

instance Show Msg2 where
  show (Msg2 a b c) = a <> " | " <> b <> (if null c then "" else "\n" <> c)

prt2Impl :: forall a . Show a
  => POpts
  -> RResults2 a
  -> Msg2
prt2Impl opts v =
  let outmsg msg = "\n*** " <> formatOMsg opts " " <> msg <> " ***\n\n"
      msg1 a = outmsg ("Step 1. Success Initial Conversion(ip) (" ++ showL opts a ++ ")")
      mkMsg2 m n r | hasNoTree opts = Msg2 m n ""
                   | otherwise = Msg2 m n r
  in case v of
       RF e t1 ->
         let (m,n) = ("Step 1. Initial Conversion(ip) Failed", e)
             r = outmsg m
              <> prtTreePure opts t1
         in mkMsg2 m n r
       RTF a t1 e t2 ->
         let (m,n) = ("Step 2. Failed Boolean Check(op)", e)
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg m
              <> prtTreePure opts t2
         in mkMsg2 m n r
       RTFalse a t1 t2 ->
         let (m,n) = ("Step 2. False Boolean Check(op)", z)
             z = let w = t2 ^. root . pString
                 in if all isSpace w then "FalseP" else "{" <> w <> "}"
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg m
              <> prtTreePure opts t2
         in mkMsg2 m n r
       RTTrue a t1 t2 ->
         let (m,n) = ("Step 2. True Boolean Check(op)", "")
             r = msg1 a
              <> fixLite opts a t1
              <> outmsg m
              <> prtTreePure opts t2
         in mkMsg2 m n r

-- | creates a 4-tuple proxy (see 'withRefined2TP' 'newRefined2TP' 'eval2P' 'prtEval2P')
--
-- use type application to set the 4-tuple or set the individual parameters directly
--
-- set the 4-tuple directly
--
-- >>> eg1 = mkProxy2 @'( OL, ReadP Int Id, Gt 10, String)
-- >>> newRefined2P eg1 "24"
-- Right (Refined2 {r2In = 24, r2Out = "24"})
--
-- skip the 4-tuple and set each parameter individually using type application
--
-- >>> eg2 = mkProxy2 @_ @OL @(ReadP Int Id) @(Gt 10)
-- >>> newRefined2P eg2 "24"
-- Right (Refined2 {r2In = 24, r2Out = "24"})
--
mkProxy2 :: forall z opts ip op i
  . ( z ~ '(opts,ip,op,i)
    ) => Proxy '(opts,ip,op,i)
mkProxy2 = Proxy

-- | same as 'mkProxy2' but checks to make sure the proxy is consistent with the 'Refined2C' constraint
mkProxy2' :: forall z opts ip op i
  . ( z ~ '(ip,op,i)
    , Refined2C opts ip op i
    ) => Proxy '(opts,ip,op,i)
mkProxy2' = Proxy

-- | type family for converting from a 4-tuple '(opts,ip,op,i) to a 'Refined2' type
type family MakeR2 p where
  MakeR2 '(opts,ip,op,i) = Refined2 opts ip op i

type family ReplaceOptT2 (o :: Opt) t where
  ReplaceOptT2 o (Refined2 _ ip op i) = Refined2 o ip op i

type family AppendOptT2 (o :: Opt) t where
  AppendOptT2 o (Refined2 o' ip op i) = Refined2 (o' ':# o) ip op i
