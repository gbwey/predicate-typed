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
-- see 'Refined3'
--
-- @
-- similar to 'Predicate.Refined2.Refined2' but also provides:
-- * quickCheck methods
-- * ability to combine refinement types
-- * a canonical output value using the \'fmt\' parameter
-- @
--
module Predicate.Refined3 (

  -- ** Refined3
    Refined3(r3In,r3Out)
  , Refined3C

 -- ** display results
  , prtEval3
  , prtEval3P
  , prtEval3IO
  , prtEval3PIO
  , prt3IO
  , prt3Impl
  , Msg3 (..)
  , RResults3 (..)

  -- ** evaluation methods
  , eval3
  , eval3P
  , eval3M
  , newRefined3
  , newRefined3P

  -- ** create a wrapped Refined3 value
  , newRefined3T
  , newRefined3TP
  , newRefined3TPIO
  , withRefined3T
  , withRefined3TIO
  , withRefined3TP

  -- ** proxy methods
  , mkProxy3
  , mkProxy3'
  , MakeR3
  , MakeR3'

  -- ** unsafe methods for creating Refined3
  , unsafeRefined3
  , unsafeRefined3'

  -- ** combine Refined3 values
  , convertRefined3TP
  , rapply3
  , rapply3P

  -- ** QuickCheck methods
  , genRefined3
  , genRefined3P

  -- ** emulate Refined3 using Refined
  , RefinedEmulate
  , eval3PX
  , eval3X

  , type ReplaceOptT3
  , type AppendOptT3

 ) where
import Predicate.Refined
import Predicate.Core
import Predicate.Util
import Data.Functor.Identity (Identity(..))
import Data.Tree
import Data.Proxy
import Control.Arrow (left)
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
import Control.Lens ((^.))
import Data.Tree.Lens (root)
import Data.Char (isSpace)
import Data.String
import Data.Hashable (Hashable(..))
import GHC.Stack

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :m + Predicate.Prelude
-- >>> :m + Data.Time

-- | Like 'Predicate.Refined2' but additionally reconstructs the output value to a standardized format
--
--   * @opts@ are the display options
--   * @ip@ converts @i@ to @PP ip i@ which is the internal type and stored in 'r3In'
--   * @op@ validates that internal type using @PP op (PP ip i) ~ Bool@
--   * @fmt@ outputs the internal type @PP fmt (PP ip i) ~ i@ and stored in 'r3Out'
--   * @i@ is the input type
--
--   * @PP fmt (PP ip i)@ should be valid as input for Refined3
--
-- Setting @ip@ to @Id@ and @fmt@ to @Id@ is equivalent to 'Refined.Refined': see 'RefinedEmulate'
--
-- Setting the input type @i@ to 'GHC.Base.String' resembles the corresponding Read/Show instances but with an additional predicate on the read value
--
--   * __read__ a string using /ip/ into an internal type and store in 'r3In'
--   * __validate__ 'r3In' using the predicate /op/
--   * __show__ 'r3In' using /fmt/ and store that formatted result in 'r3Out'
--
-- Although a common scenario is String as input, you are free to choose any input type you like
--
-- >>> newRefined3 @'OZ @(ReadBase Int 16 Id) @(Lt 255) @(PrintF "%x" Id) "00fe"
-- Right (Refined3 {r3In = 254, r3Out = "fe"})
--
-- >>> newRefined3 @'OZ @(ReadBase Int 16 Id) @(Lt 253) @(PrintF "%x" Id) "00fe"
-- Left "Step 2. False Boolean Check(op) | FalseP"
--
-- >>> newRefined3 @'OZ @(ReadBase Int 16 Id) @(Lt 255) @(PrintF "%x" Id) "00fg"
-- Left "Step 1. Initial Conversion(ip) Failed | invalid base 16"
--
-- >>> newRefined3 @'OL @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Msg "length invalid:" (Len == 4)) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) "198.162.3.1.5"
-- Left "Step 2. False Boolean Check(op) | {length invalid:5 == 4}"
--
-- >>> newRefined3 @'OZ @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Guard (PrintF "found length=%d" Len) (Len == 4) >> 'True) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) "198.162.3.1.5"
-- Left "Step 2. Failed Boolean Check(op) | found length=5"
--
-- >>> newRefined3 @'OZ @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Guard (PrintF "found length=%d" Len) (Len == 4) >> 'True) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) "198.162.3.1"
-- Right (Refined3 {r3In = [198,162,3,1], r3Out = "198.162.003.001"})
--
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> newRefined3 @'OZ @(MkDayExtra Id >> 'Just Id) @(Guard "expected a Sunday" (Thd Id == 7) >> 'True) @(UnMkDay (Fst Id)) (2019,10,13)
-- Right (Refined3 {r3In = (2019-10-13,41,7), r3Out = (2019,10,13)})
--
-- >>> newRefined3 @'OL @(MkDayExtra Id >> 'Just Id) @(Msg "expected a Sunday:" (Thd Id == 7)) @(UnMkDay (Fst Id)) (2019,10,12)
-- Left "Step 2. False Boolean Check(op) | {expected a Sunday:6 == 7}"
--
-- >>> newRefined3 @'OZ @(MkDayExtra' (Fst Id) (Snd Id) (Thd Id) >> 'Just Id) @(Guard "expected a Sunday" (Thd Id == 7) >> 'True) @(UnMkDay (Fst Id)) (2019,10,12)
-- Left "Step 2. Failed Boolean Check(op) | expected a Sunday"
--
-- >>> type T4 k = '( 'OZ, MkDayExtra Id >> 'Just Id, Guard "expected a Sunday" (Thd Id == 7) >> 'True, UnMkDay (Fst Id), k)
-- >>> newRefined3P (Proxy @(T4 _)) (2019,10,12)
-- Left "Step 2. Failed Boolean Check(op) | expected a Sunday"
--
-- >>> newRefined3P (Proxy @(T4 _)) (2019,10,13)
-- Right (Refined3 {r3In = (2019-10-13,41,7), r3Out = (2019,10,13)})
--
data Refined3 (opts :: OptT) ip op fmt i = Refined3 { r3In :: !(PP ip i), r3Out :: !(PP fmt (PP ip i)) }

type role Refined3 nominal nominal nominal nominal nominal

-- | directly load values into 'Refined3'. It still checks to see that those values are valid
unsafeRefined3' :: forall opts ip op fmt i
                . ( HasCallStack
                  , Show i
                  , Show (PP ip i)
                  , Refined3C opts ip op fmt i)
                => i
                -> Refined3 opts ip op fmt i
unsafeRefined3' i =
  let (ret,mr) = eval3 @opts @ip @op @fmt i
  in case mr of
       Nothing -> error $ show (prt3Impl (getOptT @opts) ret)
       Just r -> r

-- | directly load values into 'Refined3' without any checking
unsafeRefined3 ::
    forall opts ip op fmt i
  . PP ip i
  -> PP fmt (PP ip i)
  -> Refined3 opts ip op fmt i
unsafeRefined3 = Refined3


-- | Provides the constraints on Refined3
type Refined3C opts ip op fmt i =
       ( OptTC opts
       , P ip i
       , P op (PP ip i)
       , PP op (PP ip i) ~ Bool   -- the internal value needs to pass the predicate check
       , P fmt (PP ip i)
       , PP fmt (PP ip i) ~ i  -- the output type must match the original input type
       )

deriving instance ( Show i
                  , Show (PP ip i)
                  , Show (PP fmt (PP ip i))
                  ) => Show (Refined3 opts ip op fmt i)
deriving instance ( Eq i
                  , Eq (PP ip i)
                  , Eq (PP fmt (PP ip i))
                  ) => Eq (Refined3 opts ip op fmt i)
deriving instance ( TH.Lift (PP ip i)
                  , TH.Lift (PP fmt (PP ip i))
                  ) => TH.Lift (Refined3 opts ip op fmt i)

-- | 'IsString' instance for Refined3
--
-- >>> pureTryTest $ fromString @(Refined3 'OL (ReadP Int Id) (Id > 12) (ShowP Id) String) "523"
-- Right (Refined3 {r3In = 523, r3Out = "523"})
--
-- >>> pureTryTest $ fromString @(Refined3 'OL (ReadP Int Id) (Id > 12) (ShowP Id) String) "2"
-- Left ()
--
instance (Refined3C opts ip op fmt String, Show (PP ip String))
  => IsString (Refined3 opts ip op fmt String) where
  fromString s =
    let (ret,mr) = eval3 @opts @ip @op @fmt s
    in case mr of
         Nothing -> error $ "Refined3(fromString):" ++ show (prt3Impl (getOptT @opts) ret)
         Just r -> r

-- read instance from -ddump-deriv
-- | 'Read' instance for 'Refined3'
--
-- >>> reads @(Refined3 'OZ (ReadBase Int 16 Id) (Between 0 255 Id) (ShowBase 16 Id) String) "Refined3 {r3In = 254, r3Out = \"fe\"}"
-- [(Refined3 {r3In = 254, r3Out = "fe"},"")]
--
-- >>> reads @(Refined3 'OZ (ReadBase Int 16 Id) (Between 0 255 Id) (ShowBase 16 Id) String) "Refined3 {r3In = 300, r3Out = \"12c\"}"
-- []
--
-- >>> reads @(Refined3 'OZ (ReadBase Int 16 Id) (Id < 0) (ShowBase 16 Id) String) "Refined3 {r3In = -1234, r3Out = \"-4d2\"}"
-- [(Refined3 {r3In = -1234, r3Out = "-4d2"},"")]
--
-- >>> reads @(Refined3 'OZ (Map (ReadP Int Id) (Resplit "\\." Id)) (Guard "len/=4" (Len == 4) >> 'True) (PrintL 4 "%d.%d.%d.%d" Id) String) "Refined3 {r3In = [192,168,0,1], r3Out = \"192.168.0.1\"}"
-- [(Refined3 {r3In = [192,168,0,1], r3Out = "192.168.0.1"},"")]
--
instance ( Eq i
         , Show i
         , Show (PP ip i)
         , Refined3C opts ip op fmt i
         , Read (PP ip i)
         , Read (PP fmt (PP ip i))
         ) => Read (Refined3 opts ip op fmt i) where
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

                 let (_ret,mr) = runIdentity $ eval3MSkip @_ @opts @ip @op @fmt fld1
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
-- >>> A.encode (unsafeRefined3 @'OZ @(ReadBase Int 16 Id) @(Between 0 255 Id) @(ShowBase 16 Id) 254 "fe")
-- "\"fe\""
--
-- >>> A.encode (unsafeRefined3 @'OZ @Id @'True @Id 123 123)
-- "123"
--
instance ToJSON (PP fmt (PP ip i)) => ToJSON (Refined3 opts ip op fmt i) where
  toJSON = toJSON . r3Out


-- | 'FromJSON' instance for 'Refined3'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode' @(Refined3 'OZ (ReadBase Int 16 Id) (Id > 10 && Id < 256) (ShowBase 16 Id) String) "\"00fe\""
-- Right (Refined3 {r3In = 254, r3Out = "fe"})
--
-- >>> removeAnsi $ A.eitherDecode' @(Refined3 'OAN (ReadBase Int 16 Id) (Id > 10 && Id < 256) (ShowBase 16 Id) String) "\"00fe443a\""
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
instance (Show (PP fmt (PP ip i))
        , Show (PP ip i)
        , Refined3C opts ip op fmt i
        , FromJSON i
        ) => FromJSON (Refined3 opts ip op fmt i) where
  parseJSON z = do
                  i <- parseJSON @i z
                  let (ret,mr) = eval3 @opts @ip @op @fmt i
                  case mr of
                    Nothing -> fail $ "Refined3:" ++ show (prt3Impl (getOptT @opts) ret)
                    Just r -> return r

-- | 'Arbitrary' instance for 'Refined3'
--
-- >>> xs <- generate (vectorOf 10 (arbitrary @(Refined3 'OU (ReadP Int Id) (1 <..> 120 && Even) (ShowP Id) String)))
-- >>> all (\x -> let y = r3In x in y /= 0 && r3Out x == show y) xs
-- True
--
instance ( Arbitrary (PP ip i)
         , Refined3C opts ip op fmt i
         ) => Arbitrary (Refined3 opts ip op fmt i) where
  arbitrary = genRefined3 arbitrary

-- | create a 'Refined3' generator using a generator to restrict the values (so it completes)
--
-- >>> g = genRefined3 @'OU @(ReadP Int Id) @(Between 10 100 Id && Even) @(ShowP Id) (choose (10,100))
-- >>> xs <- generate (vectorOf 10 g)
-- >>> all (\x -> let y = r3In x in y >= 0 && y <= 100 && even y) xs
-- True
--
genRefined3 ::
    forall opts ip op fmt i
  . Refined3C opts ip op fmt i
  => Gen (PP ip i)
  -> Gen (Refined3 opts ip op fmt i)
genRefined3 = genRefined3P Proxy

-- | create a 'Refined3' generator using a proxy
genRefined3P ::
    forall opts ip op fmt i
  . Refined3C opts ip op fmt i
  => Proxy '(opts,ip,op,fmt,i)
  -> Gen (PP ip i)
  -> Gen (Refined3 opts ip op fmt i)
genRefined3P _ g =
  let o = getOptT @opts
      f !cnt = do
        mppi <- suchThatMaybe g (\a -> getValLRFromTT (runIdentity (eval @_ (Proxy @op) o a)) == Right True)
        case mppi of
          Nothing ->
             if cnt >= oRecursion o
             then error $ setOtherEffects o ("genRefined3P recursion exceeded(" ++ show (oRecursion o) ++ ")")
             else f (cnt+1)
          Just ppi -> do
             let lr = getValLRFromTT (runIdentity (eval @_ (Proxy @fmt) o ppi))
             case lr of
               Left e -> error $ "formatting failed!! " ++ e
               Right r -> pure $ unsafeRefined3 ppi r
  in f 0

-- | 'Binary' instance for 'Refined3'
--
-- >>> import Control.Arrow ((+++))
-- >>> import Control.Lens
-- >>> import Data.Time
-- >>> type K1 = MakeR3 '( 'OAN, ReadP Day Id, 'True, ShowP Id, String)
-- >>> type K2 = MakeR3 '( 'OAN, ReadP Day Id, Between (ReadP Day "2019-05-30") (ReadP Day "2019-06-01") Id, ShowP Id, String)
-- >>> r = unsafeRefined3' "2019-04-23" :: K1
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K1 (B.encode r)
-- Refined3 {r3In = 2019-04-23, r3Out = "2019-04-23"}
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K2 (B.encode r)
-- Refined3:Step 2. False Boolean Check(op) | {2019-05-30 <= 2019-04-23}
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
         , Refined3C opts ip op fmt i
         , Binary i
         ) => Binary (Refined3 opts ip op fmt i) where
  get = do
          i <- B.get @i
          let (ret,mr) = eval3 @opts @ip @op @fmt i
          case mr of
            Nothing -> fail $ "Refined3:" ++ show (prt3Impl (getOptT @opts) ret)
            Just r -> return r
  put (Refined3 _ r) = B.put @i r

-- | 'Hashable' instance for 'Refined3'
instance (Refined3C opts ip op fmt i
        , Hashable (PP ip i)
        ) => Hashable (Refined3 opts ip op fmt i) where
  hashWithSalt s (Refined3 a _) = s + hash a

-- | creates a 5-tuple proxy (see 'withRefined3TP' 'newRefined3TP' 'eval3P' 'prtEval3P')
--
-- use type application to set the 5-tuple or set the individual parameters directly
--
-- set the 5-tuple directly
--
-- >>> eg1 = mkProxy3 @'( 'OL, ReadP Int Id, Gt 10, ShowP Id, String)
-- >>> newRefined3P eg1 "24"
-- Right (Refined3 {r3In = 24, r3Out = "24"})
--
-- skip the 5-tuple and set each parameter individually using type application
--
-- >>> eg2 = mkProxy3 @_ @'OL @(ReadP Int Id) @(Gt 10) @(ShowP Id)
-- >>> newRefined3P eg2 "24"
-- Right (Refined3 {r3In = 24, r3Out = "24"})
--
mkProxy3 ::
  forall z opts ip op fmt i
       . z ~ '(opts,ip,op,fmt,i)
      => Proxy '(opts,ip,op,fmt,i)
mkProxy3 = Proxy

-- | same as 'mkProxy3' but checks to make sure the proxy is consistent with the 'Refined3C' constraint
mkProxy3' :: forall z opts ip op fmt i . (z ~ '(opts,ip,op,fmt,i), Refined3C opts ip op fmt i) => Proxy '(opts,ip,op,fmt,i)
mkProxy3' = Proxy

-- | type family for converting from a 5-tuple '(opts,ip,op,fmt,i) to a 'Refined3' type
type family MakeR3 p where
  MakeR3 '(opts,ip,op,fmt,i) = Refined3 opts ip op fmt i

type family MakeR3' opts p where
  MakeR3' opts '(ip,op,fmt,i) = Refined3 opts ip op fmt i

withRefined3TIO :: forall opts ip op fmt i m b
  . ( MonadIO m
    , Refined3C opts ip op fmt i
    , Show (PP ip i)
    , Show i
    )
  => i
  -> (Refined3 opts ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined3TIO = (>>=) . newRefined3TPIO (Proxy @'(opts,ip,op,fmt,i))

-- | create a 'Refined3' value using a continuation
--
-- This first example reads a hex string and makes sure it is between 100 and 200 and then
-- reads a binary string and adds the values together
--
-- >>> :set -XPolyKinds
-- >>> :set -XRankNTypes
-- >>> b16 :: forall opts . Proxy '( opts, ReadBase Int 16 Id, Between 100 200 Id, ShowBase 16 Id, String); b16 = Proxy
-- >>> b2 :: forall opts . Proxy '( opts, ReadBase Int 2 Id, 'True, ShowBase 2 Id, String); b2 = Proxy
-- >>> prtRefinedTIO $ withRefined3TP (b16 @'OZ) "a3" $ \x -> withRefined3TP (b2 @'OZ) "1001110111" $ \y -> pure (r3In x + r3In y)
-- 794
--
-- this example fails as the the hex value is out of range
--
-- >>> prtRefinedTIO $ withRefined3TP (b16 @'OAN) "a388" $ \x -> withRefined3TP (b2 @'OAN) "1001110111" $ \y -> pure (x,y)
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
withRefined3T :: forall opts ip op fmt i m b
  . ( Monad m
    , Refined3C opts ip op fmt i
    , Show (PP ip i)
    , Show i
    )
  => i
  -> (Refined3 opts ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined3T = (>>=) . newRefined3TP (Proxy @'(opts,ip,op,fmt,i))

withRefined3TP :: forall m opts ip op fmt i b proxy
  . ( Monad m
    , Refined3C opts ip op fmt i
    , Show (PP ip i)
    , Show i
    )
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> (Refined3 opts ip op fmt i -> RefinedT m b)
  -> RefinedT m b
withRefined3TP p = (>>=) . newRefined3TP p

-- | pure version for extracting Refined3
--
-- >>> newRefined3 @'OL @(ParseTimeP TimeOfDay "%-H:%-M:%-S" Id) @'True @(FormatTimeP "%H:%M:%S" Id) "1:15:7"
-- Right (Refined3 {r3In = 01:15:07, r3Out = "01:15:07"})
--
-- >>> newRefined3 @'OL @(ParseTimeP TimeOfDay "%-H:%-M:%-S" Id) @'True @(FormatTimeP "%H:%M:%S" Id) "1:2:x"
-- Left "Step 1. Initial Conversion(ip) Failed | ParseTimeP TimeOfDay (%-H:%-M:%-S) failed to parse"
--
-- >>> newRefined3 @'OL @(Rescan "^(\\d{1,2}):(\\d{1,2}):(\\d{1,2})$" Id >> Snd (Head Id) >> Map (ReadP Int Id) Id) @(All (0 <..> 59) Id && Len == 3) @(PrintL 3 "%02d:%02d:%02d" Id) "1:2:3"
-- Right (Refined3 {r3In = [1,2,3], r3Out = "01:02:03"})
--
newRefined3 :: forall opts ip op fmt i
  . ( Refined3C opts ip op fmt i
    , Show (PP ip i)
    , Show i
    )
   => i
   -> Either String (Refined3 opts ip op fmt i)
newRefined3 = newRefined3P Proxy

newRefined3P :: forall opts ip op fmt i proxy
  . ( Refined3C opts ip op fmt i
    , Show (PP ip i)
    , Show i
    )
   => proxy '(opts,ip,op,fmt,i)
   -> i
   -> Either String (Refined3 opts ip op fmt i)
newRefined3P _ x =
  let (lr,xs) = runIdentity $ unRavelT $ newRefined3T @_ @opts @ip @op @fmt x
  in left (\e -> e ++ (if all null xs then "" else "\n" ++ unlines xs)) lr

-- | create a wrapped 'Refined3' type
--
-- >>> prtRefinedTIO $ newRefined3T @_ @'OZ @(MkDayExtra Id >> Just Id) @(GuardSimple (Thd Id == 5) >> 'True) @(UnMkDay (Fst Id)) (2019,11,1)
-- Refined3 {r3In = (2019-11-01,44,5), r3Out = (2019,11,1)}
--
-- >>> prtRefinedTIO $ newRefined3T @_ @'OL @(MkDayExtra Id >> Just Id) @(Thd Id == 5) @(UnMkDay (Fst Id)) (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {6 == 5}]
--
-- >>> prtRefinedTIO $ newRefined3T @_ @'OL @(MkDayExtra Id >> Just Id) @(Msg "wrong day:" (Thd Id == 5)) @(UnMkDay (Fst Id)) (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {wrong day:6 == 5}]
--
newRefined3T :: forall m opts ip op fmt i
  . ( Refined3C opts ip op fmt i
    , Monad m
    , Show (PP ip i)
    , Show i
    )
   => i
   -> RefinedT m (Refined3 opts ip op fmt i)
newRefined3T = newRefined3TP (Proxy @'(opts,ip,op,fmt,i))

-- | create a wrapped 'Refined3' type
--
-- >>> prtRefinedTIO $ newRefined3TP (Proxy @'( 'OZ, MkDayExtra Id >> Just Id, GuardSimple (Thd Id == 5) >> 'True, UnMkDay (Fst Id), (Int,Int,Int))) (2019,11,1)
-- Refined3 {r3In = (2019-11-01,44,5), r3Out = (2019,11,1)}
--
-- >>> prtRefinedTIO $ newRefined3TP (Proxy @'( 'OL, MkDayExtra Id >> Just Id, Thd Id == 5, UnMkDay (Fst Id), (Int,Int,Int))) (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {6 == 5}]
--
-- >>> prtRefinedTIO $ newRefined3TP (Proxy @'( 'OL, MkDayExtra Id >> Just Id, Msg "wrong day:" (Thd Id == 5), UnMkDay (Fst Id), (Int,Int,Int))) (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {wrong day:6 == 5}]
--
newRefined3TP :: forall m opts ip op fmt i proxy
   . ( Refined3C opts ip op fmt i
     , Monad m
     , Show (PP ip i)
     , Show i
     )
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> RefinedT m (Refined3 opts ip op fmt i)
newRefined3TP = newRefined3TPImpl (return . runIdentity)

newRefined3TPIO :: forall m opts ip op fmt i proxy
   . ( Refined3C opts ip op fmt i
     , MonadIO m
     , Show (PP ip i)
     , Show i)
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> RefinedT m (Refined3 opts ip op fmt i)
newRefined3TPIO = newRefined3TPImpl liftIO

newRefined3TPImpl :: forall n m opts ip op fmt i proxy
   . ( Refined3C opts ip op fmt i
     , Monad m
     , MonadEval n
     , Show (PP ip i)
     , Show (PP fmt (PP ip i)))
  => (forall x . n x -> RefinedT m x)
   -> proxy '(opts,ip,op,fmt,i)
   -> i
   -> RefinedT m (Refined3 opts ip op fmt i)
newRefined3TPImpl f _ i = do
  (ret,mr) <- f $ eval3M  i
  let m3 = prt3Impl (getOptT @opts) ret
  tell [m3Long m3]
  case mr of
    Nothing -> throwError $ m3Desc m3 <> " | " <> m3Short m3
    Just r -> return r

newRefined3TPSkipIPImpl :: forall n m opts ip op fmt i proxy
   . ( Refined3C opts ip op fmt i
     , Monad m
     , MonadEval n
     , Show (PP ip i)
     , Show (PP fmt (PP ip i))
     )
  => (forall x . n x -> RefinedT m x)
   -> proxy '(opts,ip,op,fmt,i)
   -> PP ip i
   -> RefinedT m (Refined3 opts ip op fmt i)
newRefined3TPSkipIPImpl f _ a = do
  (ret,mr) <- f $ eval3MSkip a
  let m3 = prt3Impl (getOptT @opts) ret
  tell [m3Long m3]
  case mr of
    Nothing -> throwError $ m3Desc m3 <> " | " <> m3Short m3
    Just r -> return r

-- | attempts to cast a wrapped 'Refined3' to another 'Refined3' with different predicates
convertRefined3TP :: forall m opts ip op fmt i ip1 op1 fmt1 i1 .
  ( Refined3C opts ip op fmt i
  , Refined3C opts ip1 op1 fmt1 i1
  , Monad m
  , Show (PP ip i)
  , PP ip i ~ PP ip1 i1
  , Show i1)
  => Proxy '(opts, ip, op, fmt, i)
  -> Proxy '(opts, ip1, op1, fmt1, i1)
  -> RefinedT m (Refined3 opts ip op fmt i)
  -> RefinedT m (Refined3 opts ip1 op1 fmt1 i1)
convertRefined3TP _ _ ma = do
  Refined3 x _ <- ma
  -- we skip the input value @Id and go straight to the internal value so PP fmt (PP ip i) /= i for this call
  Refined3 a b <- newRefined3TPSkipIPImpl (return . runIdentity) (Proxy @'(opts, ip1, op1, fmt1, i1)) x
  return (Refined3 a b)

-- | applies a binary operation to two wrapped 'Refined3' parameters
rapply3 :: forall m opts ip op fmt i .
  ( Refined3C opts ip op fmt i
  , Monad m
  , Show (PP ip i)
  , Show i)
  => (PP ip i -> PP ip i -> PP ip i)
  -> RefinedT m (Refined3 opts ip op fmt i)
  -> RefinedT m (Refined3 opts ip op fmt i)
  -> RefinedT m (Refined3 opts ip op fmt i)
rapply3 = rapply3P (Proxy @'(opts,ip,op,fmt,i))

-- prtRefinedTIO $ rapply3P base16 (+) (newRefined3TP Proxy "ff") (newRefined3TP Proxy "22")

-- | same as 'rapply3' but uses a 5-tuple proxy instead
rapply3P :: forall m opts ip op fmt i proxy .
  ( Refined3C opts ip op fmt i
  , Monad m
  , Show (PP ip i)
  , Show i)
  => proxy '(opts,ip,op,fmt,i)
  -> (PP ip i -> PP ip i -> PP ip i)
  -> RefinedT m (Refined3 opts ip op fmt i)
  -> RefinedT m (Refined3 opts ip op fmt i)
  -> RefinedT m (Refined3 opts ip op fmt i)
rapply3P p f ma mb = do
  let opts = getOptT @opts
  tell [setOtherEffects opts "=== a ==="]
  Refined3 x _ <- ma
  tell [setOtherEffects opts "=== b ==="]
  Refined3 y _ <- mb
  -- we skip the input value @Id and go straight to the internal value so PP fmt (PP ip i) /= i for this call
  tell [setOtherEffects opts "=== a `op` b ==="]
  Refined3 a b <- newRefined3TPSkipIPImpl (return . runIdentity) p (f x y)
  return (Refined3 a b)

-- | An ADT that summarises the results of evaluating Refined3 representing all possible states
data RResults3 a b =
       RF !String !(Tree PE)        -- Left e
     | RTF !a !(Tree PE) !String !(Tree PE)    -- Right a + Left e
     | RTFalse !a !(Tree PE) !(Tree PE)        -- Right a + Right False
     | RTTrueF !a !(Tree PE) !(Tree PE) !String !(Tree PE) -- Right a + Right True + Left e
     | RTTrueT !a !(Tree PE) !(Tree PE) !b !(Tree PE)      -- Right a + Right True + Right b
     deriving Show

-- | same as 'prtEval3PIO' but passes in the proxy
prtEval3IO :: forall opts ip op fmt i
  . ( Refined3C opts ip op fmt i
    , Show (PP ip i)
    , Show i)
  => i
  -> IO (Either String (Refined3 opts ip op fmt i))
prtEval3IO = prtEval3PIO Proxy

-- | same as 'prtEval3P' but runs in IO
prtEval3PIO :: forall opts ip op fmt i proxy
  . ( Refined3C opts ip op fmt i
    , Show (PP ip i)
    , Show i)
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> IO (Either String (Refined3 opts ip op fmt i))
prtEval3PIO _ i = do
  x <- eval3M i
  prt3IO @opts x

-- | same as 'prtEval3P' but skips the proxy and allows you to set each parameter individually using type application
prtEval3 :: forall opts ip op fmt i
  . ( Refined3C opts ip op fmt i
    , Show (PP ip i)
    , Show i)
  => i
  -> Either Msg3 (Refined3 opts ip op fmt i)
prtEval3 = prtEval3P Proxy

-- | create a Refined3 using a 5-tuple proxy and aggregate the results on failure
prtEval3P :: forall opts ip op fmt i proxy
  . ( Refined3C opts ip op fmt i
    , Show (PP ip i)
    , Show i)
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> Either Msg3 (Refined3 opts ip op fmt i)
prtEval3P _ i =
  let (ret,mr) = eval3 i
  in maybe (Left $ prt3Impl (getOptT @opts) ret) Right mr

-- | same as 'eval3P' but can pass the parameters individually using type application
eval3 :: forall opts ip op fmt i
  . ( Refined3C opts ip op fmt i
     )
  => i
  -> (RResults3 (PP ip i) (PP fmt (PP ip i)), Maybe (Refined3 opts ip op fmt i))
eval3 = eval3P Proxy

-- | create a Refined3 value using a 5-tuple proxy (see 'mkProxy3')
--
-- use 'mkProxy3' to package all the types together as a 5-tuple
--
eval3P :: forall opts ip op fmt i proxy
   . ( Refined3C opts ip op fmt i
     )
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> (RResults3 (PP ip i) (PP fmt (PP ip i)), Maybe (Refined3 opts ip op fmt i))
eval3P _ = runIdentity . eval3M

eval3M :: forall m opts ip op fmt i
  . ( MonadEval m
    , Refined3C opts ip op fmt i
    )
  => i
  -> m (RResults3 (PP ip i) (PP fmt (PP ip i)), Maybe (Refined3 opts ip op fmt i))
eval3M i = do
  let o = getOptT @opts
  ll <- eval (Proxy @ip) o i
  case getValAndPE ll of
   (Right a, t1) -> do
     rr <- evalBool (Proxy @op) o a
     case getValAndPE rr of
      (Right True,t2) -> do
        ss <- eval (Proxy @fmt) o a
        pure $ case getValAndPE ss of
         (Right b,t3) -> (RTTrueT a t1 t2 b t3, Just (Refined3 a b))
         (Left e,t3) -> (RTTrueF a t1 t2 e t3, Nothing)
      (Right False,t2) -> pure (RTFalse a t1 t2, Nothing)
      (Left e,t2) -> pure (RTF a t1 e t2, Nothing)
   (Left e,t1) -> pure (RF e t1, Nothing)

-- | creates Refined3 value but skips the initial conversion
eval3MSkip :: forall m opts ip op fmt i
  . ( MonadEval m
    , Refined3C opts ip op fmt i
    )
   => PP ip i
   -> m (RResults3 (PP ip i) (PP fmt (PP ip i)), Maybe (Refined3 opts ip op fmt i))
eval3MSkip a = do
   let o = getOptT @opts
   rr <- evalBool (Proxy @op) o a
   case getValAndPE rr of
    (Right True,t2) -> do
      ss <- eval (Proxy @fmt) o a
      pure $ case getValAndPE ss of
       (Right b,t3) -> (RTTrueT a mkNodeSkipP t2 b t3, Just (Refined3 a b))
       (Left e,t3) -> (RTTrueF a mkNodeSkipP t2 e t3, Nothing)
    (Right False,t2) -> pure (RTFalse a mkNodeSkipP t2, Nothing)
    (Left e,t2) -> pure (RTF a mkNodeSkipP e t2, Nothing)

prt3IO :: forall opts a b r .
     (OptTC opts, Show a, Show b)
  => (RResults3 a b, Maybe r)
  -> IO (Either String r)
prt3IO (ret,mr) = do
  let m3 = prt3Impl (getOptT @opts) ret
  unless (hasNoTree (getOptT @opts)) $ putStrLn $ m3Long m3
  return $ maybe (Left (m3Desc m3 <> " | " <> m3Short m3)) Right mr

data Msg3 = Msg3 { m3Desc :: !String
                 , m3Short :: !String
                 , m3Long :: !String
                 } deriving Eq

instance Show Msg3 where
  show (Msg3 a b c) = a <> " | " <> b <> (if null c then "" else "\n" <> c)

prt3Impl :: forall a b . (Show a, Show b)
  => POpts
  -> RResults3 a b
  -> Msg3
prt3Impl opts v =
  let outmsg msg = "\n*** " <> formatOMsg opts " " <> msg <> " ***\n\n"
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
             z = let w = t2 ^. root . pString
                 in if all isSpace w then "FalseP" else "{" <> w <> "}"
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

-- | similar to 'eval3P' but it emulates 'Refined3' using 'Refined'
--
-- takes a 5-tuple proxy as input but outputs the Refined value and the result separately
--
-- * initial conversion using \'ip\' and stores that in 'Refined'
-- * runs the boolean predicate \'op\' to make sure to validate the converted value from 1.
-- * runs \'fmt\' against the converted value from 1.
-- * returns both the 'Refined' and the output from 3.
-- * if any of the above steps fail the process stops it and dumps out 'RResults3'
--
eval3PX :: forall opts ip op fmt i proxy
  . Refined3C opts ip op fmt i
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> (RResults3 (PP ip i) (PP fmt (PP ip i)), Maybe (Refined opts op (PP ip i), PP fmt (PP ip i)))
eval3PX _ i = runIdentity $ do
  let o = getOptT @opts
  ll <- eval (Proxy @ip) o i
  case getValAndPE ll of
    (Right a,t1) -> do
      rr <- evalBool (Proxy @op) o a
      case getValAndPE rr of
        (Right True,t2) -> do
          ss <- eval (Proxy @fmt) o a
          pure $ case getValAndPE ss of
            (Right b,t3) -> (RTTrueT a t1 t2 b t3, Just (unsafeRefined a, b))
            (Left e,t3) -> (RTTrueF a t1 t2 e t3, Nothing)
        (Right False,t2) -> pure (RTFalse a t1 t2, Nothing)
        (Left e,t2) -> pure (RTF a t1 e t2, Nothing)
    (Left e,t1) -> pure (RF e t1, Nothing)

-- | same as 'eval3PX' but allows you to set the parameters individually using type application
eval3X :: forall opts ip op fmt i
   . ( Refined3C opts ip op fmt i
     )
  => i
  -> (RResults3 (PP ip i) (PP fmt (PP ip i)), Maybe (Refined opts op (PP ip i), PP fmt (PP ip i)))
eval3X = eval3PX (Proxy @'(opts,ip,op,fmt,i))

-- | emulates 'Refined' using 'Refined3' by setting the input conversion and output formatting as noops
type RefinedEmulate (opts :: OptT) p a = Refined3 opts Id p Id a

-- | replace the opts type
type family ReplaceOptT3 (o :: OptT) t where
  ReplaceOptT3 o (Refined3 _ ip op fmt i) = Refined3 o ip op fmt i

-- | change the opts type
type family AppendOptT3 (o :: OptT) t where
  AppendOptT3 o (Refined3 o' ip op fmt i) = Refined3 (o' ':# o) ip op fmt i

