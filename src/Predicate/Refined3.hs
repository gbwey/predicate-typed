-- arbitrary and hash use the internal value!
-- binary and json use the external value
{-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- * a canonical output value using the \'fmt\' parameter
-- @
--
module Predicate.Refined3 (

  -- ** Refined3
    Refined3
  , r3In
  , r3Out
  , Refined3C

 -- ** display results
  , Msg3 (..)
  , RResults3 (..)

  -- ** evaluation methods
  , eval3P
  , eval3M
  , newRefined3
  , newRefined3'
  , newRefined3P
  , newRefined3P'

  -- ** proxy methods
  , mkProxy3
  , mkProxy3'
  , MakeR3

  -- ** unsafe methods for creating Refined3
  , unsafeRefined3
  , unsafeRefined3'

  -- ** QuickCheck methods
  , genRefined3
  , genRefined3P

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Data.Functor.Identity (Identity(..))
import Data.Tree (Tree(..))
import Data.Proxy (Proxy(..))
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
import Data.String (IsString(..))
import Data.Hashable (Hashable(..))
import GHC.Stack (HasCallStack)
import Control.DeepSeq (rnf, rnf2, NFData)
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
-- Setting @ip@ to @Id@ and @fmt@ to @Id@ is equivalent to 'Refined.Refined'
--
-- Setting the input type @i@ to 'GHC.Base.String' resembles the corresponding Read/Show instances but with an additional predicate on the read value
--
--   * __read__ a string using /ip/ into an internal type and store in 'r3In'
--   * __validate__ 'r3In' using the predicate /op/
--   * __show__ 'r3In' using /fmt/ and store that formatted result in 'r3Out'
--
-- Although a common scenario is String as input, you are free to choose any input type you like
--
data Refined3 (opts :: Opt) ip op fmt i = Refined3 !(PP ip i) !i

type role Refined3 phantom nominal nominal nominal nominal

-- | field accessor for the converted input value in 'Refined3'
r3In :: Refined3 opts ip op fmt i -> PP ip i
r3In (Refined3 ppi _) = ppi

-- | field accessor for the converted output value in 'Refined3'
r3Out :: Refined3 opts ip op fmt i -> i
r3Out (Refined3 _ i) = i


-- | directly load values into 'Refined3'. It still checks to see that those values are valid
unsafeRefined3' :: forall opts ip op fmt i
                . ( HasCallStack
                  , Show (PP ip i)
                  , Refined3C opts ip op fmt i
                ) => i
                  -> Refined3 opts ip op fmt i
unsafeRefined3' = either (error . show) id . newRefined3

-- | directly load values into 'Refined3' without any checking
unsafeRefined3 ::
    forall opts ip op fmt i
  .  Refined3C opts ip op fmt i
  => PP ip i
  -> i
  -> Refined3 opts ip op fmt i
unsafeRefined3 = Refined3


-- | Provides the constraints on Refined3
type Refined3C opts ip op fmt i =
       ( OptC opts
       , P ip i
       , P op (PP ip i)
       , PP op (PP ip i) ~ Bool   -- the internal value needs to pass the predicate check
       , P fmt (PP ip i)
       , PP fmt (PP ip i) ~ i  -- the output type must match the original input type
       )

deriving instance ( Refined3C opts ip op fmt i
                  , Show (PP ip i)
                  , Show i
                  ) => Show (Refined3 opts ip op fmt i)
deriving instance ( Refined3C opts ip op fmt i
                  , Eq (PP ip i)
                  , Eq i
                  ) => Eq (Refined3 opts ip op fmt i)
deriving instance ( Refined3C opts ip op fmt i
                  , Ord (PP ip i)
                  , Ord i
                  ) => Ord (Refined3 opts ip op fmt i)
deriving instance ( Refined3C opts ip op fmt i
                  , TH.Lift (PP ip i)
                  , TH.Lift i
                  ) => TH.Lift (Refined3 opts ip op fmt i)

instance ( Refined3C opts ip op fmt i
         , NFData i
         , NFData (PP ip i)
         ) => NFData (Refined3 opts ip op fmt i) where
  rnf (Refined3 a b) = rnf2 (a,b)

-- | 'IsString' instance for Refined3
--
-- >>> pureTryTest $ fromString @(Refined3 OL (ReadP Int Id) (Id > 12) (ShowP Id) String) "523"
-- Right (Refined3 523 "523")
--
-- >>> pureTryTest $ fromString @(Refined3 OL (ReadP Int Id) (Id > 12) (ShowP Id) String) "2"
-- Left ()
--
instance ( Refined3C opts ip op fmt String
         , Show (PP ip String)
         ) => IsString (Refined3 opts ip op fmt String) where
  fromString s =
    case newRefined3 s of
      Left e -> error $ "Refined3(IsString:fromString):" ++ show e
      Right r -> r

-- read instance from -ddump-deriv
-- | 'Read' instance for 'Refined3'
--
-- >>> reads @(Refined3 OZ (ReadBase Int 16) (0 <..> 0xff) (ShowBase 16) String) "Refined3 254 \"fe\""
-- [(Refined3 254 "fe","")]
--
-- >>> reads @(Refined3 OZ (ReadBase Int 16) (0 <..> 0xff) (ShowBase 16) String) "Refined3 300 \"12c\""
-- []
--
-- >>> reads @(Refined3 OZ (ReadBase Int 16) (Id < 0) (ShowBase 16) String) "Refined3 (-1234) \"-4d2\""
-- [(Refined3 (-1234) "-4d2","")]
--
-- >>> reads @(Refined3 OZ (Map' (ReadP Int Id) (Resplit "\\.")) (GuardBool "len/=4" (Len == 4)) (PrintL 4 "%d.%d.%d.%d" Id) String) "Refined3 [192,168,0,1] \"192.168.0.1\""
-- [(Refined3 [192,168,0,1] "192.168.0.1","")]
--
instance ( Eq i
         , Refined3C opts ip op fmt i
         , Read (PP ip i)
         , Read i
         ) => Read (Refined3 opts ip op fmt i) where
    readPrec
      = GR.parens
          (PCR.prec
             10
             (do GR.expectP (RL.Ident "Refined3")
                 fld1 <- PCR.step GR.readPrec
                 fld2 <- PCR.step GR.readPrec
                 let (_ret,mr) = runIdentity $ eval3MSkip @opts @ip @op @fmt fld1
                 case mr of
                   Nothing -> fail ""
                   Just (Refined3 _r1 r2)
                     | r2 == fld2 -> pure (Refined3 fld1 fld2)
                     | otherwise -> fail "" -- cant display a decent failure message
             )
           )
    readList = GR.readListDefault
    readListPrec = GR.readListPrecDefault

-- | 'ToJSON' instance for 'Refined3'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.encode (unsafeRefined3' @OZ @(ReadBase Int 16) @(0 <..> 0xff) @(ShowBase 16) "fe")
-- "\"fe\""
--
-- >>> A.encode (unsafeRefined3' @OZ @Id @'True @Id 123)
-- "123"
--
instance ( Refined3C opts ip op fmt i
         , ToJSON i
         ) => ToJSON (Refined3 opts ip op fmt i) where
  toJSON = toJSON . r3Out


-- | 'FromJSON' instance for 'Refined3'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode' @(Refined3 OZ (ReadBase Int 16) (Id > 10 && Id < 256) (ShowBase 16) String) "\"00fe\""
-- Right (Refined3 254 "fe")
--
-- >>> removeAnsi $ A.eitherDecode' @(Refined3 OAN (ReadBase Int 16) (Id > 10 && Id < 256) (ShowBase 16) String) "\"00fe443a\""
-- Error in $: Refined3(FromJSON:parseJSON):Step 2. False Boolean Check(op) | {True && False | (16663610 < 256)}
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
--
instance ( Refined3C opts ip op fmt i
         , Show (PP ip i)
         , FromJSON i
         ) => FromJSON (Refined3 opts ip op fmt i) where
  parseJSON z = do
                  i <- parseJSON @i z
                  case newRefined3 i of
                    Left e -> fail $ "Refined3(FromJSON:parseJSON):" ++ show e
                    Right r -> return r

-- | 'Arbitrary' instance for 'Refined3'
--
-- >>> xs <- generate (vectorOf 10 (arbitrary @(Refined3 OAN (ReadP Int Id) (1 <..> 120 && Even) (ShowP Id) String)))
-- >>> all (\x -> let y = r3In x in y /= 0 && r3Out x == show y) xs
-- True
--
instance ( Arbitrary (PP ip i)
         , Refined3C opts ip op fmt i
         ) => Arbitrary (Refined3 opts ip op fmt i) where
  arbitrary = genRefined3 arbitrary

-- | create a 'Refined3' generator using a generator to restrict the values (so it completes)
--
-- >>> g = genRefined3 @OAN @(ReadP Int Id) @(Between 10 100 Id && Even) @(ShowP Id) (choose (10,100))
-- >>> xs <- generate (vectorOf 10 g)
-- >>> all (\x -> let y = r3In x in y >= 0 && y <= 100 && even y) xs
-- True
--
genRefined3 ::
    forall opts ip op fmt i
  . ( Refined3C opts ip op fmt i
    , HasCallStack
    )
  => Gen (PP ip i)
  -> Gen (Refined3 opts ip op fmt i)
genRefined3 = genRefined3P Proxy

-- | create a 'Refined3' generator using a proxy
genRefined3P ::
    forall opts ip op fmt i
  . ( Refined3C opts ip op fmt i
    , HasCallStack
    )
  => Proxy '(opts,ip,op,fmt,i)
  -> Gen (PP ip i)
  -> Gen (Refined3 opts ip op fmt i)
genRefined3P _ g =
  let f !cnt = do
        mppi <- suchThatMaybe g $ \a -> evalQuick @opts @op a == Right True
        case mppi of
          Nothing ->
             let o = getOpt @opts
                 r = getMaxRecursionValue o
             in if cnt >= r
                then error $ setOtherEffects o ("genRefined3P recursion exceeded(" ++ show r ++ ")")
                else f (cnt+1)
          Just ppi ->
             case evalQuick @opts @fmt ppi of
               Left e -> error $ "genRefined3P: formatting failed!! " ++ e
               Right r -> pure $ unsafeRefined3 ppi r
  in f 0

-- | 'Binary' instance for 'Refined3'
--
-- >>> import Control.Arrow ((+++))
-- >>> import Control.Lens
-- >>> import Data.Time
-- >>> type K1 = MakeR3 '(OAN, ReadP Day Id, 'True, ShowP Id, String)
-- >>> type K2 = MakeR3 '(OAN, ReadP Day Id, Between (ReadP Day "2019-05-30") (ReadP Day "2019-06-01") Id, ShowP Id, String)
-- >>> r = unsafeRefined3' "2019-04-23" :: K1
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K1 (B.encode r)
-- Refined3 2019-04-23 "2019-04-23"
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K2 (B.encode r)
-- Refined3(Binary:get):Step 2. False Boolean Check(op) | {2019-05-30 <= 2019-04-23}
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
--
instance ( Refined3C opts ip op fmt i
         , Show (PP ip i)
         , Binary i
         ) => Binary (Refined3 opts ip op fmt i) where
  get = do
          i <- B.get @i
          case newRefined3 i of
            Left e -> fail $ "Refined3(Binary:get):" ++ show e
            Right r -> return r
  put (Refined3 _ r) = B.put @i r

-- | 'Hashable' instance for 'Refined3'
instance ( Refined3C opts ip op fmt i
         , Hashable (PP ip i)
         ) => Hashable (Refined3 opts ip op fmt i) where
  hashWithSalt s (Refined3 a _) = s + hash a

-- | creates a 5-tuple proxy (see 'eval3P' 'newRefined3P')
--
-- use type application to set the 5-tuple or set the individual parameters directly
--
-- set the 5-tuple directly
--
-- >>> eg1 = mkProxy3 @'(OL, ReadP Int Id, Gt 10, ShowP Id, String)
-- >>> newRefined3P eg1 "24"
-- Right (Refined3 24 "24")
--
-- skip the 5-tuple and set each parameter individually using type application
--
-- >>> eg2 = mkProxy3 @_ @OL @(ReadP Int Id) @(Gt 10) @(ShowP Id)
-- >>> newRefined3P eg2 "24"
-- Right (Refined3 24 "24")
--
mkProxy3 ::
  forall z opts ip op fmt i
       . z ~ '(opts,ip,op,fmt,i)
      => Proxy '(opts,ip,op,fmt,i)
mkProxy3 = Proxy

-- | same as 'mkProxy3' but checks to make sure the proxy is consistent with the 'Refined3C' constraint
mkProxy3' :: forall z opts ip op fmt i
  . ( z ~ '(opts,ip,op,fmt,i)
    , Refined3C opts ip op fmt i
    ) => Proxy '(opts,ip,op,fmt,i)
mkProxy3' = Proxy

-- | type family for converting from a 5-tuple '(opts,ip,op,fmt,i) to a 'Refined3' type
type family MakeR3 p where
  MakeR3 '(opts,ip,op,fmt,i) = Refined3 opts ip op fmt i

-- | An ADT that summarises the results of evaluating Refined3 representing all possible states
data RResults3 a =
       RF !String !(Tree PE)        -- Left e
     | RTF !a !(Tree PE) !String !(Tree PE)    -- Right a + Left e
     | RTFalse !a !(Tree PE) !(Tree PE)        -- Right a + Right False
     | RTTrueF !a !(Tree PE) !(Tree PE) !String !(Tree PE) -- Right a + Right True + Left e
     | RTTrueT !a !(Tree PE) !(Tree PE) !(Tree PE)      -- Right a + Right True + Right b
     deriving Show

-- | same as 'newRefined3P'' but passes in the proxy
newRefined3' :: forall opts ip op fmt i m
  . ( MonadEval m
    , Refined3C opts ip op fmt i
    , Show (PP ip i)
    )
  => i
  -> m (Either Msg3 (Refined3 opts ip op fmt i))
newRefined3' = newRefined3P' Proxy

-- | same as 'newRefined3P' but runs in any MonadEval instance
newRefined3P' :: forall opts ip op fmt i proxy m
  . ( MonadEval m
    , Refined3C opts ip op fmt i
    , Show (PP ip i)
    )
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> m (Either Msg3 (Refined3 opts ip op fmt i))
newRefined3P' _ i = do
  (ret,mr) <- eval3M i
  return $ maybe (Left $ prt3Impl (getOpt @opts) ret) Right mr

-- | same as 'newRefined3P' but skips the proxy and allows you to set each parameter individually using type application
--
-- >>> newRefined3 @OZ @(ReadBase Int 16) @(Lt 255) @(PrintF "%x" Id) "00fe"
-- Right (Refined3 254 "fe")
--
-- >>> newRefined3 @OZ @(ReadBase Int 16) @(GuardBool (PrintF "%#x is too large" Id) (Lt 253)) @(PrintF "%x" Id) "00fe"
-- Left Step 2. Failed Boolean Check(op) | 0xfe is too large
--
-- >>> newRefined3 @OZ @(ReadBase Int 16) @(Lt 255) @(PrintF "%x" Id) "00fg"
-- Left Step 1. Failed Initial Conversion(ip) | invalid base 16
--
-- >>> newRefined3 @OL @(Map' (ReadP Int Id) (Resplit "\\.")) @(Msg "length invalid:" (Len == 4)) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) "198.162.3.1.5"
-- Left Step 2. False Boolean Check(op) | {length invalid: 5 == 4}
--
-- >>> newRefined3 @OZ @(Map' (ReadP Int Id) (Resplit "\\.")) @(GuardBool (PrintF "found length=%d" Len) (Len == 4)) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) "198.162.3.1.5"
-- Left Step 2. Failed Boolean Check(op) | found length=5
--
-- >>> newRefined3 @OZ @(Map' (ReadP Int Id) (Resplit "\\.")) @(GuardBool (PrintF "found length=%d" Len) (Len == 4)) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) "198.162.3.1"
-- Right (Refined3 [198,162,3,1] "198.162.003.001")
--
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> newRefined3 @OZ @(MkDayExtra Id >> 'Just Id) @(GuardBool "expected a Sunday" (Thd == 7)) @(UnMkDay Fst) (2019,10,13)
-- Right (Refined3 (2019-10-13,41,7) (2019,10,13))
--
-- >>> newRefined3 @OL @(MkDayExtra Id >> 'Just Id) @(Msg "expected a Sunday:" (Thd == 7)) @(UnMkDay Fst) (2019,10,12)
-- Left Step 2. False Boolean Check(op) | {expected a Sunday: 6 == 7}
--
-- >>> newRefined3 @OZ @(MkDayExtra' Fst Snd Thd >> 'Just Id) @(GuardBool "expected a Sunday" (Thd == 7)) @(UnMkDay Fst) (2019,10,12)
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
-- >>> newRefined3 @OL @(ParseTimeP TimeOfDay "%-H:%-M:%-S") @'True @(FormatTimeP "%H:%M:%S") "1:15:7"
-- Right (Refined3 01:15:07 "01:15:07")
--
-- >>> newRefined3 @OL @(ParseTimeP TimeOfDay "%-H:%-M:%-S") @'True @(FormatTimeP "%H:%M:%S") "1:2:x"
-- Left Step 1. Failed Initial Conversion(ip) | ParseTimeP TimeOfDay (%-H:%-M:%-S) failed to parse
--
-- >>> newRefined3 @OL @(Rescan "^(\\d{1,2}):(\\d{1,2}):(\\d{1,2})$" >> L2 Head >> Map (ReadP Int Id)) @(All (0 <..> 59) && Len == 3) @(PrintL 3 "%02d:%02d:%02d" Id) "1:2:3"
-- Right (Refined3 [1,2,3] "01:02:03")
--
-- >>> newRefined3 @OL @(Resplit "\\." >> Map (ReadP Int Id)) @(BoolsN "oops" 4 (Between 0 255 Id)) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) "13.2.1.251"
-- Right (Refined3 [13,2,1,251] "013.002.001.251")
--
-- >>> newRefined3 @OZ @(Resplit "\\." >> Map (ReadP Int Id)) @(BoolsN "oops" 4 (Between 0 255 Id)) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) "13.2.1.259"
-- Left Step 2. Failed Boolean Check(op) | Bool(3) [oops]
--
-- >>> newRefined3 @OZ @(Resplit "\\." >> Map (ReadP Int Id)) @(BoolsN "oops" 4 (Between 0 255 Id)) @(PrintL 4 "%03d.%03d.%03d.%03d" Id) "13.2.1.253.1"
-- Left Step 2. Failed Boolean Check(op) | Bools:invalid length(5) expected 4
--
newRefined3 :: forall opts ip op fmt i
  . ( Refined3C opts ip op fmt i
    , Show (PP ip i)
    )
  => i
  -> Either Msg3 (Refined3 opts ip op fmt i)
newRefined3 = runIdentity . newRefined3'

-- | create a Refined3 using a 5-tuple proxy and aggregate the results on failure
--
-- >>> type T4 k = '(OZ, MkDayExtra Id >> 'Just Id, GuardBool "expected a Sunday" (Thd == 7), UnMkDay Fst, k)
-- >>> newRefined3P (Proxy @(T4 _)) (2019,10,12)
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
-- >>> newRefined3P (Proxy @(T4 _)) (2019,10,13)
-- Right (Refined3 (2019-10-13,41,7) (2019,10,13))
--
newRefined3P :: forall opts ip op fmt i proxy
  . ( Refined3C opts ip op fmt i
    , Show (PP ip i)
    )
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> Either Msg3 (Refined3 opts ip op fmt i)
newRefined3P p = runIdentity . newRefined3P' p

-- | create a Refined3 value using a 5-tuple proxy (see 'mkProxy3')
--
-- use 'mkProxy3' to package all the types together as a 5-tuple
--
eval3P :: forall opts ip op fmt i m proxy
  . ( MonadEval m
    , Refined3C opts ip op fmt i
    )
  => proxy '(opts,ip,op,fmt,i)
  -> i
  -> m (RResults3 (PP ip i), Maybe (Refined3 opts ip op fmt i))
eval3P _ = eval3M

-- | workhorse for creating 'Refined3' values
eval3M :: forall opts ip op fmt i m
  . ( MonadEval m
    , Refined3C opts ip op fmt i
    )
  => i
  -> m (RResults3 (PP ip i), Maybe (Refined3 opts ip op fmt i))
eval3M i = do
  let o = getOpt @opts
  ll <- eval (Proxy @ip) o i
  case getValAndPE ll of
   (Right a, t1) -> do
     rr <- evalBool (Proxy @op) o a
     case getValAndPE rr of
      (Right True,t2) -> do
        ss <- eval (Proxy @fmt) o a
        pure $ case getValAndPE ss of
         (Right b,t3) -> (RTTrueT a t1 t2 t3, Just (Refined3 a b))
         (Left e,t3) -> (RTTrueF a t1 t2 e t3, Nothing)
      (Right False,t2) -> pure (RTFalse a t1 t2, Nothing)
      (Left e,t2) -> pure (RTF a t1 e t2, Nothing)
   (Left e,t1) -> pure (RF e t1, Nothing)

-- | creates a 'Refined3' value but skips the initial conversion
eval3MSkip :: forall opts ip op fmt i m
  . ( MonadEval m
    , Refined3C opts ip op fmt i
    )
   => PP ip i
   -> m (RResults3 (PP ip i), Maybe (Refined3 opts ip op fmt i))
eval3MSkip a = do
   let o = getOpt @opts
   rr <- evalBool (Proxy @op) o a
   case getValAndPE rr of
    (Right True,t2) -> do
      ss <- eval (Proxy @fmt) o a
      pure $ case getValAndPE ss of
       (Right b,t3) -> (RTTrueT a mkNodeSkipP t2 t3, Just (Refined3 a b))
       (Left e,t3) -> (RTTrueF a mkNodeSkipP t2 e t3, Nothing)
    (Right False,t2) -> pure (RTFalse a mkNodeSkipP t2, Nothing)
    (Left e,t2) -> pure (RTF a mkNodeSkipP e t2, Nothing)

mkNodeSkipP :: Tree PE
mkNodeSkipP = Node (PE TrueP "skipped PP ip i = Id") []

-- | holds the results of creating a 'Refined3' value for display
data Msg3 = Msg3 { m3Desc :: !String
                 , m3Short :: !String
                 , m3Long :: !String
                 , m3ValP :: !ValP
                 } deriving Eq

instance Show Msg3 where
  show (Msg3 a b c _d) = a <> nullIf " | " b <> nullIf "\n" c

prt3Impl :: forall a . Show a
  => POpts
  -> RResults3 a
  -> Msg3
prt3Impl opts v =
  let outmsg msg = "*** " <> formatOMsg opts " " <> msg <> " ***\n"
      msg1 a = outmsg ("Step 1. Success Initial Conversion(ip) (" ++ showL opts a ++ ")")
      mkMsg3 m n r bp | hasNoTree opts = Msg3 m n "" bp
                     | otherwise = Msg3 m n r bp
  in case v of
       RF e t1 ->
         let (m,n) = ("Step 1. " <> colorValP Short opts (FailP e) <> " Initial Conversion(ip)", e)
             r = outmsg m
              <> prtTreePure opts t1
         in mkMsg3 m n r (t1 ^. root . peValP)
       RTF a t1 e t2 ->
         let (m,n) = ("Step 2. " <> colorValP Short opts (FailP e) <> " Boolean Check(op)", e)
             r = msg1 a
              <> prtTreePure opts t1
              <> "\n"
              <> outmsg m
              <> prtTreePure opts t2
         in mkMsg3 m n r (t2 ^. root . peValP)
       RTFalse a t1 t2 ->
         let (m,n) = ("Step 2. " <> colorValP Short opts FalseP <> " Boolean Check(op)", z)
             z = let w = t2 ^. root . peString
                 in if all isSpace w then "FalseP" else "{" <> w <> "}"
             r = msg1 a
              <> prtTreePure opts t1
              <> "\n"
              <> outmsg m
              <> prtTreePure opts t2
         in mkMsg3 m n r FalseP
       RTTrueF a t1 t2 e t3 ->
         let (m,n) = ("Step 3. " <> colorValP Short opts (FailP e) <> " Output Conversion(fmt)", e)
             r = msg1 a
              <> prtTreePure opts t1
              <> "\n"
              <> outmsg "Step 2. Success Boolean Check(op)"
              <> prtTreePure opts t2
              <> "\n"
              <> outmsg m
              <> prtTreePure opts t3
         in mkMsg3 m n r (t3 ^. root . peValP)
       RTTrueT a t1 t2 t3 ->
         let (m,n) = ("Step 3. Success Output Conversion(fmt)", "")
             r = msg1 a
              <> prtTreePure opts t1
              <> "\n"
              <> outmsg "Step 2. Success Boolean Check(op)"
              <> prtTreePure opts t2
              <> "\n"
              <> outmsg m
              <> prtTreePure opts t3
         in mkMsg3 m n r (t3 ^. root . peValP)
