-- tojson binary hash arbitrary all use i not PP ip i
-- all instances work with the original input [ie not the internal values]
--   we have no way to get back to i from PP ip i (unlike Refined3)
--  {-# OPTIONS -Wno-redundant-constraints #-}
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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE NoStarIsType #-}
-- | a refinement type allowing the external type to differ from the internal type
module Predicate.Refined2 (

  -- ** Refined2
    Refined2
  , r2In
  , r2Out
  , Refined2C

 -- ** display results
  , Msg2 (..)
  , RResults2 (..)
  , prt2Impl

  -- ** evaluation methods
  , eval2P
  , eval2M
  , newRefined2
  , newRefined2'
  , newRefined2P
  , newRefined2P'

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

 -- ** exception
  , Refined2Exception(..)

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Data.Tree (Tree)
import Data.Proxy (Proxy(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Language.Haskell.TH.Syntax as TH
import qualified GHC.Read as GR
import qualified Text.ParserCombinators.ReadPrec as PCR
import qualified Text.Read.Lex as RL
import qualified Data.Binary as B
import Data.Binary (Binary)
import Data.Maybe (isJust)
import Control.Lens
import Data.Tree.Lens (root)
import Data.Char (isSpace)
import Data.String (IsString(..))
import Data.Hashable (Hashable(..))
import GHC.Stack (HasCallStack)
import Test.QuickCheck
import Control.DeepSeq (rnf, rnf2, NFData)
import qualified Control.Exception as E
import GHC.Generics (Generic)
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :m + Predicate
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
data Refined2 (opts :: Opt) ip op i = Refined2 !(PP ip i) !i

-- | field accessor for the converted input value in 'Refined2'
r2In :: Refined2 (opts :: Opt) ip op i -> PP ip i
r2In (Refined2 ppi _) = ppi

-- | field accessor for the original input value in 'Refined2'
r2Out :: Refined2 (opts :: Opt) ip op i -> i
r2Out (Refined2 _ i) = i

type role Refined2 phantom nominal nominal nominal

-- | directly load values into 'Refined2'. It still checks to see that those values are valid
unsafeRefined2' :: forall opts ip op i
                . ( Show (PP ip i)
                  , Refined2C opts ip op i
                  )
                => i
                -> Refined2 opts ip op i
unsafeRefined2' = either (E.throw . Refined2Exception . show) id . newRefined2

-- | directly load values into 'Refined2' without any checking
unsafeRefined2 :: forall opts ip op i
   . Refined2C opts ip op i
   => PP ip i
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

deriving stock instance
  ( Refined2C opts ip op i
  , Show i
  , Show (PP ip i)
  ) => Show (Refined2 opts ip op i)

deriving stock instance
  ( Refined2C opts ip op i
  , Eq i
  , Eq (PP ip i)
  ) => Eq (Refined2 opts ip op i)

deriving stock instance
  ( Refined2C opts ip op i
  , Ord i
  , Ord (PP ip i)
  ) => Ord (Refined2 opts ip op i)

deriving stock instance
  ( Refined2C opts ip op i
  , TH.Lift (PP ip i)
  , TH.Lift i
  ) => TH.Lift (Refined2 opts ip op i)

instance ( Refined2C opts ip op i
         , NFData i
         , NFData (PP ip i)
         ) => NFData (Refined2 opts ip op i) where
  rnf (Refined2 a b) = rnf2 (a,b)

-- | 'IsString' instance for Refined2
--
-- >>> pureTryTest $ fromString @(Refined2 OL (ReadP Int Id) (Id > 12) String) "523"
-- Right (Refined2 523 "523")
--
-- >>> pureTryTest $ fromString @(Refined2 OL (ReadP Int Id) (Id > 12) String) "2"
-- Left ()
--
instance ( i ~ String
         , Refined2C opts ip op i
         , Show (PP ip i)
         ) => IsString (Refined2 opts ip op i) where
  fromString i =
    case newRefined2 i of
      Left e -> E.throw $ Refined2Exception  $ "IsString:fromString:" ++ show e
      Right r -> r

-- read instance from -ddump-deriv
-- | 'Read' instance for 'Refined2'
--
-- >>> reads @(Refined2 OZ (ReadBase Int 16) (0 <..> 0xff) String) "Refined2 254 \"fe\""
-- [(Refined2 254 "fe","")]
--
-- >>> reads @(Refined2 OZ (ReadBase Int 16) (0 <..> 0xff) String) "Refined2 300 \"12c\""
-- []
--
-- >>> reads @(Refined2 OZ (ReadBase Int 16) (Id < 0) String) "Refined2 (-1234) \"-4d2\""
-- [(Refined2 (-1234) "-4d2","")]
--
-- >>> reads @(Refined2 OZ (Map' (ReadP Int Id) (Resplit "\\.")) (GuardBool "len/=4" (Len == 4)) String) "Refined2 [192,168,0,1] \"192.168.0.1\""
-- [(Refined2 [192,168,0,1] "192.168.0.1","")]
--
instance ( Refined2C opts ip op i
         , Read (PP ip i)
         , Read i
         ) => Read (Refined2 opts ip op i) where
    readPrec
      = GR.parens
          (PCR.prec
             10
             (do GR.expectP (RL.Ident "Refined2")
                 fld1 <- PCR.step GR.readPrec
                 fld2 <- PCR.step GR.readPrec
                 let lr = evalQuick @opts @op fld1

                 case lr of
                   Left {} -> fail ""
                   Right True -> pure (Refined2 fld1 fld2)
                   Right False -> fail ""
             )
           )
    readList = GR.readListDefault
    readListPrec = GR.readListPrecDefault

-- | 'ToJSON' instance for 'Refined2'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.encode (unsafeRefined2 @OZ @(ReadBase Int 16) @(0 <..> 0xff) 254 "fe")
-- "\"fe\""
--
-- >>> A.encode (unsafeRefined2 @OZ @Id @'True @Int 123 123)
-- "123"
--
instance ( Refined2C opts ip op i
         , ToJSON i
         ) => ToJSON (Refined2 opts ip op i) where
  toJSON = toJSON . r2Out


-- | 'FromJSON' instance for 'Refined2'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode' @(Refined2 OZ (ReadBase Int 16) (Id > 10 && Id < 256) String) "\"00fe\""
-- Right (Refined2 254 "00fe")
--
-- >>> removeAnsi $ A.eitherDecode' @(Refined2 OAN (ReadBase Int 16) (Id > 10 && Id < 256) String) "\"00fe443a\""
-- Error in $: Refined2(FromJSON:parseJSON):Step 2. False Boolean Check(op) | {True && False | (16663610 < 256)}
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
instance ( Refined2C opts ip op i
         , Show (PP ip i)
         , FromJSON i
         ) => FromJSON (Refined2 opts ip op i) where
  parseJSON z = do
                  i <- parseJSON @i z
                  case newRefined2 i of
                    Left e -> fail $ "Refined2(FromJSON:parseJSON):" ++ show e
                    Right r -> return r

-- | 'Arbitrary' instance for 'Refined2'
--
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> xs <- generate (vectorOf 10 (arbitrary @(Refined2 OAN (ToEnum Day) (L2 (ToWeekDate Id) == "Tuesday") Int)))
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
-- >>> g = genRefined2 @OAN @(ToEnum Day) @(UnMkDay Id >> Snd == 10) arbitrary
-- >>> xs <- generate (vectorOf 10 g)
-- >>> all (\x -> let y = toEnum @Day (fromIntegral (r2Out x)) in view _2 (toGregorian y) == 10 && y == r2In x) xs
-- True
--
genRefined2 ::
    forall opts ip op i
  . ( Refined2C opts ip op i
    , HasCallStack
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
    , HasCallStack
    , Show (PP ip i)
    )
  => Proxy '(opts,ip,op,i)
  -> Gen i
  -> Gen (Refined2 opts ip op i)
genRefined2P _ g =
  let o = getOpt @opts
      r = getMaxRecursionValue o
      f !cnt = do
        mi <- suchThatMaybe g (isJust . snd . runIdentity . eval2M @opts @ip @op)
        case mi of
          Nothing | cnt >= r -> E.throw $ Refined2Exception $ setOtherEffects o $ "genRefined2P recursion exceeded(" ++ show r ++ ")"
                  | otherwise -> f (cnt+1)
          Just i ->
             case newRefined2 i of
               Left e -> errorInProgram $ "conversion failed:" ++ show e
               Right ret -> pure ret
  in f 0

-- | 'Binary' instance for 'Refined2'
--
-- >>> import Control.Arrow ((+++))
-- >>> import Data.Time
-- >>> type K1 = Refined2 OAN (ReadP Day Id) 'True String
-- >>> type K2 = Refined2 OAN (ReadP Day Id) (Between (ReadP Day "2019-05-30") (ReadP Day "2019-06-01") Id) String
-- >>> r = unsafeRefined2' "2019-04-23" :: K1
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K1 (B.encode r)
-- Refined2 2019-04-23 "2019-04-23"
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K2 (B.encode r)
-- Refined2(Binary:get):Step 2. False Boolean Check(op) | {2019-05-30 <= 2019-04-23}
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
instance ( Refined2C opts ip op i
         , Show (PP ip i)
         , Binary i
         ) => Binary (Refined2 opts ip op i) where
  get = do
          i <- B.get @i
          case newRefined2 i of
            Left e -> fail $ "Refined2(Binary:get):" ++ show e
            Right r -> return r
  put (Refined2 _ r) = B.put @i r

-- | 'Hashable' instance for 'Refined2'
instance ( Refined2C opts ip op i
         , Hashable i
         ) => Hashable (Refined2 opts ip op i) where
  hashWithSalt s (Refined2 _ b) = s + hash b

-- | An ADT that summarises the results of evaluating Refined2 representing all possible states
data RResults2 a =
       RF !String !(Tree PE)        -- fails initial conversion
     | RTF !a !(Tree PE) !String !(Tree PE)    -- op fails
     | RTFalse !a !(Tree PE) !(Tree PE)        -- op false
     | RTTrue !a !(Tree PE) !(Tree PE) -- op true
     deriving stock Show

-- | version for creating a 'Refined2' value that works for any 'MonadEval' instance
newRefined2' :: forall opts ip op i m
  . ( MonadEval m
    , Refined2C opts ip op i
    , Show (PP ip i)
    )
  => i
  -> m (Either Msg2 (Refined2 opts ip op i))
newRefined2' = newRefined2P' Proxy

-- | same as 'newRefined2P' but runs for any MonadEval instance
newRefined2P' :: forall opts ip op i proxy m
  . ( MonadEval m
    , Refined2C opts ip op i
    , Show (PP ip i)
    )
  => proxy '(opts,ip,op,i)
  -> i
  -> m (Either Msg2 (Refined2 opts ip op i))
newRefined2P' _ i = do
  (ret,mr) <- eval2M i
  return $ maybe (Left $ prt2Impl (getOpt @opts) ret) Right mr

-- | pure version for creating a 'Refined2' value
--
-- >>> newRefined2 @OZ @(ReadBase Int 16) @(Lt 255) "00fe"
-- Right (Refined2 254 "00fe")
--
-- >>> newRefined2 @OZ @(ReadBase Int 16) @(GuardBool (PrintF "%#x is too large" Id) (Lt 253)) "00fe"
-- Left Step 2. Failed Boolean Check(op) | 0xfe is too large
--
-- >>> newRefined2 @OZ @(ReadBase Int 16) @(Lt 255) "00fg"
-- Left Step 1. Failed Initial Conversion(ip) | invalid base 16
--
-- >>> newRefined2 @OL @(Map' (ReadP Int Id) (Resplit "\\.")) @(Msg "length invalid:" (Len == 4)) "198.162.3.1.5"
-- Left Step 2. False Boolean Check(op) | {length invalid: 5 == 4}
--
-- >>> newRefined2 @OZ @(Map' (ReadP Int Id) (Resplit "\\.")) @(GuardBool (PrintF "found length=%d" Len) (Len == 4)) "198.162.3.1.5"
-- Left Step 2. Failed Boolean Check(op) | found length=5
--
-- >>> newRefined2 @OZ @(Map' (ReadP Int Id) (Resplit "\\.")) @(GuardBool (PrintF "found length=%d" Len) (Len == 4)) "198.162.3.1"
-- Right (Refined2 [198,162,3,1] "198.162.3.1")
--
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> newRefined2 @OZ @(MkDayExtra Id >> 'Just Id) @(GuardBool "expected a Sunday" (Thd == 7)) (2019,10,13)
-- Right (Refined2 (2019-10-13,41,7) (2019,10,13))
--
-- >>> newRefined2 @OL @(MkDayExtra Id >> 'Just Id) @(Msg "expected a Sunday:" (Thd == 7)) (2019,10,12)
-- Left Step 2. False Boolean Check(op) | {expected a Sunday: 6 == 7}
--
-- >>> newRefined2 @OZ @(MkDayExtra' Fst Snd Thd >> 'Just Id) @(GuardBool "expected a Sunday" (Thd == 7)) (2019,10,12)
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
-- >>> newRefined2 @OL @Id @'True 22
-- Right (Refined2 22 22)
--
-- >>> newRefined2 @OL @(ReadP UTCTime Id) @(Between (MkDay '(2020,5,2)) (MkDay '(2020,5,7)) (MkJust ToDay)) "2020-05-04 12:13:14Z"
-- Right (Refined2 2020-05-04 12:13:14 UTC "2020-05-04 12:13:14Z")
--
-- >>> newRefined2 @OL @(ReadP UTCTime Id) @(Between (MkDay '(2020,5,2)) (MkDay '(2020,5,7)) (MkJust ToDay)) "2020-05-08 12:13:14Z"
-- Left Step 2. False Boolean Check(op) | {Just 2020-05-08 <= Just 2020-05-07}
--
-- >>> :m + Data.Ratio
-- >>> newRefined2 @OZ @(ReadP Rational Id) @'True "13 % 3"
-- Right (Refined2 (13 % 3) "13 % 3")
--
-- >>> newRefined2 @OZ @(ReadP Rational Id) @'True "13x % 3"
-- Left Step 1. Failed Initial Conversion(ip) | ReadP Ratio Integer (13x % 3)
--
-- >>> newRefined2 @OZ @(ReadP Rational Id) @(3 % 1 <..> 5 % 1) "13 % 3"
-- Right (Refined2 (13 % 3) "13 % 3")
--
-- >>> newRefined2 @OZ @(ReadP Rational Id) @(11 -% 2 <..> 3 -% 1) "-13 % 3"
-- Right (Refined2 ((-13) % 3) "-13 % 3")
--
-- >>> newRefined2 @OZ @(ReadP Rational Id) @(Id > (15 % 1)) "13 % 3"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> newRefined2 @OL @(ReadP Rational Id) @(Msg (PrintF "invalid=%3.2f" (FromRational Double)) (Id > (15 % 1))) "13 % 3"
-- Left Step 2. False Boolean Check(op) | {invalid=4.33 13 % 3 > 15 % 1}
--
-- >>> newRefined2 @OZ @(ReadP Rational Id) @(Id > (11 % 1)) "13 % 3"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> newRefined2 @OZ @(ReadP UTCTime Id) @'True "2018-10-19 14:53:11.5121359 UTC"
-- Right (Refined2 2018-10-19 14:53:11.5121359 UTC "2018-10-19 14:53:11.5121359 UTC")
--
-- >>> :m + Data.Aeson
-- >>> newRefined2 @OZ @(ReadP Value Id) @'True "String \"jsonstring\""
-- Right (Refined2 (String "jsonstring") "String \"jsonstring\"")
--
-- >>> newRefined2 @OZ @(ReadP Value Id) @'True "Number 123.4"
-- Right (Refined2 (Number 123.4) "Number 123.4")
--
-- >>> :m + Text.Show.Functions
-- >>> newRefined2 @OU @(Id $$ 13) @(Id > 100) (\x -> x * 14) ^? _Right . to r2Out
-- Just <function>
--
-- >>> newRefined2 @OU @(Id $$ 13) @(Id > 100) (\x -> x * 14) ^? _Right . to r2In
-- Just 182
--
-- >>> newRefined2 @OU @(Id $$ 13) @(Id > 100) (\x -> x * 14) ^? _Right . to (($ 13) . r2Out)
-- Just 182
--
-- >>> newRefined2 @OZ @(Pop0 Fst Id) @(Len > 1) (Proxy @Snd,"Abcdef") ^? _Right . to r2In
-- Just "Abcdef"
--
-- >>> newRefined2 @OZ @(Pop0 Fst Id >> Len) @(Id > 1) (Proxy @Snd,"Abcdef") ^? _Right . to r2In
-- Just 6
--
newRefined2 :: forall opts ip op i
  . ( Refined2C opts ip op i
    , Show (PP ip i)
  ) => i
    -> Either Msg2 (Refined2 opts ip op i)
newRefined2 = newRefined2P Proxy

-- | create a Refined2 using a 4-tuple proxy and aggregate the results on failure
--
-- >>> type T4 k = '(OZ, MkDayExtra Id >> 'Just Id, GuardBool "expected a Sunday" (Thd == 7), k)
-- >>> newRefined2P (Proxy @(T4 _)) (2019,10,12)
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
-- >>> newRefined2P (Proxy @(T4 _)) (2019,10,13)
-- Right (Refined2 (2019-10-13,41,7) (2019,10,13))
--
newRefined2P :: forall opts ip op i
  . ( Refined2C opts ip op i
    , Show (PP ip i)
  ) => Proxy '(opts,ip,op,i)
    -> i
    -> Either Msg2 (Refined2 opts ip op i)
newRefined2P _ i =
  let (ret,mr) = runIdentity $ eval2M i
  in maybe (Left $ prt2Impl (getOpt @opts) ret) Right mr

-- | create a Refined2 value using a 4-tuple proxy (see 'mkProxy2')
eval2P :: forall opts ip op i m
  . ( Refined2C opts ip op i
    , MonadEval m
    )
  => Proxy '(opts,ip,op,i)
  -> i
  -> m (RResults2 (PP ip i), Maybe (Refined2 opts ip op i))
eval2P _ = eval2M

-- | workhorse for creating 'Refined2' values
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

-- | holds the results of creating a 'Refined2' value for display
data Msg2 = Msg2 { m2Desc :: !String
                 , m2Short :: !String
                 , m2Long :: !String
                 , m2ValP :: !ValP
                 } deriving stock Eq

instance Show Msg2 where
  show (Msg2 a b c _d) = joinStrings a b <> nullIf "\n" c

-- | format the output from creating a 'Refined2' value into 'Msg2'
prt2Impl :: forall a . Show a
  => POpts
  -> RResults2 a
  -> Msg2
prt2Impl opts v =
  let outmsg msg = "*** " <> formatOMsg opts " " <> msg <> " ***\n"
      msg1 a = outmsg ("Step 1. Success Initial Conversion(ip) (" ++ showL opts a ++ ")")
      mkMsg2 m n r bp | hasNoTree opts = Msg2 m n "" bp
                      | otherwise = Msg2 m n r bp
  in case v of
       RF e t1 ->
         let (m,n) = ("Step 1. " <> colorValP Short opts (FailP e) <> " Initial Conversion(ip)", e)
             r = outmsg m
              <> prtTreePure opts t1
         in mkMsg2 m n r (t1 ^. root . peValP)
       RTF a t1 e t2 ->
         let (m,n) = ("Step 2. " <> colorValP Short opts (FailP e) <> " Boolean Check(op)", e)
             r = msg1 a
              <> prtTreePure opts t1
              <> "\n"
              <> outmsg m
              <> prtTreePure opts t2
         in mkMsg2 m n r (t2 ^. root . peValP)
       RTFalse a t1 t2 ->
         let (m,n) = ("Step 2. " <> colorValP Short opts FalseP <> " Boolean Check(op)", z)
             z = let w = t2 ^. root . peString
                 in if all isSpace w then "FalseP" else "{" <> w <> "}"
             r = msg1 a
              <> prtTreePure opts t1
              <> "\n"
              <> outmsg m
              <> prtTreePure opts t2
         in mkMsg2 m n r FalseP
       RTTrue a t1 t2 ->
         let (m,n) = ("Step 2. True Boolean Check(op)", "")
             r = msg1 a
              <> prtTreePure opts t1
              <> "\n"
              <> outmsg m
              <> prtTreePure opts t2
         in mkMsg2 m n r (t2 ^. root . peValP)

-- | creates a 4-tuple proxy (see 'eval2P' 'newRefined2P')
--
-- use type application to set the 4-tuple or set the individual parameters directly
--
-- set the 4-tuple directly
--
-- >>> eg1 = mkProxy2 @'(OL, ReadP Int Id, Gt 10, String)
-- >>> newRefined2P eg1 "24"
-- Right (Refined2 24 "24")
--
-- skip the 4-tuple and set each parameter individually using type application
--
-- >>> eg2 = mkProxy2 @_ @OL @(ReadP Int Id) @(Gt 10)
-- >>> newRefined2P eg2 "24"
-- Right (Refined2 24 "24")
--
mkProxy2 :: forall z opts ip op i
  . z ~ '(opts,ip,op,i)
    => Proxy '(opts,ip,op,i)
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

-- | refinement exception
newtype Refined2Exception = Refined2Exception String
  deriving Generic

instance Show Refined2Exception where
  show (Refined2Exception e) = "Refined2Exception:\n" ++ e

instance E.Exception Refined2Exception where
  displayException = show