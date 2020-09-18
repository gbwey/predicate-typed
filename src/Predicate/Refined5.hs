-- refined5 doesnt care about the input as it is thrown away so we have no choice but to use PP ip i
-- like Refined2 but ditches the original input value
-- all instances uses the internal values except for IsString [internal value is less likely to be a string!]
--   but json/binary/hash use internal input (ie PP ip i) as they json and binary have to roundtrip
-- tojson only has access to PP ip i! so fromjson can only use this!
{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
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
     see 'Refined5'
-}
module Predicate.Refined5 (

  -- ** Refined5
    Refined5

  -- ** evaluation methods
  , eval5P
  , eval5M
  , newRefined5
  , newRefined5P
  , newRefined5'
  , newRefined5P'

  -- ** create a wrapped Refined5 value
  , newRefined5T
  , newRefined5TP
  , newRefined5TIO
  , withRefined5T
  , withRefined5TP
  , withRefined5TIO

  -- ** proxy methods
  , MakeR5

  -- ** QuickCheck methods
  , genRefined5
  , genRefined5P

  -- ** unsafe methods for creating Refined5
  , unsafeRefined5
  , unsafeRefined5'

  , unRefined5

  , replaceOpt5
  , appendOpt5

 ) where
import Predicate.Refined2 (Msg2(..), RResults2(..), prt2Impl, Refined2C)
import Predicate.Refined
import Predicate.Core
import Predicate.Util
import Data.Proxy
import Control.Monad.Except
import Control.Monad.Writer (tell)
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Language.Haskell.TH.Syntax as TH
import qualified GHC.Read as GR
import qualified Text.ParserCombinators.ReadPrec as PCR
import qualified Text.Read.Lex as RL
import qualified Data.Binary as B
import Data.Binary (Binary)
import Control.Lens
import Data.String
import Data.Hashable (Hashable(..))
import GHC.Stack
import Test.QuickCheck
import Data.Coerce
import Data.Either (isRight)
-- import Data.Maybe (isJust)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :m + Predicate.Prelude
-- >>> :m + Predicate.Refined2
-- >>> :m + Data.Time

-- | Refinement type for specifying an input type that is different from the output type
--
--   * @opts@ are the display options
--   * @ip@ converts @i@ to @PP ip i@ which is stored
--   * @op@ validates that internal type using @PP op (PP ip i) ~ Bool@
--   * @i@ is the input type which is discarded after converting to PP ip i
--
newtype Refined5 (opts :: Opt) ip op i = Refined5 (PP ip i)

type role Refined5 phantom nominal nominal nominal

-- | extract the value from Refined
unRefined5 :: forall k k1 (opts :: Opt) (ip :: k) (op :: k1) i
   . Refined5 opts ip op i
  -> PP ip i
unRefined5 = coerce


-- | directly load values into 'Refined5'. It still checks to see that those values are valid
unsafeRefined5' :: forall opts ip op i
                . ( Refined2C opts ip op i
                  , HasCallStack
                  )
                => PP ip i
                -> Refined5 opts ip op i
unsafeRefined5' = either error Refined5 . evalBoolP @opts @op

-- | directly load values into 'Refined5' without any checking
unsafeRefined5 :: forall opts ip op i
   . PP ip i
  -> Refined5 opts ip op i
unsafeRefined5 = Refined5

deriving instance Show (PP ip i) => Show (Refined5 opts ip op i)
deriving instance Eq (PP ip i) => Eq (Refined5 opts ip op i)
deriving instance TH.Lift (PP ip i) => TH.Lift (Refined5 opts ip op i)

-- | 'IsString' instance for Refined5
--
-- >>> pureTryTest $ fromString @(Refined5 OL (ReadP Int Id) (Id > 12) String) "523"
-- Right (Refined5 523)
--
-- >>> pureTryTest $ fromString @(Refined5 OL (ReadP Int Id) (Id > 12) String) "2"
-- Left ()
--
instance ( i ~ String
         , Refined2C opts ip op i
         , Show (PP ip i)
         ) => IsString (Refined5 opts ip op i) where
  fromString i =
    case newRefined5 @opts @ip @op i of
      Left e -> error $ "Refined5(fromString):" ++ show e
      Right r -> r

-- read instance from -ddump-deriv
-- | 'Read' instance for 'Refined5'
--
-- >>> reads @(Refined5 OZ (ReadBase Int 16 Id) (Between 0 255 Id) String) "Refined5 254"
-- [(Refined5 254,"")]
--
-- >>> reads @(Refined5 OZ (ReadBase Int 16 Id) (Between 0 255 Id) String) "Refined5 300"
-- []
--
-- >>> reads @(Refined5 OZ (ReadBase Int 16 Id) (Id < 0) String) "Refined5 -1234"
-- [(Refined5 (-1234),"")]
--
-- >>> reads @(Refined5 OZ (Map (ReadP Int Id) (Resplit "\\.")) (GuardBool "len/=4" (Len == 4)) String) "Refined5 [192,168,0,1]"
-- [(Refined5 [192,168,0,1],"")]
--
-- >>> reads @(Refined5 OZ (ReadP Rational Id) (Id > Negate 4 % 3) String) "Refined5 (-10 % 9)"
-- [(Refined5 ((-10) % 9),"")]
--
-- >>> reads @(Refined5 OZ (ReadP Rational Id) (Id > Negate 4 % 3) String) "Refined5 (-10 % 6)"
-- []

instance ( Refined2C opts ip op i
         , Read (PP ip i)
         ) => Read (Refined5 opts ip op i) where
    readPrec
      = GR.parens
          (PCR.prec
             11
             (do GR.expectP (RL.Ident "Refined5")
                 fld0 <- PCR.reset GR.readPrec
                 case evalQuick @opts @op fld0 of
                   Left {} -> fail ""
                   Right True -> pure (Refined5 fld0)
                   Right False -> fail ""
             ))
    readList = GR.readListDefault
    readListPrec = GR.readListPrecDefault

-- | 'ToJSON' instance for 'Refined5'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.encode (unsafeRefined5' @OZ @(ReadBase Int 16 Id) @(Between 0 255 Id) 254)
-- "254"
--
-- >>> A.encode (unsafeRefined5 @OZ @Id @'True @Int 123)
-- "123"
--
instance ToJSON (PP ip i) => ToJSON (Refined5 opts ip op i) where
  toJSON (Refined5 x) = toJSON x

-- | 'FromJSON' instance for 'Refined5'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode' @(Refined5 OZ (ReadBase Int 16 Id) (Id > 10 && Id < 256) String) "123"
-- Right (Refined5 123)
--
-- >>> removeAnsi $ A.eitherDecode' @(Refined5 OL (ReadBase Int 16 Id) (Id > 10 && Id < 256) String) "9"
-- Error in $: Refined5:false boolean check | {False && True | (9 > 10)}
-- False
--
-- >>> A.eitherDecode' @(Refined5 OZ (ReadBase Int 16 Id) (Id > 10 && Id < 256) String) "254"
-- Right (Refined5 254)
--
-- >>> removeAnsi $ A.eitherDecode' @(Refined5 OAN (ReadBase Int 16 Id) (Id > 10 && Id < 256) String) "12345"
-- Error in $: Refined5:false boolean check | {True && False | (12345 < 256)}
-- False True && False | (12345 < 256)
-- |
-- +- True 12345 > 10
-- |  |
-- |  +- P Id 12345
-- |  |
-- |  `- P '10
-- |
-- `- False 12345 < 256
--    |
--    +- P Id 12345
--    |
--    `- P '256
-- <BLANKLINE>
--
instance ( Refined2C opts ip op i
         , FromJSON (PP ip i)
         ) => FromJSON (Refined5 opts ip op i) where
  parseJSON z = do
                  i <- parseJSON @(PP ip i) z
                  case evalBoolP @opts @op i of
                    Left e -> fail $ "Refined5:" ++ e
                    Right _ -> return (Refined5 i)

-- | 'Arbitrary' instance for 'Refined5'
--
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> xs <- generate (vectorOf 10 (arbitrary @(Refined5 OAN (ReadP Int Id) (Negate 10 <..> 10) String)))
-- >>> all (\x -> unRefined5 x `elem` [-10 .. 10]) xs
-- True
--
instance ( Arbitrary (PP ip i)
         , Refined2C opts ip op i
         ) => Arbitrary (Refined5 opts ip op i) where
  arbitrary = genRefined5 arbitrary

-- | create a 'Refined5' generator using a generator to restrict the values (so it completes)
genRefined5 ::
    forall opts ip op i
  . ( Refined2C opts ip op i
    , HasCallStack
    )
  => Gen (PP ip i)
  -> Gen (Refined5 opts ip op i)
genRefined5 = genRefined5P Proxy

-- generates the external value unlike Refined3 as we dont have a way to recreate the output from the internal value
-- | create a 'Refined5' generator using a proxy
genRefined5P ::
    forall opts ip op i
  . ( Refined2C opts ip op i
    , HasCallStack
    )
  => Proxy '(opts,ip,op,i)
  -> Gen (PP ip i)
  -> Gen (Refined5 opts ip op i)
genRefined5P _ g =
  let f !cnt = do
        mi <- suchThatMaybe g (isRight . evalBoolP @opts @op)
        case mi of
          Nothing ->
             let o = getOpt @opts
             in if cnt >= oRecursion o
                then error $ setOtherEffects o ("genRefined5P recursion exceeded(" ++ show (oRecursion o) ++ ")")
             else f (cnt+1)
          Just i -> pure $ unsafeRefined5 i
  in f 0

-- | 'Binary' instance for 'Refined5'
--
instance ( Refined2C opts ip op i
         , Binary (PP ip i)
         ) => Binary (Refined5 opts ip op i) where
  get = do
          i <- B.get @(PP ip i)
          case evalBoolP @opts @op i of
            Left e -> fail $ "Refined5:Binary:" ++ e
            Right _ -> return $ Refined5 i
  put (Refined5 r) = B.put @(PP ip i) r

-- | 'Hashable' instance for 'Refined5'
instance (Refined2C opts ip op i
        , Hashable (PP ip i)
        ) => Hashable (Refined5 opts ip op i) where
  hashWithSalt s (Refined5 b) = s + hash b

-- | same as 'withRefined5T' for IO
withRefined5TIO :: forall opts ip op i m b
  . ( MonadIO m
    , Refined2C opts ip op i
    , Show (PP ip i)
    )
  => i
  -> (Refined5 opts ip op i -> RefinedT m b)
  -> RefinedT m b
withRefined5TIO = (>>=) . newRefined5TIO @opts @ip @op @i

-- | create a 'Refined5' value using a continuation
--
-- This first example reads a hex string and makes sure it is between 100 and 200 and then
-- reads a binary string and adds the values together
--
-- >>> prtRefinedTIO $ withRefined5T @OZ @(ReadBase Int 16 Id) @(Between 100 200 Id) "a3" $ \x -> withRefined5T @OZ @(ReadBase Int 2 Id) @'True "1001110111" $ \y -> pure (unRefined5 x + unRefined5 y)
-- 794
--
-- this example fails as the the hex value is out of range
--
-- >>> prtRefinedTIO $ withRefined5T @OAN @(ReadBase Int 16 Id) @(Between 100 200 Id) "a388" $ \x -> withRefined5T @OAN @(ReadBase Int 2 Id) @'True "1001110111" $ \y -> pure (x,y)
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
withRefined5T :: forall opts ip op i m b
  . ( Monad m
    , Refined2C opts ip op i
    , Show (PP ip i)
    )
  => i
  -> (Refined5 opts ip op i -> RefinedT m b)
  -> RefinedT m b
withRefined5T = (>>=) . newRefined5TP (Proxy @'(opts,ip,op,i))

withRefined5TP :: forall opts ip op i b proxy m
  . ( Monad m
    , Refined2C opts ip op i
    , Show (PP ip i)
    )
  => proxy '(opts,ip,op,i)
  -> i
  -> (Refined5 opts ip op i -> RefinedT m b)
  -> RefinedT m b
withRefined5TP p = (>>=) . newRefined5TP p

-- | create a wrapped 'Refined5' type
--
-- >>> prtRefinedTIO $ newRefined5T @OL @(MkDayExtra Id >> 'Just Id) @(Thd == 5) (2019,11,1)
-- Refined5 (2019-11-01,44,5)
--
-- >>> prtRefinedTIO $ newRefined5T @OL @(MkDayExtra Id >> 'Just Id) @(Thd == 5) (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {6 == 5}]
--
-- >>> prtRefinedTIO $ newRefined5T @OL @(MkDayExtra Id >> 'Just Id) @(Msg "wrong day:" (Thd == 5)) (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {wrong day: 6 == 5}]
--
-- >>> prtRefinedTIO $ newRefined5TIO @OL @(Hide (Rescan "(\\d+)" >> ConcatMap Snd Id) >> Map (ReadP Int Id) Id) @(Len > 0 && All (0 <..> 0xff)) "|23|99|255|254.911."
-- failure msg[Step 2. False Boolean Check(op) | {True && False | (All(5) i=4 (911 <= 255))}]
--
newRefined5T :: forall opts ip op i m
   . ( Refined2C opts ip op i
     , Monad m
     , Show (PP ip i)
     ) => i
      -> RefinedT m (Refined5 opts ip op i)
newRefined5T = newRefined5TImpl (return . runIdentity)

-- | create a wrapped 'Refined5' type with an explicit proxy
newRefined5TP :: forall opts ip op i proxy m
   . ( Refined2C opts ip op i
     , Monad m
     , Show (PP ip i)
     ) => proxy '(opts,ip,op,i)
  -> i
  -> RefinedT m (Refined5 opts ip op i)
newRefined5TP _ = newRefined5TImpl (return . runIdentity)

-- | create a wrapped 'Refined5' type in IO
newRefined5TIO :: forall opts ip op i m
   . ( Refined2C opts ip op i
     , MonadIO m
     , Show (PP ip i)
     ) => i
      -> RefinedT m (Refined5 opts ip op i)
newRefined5TIO = newRefined5TImpl @IO @m liftIO

newRefined5TImpl :: forall n m opts ip op i
   . ( Refined2C opts ip op i
     , Monad m
     , MonadEval n
     , Show (PP ip i)
     ) => (forall x . n x -> RefinedT m x)
   -> i
   -> RefinedT m (Refined5 opts ip op i)
newRefined5TImpl f i = do
  (ret,mr) <- f $ eval5M i
  let m2 = prt2Impl (getOpt @opts) ret
  unlessNullM (m2Long m2) (tell . pure)
  case mr of
    Nothing -> throwError $ m2Desc m2 <> nullIf " | " (m2Short m2)
    Just r -> return r


newRefined5' :: forall opts ip op i m
  . ( MonadEval m
    , Refined2C opts ip op i
    , Show (PP ip i)
    )
  => i
  -> m (Either Msg2 (Refined5 opts ip op i))
newRefined5' = newRefined5P' Proxy

-- | same as 'newRefined5P' but runs in IO
newRefined5P' :: forall opts ip op i proxy m
  . ( MonadEval m
    , Refined2C opts ip op i
    , Show (PP ip i)
    )
  => proxy '(opts,ip,op,i)
  -> i
  -> m (Either Msg2 (Refined5 opts ip op i))
newRefined5P' _ i = do
  (ret,mr) <- eval5M i
  return $ maybe (Left $ prt2Impl (getOpt @opts) ret) Right mr

-- | pure version for extracting Refined5
--
-- >>> newRefined5 @OZ @(ReadBase Int 16 Id) @(Lt 255) "00fe"
-- Right (Refined5 254)
--
-- >>> newRefined5 @OZ @(ReadBase Int 16 Id) @(GuardBool (PrintF "0x%X is too large" Id) (Lt 253)) "00fe"
-- Left Step 2. Failed Boolean Check(op) | 0xFE is too large
--
-- >>> newRefined5 @OZ @(ReadBase Int 16 Id) @(Lt 255) "00fg"
-- Left Step 1. Initial Conversion(ip) Failed | invalid base 16
--
-- >>> newRefined5 @OL @(Map (ReadP Int Id) (Resplit "\\.")) @(Msg "length invalid:" (Len == 4)) "198.162.3.1.5"
-- Left Step 2. False Boolean Check(op) | {length invalid: 5 == 4}
--
-- >>> newRefined5 @OZ @(Map (ReadP Int Id) (Resplit "\\.")) @(GuardBool (PrintF "found length=%d" Len) (Len == 4)) "198.162.3.1.5"
-- Left Step 2. Failed Boolean Check(op) | found length=5
--
-- >>> newRefined5 @OZ @(Map (ReadP Int Id) (Resplit "\\.")) @(GuardBool (PrintF "found length=%d" Len) (Len == 4)) "198.162.3.1"
-- Right (Refined5 [198,162,3,1])
--
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> newRefined5 @OZ @(MkDayExtra Id >> 'Just Id) @(GuardBool "expected a Sunday" (Thd == 7)) (2019,10,13)
-- Right (Refined5 (2019-10-13,41,7))
--
-- >>> newRefined5 @OL @(MkDayExtra Id >> 'Just Id) @(Msg "expected a Sunday:" (Thd == 7)) (2019,10,12)
-- Left Step 2. False Boolean Check(op) | {expected a Sunday: 6 == 7}
--
-- >>> newRefined5 @OZ @(MkDayExtra' Fst Snd Thd >> 'Just Id) @(GuardBool "expected a Sunday" (Thd == 7)) (2019,10,12)
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
-- >>> newRefined5 @OL @Id @'True 22
-- Right (Refined5 22)
--
-- >>> newRefined5 @OL @(ReadP UTCTime Id) @(Between (MkDay '(2020,5,2)) (MkDay '(2020,5,7)) (MkJust ToDay)) "2020-05-04 12:13:14Z"
-- Right (Refined5 2020-05-04 12:13:14 UTC)
--
-- >>> newRefined5 @OL @(ReadP UTCTime Id) @(Between (MkDay '(2020,5,2)) (MkDay '(2020,5,7)) (MkJust ToDay)) "2020-05-08 12:13:14Z"
-- Left Step 2. False Boolean Check(op) | {Just 2020-05-08 <= Just 2020-05-07}
--
newRefined5 :: forall opts ip op i
  . ( Refined2C opts ip op i
    , Show (PP ip i)
    ) => i
    -> Either Msg2 (Refined5 opts ip op i)
newRefined5 = newRefined5P Proxy

newRefined5P :: forall opts ip op i
  . ( Refined2C opts ip op i
    , Show (PP ip i)
    ) => Proxy '(opts,ip,op,i)
    -> i
    -> Either Msg2 (Refined5 opts ip op i)
newRefined5P _ i =
  let (ret,mr) = runIdentity $ eval5M i
  in maybe (Left $ prt2Impl (getOpt @opts) ret) Right mr

eval5P :: forall opts ip op i m
  . ( Refined2C opts ip op i
    , MonadEval m
    )
  => Proxy '(opts,ip,op,i)
  -> i
  -> m (RResults2 (PP ip i), Maybe (Refined5 opts ip op i))
eval5P _ = eval5M

eval5M :: forall opts ip op i m
  . ( MonadEval m
    , Refined2C opts ip op i
    )
  => i
  -> m (RResults2 (PP ip i), Maybe (Refined5 opts ip op i))
eval5M i = do
  let o = getOpt @opts
  ll <- eval (Proxy @ip) o i
  case getValAndPE ll of
   (Right a, t1) -> do
     rr <- evalBool (Proxy @op) o a
     pure $ case getValAndPE rr of
      (Right True,t2) -> (RTTrue a t1 t2, Just (Refined5 a))
      (Right False,t2) -> (RTFalse a t1 t2, Nothing)
      (Left e,t2) -> (RTF a t1 e t2, Nothing)
   (Left e,t1) -> pure (RF e t1, Nothing)

-- | creates a 4-tuple proxy (see 'withRefined5TP' 'newRefined5TP' 'eval5P' 'newRefined5P')
--
-- use type application to set the 4-tuple or set the individual parameters directly
--
-- set the 4-tuple directly
--
-- >>> eg1 = mkProxy2 @'(OL, ReadP Int Id, Gt 10, String)
-- >>> newRefined5P eg1 "24"
-- Right (Refined5 24)
--
-- skip the 4-tuple and set each parameter individually using type application
--
-- >>> eg2 = mkProxy2 @_ @OL @(ReadP Int Id) @(Gt 10)
-- >>> newRefined5P eg2 "24"
-- Right (Refined5 24)
--

-- | type family for converting from a 4-tuple '(opts,ip,op,i) to a 'Refined5' type
type family MakeR5 p where
  MakeR5 '(opts,ip,op,i) = Refined5 opts ip op i

replaceOpt5 :: forall (opts :: Opt) opt0 ip op i
   . Refined5 opt0 ip op i
  -> Refined5 opts ip op i
replaceOpt5 = coerce

appendOpt5 :: forall (opts :: Opt) opt0 ip op i
   . Refined5 opt0 ip op i
  -> Refined5 (opt0 ':# opts) ip op i
appendOpt5 = coerce
