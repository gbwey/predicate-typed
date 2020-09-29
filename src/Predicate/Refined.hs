{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}
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
  , Msg0(..)
  , RefinedC

  -- ** create methods
  , newRefined
  , newRefined'

  -- ** QuickCheck method
  , genRefined

  -- ** unsafe create methods
  , unsafeRefined
  , unsafeRefined'

  , replaceOpt
  , appendOpt

 ) where
import Predicate.Core
import Predicate.Util
import Control.Lens
import Data.Proxy
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Language.Haskell.TH.Syntax as TH
import Test.QuickCheck
import qualified GHC.Read as GR
import qualified Text.ParserCombinators.ReadPrec as PCR
import qualified Text.Read.Lex as RL
import qualified Data.Binary as B
import Data.Binary (Binary)
import Data.String (IsString(..))
import Data.Hashable (Hashable(..))
import GHC.Stack (HasCallStack)
import Data.Coerce (coerce)
import Control.DeepSeq (NFData)
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> :m + Predicate.Prelude
-- >>> import qualified Control.Arrow as AR

-- | a simple refinement type that ensures the predicate @p@ holds for the type @a@
--
newtype Refined (opts :: Opt) p a = Refined a
  deriving stock (Show, TH.Lift)
  deriving newtype (Eq, Ord, NFData)

-- | extract the value from Refined
unRefined :: forall k (opts :: Opt) (p :: k) a. Refined opts p a -> a
unRefined (Refined a) = a

type role Refined phantom nominal nominal

-- | 'IsString' instance for Refined
--
-- >>> pureTryTest $ fromString @(Refined OL (ReadP Int Id >> Id > 244) String) "523"
-- Right (Refined "523")
--
-- >>> pureTryTest $ fromString @(Refined OL (ReadP Int Id >> Id > 244) String) "52"
-- Left ()
--
instance RefinedC opts p String => IsString (Refined opts p String) where
  fromString s =
    case newRefined @opts @p s of
      Left w -> error $ "Refined(fromString):" ++ errorDisplay (getOpt @opts) w
      Right r -> r

errorDisplay :: POpts -> Msg0 -> String
errorDisplay o (Msg0 _bp top e bpc) =
     bpc
  ++ nullIf " " top
  ++ (if null e || hasNoTree o then "" else "\n" ++ e)

-- | 'Read' instance for 'Refined'
--
-- >>> reads @(Refined OZ (0 <..> 299) Int) "Refined 254"
-- [(Refined 254,"")]
--
-- >>> reads @(Refined OZ (0 <..> 299) Int) "Refined 300"
-- []
--
-- >>> reads @(Refined OZ 'True Int) "Refined (-123)xyz"
-- [(Refined (-123),"xyz")]
--
instance ( RefinedC opts p a
         , Read a
         ) => Read (Refined opts p a) where
  readPrec
    = GR.parens
        (PCR.prec
           11
           (do GR.expectP (RL.Ident "Refined")
               fld0 <- PCR.reset GR.readPrec
               case newRefined @opts @p fld0 of -- since we cant display the failure message
                 Left _e -> fail ""
                 Right _r -> pure (Refined fld0)
           ))
  readList = GR.readListDefault
  readListPrec = GR.readListPrecDefault

-- | the constraints that 'Refined' must adhere to
type RefinedC opts p a = (OptC opts, PP p a ~ Bool, P p a)

-- | 'ToJSON' instance for 'Refined'
instance ToJSON a => ToJSON (Refined opts p a) where
  toJSON = toJSON . unRefined

-- | 'FromJSON' instance for 'Refined'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode' @(Refined OZ (Between 10 14 Id) Int) "13"
-- Right (Refined 13)
--
-- >>> removeAnsi $ A.eitherDecode' @(Refined OAN (Between 10 14 Id) Int) "16"
-- Error in $: Refined(FromJSON:parseJSON):FalseT (16 <= 14)
-- False 16 <= 14
-- |
-- +- P Id 16
-- |
-- +- P '10
-- |
-- `- P '14
-- <BLANKLINE>
--
instance ( RefinedC opts p a
         , FromJSON a
         ) => FromJSON (Refined opts p a) where
  parseJSON z = do
    a <- parseJSON z
    case newRefined @opts @p a of
      Left w -> fail $ "Refined(FromJSON:parseJSON):" ++ errorDisplay (getOpt @opts) w
      Right r -> return r

-- | 'Binary' instance for 'Refined'
--
-- >>> import Data.Time
-- >>> import Control.Lens
-- >>> import Control.Arrow ((+++))
-- >>> type K1 = Refined OZ (ReadP Day Id >> 'True) String
-- >>> type K2 = Refined OAN (Between (ReadP Day "2019-05-30") (ReadP Day "2019-06-01") (ReadP Day Id)) String
-- >>> r = unsafeRefined' @OZ "2019-04-23" :: K1
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K1 (B.encode r)
-- Refined "2019-04-23"
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K2 (B.encode r)
-- Refined(Binary:get):FalseT (2019-05-30 <= 2019-04-23)
-- False 2019-05-30 <= 2019-04-23
-- |
-- +- P ReadP Day 2019-04-23
-- |  |
-- |  `- P Id "2019-04-23"
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
instance ( RefinedC opts p a
         , Binary a
         ) => Binary (Refined opts p a) where
  get = do
    fld0 <- B.get @a
    case newRefined @opts @p fld0 of
      Left w -> fail $ "Refined(Binary:get):" ++ errorDisplay (getOpt @opts) w
      Right r -> return r
  put (Refined r) = B.put @a r

-- | 'Hashable' instance for 'Refined'
instance ( RefinedC opts p a
         , Hashable a
         ) => Hashable (Refined opts p a) where
  hashWithSalt s (Refined a) = s + hash a

-- | 'Arbitrary' instance for 'Refined'
--
-- >>> xs <- generate (vectorOf 10 (arbitrary @(Refined OAN (Id /= 0) Int)))
-- >>> all ((/=0) . unRefined) xs
-- True
--
-- >>> xs <- generate (vectorOf 10 (arbitrary @(Refined OAN IsPrime Int)))
-- >>> all (isPrime . unRefined) xs
-- True
--
instance ( Arbitrary a
         , RefinedC opts p a
         , Show a
         ) => Arbitrary (Refined opts p a) where
  arbitrary = genRefined arbitrary

-- | create 'Refined' generator using a generator to restrict the values
genRefined :: forall opts p a .
   ( RefinedC opts p a
   , HasCallStack
   )
   => Gen a
   -> Gen (Refined opts p a)
genRefined g =
  let f !cnt = do
        ma <- suchThatMaybe g $ \a -> evalQuick @opts @p a == Right True
        case ma of
          Nothing ->
             let o = getOpt @opts
             in if cnt >= oRecursion o
                then error $ setOtherEffects o ("genRefined recursion exceeded(" ++ show (oRecursion o) ++ ")")
                else f (cnt+1)
          Just a -> pure $ unsafeRefined a
  in f 0

{-
prtRefinedIO :: forall opts p a
   . RefinedC opts p a
   => a
   -> IO (Either (BoolT Bool) (Refined opts p a))
prtRefinedIO a = do
  let o = getOpt @opts
  tt <- evalBool (Proxy @p) o a
  let r = _ttBool tt
  case oDebug o of
     DZero -> pure ()
     DLite -> putStrLn $ colorBoolT o r <> " " <> topMessage tt
     _ -> putStrLn $ prtTree o tt
  pure $ case getValueLR o "" tt [] of
    Right True -> Right (Refined a)
    _ -> Left r
-}
data Msg0 = Msg0 { m0BoolT :: !(BoolT Bool)
                 , m0Short :: !String
                 , m0Long :: !String
                 , m0BoolTColor :: !String
                 } deriving Eq

instance Show Msg0 where
  show (Msg0 _a _b c _d) = c
{-
newRefined' :: forall opts p a m
   . ( MonadEval m
     , RefinedC opts p a
     )
   => a
   -> m (Either String (Refined opts p a))
newRefined' a = do
  lr <- newRefined' @opts @p a
  return $ case lr of
    Left (Msg0 _bp top e bpc) ->
      case oDebug (getOpt @opts) of
        DZero -> Left bpc
        DLite -> Left (bpc <> nullIf " " top)
        _ -> Left e
    Right r -> return r
-}
newRefinedMInternal :: forall opts p a m
   . ( MonadEval m
     , RefinedC opts p a
     )
   => a
   -> m (Msg0, Maybe (Refined opts p a))
newRefinedMInternal a = do
  let o = getOpt @opts
  pp <- evalBool (Proxy @p) o a
  let r = colorBoolT' o (_ttBool pp)
      s = prtTree o pp
      msg0 = Msg0 (_ttBool pp) (topMessage pp) s r
  pure $ (msg0,) $ case getValueLR o "" pp [] of
       Right True -> Just (Refined a)
       _ -> Nothing

newRefined' :: forall opts p a m
   . ( MonadEval m
     , RefinedC opts p a
     )
   => a
   -> m (Either Msg0 (Refined opts p a))
newRefined' = fmap maybeToEither . newRefinedMInternal

-- | returns a 'Refined' value if @a@ is valid for the predicate @p@
--
-- >>> newRefined @OL @(ReadP Int Id > 99) "123"
-- Right (Refined "123")
--
-- >>> AR.left m0Long $ newRefined @OL @(ReadP Int Id > 99) "12"
-- Left "False (12 > 99)"
--
-- >>> newRefined @OZ @(Between 10 14 Id) 13
-- Right (Refined 13)
--
-- >>> AR.left m0BoolT $ newRefined @OZ @(Between 10 14 Id) 99
-- Left FalseT
--
-- >>> newRefined @OZ @(Last >> Len == 4) ["one","two","three","four"]
-- Right (Refined ["one","two","three","four"])
--
-- >>> newRefined @OZ @(Re "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$") "141.213.1.99"
-- Right (Refined "141.213.1.99")
--
-- >>> AR.left m0BoolT $ newRefined @OZ @(Re "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$") "141.213.1"
-- Left FalseT
--
-- >>> AR.left m0BoolT $ newRefined @OZ @(Map (ReadP Int Id) (Resplit "\\.") >> GuardBool (PrintF "bad length: found %d" Len) (Len == 4)) "141.213.1"
-- Left (FailT "bad length: found 3")
--
-- >>> AR.left m0BoolT $ newRefined @OZ @(Map (ReadP Int Id) (Resplit "\\.") >> GuardBool (PrintF "bad length: found %d" Len) (Len == 4) && BoolsN (PrintT "octet %d out of range %d" Id) 4 (0 <..> 0xff)) "141.213.1.444"
-- Left (FailT "Bool(3) [octet 3 out of range 444]")
--
-- >>> AR.left m0BoolT $ newRefined @OZ @(Map (ReadP Int Id) (Resplit "\\.") >> GuardBool (PrintF "bad length: found %d" Len) (Len == 4) && BoolsN (PrintT "octet %d out of range %d" Id) 4 (0 <..> 0xff)) "141.213.1x34.444"
-- Left (FailT "ReadP Int (1x34)")
--
-- >>> newRefined @OZ @(Map ('[Id] >> ReadP Int Id) Id >> IsLuhn) "12344"
-- Right (Refined "12344")
--
-- >>> AR.left m0BoolT $ newRefined @OZ @(Map ('[Id] >> ReadP Int Id) Id >> IsLuhn) "12340"
-- Left FalseT
--
-- >>> newRefined @OZ @(Any IsPrime) [11,13,17,18]
-- Right (Refined [11,13,17,18])
--
-- >>> AR.left m0BoolT $ newRefined @OZ @(All IsPrime) [11,13,17,18]
-- Left FalseT
--
-- >>> newRefined @OZ @(Snd !! Fst >> Len > 5) (2,["abc","defghij","xyzxyazsfd"])
-- Right (Refined (2,["abc","defghij","xyzxyazsfd"]))
--
-- >>> AR.left m0BoolT $ newRefined @OZ @(Snd !! Fst >> Len > 5) (27,["abc","defghij","xyzxyazsfd"])
-- Left (FailT "(!!) index not found")
--
-- >>> AR.left m0BoolT $ newRefined @OZ @(Snd !! Fst >> Len <= 5) (2,["abc","defghij","xyzxyazsfd"])
-- Left FalseT
--
newRefined :: forall opts p a
    . RefinedC opts p a
   => a
   -> Either Msg0 (Refined opts p a)
newRefined = runIdentity . newRefined'

maybeToEither :: (a,Maybe b) -> Either a b
maybeToEither (a,mb) = maybe (Left a) Right mb

-- | create an unsafe 'Refined' value without running the predicate
unsafeRefined :: forall opts p a . a -> Refined opts p a
unsafeRefined = Refined

-- | create an unsafe 'Refined' value and also run the predicate
unsafeRefined' :: forall opts p a
  . ( RefinedC opts p a
    , HasCallStack
    ) => a -> Refined opts p a
unsafeRefined' a =
  let o = getOpt @opts
      tt = runIdentity $ evalBool (Proxy @p) o a
  in case getValueLR o "" tt [] of
       Right True -> Refined a
       _ -> let s = prtTree o tt
                bp = colorBoolT' o (_ttBool tt)
            in case oDebug o of
                 DZero -> error bp
                 DLite -> error $ bp ++ "\n" ++ s
                 _ -> error $ bp ++ "\n" ++ s

replaceOpt :: forall (opts :: Opt) opt0 p a . Refined opt0 p a -> Refined opts p a
replaceOpt = coerce

appendOpt :: forall (opts :: Opt) opt0 p a . Refined opt0 p a -> Refined (opt0 ':# opts) p a
appendOpt = coerce
