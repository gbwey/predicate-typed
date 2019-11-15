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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE RoleAnnotations #-}
{- |
     Refinement type allowing the external type to differ from the internal type
     see 'Refined2'
-}
module Predicate.Refined2 (

  -- ** Refined2
    Refined2(r2In,r2Out)
  , Refined2C

 -- ** display results
  , prtEval2IO
  , prtEval2PIO
  , prtEval2P
  , prtEval2
  , prt2IO
  , prt2
  , prt2Impl
  , Msg2 (..)
  , Results (..)
  , RResults (..)

  -- ** evaluation methods
  , eval2
  , eval2P

  -- ** create a wrapped Refined2 value
  , withRefined2TIO
  , withRefined2T
  , newRefined2T
  , newRefined2TP
  , newRefined2TIO

  -- ** unsafe methods for creating Refined2
  , unsafeRefined2
  , unsafeRefined2'

  -- ** miscellaneous
  , MakeR2
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
--
-- Although a common scenario is String as input, you are free to choose any input type you like
--
-- >>> prtEval2 @(ReadBase Int 16 Id) @(Lt 255) oz "00fe"
-- Right (Refined2 {r2In = 254, r2Out = "00fe"})
--
-- >>> prtEval2 @(ReadBase Int 16 Id) @(Lt 253) oz "00fe"
-- Left Step 2. False Boolean Check(op) | FalseP
--
-- >>> prtEval2 @(ReadBase Int 16 Id) @(Lt 255) oz "00fg"
-- Left Step 1. Initial Conversion(ip) Failed | invalid base 16
--
-- >>> prtEval2 @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Msg "length invalid:" (Len == 4)) ol "198.162.3.1.5"
-- Left Step 2. False Boolean Check(op) | {length invalid:5 == 4}
--
-- >>> prtEval2 @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Guard (PrintF "found length=%d" Len) (Len == 4) >> 'True) oz "198.162.3.1.5"
-- Left Step 2. Failed Boolean Check(op) | found length=5
--
-- >>> prtEval2 @(Map (ReadP Int Id) (Resplit "\\." Id)) @(Guard (PrintF "found length=%d" Len) (Len == 4) >> 'True) oz "198.162.3.1"
-- Right (Refined2 {r2In = [198,162,3,1], r2Out = "198.162.3.1"})
--
-- >>> :m + Data.Time.Calendar.WeekDate
-- >>> prtEval2 @(MkDay >> 'Just Id) @(Guard "expected a Sunday" (Thd Id == 7) >> 'True) oz (2019,10,13)
-- Right (Refined2 {r2In = (2019-10-13,41,7), r2Out = (2019,10,13)})
--
-- >>> prtEval2 @(MkDay >> 'Just Id) @(Msg "expected a Sunday:" (Thd Id == 7)) ol (2019,10,12)
-- Left Step 2. False Boolean Check(op) | {expected a Sunday:6 == 7}
--
-- >>> prtEval2 @(MkDay' (Fst Id) (Snd Id) (Thd Id) >> 'Just Id) @(Guard "expected a Sunday" (Thd Id == 7) >> 'True) oz (2019,10,12)
-- Left Step 2. Failed Boolean Check(op) | expected a Sunday
--
data Refined2 ip op i = Refined2 { r2In :: PP ip i, r2Out :: i }

type role Refined2 nominal nominal nominal

-- | directly load values into 'Refined2'. It still checks to see that those values are valid
unsafeRefined2' :: forall ip op i
                . (Show (PP ip i), Refined2C ip op i)
                => POpts
                -> i
                -> Refined2 ip op i
unsafeRefined2' opts i =
  let (ret,mr) = eval2 @ip @op opts i
  in fromMaybe (error $ show (prt2Impl opts ret)) mr

-- | directly load values into 'Refined2' without any checking
unsafeRefined2 :: forall ip op i . PP ip i -> i -> Refined2 ip op i
unsafeRefined2 = Refined2

-- | Provides the constraints on Refined2
type Refined2C ip op i =
       ( P ip i
       , P op (PP ip i)
       , PP op (PP ip i) ~ Bool   -- the internal value needs to pass the predicate check
       )

deriving instance (Show i, Show (PP ip i)) => Show (Refined2 ip op i)
deriving instance (Eq i, Eq (PP ip i)) => Eq (Refined2 ip op i)
deriving instance (TH.Lift (PP ip i), TH.Lift i) => TH.Lift (Refined2 ip op i)

instance (Refined2C ip op String, Show (PP ip String)) => IsString (Refined2 ip op String) where
  fromString s =
    let (ret,mr) = eval2 @ip @op o2 s
    in case mr of
         Nothing -> error $ "Refined2(fromString):" ++ show (prt2Impl o2 ret)
         Just r -> r


-- read instance from -ddump-deriv
-- | 'Read' instance for 'Refined2'
--
-- >>> reads @(Refined2 (ReadBase Int 16 Id) (Between 0 255) String) "Refined2 {r2In = 254, r2Out = \"fe\"}"
-- [(Refined2 {r2In = 254, r2Out = "fe"},"")]
--
-- >>> reads @(Refined2 (ReadBase Int 16 Id) (Between 0 255) String) "Refined2 {r2In = 300, r2Out = \"12c\"}"
-- []
--
-- >>> reads @(Refined2 (ReadBase Int 16 Id) (Id < 0) String) "Refined2 {r2In = -1234, r2Out = \"-4d2\"}"
-- [(Refined2 {r2In = -1234, r2Out = "-4d2"},"")]
--
-- >>> reads @(Refined2 (Map (ReadP Int Id) (Resplit "\\." Id)) (Guard "len/=4" (Len == 4) >> 'True) String) "Refined2 {r2In = [192,168,0,1], r2Out = \"192.168.0.1\"}"
-- [(Refined2 {r2In = [192,168,0,1], r2Out = "192.168.0.1"},"")]
--
instance ( Eq i
         , Show i
         , Show (PP ip i)
         , Refined2C ip op i
         , Read (PP ip i)
         , Read i
         ) => Read (Refined2 ip op i) where
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

                 let lr = getValLRFromTT $ runIdentity $ evalBool (Proxy @op) oz fld1
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
-- >>> A.encode (unsafeRefined2 @(ReadBase Int 16 Id) @(Between 0 255) 254 "fe")
-- "\"fe\""
--
-- >>> A.encode (unsafeRefined2 @Id @'True @Int 123 123)
-- "123"
--
instance ToJSON i => ToJSON (Refined2 ip op i) where
  toJSON = toJSON . r2Out


-- | 'FromJSON' instance for 'Refined2'
--
-- >>> import qualified Data.Aeson as A
-- >>> A.eitherDecode' @(Refined2 (ReadBase Int 16 Id) (Id > 10 && Id < 256) String) "\"00fe\""
-- Right (Refined2 {r2In = 254, r2Out = "00fe"})
--
-- >>> removeAnsi $ A.eitherDecode' @(Refined2 (ReadBase Int 16 Id) (Id > 10 && Id < 256) String) "\"00fe443a\""
-- Error in $: Refined2:Step 2. False Boolean Check(op) | {True && False | (16663610 < 256)}
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
instance (Show i
        , Show (PP ip i)
        , Refined2C ip op i
        , FromJSON i
        ) => FromJSON (Refined2 ip op i) where
  parseJSON z = do
                  i <- parseJSON @i z
                  let (ret,mr) = eval2 @ip @op o2 i
                  case mr of
                    Nothing -> fail $ "Refined2:" ++ show (prt2Impl o2 ret)
                    Just r -> return r


-- | 'Binary' instance for 'Refined2'
--
-- >>> import Control.Arrow ((+++))
-- >>> import Control.Lens
-- >>> import Data.Time
-- >>> type K1 = Refined2 (ReadP Day Id) 'True String
-- >>> type K2 = Refined2 (ReadP Day Id) (Between (ReadP Day "2019-03-30") (ReadP Day "2019-06-01")) String
-- >>> type K3 = Refined2 (ReadP Day Id) (Between (ReadP Day "2019-05-30") (ReadP Day "2019-06-01")) String
-- >>> r = unsafeRefined2' oz "2019-04-23" :: K1
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K1 (B.encode r)
-- Refined2 {r2In = 2019-04-23, r2Out = "2019-04-23"}
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K2 (B.encode r)
-- Refined2 {r2In = 2019-04-23, r2Out = "2019-04-23"}
--
-- >>> removeAnsi $ (view _3 +++ view _3) $ B.decodeOrFail @K3 (B.encode r)
-- Refined2:Step 2. False Boolean Check(op) | {2019-05-30 <= 2019-04-23}
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
instance ( Show i
         , Show (PP ip i)
         , Refined2C ip op i
         , Binary i
         ) => Binary (Refined2 ip op i) where
  get = do
          i <- B.get @i
          let (ret,mr) = eval2 @ip @op o2 i
          case mr of
            Nothing -> fail $ "Refined2:" ++ show (prt2Impl o2 ret)
            Just r -> return r
  put (Refined2 _ r) = B.put @i r

withRefined2TIO :: forall ip op i m b
  . (MonadIO m, Refined2C ip op i, Show (PP ip i))
  => POpts
  -> i
  -> (Refined2 ip op i -> RefinedT m b)
  -> RefinedT m b
withRefined2TIO opts = (>>=) . newRefined2TIO @_ @ip @op @i opts

-- | create a 'Refined2' value and pass it to a continuation to be processed
--
-- This first example reads a hex string and makes sure it is between 100 and 200 and then
-- reads a binary string and adds the values together
--
-- >>> :set -XPolyKinds
-- >>> prtRefinedTIO $ withRefined2T @(ReadBase Int 16 Id) @(Between 100 200) oz "a3" $ \x -> withRefined2T @(ReadBase Int 2 Id) @'True oz "1001110111" $ \y -> pure (r2In x + r2In y)
-- 794
--
-- this example fails as the the hex value is out of range
--
-- >>> prtRefinedTIO $ withRefined2T @(ReadBase Int 16 Id) @(Between 100 200) o0 "a388" $ \x -> withRefined2T @(ReadBase Int 2 Id) @'True o0 "1001110111" $ \y -> pure (x,y)
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
withRefined2T :: forall ip op i m b
  . (Monad m, Refined2C ip op i, Show (PP ip i))
  => POpts
  -> i
  -> (Refined2 ip op i -> RefinedT m b)
  -> RefinedT m b
withRefined2T opts = (>>=) . newRefined2T @_ @ip @op @i opts

-- | create a wrapped 'Refined2' type
--
-- >>> prtRefinedTIO $ newRefined2T @_ @(MkDay >> Just Id) @(Thd Id == 5) ol (2019,11,1)
-- Refined2 {r2In = (2019-11-01,44,5), r2Out = (2019,11,1)}
--
-- >>> prtRefinedTIO $ newRefined2T @_ @(MkDay >> Just Id) @(Thd Id == 5) ol (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {6 == 5}]
--
-- >>> prtRefinedTIO $ newRefined2T @_ @(MkDay >> Just Id) @(Msg "wrong day:" (Thd Id == 5)) ol (2019,11,2)
-- failure msg[Step 2. False Boolean Check(op) | {wrong day:6 == 5}]
--
newRefined2T :: forall m ip op i
   . (Refined2C ip op i
    , Monad m
    , Show (PP ip i)
    ) => POpts
  -> i
  -> RefinedT m (Refined2 ip op i)
newRefined2T = newRefined2TImpl (return . runIdentity)

newRefined2TP :: forall m ip op i proxy
   . (Refined2C ip op i
   , Monad m
   , Show (PP ip i)
   ) => proxy '(ip,op,i)
  -> POpts
  -> i
  -> RefinedT m (Refined2 ip op i)
newRefined2TP _ = newRefined2TImpl (return . runIdentity)


newRefined2TIO :: forall m ip op i
   . (Refined2C ip op i
    , MonadIO m
    , Show (PP ip i)
    ) => POpts
  -> i
  -> RefinedT m (Refined2 ip op i)
newRefined2TIO = newRefined2TImpl liftIO

newRefined2TImpl :: forall n m ip op i
   . (Refined2C ip op i
    , Monad m
    , MonadEval n
    , Show (PP ip i)
   ) => (forall x . n x -> RefinedT m x)
   -> POpts
   -> i
   -> RefinedT m (Refined2 ip op i)
newRefined2TImpl f opts i = do
  (ret,mr) <- f $ eval2M opts i
  let m2 = prt2Impl opts ret
  tell [m2Long m2]
  case mr of
    Nothing -> throwError $ m2Desc m2 <> " | " <> m2Short m2
    Just r -> return r

data Results a =
       XF String        -- Left e
     | XTF a String     -- Right a + Left e
     | XTFalse a        -- Right a + Right False
     | XTTrue a
     deriving (Show,Eq)

-- | An ADT that summarises the results of evaluating Refined2 representing all possible states
data RResults a =
       RF String (Tree PE)        -- fails initial conversion
     | RTF a (Tree PE) String (Tree PE)    -- op fails
     | RTFalse a (Tree PE) (Tree PE)        -- op false
     | RTTrue a (Tree PE) (Tree PE) -- op true
     deriving Show

-- | same as 'prtEval2' but runs in IO
prtEval2IO :: forall ip op i
  . ( Refined2C ip op i
    , Show (PP ip i)
    ) => POpts
  -> i
  -> IO (Either String (Refined2 ip op i))
prtEval2IO opts i = do
  x <- eval2M opts i
  prt2IO opts x

-- | same as 'prtEval2P' but runs in IO
prtEval2PIO :: forall ip op i proxy
  . ( Refined2C ip op i
    , Show (PP ip i)
    ) => proxy '(ip,op,i)
  -> POpts
  -> i
  -> IO (Either String (Refined2 ip op i))
prtEval2PIO _ opts i = do
  x <- eval2M opts i
  prt2IO opts x


prtEval2 :: forall ip op i
  . ( Refined2C ip op i
    , Show (PP ip i)
  ) => POpts
  -> i
  -> Either Msg2 (Refined2 ip op i)
prtEval2 opts = prt2 opts . eval2 opts

prtEval2P :: forall ip op i
  . ( Refined2C ip op i
    , Show (PP ip i)
  ) => Proxy '(ip,op,i)
  -> POpts
  -> i
  -> Either Msg2 (Refined2 ip op i)
prtEval2P _ opts = prt2 opts . eval2 opts

eval2P :: forall ip op i . Refined2C ip op i
  => Proxy '(ip,op,i)
  -> POpts
  -> i
  -> (RResults (PP ip i), Maybe (Refined2 ip op i))
eval2P _ opts = runIdentity . eval2M opts

eval2 :: forall ip op i . Refined2C ip op i
  => POpts
  -> i
  -> (RResults (PP ip i), Maybe (Refined2 ip op i))
eval2 opts = runIdentity . eval2M opts

eval2M :: forall m ip op i . (MonadEval m, Refined2C ip op i)
  => POpts
  -> i
  -> m (RResults (PP ip i), Maybe (Refined2 ip op i))
eval2M opts i = do
  ll <- eval (Proxy @ip) opts i
  case getValAndPE ll of
   (Right a, t1) -> do
     rr <- evalBool (Proxy @op) opts a
     pure $ case getValAndPE rr of
      (Right True,t2) -> (RTTrue a t1 t2, Just (Refined2 a i))
      (Right False,t2) -> (RTFalse a t1 t2, Nothing)
      (Left e,t2) -> (RTF a t1 e t2, Nothing)
   (Left e,t1) -> pure (RF e t1, Nothing)

prt2IO :: Show a => POpts -> (RResults a, Maybe r) -> IO (Either String r)
prt2IO opts (ret,mr) = do
  let m2 = prt2Impl opts ret
  unless (hasNoTree opts) $ putStrLn $ m2Long m2
  return $ maybe (Left (m2Desc m2 <> " | " <> m2Short m2)) Right mr

prt2 :: Show a => POpts -> (RResults a, Maybe r) -> Either Msg2 r
prt2 opts (ret,mr) = maybe (Left $ prt2Impl opts ret) Right mr

data Msg2 = Msg2 { m2Desc :: String, m2Short :: String, m2Long :: String } deriving Eq
instance Show Msg2 where
  show (Msg2 a b c) = a <> " | " <> b <> (if null c then "" else "\n" <> c)

prt2Impl :: Show a
  => POpts
  -> RResults a
  -> Msg2
prt2Impl opts v =
  let outmsg msg = "\n*** " <> msg <> " ***\n\n"
      msg1 a = outmsg ("Step 1. Success Initial Conversion(ip) [" ++ show a ++ "]")
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
             z = case t2 ^? root . pStrings . ix 0 of
                   Just w -> if null (dropWhile isSpace w) then "FalseP" else "{" <> w <> "}"
                   Nothing -> "FalseP"
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


type family MakeR2 p where
  MakeR2 '(ip,op,i) = Refined2 ip op i
