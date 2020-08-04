{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-compat #-}
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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     Dsl for evaluating and displaying type level expressions

     Contains instances of the class 'P' for evaluating expressions at the type level.
-}
module Predicate.Data.Json (

  -- ** aeson expressions
    ParseJson'
  , ParseJson
  , EncodeJson
  , EncodeJsonFile
  , ParseJsonFile'
  , ParseJsonFile
 ) where
import Predicate.Core
import Predicate.Util
import Data.Proxy
import Data.Typeable
import Data.Kind (Type)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import System.Directory (doesFileExist)
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import qualified Data.Map.Strict as M
-- >>> import qualified Data.Set as Set
-- >>> import qualified Data.Text as T
-- >>> import Safe (readNote)
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG

data ParseJson' t p

instance (P p x
        , PP p x ~ BL8.ByteString
        , Typeable (PP t x)
        , Show (PP t x)
        , A.FromJSON (PP t x)
        ) => P (ParseJson' t p) x where
  type PP (ParseJson' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "ParseJson " <> t
        t = showT @(PP t x)
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right s ->
        let hhs = [hh pp]
            msg1 = msg0 <> "(" ++ litBL opts { oWidth = oWidth opts `div` 3 } s ++ ")"
        in case A.eitherDecode' s of
           Right b -> mkNode opts (PresentT b) (msg0 <> " " ++ showL opts { oWidth = oWidth opts `div` 2 } b) hhs
           Left e -> mkNode opts (FailT (msg1 <> " " <> takeWhile (/=':') e) ) (e <> " | " <> litBL opts s) hhs

-- | parse json data
--
-- >>> pl @(ParseJson (Int,String) Id) "[10,\"abc\"]"
-- Present (10,"abc") (ParseJson (Int,[Char]) (10,"abc"))
-- PresentT (10,"abc")
--
-- >>> pl @(ParseJson (Int,String) Id) "[10,\"abc\",99]"
-- Error ParseJson (Int,[Char])([10,"abc",...) Error in $ (Error in $: cannot unpack array of length 3 into a tuple of length 2 | [10,"abc",99])
-- FailT "ParseJson (Int,[Char])([10,\"abc\",...) Error in $"
--
-- >>> pl @(ParseJson (Int,Bool) (FromString _ Id)) ("[1,true]" :: String)
-- Present (1,True) (ParseJson (Int,Bool) (1,True))
-- PresentT (1,True)
--
-- >>> pl @(ParseJson (Int,Bool) Id) (A.encode (1,True))
-- Present (1,True) (ParseJson (Int,Bool) (1,True))
-- PresentT (1,True)
--
-- >>> pl @(ParseJson () Id) "[1,true]"
-- Error ParseJson ()([1,true]) Error in $ (Error in $: parsing () failed, expected an empty array | [1,true])
-- FailT "ParseJson ()([1,true]) Error in $"
--
data ParseJson (t :: Type) p
type ParseJsonT (t :: Type) p = ParseJson' (Hole t) p

instance P (ParseJsonT t p) x => P (ParseJson t p) x where
  type PP (ParseJson t p) x = PP (ParseJsonT t p) x
  eval _ = eval (Proxy @(ParseJsonT t p))

data ParseJsonFile' t p

instance (P p x
        , PP p x ~ String
        , Typeable (PP t x)
        , Show (PP t x)
        , A.FromJSON (PP t x)
        ) => P (ParseJsonFile' t p) x where
  type PP (ParseJsonFile' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "ParseJsonFile " <> t
        t = showT @(PP t x)
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let hhs = [hh pp]
            msg1 = msg0 <> "(" <> p <> ")"
        mb <- runIO $ do
                b <- doesFileExist p
                if b then Just <$> BS8.readFile p
                else pure Nothing
        pure $ case mb of
          Nothing -> mkNode opts (FailT msg1) "" hhs
          Just Nothing -> mkNode opts (FailT (msg1 <> " file does not exist")) "" hhs
          Just (Just s) ->
            case A.eitherDecodeStrict' s of
               Right b -> mkNode opts (PresentT b) (msg1 <> " " ++ showL opts b) hhs
               Left e -> mkNode opts (FailT (msg1 <> " " <> takeWhile (/=':') e)) (e <> " | " <> litBS opts s) hhs

-- | parse a json file
--
-- >>> pz @(ParseJsonFile [A.Value] "test1.json" >> Id !! 2) ()
-- PresentT (Object (fromList [("lastName",String "Doe"),("age",Number 45.0),("firstName",String "John"),("likesPizza",Bool False)]))
--
data ParseJsonFile (t :: Type) p
type ParseJsonFileT (t :: Type) p = ParseJsonFile' (Hole t) p

instance P (ParseJsonFileT t p) x => P (ParseJsonFile t p) x where
  type PP (ParseJsonFile t p) x = PP (ParseJsonFileT t p) x
  eval _ = eval (Proxy @(ParseJsonFileT t p))

-- | encode json with pretty option
--
-- >>> pl @(EncodeJson 'False Id) (10,"def")
-- Present "[10,\"def\"]" (EncodeJson [10,"def"])
-- PresentT "[10,\"def\"]"
--
-- >>> pl @(EncodeJson 'False Id >> ParseJson (Int,Bool) Id) (1,True)
-- Present (1,True) ((>>) (1,True) | {ParseJson (Int,Bool) (1,True)})
-- PresentT (1,True)
--
data EncodeJson (pretty :: Bool) p

instance ( GetBool pretty
         , A.ToJSON (PP p x)
         , P p x
         ) => P (EncodeJson pretty p) x where
  type PP (EncodeJson pretty p) x = BL8.ByteString
  eval _ opts x = do
    let msg0 = "EncodeJson"
        pretty = getBool @pretty
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = (if pretty then AP.encodePretty else A.encode) p
        in mkNode opts (PresentT d) (msg0 <> " " <> litL opts (litBL opts d)) [hh pp]

-- | encode a json file with pretty option
data EncodeJsonFile (pretty :: Bool) p q

instance ( GetBool pretty
         , PP p x ~ String
         , P p x
         , A.ToJSON (PP q x)
         , P q x
         ) => P (EncodeJsonFile pretty p q) x where
  type PP (EncodeJsonFile pretty p q) x = ()
  eval _ opts x = do
    let msg0 = "EncodeJsonFile"
        pretty = getBool @pretty
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let d = (if pretty then AP.encodePretty else A.encode) q
            hhs = [hh pp, hh qq]
        mb <- runIO $ BL8.writeFile p d
        pure $ case mb of
          Nothing -> mkNode opts (FailT (msg0 <> " must run in IO")) "" hhs
          Just () -> mkNode opts (PresentT ()) (msg0 <> " " <> litL opts (litBL opts d)) hhs

