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
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE EmptyDataDeriving #-}
-- | promoted json encoding and decoding functions
module Predicate.Data.Json (
  -- ** parse
    ParseJson'
  , ParseJson
  , ParseJsonFile'
  , ParseJsonFile

  -- ** encode
  , EncodeJson
  , EncodeJsonFile
 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import Data.Kind (Type)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import System.Directory (doesFileExist)
import Data.Bool (bool)
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> import Predicate

-- | parse json data using the type @t@
data ParseJson' t p deriving Show

instance ( P p x
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
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right s ->
        let hhs = [hh pp]
            msg1 = msg0 <> "(" ++ litBL opts { oWidth = oWidth opts `div` 3 } s ++ ")"
        in case A.eitherDecode' s of
           Right b -> mkNode opts (Val b) (msg0 <> " " ++ showL opts { oWidth = oWidth opts `div` 2 } b) hhs
           Left e -> mkNode opts (Fail (msg1 <> " " <> e)) (litBL opts s) hhs

-- | parse json data using the type @t@
--
-- >>> pl @(ParseJson (Int,String) Id) "[10,\"abc\"]"
-- Present (10,"abc") (ParseJson (Int,[Char]) (10,"abc"))
-- Val (10,"abc")
--
-- >>> pl @(ParseJson (Int,String) Id) "[10,\"abc\",99]"
-- Error ParseJson (Int,[Char])([10,"abc",99]) Error in $: cannot unpack array of length 3 into a tuple of length 2 ([10,"abc",99])
-- Fail "ParseJson (Int,[Char])([10,\"abc\",99]) Error in $: cannot unpack array of length 3 into a tuple of length 2"
--
-- >>> pl @(ParseJson (Int,Bool) (FromString _ Id)) "[1,true]"
-- Present (1,True) (ParseJson (Int,Bool) (1,True))
-- Val (1,True)
--
-- >>> pl @(ParseJson (Int,Bool) Id) (A.encode (1,True))
-- Present (1,True) (ParseJson (Int,Bool) (1,True))
-- Val (1,True)
--
-- >>> pl @(ParseJson () Id) "[1,true]"
-- Error ParseJson ()([1,true]) Error in $: parsing () failed, expected an empty array ([1,true])
-- Fail "ParseJson ()([1,true]) Error in $: parsing () failed, expected an empty array"
--
data ParseJson (t :: Type) p deriving Show
type ParseJsonT (t :: Type) p = ParseJson' (Hole t) p

instance P (ParseJsonT t p) x => P (ParseJson t p) x where
  type PP (ParseJson t p) x = PP (ParseJsonT t p) x
  eval _ = eval (Proxy @(ParseJsonT t p))

-- | parse json file @p@ using the type @t@
data ParseJsonFile' t p deriving Show

instance ( P p x
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
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let hhs = [hh pp]
            msg1 = msg0 <> "(" <> p <> ")"
        mb <- runIO $
                ifM (doesFileExist p)
                    (Just <$> BS8.readFile p)
                    mempty
        pure $ case mb of
          Nothing -> mkNode opts (Fail (msg1 <> " must run in IO")) "" hhs
          Just Nothing -> mkNode opts (Fail (msg1 <> " file does not exist")) "" hhs
          Just (Just s) ->
            case A.eitherDecodeStrict' s of
               Right b -> mkNode opts (Val b) (msg1 <> " " ++ showL opts b) hhs
               Left e -> mkNode opts (Fail (msg1 <> " " <> e)) (litBS opts s) hhs

-- | parse a json file @p@ using the type @t@
--
-- >>> pz @(ParseJsonFile [A.Value] "test1.json" >> Id !! 2) ()
-- Val (Object (fromList [("lastName",String "Doe"),("age",Number 45.0),("firstName",String "John"),("likesPizza",Bool False)]))
--
data ParseJsonFile (t :: Type) p deriving Show
type ParseJsonFileT (t :: Type) p = ParseJsonFile' (Hole t) p

instance P (ParseJsonFileT t p) x => P (ParseJsonFile t p) x where
  type PP (ParseJsonFile t p) x = PP (ParseJsonFileT t p) x
  eval _ = eval (Proxy @(ParseJsonFileT t p))

-- | encode json with pretty option
--
-- >>> pl @(EncodeJson 'False Id) (10,"def")
-- Present "[10,\"def\"]" (EncodeJson [10,"def"])
-- Val "[10,\"def\"]"
--
-- >>> pl @(EncodeJson 'False Id >> ParseJson (Int,Bool) Id) (1,True)
-- Present (1,True) ((>>) (1,True) | {ParseJson (Int,Bool) (1,True)})
-- Val (1,True)
--
data EncodeJson (pretty :: Bool) p deriving Show

instance ( GetBool pretty
         , A.ToJSON (PP p x)
         , P p x
         ) => P (EncodeJson pretty p) x where
  type PP (EncodeJson pretty p) x = BL8.ByteString
  eval _ opts x = do
    let msg0 = "EncodeJson"
        pretty = getBool @pretty
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = bool A.encode AP.encodePretty pretty p
        in mkNode opts (Val d) (msg0 <> " " <> litL opts (litBL opts d)) [hh pp]

-- | encode a json file with pretty option
data EncodeJsonFile (pretty :: Bool) p q deriving Show

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
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let d = bool A.encode AP.encodePretty pretty q
            hhs = [hh pp, hh qq]
        mb <- runIO $ BL8.writeFile p d
        pure $ case mb of
          Nothing -> mkNode opts (Fail (msg0 <> " must run in IO")) "" hhs
          Just () -> mkNode opts (Val ()) (msg0 <> " " <> litL opts (litBL opts d)) hhs

