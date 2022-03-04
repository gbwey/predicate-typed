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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE EmptyDataDeriving #-}
-- | promoted String functions
module Predicate.Data.String (
  -- ** converters
    ToString
  , ToStringC (..)
  , FromString
  , FromString'

  -- ** predicates
  , IsPrefixCI
  , IsInfixCI
  , IsSuffixCI

  -- ** mutators
  , TrimBoth
  , TrimL
  , TrimR
  , StripR
  , StripL
 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Control.Lens
import Data.List (stripPrefix, dropWhileEnd)
import qualified Data.Text.Lens as DTL
import Data.Proxy (Proxy(Proxy))
import Data.Kind (Type)
import Data.String (IsString(..))
import Data.Char (isSpace, toLower)
import Data.Function (on)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Arrow (second)
import Data.Bool (bool)
import Data.These
import qualified Data.List.Lens
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as T
-- >>> import Predicate
-- >>> import qualified Data.Sequence as Seq

data TrimImpl (th :: These () ()) deriving Show

instance ( GetThese th
         , DTL.IsText x
         ) => P (TrimImpl th) x where
  type PP (TrimImpl th) x = x
  eval _ opts x =
    let (vv,fn) = case getThese @th of
                    These () () -> ("Both", dropWhile isSpace . dropWhileEnd isSpace)
                    This () -> ("L", dropWhile isSpace)
                    That () -> ("R", dropWhileEnd isSpace)
        msg0 = "Trim" ++ vv
        p = view DTL.unpacked x
        b =  fn p
     in pure $ mkNode opts (Val (b ^. DTL.packed)) (msg0 <> litL opts b <> litVerbose opts " | " p) []

-- | similar to 'Data.Text.stripStart'
--
-- >>> pz @(Snd >> TrimL) (20," abc   ")
-- Val "abc   "
--
data TrimL deriving Show
type TrimLT = TrimImpl ('This '())

instance P TrimLT x => P TrimL x where
  type PP TrimL x = PP TrimLT x
  eval _ = eval (Proxy @TrimLT)

-- | similar to 'Data.Text.stripEnd'
--
-- >>> pz @(Snd >> TrimR) (20," abc   ")
-- Val " abc"
--
-- >>> pz @("  abc " >> TrimR) ()
-- Val "  abc"
--
-- >>> pz @("" >> TrimR) ()
-- Val ""
--
data TrimR deriving Show
type TrimRT = TrimImpl ('That '())

instance P TrimRT x => P TrimR x where
  type PP TrimR x = PP TrimRT x
  eval _ = eval (Proxy @TrimRT)

-- | similar to 'Data.Text.strip'
--
-- >>> pz @(Snd >> TrimBoth) (20," abc   ")
-- Val "abc"
--
-- >>> pz @(Snd >> TrimBoth) (20,T.pack " abc   ")
-- Val "abc"
--
-- >>> pz @("         " >> TrimBoth) ()
-- Val ""
--
-- >>> pz @("" >> TrimBoth) ()
-- Val ""
--
data TrimBoth deriving Show
type TrimBothT = TrimImpl ('These '() '())

instance P TrimBothT x => P TrimBoth x where
  type PP TrimBoth x = PP TrimBothT x
  eval _ = eval (Proxy @TrimBothT)

data StripImpl(left :: Bool) p q deriving Show

instance ( GetBool lft
         , PP p x ~ String
         , P p x
         , DTL.IsText (PP q x)
         , P q x
         ) => P (StripImpl lft p q) x where
  type PP (StripImpl lft p q) x = Maybe (PP q x)
  eval _ opts x = do
    let msg0 = "Strip" <> bool "R" "L" lft
        lft = getBool @lft
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,view DTL.unpacked -> q,pp,qq) ->
        let b | lft = stripPrefix p q
              | otherwise = Data.List.Lens.stripSuffix p q
        in mkNode opts (Val (view DTL.packed <$> b)) (msg0 <> showL opts b <> litVerbose opts " | p=" p <> litVerbose opts " | q=" q) [hh pp, hh qq]

-- | similar to 'Data.Text.stripLeft'
--
-- >>> pz @(StripL "xyz" Id) "xyzHello"
-- Val (Just "Hello")
--
-- >>> pz @(StripL "xyz" Id) (T.pack "xyzHello")
-- Val (Just "Hello")
--
-- >>> pz @(StripL "xyz" Id) "xywHello"
-- Val Nothing
--
data StripL p q deriving Show
type StripLT p q = StripImpl 'True p q

instance P (StripLT p q) x => P (StripL p q) x where
  type PP (StripL p q) x = PP (StripLT p q) x
  eval _ = eval (Proxy @(StripLT p q))

-- | similar to 'Data.Text.stripRight'
--
-- >>> pz @(StripR "xyz" Id) "Hello xyz"
-- Val (Just "Hello ")
--
-- >>> pz @(StripR "xyz" Id) "xyzHelloxyw"
-- Val Nothing
--
-- >>> pz @(StripR "xyz" Id) ""
-- Val Nothing
--
-- >>> pz @(StripR "xyz" "xyz") ()
-- Val (Just "")
--
data StripR p q deriving Show
type StripRT p q = StripImpl 'False p q

instance P (StripRT p q) x => P (StripR p q) x where
  type PP (StripR p q) x = PP (StripRT p q) x
  eval _ = eval (Proxy @(StripRT p q))

data IsFixStringCI (cmp :: Ordering) p q deriving Show

instance ( P p x
         , P q x
         , PP p x ~ String
         , PP q x ~ String
         , GetOrdering cmp
         ) => P (IsFixStringCI cmp p q) x where
  type PP (IsFixStringCI cmp p q) x = Bool
  eval _ opts x = do
    let cmp = getOrdering @cmp
        (ff,msg0) = second (<> "CI") $ cmpOf cmp
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p',q',pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case chkSize2 opts msg0 p' q' hhs of
          Left e -> e
          Right ((_,p),(_,q)) ->
            let msg1 = joinStrings msg0 p
            in mkNodeB opts (on ff (map toLower) p q) (msg1 <> " " <> litL opts q) hhs

-- | similar to case insensitive 'Data.List.isPrefixOf' for strings
--
-- >>> pz @(IsPrefixCI "abc" "aBcbCd") ()
-- Val True
--
data IsPrefixCI p q deriving Show
type IsPrefixCIT p q = IsFixStringCI 'LT p q

instance P (IsPrefixCIT p q) x => P (IsPrefixCI p q) x where
  type PP (IsPrefixCI p q) x = PP (IsPrefixCIT p q) x
  eval _ = evalBool (Proxy @(IsPrefixCIT p q))

-- | similar to case insensitive 'Data.List.isInfixOf' for strings
--
-- >>> pl @(IsInfixCI "aB" Id) "xyzAbw"
-- True (IsInfixCI | aB xyzAbw)
-- Val True
--
-- >>> pz @(IsInfixCI "abc" "axAbCd") ()
-- Val True
--
data IsInfixCI p q deriving Show
type IsInfixCIT p q = IsFixStringCI 'EQ p q

instance P (IsInfixCIT p q) x => P (IsInfixCI p q) x where
  type PP (IsInfixCI p q) x = PP (IsInfixCIT p q) x
  eval _ = evalBool (Proxy @(IsInfixCIT p q))

-- | similar to case insensitive 'Data.List.isSuffixOf' for strings
--
data IsSuffixCI p q deriving Show
type IsSuffixCIT p q = IsFixStringCI 'GT p q

instance P (IsSuffixCIT p q) x => P (IsSuffixCI p q) x where
  type PP (IsSuffixCI p q) x = PP (IsSuffixCIT p q) x
  eval _ = evalBool (Proxy @(IsSuffixCIT p q))

-- | very simple conversion to a string
data ToString deriving Show
instance ToStringC x => P ToString x where
  type PP ToString x = String
  eval _ opts x = pure $ mkNode opts (Val (toStringC x)) "ToString" []

-- | convert string-like value to a string
class ToStringC (a :: Type) where
  toStringC :: a -> String
instance ToStringC String where
  toStringC = id
instance ToStringC T.Text where
  toStringC = T.unpack
instance ToStringC TL.Text where
  toStringC = TL.unpack
instance ToStringC BL8.ByteString where
  toStringC = BL8.unpack
instance ToStringC B8.ByteString where
  toStringC = B8.unpack

-- | 'fromString' function where you need to provide a reference to the type @t@ of the result
data FromString' t p deriving Show

instance ( P p a
         , PP p a ~ String
         , Show (PP t a)
         , IsString (PP t a)
         ) => P (FromString' t p) a where
  type PP (FromString' t p) a = PP t a
  eval _ opts a = do
    let msg0 = "FromString"
    pp <- eval (Proxy @p) opts a
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = fromString @(PP t a) p
        in mkNode opts (Val b) (msg0 <> " " <> showL opts b) [hh pp]

-- | 'fromString' function where you need to provide the type @t@ of the result
--
-- >>> pz @(FromString (Identity _) Id) "abc"
-- Val (Identity "abc")
--
-- >>> pz @(FromString (Seq.Seq Char) Id) "abc"
-- Val (fromList "abc")
--
data FromString (t :: Type) p deriving Show
type FromStringT (t :: Type) p = FromString' (Hole t) p

instance P (FromStringT t p) x => P (FromString t p) x where
  type PP (FromString t p) x = PP (FromStringT t p) x
  eval _ = eval (Proxy @(FromStringT t p))

