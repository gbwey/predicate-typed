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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     promoted String functions
-}
module Predicate.Data.String (
    TrimBoth
  , TrimL
  , TrimR
  , StripR
  , StripL

  , IsPrefixC
  , IsInfixC
  , IsSuffixC
  , IsPrefixCI
  , IsInfixCI
  , IsSuffixCI

  , ToString
  , FromString
  , FromString'
 ) where
import Predicate.Core
import Predicate.Util
import qualified GHC.TypeLits as GL
import Control.Lens hiding (iall)
import Data.List (dropWhileEnd, isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.Text.Lens as DTL
import Data.Proxy
import Data.Kind (Type)
import Data.String
import Data.Char
import Data.Function
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import qualified Data.Text as T
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Sequence as Seq

data TrimImpl (left :: Bool) (right :: Bool)

instance (FailUnlessT (OrT l r)
           ('GL.Text "TrimImpl: left and right cannot both be False")
        , GetBool l
        , GetBool r
        , DTL.IsText x
        ) => P (TrimImpl l r) x where
  type PP (TrimImpl l r) x = x
  eval _ opts x =
    let msg0 = "Trim" ++ (if l && r then "Both" else if l then "L" else "R")
        l = getBool @l
        r = getBool @r
        p = view DTL.unpacked x
        fl = if l then dropWhile isSpace else id
        fr = if r then dropWhileEnd isSpace else id
        b =  (fl . fr) p
     in pure $ mkNode opts (PresentT (b ^. DTL.packed)) (msg0 <> litL opts b <> litVerbose opts " | " p) []

-- | similar to 'T.stripStart'
--
-- >>> pz @(Snd Id >> TrimL) (20," abc   ")
-- PresentT "abc   "
--
data TrimL
type TrimLT = TrimImpl 'True 'False

instance P TrimLT x => P TrimL x where
  type PP TrimL x = PP TrimLT x
  eval _ = eval (Proxy @TrimLT)

-- | similar to 'T.stripEnd'
--
-- >>> pz @(Snd Id >> TrimR) (20," abc   ")
-- PresentT " abc"
--
-- >>> pz @("  abc " >> TrimR) ()
-- PresentT "  abc"
--
-- >>> pz @("" >> TrimR) ()
-- PresentT ""
--
data TrimR
type TrimRT = TrimImpl 'False 'True

instance P TrimRT x => P TrimR x where
  type PP TrimR x = PP TrimRT x
  eval _ = eval (Proxy @TrimRT)

-- | similar to 'T.strip'
--
-- >>> pz @(Snd Id >> TrimBoth) (20," abc   " :: String)
-- PresentT "abc"
--
-- >>> pz @(Snd Id >> TrimBoth) (20,T.pack " abc   ")
-- PresentT "abc"
--
-- >>> pz @("         " >> TrimBoth) ()
-- PresentT ""
--
-- >>> pz @("" >> TrimBoth) ()
-- PresentT ""
--
data TrimBoth
type TrimBothT = TrimImpl 'True 'True

instance P TrimBothT x => P TrimBoth x where
  type PP TrimBoth x = PP TrimBothT x
  eval _ = eval (Proxy @TrimBothT)

data StripImpl(left :: Bool) p q

instance (GetBool l
        , PP p x ~ String
        , P p x
        , DTL.IsText (PP q x)
        , P q x
        ) => P (StripImpl l p q) x where
  type PP (StripImpl l p q) x = Maybe (PP q x)
  eval _ opts x = do
    let msg0 = "Strip" ++ if l then "L" else "R"
        l = getBool @l
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,view DTL.unpacked -> q,pp,qq) ->
        let b = if l then
                  let (before,after) = splitAt (length p) q
                  in if before == p then Just after else Nothing
                else
                  let (before,after) = splitAt (length q - length p) q
                  in if after == p then Just before else Nothing
        in mkNode opts (PresentT (fmap (view DTL.packed) b)) (msg0 <> showL opts b <> litVerbose opts " | p=" p <> litVerbose opts " | q=" q) [hh pp, hh qq]

-- | similar to 'T.stripLeft'
--
-- >>> pz @(StripL "xyz" Id) ("xyzHello" :: String)
-- PresentT (Just "Hello")
--
-- >>> pz @(StripL "xyz" Id) (T.pack "xyzHello")
-- PresentT (Just "Hello")
--
-- >>> pz @(StripL "xyz" Id) "xywHello"
-- PresentT Nothing
--
data StripL p q
type StripLT p q = StripImpl 'True p q

instance P (StripLT p q) x => P (StripL p q) x where
  type PP (StripL p q) x = PP (StripLT p q) x
  eval _ = eval (Proxy @(StripLT p q))

-- | similar to 'T.stripRight'
--
-- >>> pz @(StripR "xyz" Id) "Hello xyz"
-- PresentT (Just "Hello ")
--
-- >>> pz @(StripR "xyz" Id) "xyzHelloxyw"
-- PresentT Nothing
--
-- >>> pz @(StripR "xyz" Id) ""
-- PresentT Nothing
--
-- >>> pz @(StripR "xyz" "xyz") ()
-- PresentT (Just "")
--
data StripR p q
type StripRT p q = StripImpl 'False p q

instance P (StripRT p q) x => P (StripR p q) x where
  type PP (StripR p q) x = PP (StripRT p q) x
  eval _ = eval (Proxy @(StripRT p q))

data IsFixImplC (cmp :: Ordering) (ignore :: Bool) p q

instance (GetBool ignore
        , P p x
        , P q x
        , PP p x ~ String
        , PP q x ~ String
        , GetOrdering cmp
        ) => P (IsFixImplC cmp ignore p q) x where
  type PP (IsFixImplC cmp ignore p q) x = Bool
  eval _ opts x = do
    let cmp = getOrdering @cmp
        ignore = getBool @ignore
        lwr = if ignore then map toLower else id
        (ff,msg0) = case cmp of
                    LT -> (isPrefixOf, "IsPrefixC")
                    EQ -> (isInfixOf, "IsInfixC")
                    GT -> (isSuffixOf, "IsSuffixC")
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
        Left e -> pure e
        Right s0 -> do
          let msg1 = msg0 <> (if ignore then "I" else "") <> " | " <> s0
          qq <- eval (Proxy @q) opts x
          pure $ case getValueLR opts (msg1 <> " q failed") qq [hh pp] of
            Left e -> e
            Right s1 -> mkNodeB opts (on ff lwr s0 s1) (msg1 <> " " <> litL opts s1) [hh pp, hh qq]

-- | similar to 'isPrefixOf' for strings
--
-- >>> pl @(IsPrefixC "xy" Id) "xyzabw"
-- True (IsPrefixC | xy xyzabw)
-- TrueT
--
-- >>> pl @(IsPrefixC "ab" Id) "xyzbaw"
-- False (IsPrefixC | ab xyzbaw)
-- FalseT
--
-- >>> pz @(IsPrefixC "abc" "aBcbCd") ()
-- FalseT
--
data IsPrefixC p q
type IsPrefixCT p q = IsFixImplC 'LT 'False p q

instance P (IsPrefixCT p q) x => P (IsPrefixC p q) x where
  type PP (IsPrefixC p q) x = PP (IsPrefixCT p q) x
  eval _ = evalBool (Proxy @(IsPrefixCT p q))

-- | similar to 'isInfixOf' for strings
--
-- >>> pl @(IsInfixC "ab" Id) "xyzabw"
-- True (IsInfixC | ab xyzabw)
-- TrueT
--
-- >>> pl @(IsInfixC "aB" Id) "xyzAbw"
-- False (IsInfixC | aB xyzAbw)
-- FalseT
--
-- >>> pl @(IsInfixC "ab" Id) "xyzbaw"
-- False (IsInfixC | ab xyzbaw)
-- FalseT
--
-- >>> pl @(IsInfixC (Fst Id) (Snd Id)) ("ab","xyzabw")
-- True (IsInfixC | ab xyzabw)
-- TrueT
--

data IsInfixC p q
type IsInfixCT p q = IsFixImplC 'EQ 'False p q

instance P (IsInfixCT p q) x => P (IsInfixC p q) x where
  type PP (IsInfixC p q) x = PP (IsInfixCT p q) x
  eval _ = evalBool (Proxy @(IsInfixCT p q))

-- | similar to 'isSuffixOf' for strings
--
-- >>> pl @(IsSuffixC "bw" Id) "xyzabw"
-- True (IsSuffixC | bw xyzabw)
-- TrueT
--
-- >>> pl @(IsSuffixC "bw" Id) "xyzbaw"
-- False (IsSuffixC | bw xyzbaw)
-- FalseT
--
-- >>> pz @(IsSuffixC "bCd" "aBcbCd") ()
-- TrueT
--
data IsSuffixC p q
type IsSuffixCT p q = IsFixImplC 'GT 'False p q

instance P (IsSuffixCT p q) x => P (IsSuffixC p q) x where
  type PP (IsSuffixC p q) x = PP (IsSuffixCT p q) x
  eval _ = evalBool (Proxy @(IsSuffixCT p q))

-- | similar to case insensitive 'isPrefixOf' for strings
--
-- >>> pz @(IsPrefixCI "abc" "aBcbCd") ()
-- TrueT
--
data IsPrefixCI p q
type IsPrefixCIT p q = IsFixImplC 'LT 'True p q

instance P (IsPrefixCIT p q) x => P (IsPrefixCI p q) x where
  type PP (IsPrefixCI p q) x = PP (IsPrefixCIT p q) x
  eval _ = evalBool (Proxy @(IsPrefixCIT p q))

-- | similar to case insensitive 'isInfixOf' for strings
--
-- >>> pl @(IsInfixCI "aB" Id) "xyzAbw"
-- True (IsInfixCI | aB xyzAbw)
-- TrueT
--
-- >>> pz @(IsInfixCI "abc" "axAbCd") ()
-- TrueT
--
data IsInfixCI p q
type IsInfixCIT p q = IsFixImplC 'EQ 'True p q

instance P (IsInfixCIT p q) x => P (IsInfixCI p q) x where
  type PP (IsInfixCI p q) x = PP (IsInfixCIT p q) x
  eval _ = evalBool (Proxy @(IsInfixCIT p q))

-- | similar to case insensitive 'isSuffixOf' for strings
--
data IsSuffixCI p q
type IsSuffixCIT p q = IsFixImplC 'GT 'True p q

instance P (IsSuffixCIT p q) x => P (IsSuffixCI p q) x where
  type PP (IsSuffixCI p q) x = PP (IsSuffixCIT p q) x
  eval _ = evalBool (Proxy @(IsSuffixCIT p q))

-- | very simple conversion to a string
data ToString
instance ToStringC x => P ToString x where
  type PP ToString x = String
  eval _ opts x = pure $ mkNode opts (PresentT (toStringC x)) "ToString" []

class ToStringC a where
  toStringC :: a -> String
instance ToStringC String where
  toStringC = id
instance ToStringC T.Text where
  toStringC = T.unpack
instance ToStringC TL.Text where
  toStringC = TL.unpack
instance ToStringC BL8.ByteString where
  toStringC = BL8.unpack
instance ToStringC BS8.ByteString where
  toStringC = BS8.unpack

-- | 'fromString' function where you need to provide the type \'t\' of the result
data FromString' t s

instance (P s a
        , PP s a ~ String
        , Show (PP t a)
        , IsString (PP t a)
        ) => P (FromString' t s) a where
  type PP (FromString' t s) a = PP t a
  eval _ opts a = do
    let msg0 = "FromString"
    ss <- eval (Proxy @s) opts a
    pure $ case getValueLR opts msg0 ss [] of
      Left e -> e
      Right s ->
        let b = fromString @(PP t a) s
        in mkNode opts (PresentT b) (msg0 <> " " <> showL opts b) [hh ss]

-- | 'fromString' function where you need to provide the type \'t\' of the result
--
-- >>> pz @(FromString (Identity _) Id) "abc"
-- PresentT (Identity "abc")
--
-- >>> pz @(FromString (Seq.Seq Char) Id) "abc"
-- PresentT (fromList "abc")
--
data FromString (t :: Type) p
type FromStringPT (t :: Type) p = FromString' (Hole t) p

instance P (FromStringPT t p) x => P (FromString t p) x where
  type PP (FromString t p) x = PP (FromStringPT t p) x
  eval _ = eval (Proxy @(FromStringPT t p))

