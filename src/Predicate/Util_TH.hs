{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{- |
     Template Haskell methods for creating Refined, Refined2, and Refined3 refinement types
-}
module Predicate.Util_TH
  (
  -- ** Refined
    refinedTH
  , refinedTH'

  -- ** Refined2
  , refined2TH
  , refined2TH'

  -- ** Refined3
  , refined3TH
  , refined3TH'
 ) where
import Predicate.Util
import Predicate.Core
import Predicate.Refined
import Predicate.Refined2
import Predicate.Refined3

import qualified Language.Haskell.TH.Syntax as TH
import Data.Functor.Identity
import Data.Semigroup ((<>))

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators

-- | creates a 'Refined.Refined' refinement type with terse output
--
-- @
-- >$$(refinedTH 123) :: Refined (Between 100 125 Id) Int
-- Refined {unRefined = 123}
-- it :: Refined (Between 100 125 Id) Int
-- @
--
-- @
-- >$$(refinedTH 99) :: Refined (Between 100 125 Id) Int
--
-- <interactive>:8:4: error:
--     * refinedTH: predicate failed with FalseP (100 <= 99)
--     * In the Template Haskell splice $$(refinedTH 99)
--       In the expression:
--           $$(refinedTH 99) :: Refined (Between 100 125 Id) Int
--       In an equation for \'it\':
--           it = $$(refinedTH 99) :: Refined (Between 100 125 Id) Int
-- @
--
refinedTH :: forall p i
  . (TH.Lift i, RefinedC p i)
  => i
  -> TH.Q (TH.TExp (Refined p i))
refinedTH = refinedTH' ol

-- | creates a 'Refined.Refined' refinement type
--
-- allows you to specify display options (eg 'ou' for unicode / 'o2' for ansi)
--
-- @
-- >$$(refinedTH' o2 123) :: Refined (Between 100 125 Id) Int
-- Refined {unRefined = 123}
-- it :: Refined (Between 100 125 Id) Int
-- @
--
-- @
-- >$$(refinedTH' o2 99) :: Refined (FailS "asdf" >> Between 100 125 Id) Int
--
-- <interactive>:116:4: error:
--     *
-- [Error asdf] lhs failed >>
-- |
-- `- [Error asdf] Fail asdf
--    |
--    `- P 'asdf
--
-- refinedTH: predicate failed with FailP "asdf" ((>>) lhs failed)
--     * In the Template Haskell splice $$(refinedTH' o0 99)
--       In the expression:
--           $$(refinedTH' o2 99) ::
--             Refined (FailS "asdf" >> Between 100 125 Id) Int
-- @
--
refinedTH' :: forall p i
  . (TH.Lift i, RefinedC p i)
  => POpts
  -> i
  -> TH.Q (TH.TExp (Refined p i))
refinedTH' opts i = do
  let msg0 = "refinedTH"
  let ((bp,(e,top)),mr) = runIdentity $ newRefined @p opts i
  case mr of
    Nothing ->
      let msg1 = if hasNoTree opts then "" else "\n" ++ e ++ "\n"
      in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ show bp ++ " " ++ top
    Just r -> TH.TExp <$> TH.lift r

-- | creates a 'Refined2.Refined2' refinement type with terse output
--
-- @
-- >$$(refined2TH 100) :: Refined2 Id (Between 100 125 Id) Id Int
-- Refined2 {r2In = 100, r2Out = 100}
-- @
--
refined2TH :: forall ip op i
  . (Show (PP ip i), TH.Lift i, TH.Lift (PP ip i), Refined2C ip op i)
  => i
  -> TH.Q (TH.TExp (Refined2 ip op i))
refined2TH = refined2TH' ol

-- | creates a 'Refined2.Refined2' refinement type
--
-- allows you to specify display options (eg 'ou' for unicode / 'o2' for ansi)
--
-- @
-- >$$(refined2TH' o2 100) :: Refined2 Id (Between 100 125 Id) Int
-- Refined2 {r2In = 100, r2Out = 100}
-- @
--
-- @
-- >$$(refined2TH' o2 99) :: Refined2 Id (Between 100 125 Id) Int
--
-- <interactive>:127:4: error:
--     *
-- *** Step 1. Success Initial Conversion(ip) [99] ***
--
-- P Id 99
--
-- *** Step 2. False Boolean Check(op) ***
--
-- False 100 <= 99
-- |
-- +- P Id 99
-- |
-- +- P '100
-- |
-- `- P '125
--
-- refined2TH: predicate failed with Step 2. False Boolean Check(op) | {100 <= 99}
--     * In the Template Haskell splice $$(refined2TH' o2 99)
--       In the expression:
--           $$(refined2TH' o2 99) :: Refined2 Id (Between 100 125 Id) Id Int
--       In an equation for \'it\':
--           it = $$(refined2TH' o2 99) :: Refined2 Id (Between 100 125 Id) Id Int
-- @
--
refined2TH' :: forall ip op i
  . (Show (PP ip i), TH.Lift i, TH.Lift (PP ip i), Refined2C ip op i)
  => POpts
  -> i
  -> TH.Q (TH.TExp (Refined2 ip op i))
refined2TH' opts i = do
  let msg0 = "refined2TH"
      (ret,mr) = eval2 @ip @op opts i
      m2 = prt2Impl opts ret
  case mr of
    Nothing ->
      let msg1 = if hasNoTree opts then "" else m2Long m2 ++ "\n"
      in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ (m2Desc m2 <> " | " <> m2Short m2)
    Just r -> TH.TExp <$> TH.lift r

-- | creates a 'Refined3.Refined3' refinement type with terse output
--
-- @
-- >$$(refined3TH 100) :: Refined3 Id (Between 100 125 Id) Id Int
-- Refined3 {r3In = 100, r3Out = 100}
-- @
--
refined3TH :: forall ip op fmt i
  . (Show i, Show (PP ip i), TH.Lift i, TH.Lift (PP ip i), Refined3C ip op fmt i)
  => i
  -> TH.Q (TH.TExp (Refined3 ip op fmt i))
refined3TH = refined3TH' ol

-- | creates a 'Refined3.Refined3' refinement type
--
-- allows you to specify display options (eg 'ou' for unicode / 'o2' for ansi)
--
-- @
-- >$$(refined3TH' o2 100) :: Refined3 Id (Between 100 125 Id) Id Int
-- Refined3 {r3In = 100, r3Out = 100}
-- @
--
-- @
-- >$$(refined3TH' o2 99) :: Refined3 Id (Between 100 125 Id) Id Int
--
-- <interactive>:127:4: error:
--     *
-- *** Step 1. Success Initial Conversion(ip) [99] ***
--
-- P Id 99
--
-- *** Step 2. False Boolean Check(op) ***
--
-- False 100 <= 99
-- |
-- +- P Id 99
-- |
-- +- P '100
-- |
-- `- P '125
--
-- refined3TH: predicate failed with Step 2. False Boolean Check(op) | {100 <= 99}
--     * In the Template Haskell splice $$(refined3TH' o2 99)
--       In the expression:
--           $$(refined3TH' o2 99) :: Refined3 Id (Between 100 125 Id) Id Int
--       In an equation for \'it\':
--           it = $$(refined3TH' o2 99) :: Refined3 Id (Between 100 125 Id) Id Int
-- @
--
refined3TH' :: forall ip op fmt i
  . (Show i, Show (PP ip i), TH.Lift i, TH.Lift (PP ip i), Refined3C ip op fmt i)
  => POpts
  -> i
  -> TH.Q (TH.TExp (Refined3 ip op fmt i))
refined3TH' opts i = do
  let msg0 = "refined3TH"
      (ret,mr) = eval3 @ip @op @fmt opts i
      m3 = prt3Impl opts ret
  case mr of
    Nothing ->
      let msg1 = if hasNoTree opts then "" else m3Long m3 ++ "\n"
      in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ (m3Desc m3 <> " | " <> m3Short m3)
    Just r -> TH.TExp <$> TH.lift r

