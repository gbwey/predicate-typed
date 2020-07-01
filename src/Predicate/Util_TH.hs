-- stack exec -- ghc-pkg unregister ghc-lib-parser-8.8.0.20190424 --force
{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     Template Haskell methods for creating Refined, Refined2, and Refined3 refinement types
-}
module Predicate.Util_TH
  (
  -- ** Refined
    refinedTH

  -- ** Refined1
  , refined1TH

  -- ** Refined2
  , refined2TH

  -- ** Refined3
  , refined3TH
 ) where
import Predicate.Util
import Predicate.Core
import Predicate.Refined
import Predicate.Refined1
import Predicate.Refined2
import Predicate.Refined3

import qualified Language.Haskell.TH.Syntax as TH
import Data.Functor.Identity

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XTemplateHaskell
-- >>> :m + Predicate.Prelude

-- | creates a 'Refined.Refined' refinement type
--
-- >>> $$(refinedTH 123) :: Refined 'OZ (Between 100 125 Id) Int
-- Refined 123
--
-- @
-- >$$(refinedTH 99) :: Refined 'OZ (Between 100 125 Id) Int
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
-- >>> $$(refinedTH 123) :: Refined 'OAN (Between 100 125 Id) Int
-- Refined 123
--
-- @
-- >$$(refinedTH 99) :: Refined 'OAN (FailS "asdf" >> Between 100 125 Id) Int
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
--     * In the Template Haskell splice $$(refinedTH 99)
--       In the expression:
--           $$(refinedTH 99) ::
--             Refined (FailS "asdf" >> Between 100 125 Id) Int
-- @
--
refinedTH :: forall opts p i
  . (TH.Lift i, RefinedC opts p i)
  => i
  -> TH.Q (TH.TExp (Refined opts p i))
refinedTH i = do
  let msg0 = "refinedTH"
  let ((bp,(e,top)),mr) = runIdentity $ newRefined @opts @p i
  case mr of
    Nothing ->
      let msg1 = if hasNoTree (getOptT @opts) then "" else "\n" ++ e ++ "\n"
      in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ show bp ++ " " ++ top
    Just r -> TH.TExp <$> TH.lift r

-- | creates a 'Refined1.Refined1' refinement type
--
-- >>> $$(refined1TH 100) :: Refined1 'OZ Id (Between 100 125 Id) Id Int
-- Refined1 100
--
-- >>> $$(refined1TH 100) :: Refined1 'OZ Id (Between 100 125 Id) Id Int
-- Refined1 100
--
-- >>> $$(refined1TH 100) :: Refined1 'OZ Id (Between 100 125 Id) Id Int
-- Refined1 100
--
-- @
-- >$$(refined1TH 99) :: Refined1 'OZ Id (Between 100 125 Id) Id Int
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
-- refined1TH: predicate failed with Step 2. False Boolean Check(op) | {100 <= 99}
--     * In the Template Haskell splice $$(refined1TH 99)
--       In the expression:
--           $$(refined1TH 99) :: Refined1 'OZ Id (Between 100 125 Id) Id Int
--       In an equation for \'it\':
--           it = $$(refined1TH 99) :: Refined1 'OZ Id (Between 100 125 Id) Id Int
-- @
--
refined1TH :: forall opts ip op fmt i
  . (Show i, Show (PP ip i), TH.Lift i, TH.Lift (PP ip i), Refined1C opts ip op fmt i)
  => i
  -> TH.Q (TH.TExp (Refined1 opts ip op fmt i))
refined1TH i = do
  let msg0 = "refined1TH"
      o = getOptT @opts
      (ret,mr) = eval1 @opts @ip @op @fmt i
      m1 = prt1Impl o ret
  case mr of
    Nothing ->
      let msg1 = if hasNoTree o then "" else m1Long m1 ++ "\n"
      in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ (m1Desc m1 <> " | " <> m1Short m1)
    Just r -> TH.TExp <$> TH.lift r


-- | creates a 'Refined2.Refined2' refinement type
--
-- >>> $$(refined2TH 100) :: Refined2 'OA Id (Between 100 125 Id) Int
-- Refined2 {r2In = 100, r2Out = 100}
--
-- >>> $$(refined2TH 100) :: Refined2 'OAN Id (Between 100 125 Id) Int
-- Refined2 {r2In = 100, r2Out = 100}
--
-- @
-- >$$(refined2TH 99) :: Refined2 'OAN Id (Between 100 125 Id) Int
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
--     * In the Template Haskell splice $$(refined2TH 99)
--       In the expression:
--           $$(refined2TH 99) :: Refined2 'OZ Id (Between 100 125 Id) Id Int
--       In an equation for \'it\':
--           it = $$(refined2TH 99) :: Refined2 'OZ Id (Between 100 125 Id) Id Int
-- @
--
refined2TH :: forall opts ip op i
  . ( Show (PP ip i)
    , TH.Lift i
    , TH.Lift (PP ip i)
    , Refined2C opts ip op i
    )
  => i
  -> TH.Q (TH.TExp (Refined2 opts ip op i))
refined2TH i = do
  let msg0 = "refined2TH"
      o = getOptT @opts
      (ret,mr) = eval2 @opts @ip @op i
      m2 = prt2Impl o ret
  case mr of
    Nothing ->
      let msg1 = if hasNoTree o then "" else m2Long m2 ++ "\n"
      in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ (m2Desc m2 <> " | " <> m2Short m2)
    Just r -> TH.TExp <$> TH.lift r

-- | creates a 'Refined3.Refined3' refinement type
--
-- >>> $$(refined3TH 100) :: Refined3 'OZ Id (Between 100 125 Id) Id Int
-- Refined3 {r3In = 100, r3Out = 100}
--
-- >>> $$(refined3TH 100) :: Refined3 'OAN Id (Between 100 125 Id) Id Int
-- Refined3 {r3In = 100, r3Out = 100}
--
-- @
-- >$$(refined3TH 99) :: Refined3 'OAN Id (Between 100 125 Id) Id Int
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
--     * In the Template Haskell splice $$(refined3TH 99)
--       In the expression:
--           $$(refined3TH 99) :: Refined3 'OAN Id (Between 100 125 Id) Id Int
--       In an equation for \'it\':
--           it = $$(refined3TH 99) :: Refined3 'OAN Id (Between 100 125 Id) Id Int
-- @
--
refined3TH :: forall opts ip op fmt i
  . ( Show i
    , Show (PP ip i)
    , TH.Lift i
    , TH.Lift (PP ip i)
    , Refined3C opts ip op fmt i
    )
  => i
  -> TH.Q (TH.TExp (Refined3 opts ip op fmt i))
refined3TH i = do
  let msg0 = "refined3TH"
      o = getOptT @opts
      (ret,mr) = eval3 @opts @ip @op @fmt i
      m3 = prt3Impl o ret
  case mr of
    Nothing ->
      let msg1 = if hasNoTree o then "" else m3Long m3 ++ "\n"
      in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ (m3Desc m3 <> " | " <> m3Short m3)
    Just r -> TH.TExp <$> TH.lift r

