-- stack exec -- ghc-pkg unregister ghc-lib-parser-8.8.0.20190424 --force
{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     Template Haskell methods for creating Refined, Refined2, and Refined3 refinement types
-}
module Predicate.Util_TH (
  -- ** Refined
    refinedTH
  , refinedTHIO

  -- ** Refined1
  , refined1TH
  , refined1THIO

  -- ** Refined2
  , refined2TH
  , refined2THIO

  -- ** Refined3
  , refined3TH
  , refined3THIO
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
-- >>> $$(refinedTH 123) :: Refined OZ (Between 100 125 Id) Int
-- Refined 123
--
-- @
-- >$$(refinedTH 99) :: Refined OZ (Between 100 125 Id) Int
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
-- >>> $$(refinedTH 123) :: Refined OAN (Between 100 125 Id) Int
-- Refined 123
--
-- @
-- >$$(refinedTH 99) :: Refined OAN (FailS "asdf" >> Between 100 125 Id) Int
--
-- <interactive>:116:4: error:
--     *
-- [Error asdf]
-- |
-- `- [Error asdf] Fail asdf
--    |
--    `- P '"asdf"
--
-- refinedTH: predicate failed with FailP "asdf"
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
refinedTH i =
  let msg0 = "refinedTH"
      ((bp,(top,e)),mr) = runIdentity $ newRefinedM @opts @p i
  in case mr of
       Nothing ->
         let msg1 = if hasNoTree (getOpt @opts) then "" else "\n" ++ e ++ "\n"
         in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ bp ++ " " ++ top
       Just r -> TH.TExp <$> TH.lift r

refinedTHIO :: forall opts p i
  . (TH.Lift i, RefinedC opts p i)
  => i
  -> TH.Q (TH.TExp (Refined opts p i))
refinedTHIO i = do
  let msg0 = "refinedTHIO"
  ((bp,(top,e)),mr) <- TH.runIO (newRefinedM @opts @p i)
  case mr of
       Nothing ->
         let msg1 = if hasNoTree (getOpt @opts) then "" else "\n" ++ e ++ "\n"
         in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ bp ++ " " ++ top
       Just r -> TH.TExp <$> TH.lift r

-- | creates a 'Refined1.Refined1' refinement type
--
-- >>> $$(refined1TH 100) :: Refined1 OZ Id (Between 100 125 Id) Id Int
-- Refined1 100
--
-- >>> $$(refined1TH 100) :: Refined1 OZ Id (Between 100 125 Id) Id Int
-- Refined1 100
--
-- >>> $$(refined1TH 100) :: Refined1 OZ Id (Between 100 125 Id) Id Int
-- Refined1 100
--
-- @
-- >$$(refined1TH 99) :: Refined1 OZ Id (Between 100 125 Id) Id Int
--
-- <interactive>:127:4: error:
--     *
-- *** Step 1. Success Initial Conversion(ip) (99) ***
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
--           $$(refined1TH 99) :: Refined1 OZ Id (Between 100 125 Id) Id Int
--       In an equation for \'it\':
--           it = $$(refined1TH 99) :: Refined1 OZ Id (Between 100 125 Id) Id Int
-- @
--
refined1TH :: forall opts ip op fmt i
  . ( TH.Lift (PP ip i)
    , Refined1C opts ip op fmt i
    , Show (PP ip i)
    )
  => i
  -> TH.Q (TH.TExp (Refined1 opts ip op fmt i))
refined1TH i =
  let msg0 = "refined1TH"
      o = getOpt @opts
  in case newRefined1 @opts @ip @op @fmt i of
    Left m1 ->
      let msg1 = if hasNoTree o then "" else "\n" ++ m1Long m1 ++ "\n"
      in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ (m1Desc m1 <> " | " <> m1Short m1)
    Right r -> TH.TExp <$> TH.lift r

-- | creates a 'Refined1.Refined1' refinement type using IO
refined1THIO :: forall opts ip op fmt i
  . ( TH.Lift (PP ip i)
    , Refined1C opts ip op fmt i
    , Show (PP ip i)
    )
  => i
  -> TH.Q (TH.TExp (Refined1 opts ip op fmt i))
refined1THIO i = do
  x <- TH.runIO (newRefined1' @opts @ip @op @fmt i)
  case x of
    Right a -> TH.TExp <$> TH.lift a
    Left e -> fail $ show e

-- | creates a 'Refined2.Refined2' refinement type
--
-- >>> $$(refined2TH 100) :: Refined2 OAN Id (Between 100 125 Id) Int
-- Refined2 {r2In = 100, r2Out = 100}
--
-- >>> $$(refined2TH 100) :: Refined2 OAN Id (Between 100 125 Id) Int
-- Refined2 {r2In = 100, r2Out = 100}
--
-- @
-- >$$(refined2TH 99) :: Refined2 OAN Id (Between 100 125 Id) Int
--
-- <interactive>:127:4: error:
--     *
-- *** Step 1. Success Initial Conversion(ip) (99) ***
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
--           $$(refined2TH 99) :: Refined2 OZ Id (Between 100 125 Id) Id Int
--       In an equation for \'it\':
--           it = $$(refined2TH 99) :: Refined2 OZ Id (Between 100 125 Id) Id Int
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
refined2TH i =
  let msg0 = "refined2TH"
      o = getOpt @opts
  in case newRefined2 @opts @ip @op i of
       Left m2 ->
         let msg1 = if hasNoTree o then "" else "\n" ++ m2Long m2 ++ "\n"
         in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ (m2Desc m2 <> " | " <> m2Short m2)
       Right r -> TH.TExp <$> TH.lift r

-- | creates a 'Refined2.Refined2' refinement type using IO
refined2THIO :: forall opts ip op i
  . ( TH.Lift i
    , TH.Lift (PP ip i)
    , Refined2C opts ip op i
    , Show (PP ip i)
    )
  => i
  -> TH.Q (TH.TExp (Refined2 opts ip op i))
refined2THIO i = do
  x <- TH.runIO (newRefined2' @opts @ip @op i)
  case x of
    Right a -> TH.TExp <$> TH.lift a
    Left e -> fail $ show e

-- | creates a 'Refined3.Refined3' refinement type
--
-- >>> $$(refined3TH 100) :: Refined3 OZ Id (Between 100 125 Id) Id Int
-- Refined3 {r3In = 100, r3Out = 100}
--
-- >>> $$(refined3TH 100) :: Refined3 OAN Id (Between 100 125 Id) Id Int
-- Refined3 {r3In = 100, r3Out = 100}
--
-- @
-- >$$(refined3TH 99) :: Refined3 OAN Id (Between 100 125 Id) Id Int
--
-- <interactive>:127:4: error:
--     *
-- *** Step 1. Success Initial Conversion(ip) (99) ***
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
--           $$(refined3TH 99) :: Refined3 OAN Id (Between 100 125 Id) Id Int
--       In an equation for \'it\':
--           it = $$(refined3TH 99) :: Refined3 OAN Id (Between 100 125 Id) Id Int
-- @
--
-- >>> $$(refined3TH @OZ @(Resplit "\\." Id >> Map (ReadP Int Id) Id) @(All (0 <..> 0xff) Id && Len == 4) @(PrintL 4 "%03d.%03d.%03d.%03d" Id)  "200.2.3.4")
-- Refined3 {r3In = [200,2,3,4], r3Out = "200.002.003.004"}
--
refined3TH :: forall opts ip op fmt i
  . ( Show (PP ip i)
    , TH.Lift i
    , TH.Lift (PP ip i)
    , Refined3C opts ip op fmt i
    )
  => i
  -> TH.Q (TH.TExp (Refined3 opts ip op fmt i))
refined3TH i =
  let msg0 = "refined3TH"
  in case newRefined3 @opts @ip @op @fmt i of
    Left m3 ->
      let o = getOpt @opts
          msg1 = if hasNoTree o then "" else "\n" ++ m3Long m3 ++ "\n"
      in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ (m3Desc m3 <> " | " <> m3Short m3)
    Right r -> TH.TExp <$> TH.lift r

-- | creates a 'Refined3.Refined3' refinement type using IO
refined3THIO :: forall opts ip op fmt i
  . ( TH.Lift i
    , TH.Lift (PP ip i)
    , Refined3C opts ip op fmt i
    , Show (PP ip i)
    )
  => i
  -> TH.Q (TH.TExp (Refined3 opts ip op fmt i))
refined3THIO i = do
  x <- TH.runIO (newRefined3' @opts @ip @op @fmt i)
  case x of
    Right a -> TH.TExp <$> TH.lift a
    Left e -> fail $ show e

