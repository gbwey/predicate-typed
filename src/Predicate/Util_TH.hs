{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# OPTIONS -Wunused-type-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
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

  -- ** Refined2
  , refined2TH
  , refined2THIO

  -- ** Refined3
  , refined3TH
  , refined3THIO

  -- ** Refined5
  , refined5TH
  , refined5THIO

 ) where
import Predicate.Util
import Predicate.Misc
import Predicate.Core
import Predicate.Refined
import Predicate.Refined2
import Predicate.Refined3
import Predicate.Refined5

import qualified Language.Haskell.TH.Syntax as TH

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XTemplateHaskell
-- >>> :m + Predicate.Prelude

-- | creates a 'Refined.Refined' refinement type
--
-- >>> $$(refinedTH 123) :: Refined OL (Between 100 125 Id) Int
-- Refined 123
--
-- @
-- >$$(refinedTH 99) :: Refined OL (Between 100 125 Id) Int
--
-- <interactive>:8:4: error:
--     * refinedTH: predicate failed with Val False (100 <= 99)
--     * In the Template Haskell splice $$(refinedTH 99)
--       In the expression:
--           $$(refinedTH 99) :: Refined OL (Between 100 125 Id) Int
-- @
--
-- >>> $$(refinedTH 123) :: Refined OAN (Between 100 125 Id) Int
-- Refined 123
--
refinedTH :: forall opts p i
  . (TH.Lift i, RefinedC opts p i)
  => i
  -> TH.Q (TH.TExp (Refined opts p i))
refinedTH i =
  let msg0 = "refinedTH"
  in case newRefined @opts @p i of
       Left m -> fail $ refinedFailMsg @opts msg0 m
       Right r -> [||r||]

refinedTHIO :: forall opts p i
  . (TH.Lift i, RefinedC opts p i)
  => i
  -> TH.Q (TH.TExp (Refined opts p i))
refinedTHIO i = do
  let msg0 = "refinedTHIO"
  lr <- TH.runIO (newRefined' i)
  case lr of
    Left m -> fail $ refinedFailMsg @opts msg0 m
    Right r -> [||r||]

refinedFailMsg :: forall opts . OptC opts => String -> Msg0 -> String
refinedFailMsg msg m =
  let msg1 | hasNoTree (getOpt @opts) || null (m0Long m) = ""
           | otherwise = nullIf "\n" (m0Long m)
  in msg ++ ": predicate failed with " ++ m0ValBoolColor m ++ " " ++ m0Short m ++ msg1

-- | creates a 'Refined2.Refined2' refinement type
--
-- >>> $$(refined2TH 100) :: Refined2 OAN Id (Between 100 125 Id) Int
-- Refined2 {r2In = 100, r2Out = 100}
--
-- @
-- >$$(refined2TH 99) :: Refined2 OAN Id (Between 100 125 Id) Int
--
-- <interactive>:127:4: error:
--     * Step 2. False Boolean Check(op) | {100 <= 99}
-- *** Step 1. Success Initial Conversion(ip) (99) ***
-- P Id 99
-- *** Step 2. False Boolean Check(op) ***
-- Present False 100 <= 99
-- |
-- +- P Id 99
-- |
-- +- P '100
-- |
-- `- P '125
--
--     * In the Template Haskell splice $$(refined2TH 99)
--       In the expression:
--           $$(refined2TH 99) :: Refined2 OAN Id (Between 100 125 Id) Int
-- @
--
refined2TH :: forall opts ip op i
  . ( Refined2C opts ip op i
    , TH.Lift i
    , TH.Lift (PP ip i)
    , Show (PP ip i)
    )
  => i
  -> TH.Q (TH.TExp (Refined2 opts ip op i))
refined2TH i =
  case newRefined2 i of
    Left e -> fail $ show e
    Right r -> [||r||]

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
  x <- TH.runIO (newRefined2' i)
  case x of
    Left e -> fail $ show e
    Right r -> [||r||]

-- | creates a 'Refined3.Refined3' refinement type
--
-- >>> $$(refined3TH 100) :: Refined3 OAN Id (Between 100 125 Id) Id Int
-- Refined3 {r3In = 100, r3Out = 100}
--
-- @
-- >$$(refined3TH 99) :: Refined3 OAN Id (Between 100 125 Id) Id Int
--
-- <interactive>:127:4: error:
--     * refined3TH: predicate failed with Step 2. False Boolean Check(op) | {100 <= 99}
-- *** Step 1. Success Initial Conversion(ip) (99) ***
-- P Id 99
-- *** Step 2. False Boolean Check(op) ***
-- Present False 100 <= 99
-- |
-- +- P Id 99
-- |
-- +- P '100
-- |
-- `- P '125
--
--     * In the Template Haskell splice $$(refined3TH 99)
--       In the expression:
--           $$(refined3TH 99) :: Refined3 OAN Id (Between 100 125 Id) Id Int
-- @
--
-- >>> $$(refined3TH @OL @(Resplit "\\." >> Map (ReadP Int Id) Id) @(All (0 <..> 0xff) && Len == 4) @(PrintL 4 "%03d.%03d.%03d.%03d" Id)  "200.2.3.4")
-- Refined3 {r3In = [200,2,3,4], r3Out = "200.002.003.004"}
--
refined3TH :: forall opts ip op fmt i
  . ( Refined3C opts ip op fmt i
    , TH.Lift i
    , TH.Lift (PP ip i)
    , Show (PP ip i)
    )
  => i
  -> TH.Q (TH.TExp (Refined3 opts ip op fmt i))
refined3TH i =
  case newRefined3 i of
    Left e -> fail $ show e
    Right r -> [||r||]

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
  x <- TH.runIO (newRefined3' i)
  case x of
    Left e -> fail $ show e
    Right r -> [||r||]

-- | creates a 'Refined5.Refined5' refinement type
--
-- >>> $$(refined5TH 100) :: Refined5 OAN Id (Between 100 125 Id) Int
-- Refined5 100
--
-- @
-- >$$(refined5TH 99) :: Refined5 OAN Id (Between 100 125 Id) Int
--
-- <interactive>:127:4: error:
--     * refined5TH: predicate failed with Step 2. False Boolean Check(op) | {100 <= 99}
-- *** Step 1. Success Initial Conversion(ip) (99) ***
-- P Id 99
-- *** Step 2. False Boolean Check(op) ***
-- Present False 100 <= 99
-- |
-- +- P Id 99
-- |
-- +- P '100
-- |
-- `- P '125
--
--     * In the Template Haskell splice $$(refined5TH 99)
--       In the expression:
--           $$(refined5TH 99) :: Refined5 OAN Id (Between 100 125 Id) Int
-- @
--
refined5TH :: forall opts ip op i
  . ( Refined2C opts ip op i
    , TH.Lift (PP ip i)
    , Show (PP ip i)
    )
  => i
  -> TH.Q (TH.TExp (Refined5 opts ip op i))
refined5TH i =
  case newRefined5 i of
    Left e -> fail $ show e
    Right r -> [||r||]

-- | creates a 'Refined5.Refined5' refinement type using IO
refined5THIO :: forall opts ip op i
  . ( TH.Lift (PP ip i)
    , Refined2C opts ip op i
    , Show (PP ip i)
    )
  => i
  -> TH.Q (TH.TExp (Refined5 opts ip op i))
refined5THIO i = do
  x <- TH.runIO (newRefined5' i)
  case x of
    Left e -> fail $ show e
    Right r -> [||r||]

