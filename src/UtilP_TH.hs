{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
-- |
-- Module      : UtilP_TH
-- Description : Template Haskell methods for creating Refined and Refined3 refinement types
-- Copyright   : (c) Grant Weyburne, 2019
-- License     : BSD-3
-- Maintainer  : gbwey9@gmail.com
--
-- Template Haskell methods for creating Refined and Refined3 refinement types
--
module UtilP_TH
  ( refined3TH
  , refined3TH'
  , refinedTH
  , refinedTH'
 ) where
import Refined3
import Refined
import PredicateCore
import qualified Language.Haskell.TH.Syntax as TH
import Data.Functor.Identity
import UtilP

-- | creates a 'Refined.Refined' refinement type with terse output
--
-- @
-- >$$(refinedTH 123) :: Refined (Between 100 125) Int
-- Refined {unRefined = 123}
-- it :: Refined (Between 100 125) Int
-- @
--
-- @
-- >$$(refinedTH 99) :: Refined (Between 100 125) Int
--
-- <interactive>:8:4: error:
--     * refinedTH: predicate failed with FalseP
--     * In the Template Haskell splice $$(refinedTH 99)
--       In the expression:
--           $$(refinedTH 99) :: Refined (Between 100 125) Int
--       In an equation for \'it\':
--           it = $$(refinedTH 99) :: Refined (Between 100 125) Int
-- @
--
refinedTH :: forall p i
  . (Show i, TH.Lift i, RefinedC p i)
  => i
  -> TH.Q (TH.TExp (Refined p i))
refinedTH = refinedTH' ol

-- | creates a 'Refined.Refined' refinement type
--
-- allows you to specify display options (eg 'ou' for unicode / 'o2' for ansi)
--
-- @
-- >$$(refinedTH' o2 123) :: Refined (Between 100 125) Int
-- Refined {unRefined = 123}
-- it :: Refined (Between 100 125) Int
-- @
--
-- @
-- >$$(refinedTH' o2 99) :: Refined (FailS "asdf" >> Between 100 125) Int
--
-- <interactive>:116:4: error:
--     *
-- [Error asdf] lhs failed >>
-- |
-- `- [Error asdf] Fail asdf
--    |
--    `- P 'asdf
--
-- refinedTH: predicate failed with FailP "asdf"
--     * In the Template Haskell splice $$(refinedTH' o0 99)
--       In the expression:
--           $$(refinedTH' o2 99) ::
--             Refined (FailS "asdf" >> Between 100 125) Int
-- @
--
refinedTH' :: forall p i
  . (Show i, TH.Lift i, RefinedC p i)
  => POpts
  -> i
  -> TH.Q (TH.TExp (Refined p i))
refinedTH' opts i = do
  let msg0 = "refinedTH"
  let ((bp,e),mr) = runIdentity $ newRefined @p opts i
  case mr of
    Nothing ->
      let msg1 = if oLite opts then "" else ("\n" ++ e ++ "\n")
      in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ show bp -- ++ "\n" ++ e
    Just r -> TH.TExp <$> TH.lift r

-- | creates a 'Refined3.Refined3' refinement type with terse output
--
-- @
-- >$$(refined3TH 100) :: Refined3 Id (Between 100 125) Id Int
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
-- >$$(refined3TH' o2 100) :: Refined3 Id (Between 100 125) Id Int
-- Refined3 {r3In = 100, r3Out = 100}
-- @
--
-- @
-- >$$(refined3TH' o2 99) :: Refined3 Id (Between 100 125) Id Int
--
-- <interactive>:127:4: error:
--     *
-- *** Step 1. Success Initial Conversion(ip) [99] ***
--
-- P Id 99
--
-- *** Step 2. False Boolean Check(op) ***
--
-- False False && True
-- |
-- +- False 99 >= 100
-- |  |
-- |  +- P I
-- |  |
-- |  `- P '100
-- |
-- `- True  99 <= 125
--    |
--    +- P I
--    |
--    `- P '125
--
-- refined3TH: predicate failed with Step 2. False Boolean Check(op) | FalseP
--     * In the Template Haskell splice $$(refined3TH' o2 99)
--       In the expression:
--           $$(refined3TH' o2 99) :: Refined3 Id (Between 100 125) Id Int
--       In an equation for \'it\':
--           it = $$(refined3TH' o2 99) :: Refined3 Id (Between 100 125) Id Int
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
      let msg1 = if oLite opts then "" else (m3Long m3 ++ "\n")
      in fail $ msg1 ++ msg0 ++ ": predicate failed with " ++ (m3Desc m3 <> " | " <> m3Short m3)
    Just r -> TH.TExp <$> TH.lift r

