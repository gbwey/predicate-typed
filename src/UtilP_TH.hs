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
import Control.Monad (unless)

-- | creates a 'Refined3.Refined3' refinement type with terse output
refined3TH :: forall ip op fmt i
  . (Show i, Show (PP ip i), TH.Lift i, TH.Lift (PP ip i), Refined3C ip op fmt i)
  => i
  -> TH.Q (TH.TExp (Refined3 ip op fmt i))
refined3TH = refined3TH' ol

-- | creates a 'Refined3.Refined3' refinement type and allows you to specify options (ol to just get a summary / o2 for full detail and colors)
refined3TH' :: forall ip op fmt i
  . (Show i, Show (PP ip i), TH.Lift i, TH.Lift (PP ip i), Refined3C ip op fmt i)
  => POpts
  -> i
  -> TH.Q (TH.TExp (Refined3 ip op fmt i))
refined3TH' opts i = do
  let msg0 = "refined3TH"
      (ret,mr) = eval3 @ip @op @fmt opts i
      m3 = prt3Impl opts ret
  unless (oLite opts) $
    TH.runIO $ do
      putStrLn $ "\n>>>>>>> Start " ++ msg0 ++ " " ++ show i
      putStrLn $ m3Long m3
      putStrLn $ "<<<<<<< End "++ msg0 ++ " " ++ show i -- ++ "\n"
  case mr of
    Nothing -> fail $ msg0 ++ ": predicate failed with " ++ (m3Desc m3 <> " | " <> m3Short m3)
    Just r -> TH.TExp <$> TH.lift r

-- | creates a 'Refined.Refined' refinement type with terse output
refinedTH :: forall p i
  . (Show i, TH.Lift i, RefinedC p i)
  => i
  -> TH.Q (TH.TExp (Refined p i))
refinedTH = refinedTH' ol

-- | creates a 'Refined.Refined' refinement type and allows you to specify options
refinedTH' :: forall p i
  . (Show i, TH.Lift i, RefinedC p i)
  => POpts
  -> i
  -> TH.Q (TH.TExp (Refined p i))
refinedTH' opts i = do
  let msg0 = "refinedTH"
  let ((bp,e),mr) = runIdentity $ newRefined @p opts i
  unless (oLite opts) $
    TH.runIO $ do
      putStrLn $ "\n>>>>>>> Start " ++ msg0 ++ " " ++ show i
      putStrLn $ "\n" ++ e
      putStrLn $ "<<<<<<< End " ++ msg0 ++ " " ++ show i
  case mr of
    Nothing -> fail $ msg0 ++ ": predicate failed with " ++ show bp -- ++ "\n" ++ e
    Just r -> TH.TExp <$> TH.lift r
