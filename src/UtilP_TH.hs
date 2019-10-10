-- $$ works great if you have the TH.Lift instance -- Data.Time does not: import TH_Orphans.hs ()
-- use msg0' version if that doesnt work
{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-} -- need this for using non-Type in the predicate
module UtilP_TH (
    refined3TH
  , refined3TH'
  , refinedTH
  , refinedTH'
 ) where
import Refined3
import Refined
import Predicate
import qualified Language.Haskell.TH.Syntax as TH
import Data.Functor.Identity
import UtilP

refined3TH :: forall ip op fmt i
  . (Show i, Show (PP ip i), TH.Lift i, TH.Lift (PP ip i), Refined3C ip op fmt i)
  => i
  -> TH.Q (TH.TExp (Refined3 ip op fmt i))
refined3TH = refined3TH' o2

refined3TH' :: forall ip op fmt i
  . (Show i, Show (PP ip i), TH.Lift i, TH.Lift (PP ip i), Refined3C ip op fmt i)
  => POpts
  -> i
  -> TH.Q (TH.TExp (Refined3 ip op fmt i))
refined3TH' opts i = do
  let msg0 = "refined3TH"
      (ret,mr) = eval3 @ip @op @fmt opts i
      m3 = prt3Impl opts ret
  TH.runIO $ do
    putStrLn $ "\n>>>>>>> Start " ++ msg0 ++ " " ++ show i
    putStrLn $ m3Long m3
    putStrLn $ "<<<<<<< End "++ msg0 ++ " " ++ show i -- ++ "\n"
  case mr of
    Nothing -> fail $ msg0 ++ ": predicate failed with " ++ (m3Desc m3 <> " | " <> m3Short m3)
    Just r -> TH.TExp <$> TH.lift r

refinedTH :: forall p i
  . (Show i, TH.Lift i, RefinedC p i)
  => i
  -> TH.Q (TH.TExp (Refined p i))
refinedTH = refinedTH' o2

refinedTH' :: forall p i
  . (Show i, TH.Lift i, RefinedC p i)
  => POpts
  -> i
  -> TH.Q (TH.TExp (Refined p i))
refinedTH' opts i = do
  let msg0 = "refinedTH"
  let ((bp,e),mr) = runIdentity $ newRefined @p opts i
  TH.runIO $ do
    putStrLn $ "\n>>>>>>> Start " ++ msg0 ++ " " ++ show i
    putStrLn $ "\n" ++ e
    putStrLn $ "<<<<<<< End " ++ msg0 ++ " " ++ show i -- ++ "\n"
  case mr of
    Nothing -> fail $ msg0 ++ ": predicate failed with " ++ show bp -- ++ "\n" ++ e
    Just r -> TH.TExp <$> TH.lift r
