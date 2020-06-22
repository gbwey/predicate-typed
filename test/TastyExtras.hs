{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-compat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoOverloadedLists #-} -- overloaded lists breaks some predicates
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoStarIsType #-}
module TastyExtras where
import Test.Tasty
import Test.Tasty.HUnit
import Predicate.Util
import Data.Aeson
import Data.List
import Text.Show.Functions ()

expectIO :: (HasCallStack, Show a) => IO (Either String a) -> (Either String a -> Either String ()) -> IO ()
expectIO iolr p = do
  lr <- iolr
  case p lr of
    Left e -> assertFailure $ "expectIO: " <> e <> " lr=" <> show lr
    Right () -> pure ()

expectLeftWith :: Show a => [String] -> Either String a -> Either String ()
expectLeftWith _ (Right a) = Left $ "expected fail but was actually successful " ++ show a
expectLeftWith ns (Left s)
  | all (`isInfixOf` s) ns = Right ()
  | otherwise = Left $ "found fail but infix string did not match: actual[" ++ s ++ "] infix[" ++ intercalate " | " ns ++ "]"

expectLeft :: Show b => Either a b -> IO ()
expectLeft = \case
  Left _ -> pure ()
  Right e -> assertFailure $ "expected Left but found Right " ++ show e

expectRight :: Show a => Either a b -> IO ()
expectRight = \case
  Right _ -> pure ()
  Left e -> assertFailure $ "expected Right but found Left " ++ show e

toFrom :: (FromJSON a1, ToJSON a2, a1 ~ a2) => a2 -> Either String a1
toFrom = eitherDecode . encode

orderTests :: String -> [Assertion] -> [TestTree]
orderTests s = zipWith (\i -> testCase (s <> "_" <> show i)) [1::Int ..]

expectPE :: (Show a, Eq a, HasCallStack) => BoolT a -> IO (BoolT a) -> IO ()
expectPE bp m = do
  x <- m
  print (x,bp)
  x @?= bp

expectEQR :: (Show a, Eq a, HasCallStack) => a -> IO a -> IO ()
expectEQR bp m = do
  x <- m
  print (x,bp)
  x @?= bp

expectJ :: (HasCallStack, Show a, Eq a)
  => Either [String] a
  -> Either String a
  -> IO ()
expectJ lhs rhs =
  case (lhs,rhs) of
    (Left _e,Right r) -> assertFailure $ "expected left but found right " <> show r
    (Right r,Right r1) -> r @?= r1
    (Right _r,Left e) -> assertFailure $ "expected right but found left " <> e
    (Left ss, Left e)
       | all (`isInfixOf` e) ss -> pure ()
       | otherwise -> assertFailure $ "both left but expected " <> show ss <> " in " <> e


