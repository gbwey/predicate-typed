{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wno-type-defaults #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoOverloadedLists #-} -- overloaded lists breaks some predicates
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoStarIsType #-}
module TestRefined where
import TastyExtras
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Predicate
import Predicate.TH_Orphans () -- need this else refined*TH' fails for dates

import Control.Lens
import Data.Aeson
import Control.Monad.IO.Class (MonadIO(..))
--import Text.Show.Functions ()

suite :: TestTree
suite =
  let s = "TestRefined"
  in testGroup s (namedTests <> orderTests s unnamedTests <> allProps)

namedTests :: [TestTree]
namedTests =
  [
    testCase "always true" $ (@=?) ($$(refinedTH 7) :: Refined 'OA 'True Int) (unsafeRefined 7)
  , testCase "between5and9" $ (@=?) ($$(refinedTH 7) :: Refined 'OA (Between 5 9 Id) Int) (unsafeRefined 7)
  ]

unnamedTests :: [IO ()]
unnamedTests = [
    (@=?) (unsafeRefined @'OA @'True ("1.2.3.4" :: String)) $$(refinedTH "1.2.3.4")
  , (@=?) (unsafeRefined @'OA @((Len == 4) && Luhn Id) [1,2,3,0]) $$(refinedTH [1,2,3,0])
  , (@=?) (unsafeRefined @'OA @(Not ((Len == 4) && Luhn Id)) [1,2,3,1]) $$(refinedTH [1,2,3,1])

  , (@=?) [(unsafeRefined 7, "")] (reads @(Refined 'OA (Between 2 10 Id) Int) "Refined 7")
  , (@=?) [] (reads @(Refined 'OA (Between 2 10 Id) Int) "Refined 0")
  , (@=?) [(unsafeRefined "abcaaaabb", "")] (reads @(Refined 'OA (Re "^[abc]+$" Id) String) "Refined \"abcaaaabb\"")
  , (@=?) [] (reads @(Refined 'OA (Re "^[abc]+$" Id) String) "Refined \"abcaaaabbx\"")

  , expectJ (Left ["Error in $: Refined(FromJSON:parseJSON):FalseP"]) (toFrom (unsafeRefined @'OZ @(Between 4 7 Id || Gt 14) 12))
  , expectJ (Right (unsafeRefined 22)) (toFrom (unsafeRefined @'OZ @(Between 4 7 Id || Gt 14) 22))
  , expectJ (Left ["Error in $: Refined(FromJSON:parseJSON):FailP \"someval\" (|| [someval])"]) (toFrom (unsafeRefined @'OL @(Between 4 7 Id || Gt 14 || Failt _ "someval") 12))

  , (fst <$> unRavelT (tst2 10 200)) >>= (@=? Right (10,200))
  , (fst <$> unRavelT (tst2 11 12)) >>= (@=? Left "FalseP")

  , (fst <$> unRavelT (tst1 10 200)) >>= (@=? Right (10,200))
  , (fst <$> unRavelT (tst1 11 12)) >>= (@=? Left "FalseP")
  ]

allProps :: [TestTree]
allProps =
  [
    testProperty "readshow" $ forAll (genRefined @'OA @(Between 10 45 Id) (choose (1,100))) (\r -> read @(Refined 'OA (Between 10 45 Id) Int) (show r) === r)
  , testProperty "jsonroundtrip" $ forAll (genRefined @'OA @(Between 10 45 Id) (choose (1,100))) (\r -> testRefinedJ @'OA @(Between 10 45 Id) (unRefined r) === Right r)
  ]

tst1 :: Monad m => Int -> Int -> RefinedT m (Int,Int)
tst1 i j = withRefinedT @'OA @(Between 2 11 Id) i
  $ \x -> withRefinedT @'OA @(Between 200 211 Id) j
     $ \y -> return (unRefined x, unRefined y)

tst2 :: MonadIO m => Int -> Int -> RefinedT m (Int,Int)
tst2 i j = withRefinedTIO @'OA @(Between 2 11 Id) i
  $ \x -> withRefinedTIO @'OA @(Stderr "startio..." |> Between 200 211 Id >| Stderr "...endio") j
     $ \y -> return (unRefined x, unRefined y)

-- roundtrip tojson then fromjson
testRefinedJ :: forall opts p a
   . ( ToJSON a
     , FromJSON a
     , RefinedC opts p a)
   => a
   -> Either String (Refined opts p a)
testRefinedJ a =
   let ((bp,(e,_top)),mr) = runIdentity $ newRefinedM @opts @p a
   in case mr of
        Nothing -> error $ show bp ++ "\n" ++ e
        Just r -> eitherDecode @(Refined opts p a) $ encode r

test0a :: Refined 'OA (Between 0 0xff Id) Int
test0a = $$(refinedTH 0xfe)