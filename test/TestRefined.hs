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
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.IO.Class (MonadIO)

suite :: TestTree
suite =
  let s = "TestRefined"
  in testGroup s (namedTests <> orderTests s unnamedTests <> allProps)

namedTests :: [TestTree]
namedTests =
  [
    testCase "always true" $ (@=?) (newRefined @OA @'True @Int 7) (Right (unsafeRefined 7))
  , testCase "between5and9" $ (@=?) (newRefined @OA @(Between 5 9 Id) @Int 7) (Right (unsafeRefined 7))
  ]

unnamedTests :: [IO ()]
unnamedTests = [
    (@=?) (Right (unsafeRefined @OA @'True ("1.2.3.4" :: String))) (newRefined "1.2.3.4")
  , (@=?) (Right (unsafeRefined @OA @((Len == 4) && Luhn Id) [1,2,3,0])) (newRefined [1,2,3,0])
  , (@=?) (Right (unsafeRefined @OA @(Not ((Len == 4) && Luhn Id)) [1,2,3,1])) (newRefined [1,2,3,1])

  , (@=?) [(unsafeRefined 7, "")] (reads @(Refined OA (Between 2 10 Id) Int) "Refined 7")
  , (@=?) [] (reads @(Refined OA (Between 2 10 Id) Int) "Refined 0")
  , (@=?) [(unsafeRefined "abcaaaabb", "")] (reads @(Refined OA (Re "^[abc]+$" Id) String) "Refined \"abcaaaabb\"")
  , (@=?) [] (reads @(Refined OA (Re "^[abc]+$" Id) String) "Refined \"abcaaaabbx\"")

  , expectJ (Left ["Error in $: Refined(FromJSON:parseJSON):False"]) (toFrom (unsafeRefined @OZ @(Between 4 7 Id || Gt 14) 12))
  , expectJ (Right (unsafeRefined 22)) (toFrom (unsafeRefined @OZ @(Between 4 7 Id || Gt 14) 22))
  , expectJ (Left ["Error in $: Refined(FromJSON:parseJSON):FailT someval (||)"]) (toFrom (unsafeRefined @OL @(Between 4 7 Id || Gt 14 || Failt _ "someval") 12))

  , (fst <$> unRavelT (tst2 10 200)) >>= (@?= Right (10,200))
  , (fst <$> unRavelT (tst2 11 12)) >>= (@?= Left "FalseT")

  , (fst <$> unRavelT (tst1 10 200)) >>= (@=? Right (10,200))
  , (fst <$> unRavelT (tst1 11 12)) >>= (@=? Left "FalseT")
  ]

allProps :: [TestTree]
allProps =
  [
    testProperty "readshow" $ forAll (genRefined @OA @(Between 10 45 Id) (choose (1,100))) (\r -> read @(Refined OA (Between 10 45 Id) Int) (show r) === r)
  , testProperty "jsonroundtrip" $ forAll (genRefined @OA @(Between 10 45 Id) (choose (1,100))) (\r -> testRefinedJ @OA @(Between 10 45 Id) (unRefined r) === Right r)
  ]

tst1 :: Monad m => Int -> Int -> RefinedT m (Int,Int)
tst1 i j = withRefinedT @OAN @(Between 2 11 Id) i
  $ \x -> withRefinedT @OAN @(Between 200 211 Id) j
     $ \y -> return (unRefined x, unRefined y)

tst2 :: MonadIO m => Int -> Int -> RefinedT m (Int,Int)
tst2 i j = withRefinedTIO @OAN @(Between 2 11 Id) i
  $ \x -> withRefinedTIO @OAN @(Stderr "startio..." |> Between 200 211 Id >| Stderr "...endio") j
     $ \y -> return (unRefined x, unRefined y)

-- roundtrip tojson then fromjson
testRefinedJ :: forall opts p a
   . ( ToJSON a
     , FromJSON a
     , RefinedC opts p a)
   => a
   -> Either String (Refined opts p a)
testRefinedJ a =
   let ((bp,(_top,e)),mr) = runIdentity $ newRefinedM @opts @p a
   in case mr of
        Nothing -> error $ bp ++ "\n" ++ e
        Just r -> eitherDecode @(Refined opts p a) $ encode r
