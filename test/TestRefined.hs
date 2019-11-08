{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wno-type-defaults #-}
{-# OPTIONS -Wno-redundant-constraints #-}
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
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module TestRefined where
import TastyExtras
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Predicate
import Predicate.Refined
import Predicate.Util_TH
import Predicate.TH_Orphans () -- need this else refined*TH' fails for dates

import Control.Lens
import Data.Aeson
import Control.Monad.Cont
import Text.Show.Functions ()
import GHC.TypeNats (Nat)

suite :: IO ()
suite = defaultMain $ testGroup "TestRefined" (namedTests <> orderTests unnamedTests <> allProps)

namedTests :: [TestTree]
namedTests =
  [
    testCase "always true" $ (@?=) ($$(refinedTH 7) :: Refined 'True Int) (unsafeRefined 7)
  , testCase "between5and9" $ (@?=) ($$(refinedTH 7) :: Refined (Between 5 9) Int) (unsafeRefined 7)
  ]

unnamedTests :: [IO ()]
unnamedTests = [
    (@?=) (unsafeRefined @'True ("1.2.3.4" :: String)) $$(refinedTH "1.2.3.4")
  , (@?=) (unsafeRefined @((Len == 4) && Luhn Id) [1,2,3,0]) $$(refinedTH [1,2,3,0])
  , (@?=) (unsafeRefined @(Not ((Len == 4) && Luhn Id)) [1,2,3,1]) $$(refinedTH [1,2,3,1])

  , (@?=) [(unsafeRefined 7, "")] (reads @(Refined (Between 2 10) Int) "Refined {unRefined = 7}")
  , (@?=) [] (reads @(Refined (Between 2 10) Int) "Refined {unRefined = 0}")
  , (@?=) [(unsafeRefined "abcaaaabb", "")] (reads @(Refined (Re "^[abc]+$" Id) String) "Refined {unRefined = \"abcaaaabb\"}")
  , (@?=) [] (reads @(Refined (Re "^[abc]+$" Id) String) "Refined {unRefined = \"abcaaaabbx\"}")

  , expectJ (Left ["Error in $: Refined:FalseP"]) (toFrom (unsafeRefined @(Between 4 7 || Gt 14) 12))
  , expectJ (Right (unsafeRefined 22)) (toFrom (unsafeRefined @(Between 4 7 || Gt 14) 22))
  , expectJ (Left ["Error in $: Refined:FailP \"someval\""]) (toFrom (unsafeRefined @(Between 4 7 || Gt 14 || Failt _ "someval") 12))

  , (fst $ unRavelTI (tst1 ol 10 200)) @?= Right (10,200)
  , (fst $ unRavelTI (tst1 ol 11 12)) @?= Left "FalseP"
  , (fst <$> unRavelT (tst2 ol 10 200)) >>= (@?= Right (10,200))
  , (fst <$> unRavelT (tst2 ol 11 12)) >>= (@?= Left "FalseP")
  ]

allProps :: [TestTree]
allProps =
  [
    testProperty "readshow" $ forAll (arbRefined @(Between 10 45) ol) (\r -> read @(Refined (Between 10 45) Int) (show r) === r)
  , testProperty "jsonroundtrip" $ forAll (arbRefined @(Between 10 45) ol) (\r -> testRefinedJ @(Between 10 45) ol (unRefined r) === Right r)
  ]


type Ip4RE = "^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$"

type Ip4 = Rescan Ip4RE Id >> OneP >> Map (ReadBaseInt 10 Id) (Snd Id) >> Ip4guard

type Ip4guard = Guard "4octets" (Len == 4) >> Guard "0-255" (All (Between 0 255) Id)

type Ip6 = Resplit ":" Id
        >> Guard "count is bad" (Between' 0 8 Len)
        >> Guard "not a hex" (All (All (Elem Id "abcdefABCDEF0123456789") Id) Id)
        >> Guard "len is bad" (All (Len >> Le 4) Id)

type Ip6A = Map (If (Id == "") "0" Id) (Resplit ":" Id)
         >> Map (ReadBaseInt 16 Id) Id

type Ip6B = Guard "count is bad" (Len >> Between 0 8)
         >> Guard "out of bounds" (All (Between 0 65535) Id)
         >> 'True

type Ip6A' = Resplit ":" Id
         >> Map (If (Id == "") "0" Id) Id
         >> Map (ReadBaseInt 16 Id) Id
         >> PadL 8 0 Id

type Ip6A'' = Map (If (Id == "") 0 (ReadBaseInt 16 Id)) (Resplit ":" Id) >> PadL 8 0 Id

type Ip6B' = Guard "count is bad" (Len == 8)
         >> Guard "out of bounds" (All (Between 0 65535) Id)
         >> 'True

type Ip6B'' = Msg "count is bad:" (Len == 8)
         && Msg "out of bounds:" (All (Between 0 65535) Id)

type Ip4A = Map (ReadBaseInt 10 Id) (Resplit "\\." Id)
type Ip4B = Guard "expected 4 numbers" (Len == 4)
         >> Guard "each number must be between 0 and 255" (All (Between 0 255) Id)
         >> 'True

type Ip4B' = Msg "expected 4 numbers:" (Len == 4)
          && All (Between 0 255) Id

type Ip4C = PrintL 4 "%03d.%03d.%03d.%03d" Id

-- base n number of length x and then convert to a list of length x of (0 to (n-1))
-- checks that each digit is between 0 and n-1
type MM1 (n :: Nat) = Map (ReadBase Int n Id) (Ones Id)
type MM2 (n :: Nat) = ExitWhen "found empty" IsEmpty >> Guard "0<=x<n" (All (Ge 0 && Lt n) Id)

-- prtRefinedT tst1
tst1 :: Monad m => POpts -> Int -> Int -> RefinedT m (Int,Int)
tst1 opts i j = withRefinedT @(Between 2 11) opts i
  $ \x -> withRefinedT @(Between 200 211) opts j
     $ \y -> return (unRefined x, unRefined y)

-- prtRefinedTIO tst2
tst2 :: MonadIO m => POpts -> Int -> Int -> RefinedT m (Int,Int)
tst2 opts i j = withRefinedTIO @(Between 2 11) opts i
  $ \x -> withRefinedTIO @(Stderr "start" |> Between 200 211 >| Stderr "end") opts j
     $ \y -> return (unRefined x, unRefined y)

-- roundtrip tojson then fromjson
testRefinedJ :: forall p a
   . (ToJSON a, FromJSON a, RefinedC p a)
   => POpts
   -> a
   -> Either String (Refined p a)
testRefinedJ opts a =
   let ((bp,(e,_top)),mr) = runIdentity $ newRefined @p opts a
   in case mr of
        Nothing -> error $ show bp ++ "\n" ++ e
        Just r -> eitherDecode @(Refined p a) $ encode r

