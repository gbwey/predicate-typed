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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module TestJson where
import Test.Tasty
import TestRefined
import Test.Tasty.HUnit
import Predicate
import Refined
import Refined3
import Refined3Helper
import UtilP
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson

suite :: IO ()
suite = defaultMain $ testGroup "testrefined"
  [ testCase "testperson ok" $ expectIO testPerson (() <$)
  , testCase "testperson1 ok" $ expectIO (testPerson1 2) (() <$)
  , testCase "testperson1 bad ipaddress" $ expectIO (testPerson1 3) (expectLeftWith "expected between 0 and 255 found 260")
  , testCase "testperson1 bad lastname lowercase first letter" $ expectIO (testPerson1 4) (expectLeftWith "invalid name(diaz)")
  , testCase "testperson1 age 99 out of range" $ expectIO (testPerson1 5) (expectLeftWith "Error in $[0].age1")
  ]

testPerson :: IO (Either String [Person])
testPerson = eitherDecodeFileStrict' "test1.json"

testPerson1 :: Int -> IO (Either String [Person1])
testPerson1 i = do
  let fn = "test" ++ show i ++ ".json"
  eitherDecodeFileStrict' fn

data Person = Person {
       firstName :: !Text
     , lastName :: !Text
     , age :: !Int
     , likesPizza :: Bool
     } deriving (Show,Generic,Eq)

instance ToJSON Person
instance FromJSON Person

data Person1 = Person1 {
       firstName1 :: NameR
     , lastName1 :: NameR
     , age1 :: AgeR
     , likesPizza1 :: Bool
     , date1 :: DateTimeNR
     , ipaddress1 :: Ip4R
     } deriving (Show,Generic,Eq)

instance ToJSON Person1
instance FromJSON Person1

type ValidName =
         Guard (Printf "invalid name(%s)")
        (Re "^[A-Z][a-z']+$") >> 'True

type NameR = Refined ValidName String

type NameR1 = Refined (Name1 >> 'True) String
type Name1 =
          Uncons
       >> 'Just Id
       >> Guard (Fst >> Printf "not upper first(%c)") (Fst >> '[Id] >> IsCharSet 'CUpper)
       >> Guard (Snd >> Printf "not lower rest(%s)") (Snd >> IsCharSet 'CLower)

type AgeR = Refined (Between 10 60) Int

type Ip4R = MakeR3 '(Ip4ip, Ip4op >> 'True, Ip4fmt, String)

type Ip4ip = Resplit "\\." >> Map (ReadP Int)
type Ip4op = Guard (Len >> Printf "expected length 4 found %d") (Len >> Same 4)
          >> GuardsQuick (Printf2 "guard(%d): expected between 0 and 255 found %d") (RepeatT 4 (Between 0 255))
type Ip4fmt = Printfnt 4 "%03d.%03d.%03d.%03d"

type DateTimeNR = MakeR3 DateTimeN
