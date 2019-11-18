{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module TestJson where
import TastyExtras
import Test.Tasty
import Test.Tasty.HUnit
import Predicate
import qualified Predicate.Refined as R
import qualified Predicate.Examples.Refined3 as R3
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson
import qualified Data.ByteString as BS

suite :: TestTree
suite = testGroup "testjson"
  [ testCase "testperson ok" $ expectIO testPerson (() <$)
  , testCase "testperson1 ok" $ expectIO (testPerson1 2) (() <$)
  , testCase "testperson1 bad ipaddress" $ expectIO (testPerson1 3) (expectLeftWith ["octet 3 out of range 0-255 found 260"])
  , testCase "testperson1 bad lastname lowercase first letter" $ expectIO (testPerson1 4) (expectLeftWith ["invalid name","diaz"])
  , testCase "testperson1 age 99 out of range" $ expectIO (testPerson1 5) (expectLeftWith ["Error in $[0].age1"])
  , testCase "parse fail person1" $ expectPE (FailT "ParseJsonFile [Person1](test3.json) Error in $[0].ipaddress1") $ pl @(ParseJsonFile [Person1] "test3.json") ()
  , testCase "parse ok person1" $ expectPE (PresentT 5) $ pl @(ParseJsonFile [Person1] "test2.json" >> Len) ()
  , testCase "missing file" $ expectPE (FailT "ParseJsonFile [Person1](test2.jsoxxxn) file doesn't exist") $ pl @(ParseJsonFile [Person1] "test2.jsoxxxn" >> Len) ()
  ]

testPerson :: IO (Either String [Person])
testPerson = eitherDecodeStrict' <$> BS.readFile "test1.json"

testPerson1 :: Int -> IO (Either String [Person1])
testPerson1 i = do
  let fn = "test" ++ show i ++ ".json"
  eitherDecodeStrict' <$> BS.readFile fn

data Person = Person {
       firstName :: !Text
     , lastName :: !Text
     , age :: !Int
     , likesPizza :: Bool
     } deriving (Show,Generic,Eq)

instance ToJSON Person
instance FromJSON Person

data Person1 = Person1 {
       firstName1 :: NameR1
     , lastName1 :: NameR1
     , age1 :: AgeR
     , likesPizza1 :: Bool
     , date1 :: R3.DateTimeNR
     , ipaddress1 :: R3.Ip4R
     } deriving (Show,Generic,Eq)

instance ToJSON Person1
instance FromJSON Person1

type NameR1 = R.Refined Name1 String
type Name1 = Msg "invalid name:" (Re "^[A-Z][a-z']+$" Id)

-- more specific messages
type NameR2 = R.Refined (Name2 >> 'True) String
type Name2 =
          Uncons
       >> 'Just Id
       >> Guard (PrintF "not upper first(%c)" Id) ('[Id] >> IsUpper)
      *** Guard (PrintF "not lower rest(%s)" Id) IsLower

type AgeR = R.Refined (Between 10 60 Id) Int


