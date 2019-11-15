{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wno-type-defaults #-}
{-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
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

suite :: IO ()
suite = defaultMain $ testGroup "testrefined"
  [ testCase "testperson ok" $ expectIO testPerson (() <$)
  , testCase "testperson1 ok" $ expectIO (testPerson1 2) (() <$)
  , testCase "testperson1 bad ipaddress" $ expectIO (testPerson1 3) (expectLeftWith "expected between 0 and 255 found 260")
  , testCase "testperson1 bad lastname lowercase first letter" $ expectIO (testPerson1 4) (expectLeftWith "invalid name(diaz)")
  , testCase "testperson1 age 99 out of range" $ expectIO (testPerson1 5) (expectLeftWith "Error in $[0].age1")
  ]

testPerson :: IO (Either String [Person])
testPerson = BS.readFile "test1.json" >>= return . eitherDecodeStrict'

testPerson1 :: Int -> IO (Either String [Person1])
testPerson1 i = do
  let fn = "test" ++ show i ++ ".json"
  BS.readFile fn >>= return . eitherDecodeStrict'

data Person = Person {
       firstName :: !Text
     , lastName :: !Text
     , age :: !Int
     , likesPizza :: Bool
     } deriving (Show,Generic,Eq)

instance ToJSON Person
instance FromJSON Person

data Person1 = Person1 {
       firstName1 :: NameR2
     , lastName1 :: NameR2
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

type AgeR = R.Refined (Between 10 60) Int


