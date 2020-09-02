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
{-# LANGUAGE NoStarIsType #-}
module TestJson where
import TastyExtras
import Test.Tasty
import Test.Tasty.HUnit
import Predicate
import qualified Predicate.Refined as R
import qualified Predicate.Refined3 as R3
import qualified Predicate.Examples.Refined3 as R3
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson
import qualified Data.ByteString as BS
import Control.Lens

suite :: TestTree
suite = testGroup "testjson"
  [ testCase "testperson ok" $ expectIO testPerson (() <$)
  , testCase "testperson1 ok" $ expectIO (testPerson1 @OAB 2) (() <$)
  , testCase "testperson1 bad ipaddress" $ expectIO (testPerson1 @OAB 3) (expectLeftWith ["octet 3 out of range 0-255 found 260"])
  , testCase "testperson1 bad lastname lowercase first letter" $ expectIO (testPerson1 @OAB 4) (expectLeftWith ["lastName1", "invalid name", "diaz"])
  , testCase "testperson1 bad first name lowercase first letter" $ expectIO (testPerson1 @OAB 6) (expectLeftWith ["firstName1", "not upper first(d)"])
  , testCase "testperson1 age 99 out of range" $ expectIO (testPerson1 @OAB 5) (expectLeftWith ["Error in $[0].age1"])
  , testCase "parse fail person1" $ expectPE (FailT "ParseJsonFile [Person1 'OZ](test3.json) Error in $[0].ipaddress1: Refined3:Step 2. Failed Boolean Check(op) | octet 3 out of range 0-255 found 260") $ pz @(ParseJsonFile [Person1 'OZ] "test3.json") ()
  , testCase "parse ok person1" $ expectPE (PresentT 5) $ pl @(ParseJsonFile [Person1 OA] "test2.json" >> Len) ()
  , testCase "missing file" $ expectPE (FailT "ParseJsonFile [Person1 'OZ](test2.jsoxxxn) file does not exist") $ pl @(ParseJsonFile [Person1 'OZ] "test2.jsoxxxn" >> Len) ()

  , testCase "getRow2Age1" $ do
                           x <- pz @(ParseJsonFile [Person1 OUB] "test2.json" >> Id !! 2) ()
                           (x ^? _PresentT . to (unRefined . age1)) @=? Just 45
                           (x ^? _PresentT . to (R3.r3Out . ipaddress1)) @=? Just "124.001.012.223"
  , testCase "getRow2" $ do
                           x <- pz @(ParseJsonFile [Person1 OUB] "test2.json" >> Id !! 2) ()
                           x @=? PresentT (Person1 {firstName1 = unsafeRefined "John", lastName1 = unsafeRefined "Doe", age1 = unsafeRefined 45, likesPizza1 = False, date1 = R3.unsafeRefined3 (read "2003-01-12 04:05:33 UTC") "2003-01-12 04:05:33", ipaddress1 = R3.unsafeRefined3 [124,1,12,223] "124.001.012.223"})
  ]

testPerson :: IO (Either String [Person])
testPerson = eitherDecodeStrict' <$> BS.readFile "test1.json"

testPerson1 :: forall opts . OptC opts => Int -> IO (Either String [Person1 opts])
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

data Person1 (opts :: Opt) = Person1 {
       firstName1 :: NameR2 (opts ':# 'OMsg "person1 firstname1")
     , lastName1 :: NameR1 (opts ':# 'OMsg "person1 lastname1")
     , age1 :: AgeR (opts ':# 'OMsg "age1 errors")
     , likesPizza1 :: Bool
     , date1 :: R3.DateTimeNR (opts ':# 'OMsg "person date1")
     , ipaddress1 :: R3.Ip4R (opts ':# 'OMsg "ipaddress1 errors")
     } deriving (Show,Generic,Eq)

instance OptC opts => ToJSON (Person1 opts)
instance OptC opts => FromJSON (Person1 opts)

type NameR1 (opts :: Opt) = R.Refined opts Name1 String
type Name1 = Msg "invalid name:" (Re "^[A-Z][a-z']+$" Id)

-- more specific messages
type NameR2 (opts :: Opt) = R.Refined opts Name2 String
type Name2 =
          Uncons
       >> Just'
       >> (Guard (PrintF "not upper first(%c)" Id) IsUpper
      *** Guard (PrintF "not lower rest(%s)" Id) IsLowerAll)
       >> 'True

type NameR2' (opts :: Opt) = R.Refined opts Name2' String
type Name2' =
          Uncons
       >> Just'
       >> (Fst Id >> GuardBool (PrintF "not upper first(%c)" Id) IsUpper)
       && (Snd Id >> GuardBool (PrintF "not lower rest(%s)" Id) IsLowerAll)

type AgeR (opts :: Opt) = R.Refined opts (Between 10 60 Id) Int


