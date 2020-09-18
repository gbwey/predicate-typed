{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoStarIsType #-}
module TestRefined3 where
--module TestRefined3 (suite) where
import TastyExtras
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Predicate
import Predicate.Refined3
import Predicate.Examples.Refined3
import Predicate.Examples.Common
import Data.Ratio
import Data.Typeable
import Control.Lens
import Data.Time
import GHC.Generics (Generic)
import Data.Aeson
import Control.Monad.Cont
import Text.Show.Functions ()
import Data.Tree.Lens

suite :: TestTree
suite =
  let s = "TestRefined3"
  in testGroup s (namedTests <> orderTests s (unnamedTests <> tstextras) <> allProps)

namedTests :: [TestTree]
namedTests =
  [ testCase "ip9" $ (@?=) (newRefined3 "121.0.12.13" :: Either Msg3 (MakeR3 Ip9)) (Right (unsafeRefined3 [121,0,12,13] "121.000.012.013"))
  , testCase "luhn check" $ (@?=) (newRefined3 "12345678903" :: Either Msg3 (MakeR3 (Luhn11 OAN))) (Right (unsafeRefined3 [1,2,3,4,5,6,7,8,9,0,3] "1234-5678-903"))
  , testCase "datetime utctime" $ (@?=) (newRefined3 "2019-01-04 23:00:59" :: Either Msg3 (MakeR3 (DateTime1 OZ UTCTime))) (Right (unsafeRefined3 (read "2019-01-04 23:00:59 UTC") "2019-01-04 23:00:59"))
  , testCase "datetime localtime" $ (@?=) (newRefined3 "2019-01-04 09:12:30" :: Either Msg3 (MakeR3 (DateTime1 OZ LocalTime))) (Right (unsafeRefined3 (read "2019-01-04 09:12:30") "2019-01-04 09:12:30"))
  , testCase "hms" $ (@?=) (newRefined3 "12:0:59" :: Either Msg3 (MakeR3 (Hms OAN))) (Right (unsafeRefined3 [12,0,59] "12:00:59"))
  , testCase "between5and9" $ (@?=) (newRefined3 "7" :: Either Msg3 (Refined3 OAN (ReadP Int Id) (Between 5 9 Id) (PrintF "%03d" Id) String)) (Right (unsafeRefined3 7 "007"))
  , testCase "ssn" $ (@?=) (newRefined3 "123-45-6789" :: Either Msg3 (MakeR3 (Ssn OAN))) (Right (unsafeRefined3 [123,45,6789] "123-45-6789"))
  , testCase "base16" $ (@?=) (newRefined3 "12f" :: Either Msg3 (MakeR3 (BaseN OAN 16))) (Right (unsafeRefined3 303 "12f"))
  , testCase "daten1" $ (@?=) (newRefined3 "June 25 1900" :: Either Msg3 (MakeR3 (DateN OAN))) (Right (unsafeRefined3 (read "1900-06-25") "1900-06-25"))
  , testCase "daten2" $ (@?=) (newRefined3 "12/02/99" :: Either Msg3 (MakeR3 (DateN OAN))) (Right (unsafeRefined3 (read "1999-12-02") "1999-12-02"))
  , testCase "daten3" $ (@?=) (newRefined3 "2011-12-02" :: Either Msg3 (MakeR3 (DateN OAN))) (Right (unsafeRefined3 (read "2011-12-02") "2011-12-02"))
  , testCase "ccn123" $ (@?=) (newRefined3 "123455" :: Either Msg3 (MakeR3 (Luhn OAN '[1,2,3]))) (Right (unsafeRefined3 [1,2,3,4,5,5] "1-23-455"))
  , testCase "readshow" $ (@?=) (newRefined3 "12 % 5" :: Either Msg3 (ReadShowR OAN Rational)) (Right (unsafeRefined3 (12 % 5) "12 % 5"))
  ]

unnamedTests :: [IO ()]
unnamedTests = [
    (@?=) [(unsafeRefined3 255 "ff", "")] (reads @(Refined3 OAN (ReadBase Int 16 Id) (Between 0 255 Id) (ShowBase 16 Id) String) "Refined3 {r3In = 255, r3Out = \"ff\"}") -- escape quotes cos read instance for String
  , (@?=) [] (reads @(Refined3 OAN (ReadBase Int 16 Id) (Between 0 255 Id) (ShowBase 16 Id) String) "Refined3 {r3In = 256, r3Out = \"100\"}")
  , (@?=) [(unsafeRefined3 (-1234) "-4d2", "")] (reads @(Refined3 OAN (ReadBase Int 16 Id) (Id < 0) (ShowBase 16 Id) String) "Refined3 {r3In = -1234, r3Out = \"-4d2\"}")

  , (@?=) (Right (unsafeRefined3 [1,2,3,4] "001.002.003.004")) (newRefined3 "1.2.3.4" :: Either Msg3 (Ip4R OAN))

  , expectJ (Right (G4 (unsafeRefined3 12 "12") (unsafeRefined3 [1,2,3,4] "001.002.003.004"))) (toFrom $ G4 (unsafeRefined3 12 "12") (unsafeRefined3 [1,2,3,4] "1.2.3.4"))
  , expectJ (Left ["Error in $.g4Ip", "False Boolean Check"]) (toFrom $ G4 (unsafeRefined3 12 "12") (unsafeRefined3 [1,2,3,4] "1.2.3.400"))

  , expectJ (Left ["Error in $.g4Ip", "ReadP Int (3x)"]) (toFrom $ G4 (unsafeRefined3 12 "12") (unsafeRefined3 [1,2,3,4] "1.2.3x.4"))
  , expectJ (Left ["Error in $.g4Age", "False Boolean Check"]) (toFrom $ G4 (unsafeRefined3 (-2) "-2") (unsafeRefined3 [1,2,3,4] "1.2.3.4"))
  , expectRight (testRefined3P (Proxy @(Luhn OAN '[4,4,3])) "123-45-6---789-03-")
  , expectLeft (testRefined3P (Proxy @(Luhn OAN '[4,4,3])) "123-45-6---789-04-")
  , expectRight (testRefined3P (Proxy @(Hms OAN)) "1:2:33")
  , expectLeft (testRefined3P (Proxy @(Hms OAN)) "1:2:61")
  , expectRight (testRefined3P (Proxy @(Luhn OAN '[4,4,3])) "6433-1000-006")
  , expectRight (testRefined3P (Proxy @(Luhn OAN '[4,4,3])) "6433-10000-06")
  , expectLeft (testRefined3P (Proxy @(Luhn OAN '[4,4,3])) "6433-1000-000")
  , expectRight (testRefined3P (Proxy @(Luhn OAN '[1,2,1])) "1-23-0")
  , expect3 (Left $ XF "Regex no results")
                  $ runIdentity $ eval3M @OAN @(Rescan Ip4RE >> HeadFail "failedn" Id >> Map (ReadP Int Id) Snd)
                          @((Len == 4) && All (Between 0 255 Id))
                          @(PrintL 4 "%03d.%03d.%03d.%03d" Id)
                          "1.21.x31.4"

  , expect3 (Right $ unsafeRefined3 [1,21,31,4] "001.021.031.004")
                  $ runIdentity $ eval3M @OAN @(Rescan Ip4RE >> HeadFail "failedn" Id >> Map (ReadP Int Id) Snd)
                          @((Len == 4) && All (Between 0 255 Id))
                          @(PrintL 4 "%03d.%03d.%03d.%03d" Id)
                          "1.21.31.4"

  , expect3 (Left $ XTFalse (-6.5) "(-13) % 2 > (-7) % 3")
                  $ runIdentity $ eval3M @OAN @(ReadP Double Id)
                          @(ToRational Id > 7 -% 3)
                          @(PrintF "%5.3f" Id)
                          "-6.5"

  , expect3 (Right $ unsafeRefined3 4.123 "")
                  $ runIdentity $ eval3M @OAN @(ReadP Double Id) @(ToRational Id > 7 -% 3) @""
                  "4.123"

  , expect3 (Right $ unsafeRefined3 4.123 (4123 % 1000))
                  $ runIdentity $ eval3M @OAN @Id @(Gt (7 -% 3)) @(4123 % 1000) 4.123

  , expect3 (Right $ unsafeRefined3 [1,2,3,4] "")
                  $ runIdentity $ eval3M @OAN @(Map (ReadP Int Id) (Resplit "\\.")) @(All (Between 0 255 Id) && (Len == 4)) @""
                  "1.2.3.4"

  , expect3 (Left $ XTFalse [0,0,0,291,1048319,4387,17,1] "True && False | (out of bounds: All(8) i=4 (1048319 <= 65535))")
                  $ runIdentity $ eval3M @OAN @Ip6ip @Ip6op @"" "123:Ffeff:1123:11:1"

  , expect3 (Right $ unsafeRefined3 [12,2,0,255] "abc")
                  $ runIdentity $ eval3M @OAN @Ip4ip @Ip4op' @"abc" "12.2.0.255"

  , expect3 (Right $ unsafeRefined3 [123,45,6789] "def")
                  $ runIdentity $ eval3M
                  @OAN @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" >> OneP >> Map (ReadBase Int 10 Id) Snd)
                  @(GuardBool "expected 3" (Len == 3)
                 && GuardBool "3 digits" (Between 0 999 (Ix' 0))
                 && GuardBool "2 digits" (Between 0 99 (Ix' 1))
                 && GuardBool "4 digits" (Between 0 9999 (Ix' 2))
                   ) @"def"
                   "123-45-6789"

  , expect3 (Right $ unsafeRefined3 [123,45,6789] "xyz")
                  $ runIdentity $ eval3M
                  @OAN
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" >> OneP >> Map (ReadBase Int 10 Id) Snd)
                  @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 0 999 Id, Between 0 99 Id, Between 0 9999 Id] >> 'True)
                  @"xyz"
                  "123-45-6789"

  , expect3 (Left $ XTFalse [0,0,0,291,1048319,4387,17,1] "True && False | (out of bounds: All(8) i=4 (1048319 <= 65535))")
                  $ runIdentity $ eval3M @OAN @Ip6ip @Ip6op @"xyz"
                  "123:Ffeff:1123:11:1"

  , expect3 (Right $ unsafeRefined3 [0,0,0,291,65535,4387,17,1] "xyz")
                  $ runIdentity $ eval3M @OAN @Ip6ip @Ip6op @"xyz"
                  "123:Ffff:1123:11:1"

  , expect3 (Right $ unsafeRefined3 [0,0,291,0,65535,0,0,17] "xyz")
                  $ runIdentity $ eval3M @OAN @Ip6ip @Ip6op @"xyz"
                  "123::Ffff:::11"

  , expect3 (Right $ unsafeRefined3 [0,0,291,0,65535,0,0,17] "xyz")
                  $ runIdentity $ eval3M @OAN @Ip6ip @Ip6op @"xyz"
                  "123::Ffff:::11"

  , expect3 (Right $ unsafeRefined3 [31,11,1999] "xyz")
                  $ runIdentity $ eval3M @OAN @(Rescan DdmmyyyyRE >> OneP >> Map (ReadBase Int 10 Id) Snd)
                           @Ddmmyyyyop
                           @"xyz"
                           "31-11-1999"
  , expect3 (Right $ unsafeRefined3 [123,45,6789] "xyz") $ runIdentity $ eval3M @OAN
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" >> OneP >> Map (ReadBase Int 10 Id) Snd)
                  @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 0 999 Id, Between 0 99 Id, Between 0 9999 Id] >> 'True)
                  @"xyz"
                  "123-45-6789"

  , expect3 (Right $ unsafeRefined3 [1,2,3,4] "001.002.003.004") $ runIdentity $ eval3P (ip4 @OZ) "1.2.3.4"
  , expect3 (Left $ XF "ReadP Int (3x)") $ runIdentity $ eval3P (ip4 @OZ) "1.2.3x.4"
  , expect3 (Left $ XTF [1,2,3,4,5] "Guards:invalid length(5) expected 4") $ runIdentity $ eval3P (ip4 @OZ) "1.2.3.4.5"
  , expect3 (Left $ XTF [1,2,300,4] "octet 2 out of range 0-255 found 300") $ runIdentity $ eval3P (ip4 @OZ) "1.2.300.4"
  , expect3 (Left $ XTF [1,2,3,4,5] "Bools:invalid length(5) expected 4") $ runIdentity $ eval3P (ip4' @OZ) "1.2.3.4.5"
  , expect3 (Left $ XTF [1,2,300,4] "Bool(2) [octet 2 out of range 0-255 found 300]") $ runIdentity $ eval3P (ip4' @OZ) "1.2.300.4"

  , expect3 (Left (XTF [1,2,300,4] "Bool(2) [octet 2 out of range 0-255 found 300] (300 <= 255)")) $ runIdentity $ eval3P (ip4' @OL) "1.2.300.4"
  , expect3 (Right $ unsafeRefined3 [1,2,3,4,5,6,7,8,9,0,3] "1234-5678-903") $ runIdentity $ eval3P (luhn11 @OAN) "12345678903"
  , expect3 (Left $ XTF [1,2,3,4,5,6,7,8,9,0,1] "invalid checkdigit") $ runIdentity $ eval3P (luhn11 @OZ) "12345678901"

  , expect3 (Right $ unsafeRefined3 ([12,13,14],TimeOfDay 12 13 14) "12:13:14") $ runIdentity $ eval3P hms2E "12:13:14"
--  , expect3 (Left (XTF ([12,13,99], TimeOfDay 12 13 99) "seconds invalid: found 99")) $ runIdentity $ eval3P hms2E "12:13:99"

  , expect3 (Right (unsafeRefined3 [1,2,3,4] "001.002.003.004")) $ runIdentity $ eval3M @OAN @Ip4ip @Ip4op' @(ParaN 4 (PrintF "%03d" Id) >> Concat (Intercalate '["."] Id)) "1.2.3.4"
  , expect3 (Right (unsafeRefined3 [1,2,3,4] "abc__002__3__zzz")) $ runIdentity $ eval3M @OAN @Ip4ip @Ip4op' @(Para '[W "abc",PrintF "%03d" Id,PrintF "%d" Id,W "zzz"] >> Concat (Intercalate '["__"] Id)) "1.2.3.4"

  -- keep the original value
  , expect3 (Right $ unsafeRefined3 ("1.2.3.4", [1,2,3,4]) "001.002.003.004") $ runIdentity $ eval3M @OAN @(Id &&& Ip4ip) @(Snd >> Ip4op') @(Snd >> ParaN 4 (PrintF "%03d" Id) >> Concat (Intercalate '["."] Id)) "1.2.3.4"
  ]

allProps :: [TestTree]
allProps =
  [
    testProperty "base16" $ forAll (genRefined3P (mkProxy3 @'(OAN, ReadBase Int 16 Id, 'True, ShowBase 16 Id, String)) arbitrary) (\r -> evalQuick @OL @(ReadBase Int 16 Id) (r3Out r) === Right (r3In r))
  , testProperty "readshow" $ forAll (genRefined3 arbitrary :: Gen (HexLtR3 OAN)) (\r -> read @(HexLtR3 OAN) (show r) === r)
  , testProperty "jsonroundtrip1" $ forAll (genRefined3 arbitrary :: Gen (HexLtR3 OAN))
      (\r -> testRefined3PJ Proxy (r3Out r) === Right r)
  ]

type HexLtR3 (opts :: Opt) = Refined3 opts (ReadBase Int 16 Id) (Id < 500) (ShowBase 16 Id) String
--type IntLtR3 (opts :: Opt) = Refined3 opts (ReadP Int Id) (Id < 10) (ShowP Id) String

type Tst1 = '(OAN, ReadP Int Id, Between 1 7 Id, PrintF "someval val=%03d" Id, String)

yy1, yy2, yy3, yy4 :: RefinedT Identity (MakeR3 Tst1)

yy1 = newRefined3TP (Proxy @Tst1) "4"
yy2 = newRefined3TP (Proxy @Tst1) "3"

yy3 = rapply3 (*) yy1 yy2 -- fails
yy4 = rapply3 (+) yy1 yy2 -- pure ()

hms2E :: Proxy '(OAN, Hmsip2, Hmsop2, Hmsfmt2, String)
hms2E = mkProxy3

type Hmsip2 = Hmsip &&& ParseTimeP TimeOfDay "%H:%M:%S" Id
type Hmsop2 = Fst >> Hmsop
type Hmsfmt2 = Snd >> FormatTimeP "%T" Id

-- use GuardBool for op boolean check to get better errormessages
-- 1. packaged up as a promoted tuple
type Tst3 = '(OAN, Map (ReadP Int Id) (Resplit "\\."), (Len == 4) && All (Between 0 255 Id), Concat $ Intercalate '["."] $ Map (PrintF "%03d" Id) Id, String)

www1, www2 :: String -> Either Msg3 (MakeR3 Tst3)
www1 = newRefined3P (mkProxy3 @Tst3)
www2 = newRefined3P tst3

-- just pass in an ipaddress as a string: eg 1.2.3.4 or 1.2.3.4.5 (invalid) 1.2.3.400 (invalid)

-- 2. packaged as a proxy
tst3 :: Proxy
        '(OAN, Map (ReadP Int Id) (Resplit "\\.")
        ,(Len == 4) && All (Between 0 255 Id)
        ,Concat $ Intercalate '["."] $ Map (PrintF "%03d" Id) Id
        ,String)
tst3 = mkProxy3

-- 3. direct
www3, www3' :: String -> Either Msg3 (Refined3 OAN
                               (Map (ReadP Int Id) (Resplit "\\."))
                               ((Len == 4) && All (Between 0 255 Id))
                               (Concat $ Intercalate '["."] $ Map (PrintF "%03d" Id) Id)
                               String)
www3 = newRefined3

www3' = newRefined3
        @OAN
        @(Map (ReadP Int Id) (Resplit "\\."))
        @((Len == 4) && All (Between 0 255 Id))
        @(Concat $ Intercalate '["."] $ Map (PrintF "%03d" Id) Id)

data G4 = G4 { g4Age :: MakeR3 Age
             , g4Ip :: MakeR3 Ip9
             } deriving (Show,Generic,Eq)

--type MyAge = Refined3 OAN (ReadP Int Id) (Gt 4) (ShowP Id) String

type Age = '(OAN, ReadP Int Id, Gt 4, ShowP Id, String)

type Ip9 = '(
            OAN
           ,Map (ReadP Int Id) (Resplit "\\.") -- split String on "." then convert to [Int]
           ,Len == 4 && All (Between 0 255 Id) -- process [Int] and make sure length==4 and each octet is between 0 and 255
           ,PrintL 4 "%03d.%03d.%03d.%03d" Id -- printf [Int]
           ,String -- input type is string which is also the output type
           )

instance FromJSON G4
instance ToJSON G4

tstextras :: [Assertion]
tstextras =
  [ newRefined3P (daten @OAN) "June 25 1900" @?= Right (unsafeRefined3 (fromGregorian 1900 6 25) "1900-06-25")
  , newRefined3P (daten @OAN) "12/02/19" @?= Right (unsafeRefined3 (fromGregorian 2019 12 2) "2019-12-02")
  , newRefined3P (Proxy @(Luhn OAN '[1,1,1,1])) "1230" @?= Right (unsafeRefined3 [1,2,3,0] "1-2-3-0")
  , newRefined3P (Proxy @(Luhn OAN '[1,2,3])) "123455" @?= Right (unsafeRefined3 [1,2,3,4,5,5] "1-23-455")
  , (runIdentity (unRavelT $ tst1a @OAN @Identity) ^. _1) @?= Right ((163,"a3"),(12,"12"))
  , runIdentity (unRavelT yy1) ^? _1 . _Right @?= Just (unsafeRefined3 4 "someval val=004")
  , runIdentity (unRavelT yy2) ^? _1 . _Right @?= Just (unsafeRefined3 3 "someval val=003")
  , runIdentity (unRavelT yy3) ^? _1 . _Left @?= Just "Step 2. False Boolean Check(op) | {12 <= 7}"
  , runIdentity (unRavelT yy4) ^? _1 . _Right @?= Just (unsafeRefined3 7 "someval val=007")
  , www1 "1.2.3.4" @?= Right (unsafeRefined3 [1,2,3,4] "001.002.003.004")
  , www2 "1.2.3.4" @?= Right (unsafeRefined3 [1,2,3,4] "001.002.003.004")
  , www3 "1.2.3.4" @?= Right (unsafeRefined3 [1,2,3,4] "001.002.003.004")
  , www3' "1.2.3.4" @?= Right (unsafeRefined3 [1,2,3,4] "001.002.003.004")
  , expectIO (fst <$> unRavelT (tst2a @'OAN)) (either Left (\x -> if x == ((163,"a3"),(12,"12")) then Right () else Left "tst2a failed to match"))
  ]

-- prtRefinedTIO $ tst1a @OZ
tst1a :: forall (opts :: Opt) m . (OptC opts, Monad m) => RefinedT m ((Int,String),(Int,String))
tst1a = withRefined3T @opts @(ReadBase Int 16 Id) @(Between 100 200 Id) @(ShowBase 16 Id) @String "a3"
  $ \r1 -> withRefined3T @opts @(ReadP Int Id) @'True @(ShowP Id) @String "12"
     $ \r2 -> return ((r3In r1, r3Out r1), (r3In r2, r3Out r2))

-- prtRefinedTIO $ tst2a @OZ
tst2a :: forall (opts :: Opt) m . (OptC opts, MonadIO m) => RefinedT m ((Int,String),(Int,String))
tst2a = withRefined3TIO @opts @(ReadBase Int 16 Id) @(Stderr "start" |> Between 100 200 Id >| Stdout "end") @(ShowBase 16 Id) @String "a3"
  $ \r1 -> withRefined3TIO @opts @(ReadP Int Id) @'True @(ShowP Id) @String "12"
     $ \r2 -> return ((r3In r1, r3Out r1), (r3In r2, r3Out r2))

-- have to use 'i' as we dont hold onto the input
testRefined3PJ :: forall opts ip op fmt i proxy
   . (ToJSON (PP fmt (PP ip i))
    , Show (PP ip i)
    , Refined3C opts ip op fmt i
    , FromJSON i
    )
   => proxy '(opts,ip,op,fmt,i)
   -> i
   -> Either String (Refined3 opts ip op fmt i)
testRefined3PJ _ i =
  case newRefined3 @opts @ip @op @fmt i of
    Right r -> eitherDecode @(Refined3 opts ip op fmt i) $ encode r
    Left e -> Left $ show e

-- test that roundtripping holds ie i ~ PP fmt (PP ip i)
testRefined3P :: forall opts ip op fmt i proxy
   . ( Show (PP ip i)
     , Show (PP fmt (PP ip i))
     , Refined3C opts ip op fmt i
     , Eq i
     , Eq (PP ip i)
     )
   => proxy '(opts,ip,op,fmt,i)
   -> i
   -> Either (String,String) (Refined3 opts ip op fmt i, Refined3 opts ip op fmt i)
testRefined3P _ i =
  case newRefined3 @opts @ip @op @fmt i of
    Right r ->
      case newRefined3 @opts @ip @op @fmt (r3Out r) of
        Left e -> Left ("testRefined3P(2): round trip failed: old(" ++ show i ++ ") new(" ++ show (r3Out r) ++ ")", show e)
        Right r1 ->
           if r /= r1 then Left ("testRefined3P(3): round trip pure () but values dont match: old(" ++ show i ++ ") new(" ++ show (r3Out r) ++ ")", show (r,r1))
           else Right (r,r1)
    Left e -> Left ("testRefined3P(1): bad initial predicate i=" ++ show i, show e)
{-
testRefined3PIO :: forall opts ip op fmt i proxy
   . ( Show (PP ip i)
     , Show (PP fmt (PP ip i))
     , Refined3C opts ip op fmt i
     , Eq i
     , Eq (PP ip i)
     )
   => proxy '(opts,ip,op,fmt,i)
   -> i
   -> IO (Either String (Refined3 opts ip op fmt i, Refined3 opts ip op fmt i))
testRefined3PIO p i =
  case testRefined3P p i of
    Right (r,r1) -> return $ Right (r,r1)
    Left (msg, e) -> putStrLn e >> return (Left msg)
-}
data Results3 a =
       XF String        -- Left e
     | XTF a String     -- Right a + Left e
     | XTFalse a String -- Right a + Right False
     | XTTrueF a String -- Right a + Right True + Left e
     | XTTrueT a        -- Right a + Right True + Right b
     deriving (Show,Eq)

toRResults3 :: RResults3 a -> Results3 a
toRResults3 = \case
   RF e _ -> XF e
   RTF a _ e _ -> XTF a e
   RTFalse a _ t2 -> XTFalse a (t2 ^. root . pString)
   RTTrueF a _ _ e _ -> XTTrueF a e
   RTTrueT a _ _ _ -> XTTrueT a

expect3 :: (HasCallStack, Show i, Show r, Eq i, Eq r)
  => Either (Results3 i) r
  -> (RResults3 i, Maybe r)
  -> IO ()
expect3 lhs (rhs,mr) =
  (@?=) (maybe (Left $ toRResults3 rhs) Right mr) lhs
