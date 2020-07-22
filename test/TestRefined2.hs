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
module TestRefined2 where
import TastyExtras
import Test.Tasty
import Test.Tasty.HUnit

import Predicate
import Predicate.Refined2
import Predicate.Examples.Refined2
import Predicate.Examples.Common
import Predicate.TH_Orphans () -- need this else refined*TH' fails for dates

import Data.Ratio
import Data.Typeable
import Data.Time
import GHC.Generics (Generic)
import Data.Aeson
import Control.Monad.Cont
import Text.Show.Functions ()
import Data.Tree
import Data.Tree.Lens
--import Data.Maybe
import Control.Lens

suite :: TestTree
suite =
  let s = "TestRefined2"
  in testGroup s (namedTests <> orderTests s unnamedTests)

namedTests :: [TestTree]
namedTests =
  [ testCase "ip9" $ (@?=) (newRefined2 "121.0.12.13" :: Either String (MakeR2 (Ip9 OA))) (Right (unsafeRefined2 [121,0,12,13] "121.0.12.13"))
  , testCase "luhn check" $ (@?=) (newRefined2 "12345678903" :: Either String (MakeR2 (Ccn OA 11))) (Right (unsafeRefined2 [1,2,3,4,5,6,7,8,9,0,3] "12345678903"))
  , testCase "datetime utctime" $ (@?=) (newRefined2 "2019-01-04 23:00:59" :: Either String (MakeR2 (DateTime1 OA UTCTime))) (Right (unsafeRefined2 (read "2019-01-04 23:00:59 UTC") "2019-01-04 23:00:59"))
  , testCase "datetime localtime" $ (@?=) (newRefined2 "2019-01-04 09:12:30" :: Either String (MakeR2 (DateTime1 OA LocalTime))) (Right (unsafeRefined2 (read "2019-01-04 09:12:30") "2019-01-04 09:12:30"))
  , testCase "hms" $ (@?=) (newRefined2 "12:0:59" :: Either String (MakeR2 (Hms OA))) (Right (unsafeRefined2 [12,0,59] "12:0:59"))
  , testCase "between5and9" $ (@?=) (newRefined2 "7" :: Either String (Refined2 OA (ReadP Int Id) (Between 5 9 Id) String)) (Right (unsafeRefined2 7 "7"))
  , testCase "ssn" $ (@?=) (newRefined2 "123-45-6789" :: Either String (MakeR2 (Ssn OA))) (Right (unsafeRefined2 [123,45,6789] "123-45-6789"))
  , testCase "base16" $ (@?=) (newRefined2 "12f" :: Either String (MakeR2 (BaseN OA 16))) (Right (unsafeRefined2 303 "12f"))
  , testCase "daten1" $ (@?=) (newRefined2 "June 25 1900" :: Either String (MakeR2 (DateN OA))) (Right (unsafeRefined2 (read "1900-06-25") "June 25 1900"))
  , testCase "daten2" $ (@?=) (newRefined2 "12/02/99" :: Either String (MakeR2 (DateN OA))) (Right (unsafeRefined2 (read "1999-12-02") "12/02/99"))
  , testCase "daten3" $ (@?=) (newRefined2 "2011-12-02" :: Either String (MakeR2 (DateN OA))) (Right (unsafeRefined2 (read "2011-12-02") "2011-12-02"))
  , testCase "ccn123" $ (@?=) (newRefined2 "123455" :: Either String (MakeR2 (Ccn OA 6))) (Right (unsafeRefined2 [1,2,3,4,5,5] "123455"))
  ]

unnamedTests :: [IO ()]
unnamedTests = [
    (@?=) [(unsafeRefined2 255 "ff", "")] (reads @(Refined2 OA (ReadBase Int 16 Id) (Between 0 255 Id) String) "Refined2 {r2In = 255, r2Out = \"ff\"}") -- escape quotes cos read instance for String
  , (@?=) [] (reads @(Refined2 OA (ReadBase Int 16 Id) (Between 0 255 Id) String) "Refined2 {r2In = 256, r2Out = \"100\"}")
  , (@?=) [(unsafeRefined2 (-1234) "-4d2", "")] (reads @(Refined2 OA (ReadBase Int 16 Id) (Id < 0) String) "Refined2 {r2In = -1234, r2Out = \"-4d2\"}")

  , (@?=) (Right (unsafeRefined2 [1,2,3,4] "1.2.3.4")) (newRefined2 "1.2.3.4" :: Either String (Ip4R OA))

  , expectJ (Right (G4 (unsafeRefined2 12 "12") (unsafeRefined2 [1,2,3,4] "1.2.3.4"))) (toFrom $ G4 @OA (unsafeRefined2 12 "12") (unsafeRefined2 [1,2,3,4] "1.2.3.4"))
  , expectJ (Left ["Error in $.g4Ip", "False Boolean Check"]) (toFrom $ G4 @OA (unsafeRefined2 12 "12") (unsafeRefined2 [1,2,3,4] "1.2.3.400"))
  , expectJ (Left ["Error in $.g4Ip", "ReadP Int (3x)"]) (toFrom $ G4 @OA (unsafeRefined2 12 "12") (unsafeRefined2 [1,2,3,4] "1.2.3x.4"))
  , expectJ (Left ["Error in $.g4Age", "False Boolean Check"]) (toFrom $ G4 @OA (unsafeRefined2 (-2) "-2") (unsafeRefined2 [1,2,3,4] "1.2.3.4"))
  , expectRight (testRefined2P (Proxy @(Ccn OA 11)) "123-45-6---789-03-")
  , expectLeft (testRefined2P (Proxy @(Ccn OA 11)) "123-45-6---789-04-")
  , expectRight (testRefined2P (Proxy @(Hms OA)) "1:2:33")
  , expectLeft (testRefined2P (Proxy @(Hms OA)) "1:2:61")
  , expectRight (testRefined2P (Proxy @(Ccn OA 11)) "6433-1000-006")
  , expectRight (testRefined2P (Proxy @(Ccn OA 11)) "6433-10000-06")
  , expectLeft (testRefined2P (Proxy @(Ccn OA 11)) "6433-1000-000")
  , expectRight (testRefined2P (Proxy @(Ccn OA 4)) "1-23-0")

  , expect2 (Left $ XF "Regex no results")
                  $ eval2 @OA @(Rescan Ip4RE Id >> HeadFail "failedn" Id >> Map (ReadP Int Id) (Snd Id))
                          @((Len == 4) && All (Between 0 255 Id) Id)
                          "1.21.x31.4"

  , expect2 (Right $ unsafeRefined2 [1,21,31,4] "1.21.31.4")
                  $ eval2 @OA @(Rescan Ip4RE Id >> HeadFail "failedn" Id >> Map (ReadP Int Id) (Snd Id))
                          @((Len == 4) && All (Between 0 255 Id) Id)
                          "1.21.31.4"

  , expect2 (Left $ XTFalse (-6.5) "(-13) % 2 > (-7) % 3")
                  $ eval2 @OA @(ReadP Double Id)
                          @(ToRational Id > 7 -% 3)
                          "-6.5"

  , expect2 (Right $ unsafeRefined2 4.123 "4.123")
                  $ eval2 @OA @(ReadP Double Id) @(ToRational Id > 7 -% 3)
                  "4.123"

  , expect2 (Right $ unsafeRefined2 4.123 (4123 % 1000))
                  $ eval2 @OA @Id @(Gt (7 -% 3)) 4.123

  , expect2 (Right $ unsafeRefined2 [1,2,3,4] "1.2.3.4")
                  $ eval2 @OA @(Map (ReadP Int Id) (Resplit "\\." Id)) @(All (Between 0 255 Id) Id && (Len == 4))                   "1.2.3.4"

  , expect2 (Left $ XTFalse [0,0,0,291,1048319,4387,17,1] "True && False | (out of bounds:All(8) i=4 (1048319 <= 65535))")
                  $ eval2 @OA @Ip6ip @Ip6op "123:Ffeff:1123:11:1"

  , expect2 (Right $ unsafeRefined2 [12,2,0,255] "12.2.0.255")
                  $ eval2 @OA @Ip4ip @Ip4op' "12.2.0.255"

  , expect2 (Right $ unsafeRefined2 [123,45,6789] "123-45-6789")
                  $ eval2 @OA
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" Id >> OneP Id >> Map (ReadBase Int 10 Id) (Snd Id))
                  @(Guard "expected 3" (Len == 3)
                 >> Guard "3 digits" (Ix' 0 >> Between 0 999 Id)
                 >> Guard "2 digits" (Ix' 1 >> Between 0 99 Id)
                 >> Guard "4 digits" (Ix' 2 >> Between 0 9999 Id)
                 >> 'True
                   )
                   "123-45-6789"

  , expect2 (Right $ unsafeRefined2 [123,45,6789] "123-45-6789")
                  $ eval2 @OA
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" Id >> OneP Id >> Map (ReadBase Int 10 Id) (Snd Id))
                  @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 0 999 Id, Between 0 99 Id, Between 0 9999 Id] >> 'True)
                  "123-45-6789"

  , expect2 (Left $ XTFalse [0,0,0,291,1048319,4387,17,1] "True && False | (out of bounds:All(8) i=4 (1048319 <= 65535))")
                  $ eval2 @OA @Ip6ip @Ip6op
                  "123:Ffeff:1123:11:1"

  , expect2 (Left $ XTFalse [0,0,0,291,1048319,4387,17,1] "True && False | (out of bounds:All(8) i=4 (1048319 <= 65535))")
                  $ eval2 @OA @Ip6ip @Ip6op
                  "123:Ffeff:1123:11:1"

  , expect2 (Right $ unsafeRefined2 [0,0,0,291,65535,4387,17,1] "123:Ffff:1123:11:1")
                  $ eval2 @OA @Ip6ip @Ip6op
                  "123:Ffff:1123:11:1"

  , expect2 (Right $ unsafeRefined2 [0,0,291,0,65535,0,0,17] "123::Ffff:::11")
                  $ eval2 @OA @Ip6ip @Ip6op
                  "123::Ffff:::11"

  , expect2 (Right $ unsafeRefined2 [0,0,291,0,65535,0,0,17] "123::Ffff:::11")
                  $ eval2 @OA @Ip6ip @Ip6op
                  "123::Ffff:::11"

  , expect2 (Right $ unsafeRefined2 [31,11,1999] "31-11-1999")
                  $ eval2 @OA @(Rescan DdmmyyyyRE Id >> OneP Id >> Map (ReadBase Int 10 Id) (Snd Id))
                           @(Ddmmyyyyop >> 'True)
                           "31-11-1999"
  , expect2 (Right $ unsafeRefined2 [123,45,6789] "123-45-6789") $ eval2 @OA
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" Id >> OneP Id >> Map (ReadBase Int 10 Id) (Snd Id))
                  @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 0 999 Id, Between 0 99 Id, Between 0 9999 Id] >> 'True)
                  "123-45-6789"

  , expect2 (Right $ unsafeRefined2 [1,2,3,4] "1.2.3.4") $ eval2P (ip4 @OA) "1.2.3.4"
  , expect2 (Left $ XF "ReadP Int (3x)") $ eval2P (ip4 @OA) "1.2.3x.4"
  , expect2 (Left $ XTFalse [1,2,3,4,5] "Bools:invalid length(5) expected 4") $ eval2P (ip4' @OA) "1.2.3.4.5"
  , expect2 (Left $ XTF [1,2,3,4,5] "Guards:invalid length(5) expected 4") $ eval2P (ip4 @OA) "1.2.3.4.5"
  , expect2 (Left $ XTFalse [1,2,300,4] "Bool(2) [octet 2 out of range 0-255 found 300] (300 <= 255)") $ eval2P (ip4' @OA) "1.2.300.4"
  , expect2 (Left $ XTF [1,2,300,4] "octet 2 out of range 0-255 found 300") $ eval2P (ip4 @OA) "1.2.300.4"
  , expect2 (Right $ unsafeRefined2 [1,2,3,4,5,6,7,8,9,0,3] "12345678903") $ eval2P (cc11 @OA) "12345678903"
  , expect2 (Left $ XTFalse [1,2,3,4,5,6,7,8,9,0,1] "") $ eval2P (cc11 @OZ) "12345678901"
  ]

type HexLtR3 (opts :: OptT) = Refined2 opts (ReadBase Int 16 Id) (Id < 500) String
type IntLtR3 (opts :: OptT) = Refined2 opts (ReadP Int Id) (Id < 10) String

-- better to use Guard for op boolean check cos we get better errormessages
-- 1. packaged up as a promoted tuple
type Tst3 (opts :: OptT) = '(opts, Map (ReadP Int Id) (Resplit "\\." Id), (Len == 4) && All (Between 0 255 Id) Id, String)

www1, www2 :: String -> Either String (MakeR2 (Tst3 OA))
www1 = newRefined2P (Proxy @(Tst3 OA))
www2 = newRefined2P tst3

-- just pass in an ipaddress as a string: eg 1.2.3.4 or 1.2.3.4.5 (invalid) 1.2.3.400 (invalid)

-- 2. packaged as a proxy
tst3 :: Proxy
        '( OA, Map (ReadP Int Id) (Resplit "\\." Id)
        ,(Len == 4) && All (Between 0 255 Id) Id
        ,String)
tst3 = Proxy


-- 3. direct
ww3 :: String -> Either String (Refined2 OA
                               (Map (ReadP Int Id) (Resplit "\\." Id))
                               ((Len == 4) && All (Between 0 255 Id) Id)
                               String)
ww3 = newRefined2
{-
ww3 = newRefined2
        @OA
        @(Map (ReadP Int Id) (Resplit "\\." Id))
        @((Len == 4) && All (Between 0 255 Id))
-}
data G4 (opts :: OptT) = G4 { g4Age :: MakeR2 (Age opts)
             , g4Ip :: MakeR2 (Ip9 opts)
             } deriving (Show,Generic,Eq)

type MyAge (opts :: OptT) = Refined2 opts (ReadP Int Id) (Gt 4) String

type Age (opts :: OptT) = '(opts, ReadP Int Id, Gt 4, String)

type Ip9 (opts :: OptT) = '(opts,
            Map (ReadP Int Id) (Resplit "\\." Id) -- split String on "." then convert to [Int]
           ,Len == 4 && All (Between 0 255 Id) Id -- process [Int] and make sure length==4 and each octet is between 0 and 255
           ,String -- input type is string which is also the output type
           )

instance OptTC opts => FromJSON (G4 opts)
instance OptTC opts => ToJSON (G4 opts)

tst0a :: [Bool]
tst0a =
  [ newRefined2P (daten @OUB) "June 25 1900" == Right (unsafeRefined2 (fromGregorian 1900 6 25) "June 25 1900")
  , newRefined2P (daten @OUB) "12/02/19" == Right (unsafeRefined2 (fromGregorian 2019 12 2) "12/02/19")
  , newRefined2P (Proxy @(Ccn OAB 4)) "1230" == Right (unsafeRefined2 [1,2,3,0] "1230")
  , newRefined2P (Proxy @(Ccn OAB 6)) "123455" == Right (unsafeRefined2 [1,2,3,4,5,5] "123455")
  ]

-- prtRefinedTIO tst1a
tst1a :: Monad m => RefinedT m ((Int,String),(Int,String))
tst1a = withRefined2T @OA @(ReadBase Int 16 Id) @(Between 100 200 Id) @String "a3"
  $ \r1 -> withRefined2T @OA @(ReadP Int Id) @'True @String "12"
     $ \r2 -> return ((r2In r1, r2Out r1), (r2In r2, r2Out r2))

-- prtRefinedTIO tst2a
tst2a :: MonadIO m => RefinedT m ((Int,String),(Int,String))
tst2a = withRefined2TIO @OA @(ReadBase Int 16 Id) @(Stderr "start" |> Between 100 200 Id >| Stdout "end") @String "a3"
  $ \r1 -> withRefined2TIO @OA @(ReadP Int Id) @'True @String "12"
     $ \r2 -> return ((r2In r1, r2Out r1), (r2In r2, r2Out r2))

-- have to use 'i' as we dont hold onto the input
testRefined2PJ :: forall opts ip op i proxy
   . ( ToJSON i
     , Show (PP ip i)
     , Show i
     , Refined2C opts ip op i
     , FromJSON i)
   => proxy '(opts,ip,op,i)
   -> i
   -> Either String (Refined2 opts ip op i)
testRefined2PJ _ i =
  let (ret,mr) = eval2 @opts @ip @op i
      m3 = prt2Impl (getOptT @opts) ret
  in case mr of
    Just r -> eitherDecode @(Refined2 opts ip op i) $ encode r
    Nothing -> Left $ show m3

-- test that roundtripping holds ie i ~ PP (PP ip i)
testRefined2P :: forall opts ip op i proxy
   . ( Show (PP ip i)
     , Show i
     , Refined2C opts ip op i
     , Eq i
     , Eq (PP ip i))
   => proxy '(opts,ip,op,i)
   -> i
   -> Either (String,String) (Refined2 opts ip op i, Refined2 opts ip op i)
testRefined2P _ i =
  let (ret,mr) = eval2 @opts @ip @op i
      o = getOptT @opts
      m3 = prt2Impl o ret
  in case mr of
    Just r ->
      let (ret1,mr1) = eval2 @opts @ip @op (r2Out r)
          m3a = prt2Impl o ret1
      in case mr1 of
           Nothing -> Left ("testRefined2P(2): round trip failed: old(" ++ show i ++ ") new(" ++ show (r2Out r) ++ ")", show m3a)
           Just r1 ->
             if r /= r1 then Left ("testRefined2P(3): round trip pure () but values dont match: old(" ++ show i ++ ") new(" ++ show (r2Out r) ++ ")", show (r,r1))
             else Right (r,r1)
    Nothing -> Left ("testRefined2P(1): bad initial predicate i=" ++ show i, show m3)

testRefined2PIO :: forall opts ip op i proxy
   . ( Show (PP ip i)
     , Refined2C opts ip op i
     , Eq i, Eq (PP ip i)
     , Show i
     ) => proxy '(opts,ip,op,i)
   -> i
   -> IO (Either String (Refined2 opts ip op i, Refined2 opts ip op i))
testRefined2PIO p i =
  case testRefined2P p i of
    Right (r,r1) -> return $ Right (r,r1)
    Left (msg, e) -> putStrLn e >> return (Left msg)

getTTs3 :: RResults2 a -> [Tree PE]
getTTs3 = \case
   RF _ t1 -> [t1]
   RTF _ t1 _ t2 -> [t1,t2]
   RTFalse _ t1 t2 -> [t1,t2]
   RTTrue _ t1 t2 -> [t1,t2]

data Results2 a =
       XF String        -- Left e
     | XTF a String     -- Right a + Left e
     | XTFalse a String -- Right a + Right False
     | XTTrue a
     deriving (Show,Eq)

toRResults2 :: RResults2 a -> Results2 a
toRResults2 = \case
   RF e _ -> XF e
   RTF a _ e _ -> XTF a e
   RTFalse a _ t2 -> XTFalse a (t2 ^. root . pString)
   RTTrue a _ _ -> XTTrue a

expect2 :: (HasCallStack, Show i, Show r, Eq i, Eq r)
  => Either (Results2 i) r
  -> (RResults2 i, Maybe r)
  -> IO ()
expect2 lhs (rhs,mr) =
  (@?=) (maybe (Left $ toRResults2 rhs) Right mr) lhs

test2a :: Either String (MakeR2 (BaseN OU 16))
test2a = newRefined2 "0000fe"

test2b :: Either String (Refined2 OU
   (Rescan "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$" Id >> Snd (Head Id) >> Map (ReadP Int Id) Id)
   (All (0 <..> 0xff) Id)
   String)
test2b = newRefined2 "123.211.122.1"

test2c :: Either String (Refined2 OU
   (Resplit "\\." Id >> Map (ReadP Int Id) Id)
   (All (0 <..> 0xff) Id && Len == 4)
   String)
test2c = newRefined2 "200.2.3.4"

test2d :: Either String (Refined2 OU
    TimeUtc
    (ToDay Id > Just (MkDay '(2020,05,31)))
    ())
test2d = newRefined2P Proxy ()
