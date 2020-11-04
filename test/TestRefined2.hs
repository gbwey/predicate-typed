{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoStarIsType #-}
module TestRefined2 where
--module TestRefined2 (suite) where
import TastyExtras
import Test.Tasty
import Test.Tasty.HUnit

import Predicate
import Predicate.Refined2
import Predicate.Examples.Refined2
import Predicate.Examples.Common

import Data.Ratio
import Data.Typeable
import Data.Time
import GHC.Generics (Generic)
import Data.Aeson
import Text.Show.Functions ()
import Data.Tree.Lens
import Control.Lens
import Control.Arrow (left)
import qualified Safe (readNote)
import Data.Kind (Type)
import qualified GHC.TypeLits as GL
import qualified Data.Semigroup as SG

suite :: TestTree
suite =
  let s = "TestRefined2"
  in testGroup s (namedTests <> orderTests s (unnamedTests <> tst0a))

namedTests :: [TestTree]
namedTests =
  [ testCase "ip9" $ (@?=) (newRefined2 "121.0.12.13" :: Either Msg2 (MakeR2 (Ip9 OAN))) (Right (unsafeRefined2 [121,0,12,13] "121.0.12.13"))
  , testCase "luhn check" $ (@?=) (newRefined2 "12345678903" :: Either Msg2 (MakeR2 (Luhn OAN 11))) (Right (unsafeRefined2 [1,2,3,4,5,6,7,8,9,0,3] "12345678903"))
  , testCase "datetime utctime" $ (@?=) (newRefined2 "2019-01-04 23:00:59" :: Either Msg2 (MakeR2 (DateTime1 OAN UTCTime))) (Right (unsafeRefined2 (Safe.readNote "testrefined2: utc date" "2019-01-04 23:00:59 UTC") "2019-01-04 23:00:59"))
  , testCase "datetime localtime" $ (@?=) (newRefined2 "2019-01-04 09:12:30" :: Either Msg2 (MakeR2 (DateTime1 OAN LocalTime))) (Right (unsafeRefined2 (Safe.readNote "testrefined2: localtime" "2019-01-04 09:12:30") "2019-01-04 09:12:30"))
  , testCase "hms" $ (@?=) (newRefined2 "12:0:59" :: Either Msg2 (MakeR2 (Hms OAN))) (Right (unsafeRefined2 [12,0,59] "12:0:59"))
  , testCase "between5and9" $ (@?=) (newRefined2 "7" :: Either Msg2 (Refined2 OAN (ReadP Int Id) (Between 5 9 Id) String)) (Right (unsafeRefined2 7 "7"))
  , testCase "ssn" $ (@?=) (newRefined2 "123-45-6789" :: Either Msg2 (MakeR2 (Ssn OAN))) (Right (unsafeRefined2 [123,45,6789] "123-45-6789"))
  , testCase "base16" $ (@?=) (newRefined2 "12f" :: Either Msg2 (MakeR2 (BaseN OAN 16))) (Right (unsafeRefined2 303 "12f"))
  , testCase "daten1" $ (@?=) (newRefined2 "June 25 1900" :: Either Msg2 (MakeR2 (DateN OAN))) (Right (unsafeRefined2 (Safe.readNote "testrefined2: daten1" "1900-06-25") "June 25 1900"))
  , testCase "daten2" $ (@?=) (newRefined2 "12/02/99" :: Either Msg2 (MakeR2 (DateN OAN))) (Right (unsafeRefined2 (Safe.readNote "testrefined2: daten2" "1999-12-02") "12/02/99"))
  , testCase "daten3" $ (@?=) (newRefined2 "2011-12-02" :: Either Msg2 (MakeR2 (DateN OAN))) (Right (unsafeRefined2 (Safe.readNote "testrefined2: daten3" "2011-12-02") "2011-12-02"))
  , testCase "ccn123" $ (@?=) (newRefined2 "123455" :: Either Msg2 (MakeR2 (Luhn OAN 6))) (Right (unsafeRefined2 [1,2,3,4,5,5] "123455"))
  ]

unnamedTests :: [IO ()]
unnamedTests = [
    (@?=) [(unsafeRefined2 255 "ff", "")] (reads @(Refined2 OAN (ReadBase Int 16) (0 <..> 0xff) String) "Refined2 255 \"ff\"") -- escape quotes as it is a read instance for String
  , (@?=) [] (reads @(Refined2 OAN (ReadBase Int 16) (0 <..> 0xff) String) "Refined2 256 \"100\"")
  , (@?=) [(unsafeRefined2 (-1234) "-4d2", "")] (reads @(Refined2 OAN (ReadBase Int 16) (Id < 0) String) "Refined2 (-1234) \"-4d2\"")
  , (@?=) (Right (unsafeRefined2 [1,2,3,4] "1.2.3.4")) (newRefined2 "1.2.3.4" :: Either Msg2 (Ip4R OAN))
  , expectJ (Right (G4 (unsafeRefined2 12 "12") (unsafeRefined2 [1,2,3,4] "1.2.3.4"))) (toFrom $ G4 @OAN (unsafeRefined2 12 "12") (unsafeRefined2 [1,2,3,4] "1.2.3.4"))
  , expectJ (Left ["Error in $.g4Ip", "False Boolean Check"]) (toFrom $ G4 @OAN (unsafeRefined2 12 "12") (unsafeRefined2 [1,2,3,4] "1.2.3.400"))
  , expectJ (Left ["Error in $.g4Ip", "ReadP Int (3x)"]) (toFrom $ G4 @OAN (unsafeRefined2 12 "12") (unsafeRefined2 [1,2,3,4] "1.2.3x.4"))
  , expectJ (Left ["Error in $.g4Age", "False Boolean Check"]) (toFrom $ G4 @OAN (unsafeRefined2 (-2) "-2") (unsafeRefined2 [1,2,3,4] "1.2.3.4"))
  , expectRight (testRefined2P (Proxy @(Luhn OAN 11)) "123-45-6---789-03-")
  , expectLeft (testRefined2P (Proxy @(Luhn OAN 11)) "123-45-6---789-04-")
  , expectRight (testRefined2P (Proxy @(Hms OAN)) "1:2:33")
  , expectLeft (testRefined2P (Proxy @(Hms OAN)) "1:2:61")
  , expectRight (testRefined2P (Proxy @(Luhn OAN 11)) "6433-1000-006")
  , expectRight (testRefined2P (Proxy @(Luhn OAN 11)) "6433-10000-06")
  , expectLeft (testRefined2P (Proxy @(Luhn OAN 11)) "6433-1000-000")
  , expectRight (testRefined2P (Proxy @(Luhn OAN 4)) "1-23-0")

  , expect2 (Left $ XF "Regex no results")
                  $ runIdentity $ eval2M @OAN @(Rescan Ip4RE >> HeadFail "failedn" Id >> Map' (ReadP Int Id) Snd)
                          @((Len == 4) && All (0 <..> 0xff))
                          "1.21.x31.4"

  , expect2 (Right $ unsafeRefined2 [1,21,31,4] "1.21.31.4")
                  $ runIdentity $ eval2M @OAN @(Rescan Ip4RE >> HeadFail "failedn" Id >> Map' (ReadP Int Id) Snd)
                          @((Len == 4) && All (0 <..> 0xff))
                          "1.21.31.4"

  , expect2 (Left $ XTFalse (-6.5) "(-13) % 2 > (-7) % 3")
                  $ runIdentity $ eval2M @OAN @(ReadP Double Id)
                          @(ToRational Id > 7 -% 3)
                          "-6.5"

  , expect2 (Right $ unsafeRefined2 4.123 "4.123")
                  $ runIdentity $ eval2M @OAN @(ReadP Double Id) @(ToRational Id > 7 -% 3)
                  "4.123"

  , expect2 (Right $ unsafeRefined2 4.123 (4123 % 1000))
                  $ runIdentity $ eval2M @OAN @Id @(Gt (7 -% 3)) 4.123

  , expect2 (Right $ unsafeRefined2 [1,2,3,4] "1.2.3.4")
                  $ runIdentity $ eval2M @OAN @(Map' (ReadP Int Id) (Resplit "\\.")) @(All (0 <..> 0xff) && (Len == 4)) "1.2.3.4"

  , expect2 (Left $ XTFalse [0,0,0,291,1048319,4387,17,1] "True && False | (out of bounds: All(8) i=4 (1048319 <= 65535))")
                  $ runIdentity $ eval2M @OAN @Ip6ip @Ip6op "123:Ffeff:1123:11:1"

  , expect2 (Right $ unsafeRefined2 [12,2,0,255] "12.2.0.255")
                  $ runIdentity $ eval2M @OAN @Ip4ip @Ip4op' "12.2.0.255"

  , expect2 (Right $ unsafeRefined2 [123,45,6789] "123-45-6789")
                  $ runIdentity $ eval2M @OAN
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" >> OneP >> Map' (ReadBase Int 10) Snd)
                  @(GuardBool "expected 3" (Len == 3)
                  && GuardBool "3 digits" (Between 0 999 (Ix' 0))
                  && GuardBool "2 digits" (Between 0 99 (Ix' 1))
                  && GuardBool "4 digits" (Between 0 9999 (Ix' 2))
                   )
                   "123-45-6789"

  , expect2 (Right $ unsafeRefined2 [123,45,6789] "123-45-6789")
                  $ runIdentity $ eval2M @OAN
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" >> OneP >> Map' (ReadBase Int 10) Snd)
                  @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 0 999 Id, Between 0 99 Id, Between 0 9999 Id] >> 'True)
                  "123-45-6789"

  , expect2 (Left $ XTFalse [0,0,0,291,1048319,4387,17,1] "True && False | (out of bounds: All(8) i=4 (1048319 <= 65535))")
                  $ runIdentity $ eval2M @OAN @Ip6ip @Ip6op
                  "123:Ffeff:1123:11:1"

  , expect2 (Left $ XTFalse [0,0,0,291,1048319,4387,17,1] "True && False | (out of bounds: All(8) i=4 (1048319 <= 65535))")
                  $ runIdentity $ eval2M @OAN @Ip6ip @Ip6op
                  "123:Ffeff:1123:11:1"

  , expect2 (Right $ unsafeRefined2 [0,0,0,291,65535,4387,17,1] "123:Ffff:1123:11:1")
                  $ runIdentity $ eval2M @OAN @Ip6ip @Ip6op
                  "123:Ffff:1123:11:1"

  , expect2 (Right $ unsafeRefined2 [0,0,291,0,65535,0,0,17] "123::Ffff:::11")
                  $ runIdentity $ eval2M @OAN @Ip6ip @Ip6op
                  "123::Ffff:::11"

  , expect2 (Right $ unsafeRefined2 [0,0,291,0,65535,0,0,17] "123::Ffff:::11")
                  $ runIdentity $ eval2M @OAN @Ip6ip @Ip6op
                  "123::Ffff:::11"

  , expect2 (Right $ unsafeRefined2 [31,11,1999] "31-11-1999")
                  $ runIdentity $ eval2M @OAN @(Rescan DdmmyyyyRE >> OneP >> Map' (ReadBase Int 10) Snd)
                           @Ddmmyyyyop
                           "31-11-1999"
  , expect2 (Right $ unsafeRefined2 [123,45,6789] "123-45-6789") $ runIdentity $ eval2M @OAN
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" >> OneP >> Map' (ReadBase Int 10) Snd)
                  @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 0 999 Id, Between 0 99 Id, Between 0 9999 Id] >> 'True)
                  "123-45-6789"

  , expect2 (Right $ unsafeRefined2 [1,2,3,4] "1.2.3.4") $ runIdentity $ eval2P (ip4 @OAN) "1.2.3.4"
  , expect2 (Left $ XF "ReadP Int (3x)") $ runIdentity $ eval2P (ip4 @OAN) "1.2.3x.4"
  , expect2 (Left $ XTF [1,2,3,4,5] "Bools:invalid length(5) expected 4") $ runIdentity $ eval2P (ip4' @OAN) "1.2.3.4.5"
  , expect2 (Left $ XTF [1,2,3,4,5] "Guards:invalid length(5) expected 4") $ runIdentity $ eval2P (ip4 @OAN) "1.2.3.4.5"
  , expect2 (Left $ XTF [1,2,300,4] "Bool(2) [octet 2 out of range 0-255 found 300] (300 <= 255)") $ runIdentity $ eval2P (ip4' @OAN) "1.2.300.4"
  , expect2 (Left $ XTF [1,2,300,4] "octet 2 out of range 0-255 found 300") $ runIdentity $ eval2P (ip4 @OAN) "1.2.300.4"
  , expect2 (Right $ unsafeRefined2 [1,2,3,4,5,6,7,8,9,0,3] "12345678903") $ runIdentity $ eval2P (luhn11 @OAN) "12345678903"
  , expect2 (Left $ XTF [1,2,3,4,5,6,7,8,9,0,1] "invalid checkdigit") $ runIdentity $ eval2P (luhn11 @OZ) "12345678901"
  ]

-- better to use Guard for op boolean check as we get better error messages
-- 1. packaged up as a promoted tuple
type Tst3 (opts :: Opt) = '(opts, Map' (ReadP Int Id) (Resplit "\\."), (Len == 4) && All (0 <..> 0xff), String)

www1, www2 :: String -> Either Msg2 (MakeR2 (Tst3 OAN))
www1 = newRefined2P (Proxy @(Tst3 OAN))
www2 = newRefined2P tst3

-- just pass in an ipaddress as a string: eg 1.2.3.4 or 1.2.3.4.5 (invalid) 1.2.3.400 (invalid)

-- 2. packaged as a proxy
tst3 :: Proxy
        '(OAN, Map' (ReadP Int Id) (Resplit "\\.")
        ,(Len == 4) && All (0 <..> 0xff)
        ,String)
tst3 = Proxy


-- 3. direct
www3, www3' :: String -> Either Msg2 (Refined2 OAN
                               (Map' (ReadP Int Id) (Resplit "\\."))
                               ((Len == 4) && All (0 <..> 0xff))
                               String)
www3 = newRefined2

www3' = newRefined2
        @OAN
        @(Map' (ReadP Int Id) (Resplit "\\."))
        @((Len == 4) && All (0 <..> 0xff))

data G4 (opts :: Opt) = G4
             { g4Age :: !(MakeR2 (Age opts))
             , g4Ip :: !(MakeR2 (Ip9 opts))
             } deriving (Show,Generic,Eq)

--type MyAge (opts :: Opt) = Refined2 opts (ReadP Int Id) (Gt 4) String

type Age (opts :: Opt) = '(opts, ReadP Int Id, Gt 4, String)

type Ip9 (opts :: Opt) = '(opts,
            Map' (ReadP Int Id) (Resplit "\\.") -- split String on "." then convert to [Int]
           ,Len == 4 && All (0 <..> 0xff) -- process [Int] and make sure length==4 and each octet is between 0 and 255
           ,String -- input type is string which is also the output type
           )

instance OptC opts => FromJSON (G4 opts)
instance OptC opts => ToJSON (G4 opts)

tst0a :: [Assertion]
tst0a =
  [ newRefined2P (daten @OL) "June 25 1900" @?= Right (unsafeRefined2 (fromGregorian 1900 6 25) "June 25 1900")
  , newRefined2P (daten @OL) "12/02/19" @?= Right (unsafeRefined2 (fromGregorian 2019 12 2) "12/02/19")
  , newRefined2P (Proxy @(Luhn OL 4)) "1230" @?= Right (unsafeRefined2 [1,2,3,0] "1230")
  , newRefined2P (Proxy @(Luhn OL 6)) "123455" @?= Right (unsafeRefined2 [1,2,3,4,5,5] "123455")
  , test2a @?= Right (unsafeRefined2 254 "0000fe")
  , test2b @?= Right (unsafeRefined2 [123,211,122,1] "123.211.122.1")
  , test2c @?= Right (unsafeRefined2 [200,2,3,4] "200.2.3.4")
  , expectIO (left show <$> test2d) (() <$)
  , www1 "1.2.3.4" @?= Right (unsafeRefined2 [1,2,3,4] "1.2.3.4")
  , www2 "1.2.3.4" @?= Right (unsafeRefined2 [1,2,3,4] "1.2.3.4")
  , www3 "1.2.3.4" @?= Right (unsafeRefined2 [1,2,3,4] "1.2.3.4")
  , www3' "1.2.3.4" @?= Right (unsafeRefined2 [1,2,3,4] "1.2.3.4")
  , tst1a @'OAN @=? Right ((163,"a3"),(12,"12"))
  ]

tst1a :: forall (opts :: Opt) . OptC opts => Either Msg2 ((Int,String),(Int,String))
tst1a = do
  r1 <- newRefined2 @opts @(ReadBase Int 16) @(Between 100 200 Id) @String "a3"
  r2 <- newRefined2 @opts @(ReadP Int Id) @'True @String "12"
  return ((r2In r1, r2Out r1), (r2In r2, r2Out r2))

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
  case newRefined2 @opts @ip @op i of
    Right r ->
      case newRefined2 @opts @ip @op (r2Out r) of
        Left m3a -> Left ("testRefined2P(2): round trip failed: old(" ++ show i ++ ") new(" ++ show (r2Out r) ++ ")", show m3a)
        Right r1 ->
             if r /= r1 then Left ("testRefined2P(3): round trip pure () but values dont match: old(" ++ show i ++ ") new(" ++ show (r2Out r) ++ ")", show (r,r1))
             else Right (r,r1)
    Left m3 -> Left ("testRefined2P(1): bad initial predicate i=" ++ show i, show m3)
{-
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
-}
data Results2 a =
       XF !String        -- Left e
     | XTF !a !String     -- Right a + Left e
     | XTFalse !a !String -- Right a + Right False
     | XTTrue !a
     deriving (Show,Eq)

toRResults2 :: RResults2 a -> Results2 a
toRResults2 = \case
   RF e _ -> XF e
   RTF a _ e _ -> XTF a e
   RTFalse a _ t2 -> XTFalse a (t2 ^. root . peString)
   RTTrue a _ _ -> XTTrue a

expect2 :: (HasCallStack, Show i, Show r, Eq i, Eq r)
  => Either (Results2 i) r
  -> (RResults2 i, Maybe r)
  -> IO ()
expect2 lhs (rhs,mr) =
  (@?=) (maybe (Left $ toRResults2 rhs) Right mr) lhs

test2a :: Either Msg2 (MakeR2 (BaseN OAN 16))
test2a = newRefined2 "0000fe"

test2b :: Either Msg2 (Refined2 OAN
   (Rescan "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$" >> Head >> Snd >> Map (ReadP Int Id))
   (All (0 <..> 0xff))
   String)
test2b = newRefined2 "123.211.122.1"

test2c :: Either Msg2 (Refined2 OAN
   (Resplit "\\." >> Map (ReadP Int Id))
   (All (0 <..> 0xff) && Len == 4)
   String)
test2c = newRefined2 "200.2.3.4"

test2d :: IO (Either Msg2 (Refined2 OAN
    TimeUtc
    (ToDay > 'Just (MkDay '(2020,05,31)))
    ()))
test2d = newRefined2P' Proxy ()

testKindSignature2A :: Either Msg2
    (Refined2 OU
      Id
      (PApp (Proxy (Lift "abc" :: Type -> Type)) (Proxy ()) >> 'True)
      ())
testKindSignature2A = newRefined2 ()

testKindSignature2B :: Either Msg2
   (Refined2 OU
     Id
     (Pop1' (Proxy (Lift "abc" :: GL.Nat -> Type)) (Proxy 4) () >> 'True)
     ())
testKindSignature2B = newRefined2 ()

testKindSignature2C :: Either Msg2
   (Refined2 OU
     Id
     (Pop2' (Proxy ('(,) :: Type -> Bool -> (Type,Bool))) (Proxy (W "bbb")) Fst Snd >> Snd)
     (Proxy 'True, Int))
testKindSignature2C = newRefined2 (Proxy @'True,1234)

testKindSignature2D :: Either Msg2
   (Refined2 OU
     (Second (ReadP Int Id))
     (PApp2 (Proxy ('(,) :: GL.Symbol -> Bool -> (GL.Symbol,Bool))) (Proxy "bbb") Fst >> Pop0 Id () >> Snd)
     (Proxy 'True, String))
testKindSignature2D = newRefined2 (Proxy @'True,"1234")

testKindSignature2E :: Either Msg2
   (Refined2 OU
     '(Id,4 % 6)
     (Pop1 (Proxy (MkJust :: Type -> Type)) L13 Id >> Just' >> Len > 3)
     (Int,Int,String))
testKindSignature2E = newRefined2 (1,2,"defghi")

testKindSignature2F :: Either Msg2
   (Refined2 OU
     (Second (Map (ReadP Int '[Id])))
     (Pop1 (Proxy (FoldAla :: Type -> Type)) (SG.Product Int) Snd >> Id > 4)
     (Char,String))
testKindSignature2F = newRefined2 ('x',"23498")

testKindSignature2G :: Either Msg2
   (Refined2 OU
     (Id <> Id)
     (PApp (Proxy ((<>) :: GL.Symbol -> GL.Symbol -> Type)) Id >> PApp Id (Proxy "def") >> Pop0 Id () >> Len > 6)
     (Proxy "1234"))
testKindSignature2G = newRefined2 (Proxy @"1234")
