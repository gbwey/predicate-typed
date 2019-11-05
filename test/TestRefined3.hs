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
module TestRefined3 where
import TastyExtras
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Predicate
import TestRefined hiding (namedTests,unnamedTests,allProps)
import Predicate.Refined
import Predicate.Refined3
import Predicate.Refined3Helper
import Predicate.Util_TH
import Predicate.TH_Orphans () -- need this else refined*TH' fails for dates

import Data.Ratio
import Data.Typeable
import Control.Lens
import Data.Time
import GHC.Generics (Generic)
import Data.Aeson
import Control.Monad.Cont
import Text.Show.Functions ()
import Data.Tree
import Test.QuickCheck ((===))
import qualified Data.Semigroup as SG
import GHC.TypeLits (Nat)

suite :: IO ()
suite = defaultMain $ testGroup "TestRefined3" (namedTests <> orderTests unnamedTests <> allProps)

namedTests :: [TestTree]
namedTests =
  [ testCase "ip9" $ (@?=) ($$(refined3TH "121.0.12.13") :: MakeR3 Ip9) (unsafeRefined3 [121,0,12,13] "121.000.012.013")
  , testCase "luhn check" $ (@?=) ($$(refined3TH "12345678903") :: MakeR3 CC11) (unsafeRefined3 [1,2,3,4,5,6,7,8,9,0,3] "1234-5678-903")
  , testCase "datetime utctime" $ (@?=) ($$(refined3TH "2019-01-04 23:00:59") :: MakeR3 (DateTime1 UTCTime)) (unsafeRefined3 (read "2019-01-04 23:00:59 UTC") "2019-01-04 23:00:59")
  , testCase "datetime localtime" $ (@?=) ($$(refined3TH "2019-01-04 09:12:30") :: MakeR3 (DateTime1 LocalTime)) (unsafeRefined3 (read "2019-01-04 09:12:30") "2019-01-04 09:12:30")
  , testCase "hms" $ (@?=) ($$(refined3TH "12:0:59") :: MakeR3 Hms) (unsafeRefined3 [12,0,59] "12:00:59")
  , testCase "between5and9" $ (@?=) ($$(refined3TH "7") :: Refined3 (ReadP Int Id) (Between 5 9) (Printf "%03d" Id) String) (unsafeRefined3 7 "007")
  , testCase "ssn" $ (@?=) ($$(refined3TH "123-45-6789") :: MakeR3 Ssn) (unsafeRefined3 [123,45,6789] "123-45-6789")
  , testCase "base16" $ (@?=) ($$(refined3TH "12f") :: MakeR3 (BaseN 16)) (unsafeRefined3 303 "12f")
  , testCase "daten1" $ (@?=) ($$(refined3TH "June 25 1900") :: MakeR3 DateN) (unsafeRefined3 (read "1900-06-25") "1900-06-25")
  , testCase "daten2" $ (@?=) ($$(refined3TH "12/02/99") :: MakeR3 DateN) (unsafeRefined3 (read "1999-12-02") "1999-12-02")
  , testCase "daten3" $ (@?=) ($$(refined3TH "2011-12-02") :: MakeR3 DateN) (unsafeRefined3 (read "2011-12-02") "2011-12-02")
  , testCase "ccn123" $ (@?=) ($$(refined3TH "123455") :: MakeR3 (Ccn '[1,2,3])) (unsafeRefined3 [1,2,3,4,5,5] "1-23-455")
  , testCase "readshow" $ (@?=) ($$(refined3TH "12 % 5") :: ReadShowR Rational) (unsafeRefined3 (12 % 5) "12 % 5")
  ]

unnamedTests :: [IO ()]
unnamedTests = [
    (@?=) [(unsafeRefined3 255 "ff", "")] (reads @(Refined3 (ReadBase Int 16 Id) (Between 0 255) (ShowBase 16 Id) String) "Refined3 {r3In = 255, r3Out = \"ff\"}") -- escape quotes cos read instance for String
  , (@?=) [] (reads @(Refined3 (ReadBase Int 16 Id) (Between 0 255) (ShowBase 16 Id) String) "Refined3 {r3In = 256, r3Out = \"100\"}")
  , (@?=) [(unsafeRefined3 (-1234) "-4d2", "")] (reads @(Refined3 (ReadBase Int 16 Id) (Id < 0) (ShowBase 16 Id) String) "Refined3 {r3In = -1234, r3Out = \"-4d2\"}")

  , (@?=) (unsafeRefined3 [1,2,3,4] "001.002.003.004") ($$(refined3TH "1.2.3.4") :: MakeR3 Ip)

  , expectJ (Right (G4 (unsafeRefined3 12 "12") (unsafeRefined3 [1,2,3,4] "001.002.003.004"))) (toFrom $ G4 (unsafeRefined3 12 "12") (unsafeRefined3 [1,2,3,4] "1.2.3.4"))
  , expectJ (Left ["Error in $.g4Ip", "False Boolean Check"]) (toFrom $ G4 (unsafeRefined3 12 "12") (unsafeRefined3 [1,2,3,4] "1.2.3.400"))
  , expectJ (Left ["Error in $.g4Ip", "ReadP Int (3x)"]) (toFrom $ G4 (unsafeRefined3 12 "12") (unsafeRefined3 [1,2,3,4] "1.2.3x.4"))
  , expectJ (Left ["Error in $.g4Age", "False Boolean Check"]) (toFrom $ G4 (unsafeRefined3 (-2) "-2") (unsafeRefined3 [1,2,3,4] "1.2.3.4"))
  , expectRight (testRefined3P (Proxy @(Ccn '[4,4,3])) ol "123-45-6---789-03-")
  , expectLeft (testRefined3P (Proxy @(Ccn '[4,4,3])) ol "123-45-6---789-04-")
  , expectRight (testRefined3P (Proxy @Hms) ol "1:2:33")
  , expectLeft (testRefined3P (Proxy @Hms) ol "1:2:61")
  , expectRight (testRefined3P (Proxy @(Ccn '[4,4,3])) ol "6433-1000-006")
  , expectRight (testRefined3P (Proxy @(Ccn '[4,4,3])) ol "6433-10000-06")
  , expectLeft (testRefined3P (Proxy @(Ccn '[4,4,3])) ol "6433-1000-000")
  , expectRight (testRefined3P (Proxy @(Ccn '[1,2,1])) ol "1-23-0")

  , expect3 (Left $ XF "Regex no results")
                  $ eval3 @(Rescan Ip4RE Id >> HeadFail "failedn" Id >> Map (ReadP Int Id) (Snd Id))
                          @((Len >> Same 4) && All (Between 0 255) Id)
                          @(Printfnt 4 "%03d.%03d.%03d.%03d" Id)
                          ol "1.21.x31.4"

  , expect3 (Right $ unsafeRefined3 [1,21,31,4] "001.021.031.004")
                  $ eval3 @(Rescan Ip4RE Id >> HeadFail "failedn" Id >> Map (ReadP Int Id) (Snd Id))
                          @((Len >> Same 4) && All (Between 0 255) Id)
                          @(Printfnt 4 "%03d.%03d.%03d.%03d" Id)
                          ol "1.21.31.4"

  , expect3 (Left $ XTFalse (-6.3))
                  $ eval3 @(ReadP Double Id)
                          @(Cmp 'Cgt (ToRational Id) (7 %- 3))
                          @(Printf "%5.3f" Id)
                          ol "-6.3"

  , expect3 (Right $ unsafeRefined3 4.123 "")
                  $ eval3 @(ReadP Double Id) @(Cmp 'Cgt (ToRational Id) (7 %- 3)) @""
                  ol "4.123"

  , expect3 (Right $ unsafeRefined3 4.123 (4123 % 1000))
                  $ eval3 @Id @(Gt (7 %- 3)) @(4123 % 1000)
                  ol 4.123

  , expect3 (Right $ unsafeRefined3 [1,2,3,4] "")
                  $ eval3 @(Map (ReadP Int Id) (Resplit "\\." Id)) @(All (Between 0 255) Id && (Len >> Same 4)) @""
                  ol "1.2.3.4"

  , expect3 (Left $ XTF [291,1048319,4387,17,1] "out of bounds")
                  $ eval3 @Ip6A @Ip6B @""
                  ol "123:Ffeff:1123:11:1"

  , expect3 (Right $ unsafeRefined3 [12,2,0,255] "abc")
                  $ eval3 @Ip4A @Ip4B @"abc"
                  ol "12.2.0.255"

  , expect3 (Right $ unsafeRefined3 [123,45,6789] "def")
                  $ eval3
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" Id >> OneP >> Map (ReadBaseInt 10 Id) (Snd Id))
                  @(Skip (Guard "expected 3" (Len >> Same 3))
                 |> Guard "3 digits" (Ix' 0 >> Between 0 999)
                 |> Guard "2 digits" (Ix' 1 >> Between 0 99)
                 |> Guard "4 digits" (Ix' 2 >> Between 0 9999)
                   ) @"def"
                   ol "123-45-6789"

  , expect3 (Right $ unsafeRefined3 [123,45,6789] "xyz")
                  $ eval3
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" Id >> OneP >> Map (ReadBaseInt 10 Id) (Snd Id))
                  @(GuardsQuick (Printf2 "guard(%d) %d is out of range" Id) '[Between 0 999, Between 0 99, Between 0 9999])
                  @"xyz"
                  ol "123-45-6789"

  , expect3 (Left $ XTF [0,0,0,291,1048319,4387,17,1] "out of bounds")
                  $ eval3 @Ip6A'' @Ip6B' @"xyz"
                  ol "123:Ffeff:1123:11:1"

  , expect3 (Right $ unsafeRefined3 [0,0,0,291,65535,4387,17,1] "xyz")
                  $ eval3 @Ip6A'' @Ip6B' @"xyz"
                  ol "123:Ffff:1123:11:1"

  , expect3 (Right $ unsafeRefined3 [0,0,291,0,65535,0,0,17] "xyz")
                  $ eval3 @Ip6A'' @Ip6B' @"xyz"
                  ol "123::Ffff:::11"

  , expect3 (Right $ unsafeRefined3 [31,11,1999] "xyz")
                  $ eval3 @(Rescan DdmmyyyyRE Id >> OneP >> Map (ReadBaseInt 10 Id) (Snd Id))
                           @Ddmmyyyyval
                           @"xyz"
                           ol "31-11-1999"
  , expect3 (Right $ unsafeRefined3 [123,45,6789] "xyz") $ eval3
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" Id >> OneP >> Map (ReadBaseInt 10 Id) (Snd Id))
                  @(GuardsQuick (Printf2 "guard(%d) %d is out of range" Id) '[Between 0 999, Between 0 99, Between 0 9999])
                  @"xyz"
                  ol "123-45-6789"

  , expect3 (Right $ unsafeRefined3 [1,2,3,4] "001.002.003.004") $ eval3P ip4 ol "1.2.3.4"
  , expect3 (Left $ XF "invalid base 10") $ eval3P ip4 ol "1.2.3x.4"
  , expect3 (Left $ XTF [1,2,3,4,5] "expected 4 numbers") $ eval3P ip4 ol "1.2.3.4.5"
  , expect3 (Left $ XTF [1,2,300,4] "each number must be between 0 and 255") $ eval3P ip4 ol "1.2.300.4"
  , expect3 (Right $ unsafeRefined3 [1,2,3,4,5,6,7,8,9,0,3] "1234-5678-903") $ eval3P cc ol "12345678903"
  , expect3 (Left $ XTFalse [1,2,3,4,5,6,7,8,9,0,1]) $ eval3P cc ol "12345678901"
--  , expect3 (Right $ unsafeRefined3 True ["T","r","ue","Tr","ue"]) $ eval3P (Proxy @'(Id, Id, Do '[ShowP Id, Dup, Sapa, SplitAts '[1,1,2,2]], Bool)) True
  , expect3 (Right $ unsafeRefined3 ([12,13,14],TimeOfDay 12 13 14) "12:13:14") $ eval3P hms2E ol "12:13:14"
  , expect3 (Left (XTF ([12,13,99], TimeOfDay 12 13 99) "guard(2) 99 secs is out of range")) $ eval3P hms2E ol "12:13:99"
  ]

allProps :: [TestTree]
allProps =
  [
    testProperty "base16" $ forAll (arbRefined3 (mkProxy3P @'(ReadBase Int 16 Id, 'True, ShowBase 16 Id, String))) (\r -> evalQuick @(ReadBase Int 16 Id) (r3Out r) === Right (r3In r))
  , testProperty "readshow" $ forAll (arbRefined3 Proxy :: Gen HexLtR3) (\r -> read @HexLtR3 (show r) === r)
  , testProperty "jsonroundtrip" $ forAll (arbRefined3 Proxy :: Gen HexLtR3) (\r -> testRefined3PJ Proxy ol (r3Out r) === Right r)
  ]

type HexLtR3 = Refined3 (ReadBase Int 16 Id) (Id < 500) (ShowBase 16 Id) String
type IntLtR3 = Refined3 (ReadP Int Id) (Id < 10) (ShowP Id) String

-- printf breaks with negative numbers!
type Tst1 = '(ReadP Int Id, Between 1 7, Printf "someval val=%03d" Id, String)

yy1, yy2, yy3, yy4 :: RefinedT Identity (MakeR3 Tst1)

yy1 = newRefined3TP @Identity (Proxy @Tst1) o2 "4"
yy2 = newRefined3TP @Identity (Proxy @Tst1) o2 "3"

yy3 = rapply3 o2 (*) yy1 yy2 -- fails
yy4 = rapply3 o2 (+) yy1 yy2 -- pure ()

type Ip4T = '(Ip4A, Ip4B, Ip4C, String)

ip4 :: Proxy Ip4T -- '(Ip4A, Ip4B, Ip4C, String)
ip4 = mkProxy3 -- safer cos checks that ~ Bool etc

ip4expands :: Proxy '(Ip4A, Ip4B, Ip4C, String)
ip4expands = mkProxy3

-- this works but ParseTimeP is easier
type DdmmyyyyRE = "^(\\d{2})-(\\d{2})-(\\d{4})$"
type Ddmmyyyyval' = GuardsQuick (Printf2 "guard(%d) %d is out of range" Id) '[Between 1 31, Between 1 12, Between 1990 2050]
type Ddmmyyyyval =
    Guards '[ '(Printf2 "guard(%d) day %d is out of range" Id, Between 1 31)
            , '(Printf2 "guard(%d) month %d is out of range" Id, Between 1 12)
            , '(Printf2 "guard(%d) year %d is out of range" Id, Between 1990 2050) ]

cc :: Proxy CC11
cc = mkProxy3

type Ipz1 = '(Id &&& Ip4A
           , Snd Id >> Ip4B
           , Snd Id >> Para (RepeatT 4 (Printf "%03d" Id)) >> Intercalate '["."] Id >> Concat
           , String)
type Ipz2 = '(Id, Ip4A, Ip4B, String) -- skips fmt and just uses the original input
type Ipz3 = '(Ip4A, Ip4B, Id, String)


-- need to add 'True to make it a predicate
-- guards checks also that there are exactly 3 entries!
type Hmsconv = Do '[Rescan HmsRE Id, Head Id, (Snd Id), Map (ReadBaseInt 10 Id) Id]

type Hmsz1 = '(Hmsconv &&& ParseTimeP TimeOfDay "%H:%M:%S" Id
            , Fst Id >> Hmsop
            , Snd Id
            , String)

-- better error messages cos doesnt do a strict regex match
type Hmsz2 = '(Hmsip &&& ParseTimeP TimeOfDay "%H:%M:%S" Id
             , Fst Id >> Hmsop
             , Snd Id
             , String)

type Hmsip2 = Hmsip &&& ParseTimeP TimeOfDay "%H:%M:%S" Id
type Hmsop2 = Fst Id >> Hmsop

-- >mkProxy3 @Hmsip2 @Hmsop2 @((Snd Id) >> FormatTimeP "%F %T" Id) @String
hms2E :: Proxy '(Hmsip2, Hmsop2, (Snd Id) >> FormatTimeP "%T" Id, String)
hms2E = mkProxy3P


-- better to use Guard for op boolean check cos we get better errormessages
-- 1. packaged up as a promoted tuple
type Tst3 = '(Map (ReadP Int Id) (Resplit "\\." Id), (Len >> Same 4) && All (Between 0 255) Id, ConcatMap (Printf "%03d" Id) Id, String)

www1, www2 :: String -> Either Msg3 (MakeR3 Tst3)
www1 = prtEval3P (Proxy :: MkProxy3T Tst3) o2
www2 = prtEval3P tst3 o2

-- just pass in an ipaddress as a string: eg 1.2.3.4 or 1.2.3.4.5 (invalid) 1.2.3.400 (invalid)

-- 2. packaged as a proxy
tst3 :: Proxy
        '(Map (ReadP Int Id) (Resplit "\\." Id)
        ,(Len >> Same 4) && All (Between 0 255) Id
        ,ConcatMap (Printf "%03d" Id) Id
        ,String)
tst3 = mkProxy3


-- 3. direct
ww3 :: String -> Either Msg3 (Refined3
                               (Map (ReadP Int Id) (Resplit "\\." Id))
                               ((Len >> Same 4) && All (Between 0 255) Id)
                               (ConcatMap (Printf "%03d" Id) Id)
                               String)
ww3 = prtEval3 o2
{-
ww3 = prtEval3
        @(Map (ReadP Int Id) (Resplit "\\." Id))
        @((Len >> Same 4) && All (Between 0 255))
        @(ConcatMap (Printf "%03d" Id) Id)
        o2
-}
data G4 = G4 { g4Age :: MakeR3 Age
             , g4Ip :: MakeR3 Ip9
             } deriving (Show,Generic,Eq)

type MyAge = Refined3 (ReadP Int Id) (Gt 4) (ShowP Id) String

type Age = '(ReadP Int Id, Gt 4, ShowP Id, String)

type Ip9 = '(
            Map (ReadP Int Id) (Resplit "\\." Id) -- split String on "." then convert to [Int]
           ,(Len >> Same 4) && All (Between 0 255) Id -- process [Int] and make sure length==4 and each octet is between 0 and 255
           ,Printfnt 4 "%03d.%03d.%03d.%03d" Id -- printf [Int]
           ,String -- input type is string which is also the output type
           )

instance FromJSON G4
instance ToJSON G4
{- ol= summary vs o2 = detail
prtEval3P daten ol "June 25 1900"
prtEval3P daten o2 "12/02/19"
prtEval3P (Proxy @(Ccn '[1,1,1,1])) ol "1230"
prtEval3P (Proxy @(Ccn '[1,2,3])) ol "123455" -- succeeds
-}

-- prtRefinedT tst1a
tst1a :: Monad m => POpts -> RefinedT m ((Int,String),(Int,String))
tst1a opts = withRefined3T @(ReadBase Int 16 Id) @(Between 100 200) @(ShowBase 16 Id) @String opts "a3"
  $ \r1 -> withRefined3T @(ReadP Int Id) @'True @(ShowP Id) @String opts "12"
     $ \r2 -> return ((r3In r1, r3Out r1), (r3In r2, r3Out r2))

-- prtRefinedTIO tst2a
tst2a :: MonadIO m => POpts -> RefinedT m ((Int,String),(Int,String))
tst2a opts = withRefined3TIO @(ReadBase Int 16 Id) @(Stderr "start" |> Between 100 200 >| Stdout "end") @(ShowBase 16 Id) @String opts "a3"
  $ \r1 -> withRefined3TIO @(ReadP Int Id) @'True @(ShowP Id) @String opts "12"
     $ \r2 -> return ((r3In r1, r3Out r1), (r3In r2, r3Out r2))

-- have to use 'i' as we dont hold onto the input
testRefined3PJ :: forall ip op fmt i proxy
   . (ToJSON (PP fmt (PP ip i))
    , Show (PP ip i)
    , Show (PP fmt (PP ip i))
    , Refined3C ip op fmt i
    , FromJSON i)
   => proxy '(ip,op,fmt,i)
   -> POpts
   -> i
   -> Either String (Refined3 ip op fmt i)
testRefined3PJ _ opts i =
  let (ret,mr) = eval3 @ip @op @fmt opts i
      m3 = prt3Impl opts ret
  in case mr of
    Just r -> eitherDecode @(Refined3 ip op fmt i) $ encode r
    Nothing -> Left $ show m3

-- test that roundtripping holds ie i ~ PP fmt (PP ip i)
testRefined3P :: forall ip op fmt i proxy
   . (Show (PP ip i)
    , Show (PP fmt (PP ip i))
    , Refined3C ip op fmt i
    , Eq i
    , Eq (PP ip i))
   => proxy '(ip,op,fmt,i)
   -> POpts
   -> i
   -> Either (String,String) (Refined3 ip op fmt i, Refined3 ip op fmt i)
testRefined3P _ opts i =
  let (ret,mr) = eval3 @ip @op @fmt opts i
      m3 = prt3Impl opts ret
  in case mr of
    Just r ->
      let (ret1,mr1) = eval3 @ip @op @fmt opts (r3Out r)
          m3a = prt3Impl opts ret1
      in case mr1 of
           Nothing -> Left ("testRefined3P(2): round trip failed: old(" ++ show i ++ ") new(" ++ show (r3Out r) ++ ")", show m3a)
           Just r1 ->
             if r /= r1 then Left ("testRefined3P(3): round trip pure () but values dont match: old(" ++ show i ++ ") new(" ++ show (r3Out r) ++ ")", show (r,r1))
             else Right (r,r1)
    Nothing -> Left ("testRefined3P(1): bad initial predicate i=" ++ show i, show m3)

testRefined3PIO :: forall ip op fmt i proxy
   . (Show (PP ip i), Show (PP fmt (PP ip i)), Refined3C ip op fmt i, Eq i, Eq (PP ip i))
   => proxy '(ip,op,fmt,i)
   -> POpts
   -> i
   -> IO (Either String (Refined3 ip op fmt i, Refined3 ip op fmt i))
testRefined3PIO p opts i =
  case testRefined3P p opts i of
    Right (r,r1) -> return $ Right (r,r1)
    Left (msg, e) -> putStrLn e >> return (Left msg)

getTTs3 :: RResults a b -> [Tree PE]
getTTs3 = \case
   RF _ t1 -> [t1]
   RTF _ t1 _ t2 -> [t1,t2]
   RTFalse _ t1 t2 -> [t1,t2]
   RTTrueF _ t1 t2 _ t3 -> [t1,t2,t3]
   RTTrueT _ t1 t2 _ t3 -> [t1,t2,t3]

toRResults3 :: RResults a b -> Results a b
toRResults3 = \case
   RF e _ -> XF e
   RTF a _ e _ -> XTF a e
   RTFalse a _ _ -> XTFalse a
   RTTrueF a _ _ e _ -> XTTrueF a e
   RTTrueT a _ _ b _ -> XTTrueT a b

expect3 :: (HasCallStack, Show i, Show r, Eq i, Eq r, Eq j, Show j)
  => Either (Results i j) r
  -> (RResults i j, Maybe r)
  -> IO ()
expect3 lhs (rhs,mr) = do
  (@?=) lhs $ maybe (Left $ toRResults3 rhs) Right mr

type LuhnR' (n :: Nat) = MakeR3 (LuhnX n)

type LuhnX (n :: Nat) =
   '(Map (ReadP Int Id) (Ones Id)
   , Luhn'' n
   , ConcatMap (ShowP Id) Id
   , String)

type Luhn'' (n :: Nat) =
         Skip (Guard (Printfn "incorrect number of digits found %d but expected %d in [%s]" (TupleI '[Len, W n, ShowP Id])) (Len >> Same n))
      >> Do '[
              Reverse
             ,ZipL [1,2] Id
             ,Map (Fst Id * Snd Id >> If (Id >= 10) (Id - 9) Id) Id
             ,FoldMap (SG.Sum Int) Id
             ]
        >> Skip (Guard (Printfn "expected %d mod 10 = 0 but found %d" (TupleI '[Id, Id `Mod` 10])) (Mod Id 10 >> Same 0))

type Luhn' (n :: Nat) =
       Msg "Luhn'" (Do
       '[Skip (Guard (Printfn "incorrect number of digits found %d but expected %d in [%s]" (TupleI '[Len, W n, Id])) (Len >> Same n))
        ,Do
            '[Ones Id
            ,Map (ReadP Int Id) Id
            ,Reverse
            ,ZipL [1,2] Id
            ,Map (Fst Id * Snd Id >> If (Id >= 10) (Id - 9) Id) Id
            ,FoldMap (SG.Sum Int) Id
           ]
        ,Skip (Guard (Printfn "expected %d mod 10 = 0 but found %d" (TupleI '[Id, Id `Mod` 10])) (Mod Id 10 >> Same 0))
        ])
