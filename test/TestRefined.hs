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
{-# LANGUAGE LambdaCase #-}
module TestRefined where
import Test.Tasty
import Test.Tasty.HUnit
import Predicate
import Data.Tree
import Refined
import Refined3
import Refined3Helper
import Control.Lens
import qualified RefinedE as RE
import UtilP
import UtilP_TH
import GHC.TypeLits (Symbol,Nat)
import Data.Typeable
import Data.Time
import GHC.Generics (Generic)
import Data.Aeson
import TH_Orphans () -- need this else refined*TH' fails for dates
import Control.Monad.Cont
import Control.Arrow
import Data.List

suite :: IO ()
suite = defaultMain $ testGroup "testrefined"
  [ testCase "ok3 ip9" $ (@?=) ($$(refined3TH "121.0.12.13") :: MakeR3 Ip9) (unsafeRefined3 [121,0,12,13] "121.000.012.013")
  , testCase "ok3 luhn check" $ (@?=) ($$(refined3TH "12345678903") :: MakeR3 CC11) (unsafeRefined3 [1,2,3,4,5,6,7,8,9,0,3] "1234-5678-903")
  , testCase "ok3 datetime utctime" $ (@?=) ($$(refined3TH "2019-01-04 23:00:59") :: MakeR3 (DateTime1 UTCTime)) (unsafeRefined3 (read "2019-01-04 23:00:59 UTC") "2019-01-04 23:00:59")
  , testCase "ok3 datetime localtime" $ (@?=) ($$(refined3TH "2019-01-04 09:12:30") :: MakeR3 (DateTime1 LocalTime)) (unsafeRefined3 (read "2019-01-04 09:12:30") "2019-01-04 09:12:30")
  , testCase "ok3 hms" $ (@?=) ($$(refined3TH "12:0:59") :: MakeR3 Hms) (unsafeRefined3 [12,0,59] "12:00:59")
  , testCase "always true" $ (@?=) ($$(refinedTH 7) :: Refined 'True Int) (unsafeRefined 7)
  , testCase "between5and9" $ (@?=) ($$(refinedTH 7) :: Refined (Between 5 9) Int) (unsafeRefined 7)
  , testCase "ok3 between5and9" $ (@?=) ($$(refined3TH "7") :: Refined3 (ReadP Int) (Between 5 9) (Printf "%03d") String) (unsafeRefined3 7 "007")
  , testCase "ok3 ssn" $ (@?=) ($$(refined3TH "123-45-6789") :: MakeR3 Ssn) (unsafeRefined3 [123,45,6789] "123-45-6789")
  , testCase "ok3 base16" $ (@?=) ($$(refined3TH "12f") :: MakeR3 (BaseN 16)) (unsafeRefined3 303 "12f")
  , testCase "ok3 daten1" $ (@?=) ($$(refined3TH "June 25 1900") :: MakeR3 DateN) (unsafeRefined3 (read "1900-06-25") "1900-06-25")
  , testCase "ok3 daten2" $ (@?=) ($$(refined3TH "12/02/99") :: MakeR3 DateN) (unsafeRefined3 (read "1999-12-02") "1999-12-02")
  , testCase "ok3 daten3" $ (@?=) ($$(refined3TH "2011-12-02") :: MakeR3 DateN) (unsafeRefined3 (read "2011-12-02") "2011-12-02")
  , testCase "ok3 ccn123" $ (@?=) ($$(refined3TH "123455") :: MakeR3 (Ccn '[1,2,3])) (unsafeRefined3 [1,2,3,4,5,5] "1-23-455")
  ]

type Ip4 = ("^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$" :: Symbol)

type Ip4' = Rescan Ip4 >> OneP >> Snd >> Map (ReadBaseInt 10) >> Ip4guard

type Ip4guard = Guard "4octets" (Len >> Same 4) >> Guard "0-255" (All (Between 0 255))

type Ip6 = Resplit ":"
        >> Guard "count is bad" (Len >> Between 0 8)
        >> Guard "not a hex" (All (All (Elem Id "abcdefABCDEF0123456789")))
        >> Guard "len is bad" (All (Len >> Le 4))

type Ip6A = Resplit ":"
         >> Map (If (Id == "") "0" Id)
         >> Map (ReadBaseInt 16)

type Ip6B = Guard "count is bad" (Len >> Between 0 8)
         >> Guard "out of bounds" (All (Between 0 65535))
         >> 'True

type Ip6A' = Resplit ":"
         >> Map (If (Id == "") "0" Id)
         >> Map (ReadBaseInt 16)
         >> PadL 8 0 Id

type Ip6A'' = Resplit ":" >> Map (If (Id == "") 0 (ReadBaseInt 16)) >> PadL 8 0 Id

type Ip6B' = Guard "count is bad" (Len >> Same 8)
         >> Guard "out of bounds" (All (Between 0 65535))
         >> 'True

type Ip4A = Resplit "\\." >> Map (ReadBaseInt 10)
type Ip4B = Guard "expected 4 numbers" (Len >> Same 4)
         >> Guard "each number must be between 0 and 255" (All (Between 0 255))
         >> 'True

type Ip4C = Printfnt 4 "%03d.%03d.%03d.%03d"

type Tst1 = '(ReadP Int, Between 1 7, Printf "someval val=%03d", String)

yy1, yy2, yy3, yy4 :: RefinedT Identity (MakeR3 Tst1) -- MakeRVT Identity Tst1

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
type DdmmyyyyR = "^(\\d{2})-(\\d{2})-(\\d{4})$"
type Ddmmyyyyval' = Guards (ToGuardsT (Printf2 "guard(%d) %d is out of range") '[Between 1 31, Between 1 12, Between 1990 2050])
type Ddmmyyyyval =
    Guards '[ '(Printf2 "guard(%d) day %d is out of range", Between 1 31)
            , '(Printf2 "guard(%d) month %d is out of range", Between 1 12)
            , '(Printf2 "guard(%d) year %d is out of range", Between 1990 2050) ]

cc :: Proxy CC11
cc = mkProxy3

-- base n number of length x and then convert to a list of length x of (0 to (n-1))
-- checks that each digit is between 0 and n-1
type MM1 (n :: Nat) = Ones >> Map (ReadBase Int n)
type MM2 (n :: Nat) = Exitwhen "found empty" IsEmpty >> Guard "0<=x<n" (All (Ge 0 && Lt n))

type Ipz1 = '(Id &&& Ip4A
           , Snd >> Ip4B
           , Snd >> Para (RepeatT 4 (Printf "%03d")) >> Intercalate '["."] Id >> Concat
           , String)
type Ipz2 = '(Id, Ip4A, Ip4B, String) -- skips fmt and just uses the original input
type Ipz3 = '(Ip4A, Ip4B, Id, String)

-- need to add 'True to make it a predicate
-- guards checks also that there are exactly 3 entries!
type Hmsz1 = '(Hmsconv &&& ParseTimeP TimeOfDay "%H:%M:%S" Id
            , Fst >> Hmsval >> 'True
            , Snd
            , String)

-- better error messages cos doesnt do a strict regex match
type Hmsz2 = '(Hmsip &&& ParseTimeP TimeOfDay "%H:%M:%S" Id
             , Fst >> Hmsop >> 'True
             , Snd
             , String)

type Hmsip2 = Hmsip &&& ParseTimeP TimeOfDay "%H:%M:%S" Id
type Hmsop2 = Fst >> Hmsop >> 'True

-- >mkProxy3 @Hmsip2 @Hmsop2 @(Snd >> FormatTimeP "%F %T") @String
hms2E :: Proxy '(Hmsip2, Hmsop2, Snd >> FormatTimeP "%T", String)
hms2E = RE.mkProxy3E

-- better to use Guard for op boolean check cos we get better errormessages
-- 1. packaged up as a promoted tuple
type Tst3 = '(Resplit "\\." >> Map (ReadP Int), (Len >> Same 4) && All (Between 0 255), Map (Printf "%03d") >> Concat, String)

www1, www2 :: String -> Either Msg3 (MakeR3 Tst3)
www1 = prt3 o2 . eval3P (Proxy :: MkProxyT Tst3) o2
www2 = prt3 o2 . eval3P tst3 o2

-- just pass in an ipaddress as a string: eg 1.2.3.4 or 1.2.3.4.5 (invalid) 1.2.3.400 (invalid)
ww1, ww2, ww3 :: String -> Either Msg3 (Refined ((Len >> Same 4) && All (Between 0 255)) [Int], String)
ww1 = prt3 o2 . eval3PX (Proxy :: MkProxyT Tst3) o2

-- 2. packaged as a proxy
tst3 :: Proxy
        '(Resplit "\\." >> Map (ReadP Int)
        ,(Len >> Same 4) && All (Between 0 255)
        ,Map (Printf "%03d") >> Concat
        ,String)
tst3 = mkProxy3

ww2 = prt3 o2 . eval3PX tst3 o2

-- 3. direct (has the advantage that we dont need to specify String

ww3 = prt3 o2 . eval3PX
        @(Resplit "\\." >> Map (ReadP Int))
        @((Len >> Same 4) && All (Between 0 255))
        @(Map (Printf "%03d") >> Concat)
        Proxy
        o2

data G4 = G4 { g4Age :: MakeR3 Age
             , g4Ip :: MakeR3 Ip9
             } deriving (Show,Generic,Eq)

type MyAge = Refined3 (ReadP Int) (Gt 4) ShowP String

type Age = '(ReadP Int, Gt 4, ShowP, String)

type Ip9 = '(
            Resplit "\\." >> Map (ReadP Int) -- split String on "." then convert to [Int]
           ,(Len >> Same 4) && All (Between 0 255) -- process [Int] and make sure length==4 and each octet is between 0 and 255
           ,Printfnt 4 "%03d.%03d.%03d.%03d" -- printf [Int]
           ,String -- input type is string which is also the output type
           )

instance FromJSON G4
instance ToJSON G4

type Fizzbuzz = Id &&& If (Id `Mod` 3==0) "fizz" "" <> If (Id `Mod` 5==0) "buzz" ""
type Fizzbuzz' = Id &&& Case "" '[Id `Mod` 15 == 0, Id `Mod` 3 == 0, Id `Mod` 5 == 0] '["fizzbuzz", "fizz", "buzz"] Id
type Fizzbuzz'' t t1 = Case (MkLeft t Fst) '[Id `Mod` 15 == 0, Id `Mod` 3 == 0, Id `Mod` 5 == 0] '[ MkRight t1 "fizzbuzz", MkRight t1 "fizz", MkRight t1 "buzz"] Id
--type Fizzbuzz''' = Case (MkLeft' (Snd >> Proxyfabb >> Unproxy) Fst) '[Id `Mod` 15 == 0, Id `Mod` 3 == 0, Id `Mod` 5 == 0] '[ MkRight Int "fizzbuzz", MkRight Int "fizz", MkRight Int "buzz"] Id
type Fizzbuzz'''' = Case (MkLeft String Fst) '[Id `Mod` 15 == 0, Id `Mod` 3 == 0, Id `Mod` 5 == 0] '[ MkRight Int "fizzbuzz", MkRight Int "fizz", MkRight Int "buzz"] Id
-- this is also good: makes use of type family MapT which does the apply on ADTs: so type synonyms dont work
type Fizzbuzznew = Case (MkLeft String Fst) '[Id `Mod` 15 == 0, Id `Mod` 3 == 0, Id `Mod` 5 == 0] (MapT (MkRight' (Hole Int)) '[ "fizzbuzz", "fizz", "buzz"]) Id

type Fizzbuzzalt = '(Id,  If (Id `Mod` 3==0) "fizz" "" <> If (Id `Mod` 5==0) "buzz" "")
type Fizzbuzzs = Map Fizzbuzz
type Fizzbuzzs1 t t1 = Map (Fizzbuzz >> If (Snd >> Null) (MkLeft t Fst) (MkRight t1 Snd))
type Fizzbuzzs2 = Map (Fizzbuzz >> If (Snd >> Null) (MkLeft String Fst) (MkRight Int Snd))
-- best one cos leverages type info to determine Either a b
type Fizzbuzzs3 = Map (Fizzbuzz >> If (Snd == "") (MkLeft' Snd Fst) (MkRight' Fst Snd))

{- ol= summary vs o2 = detail
prtEval3P daten ol "June 25 1900"
prtEval3P daten o2 "12/02/19"
prtEval3P (Proxy @(Ccn '[1,1,1,1])) ol "1230"
prtEval3P (Proxy @(Ccn '[1,2,3])) ol "123455" -- succeeds
-}

-- prtRefinedT tst1
tst1 :: Monad m => RefinedT m (Int,Int)
tst1 = withRefinedT @(Between 2 11) o2 10
  $ \x -> withRefinedT @(Between 200 211) o2 10
     $ \y -> return (unRefined x, unRefined y)

-- prtRefinedTIO tst2
tst2 :: MonadIO m => RefinedT m (Int,Int)
tst2 = withRefinedTIO @(Between 2 11) o2 10
  $ \x -> withRefinedTIO @(Stderr "start" |> Between 200 211 >| Stderr "end") o2 10
     $ \y -> return (unRefined x, unRefined y)

-- prtRefinedT tst1a
tst1a :: Monad m => RefinedT m ((Int,String),(Int,String))
tst1a = withRefined3T @(ReadBase Int 16) @(Between 100 200) @(ShowBase 16) @String o2 "a3"
  $ \r1 -> withRefined3T @(ReadP Int) @'True @ShowP @String o2 "12"
     $ \r2 -> return ((in3 r1, out3 r1), (in3 r2, out3 r2))

-- prtRefinedTIO tst2a
tst2a :: MonadIO m => RefinedT m ((Int,String),(Int,String))
tst2a = withRefined3TIO @(ReadBase Int 16) @(Stderr "start" |> Between 100 200 >| Stdout "end") @(ShowBase 16) @String o2 "a3"
  $ \r1 -> withRefined3TIO @(ReadP Int) @'True @ShowP @String o2 "12"
     $ \r2 -> return ((in3 r1, out3 r1), (in3 r2, out3 r2))

-- have to use 'i' as we dont hold onto the input
testRefined3PJ :: forall ip op fmt i proxy
   . (ToJSON (PP fmt (PP ip i)), Show (PP ip i), Show (PP fmt (PP ip i)), Refined3C ip op fmt i, FromJSON i)
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
   . (Show (PP ip i), Show (PP fmt (PP ip i)), Refined3C ip op fmt i, Eq i, Eq (PP ip i))
   => proxy '(ip,op,fmt,i)
   -> POpts
   -> i
   -> Either (String,String) (Refined3 ip op fmt i, Refined3 ip op fmt i)
testRefined3P _ opts i =
  let (ret,mr) = eval3 @ip @op @fmt opts i
      m3 = prt3Impl opts ret
  in case mr of
    Just r ->
      let (ret1,mr1) = eval3 @ip @op @fmt opts (out3 r)
          m3a = prt3Impl opts ret1
      in case mr1 of
           Nothing -> Left ("testRefined3P(2): round trip failed: old(" ++ show i ++ ") new(" ++ show (out3 r) ++ ")", show m3a)
           Just r1 ->
             if r /= r1 then Left ("testRefined3P(3): round trip pure () but values dont match: old(" ++ show i ++ ") new(" ++ show (out3 r) ++ ")", show (r,r1))
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

getTTsE :: RE.RResults a -> [Tree PE]
getTTsE = \case
   RE.RF _ t1 -> [t1]
   RE.RTF _ t1 _ t2 -> [t1,t2]
   RE.RTFalse _ t1 t2 -> [t1,t2]

toRResultsE :: RE.RResults a -> RE.Results a
toRResultsE = \case
   RE.RF e _ -> RE.XF e
   RE.RTF a _ e _ -> RE.XTF a e
   RE.RTFalse a _ _ -> RE.XTFalse a

-- does tojson then fromjson in one go
testRefinedEP :: forall ip op i proxy
   . (ToJSON (PP ip i), RE.RefinedEC ip op i, FromJSON (PP ip i))
   => proxy '(ip,op,i)
   -> POpts
   -> i
   -> Either String (Refined op (PP ip i))
testRefinedEP p opts i =
  case RE.evalEP p opts i of
    Right r -> eitherDecode @(Refined op (PP ip i)) $ encode r
    Left ret -> Left $ RE.msgRResults ret

-- roundtrip tojson then fromjson
testRefined :: forall p a
   . (ToJSON a, FromJSON a, RefinedC p a)
   => POpts
   -> a
   -> Either String (Refined p a)
testRefined opts a =
   let ((bp,e),mr) = runIdentity $ newRefined @p opts a
   in case mr of
        Nothing -> error $ show bp ++ "\n" ++ e
        Just r -> eitherDecode @(Refined p a) $ encode r

expectIO :: (HasCallStack, Show a) => IO (Either String a) -> (Either String a -> Either String ()) -> IO ()
expectIO iolr p = do
  lr <- iolr
  case p lr of
    Left e -> assertFailure $ "expectIO: " <> e <> " lr=" <> show lr
    Right () -> pure ()

expectLeftWith :: Show a => String -> Either String a -> Either String ()
expectLeftWith _ (Right a) = Left $ "expected fail but was actually successful " ++ show a
expectLeftWith n (Left s)
  | n `isInfixOf` s = Right ()
  | otherwise = Left $ "found fail but infix string did not match: actual[" ++ s ++ "] infix[" ++ n ++ "]"

expectLeft :: Show b => Either a b -> IO ()
expectLeft = \case
  Left _ -> pure ()
  Right e -> assertFailure $ "expected Left but found Right " ++ show e

expectRight :: Show a => Either a b -> IO ()
expectRight = \case
  Right _ -> pure ()
  Left e -> assertFailure $ "expected Right but found Left " ++ show e


expectPE :: (Show a, Eq a, HasCallStack) => BoolT a -> IO (BoolT a) -> IO ()
expectPE bp m = do
  x <- m
  print (x,bp)
  bp @?= x

expect3 :: (HasCallStack, Show i, Show r, Eq i, Eq r, Eq j, Show j)
  => Either (Results i j) r
  -> (RResults i j, Maybe r)
  -> IO ()
expect3 lhs (rhs,mr) = do
  (@?=) lhs $ maybe (Left $ toRResults3 rhs) Right mr

expectE :: (HasCallStack, Show a, Eq a)
  => Either (RE.Results a) (Refined op a)
  -> Either (RE.RResults a) (Refined op a)
  -> IO ()
expectE lhs rhs = do
  (@?=) lhs $ left toRResultsE rhs

expectJ :: (HasCallStack, Show a, Eq a)
  => Either [String] a
  -> Either String a
  -> IO ()
expectJ lhs rhs =
  case (lhs,rhs) of
    (Left _e,Right r) -> assertFailure $ "expected left but found right " <> show r
    (Right r,Right r1) -> r @?= r1
    (Right _r,Left e) -> assertFailure $ "expected right but found left " <> e
    (Left ss, Left e)
       | all (`isInfixOf` e) ss -> pure ()
       | otherwise -> assertFailure $ "both left but expected " <> (show ss) <> " in " <> e

toFrom :: (FromJSON a1, ToJSON a2, a1 ~ a2) => a2 -> Either String a1
toFrom = eitherDecode . encode
