{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wno-type-defaults #-}
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
{-# LANGUAGE NoStarIsType #-}
module TestPredicate where
import TastyExtras
import Test.Tasty
import Test.Tasty.HUnit
import Predicate
import Predicate.Examples.Common
import Data.Ratio

import Data.Typeable
import Control.Lens
import Data.Time
import Text.Show.Functions ()
import qualified Data.Monoid as MM
import qualified Data.Semigroup as SG
import Data.These
import GHC.TypeLits (Nat)

suite :: TestTree
suite =
  let s = "TestPredicate"
  in testGroup s (orderTests s allTests)

allTests :: [IO ()]
allTests =
  [ expectPE (PresentT [False,True,True,False]) $ pl @'[Gt 5, Lt 9, Same 4, W 'False] 4
  , expectPE (PresentT [21,19,20,40,60,2]) $ pl @'[Succ Id, Pred Id, Id, Id + Id, Id * 3, Id `Mod` 3] 20
  , expectPE (PresentT [False,False,False,True]) $ pl @(Map (Mod Id 3) (Fst Id) >> Map (Gt 1) Id) ([10,12,3,5],"ss")
  , expectPE (PresentT 5) $ pl @(Snd Id >> Snd Id >> Snd Id >> Snd Id >> Id) (9,(1,(2,(3,5))))
  , expectPE (PresentT (-1.0)) $ pl @(Negate Id >> Dup >> First (Succ Id) >> Swap >> Fst Id - Snd Id) 4
  , expectPE (PresentT False) $ pl @(Msg "someval4" (Gt 4 >> Id)) 4
  , expectPE (PresentT ()) $ pl @(Snd Id >> Snd Id >> Snd Id >> Snd Id >> Id) (1,('a',(3,(True,()))))
  , expectPE (PresentT True) $ pl @(Thd Id >> Fst Id) (1,2,(True,4))
  , expectPE (PresentT True) $ pl @(Fst (Thd Id)) (1,2,(True,4))
  , expectPE (FailT "failed3") $ pl @((Fst Id >> Failt _ "failed3" >> Le (6 -% 1)) || 'False) ([-5],True)
  , expectPE (PresentT [(-999) % 1,10 % 1,20 % 1,(-999) % 1,30 % 1]) $ pl @(Map (Wrap (MM.First _) Id &&& (Pure Maybe (999 -% 1 ) >> Wrap (MM.First _) Id)) Id >> Map SapA Id >> Map ('Just (Unwrap Id)) Id) [Nothing,Just 10,Just 20,Nothing,Just 30]

  , expectPE (PresentT (True,3.4)) $ pl @(Thd Id >> Snd Id >> Fst Id) (1,'a',('x',((True,3.4),999)))
  , expectPE (PresentT (True,3.4)) $ pl @(Fst (Snd (Thd Id))) (1,'a',('x',((True,3.4),999)))
  , expectPE (PresentT [13,16,17]) $ pl @(Guard "err" (Len > 2) >> Map (Succ Id) Id) [12,15,16]
  , expectPE (PresentT 55) $ pl @(Map (Wrap (SG.Sum _) Id) Id >> MConcat Id >> Unwrap Id) [1..10]
  , expectPE (PresentT 9) $ pl @((Wrap _ Id *** Wrap (SG.Sum _) Id) >> SapA >> Unwrap Id) (4,5)
  , expectPE (PresentT (SG.Sum 9)) $ pl @((Wrap _ Id *** Wrap _ Id) >> SapA) (4,5)
  , expectPE (FailT "len is bad") $ pl @Ip6Test "FE80::203:Baff:FE77:326FF"
  , expectPE (FailT "not a hex") $ pl @Ip6Test "FE80::203:Baff:GE77:326F"
  , expectPE (FailT "count is bad") $ pl @Ip6Test "FE80::203:Baff:FE77:326F:::::"
  , expectPE (PresentT [1,2,3,244]) $ pl @(Rescan Ip4RE Id >> OneP Id >> Map (ReadBase Int 10 Id) (Snd Id) >> Ip4op) "1.2.3.244"
  , expectPE (FailT "octet 1 out of range 0-255 found 256") $ pl @(Rescan Ip4RE Id >> OneP Id >> Map (ReadBase Int 10 Id) (Snd Id) >> Ip4op) "1.256.3.244"
  , expectPE (FailT "Guards:invalid length(5) expected 4") $ pl @(Rescan "(\\d+)\\.?" Id >> ConcatMap (Snd Id) Id >> Map (ReadBase Int 10 Id) Id >> Ip4op) "1.22.244.66.77"
  , expectPE (PresentT 256) $ pl @(Rescan "(?i)^\\\\x([0-9a-f]{2})$" Id >> OneP Id >> Snd Id >> OneP Id >> ReadBase Int 16 Id >> Succ Id) "\\xfF"
  , expectPE (PresentT 256) $ pl @(Rescan "(?i)^\\\\x(.{2})$" Id >> OneP Id >> Snd Id >> OneP Id >> ReadBase Int 16 Id >> Succ Id) "\\xfF"
  , expectPE (PresentT (("fF",(255,"ff")),False)) $ pl @(Rescan "(?i)^\\\\x([0-9a-f]{2})$" Id >> OneP Id >> Snd Id >> OneP Id >> (Id &&& (ReadBase Int 16 Id >> (Id &&& ShowBase 16 Id))) >> (Id &&& ((Id *** Snd Id) >> Fst Id == Snd Id))) "\\xfF"
  , expectPE (PresentT [31,11,1999]) $ pl @(Rescan DdmmyyyyRE Id >> OneP Id >> Map (ReadBase Int 10 Id) (Snd Id) >> Ddmmyyyyop) "31-11-1999"
  , expectPE (PresentT (TimeOfDay 23 13 59)) $ pl @(Guard "hh:mm:ss regex failed" (Re HmsRE Id) >> ReadP TimeOfDay Id) "23:13:59"
  , expectPE (FailT "hh:mm:ss regex failed") $ pl @(Guard "hh:mm:ss regex failed" (Re HmsRE Id) >> ReadP TimeOfDay Id) "23:13:60"
  , expectPE (PresentT (124,["1","2","2"])) $ pl @(Left' >> (Succ Id &&& (Pred Id >> ShowP Id >> Ones Id))) (Left 123)
  , expectPE (PresentT (1,("asdf",True))) $ pl @'(1,'("asdf",'True)) ()
  , expectPE (PresentT (12, False)) $ pl @('These Id (Not Id)) (These 12 True)
    --- have to wrap with W cos different kinds
  , expectPE TrueT $ pl @('PresentT I >> Not 'FalseT) False
  -- IxL "d" doesnt work cos is Text not String
  -- use Fromstring
  , expectPE (PresentT [7,9,9,2,7,3,9,8,7,1,3]) $ pl @(Map (ReadP Int Id) (Ones Id) >> Guard "checkdigit fail" (IsLuhn Id)) "79927398713"
  , expectPE (FailT "checkdigit fail") $ pl @(Map (ReadP Int Id) (Ones Id) >> Guard "checkdigit fail" (IsLuhn Id)) "79927398714"
  , expectPE (PresentT [10,14,15,9]) $ pl @(MM1 16 >> MM2 16) "aef9"
  , expectPE (FailT "invalid base 16") $ pl @(MM1 16 >> MM2 16) "aef9g"
  , expectPE (FailT "found empty") $ pl @(MM1 16 >> MM2 16) ""
  , expectPE (FailT "0<=x<n") $ pl @(MM2 16) [10,1,17,1,-3,7]
  , expectPE (PresentT 70) $ pl @(Luhn' 11) "79927398713"
  , expectPE (FailT "expected 71 mod 10 = 0 but found 1") $ pl @(Luhn' 11) "79927398714"

-- works but way to difficult: use Guard to do all the work
--  >pl @(((Rescan "([[:xdigit:]])" >> Map (Snd Id) >> (Id &&& Len)) &&& Len) >> Guard "notallmatched" ((Snd Id *** Id) >> Fst Id == Snd Id)) "134F"
-- have to check the length of the match vs input to see that are the same
  , expectPE (PresentT [1,3,4,15]) $ pl @(((Rescan "([[:xdigit:]])" Id >> Map (Snd Id >> OneP Id >> ReadBase Int 16 Id) Id) &&& Id) >> Guard "notallmatched" ((Len *** Len) >> Fst Id == Snd Id) >> Fst Id) "134F"
  , expectPE (FailT "notallmatched") $ pl @(((Rescan "([[:xdigit:]])" Id >> Map (Snd Id >> OneP Id >> ReadBase Int 16 Id) Id) &&& Id) >> Guard "notallmatched" ((Len *** Len) >> Fst Id == Snd Id) >> Fst Id) "134g"
  , expectPE TrueT $ pl @(Map (ReadP _ Id) (Ones Id) >> IsLuhn Id) "12345678903"
  , expectPE FalseT $ pl @(Map (ReadP _ Id) (Ones Id) >> IsLuhn Id) "12345678904"
  , expectPE (FailT "incorrect length: found 10 but expected 11 in [1234567890]") $ pl @(Luhn' 11) "1234567890"
  , (@?=) (Just "abc") ((_FailT # "abc") ^? _FailT)
  , (@?=) (Just ()) ((_TrueT # ()) ^? _TrueT)
  , (@?=) (Just ()) ((_FalseT # ()) ^? _FalseT)
  , (@?=) (Just 'x') ((_PresentT # 'x') ^? _PresentT)
  , expectPE (PresentT (111,'b')) $ pl @('(123,Char1 "c") >> (Id - 12 *** Pred Id)) ()
  , expectPE (FailT "'Nothing found Just") $ pl @'Nothing (Just 12)

  -- need to fill in the types for both even in ghci
  , expectPE (PresentT [Just 1,Just 2,Just 3,Just 4]) $ pl @Sequence (Just [1..4])

  , expectPE (PresentT [13,2,1999]) $ pl @(Rescan DdmmyyyyRE Id >> OneP Id >> Map (ReadP Int Id) (Snd Id)) "13-02-1999"
  , expectPE (PresentT [3,2,1999]) $ pl @(Rescan DdmmyyyyRE Id >> OneP Id >> Map (ReadP Int Id) (Snd Id) >> Ddmmyyyyop) "03-02-1999"
  , expectPE (FailT "month 13 is out of range") $ pl @(Rescan DdmmyyyyRE Id >> OneP Id >> Map (ReadP Int Id) (Snd Id) >> Ddmmyyyyop) "12-13-1999"
  , expectPE (PresentT 10) $ pl @(Luhn' 4) "1230"
  , expectPE (FailT "expected 14 mod 10 = 0 but found 4") $ pl @(Luhn' 4) "1234"
  , expectPE (PresentT [4, 7, 8, 9]) $ pl @'[4,7,8,9] ()
  , expectPE (PresentT ["aa","b","","ddd"]) $ pl @'["aa","b","","ddd"] ()
  , expectPE (PresentT "abcdef") $ pl @(Fst Id <> (Snd Id >> Fst Id)) ("abc",("def",12))
  , expectPE (PresentT 23) $ pl @(Fst Id + Last (Snd Id)) (10,[12,13])
  , expectPE (PresentT 157) $ pl @(Fst Id * (Snd Id >> Fst Id) + (Snd Id >> Snd Id) `Div` 2) (12,(13,3))
  , expectPE (PresentT (Proxy @'["xy","xy","xy","xy"])) $ pl @(Proxy (RepeatT 4 "xy")) 3
  , expectPE (PresentT (66788,26232)) $ pl @(Last Id >> Id * 123 >> Dup >> (Pred Id *** (ShowP Id >> Rescan "(\\d{2})" Id >> Concat (ConcatMap (Snd Id) Id) >> ReadBase Int 16 Id))) [12,13,543::Int]


  , expectPE (PresentT ('x',('x',"someval"))) $ pl @Duplicate ('x',"someval")
  , expectPE (PresentT "someval") $ pl @Extract ('x',"someval")
  , expectPE (PresentT (9,"abc")) $ pl @(I $$ 9 $$ "abc") (,)
  , expectPE (PresentT ("abc",9)) $ pl @(9 $& "abc" $& I) (,)
  , expectPE (PresentT "28") $ pl @(Fst Id $$ Snd Id) (show . (7*),4)
  , expectPE (PresentT (12,"12")) $ pl @(Fst Id $$ Snd Id $$ ShowP (Snd Id)) ((,),12)
  , expectPE (PresentT (4,("aa",'x'))) $ pl @'(4,'(Fst Id,Snd Id)) ("aa",'x')
  , expectPE (PresentT (4,"aa",'x')) $ pl @'(4,Fst Id,Snd Id) ("aa",'x')
  , expectPE (PresentT (map ModifiedJulianDay [0,1,2,3,4,5])) $ pl @(EnumFromTo (Fst Id) (Snd Id)) (ModifiedJulianDay 0, ModifiedJulianDay 5)
  , expectPE (PresentT (4,'x')) $ pl @('(,) 4 %% Char1 "x") ()
  , expectPE (PresentT (4,"abc")) $ pl @('(,) %% 4 %% "abc") ()
  , expectPE (PresentT ("abc",4)) $ pl @(4 %& "abc" %& '(,)) ()
  , expectPE (PresentT ("abc",4)) $ pl @(FlipT '(,) 4 "abc") ()
  , expectPE (PresentT []) $ pl @'[] 4
  , expectPE (PresentT (-5 % 3)) $ pl @(Snd Id / Fst Id) (-3,5)
  , expectPE (FailT "(/) zero denominator") $ pl @(Snd Id / Fst Id) (0,5)
  , expectPE (PresentT (False,7))
     $ pl @(Foldl (If (Fst (Fst Id))
                    (If (Snd Id > Snd (Fst Id))
                       '( 'True, Snd Id )
                       '( 'False, Snd (Fst Id) )
                    ) (Fst Id))
                   '( 'True, Head Id) (Tail Id)) [1,4,7,6,16]
  , expectPE (PresentT [10,12,13]) $ pl @(CatMaybes Id) [Just 10, Just 12, Nothing, Just 13]


  , expectPE (FailT "abcsomeval") $ pl @(Fail (Snd Id >> Unproxy) (Fst Id <> "someval")) ("abc",Proxy @Int)


  , expectPE (PresentT [Left 1,Left 2,Right "fizz",Left 4,Right "buzz",Right "fizz",Left 7,Left 8,Right "fizz",Right "buzz",Left 11,Right "fizz",Left 13,Left 14,Right "fizzbuzz"]) $ pl @(Map Fizzbuzz''' Id) [1..15]

  , expectPE (PresentT "abc") $ pl @(Thd (Snd (Fst Id))) (('x',(13,False,"abc")),True,'y')
  , expectPE (PresentT 9.3) $ pl @(Fst (Snd (Thd Id))) ('x',True,(13,(9.3,False),"def"))
  , expectPE (PresentT (4,"helo|oleh")) $ pl @'(Len, Id <> "|" <> Reverse) "helo"
  , expectPE (PresentT (123,"helo")) $ pl @'(Snd Id, Fst Id) ("helo",123)
  , expectPE (PresentT (4,"helo","oleh")) $ pl @'(Len, Id, Reverse) "helo"
  , expectPE (PresentT [1,2,3,1000,998]) $ pl @'[W 1, W 2, W 3, Succ Id, Pred Id] 999
  , expectPE (PresentT [3996,998]) $ pl @'[Id * 4, Pred Id] 999


  -- test semigroup interaction
  , expectEQR (These (PresentT 6) (FailT "xyzhello")) $ fmap This (pz @Predicate.Sum [1,2,3]) <> fmap That (pz @(FailS "xyz") 5) <> fmap That (pz @(FailS "hello") 1)
  , expectEQR (These (PresentT 6) (PresentT ("5",6))) $ fmap This (pz @Predicate.Sum [1,2,3]) <> fmap That (pz @(ShowP Id &&& Succ Id) 5)
-- test options
  , oRecursion testopts1 @?= 11
  , oDebug testopts1 @?= DVerbose
  , oNoColor testopts1 @?= True
  , oMsg testopts1 @?= ["abc", "def"]
  , oWidth testopts1 @?= 99
  , oDisp testopts1 @?= Unicode
  , fst (oColor testopts1) @?= "nocolor"
  , fst (oColor testopts2) @?= "testcolor"
  , oDisp testopts2 @?= Ansi
  , fst (oColor testopts3) @?= "testcolor1"
  , oDisp testopts3 @?= Unicode
  , oMsg testopts3 @?= ["def"]
  ]

testopts1, testopts2, testopts3 :: POpts
testopts1 = getOpt @('ORecursion 11 ':# 'OVerbose ':# 'OColorOff ':# 'OWidth 123 ':# 'OMsg "abc" ':# 'OColor "testcolor" 'Red 'Green 'Default 'White 'Default 'White 'Default 'White ':# 'OMsg "def" ':# 'OEmpty ':# 'ORecursion 11 ':# 'OUnicode ':# 'OWidth 99)
testopts2 = getOpt @('OColorOn ':# 'OColor "testcolor" 'Red 'Green 'Default 'White 'Default 'White 'Default 'White)
testopts3 = getOpt @('OColor "testcolor1" 'Red 'Green 'Default 'White 'Default 'White 'Default 'White ':# 'OMsg "def" ':# 'OUnicode)


type Fizzbuzz = '(Id,  If (Id `Mod` 3==0) "fizz" "" <> If (Id `Mod` 5==0) "buzz" "")
type Fizzbuzz'' = Case (MkLeft String (Fst Id)) '[Id `Mod` 15 == 0, Id `Mod` 3 == 0, Id `Mod` 5 == 0] '[ MkRight Int "fizzbuzz", MkRight Int "fizz", MkRight Int "buzz"] Id
-- makes use of type family MapT which does the apply on ADTs: so type synonyms dont work
type Fizzbuzz''' = Case (MkLeft String (Fst Id)) '[Id `Mod` 15 == 0, Id `Mod` 3 == 0, Id `Mod` 5 == 0] (MapT (MkRight' (Hole Int)) '[ "fizzbuzz", "fizz", "buzz"]) Id

type Fizzbuzzs = Map Fizzbuzz Id
type Fizzbuzzs2 = Map (Fizzbuzz >> If (Null' (Snd Id)) (MkLeft String (Fst Id)) (MkRight Int (Snd Id))) Id
-- best one cos leverages type info to determine Either a b
type Fizzbuzzs3 = Map (Fizzbuzz >> If (Snd Id == "") (MkLeft' (Snd Id) (Fst Id)) (MkRight' (Fst Id) (Snd Id))) Id

type Ip6Test = Resplit ":" Id
        >> Guard "count is bad" (Between 0 8 Len)
        >> Guard "not a hex" (All (All (Elem Id "abcdefABCDEF0123456789") Id) Id)
        >> Guard "len is bad" (All (Len >> Le 4) Id)

-- base n number of length x and then convert to a list of length x of (0 to (n-1))
-- checks that each digit is between 0 and n-1
type MM1 (n :: Nat) = Map (ReadBase Int n Id) (Ones Id)
type MM2 (n :: Nat) = ExitWhen "found empty" IsEmpty >> Guard "0<=x<n" (All (Ge 0 && Lt n) Id)
