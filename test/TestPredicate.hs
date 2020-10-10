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
--module TestPredicate (suite) where
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
  [ expectBT (Val [False,True,True,False]) $ pl @'[Gt 5, Lt 9, Same 4, W 'False] 4
  , expectBT (Val [21,19,20,40,60,2]) $ pl @'[Succ, Pred, Id, Id + Id, Id * 3, Id `Mod` 3] 20
  , expectBT (Val [False,False,False,True]) $ pl @(Map' (Mod Id 3) Fst >> Map (Gt 1)) ([10,12,3,5],"ss")
  , expectBT (Val 5) $ pl @(Snd >> Snd >> Snd >> Snd) (9,(1,(2,(3,5))))
  , expectBT (Val (-1.0)) $ pl @(Negate Id >> Dup >> First Succ >> Swap >> Fst - Snd) 4
  , expectBT (Val False) $ pl @(Msg "someval4" (Gt 4 >> Id)) 4
  , expectBT (Val ()) $ pl @(Snd >> Snd >> Snd >> Snd >> Id) (1,('a',(3,(True,()))))
  , expectBT (Val ()) $ pl @(L22 >> L22) (1,('a',(3,(True,()))))
  , expectBT (Val True) $ pl @L31 (1,2,(True,4))
  , expectBT (Fail "failed3") $ pl @((Fst >> Failt _ "failed3" >> Le (6 -% 1)) || 'False) ([-5],True)
  , expectBT (Val [(-999) % 1,10 % 1,20 % 1,(-999) % 1,30 % 1]) $ pl @(Map (Wrap (MM.First _) Id &&& (Pure Maybe (999 -% 1 ) >> Wrap (MM.First _) Id)) >> Map SapA >> Map ('Just Unwrap)) [Nothing,Just 10,Just 20,Nothing,Just 30]
  , expectBT (Val (True,3.4)) $ pl @(Thd >> Snd >> Fst) (1,'a',('x',((True,3.4),999)))
  , expectBT (Val [13,16,17]) $ pl @(Guard "err" (Len > 2) >> Map Succ) [12,15,16]
  , expectBT (Val 55) $ pl @(Map (Wrap (SG.Sum _) Id) >> MConcat Id >> Unwrap) [1..10]
  , expectBT (Val 9) $ pl @((Wrap _ Id *** Wrap (SG.Sum _) Id) >> SapA >> Unwrap) (4,5)
  , expectBT (Val (SG.Sum 9)) $ pl @((Wrap _ Id *** Wrap _ Id) >> SapA) (4,5)
  , expectBT (Fail "len is bad") $ pl @Ip6Test "FE80::203:Baff:FE77:326FF"
  , expectBT (Fail "not a hex") $ pl @Ip6Test "FE80::203:Baff:GE77:326F"
  , expectBT (Fail "count is bad") $ pl @Ip6Test "FE80::203:Baff:FE77:326F:::::"
  , expectBT (Val [1,2,3,244]) $ pl @(Rescan Ip4RE >> OneP >> Map' (ReadBase Int 10) Snd >| Ip4op) "1.2.3.244"
  , expectBT (Fail "octet 1 out of range 0-255 found 256") $ pl @(Rescan Ip4RE >> OneP >> Map' (ReadBase Int 10) Snd >| Ip4op) "1.256.3.244"
  , expectBT (Fail "Guards:invalid length(5) expected 4") $ pl @(Rescan "(\\d+)\\.?" >> ConcatMap Snd Id >> Map (ReadBase Int 10) >| Ip4op) "1.22.244.66.77"
  , expectBT (Val 256) $ pl @(Rescan "(?i)^\\\\x([0-9a-f]{2})$" >> OneP >> Snd >> OneP >> ReadBase Int 16 >> Succ) "\\xfF"
  , expectBT (Val 256) $ pl @(Rescan "(?i)^\\\\x(.{2})$" >> OneP >> Snd >> OneP >> ReadBase Int 16 >> Succ) "\\xfF"
  , expectBT (Val (("fF",(255,"ff")),False)) $ pl @(Rescan "(?i)^\\\\x([0-9a-f]{2})$" >> OneP >> Snd >> OneP >> (Id &&& (ReadBase Int 16 >> (Id &&& ShowBase 16))) >> (Id &&& ((Id *** Snd) >> Fst == Snd))) "\\xfF"
  , expectBT (Val [31,11,1999]) $ pl @(Rescan DdmmyyyyRE >> OneP >> Map' (ReadBase Int 10) Snd >| Ddmmyyyyop) "31-11-1999"
  , expectBT (Val (TimeOfDay 23 13 59)) $ pl @(Guard "hh:mm:ss regex failed" (Re HmsRE) >> ReadP TimeOfDay Id) "23:13:59"
  , expectBT (Fail "hh:mm:ss regex failed") $ pl @(Guard "hh:mm:ss regex failed" (Re HmsRE) >> ReadP TimeOfDay Id) "23:13:60"
  , expectBT (Val (124,["1","2","2"])) $ pl @(Left' >> (Succ &&& (Pred >> ShowP Id >> Ones))) (Left 123)
  , expectBT (Val (1,("asdf",True))) $ pl @'(1,'("asdf",'True)) ()
  , expectBT (Val (12, False)) $ pl @('These Id (Not Id)) (These 12 True)
    --- have to wrap with W cos different kinds
  -- IxL "d" doesnt work cos is Text not String
  -- use Fromstring
  , expectBT (Val [7,9,9,2,7,3,9,8,7,1,3]) $ pl @(Map' (ReadP Int Id) Ones >> Guard "invalid checkdigit" IsLuhn) "79927398713"
  , expectBT (Fail "invalid checkdigit") $ pl @(Map' (ReadP Int Id) Ones >> Guard "invalid checkdigit" IsLuhn) "79927398714"
  , expectBT (Val [10,14,15,9]) $ pl @(MM1 16 >> MM2 16) "aef9"
  , expectBT (Fail "invalid base 16") $ pl @(MM1 16 >> MM2 16) "aef9g"
  , expectBT (Fail "found empty") $ pl @(MM1 16 >> MM2 16) ""
  , expectBT (Fail "0<=x<n") $ pl @(MM2 16) [10,1,17,1,-3,7]
  , expectBT (Val 70) $ pl @(Luhn' 11) "79927398713"
  , expectBT (Fail "expected 71 mod 10 = 0 but found 1") $ pl @(Luhn' 11) "79927398714"

-- works but way to difficult: use Guard to do all the work
--  >pl @(((Rescan "([[:xdigit:]])" >> Map Snd >> (Id &&& Len)) &&& Len) >> Guard "notallmatched" ((Snd *** Id) >> Fst == Snd)) "134F"
-- have to check the length of the match vs input to see that are the same
  , expectBT (Val [1,3,4,15]) $ pl @(((Rescan "([[:xdigit:]])" >> Map (Snd >> OneP >> ReadBase Int 16)) &&& Id) >> Guard "notallmatched" ((Len *** Len) >> Fst == Snd) >> Fst) "134F"
  , expectBT (Fail "notallmatched") $ pl @(((Rescan "([[:xdigit:]])" >> Map (Snd >> OneP >> ReadBase Int 16)) &&& Id) >> Guard "notallmatched" ((Len *** Len) >> Fst == Snd) >> Fst) "134g"
  , expectBT (Val True) $ pl @(Map' (ReadP _ Id) Ones >> IsLuhn) "12345678903"
  , expectBT (Val False) $ pl @(Map' (ReadP _ Id) Ones >> IsLuhn) "12345678904"
  , expectBT (Fail "incorrect length: found 10 but expected 11 in [1234567890]") $ pl @(Luhn' 11) "1234567890"
  , (@?=) (Just "abc") ((_Fail # "abc") ^? _Fail)
  , (@?=) (Just 'x') ((_Val # 'x') ^? _Val)
  , expectBT (Val (111,'b')) $ pl @('(123,Char1 "c") >> (Id - 12 *** Pred)) ()
  , expectBT (Fail "'Nothing found Just") $ pl @'Nothing (Just 12)

  -- need to fill in the types for both even in ghci
  , expectBT (Val [Just 1,Just 2,Just 3,Just 4]) $ pl @Sequence (Just [1..4])

  , expectBT (Val [13,2,1999]) $ pl @(Rescan DdmmyyyyRE >> OneP >> Map' (ReadP Int Id) Snd) "13-02-1999"
  , expectBT (Val [3,2,1999]) $ pl @(Rescan DdmmyyyyRE >> OneP >> Map' (ReadP Int Id) Snd >| Ddmmyyyyop) "03-02-1999"
  , expectBT (Fail "month 13 is out of range") $ pl @(Rescan DdmmyyyyRE >> OneP >> Map' (ReadP Int Id) Snd >| Ddmmyyyyop) "12-13-1999"
  , expectBT (Val 10) $ pl @(Luhn' 4) "1230"
  , expectBT (Fail "expected 14 mod 10 = 0 but found 4") $ pl @(Luhn' 4) "1234"
  , expectBT (Val [4, 7, 8, 9]) $ pl @'[4,7,8,9] ()
  , expectBT (Val ["aa","b","","ddd"]) $ pl @'["aa","b","","ddd"] ()
  , expectBT (Val "abcdef") $ pl @(Fst <> L21) ("abc",("def",12))
  , expectBT (Val 23) $ pl @(Fst + (Snd >> Last)) (10,[12,13])
  , expectBT (Val 157) $ pl @(Fst * L21 + L22 `Div` 2) (12,(13,3))
  , expectBT (Val (Proxy @'["xy","xy","xy","xy"])) $ pl @(Proxy (RepeatT 4 "xy")) 3
  , expectBT (Val (66788,26232)) $ pl @(Last >> Id * 123 >> Dup >> (Pred *** (ShowP Id >> Rescan "(\\d{2})" >> ConcatMap Snd Id >> Concat >> ReadBase Int 16))) [12,13,543]

  , expectBT (Val (9,"abc")) $ pl @(Id $$ 9 $$ "abc") (,)
  , expectBT (Val ("abc",9)) $ pl @(9 $& "abc" $& Id) (,)
  , expectBT (Val "28") $ pl @(Fst $$ Snd) (show . (7*),4)
  , expectBT (Val (12,"12")) $ pl @(Fst $$ Snd $$ ShowP Snd) ((,),12)
  , expectBT (Val (4,("aa",'x'))) $ pl @'(4,'(Fst,Snd)) ("aa",'x')
  , expectBT (Val (4,"aa",'x')) $ pl @'(4,Fst,Snd) ("aa",'x')
  , expectBT (Val (map ModifiedJulianDay [0,1,2,3,4,5])) $ pl @(Fst ... Snd) (ModifiedJulianDay 0, ModifiedJulianDay 5)
  , expectBT (Val (4,'x')) $ pl @('(,) 4 %% Char1 "x") ()
  , expectBT (Val (4,"abc")) $ pl @('(,) %% 4 %% "abc") ()
  , expectBT (Val ("abc",4)) $ pl @(4 %& "abc" %& '(,)) ()
  , expectBT (Val ("abc",4)) $ pl @(FlipT '(,) 4 "abc") ()
  , expectBT (Val []) $ pl @'[] 4
  , expectBT (Val (-5 % 3)) $ pl @(Snd / Fst) (-3,5)
  , expectBT (Fail "(/) zero denominator") $ pl @(Snd / Fst) (0,5)
  , expectBT (Val (False,7))
     $ pl @(Foldl (If L11
                    (If (Snd > L12)
                       '( 'True, Snd )
                       '( 'False, L12 )
                    ) Fst)
                   '( 'True, Head) Tail) [1,4,7,6,16]
  , expectBT (Val [10,12,13]) $ pl @CatMaybes [Just 10, Just 12, Nothing, Just 13]


  , expectBT (Fail "abcsomeval") $ pl @(Fail (Snd >> Unproxy) (Fst <> "someval")) ("abc",Proxy @Int)


  , expectBT (Val [Left 1,Left 2,Right "fizz",Left 4,Right "buzz",Right "fizz",Left 7,Left 8,Right "fizz",Right "buzz",Left 11,Right "fizz",Left 13,Left 14,Right "fizzbuzz"]) $ pl @(Map Fizzbuzz''') [1..15]
  , expectBT (Val [Left 1,Left 2,Right "fizz",Left 4,Right "buzz",Right "fizz",Left 7,Left 8,Right "fizz",Right "buzz",Left 11,Right "fizz",Left 13,Left 14,Right "fizzbuzz"]) $ pl @(Map Fizzbuzz'') [1..15]
  , expectBT (Val [(1,""),(2,""),(3,"fizz"),(4,""),(5,"buzz"),(6,"fizz"),(7,""),(8,""),(9,"fizz"),(10,"buzz"),(11,""),(12,"fizz"),(13,""),(14,""),(15,"fizzbuzz")]) $ pl @Fizzbuzzs [1..15]
  , expectBT (Val [Left 1,Left 2,Right "fizz",Left 4,Right "buzz",Right "fizz",Left 7,Left 8,Right "fizz",Right "buzz",Left 11,Right "fizz",Left 13,Left 14,Right "fizzbuzz"]) $ pl @Fizzbuzzs2 [1..15]
  , expectBT (Val [Left 1,Left 2,Right "fizz",Left 4,Right "buzz",Right "fizz",Left 7,Left 8,Right "fizz",Right "buzz",Left 11,Right "fizz",Left 13,Left 14,Right "fizzbuzz"]) $ pl @Fizzbuzzs3 [1..15]

  , expectBT (Val "abc") $ pl @(L3 L12) (('x',(13,False,"abc")),True,'y')
  , expectBT (Val 9.3) $ pl @(L1 L32) ('x',True,(13,(9.3,False),"def"))
  , expectBT (Val (4,"helo|oleh")) $ pl @'(Len, Id <> "|" <> Reverse) "helo"
  , expectBT (Val (123,"helo")) $ pl @'(Snd, Fst) ("helo",123)
  , expectBT (Val (4,"helo","oleh")) $ pl @'(Len, Id, Reverse) "helo"
  , expectBT (Val [1,2,3,1000,998]) $ pl @'[W 1, W 2, W 3, Succ, Pred] 999
  , expectBT (Val [3996,998]) $ pl @'[Id * 4, Pred] 999
  , expectBT (Val [2,3,4,5,6]) $ pz @(FlipT Map' Id Succ) [1..5]
  , expectBT (Val (2,True)) $ pz @( FlipT '(,) 'True 2) ()
  , expectBT (Val (1,"ab",2)) $ pz @( FlipT ('(,,) 1) 2 Id) "ab"
  , expectBT (Val 13) $ pz @(12 & Lift Succ) ()
  , expectBT (Val 10) $ pz @('[1,2,3,4] & FoldMap (SG.Sum _)) ()


  -- test semigroup interaction
  , expectEQR (These (Val 6) (Fail "xyz | hello")) $ fmap This (pz @Predicate.Sum [1,2,3]) <> fmap That (pz @(FailS "xyz") 5) <> fmap That (pz @(FailS "hello") 1)
  , expectEQR (These (Val 6) (Val ("5",6))) $ fmap This (pz @Predicate.Sum [1,2,3]) <> fmap That (pz @(ShowP Id &&& Succ) 5)
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
type Fizzbuzz'' = Case (MkLeft String Fst) '[Id `Mod` 15 == 0, Id `Mod` 3 == 0, Id `Mod` 5 == 0] '[MkRight Int "fizzbuzz", MkRight Int "fizz", MkRight Int "buzz"] Id
-- makes use of type family MapT which does the apply on ADTs: so type synonyms dont work
type Fizzbuzz''' = Case (MkLeft String Fst) '[Id `Mod` 15 == 0, Id `Mod` 3 == 0, Id `Mod` 5 == 0] (MapT (MkRight' (Hole Int)) '["fizzbuzz", "fizz", "buzz"]) Id

type Fizzbuzzs = Map Fizzbuzz
type Fizzbuzzs2 = Map (Fizzbuzz >> If (Null' Snd) (MkLeft String Fst) (MkRight Int Snd))
-- best one cos leverages type info to determine Either a b
type Fizzbuzzs3 = Map (Fizzbuzz >> If (Snd == "") (MkLeft' Snd Fst) (MkRight' Fst Snd))

type Ip6Test = Resplit ":"
        >> Guard "count is bad" (Between 0 8 Len)
        >> Guard "not a hex" (All (All (Elem Id "abcdefABCDEF0123456789")))
        >> Guard "len is bad" (All (Len >> Le 4))

-- base n number of length x and then convert to a list of length x of (0 to (n-1))
-- checks that each digit is between 0 and n-1
type MM1 (n :: Nat) = Map' (ReadBase Int n) Ones
type MM2 (n :: Nat) = ExitWhen "found empty" IsEmpty >> Guard "0<=x<n" (All (Ge 0 && Lt n))
