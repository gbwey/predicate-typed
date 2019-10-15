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
{-# LANGUAGE NoOverloadedLists #-} -- overloaded lists messes stuff up
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module TestPredicate where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Predicate
import Refined
import Refined3
import Refined3Helper
import qualified RefinedE as RE
import RefinedE (evalE,evalEP,proxyEToV,mkProxy3E)
import UtilP
import UtilP_TH
import Data.Ratio
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Typeable
import Control.Lens
import qualified Data.Monoid as MM
import qualified Data.Semigroup as SG
import Data.These
import Data.Time
import Text.Show.Functions ()
import Data.Functor.Compose
import TestRefined hiding (suite)

suite :: IO ()
suite = defaultMain $ testGroup "TestPredicate" (orderTests allTests <> allProps)

allProps :: [TestTree]
allProps =
  [testProperty "base16" $ forAll (arbRefined3 (mkProxy3P @'(ReadBase Int 16, 'True, ShowBase 16, String)) ol) (\r -> evalQuick @(ReadBase Int 16) (out3 r) == Right (in3 r))
  ]

orderTests :: [Assertion] -> [TestTree]
orderTests = zipWith (\i t -> testCase (show i) t) [1::Int ..]

allTests :: [IO ()]
allTests =
   [expectPE (PresentT LT) $ pl @("aa" === Id) "aaaa"
  , expectPE FalseT $ pl @("aa" === Id >> FromEnum >> Same 1) "aaaa"
  , expectPE (PresentT (Right 1)) $ pl @(HeadDef 'False +++ Id) (Right @[Bool] 1) -- need @[Bool] cos we said 'False!
  , expectPE (PresentT (Left True)) $ pl @(HeadDef 'False +++ Id) (Left @_ @Int [True,False]) -- need @[Bool] cos we said 'False!
  , expectPE (PresentT (Right True)) $ pl @(Not +++ Id) (Right True)
  , expectPE (PresentT (4,4)) $ pl @Dup 4
  , expectPE (PresentT (4,4)) $ pl @(Dup >> Id) 4
  , expectPE (PresentT (Right 12)) $ pl @(Not +++ Id) (Right 12)
  , expectPE (PresentT (Right 1)) $ pl @(HeadDef () +++ Id) (Right @[()] 1) -- breaks otherwise: Id says () -> () so has to be a list of [()]
  , expectPE (PresentT (Right 1)) $ pl @(HeadDef () +++ Id) (Right @[()] 1) -- this breaks! cos Left doesnt have a type
  , expectPE FalseT $ pl @(Fst >> Len >> Le 6 >> Not) ([2..7],True)
  , expectPE TrueT $ pl @(Fst >> Len >> Le 6) ([2..7],True)
  , expectPE TrueT $ pl @(Fst >> Len >> Le 6) ([2..7],True)
  , expectPE TrueT $ pl @((Fst >> Len) >> Le 6) ([2..7],True)
  , expectPE TrueT $ pl @(Fst >> (Len >> Le 6)) ([2..7],True)
  , expectPE TrueT $ pl @(Fst >> HeadDef 1 >> Le 6) ([],True)
  , expectPE FalseT $ pl @(Fst >> HeadDef 12 >> Le 6) ([],True)
  , expectPE TrueT $ pl @(Fst >> HeadDef 1 >> Le 6) ([],True)
  , expectPE (FailT "Head(empty)") $ pl @(Fst >> Head' >> Le 6) ([] @Int, True)
  , expectPE FalseT $ pl @(Fst >> HeadDef 10 >> Le 6) ([],True)
  , expectPE (FailT "zz") $ pl @(Fst >> Head'' "zz" >> Le 6) ([],True)
  , expectPE (FailT "failed1") $ pl @((Fst >> Head'' "failed1" >> Le 6) `OR` 'False) ([],True)
  , expectPE TrueT $ pl @((Fst >> Head'' "failed2" >> Le (Neg 6)) `OR` 'False) ([-9],True)
  , expectPE (FailT "failed3") $ pl @((Fst >> Failt _ "failed3" >> Le (Neg 6)) `OR` 'False) ([-5],True)
  , expectPE TrueT $ pl @(MaybeIn 'True Id) (Nothing @Bool) -- need @() else breaks
  , expectPE (PresentT 10) $ pl @(MaybeIn (Failt _ "failed4") Id) (Just 10)
  , expectPE (PresentT 10) $ pl @Just' (Just 10)
  , expectPE FalseT $ pl @(MaybeIn 'False Id) (Nothing @Bool) -- breaks otherwise
  , expectPE FalseT $ pl @(Id > "xx") "abc"
  , expectPE TrueT $ pl @(Id > "aa") "abc"
  , expectPE TrueT $ pl @(Gt 4) 5
  , expectPE TrueT $ pl @(Fst >> (Map (Gt 3) >> Ors)) ([10,12,3,5],"ss")
  , expectPE FalseT $ pl @(Fst >> (Map (Gt 3) >> Ands)) ([10,12,3,5],"ss")
  , expectPE (PresentT [False,False,False,True]) $ pl @(Fst >> Map (Mod Id 3) >> Map (Gt 1)) ([10,12,3,5],"ss")
  , expectPE (PresentT (12,5)) $ pl @(Fst >> Dup >> Star (Ix 1 (Failp "failed5")) (Ix 3 (Failp "failed5")) >> Id) ([10,12,3,5],"ss")
  , expectPE FalseT $ pl @(Fst >> Dup >> Star (Ix 1 (Failp "failed5")) (Ix 3 (Failp "failed5")) >> Fst < Snd) ([10,12,3,5],"ss")
  , expectPE TrueT $ pl @(Fst >> Dup >> Star (Ix 1 (Failp "failed5")) (Ix 3 (Failp "failed5")) >> Fst > Snd) ([10,12,3,5],"ss")
  , expectPE TrueT $ pl @(Fst > Snd) (True,False)
  , expectPE FalseT $ pl @(Fst == Snd) (True,False)
  , expectPE TrueT $ pl @(Star Not Id >> Fst == Snd) (True,False)
  , expectPE FalseT $ pl @(Snd >> Len &&& Ix 3 (Failp "someval1") >> Fst == Snd) ('x',[1..5])
  , expectPE FalseT $ pl @(Snd >> Len &&& Ix 3 (Failp "someval2") >> Fst < Snd) ('x',[1..5])
  , expectPE TrueT $ pl @(Snd >> Len &&& Ix 3 (Failp "someval3") >> Fst > Snd) ('x',[1..5])
  , expectPE FalseT $ pl @(Snd >> SplitAt 2 Id >> Star Len Len >> Fst > Snd) ('x',[1..5])
  , expectPE FalseT $ pl @(Map (Same 2) >> Ors) [1,4,5]
  , expectPE TrueT $ pl @(Map (Same 2) >> Ors) [1,4,5,2,1]
  , expectPE TrueT $ pl @(Elem Id '[2,3,4]) 2
  , expectPE FalseT $ pl @(Elem Id '[2,3,4]) 6
  , expectPE TrueT $ pl @(Elem Id '[PosR 13 2]) 6.5
  , expectPE TrueT $ pl @(Elem Id '[PosR 13 2, Pos 12]) 6.5
  , expectPE FalseT $ pl @(Elem Id '[PosR 13 2, Pos 12]) 6
  , expectPE (FailT "lhs") $ pl @(Map Len >> (Ix 3 (Failp "lhs")) &&& (Ix 0 5) >> Fst == Snd) [[1..4],[4..5]]
  , expectPE FalseT $ pl @(Map Len >> (Ix 0 (Failp "lhs")) &&& (Ix 1 5) >> Fst == Snd) [[1..4],[4..5]]
  , expectPE (FailT "rhs") $ pl @(Map Len >> (Ix 1 (Failp "lhs")) &&& (Ix 3 (Failp "rhs")) >> Fst == Snd) [[1..4],[4..5]]
  , expectPE (FailT "lhs") $ pl @(Map Len >> (Ix 10 (Failp "lhs")) &&& (Ix 1 (Failp "rhs")) >> Fst == Snd) [[1..4],[4..5]]
  , expectPE (FailT "rhs") $ pl @(Map Len >> (Ix 0 (Failp "lhs")) &&& (Ix 10 (Failp "rhs")) >> Fst == Snd) [[1..4],[4..5]]
  , expectPE FalseT $ pl @(Map Len >> (Ix 10 3) &&& (Ix 1 (Failp "rhs")) >> Fst == Snd) [[1..4],[4..5]]
  , expectPE FalseT $ pl @(Map Len >> (Ix 10 3) &&& (Ix 1 (Failp "rhs")) >> Fst == Snd) [[1..4],[4..5]]
  , expectPE FalseT $ pl @(Map Len >> (Ix 3 3) &&& (Ix 1 4) >> Fst == Snd) [[1..4],[4..5]]
  , expectPE FalseT $ pl @(Map Len >> (Ix 10 3) &&& (Ix 1 4) >> Fst == Snd) [[1..4],[4..5]]
  , expectPE FalseT $ pl @(Map Len >> (Ix 10 5) &&& (Ix 1 4) >> Fst == Snd) [[1..4],[4..5]]
  , expectPE TrueT $ pl @(Map Len >> (Ix 10 2) &&& (Ix 1 4) >> Fst == Snd) [[1..4],[4..5]]
  , expectPE (PresentT ([1],[2,3,4,5])) $ pl @(Partition (Lt 2) Id >> Id) [1,2,3,4,5]
  , expectPE (PresentT [1,2,3]) $ pl @(MaybeIn MemptyP Id) (Just [1,2,3])
  , expectPE (PresentT []) $ pl @(MaybeIn MemptyP Id) (Nothing @[Int])
  , expectPE (FailT "'Just found Nothing") $ pl @('Just (FailS "someval")) (Nothing @()) -- breaks otherwise
  , expectPE (PresentT (4,4)) $ pl @Dup 4
  , expectPE (PresentT (4,4)) $ pl @(Dup >> Id) 4
  , expectPE (PresentT (4,4)) $ pl @Dup 4
  , expectPE (PresentT 3) $ pl @Last' [1,2,3]
  , expectPE (PresentT 123) $ pl @(Just' >> Id) (Just 123)
  , expectPE (FailT "Asdf") $ pl @(Head'' "Asdf") ([] @()) -- breaks otherwise
  , expectPE (FailT "Head(empty)") $ pl @Head' ([] @Int)
  , expectPE (FailT "Head(empty)") $ pl @Head' ([] @Double)
  , expectPE (FailT "Succ bounded failed") $ pl @SuccB' GT
  , expectPE (PresentT LT) $ pl @(SuccB 'LT) GT
  , expectPE (PresentT EQ) $ pl @(SuccB 'GT) LT
  , expectPE (PresentT EQ) $ pl @SuccB' LT
  , expectPE (FailT "Pred bounded failed") $ pl @PredB' LT
  , expectPE (PresentT GT) $ pl @(PredB 'GT) LT
  , expectPE (PresentT EQ) $ pl @(PredB 'LT) GT
  , expectPE (PresentT EQ) $ pl @PredB' GT
  , expectPE (FailT "ToEnum bounded failed") $ pl @(ToEnumBF Ordering) 44
  , expectPE (PresentT LT) $ pl @(ToEnumB Ordering 'LT) 123
  , expectPE (PresentT EQ) $ pl @(ToEnumB Ordering 'GT) 1
  , expectPE (PresentT EQ) $ pl @(ToEnumBF Ordering) 1
  , expectPE (PresentT 11) $ pl @Succ 10
  , expectPE (FailT "Succ IO e=Prelude.Enum.Bool.succ: bad argument") $ pl @Succ True -- captures the exception
  , expectPE (PresentT ([4,5,6,7,8,9,10],[1,2,3])) $ pl @(Partition (Gt 3) Id) [1..10]
  , expectPE (PresentT ([2,4,6],[1,3,5])) $ pl @(Partition Even Id) [1..6]
  , expectPE TrueT $ pl @(Partition Even Id >> Star Null (Len >> Gt 4) >> Fst == Snd) [1..6]
  , expectPE (PresentT 5) $ pl @(Snd >> Snd >> Snd >> Snd >> Id) (9,(1,(2,(3,5))))
  , expectPE (FailT "Exitwhen") $ pl @((Head'' "failedn") &&& (Len >> Same 1 >> Exitwhen' Id) >> Fst) [3]
  , expectPE (PresentT 3) $ pl @((Head'' "failedn") &&& (Len >> Same 1 >> Not >> Exitwhen' Id) >> Fst) [3]
  , expectPE (PresentT 3) $ pl @((Head'' "failedn") &&& (Len >> Same 1 >> Exitwhen' Not) >> Fst) [3]
  , expectPE (FailT "Exitwhen") $ pl @(Exitwhen' (Len >> Ne 1) >> Head'' "failedn") [3,1]
  , expectPE (PresentT 3) $ pl @(Exitwhen' (Len >> Ne 1) >> Head'' "failedn") [3]
  , expectPE TrueT $ pl @(Exitwhen' (Len >> Ne 1) >> Head'' "failedn" >> Gt (Neg 20)) [3]
  , expectPE FalseT $ pl @(Exitwhen' (Len >> Ne 1) >> Head'' "failedn" >> Gt (Neg 20)) [-23]
  , expectPE (PresentT (-1.0)) $ pl @(Negate >> Dup >> First Succ >> Swap >> Fst - Snd) 4
  , expectPE (PresentT (Right True)) $ pl @(Not +++ Id) (Right True)
  , expectPE (PresentT (Right 12)) $ pl @(Not +++ Id) (Right @Bool 12)
  , expectPE FalseT $ pl @("aa" === Id >> FromEnum >> Same 1) "aaaa"
  , expectPE (PresentT Cgt) $ pl @("aa" === Id >> FromEnum >> ToEnum OrderingP) "aaaa"
  , expectPE (PresentT False) $ pl @(Msg "someval4" (Gt 4 >> Id)) 4
  , expectPE (PresentT ()) $ pl @(Snd >> Snd >> Snd >> Snd >> Id) (1,('a',(3,(True,()))))
  , expectPE (PresentT ()) $ pl @(Snd >> Snd >> Snd >> Snd >> Id) (1,('a',(3,(True,()))))
  , expectPE (PresentT ()) $ pl @(Snd >> Snd >> Snd >> Snd >> Id) (1,('a',(3,(True,()))))
  , expectPE TrueT $ pl @(Re "\\d{4}-\\d{3}") "1234-123"
  , expectPE FalseT $ pl @(Re "\\d{4}-\\d{3}") "1234-1x3"
  , expectPE TrueT $ pl @(Re' "ab" '[ 'Caseless, 'Dotall ]) "aB"
  , expectPE TrueT $ pl @(Re' "ab." '[ 'Caseless, 'Dotall ]) "aB\n"
  , expectPE FalseT $ pl @(Re' "ab." '[ 'Caseless ]) "aB\n"
  , expectPE TrueT $ pl @(Re "(?i)ab") "aB" -- runtime [use 'Caseless instead]
  , expectPE FalseT $ pl @(Re "ab") "aB"
  , expectPE (PresentT [("aB",["B"]),("cd",["d"])]) $ pl @(Rescan ".(.)") "aBcd"
  , expectPE (PresentT [14,12,10,4,2]) $ pl @(SortOnDesc Id Id) [10,4,2,12,14]
  , expectPE (PresentT [2,4,10,12,14]) $ pl @(SortOn Id Id) [10,4,2,12,14]
  , expectPE (PresentT [14,12,10,4,2]) $ pl @(SortOn Negate Id) [10,4,2,12,14]
  , expectPE (PresentT [('a',4),('a',14),('b',2),('c',10),('d',12),('z',1)]) $ pl @(SortOn Fst Id) (zip "cabdaz" [10,4,2,12,14,1])
  , expectPE (FailT "asdf(4)") $ pl @(SortOn (FailS "asdf") Id) [10,4,2,12,14]
  , expectPE TrueT $ pl @(Min &&& Max >> Id >> Fst < Snd) [10,4,2,12,14]
  , expectPE (FailT "Exitwhen") $ pl @(Partition (Exitwhen' (Gt 10) >> Gt 2) Id) [1..11]
  , expectPE (PresentT [False,False,True,True,True]) $ pl @(Map (Exitwhen' (Gt 10) >> Gt 2)) [1..5]
  , expectPE (PresentT ([1,2],[3,4,5,6,7,8,9,10,11])) $ pl @(Break (Gt 2) Id) [1..11]
  , expectPE (PresentT ([1,2,3],[4,5,6,7,8,9,10,11])) $ pl @(Span (Lt 4) Id) [1..11]
  , expectPE (PresentT [GT,GT,LT,EQ]) $ pl @(Pairs >> Map (First (Succ >> Succ) >> Fst === Snd)) [1,2,3,6,8]
  , expectPE TrueT $ pl @(Re "^\\d{1,3}(?:\\.\\d{1,3}){3}$") "123.1.1.21"
  , expectPE (PresentT [("123.",["."]),("8.",["."]),("99.",["."]),("21",[])]) $ pl @(Rescan "\\d{1,3}(\\.)?") "123.8.99.21"
  , expectPE (PresentT 117) $ pl @(MaybeIn (Failp "err") Succ) (Just 116)
  , expectPE (PresentT 99) $ pl @(MaybeIn 99 Succ) (Nothing @Int)
  , expectPE (FailT "someval") $ pl @(MaybeIn (Failp "someval") Succ) (Nothing @())
  , expectPE TrueT $ pl @(MaybeIn 'True 'False) (Nothing @())
  , expectPE FalseT $ pl @(MaybeIn 'True 'False) (Just "aa")
  , expectPE (PresentT LT) $ pl @(MaybeIn MemptyP (Fst === Snd)) (Just ('x','z'))
  , expectPE (PresentT EQ) $ pl @(MaybeIn MemptyP (Fst === Snd)) (Nothing @(Char,Char))
  , expectPE TrueT $ pl @('True ||| 'False) (Left @_ @() "someval")
  , expectPE FalseT $ pl @('True ||| 'False) (Right @() "someval")
  , expectPE (PresentT 123) $ pl @('Left Id) (Left 123)
  , expectPE (FailT "'Left found Right") $ pl @('Left Id) (Right @() 123)
  , expectPE (PresentT 123) $ pl @('Right Id) (Right 123)
  , expectPE (FailT "'Right found Left") $ pl @('Right Id) (Left @_ @() 123)
  , expectPE (PresentT ["1","2","3"]) $ pl @(MaybeIn MemptyP (ShowP >> Ones)) (Just 123)
  , expectPE (PresentT []) $ pl @(MaybeIn MemptyP (ShowP >> Ones)) (Nothing @String)
  , expectPE (PresentT "124") $ pl @((Succ >> ShowP) ||| ShowP ) (Left @_ @() 123)
  , expectPE (PresentT "True") $ pl @((Succ >> ShowP) ||| ShowP ) (Right @Int True)
  , expectPE (PresentT (123 % 4)) $ pl @(ReadP Rational) "123 % 4"
  , expectPE (FailT "ReadP Ratio Integer (x123 % 4) failed") $ pl @(ReadP Rational) "x123 % 4"
  , expectPE (PresentT "") $ pl @('Proxy >> MemptyP) "abc"
  , expectPE (PresentT ["a","b","c"]) $ pl @(MemptyT _ ||| Ones) (Right @() "abc")
  , expectPE (PresentT []) $ pl @(MemptyT _ ||| Ones) (Left @_ @[String] ["ab"])
  , expectPE (PresentT ["a","b"]) $ pl @(MaybeIn MemptyP Ones) (Just @String "ab")
  , expectPE (PresentT []) $ pl @(MaybeIn MemptyP Ones) (Nothing @String)
  , expectPE (PresentT (True, 13)) $ pl @((IsNothing >> Not) &&& (Just' >> Add Id 12)) (Just 1)
  , expectPE (FailT "expected Just") $ pl @((IsNothing >> Not) &&& (Just' >> Add Id 12)) Nothing
  , expectPE (PresentT True) $ pl @(ThdL _ >> FstL _) (1,2,(True,4))
  , expectPE (PresentT True) $ pl @(ThdL _ >> FstL _) (1,2,(True,4))
  , expectPE (PresentT 'd') $ pl @(Id !! 3) ("asfd" :: T.Text)
  , expectPE (FailT "(!!) index not found") $ pl @(Id !! 4) ("asfd" :: T.Text)
  , expectPE (PresentT "dfsa") $ pl @ReverseL ("asfd" :: T.Text)
  , expectPE (PresentT (Left "asfd")) $ pl @Swap (Right @() "asfd") -- @() else breaks: ok in ghci
  , expectPE (PresentT ("asfd",12)) $ pl @Swap (12,"asfd")
  , expectPE (PresentT (Just ('a',"sfd"))) $ pl @Uncons ("asfd" :: T.Text)
  , expectPE (PresentT Nothing) $ pl @Uncons ("" :: T.Text)
  , expectPE (PresentT (Just ("asf",'d'))) $ pl @Unsnoc ("asfd" :: T.Text)
  , expectPE (PresentT Nothing) $ pl @Unsnoc ("" :: T.Text)
  , expectPE FalseT $ pl @IsEmpty ("failed11" :: T.Text)
  , expectPE TrueT $ pl @IsEmpty ("" :: T.Text)
  , expectPE (PresentT 14) $ pl @(Unwrap >> Succ) (SG.Sum 13)
  , expectPE (PresentT 4) $ pl @(MemptyT (SG.Sum _) >> Unwrap >> Add Id 4) ()
  , expectPE (PresentT (SG.Sum 13)) $ pl @(Wrap (SG.Sum _) Id) 13
  , expectPE (PresentT "a") $ pl @(Id !! MemptyT _) (Just "a")
  , expectPE (FailT "(!!) index not found") $ pl @(Id !! MemptyT _) (Nothing @()) -- had to add @() to keep this happy: ghci is fine
  , expectPE (PresentT 'a') $ pl @(Id !! 0) ('a','b','c')
  , expectPE (FailT "err") $ pl @(Id !! Failt _ "err") ('a','b','c')
  , expectPE (PresentT 3) $ pl @(Id !! "d") (M.fromList $ zip (map (:[]) "abcd") [0 ..])
  , expectPE (PresentT 3) $ pl @(Id !! ("d" >> Head'' "failedn")) (M.fromList $ zip "abcd" [0 ..]) -- had to String (instead of _) to keep this happy: ghci is fine
  , expectPE (PresentT ()) $ pl @(Id !! ("d" >> Head'' "failedn")) (S.fromList "abcd") -- had to String (instead of _) to keep this happy: ghci is fine
  , expectPE (FailT "(!!) index not found") $ pl @(Id !! ("e" >> Head'' "failedn")) (S.fromList "abcd") -- had to String (instead of _) to keep this happy: ghci is fine
  , expectPE (PresentT 13.345) $ pl @(Exitwhen' (Re "^\\d+(?:\\.\\d+)?$" >> Not) >> ReadP Double) "13.345"
  , expectPE (PresentT 13) $ pl @(Exitwhen' (Re "^\\d+(?:\\.\\d+)?$" >> Not) >> ReadP Double) "13"
  , expectPE (FailT "regex failed") $ pl @(Exitwhen "regex failed" (Re "^\\d+(?:\\.\\d+)?$" >> Not) >> ReadP Double) "-13.4"
  , expectPE (PresentT GT) $ pl @(Repeat 2 Id Succ) LT
  , expectPE (FailT "Succ IO e=Prelude.Enum.Ordering.succ: bad argument") $ pl @(Repeat 30 Id Succ) LT
  , expectPE (PresentT 'g') $ pl @(Repeat 6 Id Succ) 'a'
  , expectPE (PresentT '[') $ pl @(Repeat 6 Id Pred) 'a'
  , expectPE (FailT "Regex failed to compile") $ pl @(Re "\\d{4}\\") "ayx"
  , expectPE (PresentT LT) $ pl @(Repeat 0 Id Succ) LT
  , expectPE (PresentT LT) $ pl @(Repeat 2 Id Succ >> Repeat 2 Id Pred) LT
  , expectPE (PresentT ["2","2"]) $ pl @(ShowP >> Rescan "." >> Map Fst >> FilterBy (Same "2") Id) 12324
  , expectPE (PresentT [LT,LT,LT,GT,EQ,LT]) $ pl @((Ones << Id << ShowP) >> Pairs >> Map (Fst === Snd)) 1234223
  , expectPE (PresentT [(0,'a'),(1,'b'),(2,'c'),(3,'d')]) $ pl @(IToList _) ("abcd" :: String)
  , expectPE (PresentT "abcd") $ pl @ToList (M.fromList $ zip [0..] "abcd")
  , expectPE (PresentT [123]) $ pl @ToList (Just 123)
  , expectPE (FailT "failed20") $ pl @(MaybeIn (Failp "failed20") 'False) (Nothing @Int)
  , expectPE (FailT "failed21") $ pl @(MaybeIn ('False >> FailS "failed21") 'False) (Nothing @Double)
  , expectPE (FailT "err") $ pl @(MaybeIn (Failp "err") Id) (Nothing @Int)
  , expectPE (FailT "err") $ pl @(MaybeIn (Failp "err") Id) (Nothing @())
  , expectPE (PresentT [(0,'a'),(1,'b'),(2,'c'),(3,'d')]) $ pl @(IToList _) (M.fromList $ itoList ("abcd" :: String))
  , expectPE (PresentT [('a',1),('b',2),('c',3),('d',4),('a',5),('b',6),('c',7)]) $ pl @(Ziplc "abcd" Id) [1..7]
  , expectPE (PresentT [('a',1),('b',2),('c',3),('d',4)]) $ pl @(Zipn "abcd" Id) [1..7]
  , expectPE (PresentT []) $ pl @(Zipn "" Id) [1..7]
  , expectPE (PresentT [(1 % 1,'a'),(2 % 1,'b'),(3 % 1,'c'),(1 % 1,'d')]) $ pl @(Ziplc '[Pos 1, Pos 2, Pos 3] Id) ("abcd" )
  , expectPE (PresentT []) $ pl @(Ziprc (EmptyT _) Id) "abcd"
  , expectPE (PresentT [9,2,7,4]) $ pl @ToList (M.fromList (zip ['a'..] [9,2,7,4]))
  , expectPE (PresentT [(0,9),(1,2),(2,7),(3,4)]) $ pl @(IToList _) [9,2,7,4]
  , expectPE (PresentT [('a',9),('b',2),('c',7),('d',4)]) $ pl @(IToList _) (M.fromList (zip ['a'..] [9,2,7,4]))
  , expectPE (PresentT [((),234)]) $ pl @(IToList _) (Just 234)
  , expectPE (PresentT []) $ pl @(IToList _) (Nothing @Double)
  , expectPE (PresentT (-4,5)) $ pl @(DivMod Negate 7) 23
  , expectPE (PresentT (-3,-2)) $ pl @(QuotRem Negate 7) 23
  , expectPE (PresentT (True,3.4)) $ pl @(ThdL _ >> SndL _ >> FstL _) (1,'a',('x',((True,3.4),999)))
  , expectPE (PresentT 7) $ pl @(FstL _) (7,999.12)
  , expectPE (PresentT (M.fromList [(1,'a')])) $ pl @(MaybeIn MemptyP Id) (Just (M.fromList [(1,'a')]))
  , expectPE (PresentT (M.fromList [])) $ pl @(MaybeIn MemptyP Id) (Nothing @(M.Map () ()))
  , expectPE (PresentT [("1",["1"]),("2",["2"]),("3",["3"]),("4",["4"])]) $ pl @(Rescan "(\\d)+?") "1234"
  , expectPE (PresentT [("1234",["4"])]) $ pl @(Rescan "(\\d)+") "1234"
  , expectPE (PresentT [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])]) $ pl @(Rescan "(\\d{1,3})(\\.(\\d{1,3}))+?") "1.2.3.4" -- overcapturing
  , expectPE (PresentT [("1234",["4"])]) $ pl @(Rescan "^(\\d)+?$") "1234"
  , expectPE (PresentT [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])]) $ pl @(Rescan "(\\d{1,3})(\\.(\\d{1,3}))+?") "1.2.3.4"
  , expectPE (PresentT ["123","2","3","5","6"]) $ pl @(Resplit "\\.") "123.2.3.5.6"
  , expectPE (PresentT [("1.2",["1","2"]),("3.4",["3","4"])]) $ pl @(Rescan "(\\d{1,3})(?:\\.(\\d{1,3}))+?") "1.2.3.4" -- bizzare!
  , expectPE (PresentT [("1.2.3.4",["1","2","3","4"])]) $ pl @(Rescan "^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$") "1.2.3.4" -- this is good!
  , expectPE (PresentT [13,16,17]) $ pl @(Guard "err" (Len >> Gt 2) >> Map Succ) [12,15,16]
  , expectPE (FailT "err found len=3") $ pl @(Guard (Len >> Printf "err found len=%d") (Len >> Gt 5) >> Map Succ) [12,15,16]
  , expectPE (FailT "Printf (IO e=printf: bad formatting char 'd')") $ pl @(Printf "someval %d") ("!23"::String)
  , expectPE (PresentT [12,0,1,13,0,1,14,0,1,15,0,1,16]) $ pl @(Intercalate Fst Snd) ([0,1], [12,13,14,15,16])
  , expectPE (PresentT [12,-5,13,-5,14,-5,15,-5,16]) $ pl @(((Pure [] (Len >> Negate)) &&& Id) >> Intercalate Fst Snd) [12,13,14,15,16]
  , expectPE (PresentT [13,16,17]) $ pl @(If (Len >> Gt 2) (Map Succ) (FailS "someval")) [12,15,16]
  , expectPE (PresentT [13,16,17]) $ pl @(Guard' (Len >> Gt 2) >> Map Succ) [12,15,16]
  , expectPE (FailT "err") $ pl @(Guard "err" (Len >> Gt 2) >> Map Succ) [12]
  , expectPE (PresentT [13]) $ pl @(Exitwhen "err" (Len >> Gt 2) >> Map Succ) [12]
  , expectPE (FailT "err") $ pl @(Exitwhen "err" (Len >> Gt 2) >> Map Succ) [12,15,16]
  , expectPE (PresentT [13]) $ pl @(Exitwhen "err" (Len >> Gt 2) >> Map Succ) [12]
  , expectPE (FailT "err") $ pl @(Exitwhen "err" (Len >> Gt 2) >> Map Succ) [12,15,16]
  , expectPE (PresentT [13,16,17]) $ pl @(Guard "err" (Len >> Gt 2) >> Map Succ) [12,15,16]
  , expectPE (FailT "err") $ pl @(Guard "err" (Len >> Gt 2) >> Map Succ) [12]
  , expectPE (PresentT 12) $ pl @OneP [12]
  , expectPE (FailT "expected list of length 1 but found length=5") $ pl @OneP [1..5]
  , expectPE (FailT "expected list of length 1 but found length=0") $ pl @OneP ([] @())
  , expectPE (FailT "err(8)") $ pl @(Map (If (Lt 3) 'True (Failt _ "err"))) [1..10]
  , expectPE (FailT "someval(8)") $ pl @(Map (If (Lt 3) 'True (Failt _ "someval"))) [1..10] -- use Guard ie Guard
  , expectPE (PresentT [True,True,False,False,False]) $ pl @(Map (If (Lt 3) 'True 'False)) [1..5]
  , expectPE (PresentT ["a","b","c"]) $ pl @(MaybeIn MemptyP Ones) (Just @String "abc")
  , expectPE (FailT "someval") $ pl @(Guard "someval" (Len >> Same 2) >> (ShowP &&& Id)) ([] @Int)
  , expectPE (PresentT ([2,3],"[2,3]")) $ pl @(Guard "someval" (Len >> Same 2) >> (Id &&& ShowP)) [2,3]
  , expectPE (FailT "someval") $ pl @(Guard "someval" (Len >> Same 2) >> (ShowP &&& Id)) [2,3,4]
  , expectPE (PresentT 55) $ pl @(Map (Wrap (SG.Sum _) Id) >> Mconcat >> Unwrap) [1..10]
  , expectPE (PresentT True) $ pl @(EitherIn Not Id) (Right @Bool True)
  , expectPE FalseT $ pl @(EitherIn Not Id) (Left @_ @Bool True)
  , expectPE FalseT $ pl @(Re "^\\d+$") "123\nx"
  , expectPE TrueT $ pl @(Re "(?m)^\\d+$") "123\nx" -- (?m) anchors match beginning/end of line instead of whole string
  , expectPE (PresentT (Just 'x')) $ pl @(Pure Maybe Id) 'x'
  , expectPE (PresentT (Right @() 'x')) $ pl @(Pure (Either _) Id) 'x'
  , expectPE (PresentT Nothing) $ pl @(MemptyT (Maybe ())) 'x'
  , expectPE (PresentT (Left @_ @() 'x')) $ pl @(Pure (Either _) Id >> Swap) 'x'
  , expectPE (PresentT (Left 'x')) $ pl @(Pure (Either ()) Id >> Swap) 'x'
  , expectPE (PresentT (SG.Sum 52)) $ pl @(STimes 4 Id) (SG.Sum 13)
  , expectPE (PresentT (SG.Sum 52)) $ pl @(Wrap (SG.Sum _) Id >> STimes 4 Id) 13
  , expectPE (PresentT 52) $ pl @(Foldmap (SG.Sum _)) [14,8,17,13]
  , expectPE (PresentT 17) $ pl @(Foldmap (SG.Max _)) [14 :: Int,8,17,13] -- cos Bounded!
  , expectPE FalseT $ pl @(Catch (Re "\\d+(") 'False) "123"
  , expectPE TrueT $ pl @(Catch (Re "\\d+") 'False) "123"
  , expectPE (PresentT 3) $ pl @(Id !! ("d" >> Head')) (M.fromList $ zip "abcd" [0 ..])   -- use Char1 "d" instead of "d" >> Head'
  , expectPE (PresentT 10) $ pl @(Id !! MemptyT _) (Just 10)
  , expectPE (FailT "(!!) index not found") $ pl @(Id !! MemptyT _) (Nothing @())
  , expectPE TrueT $ pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) || (Foldmap (SG.Sum _) >> Gt 200)) [1..20]
  , expectPE FalseT $ pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) || (Foldmap (SG.Sum _) >> Gt 200)) [1..19]
  , expectPE TrueT $ pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) || (Foldmap (SG.Sum _) >> Gt 200)) []
  , expectPE (PresentT (False, 210)) $ pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) &&& Foldmap (SG.Sum _)) [1..20]
  , expectPE (PresentT 'g') $ pl @(Id !! 6) ['a'..'z']
  , expectPE (PresentT ([141,214,125,1,2,3333],(False,False))) $ pl @(Resplit "\\." >> Map (ReadP Int) >> '(Id, '(Len >> Same 4, All (Between 0 255)))) "141.214.125.1.2.3333"
  , expectPE (PresentT ([141,214,125,1,2,6],(False,True))) $ pl @(Resplit "\\." >> Map (ReadP Int) >> Id &&& ((Len >> Same 4) &&& All (Between 0 255))) "141.214.125.1.2.6"
  , expectPE (FailT "ReadP Int () failed") $ pl @(Resplit "\\." >> Map (ReadP Int) >> Id &&& ((Len >> Same 4) &&& All (Between 0 255))) "141.214.125."
  , expectPE (PresentT 9) $ pl @((Wrap _ Id *** Wrap (SG.Sum _) Id) >> Sapa >> Unwrap) (4,5)
  , expectPE (PresentT (SG.Sum 9)) $ pl @((Wrap _ Id *** Wrap _ Id) >> Sapa) (4,5)
  , expectPE (PresentT 9) $ pl @(Sapa' (SG.Sum _) >> Unwrap) (4,5)
  , expectPE (PresentT "abcde") $ pl @(ScanNA Succ) (4,'a')
  , expectPE (PresentT ["abcd","bcd","cd","d",""]) $ pl @(ScanNA Tail) (4,"abcd" :: String)
  , expectPE (PresentT ["abcd","bcd","cd","d",""]) $ pl @(Len &&& Id >> ScanNA Tail) "abcd"
  , expectPE (PresentT ["abcd","bcd","cd","d",""]) $ pl @Tails ("abcd" :: String)
  , expectPE (PresentT (-4,-2)) $ pl @(DivMod Fst Snd) (10,-3)
  , expectPE (PresentT (-3,1)) $ pl @(QuotRem Fst Snd) (10,-3)
  , expectPE (FailT "DivMod zero denominator") $ pl @(DivMod Fst Snd) (10,0)
  , expectPE (PresentT 'd') $ pl @(Snd !! Fst) (3,"abcde" :: String)
  , expectPE (FailT "(!!) index not found") $ pl @(Snd !! Fst) (4,[9,8])
  , expectPE (PresentT 'c') $ pl @(2 &&& Id >> Snd !! Fst) ("abcdef" :: String)
  , expectPE (PresentT 'f') $ pl @((Len >> Pred) &&& Id >> Snd !! Fst) "abcdef"
  , expectPE (FailT "len is bad") $ pl @Ip6 "FE80::203:Baff:FE77:326FF"
  , expectPE (FailT "not a hex") $ pl @Ip6 "FE80::203:Baff:GE77:326F"
  , expectPE (FailT "count is bad") $ pl @Ip6 "FE80::203:Baff:FE77:326F:::::"
  , expectPE (PresentT 65504) $ pl @(ReadBaseInt 16) "fFe0"
  , expectPE (PresentT "ffe0") $ pl @(ShowBase 16) 65504
  , expectPE (FailT "invalid base 22") $ pl @(ReadBaseInt 22) "zzz"
  , expectPE (PresentT ("ffe0","fFe0")) $ pl @((ReadBaseInt 16 &&& Id) >> (First (ShowBase 16))) "fFe0"
  , expectPE FalseT $ pl @(Id == "Abc") "abc"
  , expectPE TrueT $ pl @("Abc" ==? Id) "abc"
  , expectPE (PresentT LT) $ pl @("Abc" === Id) "abc"
  , expectPE (PresentT EQ) $ pl @("Abc" ===? Id) "abc"
  , expectPE (PresentT 'd') $ pl @(Id !! 3) ('a','b','c','d','e')
  , expectPE (PresentT 99) $ pl @(Id !! "s") $ M.fromList [("t",1), ("s", 20), ("s", 99)]
  , expectPE (PresentT 1) $ pl @Head' [1,2,3]
  , expectPE (PresentT (Just (1,[2,3,4,5]))) $ pl @Uncons [1..5] -- with Typeable would need to specify the type of [1..5]
  , expectPE (PresentT (Just ([1,2,3,4],5))) $ pl @Unsnoc [1..5]
  , expectPE (PresentT [(0,1),(1,2),(2,3),(3,4),(4,5)]) $ pl @(IToList _) [1..5]
  , expectPE (PresentT [(0,'a'),(1,'b'),(2,'c')]) $ pl @(IToList _) ['a','b','c']
  , expectPE (PresentT [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')]) $ pl @(Zipn Fst Snd) ([1..5],['a'..'z'])
  , expectPE (PresentT [1,2,3,8,8]) $ pl @(PadR 5 8 Id) [1..3]
  , expectPE (PresentT [1,2,3,4,5]) $ pl @(PadR 5 0 Id) [1..5]
  , expectPE (PresentT [1,2,3,4,5,6]) $ pl @(PadR 5 0 Id) [1..6]
  , expectPE (PresentT [0,0,1,2,3]) $ pl @(PadL 5 0 Id) [1..3]
  , expectPE (PresentT []) $ pl @(Catch (Resplit "\\d+(") (Snd >> MemptyP)) "123"
  , expectPE (FailT "someval(8)") $ pl @(Map (Guard "someval" (Lt 3) >> 'True)) [1::Int ..10]
  , expectPE (PresentT [True,True,True,True,True,True,True,True,True,True])
      $ pl @(Map (Guard "someval" (Ge 1) >> 'True)) [1::Int ..10]
  , expectPE (PresentT [4,5,6]) $ pl @(ScanN 2 Id Succ) 4
  , expectPE (PresentT [4,4,4,4,4,4]) $ pl @(ScanN 5 Id Id) 4
  , expectPE (PresentT "") $ pl @('Proxy >> MemptyP) "abc"
  , expectPE (PresentT [1,2,3,244])
      $ pl @(Rescan Ip4 >> OneP >> Snd >> Map (ReadBaseInt 10) >> Ip4guard) "1.2.3.244"
  , expectPE (FailT "0-255")
      $ pl @(Rescan Ip4 >> OneP >> Snd >> Map (ReadBaseInt 10) >> Ip4guard) "1.256.3.244"
  , expectPE (FailT "0-255")
      $ pl @(Rescan "(\\d+)\\.?" >> Map Snd >> Concat >> Map (ReadBaseInt 10) >> Ip4guard) "1.22.312.66"
  , expectPE (FailT "4octets")
      $ pl @(Rescan "(\\d+)\\.?" >> Map Snd >> Concat >> Map (ReadBaseInt 10) >> Ip4guard) "1.22.244.66.77"
  , expectPE (PresentT [1,23,43,214])
      $ pl @(Rescan "(\\d+)\\.?" >> Map Snd >> Concat >> Map (ReadBaseInt 10) >> Ip4guard) "1.23.43.214"
  , expectPE (PresentT (SG.Sum 123)) $ pl @(JustP' Id) (Just (SG.Sum 123))
  , expectPE (PresentT (SG.Sum 0)) $ pl @(JustP' Id) (Nothing @(SG.Sum _))
  , expectPE (PresentT (636 % 5)) $ pl @(((123 >> ToRational) &&& Id) >> Fst + Snd) 4.2
  , expectPE (PresentT 127) $ pl @((123 &&& Id) >> Fst + Snd) 4
  , expectPE (PresentT 256)
      $ pl @(Rescan "(?i)^\\\\x([0-9a-f]{2})$" >> OneP >> Snd >> OneP >> ReadBaseInt 16 >> Succ) "\\xfF"
  , expectPE (PresentT 256)
      $ pl @(Rescan "(?i)^\\\\x(.{2})$" >> OneP >> Snd >> OneP >> ReadBaseInt 16 >> Succ) "\\xfF"
  , expectPE (PresentT (("fF",(255,"ff")),False))
      $ pl @(Rescan "(?i)^\\\\x([0-9a-f]{2})$" >> OneP >> Snd >> OneP >> (Id &&& (ReadBaseInt 16 >> (Id &&& ShowBase 16))) >> (Id &&& ((Id *** Snd) >> Fst == Snd))) "\\xfF"
  , expectPE (PresentT (("fF",(255,"ff")),False))
      $ pl @(Rescan "(?i)^\\\\x([0-9a-f]{2})$" >> OneP >> Snd >> OneP >> (Id &&& (ReadBaseInt 16 >> (Id &&& ShowBase 16))) >> (Id &&& ((Id *** Snd) >> Fst == Snd))) "\\xfF"
  , expectPE (PresentT [1,2,4,0])
      $ pl @(Do '[Succ,Id,ShowP,Ones,Map (ReadBaseInt 8)]) 1239
  , expectPE (PresentT [1,2,4,0])
      $ pl @(Do '[Succ,Id,ShowP,Ones,Map (ReadBaseInt 8)]) 1239
  , expectPE (FailT "invalid base 8")
      $ pl @(Do '[Pred,Id,ShowP,Ones,Map (ReadBaseInt 8)]) 1239
  , expectPE (PresentT 47) $ pl @(ReadBaseInt 2) "101111"
  , expectPE (PresentT [LT,EQ,GT,EQ,EQ,EQ,EQ,EQ,EQ,EQ]) $ pl @(ScanN 2 Id Succ >> PadR 10 (MemptyT Ordering) Id) LT
  , expectPE (PresentT [LT,EQ,GT,EQ,EQ,EQ,EQ,EQ,EQ,EQ]) $ pl @(ScanN 2 Id Succ >> PadR 10 (MemptyT Ordering) Id) LT
  , expectPE (PresentT 12) $ pl @('This Id) (This 12)
  , expectPE (FailT "'This found That") $ pl @('This Id) (That @() 12)
  , expectPE (PresentT (SG.Sum 12)) $ pl @(ThisP' Id) (This @_ @() (SG.Sum 12))
  , expectPE (PresentT ()) $ pl @(ThisP' Id) (That 12)
  , expectPE (PresentT (SG.Sum 12)) $ pl @(ThisFail "sdf" Id) (This @_ @() (SG.Sum 12))
  , expectPE (FailT "sdf") $ pl @(ThisFail "sdf" Id) (That @() (SG.Sum 12))
  , expectPE (FailT "sdf") $ pl @(ThisFail "sdf" Id) (That @Int 12)
  , expectPE (PresentT "this") $ pl @(TheseIn "this" "that" "these") (This @_ @() (SG.Sum 12))
  , expectPE FalseT $ pl @IsThese (That @() (SG.Sum 12))
  , expectPE TrueT $ pl @IsThese (These 1 (SG.Sum 12))
  , expectPE (PresentT ("Ab",13)) $ pl @(TheseIn (Id &&& 999) ("no value" &&& Id) Id) (These "Ab" 13)
  , expectPE (PresentT ("Ab",999)) $ pl @(TheseIn (Id &&& 999) ("no value" &&& Id) Id) (This "Ab")
  , expectPE (PresentT ("no value",13)) $ pl @(TheseIn (Id &&& 999) ("no value" &&& Id) Id) (That 13)
  , expectPE (PresentT "wxydef") $ pl @(ZipThese Fst Snd >> Map (TheseIn Id Id Fst)) (['w'..'y'],['a'..'f'])
  , expectPE (PresentT [("fe",["fe"]),("b1",["b1"]),("2a",["2a"])]) $ pl @(Rescan "([[:xdigit:]]{2})") "wfeb12az"
  -- anchored means it has to start at the beginning: can have junk on the end which we cant detect but at least we know it starts at beginning
  , expectPE (FailT "Regex no results") $ pl @(Rescan' "([[:xdigit:]]{2})" '[ 'Anchored ]) "wfeb12az"
  , expectPE (PresentT [('s',1),('d',2),('f',3),('x',4),('x',5)]) $ pl @(("sdf" &&& Id) >> ZipThese Fst Snd >> Map (TheseIn (Id &&& 0) (("x" >> Head') &&& Id) Id)) [1..5]
  , expectPE (PresentT "abc") $ pl @"abc" ()
  , expectPE (PresentT 123) $ pl @123 ()
  , expectPE FalseT $ pl @('True >> Not) ()
  , expectPE TrueT $ pl @'True ()
  , expectPE FalseT $ pl @'False ()
  , expectPE (PresentT LT) $ pl @'LT ()
  , expectPE (PresentT 123) $ pl @123 ()
  , expectPE (PresentT (4,("sadf",LT))) $ pl @(4 &&& "sadf" &&& 'LT) ()
  , expectPE (PresentT (4,("sadf",LT))) $ pl @(4 *** "sadf" *** 'LT) ('x',("abv",[1]))
  , expectPE (PresentT 6) $ pl @(Do '[4,5,6]) ()
  , expectPE (PresentT "hhhhh") $ pl @(Do '["abc", "Def", "ggg", "hhhhh"]) ()
  , expectPE (PresentT GT) $ pl @(Do '[ 'LT, 'EQ, 'GT ]) ()
  , expectPE (PresentT (-3 % 1)) $ pl @(Do '[Rat 'True 4 4,Pos 22,NegR 12 4]) ()
  , expectPE (PresentT [10,2,5,8]) $ pl @(GuardsLax (ToGuardsT (Printf2 "guard(%d) %d is out of range") '[Between 0 11, Between 1 4,Between 3 5])) [10::Int,2,5,8]
  , expectPE (PresentT [31,11,1999]) $ pl @(Rescan DdmmyyyyR >> OneP >> Snd >> Map (ReadBaseInt 10) >> Ddmmyyyyval) "31-11-1999"
  , expectPE (PresentT [31,11,1999]) $ pl @(Guards (ToGuardsT (Printf2 "guard(%d) %d is out of range") '[Between 1 31, Between 1 12, Between 1990 2050])) [31,11,1999::Int]
  , expectPE (PresentT [31,11,1999,123,44]) $ pl @(GuardsLax (ToGuardsT (Printf2 "guard(%d) %d is out of range") '[Between 1 31, Between 1 12, Between 1990 2050])) [31,11,1999,123,44::Int]
  , expectPE (FailT "Guards: data elements(2) /= predicates(3)") $ pl @(Guards (ToGuardsT (Printf2 "guard(%d) %d is out of range") '[Between 1 31, Between 1 12, Between 1990 2050])) [31,11::Int]
  , expectPE (FailT "guard(1) 13 is out of range") $ pl @(Guards (ToGuardsT (Printf2 "guard(%d) %d is out of range") '[Between 1 31, Between 1 12, Between 1990 2050])) [31,13,1999::Int]
  , expectPE (FailT "guard(0) 0 is out of range") $ pl @(Guards (ToGuardsT (Printf2 "guard(%d) %d is out of range") '[Between 1 31, Between 1 12, Between 1990 2050])) [0,44,1999::Int]
  , expectPE (PresentT (fromGregorian 1999 11 30)) $ pl @(ReadP Day) "1999-11-30"
  , expectPE (FailT "ReadP Day (1999-02-29) failed") $ pl @(ReadP Day) "1999-02-29"
  , expectPE (PresentT (TimeOfDay 14 59 20)) $ pl @(ReadP TimeOfDay) "14:59:20"
  , expectPE (PresentT (TimeOfDay 26 61 61)) $ pl @(ReadP TimeOfDay) "26:61:61" -- yep: this is valid! need to do your own validation
  , expectPE (FailT "ParseTimeP TimeOfDay (%H:%M%S) failed to parse") $ pl @(ParseTimeP TimeOfDay "%H:%M%S" Id) "14:04:61"
  , expectPE (PresentT (TimeOfDay 23 13 59)) $ pl @(Guard "hh:mm:ss regex failed" (Re HmsR) >> ReadP TimeOfDay) "23:13:59"
  , expectPE (FailT "hh:mm:ss regex failed") $ pl @(Guard "hh:mm:ss regex failed" (Re HmsR) >> ReadP TimeOfDay) "23:13:60"
  , expectPE (FailT "Guards: data elements(5) /= predicates(3)") $ pl @(Guards (ToGuardsT (Printf2 "guard(%d) %d is out of range") '[Between 1 31, Between 1 12, Between 1990 2050])) [31,11,2000,1,2::Int]
  , expectPE (PresentT [31,11,2000,1,2]) $ pl @(GuardsLax (ToGuardsT (Printf2 "guard(%d) %d is out of range") '[Between 1 31, Between 1 12, Between 1990 2050])) [31,11,2000,1,2::Int]
  , expectPE (PresentT [0,0,0,0,0,0,0,1,2,3]) $ pl @(PadL 10 0 Id) [1..3]
  , expectPE (PresentT (124,["1","2","2"])) $ pl @('Left Id >> (Succ &&& (Pred >> ShowP >> Ones))) (Left 123)
  , expectPE (PresentT [1,2,3,4]) $ pl @(Guards (ToGuardsT (Printf2 "guard(%d) %d is out of range") (RepeatT 4 (Between 0 255)))) [1,2,3,4::Int]
  , expectPE (FailT "Guards: data elements(5) /= predicates(4)") $ pl @(Guards (ToGuardsT (Printf2 "guard(%d) %d is out of range") (RepeatT 4 (Between 0 255)))) [1,2,3,4,5::Int]
  , expectPE (FailT "Guards: data elements(3) /= predicates(4)") $ pl @(Guards (ToGuardsT (Printf2 "guard(%d) %d is out of range") (RepeatT 4 (Between 0 255)))) [1,2,3::Int]
  , expectPE (PresentT (read "1999-01-01 12:12:12 Utc")) $ pl @(ParseTimeP UTCTime "%F %T" Id) "1999-01-01 12:12:12"
  , expectPE (PresentT 123) $ pl @(JustDef 0 Id) (Just 123)
  , expectPE (PresentT 0) $ pl @(JustDef 0 Id) Nothing
  , expectPE (PresentT 12) $ pl @(LastDef 0) [1..12]
  , expectPE (PresentT 0) $ pl @(LastDef 0) []
  , expectPE (PresentT (1,("asdf",True))) $ pl @'(1,'("asdf",'True)) ()
  , expectPE (PresentT (1,("asdf",(True,())))) $ pl @(TupleI '[W 1,W "asdf",W 'True]) ()
  , expectPE (PresentT ("abc", True)) $ pl @(Theseid 'True "xyz") (This "abc")
  , expectPE (PresentT ("xyz", False)) $ pl @(Theseid 'True "xyz") (That False)
  , expectPE (PresentT ("abc", False)) $ pl @(Theseid 'True "xyz") (These "abc" False)
  , expectPE (PresentT ("xyz", True)) $ pl @(TheseDef '("xyz",'True) Id) (This "abc")
  , expectPE (PresentT ("xyz", True)) $ pl @(TheseDef '("xyz",'True) Id) (That False)
  , expectPE (PresentT ("abc", False)) $ pl @(TheseDef '("xyz",'True) Id) (These "abc" False)
  , expectPE (PresentT 3) $ pl @(Id !! Char1 "d") (M.fromList $ zip "abcd" [0 ..])
  , expectPE (PresentT (12, False)) $ pl @('These Id Not) (These 12 True)
  , expectPE (PresentT (SG.Any True)) $ pl @(Coerce SG.Any) True
  , expectPE (PresentT True) $ pl @(Coerce Bool) (SG.Any True)
  , expectPE (PresentT (3, SG.Any True)) $ pl @(Id !! (FromStringP' (T _) "d") &&& (IToList _ >> Map (Snd >> Gt 3 >> Coerce SG.Any) >> Mconcat ) ) (M.fromList $ zip (map T.singleton "abcdefgh") [0 ..])
  , expectPE (PresentT (3, True)) $ pl @(Id !! ("d" >> FromStringP _) &&& (IToList _ >> Map (Snd >> Gt 3 >> Wrap SG.Any Id) >> Mconcat >> Unwrap) ) (M.fromList $ zip (map T.singleton "abcdefgh") [0 ..])
    --- have to wrap with W cos different kinds
--  , expectPE TrueT $ pl @(Do '[ W ('PresentT I), W 'FalseT, Not]) False
--  , expectPE FalseT $ pl @(Do '[ W ('PresentT Id), W 'FalseT ]) True -- have to wrap them cos BoolT a vs BoolT Bool ie different types
--  , expectPE TrueT $ pl @('PresentT I >> 'FalseT >> Not) False
  -- IxL "d" doesnt work cos is Text not String
  , expectPE (PresentT 3) $ pl @(Id !! KST _ "d") (M.fromList $ zip (map T.singleton "abcd") [0 ..])
  -- use Fromstring
  , expectPE (PresentT 3) $ pl @(Id !! ("d" >> FromStringP _)) (M.fromList $ zip (map T.singleton "abcd") [0 ..])
  , expectPE (PresentT [7,9,9,2,7,3,9,8,7,1,3]) $ pl @(Ones >> Map (ReadP Int) >> Guard "checkdigit fail" Luhn) "79927398713"
  , expectPE (FailT "checkdigit fail") $ pl @(Ones >> Map (ReadP Int) >> Guard "checkdigit fail" Luhn) "79927398714"
  , expectPE TrueT $ pl @(Ccip >> Ccop 11) "79927398713"
  , expectPE (FailT "expected 10 digits but found 11") $ pl @(Ccip >> Ccop 10) "79927398713"
  , expectPE (PresentT [10,14,15,9]) $ pl @(MM1 16 >> MM2 16) "aef9"
  , expectPE (FailT "invalid base 16") $ pl @(MM1 16 >> MM2 16) "aef9g"
  , expectPE (FailT "found empty") $ pl @(MM1 16 >> MM2 16) ""
  , expectPE (FailT "0<=x<n") $ pl @(MM2 16) [10,1,17,1,-3,7]
  , expectPE (PresentT ((10,'c'),True)) $ pl @Assocl (10,('c',True))
  , expectPE (PresentT (10,('c',True))) $ pl @Assocr ((10,'c'),True)
  , expectPE (PresentT 70) $ pl @(Luhn' 11) "79927398713"
  , expectPE (FailT "expected 71 mod 10 = 0 but found 1") $ pl @(Luhn' 11) "79927398714"

-- works but way to difficult: use Guard to do all the work
--  >pl @(((Rescan "([[:xdigit:]])" >> Map Snd >> (Id &&& Len)) &&& Len) >> Guard "notallmatched" ((Snd *** Id) >> Fst == Snd)) "134F"
-- have to check the length of the match vs input to see that are the same
  , expectPE (PresentT [1,3,4,15]) $ pl @(((Rescan "([[:xdigit:]])" >> Map (Snd >> OneP >> ReadBase Int 16)) &&& Id) >> Guard "notallmatched" ((Len *** Len) >> Fst == Snd) >> Fst) "134F"
  , expectPE (FailT "notallmatched") $ pl @(((Rescan "([[:xdigit:]])" >> Map (Snd >> OneP >> ReadBase Int 16)) &&& Id) >> Guard "notallmatched" ((Len *** Len) >> Fst == Snd) >> Fst) "134g"
  , expectPE (PresentT True) $ pl @(Foldmap SG.Any) [False,False,True,False]
  , expectPE (PresentT False) $ pl @(Foldmap SG.All) [False,False,True,False]
  , expectPE TrueT $ pl @(Ones >> Map (ReadP _) >> Luhn) "12345678903"
  , expectPE FalseT $ pl @(Ones >> Map (ReadP _) >> Luhn) "12345678904"
  , expectPE (FailT "incorrect number of digits found 10 but expected 11 in [1234567890]") $ pl @(Luhn' 11) "1234567890"
  , expectPE (PresentT ([1,2],[3,4,5,6,7,8])) $ pl @(Break (If (Gt 2) 'True (If (Gt 4) (Failt _ "ASfd") 'False)) Id) [1..8]
  , expectPE (PresentT ([1,2],[3,4,5,6,7,8])) $ pl @(Break (Case 'False '[Gt 2,Gt 4] '[ W 'True, Failt _ "ASfd"] Id) Id) [1..8]  -- case version
  , expectPE (FailT "ASfd") $ pl @(Break (If (Gt 2) (Failt _ "ASfd") 'False) Id) [1..8]
  , expectPE (PresentT ([(1,False),(2,False),(3,False)],[(4,True),(5,True),(6,False)])) $ pl @(Break Snd Id) (zip [1..] [False,False,False,True,True,False])
  , expectPE (PresentT ([(1,False),(2,False),(3,False),(4,False)],[])) $ pl @(Break Snd Id) (zip [1..] [False,False,False,False])
  , expectPE (PresentT ([],[(1,True),(2,True),(3,True),(4,True)])) $ pl @(Break Snd Id) (zip [1..] [True,True,True,True])
  , (@?=) (Just "abc") ((_FailT # "abc") ^? _FailT)
  , (@?=) (Just ()) ((_TrueT # ()) ^? _TrueT)
  , (@?=) (Just ()) ((_FalseT # ()) ^? _FalseT)
  , (@?=) (Just 'x') ((_PresentT # 'x') ^? _PresentT)
  , expectPE (PresentT (111,'b')) $ pl @('(123,Char1 "c") >> (Sub Id 12 *** Pred)) ()
  , expectPE (PresentT (SG.Min 19)) $ pl @(((12 >> FromInteger _) &&& Id) >> Fst + Snd) (SG.Min 7)
  , expectPE (PresentT (SG.Product 84)) $ pl @(((12 >> FromInteger _) &&& Id) >> Sapa) (SG.Product 7)
  , expectPE (PresentT (123,((),()))) $ pl @(TupleI '[W 123,()]) 99
  , expectPE (PresentT (4,(5,(6,(7,()))))) $ pl @(TupleI '[4,5,6,7]) 99
  , expectPE (PresentT ("ss",(4,(SG.Min 9223372036854775807,())))) $ pl @(TupleI '[W "ss",W 4,MemptyT (SG.Min Int)]) 99
  , expectPE (PresentT ("ss",(4,(SG.Sum 0,())))) $ pl @(TupleI '[W "ss",W 4,MemptyT (SG.Sum _)]) 99
  , expectPE (PresentT "xyxyxyxy") $ pl @(STimes Fst Snd) (4,['x','y'])
  , expectPE (PresentT (concat (replicate 16 "abc"))) $ pl @(Repeat 4 Id ((Id &&& Id) >> Sapa)) "abc"
  , expectPE (PresentT (concat (replicate 4 "abc"))) $ pl @(STimes Fst Snd) (4,"abc")
  , expectPE (PresentT (concat (replicate 4 "abc"))) $ pl @(STimes 4 Id) "abc"
  , expectPE (PresentT "abcd") $ pl @(Map FromEnum >> Map (ToEnum Char)) ("abcd" :: String)
  , expectPE
      (FailT "ToEnum IO e=Prelude.Enum.Ordering.toEnum: bad argument(2)")
       $ pl @(Map FromEnum >> Map (Sub Id 97 >> ToEnum Ordering)) ("abcde" :: String)
  , expectPE (PresentT ([2,3,5,7,11,13], [1,4,6,8,9,10,12,14,15])) $ pl @(Partition Prime Id) [1..15]
  , expectPE (PresentT Nothing) $ pl @Nothing' (Nothing @Int)
  , expectPE (FailT "expected Nothing") $ pl @Nothing' (Just 10)
  , expectPE (FailT "'Nothing found Just") $ pl @'Nothing (Just 12)
  , expectPE (PresentT (Just 10,((),()))) $ pl @(Id &&& '() &&& ()) (Just 10)
  , expectPE (PresentT [(-999) % 1,10 % 1,20 % 1,(-999) % 1,30 % 1]) $ pl @(Map (Wrap (MM.First _) Id &&& (Pure Maybe (Neg 999) >> Wrap (MM.First _) Id)) >> Map Sapa >> Map (Unwrap >> Just')) [Nothing,Just 10,Just 20,Nothing,Just 30]
  , expectPE (PresentT 12) $ pl @(MaybeIn 99 Id) (Just 12)
  , expectPE (PresentT 12) $ pl @(JustDef 99 Id) (Just 12)
  , expectPE (PresentT 99) $ pl @(MaybeIn 99 Id) Nothing
  , expectPE (PresentT 99) $ pl @(JustDef 99 Id) Nothing
  , expectPE (PresentT (-99)) $ pl @(MaybeIn (Neg 99) Id) Nothing
  , expectPE (PresentT (-99)) $ pl @(JustDef (Neg 99) Id) Nothing
  , expectPE (PresentT [1,2,3,4,12]) $ pl @(Para (RepeatT 5 (Guard "0-255" (Between 0 255)))) [1,2,3,4,12]
  , expectPE (FailT "0-255")
     $ pl @(Para (RepeatT 5 (Guard "0-255" (Between 0 255)))) [1,2,3,400,12]
  , expectPE (PresentT ["141","021","003","000"]) $ pl @(Para (RepeatT 4 (Printf "%03d"))) [141,21,3,0::Int]
  , expect3 (Right $ unsafeRefined3 [1,2,3,4] "001.002.003.004") $ eval3 @Ip4A @Ip4B @(Para (RepeatT 4 (Printf "%03d")) >> Intercalate '["."] Id >> Concat) ol "1.2.3.4"
  , expect3 (Right $ unsafeRefined3 [1,2,3,4] "abc__002__3__zzz") $ eval3 @Ip4A @Ip4B @(Para '[W "abc",Printf "%03d",Printf "%d",W "zzz"] >> Intercalate '["__"] Id >> Concat) ol "1.2.3.4"
  , expect3 (Right (unsafeRefined [1,2,3,4], "001.002.003.004")) $ eval3PX (Proxy @'(Ip4A, Ip4B, Para (RepeatT 4 (Printf "%03d")) >> Intercalate '["."] Id >> Concat, _)) ol "1.2.3.4"
  , expect3 (Right (unsafeRefined [1,2,3,4], "001.002.003.004")) $ eval3PX (mkProxy3E @Ip4A @Ip4B @(Para (RepeatT 4 (Printf "%03d")) >> Intercalate '["."] Id >> Concat)) ol "1.2.3.4"

  -- keep the original value
  , expect3
      (Right $ unsafeRefined3 ("1.2.3.4", [1,2,3,4]) "001.002.003.004")
      $ eval3 @(Id &&& Ip4A) @(Snd >> Ip4B) @(Snd >> Para (RepeatT 4 (Printf "%03d")) >> Intercalate '["."] Id >> Concat) ol "1.2.3.4"

  -- need to fill in the types for both even in ghci
  , expectPE (PresentT (Just (SG.Sum 10))) $ pl @(Coerce2 (SG.Sum Int)) (Just (10 :: Int))
  , expectPE (PresentT (Just (SG.Sum 0))) $ pl @(MemptyT2 (SG.Sum _)) (Just ())
  , expectPE (PresentT 13) $ pl @(Foldmap (SG.Sum _)) (Just 13)
  , expectPE (PresentT 55) $ pl @(Foldmap (SG.Sum _)) [1..10]
  , expectPE (PresentT [Just 1,Just 2,Just 3,Just 4]) $ pl @Sequence (Just [1..4])
  , expectPE (PresentT (Just (SG.Sum 20))) $ pl @(Pure2 SG.Sum) (Just 20)
  , expectPE (PresentT Nothing) $ pl @(Traverse (If (Gt 3) (Pure Maybe Id) (EmptyT Maybe))) [1..5]
  , expectPE (PresentT Nothing) $ pl @(Traverse (MaybeB (Le 3) Id)) [1..5]
  , expectPE (PresentT (Just [1,2,3,4,5])) $ pl @(Traverse (If (Gt 0) (Pure Maybe Id) (EmptyT Maybe))) [1..5]
  , expectPE (PresentT (Just [1,2,3,4,5])) $ pl @(Traverse (If (Gt 0) (Pure Maybe Id) (MkNothing _))) [1..5]
  , expectPE (PresentT (Just [1,2,3,4,5])) $ pl @(Traverse (MaybeB (Id >= 0) Id)) [1..5]
  , expectPE (PresentT Nothing) $ pl @(Traverse (MaybeB (Id <= 3) Id)) [1..5]

  , expectPE (FailT "Printf (IO e=printf: bad formatting char 's')")
     $ pl @(Printf "%-6s") (1234 :: Int)
  , expectPE (PresentT "0004d2") $ pl @(Printf "%06x") (1234 :: Int)
  , expectPE (PresentT (Left 123)) $ pl @(Pure (Either String) Id >> Swap) 123
  , expectPE (PresentT [13,2,1999]) $ pl @(Rescan DdmmyyyyR >> OneP >> Snd >> Map (ReadP Int)) "13-02-1999"
  , expectPE (PresentT [3,2,1999]) $ pl @(Rescan DdmmyyyyR >> OneP >> Snd >> Map (ReadP Int) >> Ddmmyyyyval) "03-02-1999"
  , expectPE (FailT "guard(1) month 13 is out of range") $ pl @(Rescan DdmmyyyyR >> OneP >> Snd >> Map (ReadP Int) >> Ddmmyyyyval) "12-13-1999"
  , expectPE (PresentT [[1],[2,3,4],[5,6,7,8],[9,10,11,12]]) $ pl @(SplitAts '[1,3,4]) [1..12]
  , expectPE (PresentT [[1,2,3],[4]]) $ pl @(SplitAts '[3,1,1,1] >> FilterBy (Null >> Not) Id) [1..4]
  , expectPE (PresentT 1) $ pl @(Msg (Len >> Printf "digits=%d") Head') [1..4]
  , expectPE (PresentT 10) $ pl @(Luhn' 4) "1230"
  , expectPE (FailT "expected 14 mod 10 = 0 but found 4") $ pl @(Luhn' 4) "1234"
  , expectPE (PresentT "lhs = 123 rhs = asdf") $ pl @(Printf2 "lhs = %d rhs = %s") (123::Int,"asdf"::String)
  , expectPE TrueT $ pl @(DirExists ".") ()
  , expectPE FalseT $ pl @(DirExists "xxy") ()
  , expectPE TrueT $ pl @(FileExists ".ghci") ()
  , expectPE FalseT $ pl @(FileExists "xxy") ()
  , expectPE TrueT $ pl @(IsInfix "ab" Id) "xyzabw"
  , expectPE FalseT $ pl @(IsInfix "aB" Id) "xyzAbw"
  , expectPE TrueT $ pl @(IsInfixI "aB" Id) "xyzAbw"
  , expectPE FalseT $ pl @(IsInfix "ab" Id) "xyzbaw"
  , expectPE TrueT $ pl @(IsPrefix "xy" Id) "xyzabw"
  , expectPE FalseT $ pl @(IsPrefix "ab" Id) "xyzbaw"
  , expectPE TrueT $ pl @(IsSuffix "bw" Id) "xyzabw"
  , expectPE FalseT $ pl @(IsSuffix "bw" Id) "xyzbaw"
  , expectPE TrueT $ pl @(IsInfix Fst Snd) ("ab","xyzabw")
  , expectPE TrueT $ pl @(IsInfix Fst Snd) ("ab","xyzabw")
  , expectPE (PresentT [1 % 1,(-3) % 2,(-3) % 1]) $ pl @'[Pos 1,NegR 3 2,Neg 3] ()
  , expectPE (PresentT [4, 7, 8, 9]) $ pl @'[4,7,8,9] ()
  , expectPE (PresentT ["aa","b","","ddd"]) $ pl @'["aa","b","","ddd"] ()
  , expectPE (PresentT 17) $ pl @(Do (RepeatT 4 (Add Id 4))) 1
  , expectPE (PresentT 24) $ pl @((Id <> Id) >> Unwrap) (SG.Sum 12)
  , expectPE (PresentT "abcdef") $ pl @(Fst <> (Snd >> Fst)) ("abc",("def",12))
  , expectPE (PresentT (SG.Sum 25)) $ pl @(Wrap _ 13 <> Id) (SG.Sum @Int 12)
  , expectPE (PresentT 23) $ pl @(Add Fst (Snd >> Last')) (10,[12,13])
  , expectPE (PresentT (-1,12)) $ pl @(DivMod (Sub 9 Fst) (Snd >> Last')) (10,[12,13])
  , expectPE (PresentT [True,False,False,True]) $ pl @(Para '[ W 'True, Ge 12, W 'False, Lt 2 ]) [1,2,-99,-999]
  , expectPE (FailT "Para: data elements(3) /= predicates(4)") $ pl @(Para '[ W 'True, Ge 12, W 'False, Lt 2 ]) [1,2,-99]
  , expectPE (PresentT [True, False, False]) $ pl @(ParaLax '[ W 'True, Ge 12, W 'False, Lt 2 ]) [1,2,-99]
  , expectPE (FailT "Para: data elements(7) /= predicates(4)") $ pl @(Para '[ W 'True, Ge 12, W 'False, Lt 2 ]) [1,2,-99,-999,1,1,2]
  , expectPE (FailT "guard(1) err 002") $ pl @(Guards (ToGuardsT (Printf2 "guard(%d) err %03d") '[ W 'True, Ge 12, W 'False, Lt 2 ])) [1,2,-99,-999]
  , expectPE (FailT "Guards: data elements(3) /= predicates(4)") $ pl @(Guards (ToGuardsT (Printf2 "guard(%d) err %03d") '[ W 'True, Ge 12, W 'False, Lt 2 ])) [1,2,-99]
  , expectPE (FailT "Guards: data elements(7) /= predicates(4)") $ pl @(Guards (ToGuardsT (Printf2 "guard(%d) err %03d") '[ W 'True, Ge 12, W 'True, Lt 2 ])) [1,22,-99,-999,1,1,2]
  , expectPE (PresentT [1,22,-99,-999,1,1,2]) $ pl @(GuardsLax (ToGuardsT (Printf2 "guard(%d) err %03d") '[ W 'True, Ge 12, W 'True, Lt 2 ])) [1,22,-99,-999,1,1,2]
  , expectPE (PresentT [1,22]) $ pl @(GuardsLax (ToGuardsT (Printf2 "guard(%d) err %03d") '[ W 'True, Ge 12, W 'True, Lt 2 ])) [1,22]
  , expectPE (PresentT [1,22,-99,-999]) $ pl @(Guards (ToGuardsT (Printf2 "guard(%d) err %03d") '[ W 'True, Ge 12, W 'True, Lt 2 ])) [1,22,-99,-999]
  , expectPE FalseT $ pl @(IsSuffix "bw" Id) "xyzbaw"
  , expectPE TrueT $ pl @(Fst /= Snd) ("ab","xyzabw")
  , expectPE FalseT $ pl @(Fst == Snd) ("ab","xyzabw")
  , expectPE (PresentT 157) $ pl @(Fst * (Snd >> Fst) + (Snd >> Snd) `Div` 2) (12,(13,3))
  , expectPE TrueT $ pl @(Fst >= Snd || Snd > 23 || NegR 12 5 <= (Fst >> ToRational)) (12,13)
  , expectPE (PresentT LT) $ pl @(Fst === Snd) (3,12)
  , expectPE TrueT $ pl @(Fst ==? Snd) ("aBc","AbC")
  , expectPE (PresentT EQ) $ pl @(Fst ===? Snd) ("aBc","AbC")
  , expectPE FalseT $ pl @(Fst == Snd) ("aBc","AbC")
  , expectPE (PresentT GT) $ pl @(Fst === Snd) ("aBc","AbC")
  , expectPE (PresentT LT) $ pl @(Snd === Fst) ("aBc","AbC")
  , expectPE TrueT $ pl @(Fst ==? Snd && Fst == Snd) ("Abc","Abc")
  , expectPE (PresentT (EQ,EQ)) $ pl @(Fst ===? Snd &&& Fst === Snd) ("abc","abc")
  , expectPE (PresentT "ask%dfas%kef00035 hey %") $ pl @(Printf "ask%%dfas%%kef%05d hey %%") (35 :: Int)
  , expectPE (PresentT 100) $ pl @(Id !! 2 !! 0) [[1..5],[10..14],[100..110]]
  , expectPE (FailT "(!!) index not found") $ pl @(Id !! 1 !! 7) [[1..5],[10..14],[100..110]]
  , expectPE (PresentT '2') $ pl @(IxL Id 1 (Char1 "x")) ("123" :: T.Text)
  , expectPE (PresentT 'x') $ pl @(IxL Id 15 (Char1 "x")) ("123" :: T.Text)
  , expectPE (FailT "someval int=45") $ pl @(Fail () (Printf "someval int=%d")) (45 :: Int)
  , expectPE (FailT "failing with 45") $ pl @(If (Gt 4) (Fail (T _) (Printf "failing with %d")) ()) 45
  , expectPE (PresentT 21) $ pl @(If (Gt 4) (Fail (T _) (Printf "failing with %d")) (Id * 7)) 3
  , expectPE (PresentT ["2","1"]) $ pl @(If (Gt 4) (Fail (T _) (Printf "failing with %d")) (Id * 7 >> ShowP >> Ones)) 3
  , expectPE (FailT "failing with 19") $ pl @(If (Gt 4) (Fail (T _) (Printf "failing with %d")) (Id * 7 >> ShowP >> Ones)) 19
  , expectPE (PresentT 31) $ pl @(Do (RepeatT 4 (Id + 7))) 3
  , expectPE (PresentT 9) $ pl @(Do (RepeatT 4 9)) ()
  , expectPE (PresentT 3) $ pl @(Do '[1,2,3]) ()
  , expectPE (PresentT "xy") $ pl @(Do (RepeatT 4 "xy")) 3
  , expectPE (PresentT ["xy","xy","xy","xy"]) $ pl @(RepeatT 4 "xy") 3
  , expectPE (PresentT (Proxy @'["xy","xy","xy","xy"])) $ pl @(Proxy (RepeatT 4 "xy")) 3
  , expectPE (PresentT (This @_ @() 'x')) $ pl @(MkThis () Id) 'x'
  , expectPE (PresentT (This @_ @() 'x')) $ pl @(MkThis () Fst) ('x',True)
  , expectPE (PresentT (That 'x')) $ pl @(MkThat () Id) 'x'
  , expectPE (PresentT (These 'x' True)) $ pl @(MkThese Id 'True) 'x'
  , expectPE (PresentT 123) $ pl @(MaybeIn 123 Id) (Nothing @Int)
  , expectPE (PresentT 9) $ pl @(MaybeIn 123 Id) (Just 9)
  , expectPE (PresentT [1,2,3]) $ pl @Just' (Just [1,2,3])
  , expectPE (FailT "expected Just") $ pl @Just' (Nothing @[Int])
  , expectPE (PresentT [1,2,3]) $ pl @Just' (Just [1,2,3])
  , expectPE (FailT "expected Just") $ pl @Just' (Nothing @[Int])
  , expectPE (PresentT (66788,26232)) $ pl @(Last' >> Id * 123 >> Dup >> (Pred *** (ShowP >> Rescan "(\\d{2})" >> MapF Snd >> Concat >> Concat >> ReadBase Int 16))) [12,13,543::Int]
  , expectPE (PresentT "d=009 s=ab") $ pl @(Printfn "d=%03d s=%s") (9::Int,("ab"::String,()))
  , expectPE (PresentT "d=009 s=ab c=x f=1.54") $ pl @(Printfn "d=%03d s=%s c=%c f=%4.2f") (9::Int,("ab"::String,('x',(1.54::Float,()))))
  , expectPE (FailT "Printfn(4)(IO e=printf: formatting string ended prematurely)") $ pl @(Printfn "d=%03d s=%s") (9::Int,("ab"::String,('x',(1.54::Float,()))))
  , expectPE (PresentT "lhs = 123 rhs = asdf c=120") $ pl @(Printf3 "lhs = %d rhs = %s c=%d") (123::Int,("asdf"::String,'x'))
  , expectPE (PresentT (1,('x',(True,())))) $ pl @(FstL Int &&& SndL Char &&& ThdL Bool &&& ()) (1,'x',True)
  , expectPE (PresentT (1,('x',(True,())))) $ pl @(FstL _ &&& SndL _ &&& ThdL _ &&& ()) (1,'x',True)
  , expectPE (PresentT (1,(1.4,("aaa",())))) $ pl @(FstL _ &&& SndL _ &&& ThdL _ &&& ()) (1,1.4,"aaa")
  , (@?=) ("xx",(True,('x',(1,())))) (reverseTupleC (1,('x',(True,("xx",())))))
  , expectPE (PresentT "hello d=12 z someval") $ pl @(TupleI '[W 12, Char1 "z", W "someval"] >> Printfn "hello d=%d %c %s") ()
  , expectPE (PresentT "ipaddress 001.002.003.004") $ pl @(TupleI '[1,2,3,4] >> Printfn "ipaddress %03d.%03d.%03d.%03d") ()
  , expectPE (PresentT (1,(2,(3,(4,()))))) $ pl @(TupleI '[1,2,3,4]) 4
  , expectPE (PresentT (4,(3,(2,(1,()))))) $ pl @(TupleI '[1,2,3,4] >> ReverseTupleN) 4
  , expectPE (PresentT (1,(2,(3,(4,()))))) $ pl @(TupleI '[1,2,3,4] >> ReverseTupleN >> ReverseTupleN) 4

  , expectPE (PresentT "001.002.003.004") $ pl @(Printfnt 4 "%03d.%03d.%03d.%03d") [1,2,3,4::Int]
  , expectPE (FailT "TupleList(4) is strict and has 1 extra element") $ pl @(Printfnt 4 "%03d.%03d.%03d.%03d") [1,2,3,4,5::Int]
  , expectPE (FailT "TupleList(4) no data left") $ pl @(Printfnt 4 "%03d.%03d.%03d.%03d") [1,2,3::Int]

  , expectPE (PresentT "001.002.003.004") $ pl @(PrintfntLax 4 "%03d.%03d.%03d.%03d") [1,2,3,4::Int]
  , expectPE (PresentT "001.002.003.004") $ pl @(PrintfntLax 4 "%03d.%03d.%03d.%03d") [1,2,3,4,5::Int]
  , expectPE (FailT "TupleListLax(4) no data left") $ pl @(PrintfntLax 4 "%03d.%03d.%03d.%03d") [1,2,3::Int]
  , expectPE (FailT "Pairs no data found") $ pl @Pairs ([] @())
  , expectPE (FailT "Pairs only one element found") $ pl @Pairs [1]
  , expectPE (PresentT [(1,2)]) $ pl @Pairs [1,2]
  , expectPE (PresentT [(1,2),(2,3)]) $ pl @Pairs [1,2,3]
  , expectPE (PresentT [(1,2),(2,3),(3,4)]) $ pl @Pairs [1,2,3,4]
  , expectPE (PresentT "1    2 3 004") $ pl @(PrintfntLax 4 "%d %4d %-d %03d") [1..10::Int]
  , expectPE (PresentT "2019-08-17") $ pl @(FormatTimeP "%Y-%m-%d") (read "2019-08-17" :: Day)
  , expectPE (PresentT (20,20)) $ pl @(Dup << Fst * Snd) (4,5)
  , expectPE (PresentT (20,20)) $ pl @(Fst * Snd >> Dup) (4,5)
  , expectPE (PresentT (These "xxx" 4)) $ pl @(Fst <$ Snd) (4,These "xxx" 'a')
  , expectPE (PresentT (This 'a')) $ pl @(Fst <$ Snd) (4,This @_ @String 'a')
  , expectPE (PresentT (Just 4)) $ pl @(Fst <$ Snd) (4,Just 'a')
  , expectPE (PresentT Nothing) $ pl @(Fst <$ Snd) (4,Nothing @Int)
  , expectPE (PresentT (Just 4)) $ pl @(Fst <* Snd) (Just 4,Just 'a')
  , expectPE (PresentT (Just 'a')) $ pl @(Fst *> Snd) (Just 4,Just 'a')
  , expectPE (PresentT ('x',('x',"someval"))) $ pl @Duplicate ('x',"someval")
  , expectPE (PresentT "someval") $ pl @Extract ('x',"someval")
  , expectPE (PresentT (Just "cdef")) $ pl @(Fst <|> Snd) (Just "cdef",Just "ab")
  , expectPE (PresentT "cdefab") $ pl @(Fst <|> Snd) ("cdef","ab"::String)
  , expectPE (PresentT (9,"abc")) $ pl @(I $ 9 $ "abc") (,)
  , expectPE (PresentT ("abc",9)) $ pl @(9 & "abc" & I) (,)
  , expectPE (PresentT "28") $ pl @(Fst $ Snd) (show . (7*),4)
  , expectPE (PresentT (12,"12")) $ pl @(Fst $ Snd $ (Snd >> ShowP)) ((,),12)
  , expectPE (PresentT (Just (This [1,2,3,4]))) $ pl @(ZipTheseF Fst Snd) (Just [1..4],Nothing @())
  , expectPE (PresentT [These 1 'a',These 2 'b',These 3 'c',This 4]) $ pl @(ZipTheseF Fst Snd) ([1..4],['a'..'c'])
  , expectPE (PresentT [True,True,True,True]) $ pl @('True <$ Id) [1..4]
  , expectPE (PresentT (Compose (Just "aaaa"))) $ pl @(Char1 "ab" <$ Id) (Compose $ Just [1..4])
  , expectPE (PresentT (4,("aa",'x'))) $ pl @'(4,'(Fst,Snd)) ("aa",'x')
  , expectPE (PresentT (4,"aa",'x')) $ pl @'(4,Fst,Snd) ("aa",'x')
  , expectPE (PresentT (Just [10])) $ pl @(Pure2 []) (Just 10)
  , expectPE (PresentT "hello") $ pl @Extract (10,"hello")
  , expectPE (PresentT (M.fromList [(4,"x"),(5,"dd")])) $ pl @(FromList (M.Map _ _)) [(4,"x"),(5,"dd")]
  , expectPE (PresentT False) $ pl @(FromList (M.Map _ _) >> I !! Char1 "y") [('x',True),('y',False)]
  -- FromListF works only if OverloadedLists is on
--  , expectPE (PresentT (M.fromList [(4,"x"),(5,"dd")])) $ pl @(FromListF (M.Map _ _)) [(4,"x"),(5,"dd")]
  , expectPE (PresentT (Just False)) $ pl @(FromList (M.Map _ _) >> Lookup Id (Char1 "y")) [('x',True),('y',False)]
  , expectPE (PresentT Nothing) $ pl @(FromList (M.Map _ _) >> Lookup Id (Char1 "z")) [('x',True),('y',False)]
  , expectPE (FailT "index('z') not found") $ pl @(FromList (M.Map _ _) >> (Char1 "z" &&& Lookup Id (Char1 "z")) >> If (Snd >> IsNothing) (Fst >> ShowP >> Fail I (Printf "index(%s) not found") >> 'False) (Snd >> 'Just Id)) [('x',True),('y',False)]
  , expectPE (PresentT True) $ pl @(FromList (M.Map _ _) >> (Char1 "z" &&& Lookup Id (Char1 "x")) >> If (Snd >> IsNothing) (Fst >> ShowP >> Fail I (Printf "index(%s) not found") >> 'False) (Snd >> 'Just Id)) [('x',True),('y',False)]
  , expectPE (FailT "index('z') not found") $ pl @(FromList (M.Map _ _) >> Lookup' _ Id (Char1 "z")) [('x',True),('y',False)]
  , expectPE (PresentT [[1,2],[3,4],[5]]) $ pl @(Unfoldr (If Null (MkNothing _) (Pure _ (SplitAt 2 Id))) Id) [1..5]
  , expectPE (PresentT [[1,2],[3,4],[5]]) $ pl @(Unfoldr (MaybeB (Null >> Not) (SplitAt 2 Id)) Id) [1..5]
  , expectPE (PresentT [99,1,2,3,4,5]) $ pl @(FlipT (:+) Fst Snd) ([1..5],99)
  , expectPE (PresentT [99,1,2,3,4,5]) $ pl @(Fst :+ Snd) (99,[1..5])
  , expectPE (PresentT [[99],[1,99],[2,1,99],[3,2,1,99],[4,3,2,1,99],[5,4,3,2,1,99]]) $ pl @(Scanl (Snd :+ Fst) Fst Snd) ([99],[1..5])
  , expectPE (PresentT [[99]]) $ pl @(Scanl (Snd :+ Fst) Fst Snd) ([99],[])
  , expectPE (FailT "yy") $ pl @(Unfoldr (If Null (MkNothing _) (Guard "yy" (Len >> Id < 3) >> Pure _ (SplitAt 2 Id))) Id) [1..5]
  , expectPE (FailT "yy") $ pl @(Unfoldr (MaybeB (Null >> Not) (Guard "yy" (Len >> Id < 3) >> SplitAt 2 Id)) Id) [1..5]
  , expectPE (PresentT [4,1,2,3]) $ pl @(4 :+ '[1,2,3]) ()
  , expectPE (PresentT [1,2,3,4]) $ pl @('[1,2,3] +: 4) ()
  , expectPE (PresentT [4,1,2,3]) $ pl @(Fst :+ Snd) (4,[1,2,3])
  , expectPE (PresentT [1,2,3,4]) $ pl @(Snd +: Fst) (4,[1,2,3])
  , expectPE (PresentT "abcx") $ pl @("abc" +: Char1 "x") ()
  , expectPE (PresentT "abcx") $ pl @(Fst +: Snd) ("abc" :: T.Text,'x')
  , expectPE (PresentT [5,1,2,3]) $ pl @(FlipT (:+) '[1,2,3] 5) ()
  , expectPE (PresentT (map ModifiedJulianDay [0,1,2,3,4,5])) $ pl @(EnumFromTo Fst Snd) (ModifiedJulianDay 0, ModifiedJulianDay 5)
  , expectPE (PresentT (map ModifiedJulianDay [0,1,2,3,4,5])) $ pl @((ToEnum Day *** ToEnum Day) >> EnumFromTo Fst Snd) (0,5)
  , expectPE (FailT "xx") $ pl @(Unfoldr (Guard "xx" (Len >> Id > 4) >> Uncons) Id) [1..10]
  , expectPE (PresentT [1,2,3,4,5,6,7,8,9,10]) $ pl @(Unfoldr Uncons Id) [1..10]
  , expectPE (PresentT [99,98,97,96]) $ pl @(IterateN 4 Pred) 99
  , expectPE (PresentT [[1,2],[3,4],[5]]) $ pl @(Unfoldr (MaybeB (Null >> Not) (SplitAt 2 Id)) Id) [1..5]
  , expectPE (PresentT (4,'x')) $ pl @('(,) 4 % Char1 "x") ()
  , expectPE (PresentT (Just False)) $ pl @(FromList (M.Map _ _) >> Lookup Id % Char1 "y") [('x',True),('y',False)]
  , expectPE (PresentT (4,"abc")) $ pl @('(,) % 4 % "abc") ()
  , expectPE (PresentT ("abc",4)) $ pl @(4 %& "abc" %& '(,)) ()
  , expectPE (PresentT ("abc",4)) $ pl @(FlipT '(,) 4 "abc") ()
  , expectPE (PresentT (1,[])) $ pl @(Uncons >> MaybeIn '(1,MemptyT _) Id) []
  , expectPE (PresentT []) $ pl @'[] 4
  , expectPE (PresentT (SG.Sum 3)) $ pl @(Fst >> FromInteger (SG.Sum _)) (3,"A")
  , expectPE (PresentT (123 :: DiffTime)) $ pl @(123 >> FromInteger DiffTime) 'x'
  , expectPE (PresentT (0.8 :: Float)) $ pl @(PosR 4 5 >> FromRational Float) ()
  , expectPE (PresentT (14 % 1)) $ pl @(14 >> ToRational) ()
  , expectPE (PresentT ('y',3)) $ pl @(Id !! 1) [('x',14),('y',3),('z',5)]
  , expectPE (PresentT ('y',3)) $ pl @(Id !!! 1) [('x',14),('y',3),('z',5)]
  , expectPE (PresentT (Just ('y',3))) $ pl @(Lookup Id 1) [('x',14),('y',3),('z',5)]
  , expectPE (PresentT Nothing) $ pl @(Lookup Id 14) [('x',14),('y',3),('z',5)]
  , expectPE (PresentT ('y',3)) $ pl @(Lookup' _ Id 1) [('x',14),('y',3),('z',5)]
  , expectPE (FailT "index(14) not found") $ pl @(Lookup' _ Id 14) [('x',14),('y',3),('z',5)]
  , expectPE (PresentT 99) $ pl @(FstL _) (99,'a',False,"someval","abc",1.3)
  , expectPE (PresentT 'a') $ pl @(SndL _) (99,'a',False,"someval","abc",1.3)
  , expectPE (PresentT False) $ pl @(ThdL _) (99,'a',False,"someval","abc",1.3)
  , expectPE (PresentT "someval") $ pl @(FthL _) (99,'a',False,"someval","abc",1.3)
  , expectPE (PresentT [1,-5,5,-1]) $ pl @('[Pos 1,Negate << PosR 33 7, PosR 21 4,NegR 7 5 >> Signum] >> Map (Floor _)) ()
  , expectPE (PresentT [1,-4,6,-1]) $ pl @('[Pos 1,Negate << PosR 33 7, PosR 21 4,NegR 7 5 >> Signum] >> Map (Ceiling _)) ()
  , expectPE (PresentT [1,-4,5,-1]) $ pl @('[Pos 1,Negate << PosR 33 7, PosR 21 4,NegR 7 5 >> Signum] >> Map (Truncate _)) ()
  , expectPE (PresentT @Integer 2) $ pl @(Truncate' (Fst >> Unproxy ) Snd) (Proxy @Integer,2.3)
  , expectPE (PresentT @Int 2) $ pl @(Truncate' Fst Snd) (1::Int,2.3)
  , expectPE (PresentT @Float 0.4) $ pl @(FromRational' Fst Snd) (1::Float,2 % 5)
  , expectPE (PresentT (5 % 3)) $ pl @((ToRational << 5) / (ToRational << 3)) 'x'
  , expectPE (PresentT (-5 % 3)) $ pl @(Pos 5 / Neg 3) 'x'
  , expectPE (PresentT (-5 % 3)) $ pl @(Neg 5 / Fst) (3,'x')
  , expectPE (PresentT (-5 % 3)) $ pl @(Snd / Fst) (-3,5)
  , expectPE (FailT "DivF zero denominator") $ pl @(Snd / Fst) (0,5)
  , expectPE (PresentT 16) $ pl @(Foldl (Guard "someval" (Fst < Snd) >> Snd) Head' Tail) [1,4,7,9,16]
  , expectPE (FailT "7 not less than 6") $ pl @(Foldl (Guard (Printf2 "%d not less than %d") (Fst < Snd) >> Snd) Head' Tail) [1,4,7,6,16::Int]
  , expectPE (PresentT (True,16)) $ pl @(Foldl (If ((Fst >> Fst) && (Snd > (Fst >> Snd))) '( 'True, Snd ) '( 'False, Fst >> Snd )) '( 'True, Head' ) Tail) [1,4,7,9,16]
  , expectPE (PresentT (False,16)) $ pl @(Foldl (If ((Fst >> Fst) && (Snd > (Fst >> Snd))) '( 'True, Snd ) '( 'False, Fst >> Snd )) '( 'True, Head' ) Tail) [1,4,7,9,16,2]
  , expectPE (PresentT (False,7))
     $ pl @(Foldl (If (Fst >> Fst)
                    (If (Snd > (Fst >> Snd))
                       '( 'True, Snd )
                       '( 'False, Fst >> Snd )
                    ) Fst)
                   '( 'True, Head' ) Tail) [1,4,7,6,16]
  , expectPE (PresentT [1,2,3,4]) $ pl @Init' [1..5]
  , expectPE (FailT "Init(empty)") $ pl @Init' ([] @())
  , expectPE (PresentT [2,3,4,5]) $ pl @Tail' [1..5]
  , expectPE (FailT "Tail(empty)") $ pl @Tail' ([] @())
  , expectPE (PresentT [10,12,13]) $ pl @CatMaybes [Just 10, Just 12, Nothing, Just 13]
  , expectPE (PresentT [5,4,3,2,1]) $ pl @(Foldl (Snd :+ Fst) (MemptyT [_]) Id) [1..5]
  , expectPE (PresentT (map SG.Min [9,10,11,12,13])) $ pl @(EnumFromTo (Pure SG.Min 9) (Pure _ 13)) ()
  , expectPE (PresentT (map SG.Min [9,10,11,12,13])) $ pl @(EnumFromTo (Wrap (SG.Min _) 9) (Wrap _ 13)) ()
--  , expectPE (PresentT (Just 'x')) $ pl @(Purex Fst Snd) (Just 10,'x')
  , expectPE (PresentT (Just 'x')) $ pl @(Snd <$ Fst) (Just 10,'x')
  , expectPE (PresentT (Nothing @(SG.Sum _))) $ pl @(MemptyT' Id) (Just (SG.Sum 12))
  , expectPE (PresentT ([4,99],"xy")) $ pl @PartitionEithers [Left 4, Right 'x', Right 'y',Left 99]
  , expectPE (PresentT ([(3,'b'),(5,'x')], ([4,99],"xy"))) $ pl @PartitionThese [This 4, That 'x', That 'y',These 3 'b', This 99, These 5 'x']
  , expectPE (PresentT [1,2,3]) $ pl @(MapMaybe (MaybeB (Le 3) Id)) [1..5]
  , expectPE (PresentT [4,5]) $ pl @(MapMaybe (MaybeB (Gt 3) Id)) [1..5]
  , expectPE (PresentT [94,93,92,91]) $ pl @(IterateWhile (Id > 90) Pred) 94
  , expectPE (PresentT [94,93,92,91,90]) $ pl @(IterateUntil (Id < 90) Pred) 94
  , expectPE (PresentT [95,94,93,92,91]) $ pl @(IterateNWhile 10 (Id > 90) Pred) 95
  , expectPE (PresentT [95,94,93]) $ pl @(IterateNWhile 3 (Id > 90) Pred) 95
  , expectPE (PresentT [95,94,93,92,91]) $ pl @(IterateNUntil 10 (Id <= 90) Pred) 95
  , expectPE (PresentT [95,94,93]) $ pl @(IterateNUntil 3 (Id <= 90) Pred) 95
  -- check for infinite loops
  , expectPE (FailT "Unfoldr (9999,1):failed at i=100") $ pl @(IterateNUntil 9999 'False I) 1
  , expectPE (FailT "Scanl:failed at i=100") $ pl @(Foldl Fst '() (EnumFromTo 1 9999)) ()
  , expectPE (PresentT [1,2,3,4,5,99]) $ pl @(MaybeX Fst ((Fst >> Fst) +: Snd) Snd) ([1..5],Just 99)
  , expectPE (PresentT [1,2,3,4,5]) $ pl @(MaybeX Fst ((Fst >> Fst) +: Snd) Snd) ([1..5],Nothing)
  , expectPE (PresentT "a=9 b=rhs") $ pl @(TheseX (Snd >> Succ >> Printf "a=%d") ("b=" <> Snd) (Snd >> Printf2 "a=%d b=%s") Id) (These @Int 9 "rhs")
  , expectPE (PresentT "a=10") $ pl @(TheseX (Snd >> Succ >> Printf "a=%d") ("b=" <> Snd) (Snd >> Printf2 "a=%d b=%s") Id) (This @Int 9)
  , expectPE (PresentT "b=rhs") $ pl @(TheseX (Snd >> Succ >> Printf "a=%d") ("b=" <> Snd) (Snd >> Printf2 "a=%d b=%s") Id) (That @Int "rhs")
  , expectPE (PresentT ([] @Int)) $ pl @HeadP (map (:[]) ([] @Int))
  , expectPE (PresentT ([10] :: [Int])) $ pl @HeadP (map (:[]) ([10..14] :: [Int]))
  , expectPE (PresentT 10) $ pl @(HeadDef' Fst Snd) (99,[10..14])
  , expectPE (PresentT 99) $ pl @(HeadDef' Fst Snd) (99,[] @Int)
  , expectPE (PresentT 43) $ pl @(HeadDef' 43 Snd) (99,[] @Int)
  , expectPE (PresentT (Just 'd')) $ pl @(Lookup "abcdef" 3) ()
  , expectPE (PresentT (Just 5)) $ pl @(Lookup '[1,2,3,4,5,6] 4) ()
  , expectPE (PresentT 5) $ pl @(LookupDef '[1,2,3,4,5,6] 4 Id) 23
  , expectPE (PresentT 5) $ pl @(LookupDef '[1,2,3,4,5,6] 4 Fst) (23,'x')
  , expectPE (PresentT 23) $ pl @(LookupDef '[1,2,3,4,5,6] 99 Id) 23
  , expectPE (PresentT 23) $ pl @(LookupDef '[1,2,3,4,5,6] 99 Fst) (23,'x')
  , expectPE (PresentT 5) $ pl @(LookupDef '[1,2,3,4,5,6] 4 999) (23,'x')
  , expectPE (PresentT 999) $ pl @(LookupDef '[1,2,3,4,5,6] 40 999) (23,'x')
  , expectPE (PresentT (SG.Min 5)) $ pl @(LookupP' Fst 4) (map SG.Min [1::Int .. 10],'x')
  , expectPE (PresentT (mempty @(SG.Min _))) $ pl @(LookupP' Fst 999) (map SG.Min [1::Int .. 10],'x')
  , expectPE (FailT "someval") $ pl @(LookupFail "someval" Fst 999) (map SG.Min [1::Int .. 10],'x')
  , expectPE (FailT "abcsomeval") $ pl @(Fail (Snd >> Unproxy) (Fst <> "someval")) ("abc",Proxy @Int)
  , expectPE (FailT "char=x") $ pl @(LookupFail (Snd >> Printf "char=%c") Fst 49) (map SG.Min [1::Int ..10],'x')
  , expectPE (FailT "someval=13") $ pl @(LeftFail (Fst >> Printf "someval=%d") Snd) (13::Int,Right @(SG.Sum Int) "abc")
  , expectPE (FailT "someval=Right \"abc\"") $ pl @(LeftFail (ShowP >> Printf "someval=%s") Id) (Right @(SG.Sum Int) "abc")
  , expectPE (FailT "msg=(\"ASfd\",[]) Asdf") $ pl @(GFail Uncons (ShowP >> Printf "msg=%s Asdf") Snd) ("ASfd",[]::[Int])
  , expectPE (PresentT 'c') $ pl @(LookupDef' Fst Snd (Char1 "xx") Id) (['a'..'e'],2)
  , expectPE (PresentT 'x') $ pl @(LookupDef' Fst Snd (Char1 "xx") Id) (['a'..'e'],999)
  , expectPE (PresentT 'x') $ pl @(LookupDef' Fst Snd (Char1 "xx") Id) ([],2)
  , expectPE (PresentT 'x') $ pl @(LookupDef' Fst Snd (Char1 "xx") Snd) ('w',([],2))
  , expectPE (PresentT 'c') $ pl @(LookupDef' Fst Snd Fst Snd) ('x',(['a'..'e'],2))
  , expectPE (PresentT(SG.Min 13)) $ pl @(LookupP'' Fst Snd Snd) ('x',(map SG.Min [10..15::Int], 3))

  , expectPE (PresentT 9) $ pl @(HeadDef' 9 Fst) ([],True)
  , expectPE (PresentT 1) $ pl @(HeadDef' 9 Fst) ([1..5],True)
  , expectPE (PresentT 10) $ pl @(HeadDef' 3 Fst) ([10..15],True)

  , expectPE (PresentT 9) $ pl @(LastDef' 9 Fst) ([],True)
  , expectPE (PresentT 5) $ pl @(LastDef' 9 Fst) ([1..5],True)
  , expectPE (PresentT 15) $ pl @(LastDef' 3 Fst) ([10..15],True)

  , expectPE (PresentT [9,7]) $ pl @(InitDef' '[9,7] Fst) ([],True)
  , expectPE (PresentT [1,2,3,4]) $ pl @(InitDef' '[9,7] Fst) ([1..5],True)
  , expectPE (PresentT [10,11,12,13,14]) $ pl @(InitDef' '[3] Fst) ([10..15],True)
  , expectPE (PresentT [10,11,12,13,14]) $ pl @(InitP' Fst) ([10..15],True)
  , expectPE (PresentT []) $ pl @(InitP' Fst) ([] @Int,True)

  , expectPE (PresentT [9,7]) $ pl @(TailDef' '[9,7] Fst) ([],True)
  , expectPE (PresentT [2,3,4,5]) $ pl @(TailDef' '[9,7] Fst) ([1..5],True)
  , expectPE (PresentT [11,12,13,14,15]) $ pl @(TailDef' '[3] Fst) ([10..15],True)
  , expectPE (PresentT [11,12,13,14,15]) $ pl @(TailP' Fst) ([10..15],True)
  , expectPE (PresentT []) $ pl @(TailP' Fst) ([] @Int,True)

  , expectPE (FailT "a=4 b=someval") $ pl @(TailFail (Snd >> Printf2 "a=%d b=%s") Fst) ([]::[()],(4::Int,"someval" :: String))
  , expectPE (PresentT 3) $ pl @(JustDef' 44 (Fst >> Fst >> Fst) Snd) (3,Just 20)
  , expectPE (PresentT 999) $ pl @(JustDef' 44 999 Snd) ("xxx",Just 20)
  , expectPE (PresentT "xxabcd") $ pl @(JustDef' "dd" ((Fst >> Fst >> Fst) <> Snd) Snd) ("xx",Just "abcd")
  , expectPE (PresentT "dd") $ pl @(JustDef' "dd" ((Fst >> Fst >> Fst) <> Snd) Snd) ("xx",Nothing)
  , expectPE (PresentT "xx") $ pl @(JustDef' Fst ((Fst >> Fst >> Fst) <> Snd) Snd) ("xx",Nothing)

  , expectPE (PresentT 3) $ pl @(JustDef' 44 (Fst >> Fst >> Snd) Fst) (Just 20,3)
  , expectPE (PresentT 999) $ pl @(JustDef' 44 999 Fst) (Just 20,"xxx")
  , expectPE (PresentT "xxabcd") $ pl @(JustDef' "dd" ((Fst >> Fst >> Snd) <> Snd) Fst) (Just "abcd","xx")
  , expectPE (PresentT "dd")   $ pl @(JustDef' "dd" ((Fst >> Fst >> Snd) <> Snd) Fst) (Nothing,"xx")
  , expectPE (PresentT "xx")   $ pl @(JustDef' Snd ((Fst >> Fst >> Snd) <> Snd) Fst) (Nothing,"xx")

  , expectPE (PresentT "xxya") $ pl @((Id &&& Snd) >> MaybeXP (Fst >> Fst >> Fst) ((Fst >> Fst >> Fst) <> Snd) Snd) ("xx",Just "ya")
  , expectPE (PresentT "xxya") $ pl @((Id &&& Fst) >> MaybeXP (Fst >> Fst >> Snd) ((Fst >> Fst >> Snd) <> Snd) Snd) (Just "ya","xx")

  , expectPE (PresentT "xx") $ pl @((Id &&& Snd) >> MaybeXP (Fst >> Fst >> Fst) ((Fst >> Fst >> Fst) <> Snd) Snd) ("xx",Nothing)

  , expectPE (PresentT "aabb") $ pl @(JustDef''' Fst ((Fst >> Fst) <> Snd) Snd) ("aa", Just "bb")
  , expectPE (PresentT "aa") $ pl @(JustDef''' Fst ((Fst >> Fst) <> Snd) Snd) ("aa", Nothing)
  , expectPE (PresentT "ssbb") $ pl @(JustDef''' Fst ("ss" <> Snd) Snd) ("aa", Just "bb")

  , expectPE (PresentT (Just 1)) $ pl @Fmap_1 (Just (1,'x'))
  , expectPE (PresentT (Just 'x')) $ pl @Fmap_2 (Just (1,'x'))
  , expectPE (PresentT (Nothing @Int)) $ pl @Fmap_2 (Nothing @(Char,Int))
  , expectPE (PresentT [1,2,3]) $ pl @Fmap_1 [(1,'x'), (2,'y'), (3,'z')]
  , expectPE (PresentT (Right 'x')) $ pl @Fmap_2 (Right @() (1,'x'))
  , expectPE (PresentT (Left @_ @Double "x")) $ pl @Fmap_2 (Left @_ @(Int,Double) "x")

  , expectPE (PresentT [1,10,99]) $ pl @Thiss [This 1, This 10,That 'x', This 99, That 'y']
  , expectPE (PresentT "xy") $ pl @Thats [This 1, This 10,That 'x', This 99, That 'y']
  , expectPE (PresentT ("xabz",[1,10])) $ pl @PartitionEithers [Left 'x', Right 1,Left 'a', Left 'b',Left 'z', Right 10]
  , expectPE (FailT "found rhs=Right 10") $ pl @(LeftFail (ShowP >> Printf "found rhs=%s") Id) (Right @String 10)
  , expectPE (FailT "found rhs=23") $ pl @(LeftFail (Snd >> Snd >> Printf "found rhs=%d") (Snd >> Fst)) ('x',(Right @() 10,23::Int))
  , expectPE (PresentT "abc") $ pl @(LeftFail (Snd >> Snd >> Printf "found rhs=%d") (Snd >> Fst)) ('x',(Left @_ @() "abc",23::Int))
  , expectPE (PresentT ([(9,'z'),(8,'y')], ([1,4,10],"xy"))) $ pl @PartitionThese [This 1,That 'x',This 4,That 'y',These 9 'z',This 10,These 8 'y']
  , expectPE (PresentT [('a',1),('a',10),('z',14),('m',22)]) $ pl @(SortOn Snd Snd) ((),[('z',14),('a',10),('m',22),('a',1)])
  , expectPE (PresentT [('z',1),('m',22),('a',10)]) $ pl @(SortOnDesc Fst Snd) ((),[('z',1),('a',10),('m',22)])
  , expectPE (PresentT [('a',10),('m',22),('z',1)]) $ pl @(SortOn Fst Snd) ((),[('z',1),('a',10),('m',22)])
  , expectPE (PresentT [('z',1),('m',22),('a',10)]) $ pl @(SortBy (Swap >> OrdA Fst) Snd) ((),[('z',1),('a',10),('m',22)])
  , expectPE (PresentT ["aa","cx","by","az"]) $ pl @(SortBy (OrdA Reverse) Id) ["az","by","cx","aa"]
  , expectPE (PresentT [('a',10),('a',9),('m',22),('m',10),('z',1)]) $ pl @(SortOn Fst Id) [('z',1),('a',10),('m',22),('a',9),('m',10)]
  , expectPE (PresentT [('a',9),('a',10),('m',10),('m',22),('z',1)]) $ pl @(SortOn Id Id) [('z',1),('a',10),('m',22),('a',9),('m',10)]
  , expectPE (PresentT (False,9)) $ pl @(Uncons >> Just' >> Foldl (If (Fst>>Fst) (If ((Fst>>Snd) < Snd) '( 'True,Snd ) '( 'False,Snd)) Fst) '( 'True,Fst) Snd) [-10,-2,2,3,4,10,9,11]
  , expectPE (PresentT (True,11)) $ pl @(Uncons >> Just' >> Foldl (If (Fst>>Fst) (If ((Fst>>Snd) < Snd) '( 'True,Snd ) '( 'False,Snd)) Fst) '( 'True,Fst) Snd) [-10,2,3,4,10,11]
  , expectPE (FailT "pivot=5 value=3(2)") $ pl @(SortBy (If (Fst==5 && Snd==3) (FailPrt2 _ "pivot=%d value=%d") 'GT) Snd) ((), [5,7,3,1,6,2,1,3])
  , expectPE (PresentT [1,1,2,3,3,5,6,7]) $ pl @(SortBy (If (Fst==50 && Snd==3) (FailPrt2 _ "pivot=%d value=%d") (OrdA Id)) Snd) ((), [5,7,3,1,6,2,1,3])
  , expectPE TrueT $ pl @(Between' (Fst >> Fst) (Fst >> Snd) Snd) ((1,4),3)
  , expectPE FalseT $ pl @(Between' (Fst >> Fst) (Fst >> Snd) Snd) ((1,4),10)
  , expectPE (FailT "no match on [03/29/0x7]") $ pl @(Map (ParseTimes Day '["%Y-%m-%d", "%m/%d/%y", "%b %d %Y"] Id)) ["2001-01-01", "Jan 24 2009", "03/29/0x7"]
  , expectPE (PresentT [read @Day "2001-01-01", read @Day "2009-01-24", read @Day "2007-03-29"]) $ pl @(Map (ParseTimes Day '["%Y-%m-%d", "%m/%d/%y", "%b %d %Y"] Id)) ["2001-01-01", "Jan 24 2009", "03/29/07"]
  , (@?=) [(unsafeRefined3 255 "ff", "")] (reads @(Refined3 (ReadBase Int 16) 'True (ShowBase 16) String) "\"0ff\"") -- escape quotes cos read instance for String
  , (@?=) [] (reads @(Refined3 (ReadBase Int 16) 'True (ShowBase 16) String) "\"0fz\"") -- escape quotes cos read instance for String
  , (@?=) [(unsafeRefined 7, "")] (reads @(Refined (Between 2 10) Int) "7")
  , (@?=) [] (reads @(Refined (Between 2 10) Int) "0")
  , (@?=) [(unsafeRefined "abcaaaabb", "")] (reads @(Refined (Re "^[abc]+$") String) "\"abcaaaabb\"")
  , (@?=) [] (reads @(Refined (Re "^[abc]+$") String) "\"abcaaaabbx\"")

  , expectJ (Right (G4 (unsafeRefined3 12 "12") (unsafeRefined3 [1,2,3,4] "001.002.003.004"))) (toFrom $ G4 (unsafeRefined3 12 "12") (unsafeRefined3 [1,2,3,4] "1.2.3.4"))
  , expectJ (Left ["Error in $.g4Ip", "False Boolean Check"]) (toFrom $ G4 (unsafeRefined3 12 "12") (unsafeRefined3 [1,2,3,4] "1.2.3.400"))
  , expectJ (Left ["Error in $.g4Ip", "ReadP Int (3x)"]) (toFrom $ G4 (unsafeRefined3 12 "12") (unsafeRefined3 [1,2,3,4] "1.2.3x.4"))
  , expectJ (Left ["Error in $.g4Age", "False Boolean Check"]) (toFrom $ G4 (unsafeRefined3 (-2) "-2") (unsafeRefined3 [1,2,3,4] "1.2.3.4"))
  , expectJ (Left ["Error in $: Refined:FalseP"]) (toFrom (unsafeRefined @(Between 4 7 || Gt 14) 12))
  , expectJ (Right (unsafeRefined 22)) (toFrom (unsafeRefined @(Between 4 7 || Gt 14) 22))
  , expectJ (Left ["Error in $: Refined:FailP \"someval\""]) (toFrom (unsafeRefined @(Between 4 7 || Gt 14 || Failt _ "someval") 12))
  , expectPE (PresentT ["abc","bcd","cde","def","efg","fgh","ghi","hi","i"]) $ pl @(Unfoldr (If Null (MkNothing _) ('(Take 3 , Drop 1) >> MkJust)) Id) "abcdefghi"
  , expectRight (testRefined3P (Proxy @(Ccn '[4,4,3])) ol "123-45-6---789-03-")
  , expectLeft (testRefined3P (Proxy @(Ccn '[4,4,3])) ol "123-45-6---789-04-")
  , expectRight (testRefined3P (Proxy @Hms) ol "1:2:33")
  , expectLeft (testRefined3P (Proxy @Hms) ol "1:2:61")
  , expectRight (testRefined3P (Proxy @(Ccn '[4,4,3])) ol "6433-1000-006")
  , expectRight (testRefined3P (Proxy @(Ccn '[4,4,3])) ol "6433-10000-06")
  , expectLeft (testRefined3P (Proxy @(Ccn '[4,4,3])) ol "6433-1000-000")
  , expectRight (testRefined3P (Proxy @(Ccn '[1,2,1])) ol "1-23-0")

  , expect3 (Left $ XF "Regex no results")
                  $ eval3 @(Rescan Ip4 >> Head'' "failedn" >> Snd >> Map (ReadP Int))
                          @((Len >> Same 4) && All (Between 0 255))
                          @(Printfnt 4 "%03d.%03d.%03d.%03d")
                          ol "1.21.x31.4"

  , expect3 (Right $ unsafeRefined3 [1,21,31,4] "001.021.031.004")
                  $ eval3 @(Rescan Ip4 >> Head'' "failedn" >> Snd >> Map (ReadP Int))
                          @((Len >> Same 4) && All (Between 0 255))
                          @(Printfnt 4 "%03d.%03d.%03d.%03d")
                          ol "1.21.31.4"

  , expectE (Right $ unsafeRefined [1,21,31,4])
                  $ evalE @(Rescan Ip4 >> Head'' "failedn" >> Snd >> Map (ReadP Int))
                          @((Len >> Same 4) && All (Between 0 255))
                          ol "1.21.31.4"

  , expect3 (Left $ XTFalse (-6.3))
                  $ eval3 @(ReadP Double)
                          @(Cmp 'Cgt ToRational (NegR 7 3))
                          @(Printf "%5.3f")
                          ol "-6.3"

  , expectE (Left $ RE.XTFalse (-6.3))
                  $ evalE @(ReadP Double) @(Cmp 'Cgt ToRational (NegR 7 3))
                  ol "-6.3"

  , expect3 (Right $ unsafeRefined3 4.123 "")
                  $ eval3 @(ReadP Double) @(Cmp 'Cgt ToRational (NegR 7 3)) @""
                  ol "4.123"

  , expect3 (Right $ unsafeRefined3 4.123 (4123 % 1000))
                  $ eval3 @Id @(Gt (NegR 7 3)) @(PosR 4123 1000)
                  ol 4.123

  , expect3 (Right $ unsafeRefined3 [1,2,3,4] "")
                  $ eval3 @(Resplit "\\." >> Map (ReadP Int)) @(All (Between 0 255) && (Len >> Same 4)) @""
                  ol "1.2.3.4"

  , expectE (Right $ unsafeRefined [1,2,3,4])
                  $ evalE @(Resplit "\\." >> Map (ReadP Int)) @(All (Between 0 255) && (Len >> Same 4))
                  ol "1.2.3.4"

  , expect3 (Left $ XTF [291,1048319,4387,17,1] "out of bounds")
                  $ eval3 @Ip6A @Ip6B @""
                  ol "123:Ffeff:1123:11:1"

  , expectE (Left $ RE.XTF [291,1048319,4387,17,1] "out of bounds")
                  $ evalE @Ip6A @Ip6B
                  ol "123:Ffeff:1123:11:1"

  , expect3 (Right $ unsafeRefined3 [12,2,0,255] "abc")
                  $ eval3 @Ip4A @Ip4B @"abc"
                  ol "12.2.0.255"

  , expect3 (Right $ unsafeRefined3 [123,45,6789] "def")
                  $ eval3
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" >> OneP >> Snd >> Map (ReadBaseInt 10))
                  @(Guard "expected 3" (Len >> Same 3)
                 >> Guard "3 digits" (Ix' 0 >> Between 0 999)
                 >> Guard "2 digits" (Ix' 1 >> Between 0 99)
                 >> Guard "4 digits" (Ix' 2 >> Between 0 9999)
                 >> 'True
                   ) @"def"
                   ol "123-45-6789"

  , expect3 (Right $ unsafeRefined3 [123,45,6789] "xyz")
                  $ eval3
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" >> OneP >> Snd >> Map (ReadBaseInt 10))
                  @(Guards (ToGuardsT (Printf2 "guard(%d) %d is out of range") '[Between 0 999, Between 0 99, Between 0 9999]) >> 'True)
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
                  $ eval3 @(Rescan DdmmyyyyR >> OneP >> Snd >> Map (ReadBaseInt 10))
                           @(Ddmmyyyyval >> 'True)
                           @"xyz"
                           ol "31-11-1999"
  , expect3 (Right $ unsafeRefined3 [123,45,6789] "xyz") $ eval3
                  @(Rescan "^(\\d{3})-(\\d{2})-(\\d{4})$" >> OneP >> Snd >> Map (ReadBaseInt 10))
                  @(Guards (ToGuardsT (Printf2 "guard(%d) %d is out of range") '[Between 0 999, Between 0 99, Between 0 9999]) >> 'True)
                  @"xyz"
                  ol "123-45-6789"

  , expect3 (Right $ unsafeRefined3 [1,2,3,4] "001.002.003.004") $ eval3P ip4 ol "1.2.3.4"
  , expect3 (Left $ XF "invalid base 10") $ eval3P ip4 ol "1.2.3x.4"
  , expect3 (Left $ XTF [1,2,3,4,5] "expected 4 numbers") $ eval3P ip4 ol "1.2.3.4.5"
  , expect3 (Left $ XTF [1,2,300,4] "each number must be between 0 and 255") $ eval3P ip4 ol "1.2.300.4"
  , expect3 (Right $ unsafeRefined3 [1,2,3,4,5,6,7,8,9,0,3] "1234-5678-903") $ eval3P cc ol "12345678903"
  , expectE (Right $ unsafeRefined ([1,2,3,4,5,6,7,8,9,0,3], "1234-5678-903")) $ evalEP (proxyEToV cc) ol "12345678903"
  , expect3 (Left $ XTFalse [1,2,3,4,5,6,7,8,9,0,1]) $ eval3P cc ol "12345678901"
  , expectE (Left $ RE.XTFalse ([1,2,3,4,5,6,7,8,9,0,1],"1234-5678-901")) $ evalEP (proxyEToV cc) ol "12345678901"
--  , expect3 (Right $ unsafeRefined3 True ["T","r","ue","Tr","ue"]) $ eval3P (Proxy @'(Id, Id, Do '[ShowP, Dup, Sapa, SplitAts '[1,1,2,2]], Bool)) True
  , expectE (Right $ unsafeRefined (True,["T","r","ue","Tr","ue"])) $ evalEP (proxyEToV $ Proxy @'(Id, Id, Do '[ShowP, W Dup, Sapa, SplitAts '[1,1,2,2]], Bool)) ol True
  , expectE (Right $ unsafeRefined (True,["T","r","ue","Tr","ue"])) $ evalEP (proxyEToV $ mkProxy3E @Id @Id @(Do '[ShowP, W Dup, Sapa, SplitAts '[1,1,2,2]]) @Bool) ol True
  , expect3 (Right $ unsafeRefined3 ([12,13,14],TimeOfDay 12 13 14) "12:13:14") $ eval3P hms2E ol "12:13:14"
  , expect3 (Left (XTF ([12,13,99], TimeOfDay 12 13 99) "guard(2) 99 secs is out of range")) $ eval3P hms2E ol "12:13:99"

  , expectPE (PresentT "gt3") $ pl @(Case (Snd >> Failp "xx") '[Gt 3, Lt 2, Same 3] '["gt3","lt2","eq3"] Id) 15
  , expectPE (PresentT "lt2") $ pl @(Case (Snd >> Failp "xx") '[Gt 3, Lt 2, Same 3] '["gt3","lt2","eq3"] Id) 1
  , expectPE (PresentT "eq3") $ pl @(Case (Snd >> Failp "xx") '[Gt 3, Lt 2, Same 3] '["gt3","lt2","eq3"] Id) 3

  , expectPE (FailT "no match") $ pl @(Case (Snd >> Failp "no match") '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (FailT "no match for 015") $ pl @(Case (Fail (Snd >> Unproxy) (Fst >> Printf "no match for %03d")) '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (FailT "no match for 015") $ pl @(Case (FailCase (Printf "no match for %03d")) '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (FailT "no match for 015") $ pl @(Case'' (Printf "no match for %03d") '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (PresentT "other") $ pl @(Case "other" '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (PresentT "151515") $ pl @(Case (Fst >> ShowP >> Id <> Id <> Id) '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (FailT "Case:no match") $ pl @(Case' '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (FailT "no match for -012") $ pl @(Case (FailCase (Printf "no match for %04d")) '[Between 0 5, Same 6, Between 7 10] '[ 'LT, 'EQ, 'GT] Id) (-12)
  , expectPE (PresentT [Left 1,Left 2,Right "fizz",Left 4,Right "buzz",Right "fizz",Left 7,Left 8,Right "fizz",Right "buzz",Left 11,Right "fizz",Left 13,Left 14,Right "fizzbuzz"]) $ pl @(Map Fizzbuzznew) [1..15]
  , expectPE (PresentT (Left 'x')) $ pl @(EitherB (Fst > 10) (Snd >> Fst) (Snd >> Snd)) (7,('x',99))
  , expectPE (PresentT (Right 99)) $ pl @(EitherB (Fst > 10) (Snd >> Fst) (Snd >> Snd)) (11,('x',99))
  , expectPE (PresentT (Right 99)) $ pl @(EitherB (Gt 10) "found left" 99) 12
  , expectPE (PresentT (Left "found left")) $ pl @(EitherB (Gt 10) "found left" 99) 7

  , expectPE (FailT "msg=someval caught(044)") $ pl @(Catch' (Failt Int "someval") (Printf2 "msg=%s caught(%03d)")) (44 :: Int)
  , expectPE (FailT "msg=expected list of length 1 but found length=3 caught([10,12,13])") $ pl @(Catch' OneP (Second ShowP >> Printf2 "msg=%s caught(%s)")) [10,12,13]
  , expectPE (PresentT 10) $ pl @(Catch' OneP (Second ShowP >> Printf2 "msg=%s caught(%s)")) [10]
  , (@?=) (unsafeRefined3 [1,2,3,4] "001.002.003.004") ($$(refined3TH "1.2.3.4") :: MakeR3 Ip)
  , (@?=) (unsafeRefined @'True ("1.2.3.4" :: String)) $$(refinedTH "1.2.3.4")
  , (@?=) (unsafeRefined @((Len >> Same 4) && Luhn) [1,2,3,0]) $$(refinedTH [1,2,3,0])
  , (@?=) (unsafeRefined @((Len >> Same 4) && Luhn >> Not) [1,2,3,1]) $$(refinedTH [1,2,3,1])
  , expectPE (FailT "msg=expected list of length 1 but found length=2 err s=[10,11]") $ pl @(Catch' OneP (Second ShowP >> Printf2 "msg=%s err s=%s")) [10,11]
  , expectPE (PresentT 99) $ pl @(Catch OneP 99) [10,11]
  , expectPE (PresentT 10) $ pl @(Catch OneP 99) [10]
  , expectPE (PresentT False) $ pl @(Catch OneP 'True) [False]  -- cant know that this is FalseT cos is driven by type of the list not the 'True part
  , expectPE FalseT $ pl @(Catch OneP 'False) [True,True,False]
  , expectPE TrueT $ pl @(Catch OneP 'True) []
  , expectPE (PresentT (-255)) $ pl @(ReadBase Int 16) "-ff"
  , expectPE (PresentT 255) $ pl @(ReadBase Int 16) "ff"
  , expectPE (PresentT "-7b") $ pl @(ShowBase 16) (-123)
  , expectPE (PresentT "7b") $ pl @(ShowBase 16) 123
  ]

