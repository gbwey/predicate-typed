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
module TestPredicate where
import Safe
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
import Data.Functor.Compose
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
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
  , expectPE (PresentT LT) $ pl @("aa" ==! Id) "aaaa"
  , expectPE FalseT $ pl @(FromEnum ("aa" ==! Id) >> Same 1) "aaaa"
  , expectPE (PresentT (Right 1)) $ pl @(HeadDef 'False Id +++ Id) (Right @[Bool] 1) -- need @[Bool] cos we said 'False!
  , expectPE (PresentT (Left True)) $ pl @(HeadDef 'False Id +++ Id) (Left @_ @Int [True,False]) -- need @[Bool] cos we said 'False!
  , expectPE (PresentT (Right True)) $ pl @(Not Id +++ Id) (Right True)
  , expectPE (PresentT (4,4)) $ pl @(Dup >> Id) 4
  , expectPE (PresentT (Right 12)) $ pl @(Not Id +++ Id) (Right 12)
  , expectPE (PresentT (Right 1)) $ pl @(HeadDef () Id +++ Id) (Right @[()] 1) -- breaks otherwise: Id says () -> () so has to be a list of [()]
  , expectPE (PresentT (Right 1)) $ pl @(HeadDef () Id +++ Id) (Right @[()] 1) -- this breaks! cos Left doesnt have a type
  , expectPE FalseT $ pl @(Not (Fst Id >> Len <= 6)) ([2..7],True)
  , expectPE TrueT $ pl @(Fst Id >> Len <= 6) ([2..7],True)
  , expectPE TrueT $ pl @(Length (Fst Id) <= 6) ([2..7],True)
  , expectPE TrueT $ pl @(Fst Id >> (Len <= 6)) ([2..7],True)
  , expectPE FalseT $ pl @(HeadDef 12 (Fst Id) >> Le 6) ([],True)
  , expectPE TrueT $ pl @(HeadDef 1 (Fst Id) >> Le 6) ([],True)
  , expectPE (FailT "Head(empty)") $ pl @(Head (Fst Id) >> Le 6) ([]::[Int], True)
  , expectPE FalseT $ pl @(HeadDef 10 (Fst Id) >> Le 6) ([],True)
  , expectPE (FailT "zz") $ pl @(HeadFail "zz" (Fst Id) >> Le 6) ([],True)
  , expectPE (FailT "failed1") $ pl @((HeadFail "failed1" (Fst Id) >> Le 6) || 'False) ([],True)
  , expectPE TrueT $ pl @((Fst Id >> HeadFail "failed2" Id >> Le (6 -% 1)) || 'False) ([-9],True)
  , expectPE (FailT "failed3") $ pl @((Fst Id >> Failt _ "failed3" >> Le (6 -% 1)) || 'False) ([-5],True)
  , expectPE TrueT $ pl @(MaybeIn 'True Id) (Nothing @Bool) -- need @() else breaks
  , expectPE (PresentT 10) $ pl @(MaybeIn (Failt _ "failed4") Id) (Just 10)
  , expectPE (PresentT 10) $ pl @(Just Id) (Just 10)
  , expectPE FalseT $ pl @(MaybeIn 'False Id) (Nothing @Bool) -- breaks otherwise
  , expectPE FalseT $ pl @(Id > "xx") "abc"
  , expectPE TrueT $ pl @(Id > "aa") "abc"
  , expectPE TrueT $ pl @(Gt 4) 5
  , expectPE TrueT $ pl @(Any (Gt 3) (Fst Id)) ([10,12,3,5],"ss")
  , expectPE FalseT $ pl @(All (Gt 3) (Fst Id)) ([10,12,3,5],"ss")
  , expectPE (PresentT [False,False,False,True]) $ pl @(Map (Mod Id 3) (Fst Id) >> Map (Gt 1) Id) ([10,12,3,5],"ss")
  , expectPE (PresentT (12,5)) $ pl @(Fst Id >> Dup >> (Ix 1 (Failp "failed5") *** Ix 3 (Failp "failed5")) >> Id) ([10,12,3,5],"ss")
  , expectPE FalseT $ pl @(Fst Id >> Dup >> (Ix 1 (Failp "failed5") *** Ix 3 (Failp "failed5")) >> Fst Id < Snd Id) ([10,12,3,5],"ss")
  , expectPE TrueT $ pl @(Fst Id >> Dup >> (Ix 1 (Failp "failed5") *** Ix 3 (Failp "failed5")) >> Fst Id > Snd Id) ([10,12,3,5],"ss")
  , expectPE TrueT $ pl @(Fst Id > Snd Id) (True,False)
  , expectPE FalseT $ pl @(Fst Id == Snd Id) (True,False)
  , expectPE TrueT $ pl @(Not Id*** Id >> Fst Id == Snd Id) (True,False)
  , expectPE FalseT $ pl @(Snd Id >> Len &&& Ix 3 (Failp "someval1") >> Fst Id == Snd Id) ('x',[1..5])
  , expectPE FalseT $ pl @(Snd Id >> Len &&& Ix 3 (Failp "someval2") >> Fst Id < Snd Id) ('x',[1..5])
  , expectPE TrueT $ pl @(Snd Id >> Len &&& Ix 3 (Failp "someval3") >> Fst Id > Snd Id) ('x',[1..5])
  , expectPE FalseT $ pl @(Snd Id >> SplitAt 2 Id >> Len *** Len >> Fst Id > Snd Id) ('x',[1..5])
  , expectPE FalseT $ pl @(Any (Same 2) Id) [1,4,5]
  , expectPE TrueT $ pl @(Any (Same 2) Id) [1,4,5,2,1]
  , expectPE TrueT $ pl @(Elem Id '[2,3,4]) 2
  , expectPE FalseT $ pl @(Elem Id '[2,3,4]) 6
  , expectPE TrueT $ pl @(Elem Id '[13 % 2]) 6.5
  , expectPE TrueT $ pl @(Elem Id '[13 % 2, 12 % 1]) 6.5
  , expectPE FalseT $ pl @(Elem Id '[13 % 2, 12 % 1]) 6
  , expectPE (FailT "lhs") $ pl @(Map Len Id >> Ix 3 (Failp "lhs") &&& Ix 0 5 >> Fst Id == Snd Id) [[1..4],[4..5]]
  , expectPE FalseT $ pl @(Map Len Id >> Ix 0 (Failp "lhs") &&& Ix 1 5 >> Fst Id == Snd Id) [[1..4],[4..5]]
  , expectPE (FailT "rhs") $ pl @(Map Len Id >> Ix 1 (Failp "lhs") &&& Ix 3 (Failp "rhs") >> Fst Id == Snd Id) [[1..4],[4..5]]
  , expectPE (FailT "lhs") $ pl @(Map Len Id >> Ix 10 (Failp "lhs") &&& Ix 1 (Failp "rhs") >> Fst Id == Snd Id) [[1..4],[4..5]]
  , expectPE (FailT "rhs") $ pl @(Map Len Id >> Ix 0 (Failp "lhs") &&& Ix 10 (Failp "rhs") >> Fst Id == Snd Id) [[1..4],[4..5]]
  , expectPE FalseT $ pl @(Map Len Id >> Ix 10 3 &&& Ix 1 (Failp "rhs") >> Fst Id == Snd Id) [[1..4],[4..5]]
  , expectPE FalseT $ pl @(Map Len Id >> Ix 3 3 &&& Ix 1 4 >> Fst Id == Snd Id) [[1..4],[4..5]]
  , expectPE FalseT $ pl @(Map Len Id >> Ix 10 3 &&& Ix 1 4 >> Fst Id == Snd Id) [[1..4],[4..5]]
  , expectPE FalseT $ pl @(Map Len Id >> Ix 10 5 &&& Ix 1 4 >> Fst Id == Snd Id) [[1..4],[4..5]]
  , expectPE TrueT $ pl @(Map Len Id >> Ix 10 2 &&& Ix 1 4 >> Fst Id == Snd Id) [[1..4],[4..5]]
  , expectPE (PresentT ([1],[2,3,4,5])) $ pl @(Partition (Lt 2) Id >> Id) [1,2,3,4,5]
  , expectPE (PresentT [1,2,3]) $ pl @(MaybeIn MEmptyP Id) (Just [1,2,3])
  , expectPE (PresentT []) $ pl @(MaybeIn MEmptyP Id) (Nothing @[Int])
  , expectPE (FailT "'Just found Nothing") $ pl @('Just (FailS "someval")) (Nothing @()) -- breaks otherwise
  , expectPE (PresentT (4,4)) $ pl @Dup 4
  , expectPE (PresentT 3) $ pl @(Last Id) [1,2,3]
  , expectPE (PresentT 123) $ pl @(Just Id >> Id) (Just 123)
  , expectPE (FailT "Asdf") $ pl @(HeadFail "Asdf" Id) ([] :: [()]) -- breaks otherwise
  , expectPE (FailT "Head(empty)") $ pl @(Head Id) ([] :: [Int])
  , expectPE (FailT "Head(empty)") $ pl @(Head Id) ([] :: [Double])
  , expectPE (FailT "Succ bounded") $ pl @(SuccB' Id) GT
  , expectPE (PresentT LT) $ pl @(SuccB 'LT Id) GT
  , expectPE (PresentT EQ) $ pl @(SuccB 'GT Id) LT
  , expectPE (PresentT EQ) $ pl @(SuccB' Id) LT
  , expectPE (FailT "Pred bounded") $ pl @(PredB' Id) LT
  , expectPE (PresentT GT) $ pl @(PredB 'GT Id) LT
  , expectPE (PresentT EQ) $ pl @(PredB 'LT Id) GT
  , expectPE (PresentT EQ) $ pl @(PredB' Id) GT
  , expectPE (FailT "ToEnum bounded") $ pl @(ToEnumBFail Ordering) 44
  , expectPE (PresentT LT) $ pl @(ToEnumBDef Ordering 'LT) 123
  , expectPE (PresentT EQ) $ pl @(ToEnumBDef Ordering 'GT) 1
  , expectPE (PresentT EQ) $ pl @(ToEnumBFail Ordering) 1
  , expectPE (PresentT 11) $ pl @(Succ Id) 10
  , expectPE (FailT "Succ IO e=Prelude.Enum.Bool.succ: bad argument") $ pl @(Succ Id) True -- captures the exception
  , expectPE (PresentT ([4,5,6,7,8,9,10],[1,2,3])) $ pl @(Partition (Gt 3) Id) [1..10]
  , expectPE (PresentT ([2,4,6],[1,3,5])) $ pl @(Partition Even Id) [1..6]
  , expectPE TrueT $ pl @(Partition Even Id >> Null *** (Len > 4) >> Fst Id == Snd Id) [1..6]
  , expectPE (PresentT 5) $ pl @(Snd Id >> Snd Id >> Snd Id >> Snd Id >> Id) (9,(1,(2,(3,5))))
  , expectPE (FailT "ExitWhen") $ pl @(HeadFail "failedn" Id &&& (Len == 1 >> ExitWhen "ExitWhen" Id) >> Fst Id) [3]
  , expectPE (PresentT 3) $ pl @(Head Id &&& (Len == 1 >> Not Id >> ExitWhen "ExitWhen" Id) >> Fst Id) [3]
  , expectPE (PresentT 3) $ pl @(Head Id &&& (Len == 1 >> ExitWhen "ExitWhen" (Not Id)) >> Fst Id) [3]
  , expectPE (FailT "ExitWhen") $ pl @(ExitWhen "ExitWhen" (Len /= 1) >> Head Id) [3,1]
  , expectPE (PresentT 3) $ pl @(ExitWhen "ExitWhen" (Len /= 1) >> Head Id) [3]
  , expectPE TrueT $ pl @(ExitWhen "ExitWhen" (Len /= 1) >> Head Id >> Gt (20 -% 1 )) [3]
  , expectPE FalseT $ pl @(ExitWhen "ExitWhen" (Len /= 1) >> Head Id >> Gt (20 -% 1 )) [-23]
  , expectPE (PresentT (-1.0)) $ pl @(Negate Id >> Dup >> First (Succ Id) >> Swap >> Fst Id - Snd Id) 4
  , expectPE (PresentT (Right 12)) $ pl @(Not Id +++ Id) (Right @Bool 12)
  , expectPE (PresentT CGt) $ pl @(FromEnum ("aa" ==! Id) >> ToEnum OrderingP Id) "aaaa"
  , expectPE (PresentT False) $ pl @(Msg "someval4" (Gt 4 >> Id)) 4
  , expectPE (PresentT ()) $ pl @(Snd Id >> Snd Id >> Snd Id >> Snd Id >> Id) (1,('a',(3,(True,()))))
  , expectPE TrueT $ pl @(Re "\\d{4}-\\d{3}" Id) "1234-123"
  , expectPE FalseT $ pl @(Re "\\d{4}-\\d{3}" Id) "1234-1x3"
  , expectPE TrueT $ pl @(Re' '[ 'Caseless, 'Dotall ] "ab" Id) "aB"
  , expectPE TrueT $ pl @(Re' '[ 'Caseless, 'Dotall ] "ab." Id) "aB\n"
  , expectPE FalseT $ pl @(Re' '[ 'Caseless ] "ab." Id) "aB\n"
  , expectPE TrueT $ pl @(Re "(?i)ab" Id) "aB" -- runtime [use 'Caseless instead]
  , expectPE FalseT $ pl @(Re "ab" Id) "aB"
  , expectPE (PresentT [("aB",["B"]),("cd",["d"])]) $ pl @(Rescan ".(.)" Id) "aBcd"
  , expectPE (PresentT [14,12,10,4,2]) $ pl @(SortOnDesc Id Id) [10,4,2,12,14]
  , expectPE (PresentT [2,4,10,12,14]) $ pl @(SortOn Id Id) [10,4,2,12,14]
  , expectPE (PresentT [14,12,10,4,2]) $ pl @(SortOn (Negate Id) Id) [10,4,2,12,14]
  , expectPE (PresentT [('a',4),('a',14),('b',2),('c',10),('d',12),('z',1)]) $ pl @(SortOn (Fst Id) Id) (zip "cabdaz" [10,4,2,12,14,1])
  , expectPE (FailT "asdf(4)") $ pl @(SortOn (FailS "asdf") Id) [10,4,2,12,14]
  , expectPE TrueT $ pl @(Min &&& Max >> Id >> Fst Id < Snd Id) [10,4,2,12,14]
  , expectPE (FailT "ExitWhen") $ pl @(Partition (ExitWhen "ExitWhen" (Gt 10) >> Gt 2) Id) [1..11]
  , expectPE (PresentT [False,False,True,True,True]) $ pl @(Map (ExitWhen "ExitWhen" (Gt 10) >> Gt 2) Id) [1..5]
  , expectPE (PresentT ([1,2],[3,4,5,6,7,8,9,10,11])) $ pl @(Break (Gt 2) Id) [1..11]
  , expectPE (PresentT ([1,2,3],[4,5,6,7,8,9,10,11])) $ pl @(Span (Lt 4) Id) [1..11]
  , expectPE (PresentT [GT,GT,LT,EQ]) $ pl @(Pairs >> Map (First (Succ Id >> Succ Id) >> Fst Id ==! Snd Id) Id) [1,2,3,6,8]
  , expectPE TrueT $ pl @(Re "^\\d{1,3}(?:\\.\\d{1,3}){3}$" Id) "123.1.1.21"
  , expectPE (PresentT [("123.",["."]),("8.",["."]),("99.",["."]),("21",[])]) $ pl @(Rescan "\\d{1,3}(\\.)?" Id) "123.8.99.21"
  , expectPE (PresentT 117) $ pl @(MaybeIn (Failp "err") (Succ Id)) (Just 116)
  , expectPE (PresentT 99) $ pl @(MaybeIn 99 (Succ Id)) (Nothing @Int)
  , expectPE (FailT "someval") $ pl @(MaybeIn (Failp "someval") (Succ Id)) (Nothing @())
  , expectPE TrueT $ pl @(MaybeIn 'True 'False) (Nothing @())
  , expectPE FalseT $ pl @(MaybeIn 'True 'False) (Just "aa")
  , expectPE (PresentT LT) $ pl @(MaybeIn MEmptyP (Fst Id ==! Snd Id)) (Just ('x','z'))
  , expectPE (PresentT EQ) $ pl @(MaybeIn MEmptyP (Fst Id ==! Snd Id)) (Nothing @(Char,Char))
  , expectPE TrueT $ pl @('True ||| 'False) (Left @_ @() "someval")
  , expectPE FalseT $ pl @('True ||| 'False) (Right @() "someval")
  , expectPE (PresentT 123) $ pl @('Left Id) (Left 123)
  , expectPE (FailT "'Left found Right") $ pl @('Left Id) (Right @() 123)
  , expectPE (PresentT 123) $ pl @('Right Id) (Right 123)
  , expectPE (FailT "'Right found Left") $ pl @('Right Id) (Left @_ @() 123)
  , expectPE (PresentT ["1","2","3"]) $ pl @(MaybeIn MEmptyP (Ones (ShowP Id))) (Just 123)
  , expectPE (PresentT []) $ pl @(MaybeIn MEmptyP (Ones (ShowP Id))) (Nothing @String)
  , expectPE (PresentT "124") $ pl @(ShowP (Succ Id) ||| ShowP Id ) (Left @_ @() 123)
  , expectPE (PresentT "True") $ pl @(ShowP (Succ Id) ||| ShowP Id) (Right @Int True)
  , expectPE (PresentT (123 % 4)) $ pl @(ReadP Rational Id) "123 % 4"
  , expectPE (FailT "ReadP Ratio Integer (x123 % 4)") $ pl @(ReadP Rational Id) "x123 % 4"
  , expectPE (PresentT "") $ pl @('Proxy >> MEmptyP) "abc"
  , expectPE (PresentT ["a","b","c"]) $ pl @(MEmptyT _ ||| Ones Id) (Right @() "abc")
  , expectPE (PresentT []) $ pl @(MEmptyT _ ||| Ones Id) (Left @_ @[String] ["ab"])
  , expectPE (PresentT ["a","b"]) $ pl @(MaybeIn MEmptyP (Ones Id)) (Just @String "ab")
  , expectPE (PresentT []) $ pl @(MaybeIn MEmptyP (Ones Id)) (Nothing @String)
  , expectPE (PresentT (True, 13)) $ pl @(Not (IsNothing Id) &&& (Just Id >> Id + 12)) (Just 1)
  , expectPE (FailT "Just(empty)") $ pl @(Not (IsNothing Id) &&& (Just Id >> Id + 12)) Nothing
  , expectPE (PresentT True) $ pl @(Thd Id >> Fst Id) (1,2,(True,4))
  , expectPE (PresentT True) $ pl @(Fst (Thd Id)) (1,2,(True,4))
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
  , expectPE (PresentT 14) $ pl @(Unwrap Id >> Succ Id) (SG.Sum 13)
  , expectPE (PresentT 4) $ pl @(MEmptyT (SG.Sum _) >> Unwrap Id >> Id + 4) ()
  , expectPE (PresentT (SG.Sum 13)) $ pl @(Wrap (SG.Sum _) Id) 13
  , expectPE (PresentT "a") $ pl @(Id !! MEmptyT _) (Just "a")
  , expectPE (FailT "(!!) index not found") $ pl @(Id !! MEmptyT _) (Nothing @()) -- had to add @() to keep this happy: ghci is fine
  , expectPE (PresentT 'a') $ pl @(Id !! 0) ('a','b','c')
  , expectPE (FailT "err") $ pl @(Id !! Failt _ "err") ('a','b','c')
  , expectPE (PresentT 3) $ pl @(Id !! "d") (M.fromList $ zip (map (:[]) "abcd") [0 ..])
  , expectPE (PresentT 3) $ pl @(Id !! Head "d") (M.fromList $ zip "abcd" [0 ..]) -- had to String (instead of _) to keep this happy: ghci is fine
  , expectPE (PresentT ()) $ pl @(Id !! Head "d") (S.fromList "abcd") -- had to String (instead of _) to keep this happy: ghci is fine
  , expectPE (FailT "(!!) index not found") $ pl @(Id !! HeadFail "failedn" "e") (S.fromList "abcd") -- had to String (instead of _) to keep this happy: ghci is fine
  , expectPE (PresentT 13.345) $ pl @(Guard "regex failed" (Re "^\\d+(?:\\.\\d+)?$" Id) >> ReadP Double Id) "13.345"
  , expectPE (PresentT 13) $ pl @(Guard "regex failed" (Re "^\\d+(?:\\.\\d+)?$" Id) >> ReadP Double Id) "13"
  , expectPE (FailT "regex failed") $ pl @(ExitWhen "regex failed" (Not (Re "^\\d+(?:\\.\\d+)?$" Id)) >> ReadP Double Id) "-13.4"
  , expectPE (PresentT GT) $ pl @(FoldN 2 Id (Succ Id)) LT
  , expectPE (FailT "Succ IO e=Prelude.Enum.Ordering.succ: bad argument") $ pl @(FoldN 30 Id (Succ Id)) LT
  , expectPE (PresentT 'g') $ pl @(FoldN 6 Id (Succ Id)) 'a'
  , expectPE (PresentT '[') $ pl @(FoldN 6 Id (Pred Id)) 'a'
  , expectPE (FailT "Regex failed to compile") $ pl @(Re "\\d{4}\\" Id) "ayx"
  , expectPE (PresentT LT) $ pl @(FoldN 0 Id (Succ Id)) LT
  , expectPE (PresentT LT) $ pl @(FoldN 2 Id (Succ Id) >> FoldN 2 Id (Pred Id)) LT
  , expectPE (PresentT ["2","2"]) $ pl @(Map (Fst Id) (Rescan "." (ShowP Id)) >> Filter (Same "2") Id) 12324
  , expectPE (PresentT [LT,LT,LT,GT,EQ,LT]) $ pl @((Ones Id << ShowP Id) >> Map (Fst Id ==! Snd Id) Pairs) 1234223
  , expectPE (PresentT [(0,'a'),(1,'b'),(2,'c'),(3,'d')]) $ pl @(IToList _ Id) ("abcd" :: String)
  , expectPE (PresentT "abcd") $ pl @ToList (M.fromList $ zip [0..] "abcd")
  , expectPE (PresentT [123]) $ pl @ToList (Just 123)
  , expectPE (FailT "failed20") $ pl @(MaybeIn (Failp "failed20") 'False) (Nothing @Int)
  , expectPE (FailT "failed21") $ pl @(MaybeIn ('False >> FailS "failed21") 'False) (Nothing @Double)
  , expectPE (FailT "err") $ pl @(MaybeIn (Failp "err") Id) (Nothing @Int)
  , expectPE (FailT "err") $ pl @(MaybeIn (Failp "err") Id) (Nothing @())
  , expectPE (PresentT [(0,'a'),(1,'b'),(2,'c'),(3,'d')]) $ pl @(IToList _ Id) (M.fromList $ itoList ("abcd" :: String))
  , expectPE (PresentT [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(99,'e'),(99,'f'),(99,'g')]) $ pl @(ZipL 99 Id "abcdefg") [1..4]
  , expectPE (FailT "Zip(3,7) length mismatch") $ pl @(Zip "abc" Id) [1..7]
  , expectPE (PresentT [(1 % 1,'a'),(2 % 1,'b'),(3 % 1,'c'),(99 % 4,'d'),(99 % 4,'e')]) $ pl @(ZipL (99 % 4) '[1 % 1 , 2 % 1 , 3 % 1 ] Id) "abcde"

  , expectPE (PresentT [("X",'a'),("X",'b'),("X",'c'),("X",'d')]) $ pl @(ZipL "X" (EmptyT _ Id) Id) ("abcd" :: String)

  , expectPE (FailT "ZipR(0,4) rhs would be truncated") $ pl @(ZipR (Char1 "Y") (EmptyT _ Id) Id) "abcd"

  , expectPE (PresentT [9,2,7,4]) $ pl @ToList (M.fromList (zip ['a'..] [9,2,7,4]))
  , expectPE (PresentT [(0,9),(1,2),(2,7),(3,4)]) $ pl @(IToList _ Id) [9,2,7,4]
  , expectPE (PresentT [('a',9),('b',2),('c',7),('d',4)]) $ pl @(IToList _ Id) (M.fromList (zip ['a'..] [9,2,7,4]))
  , expectPE (PresentT [((),234)]) $ pl @(IToList _ Id) (Just 234)
  , expectPE (PresentT []) $ pl @(IToList _ Id) (Nothing @Double)
  , expectPE (PresentT (-4,5)) $ pl @(DivMod (Negate Id) 7) 23
  , expectPE (PresentT (-3,-2)) $ pl @(QuotRem (Negate Id) 7) 23
  , expectPE (PresentT (True,3.4)) $ pl @(Thd Id >> Snd Id >> Fst Id) (1,'a',('x',((True,3.4),999)))
  , expectPE (PresentT (True,3.4)) $ pl @(Fst (Snd (Thd Id))) (1,'a',('x',((True,3.4),999)))
  , expectPE (PresentT 7) $ pl @(Fst Id) (7,999.12)
  , expectPE (PresentT (M.fromList [(1,'a')])) $ pl @(MaybeIn MEmptyP Id) (Just (M.fromList [(1,'a')]))
  , expectPE (PresentT (M.fromList [])) $ pl @(MaybeIn MEmptyP Id) (Nothing @(M.Map () ()))
  , expectPE (PresentT [("1",["1"]),("2",["2"]),("3",["3"]),("4",["4"])]) $ pl @(Rescan "(\\d)+?" Id) "1234"
  , expectPE (PresentT [("1234",["4"])]) $ pl @(Rescan "(\\d)+" Id) "1234"
  , expectPE (PresentT [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])]) $ pl @(Rescan "(\\d{1,3})(\\.(\\d{1,3}))+?" Id) "1.2.3.4" -- overcapturing
  , expectPE (PresentT [("1234",["4"])]) $ pl @(Rescan "^(\\d)+?$" Id) "1234"
  , expectPE (PresentT [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])]) $ pl @(Rescan "(\\d{1,3})(\\.(\\d{1,3}))+?" Id) "1.2.3.4"
  , expectPE (PresentT ["123","2","3","5","6"]) $ pl @(Resplit "\\." Id) "123.2.3.5.6"
  , expectPE (PresentT [("1.2",["1","2"]),("3.4",["3","4"])]) $ pl @(Rescan "(\\d{1,3})(?:\\.(\\d{1,3}))+?" Id) "1.2.3.4" -- bizzare!
  , expectPE (PresentT [("1.2.3.4",["1","2","3","4"])]) $ pl @(Rescan "^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$" Id) "1.2.3.4" -- this is good!
  , expectPE (PresentT [13,16,17]) $ pl @(Guard "err" (Len > 2) >> Map (Succ Id) Id) [12,15,16]
  , expectPE (FailT "err found len=3") $ pl @(Guard (PrintF "err found len=%d" Len) (Len > 5) >> Map (Succ Id) Id) [12,15,16]
  , expectPE (FailT "PrintF (IO e=printf: bad formatting char 'd')") $ pl @(PrintF "someval %d" Id) ("!23"::String)
  , expectPE (PresentT [12,0,1,13,0,1,14,0,1,15,0,1,16]) $ pl @(Intercalate (Fst Id) (Snd Id)) ([0,1], [12,13,14,15,16])
  , expectPE (PresentT [12,-5,13,-5,14,-5,15,-5,16]) $ pl @((Pure [] (Negate Len) &&& Id) >> Intercalate (Fst Id) (Snd Id)) [12,13,14,15,16]
  , expectPE (PresentT [13,16,17]) $ pl @(If (Len > 2) (Map (Succ Id) Id) (FailS "someval")) [12,15,16]
  , expectPE (PresentT [13,16,17]) $ pl @(Guard "oops" (Len > 2) >> Map (Succ Id) Id) [12,15,16]
  , expectPE (FailT "err") $ pl @(ExitWhen "err" (Len > 2) >> Map (Succ Id) Id) [12,15,16]
  , expectPE (PresentT [13]) $ pl @(ExitWhen "err" (Len > 2) >> Map (Succ Id) Id) [12]
  , expectPE (FailT "err") $ pl @(Guard "err" (Len > 2) >> Map (Succ Id) Id) [12]
  , expectPE (PresentT 12) $ pl @(OneP Id) [12]
  , expectPE (FailT "OneP 5 elements") $ pl @(OneP Id) [1..5]
  , expectPE (FailT "OneP empty") $ pl @(OneP Id) ([] ::[()])
  , expectPE (FailT "err(8)") $ pl @(Map (If (Lt 3) 'True (Failt _ "err")) Id) [1..10]
  , expectPE (FailT "someval(8)") $ pl @(Map (If (Lt 3) 'True (Failt _ "someval")) Id) [1..10]
  , expectPE (PresentT [True,True,False,False,False]) $ pl @(Map (If (Lt 3) 'True 'False) Id) [1..5]
  , expectPE (PresentT ["a","b","c"]) $ pl @(MaybeIn MEmptyP (Ones Id)) (Just @String "abc")
  , expectPE (FailT "someval") $ pl @(Guard "someval" (Len == 2) >> (ShowP Id &&& Id)) ([] :: [Int])
  , expectPE (PresentT ([2,3],"[2,3]")) $ pl @(Guard "someval" (Len == 2) >> (Id &&& ShowP Id)) [2,3]
  , expectPE (FailT "someval") $ pl @(Guard "someval" (Len == 2) >> (ShowP Id &&& Id)) [2,3,4]
  , expectPE (PresentT 55) $ pl @(Map (Wrap (SG.Sum _) Id) Id >> MConcat Id >> Unwrap Id) [1..10]
  , expectPE (PresentT True) $ pl @(EitherIn (Not Id) Id) (Right @Bool True)
  , expectPE FalseT $ pl @(EitherIn (Not Id) Id) (Left @_ @Bool True)
  , expectPE FalseT $ pl @(Re "^\\d+$" Id) "123\nx"
  , expectPE TrueT $ pl @(Re "(?m)^\\d+$" Id) "123\nx" -- (?m) anchors match beginning/end of line instead of whole string
  , expectPE (PresentT (Just 'x')) $ pl @(Pure Maybe Id) 'x'
  , expectPE (PresentT (Right @() 'x')) $ pl @(Pure (Either _) Id) 'x'
  , expectPE (PresentT Nothing) $ pl @(MEmptyT (Maybe ())) 'x'
  , expectPE (PresentT (Left @_ @() 'x')) $ pl @(Pure (Either _) Id >> Swap) 'x'
  , expectPE (PresentT (Left 'x')) $ pl @(Pure (Either ()) Id >> Swap) 'x'
  , expectPE (PresentT (SG.Sum 52)) $ pl @(STimes 4 Id) (SG.Sum 13)
  , expectPE (PresentT (SG.Sum 52)) $ pl @(Wrap (SG.Sum _) Id >> STimes 4 Id) 13
  , expectPE (PresentT 52) $ pl @(FoldMap (SG.Sum _) Id) [14,8,17,13]
  , expectPE (PresentT 17) $ pl @(FoldMap (SG.Max _) Id) [14 :: Int,8,17,13] -- cos Bounded!
  , expectPE FalseT $ pl @(Catch (Re "\\d+(" Id) 'False) "123"
  , expectPE TrueT $ pl @(Catch (Re "\\d+" Id) 'False) "123"
  , expectPE (PresentT 3) $ pl @(Id !! Head "d") (M.fromList $ zip "abcd" [0 ..])   -- use Char1 "d" instead of "d" >> Head
  , expectPE (PresentT 10) $ pl @(Id !! MEmptyT _) (Just 10)
  , expectPE (FailT "(!!) index not found") $ pl @(Id !! MEmptyT _) (Nothing @())
  , expectPE TrueT $ pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) || (FoldMap (SG.Sum _) Id >> Gt 200)) [1..20]
  , expectPE FalseT $ pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) || (FoldMap (SG.Sum _) Id >> Gt 200)) [1..19]
  , expectPE TrueT $ pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) || (FoldMap (SG.Sum _) Id >> Gt 200)) []
  , expectPE (PresentT (False, 210)) $ pl @((Len >> (Elem Id '[4,7,1] || (Mod Id 3 >> Same 0))) &&& FoldMap (SG.Sum _) Id) [1..20]
  , expectPE (PresentT 'g') $ pl @(Id !! 6) ['a'..'z']
  , expectPE (PresentT ([141,214,125,1,2,3333],(False,False))) $ pl @(Map (ReadP Int Id) (Resplit "\\." Id) >> '(Id, '(Len == 4, All (Between 0 255 Id) Id))) "141.214.125.1.2.3333"
  , expectPE (PresentT ([141,214,125,1,2,6],(False,True))) $ pl @(Map (ReadP Int Id) (Resplit "\\." Id) >> Id &&& ((Len == 4) &&& All (Between 0 255 Id) Id)) "141.214.125.1.2.6"
  , expectPE (FailT "ReadP Int ()") $ pl @(Resplit "\\." Id >> Map (ReadP Int Id) Id >> Id &&& ((Len == 4) &&& All (Between 0 255 Id) Id)) "141.214.125."
  , expectPE (PresentT 9) $ pl @((Wrap _ Id *** Wrap (SG.Sum _) Id) >> SapA >> Unwrap Id) (4,5)
  , expectPE (PresentT (SG.Sum 9)) $ pl @((Wrap _ Id *** Wrap _ Id) >> SapA) (4,5)
  , expectPE (PresentT 9) $ pl @(SapA' (SG.Sum _) >> Unwrap Id) (4,5)
  , expectPE (PresentT "abcde") $ pl @(ScanNA (Succ Id)) (4,'a')
  , expectPE (PresentT ["abcd","bcd","cd","d",""]) $ pl @(ScanNA (Tail Id)) (4,"abcd" :: String)
  , expectPE (PresentT ["abcd","bcd","cd","d",""]) $ pl @(Len &&& Id >> ScanNA (Tail Id)) "abcd"
  , expectPE (PresentT ["abcd","bcd","cd","d",""]) $ pl @Tails ("abcd" :: String)
  , expectPE (PresentT (-4,-2)) $ pl @(DivMod (Fst Id) (Snd Id)) (10,-3)
  , expectPE (PresentT (-3,1)) $ pl @(QuotRem (Fst Id) (Snd Id)) (10,-3)
  , expectPE (FailT "DivMod zero denominator") $ pl @(DivMod (Fst Id) (Snd Id)) (10,0)
  , expectPE (PresentT 'd') $ pl @(Snd Id !! Fst Id) (3,"abcde" :: String)
  , expectPE (FailT "(!!) index not found") $ pl @(Snd Id !! Fst Id) (4,[9,8])
  , expectPE (PresentT 'c') $ pl @(2 &&& Id >> Snd Id !! Fst Id) ("abcdef" :: String)
  , expectPE (PresentT 'f') $ pl @((Len >> Pred Id) &&& Id >> Snd Id !! Fst Id) "abcdef"
  , expectPE (FailT "len is bad") $ pl @Ip6Test "FE80::203:Baff:FE77:326FF"
  , expectPE (FailT "not a hex") $ pl @Ip6Test "FE80::203:Baff:GE77:326F"
  , expectPE (FailT "count is bad") $ pl @Ip6Test "FE80::203:Baff:FE77:326F:::::"
  , expectPE (PresentT 65504) $ pl @(ReadBase Int 16 Id) "fFe0"
  , expectPE (PresentT "ffe0") $ pl @(ShowBase 16 Id) 65504
  , expectPE (FailT "invalid base 22") $ pl @(ReadBase Int 22 Id) "zzz"
  , expectPE (PresentT ("ffe0","fFe0")) $ pl @((ReadBase Int 16 Id &&& Id) >> First (ShowBase 16 Id)) "fFe0"
  , expectPE FalseT $ pl @(Id == "Abc") "abc"
  , expectPE TrueT $ pl @("Abc" ==~ Id) "abc"
  , expectPE (PresentT LT) $ pl @("Abc" ==! Id) "abc"
  , expectPE (PresentT EQ) $ pl @("Abc" ===~ Id) "abc"
  , expectPE (PresentT 'd') $ pl @(Id !! 3) ('a','b','c','d','e')
  , expectPE (PresentT 99) $ pl @(Id !! "s") $ M.fromList [("t",1), ("s", 20), ("s", 99)]
  , expectPE (PresentT 1) $ pl @(Head Id) [1,2,3]
  , expectPE (PresentT (Just (1,[2,3,4,5]))) $ pl @Uncons [1..5] -- with Typeable would need to specify the type of [1..5]
  , expectPE (PresentT (Just ([1,2,3,4],5))) $ pl @Unsnoc [1..5]
  , expectPE (PresentT [(0,1),(1,2),(2,3),(3,4),(4,5)]) $ pl @(IToList _ Id) [1..5]
  , expectPE (PresentT [(0,'a'),(1,'b'),(2,'c')]) $ pl @(IToList _ Id) ['a','b','c']
  , expectPE (PresentT [1,2,3,8,8]) $ pl @(PadR 5 8 Id) [1..3]
  , expectPE (PresentT [1,2,3,4,5]) $ pl @(PadR 5 0 Id) [1..5]
  , expectPE (PresentT [1,2,3,4,5,6]) $ pl @(PadR 5 0 Id) [1..6]
  , expectPE (PresentT [0,0,1,2,3]) $ pl @(PadL 5 0 Id) [1..3]
  , expectPE (PresentT []) $ pl @(Catch (Resplit "\\d+(" Id) (Snd Id >> MEmptyP)) "123"
  , expectPE (FailT "someval(8)") $ pl @(Map (Guard "someval" (Lt 3) >> 'True) Id) [1::Int ..10]
  , expectPE (FailT "(3 < 3) | (4 < 3) | (5 < 3) | (6 < 3) | (7 < 3) | (8 < 3) | (9 < 3) | (10 < 3)") $ pl @(Map (GuardSimple (Lt 3) >> 'True) Id) [1::Int .. 10]
  , expectPE FalseT $ pl @(All (Lt 3) Id) [1::Int .. 10]
  , expectPE (PresentT [True,True,True,True,True,True,True,True,True,True]) $ pl @(Map (GuardSimple (Ge 1) >> 'True) Id) [1::Int .. 10]
  , expectPE (PresentT [4,5,6]) $ pl @(ScanN 2 Id (Succ Id)) 4
  , expectPE (PresentT [4,4,4,4,4,4]) $ pl @(ScanN 5 Id Id) 4
  , expectPE (PresentT [1,2,3,244]) $ pl @(Rescan Ip4RE Id >> OneP Id >> Map (ReadBase Int 10 Id) (Snd Id) >> Ip4op) "1.2.3.244"
  , expectPE (FailT "octet 1 out of range 0-255 found 256") $ pl @(Rescan Ip4RE Id >> OneP Id >> Map (ReadBase Int 10 Id) (Snd Id) >> Ip4op) "1.256.3.244"
  , expectPE (FailT "Guards:invalid length(5) expected 4") $ pl @(Rescan "(\\d+)\\.?" Id >> ConcatMap (Snd Id) Id >> Map (ReadBase Int 10 Id) Id >> Ip4op) "1.22.244.66.77"
  , expectPE (PresentT (SG.Sum 123)) $ pl @(JustDef (MEmptyT _) Id) (Just (SG.Sum 123))
  , expectPE (PresentT (SG.Sum 0)) $ pl @(JustDef (MEmptyT _) Id) (Nothing @(SG.Sum _))
  , expectPE (PresentT (636 % 5)) $ pl @((ToRational 123 &&& Id) >> Fst Id + Snd Id) 4.2
  , expectPE (PresentT 127) $ pl @((123 &&& Id) >> Fst Id + Snd Id) 4
  , expectPE (PresentT 256) $ pl @(Rescan "(?i)^\\\\x([0-9a-f]{2})$" Id >> OneP Id >> Snd Id >> OneP Id >> ReadBase Int 16 Id >> Succ Id) "\\xfF"
  , expectPE (PresentT 256) $ pl @(Rescan "(?i)^\\\\x(.{2})$" Id >> OneP Id >> Snd Id >> OneP Id >> ReadBase Int 16 Id >> Succ Id) "\\xfF"
  , expectPE (PresentT (("fF",(255,"ff")),False)) $ pl @(Rescan "(?i)^\\\\x([0-9a-f]{2})$" Id >> OneP Id >> Snd Id >> OneP Id >> (Id &&& (ReadBase Int 16 Id >> (Id &&& ShowBase 16 Id))) >> (Id &&& ((Id *** Snd Id) >> Fst Id == Snd Id))) "\\xfF"
  , expectPE (PresentT [1,2,4,0]) $ pl @(Do '[Succ Id,Id,ShowP Id,Ones Id,Map (ReadBase Int 8 Id) Id]) 1239
  , expectPE (FailT "invalid base 8") $ pl @(Do '[Pred Id,Id,ShowP Id,Ones Id,Map (ReadBase Int 8 Id) Id]) 1239
  , expectPE (PresentT 47) $ pl @(ReadBase Int 2 Id) "101111"
  , expectPE (PresentT [LT,EQ,GT,EQ,EQ,EQ,EQ,EQ,EQ,EQ]) $ pl @(ScanN 2 Id (Succ Id) >> PadR 10 (MEmptyT Ordering) Id) LT
  , expectPE (PresentT 12) $ pl @('This Id) (This 12)
  , expectPE (FailT "'This found That") $ pl @('This Id) (That @() 12)
  , expectPE (PresentT (SG.Sum 12)) $ pl @(ThisDef (MEmptyT _) Id) (This @_ @() (SG.Sum 12))
  , expectPE (PresentT ()) $ pl @(ThisDef (MEmptyT _) Id) (That 12)
  , expectPE (PresentT (SG.Sum 12)) $ pl @(ThisFail "sdf" Id) (This @_ @() (SG.Sum 12))
  , expectPE (FailT "sdf") $ pl @(ThisFail "sdf" Id) (That @() (SG.Sum 12))
  , expectPE (FailT "sdf") $ pl @(ThisFail "sdf" Id) (That @Int 12)
  , expectPE (PresentT "this") $ pl @(TheseIn "this" "that" "these") (This @_ @() (SG.Sum 12))
  , expectPE FalseT $ pl @(IsThese Id) (That @() (SG.Sum 12))
  , expectPE TrueT $ pl @(IsThese Id) (These 1 (SG.Sum 12))
  , expectPE (PresentT ("Ab",13)) $ pl @(TheseIn (Id &&& 999) ("no value" &&& Id) Id) (These "Ab" 13)
  , expectPE (PresentT ("Ab",999)) $ pl @(TheseIn (Id &&& 999) ("no value" &&& Id) Id) (This "Ab")
  , expectPE (PresentT ("no value",13)) $ pl @(TheseIn (Id &&& 999) ("no value" &&& Id) Id) (That 13)
  , expectPE (PresentT "wxydef") $ pl @(ZipThese (Fst Id) (Snd Id) >> Map (TheseIn Id Id (Fst Id)) Id) (['w'..'y'],['a'..'f'])
  , expectPE (PresentT [("fe",["fe"]),("b1",["b1"]),("2a",["2a"])]) $ pl @(Rescan "([[:xdigit:]]{2})" Id) "wfeb12az"
  -- anchored means it has to start at the beginning: can have junk on the end which we cant detect but at least we know it starts at beginning
  , expectPE (FailT "Regex no results") $ pl @(Rescan' '[ 'Anchored ] "([[:xdigit:]]{2})" Id) "wfeb12az"
  , expectPE (PresentT [('s',1),('d',2),('f',3),('x',4),('x',5)]) $ pl @(("sdf" &&& Id) >> ZipThese (Fst Id) (Snd Id) >> Map (TheseIn (Id &&& 0) (Head "x" &&& Id) Id) Id) [1..5]
  , expectPE (PresentT "abc") $ pl @"abc" ()
  , expectPE FalseT $ pl @(Not 'True) ()
  , expectPE TrueT $ pl @'True ()
  , expectPE FalseT $ pl @'False ()
  , expectPE (PresentT LT) $ pl @'LT ()
  , expectPE (PresentT 123) $ pl @123 ()
  , expectPE (PresentT (4,("sadf",LT))) $ pl @(4 &&& "sadf" &&& 'LT) ()
  , expectPE (PresentT (4,("sadf",LT))) $ pl @(4 *** "sadf" *** 'LT) ('x',("abv",[1]))
  , expectPE (PresentT 6) $ pl @(Do '[4,5,6]) ()
  , expectPE (PresentT "hhhhh") $ pl @(Do '["abc", "Def", "ggg", "hhhhh"]) ()
  , expectPE (PresentT GT) $ pl @(Do '[ 'LT, 'EQ, 'GT ]) ()
  , expectPE (PresentT (-3 % 1)) $ pl @(Do '[4 % 4,22 % 1 ,12 -% 4]) ()
  , expectPE (PresentT [10,2,5]) $ pl @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 0 11 Id, Between 1 4 Id,Between 3 5 Id]) [10::Int,2,5]
  , expectPE (PresentT [31,11,1999]) $ pl @(Rescan DdmmyyyyRE Id >> OneP Id >> Map (ReadBase Int 10 Id) (Snd Id) >> Ddmmyyyyop) "31-11-1999"
  , expectPE (PresentT [31,11,1999]) $ pl @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]) [31,11,1999::Int]
  , expectPE (FailT "Guards:invalid length(2) expected 3") $ pl @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]) [31,11::Int]
  , expectPE (FailT "guard(1) 13 is out of range") $ pl @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]) [31,13,1999::Int]
  , expectPE (FailT "guard(0) 0 is out of range") $ pl @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]) [0,44,1999::Int]
  , expectPE (PresentT (fromGregorian 1999 11 30)) $ pl @(ReadP Day Id) "1999-11-30"
  , expectPE (FailT "ReadP Day (1999-02-29)") $ pl @(ReadP Day Id) "1999-02-29"
  , expectPE (PresentT (TimeOfDay 14 59 20)) $ pl @(ReadP TimeOfDay Id) "14:59:20"
--  , expectPE (PresentT (TimeOfDay 26 61 61)) $ pl @(ReadP TimeOfDay Id) "26:61:61" -- yep: this is valid in <=time-1.8 ! need to do your own validation
  , expectPE (FailT "ParseTimeP TimeOfDay (%H:%M%S) failed to parse") $ pl @(ParseTimeP TimeOfDay "%H:%M%S" Id) "14:04:61"
  , expectPE (PresentT (TimeOfDay 23 13 59)) $ pl @(Guard "hh:mm:ss regex failed" (Re HmsRE Id) >> ReadP TimeOfDay Id) "23:13:59"
  , expectPE (FailT "hh:mm:ss regex failed") $ pl @(Guard "hh:mm:ss regex failed" (Re HmsRE Id) >> ReadP TimeOfDay Id) "23:13:60"
  , expectPE (FailT "Guards:invalid length(5) expected 3") $ pl @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]) [31,11,2000,1,2::Int]
  , expectPE (PresentT [0,0,0,0,0,0,0,1,2,3]) $ pl @(PadL 10 0 Id) [1..3]
  , expectPE (PresentT (124,["1","2","2"])) $ pl @('Left Id >> (Succ Id &&& (Pred Id >> ShowP Id >> Ones Id))) (Left 123)
  , expectPE (PresentT [1,2,3,4]) $ pl @(GuardsN (PrintT "guard(%d) %d is out of range" Id) 4 (Between 0 255 Id)) [1,2,3,4::Int]
  , expectPE (FailT "Guards:invalid length(5) expected 4") $ pl @(GuardsN (PrintT "guard(%d) %d is out of range" Id) 4 (Between 0 255 Id)) [1,2,3,4,5::Int]
  , expectPE (FailT "Guards:invalid length(3) expected 4") $ pl @(GuardsN (PrintT "guard(%d) %d is out of range" Id) 4 (Between 0 255 Id)) [1,2,3::Int]
  , expectPE (PresentT (readNote @UTCTime "failed to read utc" "1999-01-01 12:12:12 UTC")) $ pl @(ParseTimeP UTCTime "%F %T" Id) "1999-01-01 12:12:12"
  , expectPE (PresentT 123) $ pl @(JustDef 0 Id) (Just 123)
  , expectPE (PresentT 0) $ pl @(JustDef 0 Id) Nothing
  , expectPE (PresentT 12) $ pl @(LastDef 0 Id) [1..12]
  , expectPE (PresentT 0) $ pl @(LastDef 0 Id) []
  , expectPE (PresentT (1,("asdf",True))) $ pl @'(1,'("asdf",'True)) ()
  , expectPE (PresentT ("abc", True)) $ pl @(TheseId 'True "xyz") (This "abc")
  , expectPE (PresentT ("xyz", False)) $ pl @(TheseId 'True "xyz") (That False)
  , expectPE (PresentT ("abc", False)) $ pl @(TheseId 'True "xyz") (These "abc" False)
  , expectPE (PresentT ("xyz", True)) $ pl @(TheseDef '("xyz",'True) Id) (This "abc")
  , expectPE (PresentT ("xyz", True)) $ pl @(TheseDef '("xyz",'True) Id) (That False)
  , expectPE (PresentT ("abc", False)) $ pl @(TheseDef '("xyz",'True) Id) (These "abc" False)
  , expectPE (PresentT 3) $ pl @(Id !! Char1 "d") (M.fromList $ zip "abcd" [0 ..])
  , expectPE (PresentT (12, False)) $ pl @('These Id (Not Id)) (These 12 True)
  , expectPE (PresentT (SG.Any True)) $ pl @(Coerce SG.Any) True
  , expectPE (PresentT True) $ pl @(Coerce Bool) (SG.Any True)
  , expectPE (PresentT (3, SG.Any True)) $ pl @(Id !! FromString _ "d" &&& (Map (Snd Id >> Gt 3 >> Coerce SG.Any) (IToList _ Id) >> MConcat Id) ) (M.fromList $ zip (map T.singleton "abcdefgh") [0 ..])
  , expectPE (PresentT (3, True)) $ pl @(Id !! FromString _ "d" &&& (Map (Snd Id >> Gt 3 >> Wrap SG.Any Id) (IToList _ Id) >> MConcat Id >> Unwrap Id) ) (M.fromList $ zip (map T.singleton "abcdefgh") [0 ..])
    --- have to wrap with W cos different kinds
--  , expectPE TrueT $ pl @(Do '[ W ('PresentT I), W 'FalseT, Not Id]) False
--  , expectPE FalseT $ pl @(Do '[ W ('PresentT Id), W 'FalseT ]) True -- have to wrap them cos BoolT a vs BoolT Bool ie different types
--  , expectPE TrueT $ pl @('PresentT I >> Not 'FalseT) False
  -- IxL "d" doesnt work cos is Text not String
  , expectPE (PresentT 3) $ pl @(Id !! FromString _ "d") (M.fromList $ zip (map T.singleton "abcd") [0 ..])
  -- use Fromstring
  , expectPE (PresentT 3) $ pl @(Id !! FromString _ "d") (M.fromList $ zip (map T.singleton "abcd") [0 ..])
  , expectPE (PresentT [7,9,9,2,7,3,9,8,7,1,3]) $ pl @(Map (ReadP Int Id) (Ones Id) >> Guard "checkdigit fail" (Luhn Id)) "79927398713"
  , expectPE (FailT "checkdigit fail") $ pl @(Map (ReadP Int Id) (Ones Id) >> Guard "checkdigit fail" (Luhn Id)) "79927398714"
  , expectPE (PresentT [10,14,15,9]) $ pl @(MM1 16 >> MM2 16) "aef9"
  , expectPE (FailT "invalid base 16") $ pl @(MM1 16 >> MM2 16) "aef9g"
  , expectPE (FailT "found empty") $ pl @(MM1 16 >> MM2 16) ""
  , expectPE (FailT "0<=x<n") $ pl @(MM2 16) [10,1,17,1,-3,7]
  , expectPE (PresentT ((10,'c'),True)) $ pl @Unassoc (10,('c',True))
  , expectPE (PresentT (10,('c',True))) $ pl @Assoc ((10,'c'),True)
  , expectPE (PresentT ((10,'c'),True)) $ pl @(Assoc >> Unassoc) ((10,'c'),True)
  , expectPE (PresentT 70) $ pl @(Luhn' 11) "79927398713"
  , expectPE (FailT "expected 71 mod 10 = 0 but found 1") $ pl @(Luhn' 11) "79927398714"

-- works but way to difficult: use Guard to do all the work
--  >pl @(((Rescan "([[:xdigit:]])" >> Map (Snd Id) >> (Id &&& Len)) &&& Len) >> Guard "notallmatched" ((Snd Id *** Id) >> Fst Id == Snd Id)) "134F"
-- have to check the length of the match vs input to see that are the same
  , expectPE (PresentT [1,3,4,15]) $ pl @(((Rescan "([[:xdigit:]])" Id >> Map (Snd Id >> OneP Id >> ReadBase Int 16 Id) Id) &&& Id) >> Guard "notallmatched" ((Len *** Len) >> Fst Id == Snd Id) >> Fst Id) "134F"
  , expectPE (FailT "notallmatched") $ pl @(((Rescan "([[:xdigit:]])" Id >> Map (Snd Id >> OneP Id >> ReadBase Int 16 Id) Id) &&& Id) >> Guard "notallmatched" ((Len *** Len) >> Fst Id == Snd Id) >> Fst Id) "134g"
  , expectPE (PresentT True) $ pl @(FoldMap SG.Any Id) [False,False,True,False]
  , expectPE (PresentT False) $ pl @(FoldMap SG.All Id) [False,False,True,False]
  , expectPE TrueT $ pl @(Map (ReadP _ Id) (Ones Id) >> Luhn Id) "12345678903"
  , expectPE FalseT $ pl @(Map (ReadP _ Id) (Ones Id) >> Luhn Id) "12345678904"
  , expectPE (FailT "incorrect number of digits found 10 but expected 11 in [1234567890]") $ pl @(Luhn' 11) "1234567890"
  , expectPE (PresentT ([1,2],[3,4,5,6,7,8])) $ pl @(Break (If (Gt 2) 'True (If (Gt 4) (Failt _ "ASfd") 'False)) Id) [1..8]
  , expectPE (PresentT ([1,2],[3,4,5,6,7,8])) $ pl @(Break (Case 'False '[Gt 2,Gt 4] '[ W 'True, Failt _ "ASfd"] Id) Id) [1..8]  -- case version
  , expectPE (FailT "ASfd") $ pl @(Break (If (Gt 2) (Failt _ "ASfd") 'False) Id) [1..8]
  , expectPE (PresentT ([(1,False),(2,False),(3,False)],[(4,True),(5,True),(6,False)])) $ pl @(Break (Snd Id) Id) (zip [1..] [False,False,False,True,True,False])
  , expectPE (PresentT ([(1,False),(2,False),(3,False),(4,False)],[])) $ pl @(Break (Snd Id) Id) (zip [1..] [False,False,False,False])
  , expectPE (PresentT ([],[(1,True),(2,True),(3,True),(4,True)])) $ pl @(Break (Snd Id) Id) (zip [1..] [True,True,True,True])
  , (@?=) (Just "abc") ((_FailT # "abc") ^? _FailT)
  , (@?=) (Just ()) ((_TrueT # ()) ^? _TrueT)
  , (@?=) (Just ()) ((_FalseT # ()) ^? _FalseT)
  , (@?=) (Just 'x') ((_PresentT # 'x') ^? _PresentT)
  , expectPE (PresentT (111,'b')) $ pl @('(123,Char1 "c") >> (Id - 12 *** Pred Id)) ()
  , expectPE (PresentT (SG.Min 19)) $ pl @((FromInteger _ 12 &&& Id) >> Fst Id + Snd Id) (SG.Min 7)
  , expectPE (PresentT (SG.Product 84)) $ pl @((FromInteger _ 12 &&& Id) >> SapA) (SG.Product 7)
  , expectPE (PresentT "xyxyxyxy") $ pl @(STimes (Fst Id) (Snd Id)) (4,['x','y'])
  , expectPE (PresentT (concat (replicate 16 "abc"))) $ pl @(FoldN 4 Id ((Id &&& Id) >> SapA)) "abc"
  , expectPE (PresentT (concat (replicate 4 "abc"))) $ pl @(STimes (Fst Id) (Snd Id)) (4,"abc")
  , expectPE (PresentT (concat (replicate 4 "abc"))) $ pl @(STimes 4 Id) "abc"
  , expectPE (PresentT "abcd") $ pl @(Map (FromEnum Id) Id >> Map (ToEnum Char Id) Id) ("abcd" :: String)
  , expectPE (FailT "ToEnum IO e=Prelude.Enum.Ordering.toEnum: bad argument(2)") $ pl @(Map (FromEnum Id) Id >> Map (Id - 97 >> ToEnum Ordering Id) Id) ("abcde" :: String)
  , expectPE (PresentT ([2,3,5,7,11,13], [1,4,6,8,9,10,12,14,15])) $ pl @(Partition (Prime Id) Id) [1..15]
  , expectPE (FailT "'Nothing found Just") $ pl @'Nothing (Just 12)
  , expectPE (PresentT (Just 10,((),()))) $ pl @(Id &&& '() &&& ()) (Just 10)
  , expectPE (PresentT [(-999) % 1,10 % 1,20 % 1,(-999) % 1,30 % 1]) $ pl @(Map (Wrap (MM.First _) Id &&& (Pure Maybe (999 -% 1 ) >> Wrap (MM.First _) Id)) Id >> Map SapA Id >> Map (Just (Unwrap Id)) Id) [Nothing,Just 10,Just 20,Nothing,Just 30]
  , expectPE (PresentT 12) $ pl @(MaybeIn 99 Id) (Just 12)
  , expectPE (PresentT 12) $ pl @(JustDef 99 Id) (Just 12)
  , expectPE (PresentT 99) $ pl @(MaybeIn 99 Id) Nothing
  , expectPE (PresentT 99) $ pl @(JustDef 99 Id) Nothing
  , expectPE (PresentT (-99)) $ pl @(MaybeIn (99 -% 1 ) Id) Nothing
  , expectPE (PresentT (-99)) $ pl @(JustDef (99 -% 1 ) Id) Nothing
  , expectPE (PresentT [1,2,3,4,12]) $ pl @(ParaN 5 (Guard "0-255" (Between 0 255 Id))) [1,2,3,4,12]
  , expectPE (FailT "0-255") $ pl @(ParaN 5 (Guard "0-255" (Between 0 255 Id))) [1,2,3,400,12]
  , expectPE (PresentT ["141","021","003","000"]) $ pl @(ParaN 4 (PrintF "%03d" Id)) [141,21,3,0::Int]

  -- need to fill in the types for both even in ghci
  , expectPE (PresentT (Just (SG.Sum 10))) $ pl @(Coerce2 (SG.Sum Int)) (Just (10 :: Int))
  , expectPE (PresentT (Just (SG.Sum 0))) $ pl @(MEmpty2 (SG.Sum _)) (Just ())
  , expectPE (PresentT 13) $ pl @(FoldMap (SG.Sum _) Id) (Just 13)
  , expectPE (PresentT 55) $ pl @(FoldMap (SG.Sum _) Id) [1..10]
  , expectPE (PresentT [Just 1,Just 2,Just 3,Just 4]) $ pl @Sequence (Just [1..4])
  , expectPE (PresentT (Just (SG.Sum 20))) $ pl @(Pure2 SG.Sum) (Just 20)
  , expectPE (PresentT Nothing) $ pl @(Traverse (If (Gt 3) (Pure Maybe Id) (EmptyT Maybe Id)) Id) [1..5]
  , expectPE (PresentT Nothing) $ pl @(Traverse (MaybeBool (Le 3) Id) Id) [1..5]
  , expectPE (PresentT (Just [1,2,3,4,5])) $ pl @(Traverse (If (Gt 0) (Pure Maybe Id) (EmptyT Maybe Id)) Id) [1..5]
  , expectPE (PresentT (Just [1,2,3,4,5])) $ pl @(Traverse (If (Gt 0) (Pure Maybe Id) (MkNothing _)) Id) [1..5]
  , expectPE (PresentT (Just [1,2,3,4,5])) $ pl @(Traverse (MaybeBool (Id >= 0) Id) Id) [1..5]
  , expectPE (PresentT Nothing) $ pl @(Traverse (MaybeBool (Id <= 3) Id) Id) [1..5]

  , expectPE (FailT "PrintF (IO e=printf: bad formatting char 's')") $ pl @(PrintF "%-6s" Id) (1234 :: Int)
  , expectPE (PresentT "0004d2") $ pl @(PrintF "%06x" Id) (1234 :: Int)
  , expectPE (PresentT (Left 123)) $ pl @(Pure (Either String) Id >> Swap) 123
  , expectPE (PresentT [13,2,1999]) $ pl @(Rescan DdmmyyyyRE Id >> OneP Id >> Map (ReadP Int Id) (Snd Id)) "13-02-1999"
  , expectPE (PresentT [3,2,1999]) $ pl @(Rescan DdmmyyyyRE Id >> OneP Id >> Map (ReadP Int Id) (Snd Id) >> Ddmmyyyyop) "03-02-1999"
  , expectPE (FailT "month 13 is out of range") $ pl @(Rescan DdmmyyyyRE Id >> OneP Id >> Map (ReadP Int Id) (Snd Id) >> Ddmmyyyyop) "12-13-1999"
  , expectPE (PresentT [[1],[2,3,4],[5,6,7,8],[9,10,11,12]]) $ pl @(SplitAts '[1,3,4] Id) [1..12]
  , expectPE (PresentT [[1,2,3],[4]]) $ pl @(SplitAts '[3,1,1,1] Id >> Filter (Not Null) Id) [1..4]
  , expectPE (PresentT 1) $ pl @(Msg (PrintF "digits=%d" Len) (Head Id)) [1..4]
  , expectPE (PresentT 10) $ pl @(Luhn' 4) "1230"
  , expectPE (FailT "expected 14 mod 10 = 0 but found 4") $ pl @(Luhn' 4) "1234"
  , expectPE (PresentT "lhs = 123 rhs = asdf") $ pl @(PrintT "lhs = %d rhs = %s" Id) (123::Int,"asdf"::String)
  , expectPE TrueT $ pl @(DirExists ".") ()
  , expectPE FalseT $ pl @(DirExists "xxy") ()
  , expectPE FalseT $ pl @(FileExists "xxy") ()
  , expectPE TrueT $ pl @(IsInfix "ab" Id) "xyzabw"
  , expectPE FalseT $ pl @(IsInfix "aB" Id) "xyzAbw"
  , expectPE TrueT $ pl @(IsInfixI "aB" Id) "xyzAbw"
  , expectPE FalseT $ pl @(IsInfix "ab" Id) "xyzbaw"
  , expectPE TrueT $ pl @(IsPrefix "xy" Id) "xyzabw"
  , expectPE FalseT $ pl @(IsPrefix "ab" Id) "xyzbaw"
  , expectPE TrueT $ pl @(IsSuffix "bw" Id) "xyzabw"
  , expectPE FalseT $ pl @(IsSuffix "bw" Id) "xyzbaw"
  , expectPE TrueT $ pl @(IsInfix (Fst Id) (Snd Id)) ("ab","xyzabw")
  , expectPE (PresentT [1 % 1,(-3) % 2,(-3) % 1]) $ pl @'[1 % 1 ,3 -% 2,3 -% 1 ] ()
  , expectPE (PresentT [4, 7, 8, 9]) $ pl @'[4,7,8,9] ()
  , expectPE (PresentT ["aa","b","","ddd"]) $ pl @'["aa","b","","ddd"] ()
  , expectPE (PresentT 17) $ pl @(DoN 4 (Id + 4)) 1
  , expectPE (PresentT 24) $ pl @((Id <> Id) >> Unwrap Id) (SG.Sum 12)
  , expectPE (PresentT "abcdef") $ pl @(Fst Id <> (Snd Id >> Fst Id)) ("abc",("def",12))
  , expectPE (PresentT (SG.Sum 25)) $ pl @(Wrap _ 13 <> Id) (SG.Sum @Int 12)
  , expectPE (PresentT 23) $ pl @(Fst Id + Last (Snd Id)) (10,[12,13])
  , expectPE (PresentT (-1,12)) $ pl @(DivMod (9 - Fst Id) (Last (Snd Id))) (10,[12,13])
  , expectPE (PresentT [True,False,False,True]) $ pl @(Para '[ W 'True, Ge 12, W 'False, Lt 2 ]) [1,2,-99,-999]
  , expectPE (FailT "Para:invalid length(3) expected 4") $ pl @(Para '[ W 'True, Ge 12, W 'False, Lt 2 ]) [1,2,-99]
  , expectPE (FailT "Para:invalid length(7) expected 4") $ pl @(Para '[ W 'True, Ge 12, W 'False, Lt 2 ]) [1,2,-99,-999,1,1,2]
  , expectPE (FailT "guard(1) err 002") $ pl @(GuardsQuick (PrintT "guard(%d) err %03d" Id) '[ W 'True, Ge 12, W 'False, Lt 2 ]) [1,2,-99,-999]
  , expectPE (FailT "Guards:invalid length(3) expected 4") $ pl @(GuardsQuick (PrintT "guard(%d) err %03d" Id) '[ W 'True, Ge 12, W 'False, Lt 2 ]) [1,2,-99]
  , expectPE (FailT "Guards:invalid length(7) expected 4") $ pl @(GuardsQuick (PrintT "guard(%d) err %03d" Id) '[ W 'True, Ge 12, W 'True, Lt 2 ]) [1,22,-99,-999,1,1,2]
  , expectPE TrueT $ pl @(Fst Id /= Snd Id) ("ab","xyzabw")
  , expectPE FalseT $ pl @(Fst Id == Snd Id) ("ab","xyzabw")
  , expectPE (PresentT 157) $ pl @(Fst Id * (Snd Id >> Fst Id) + (Snd Id >> Snd Id) `Div` 2) (12,(13,3))
  , expectPE TrueT $ pl @(Fst Id >= Snd Id || Snd Id > 23 || 12 -% 5 <= ToRational (Fst Id)) (12,13)
  , expectPE (PresentT LT) $ pl @(Fst Id ==! Snd Id) (3,12)
  , expectPE TrueT $ pl @(Fst Id ==~ Snd Id) ("aBc","AbC")
  , expectPE (PresentT EQ) $ pl @(Fst Id ===~ Snd Id) ("aBc","AbC")
  , expectPE FalseT $ pl @(Fst Id == Snd Id) ("aBc","AbC")
  , expectPE (PresentT GT) $ pl @(Fst Id ==! Snd Id) ("aBc","AbC")
  , expectPE (PresentT LT) $ pl @(Snd Id ==! Fst Id) ("aBc","AbC")
  , expectPE TrueT $ pl @(Fst Id ==~ Snd Id && Fst Id == Snd Id) ("Abc","Abc")
  , expectPE (PresentT (EQ,EQ)) $ pl @(Fst Id ===~ Snd Id &&& Fst Id ==! Snd Id) ("abc","abc")
  , expectPE (PresentT "ask%dfas%kef00035 hey %") $ pl @(PrintF "ask%%dfas%%kef%05d hey %%" Id) (35 :: Int)
  , expectPE (PresentT 100) $ pl @(Id !! 2 !! 0) [[1..5],[10..14],[100..110]]
  , expectPE (FailT "(!!) index not found") $ pl @(Id !! 1 !! 7) [[1..5],[10..14],[100..110]]
  , expectPE (PresentT '2') $ pl @(IxL Id 1 (Char1 "x")) ("123" :: T.Text)
  , expectPE (PresentT 'x') $ pl @(IxL Id 15 (Char1 "x")) ("123" :: T.Text)
  , expectPE (FailT "someval int=45") $ pl @(Fail () (PrintF "someval int=%d" Id)) (45 :: Int)
  , expectPE (FailT "failing with 45") $ pl @(If (Gt 4) (Fail (Hole _) (PrintF "failing with %d" Id)) ()) 45
  , expectPE (PresentT 21) $ pl @(If (Gt 4) (Fail (Hole _) (PrintF "failing with %d" Id)) (Id * 7)) 3
  , expectPE (PresentT ["2","1"]) $ pl @(If (Gt 4) (Fail (Hole _) (PrintF "failing with %d" Id)) (Id * 7 >> ShowP Id >> Ones Id)) 3
  , expectPE (FailT "failing with 19") $ pl @(If (Gt 4) (Fail (Hole _) (PrintF "failing with %d" Id)) (ShowP (Id * 7) >> Ones Id)) 19
  , expectPE (PresentT 31) $ pl @(DoN 4 (Id + 7)) 3
  , expectPE (PresentT 9) $ pl @(DoN 4 9) ()
  , expectPE (PresentT 3) $ pl @(Do '[1,2,3]) ()
  , expectPE (PresentT "xy") $ pl @(DoN 4 "xy") 3
  , expectPE (PresentT ["xy","xy","xy","xy"]) $ pl @(Repeat 4 "xy") 3
  , expectPE (PresentT (Proxy @'["xy","xy","xy","xy"])) $ pl @(Proxy (RepeatT 4 "xy")) 3
  , expectPE (PresentT (This @_ @() 'x')) $ pl @(MkThis () Id) 'x'
  , expectPE (PresentT (This @_ @() 'x')) $ pl @(MkThis () (Fst Id)) ('x',True)
  , expectPE (PresentT (That 'x')) $ pl @(MkThat () Id) 'x'
  , expectPE (PresentT (These 'x' True)) $ pl @(MkThese Id 'True) 'x'
  , expectPE (PresentT 123) $ pl @(MaybeIn 123 Id) (Nothing @Int)
  , expectPE (PresentT 9) $ pl @(MaybeIn 123 Id) (Just 9)
  , expectPE (PresentT [1,2,3]) $ pl @(Just Id) (Just [1,2,3])
  , expectPE (FailT "Just(empty)") $ pl @(Just Id) (Nothing @[Int])
  , expectPE (PresentT (66788,26232)) $ pl @(Last Id >> Id * 123 >> Dup >> (Pred Id *** (ShowP Id >> Rescan "(\\d{2})" Id >> Concat (ConcatMap (Snd Id) Id) >> ReadBase Int 16 Id))) [12,13,543::Int]
  , expectPE (PresentT "d=009 s=ab") $ pl @(PrintT "d=%03d s=%s" Id) (9::Int,"ab"::String)
  , expectPE (PresentT "d=009 s=ab c=x f=1.54") $ pl @(PrintT "d=%03d s=%s c=%c f=%4.2f" Id) (9::Int,"ab"::String,'x',1.54::Float)
  , expectPE (FailT "PrintT(IO e=printf: formatting string ended prematurely)") $ pl @(PrintT "d=%03d s=%s" Id) (9::Int, "ab"::String,'x',1.54::Float)
  , expectPE (PresentT "lhs = 123 rhs = asdf c=120") $ pl @(PrintT "lhs = %d rhs = %s c=%d" Id) (123::Int,"asdf"::String,'x')
  , expectPE (PresentT (1,('x',(True,())))) $ pl @(Fst Id &&& Snd Id &&& Thd Id &&& ()) (1,'x',True)
  , expectPE (PresentT (1,('x',(True,())))) $ pl @(Fst Id &&& Snd Id &&& Thd Id &&& ()) (1,'x',True)
  , expectPE (PresentT (1,(1.4,("aaa",())))) $ pl @(Fst Id &&& Snd Id &&& Thd Id &&& ()) (1,1.4,"aaa")
  , expectPE (PresentT "hello d=12 z someval") $ pl @(PrintT "hello d=%d %c %s" '(12, Char1 "z", "someval")) ()
  , expectPE (PresentT "ipaddress 001.002.003.004") $ pl @(PrintT "ipaddress %03d.%03d.%03d.%03d" '(1,2,3,4)) ()

  , expectPE (PresentT "001.002.003.004") $ pl @(PrintL 4 "%03d.%03d.%03d.%03d" Id) [1,2,3,4::Int]
  , expectPE (FailT "PrintL(4) arg count=5") $ pl @(PrintL 4 "%03d.%03d.%03d.%03d" Id) [1,2,3,4,5::Int]
  , expectPE (FailT "PrintL(4) arg count=3") $ pl @(PrintL 4 "%03d.%03d.%03d.%03d" Id) [1,2,3::Int]

  , expectPE (PresentT "001.002.003.004") $ pl @(PrintL 4 "%03d.%03d.%03d.%03d" Id) [1,2,3,4::Int]
  , expectPE (FailT "Pairs no data found") $ pl @Pairs ([] :: [()])
  , expectPE (FailT "Pairs only one element found") $ pl @Pairs [1]
  , expectPE (PresentT [(1,2)]) $ pl @Pairs [1,2]
  , expectPE (PresentT [(1,2),(2,3)]) $ pl @Pairs [1,2,3]
  , expectPE (PresentT [(1,2),(2,3),(3,4)]) $ pl @Pairs [1,2,3,4]
  , expectPE (PresentT "1    2 3 004") $ pl @(PrintL 4 "%d %4d %-d %03d" Id) [1..4::Int]
  , expectPE (PresentT "2019-08-17") $ pl @(FormatTimeP "%Y-%m-%d" Id) (readNote @Day "invalid day" "2019-08-17")
  , expectPE (PresentT (20,20)) $ pl @(Dup << Fst Id * Snd Id) (4,5)
  , expectPE (PresentT (20,20)) $ pl @(Fst Id * Snd Id >> Dup) (4,5)
  , expectPE (PresentT (These "xxx" 4)) $ pl @(Fst Id <$ Snd Id) (4,These "xxx" 'a')
  , expectPE (PresentT (This 'a')) $ pl @(Fst Id <$ Snd Id) (4,This @_ @String 'a')
  , expectPE (PresentT (Just 4)) $ pl @(Fst Id <$ Snd Id) (4,Just 'a')
  , expectPE (PresentT Nothing) $ pl @(Fst Id <$ Snd Id) (4,Nothing @Int)
  , expectPE (PresentT (Just 4)) $ pl @(Fst Id <* Snd Id) (Just 4,Just 'a')
  , expectPE (PresentT (Just 'a')) $ pl @(Fst Id *> Snd Id) (Just 4,Just 'a')
  , expectPE (PresentT ('x',('x',"someval"))) $ pl @Duplicate ('x',"someval")
  , expectPE (PresentT "someval") $ pl @Extract ('x',"someval")
  , expectPE (PresentT (Just "cdef")) $ pl @(Fst Id <|> Snd Id) (Just "cdef",Just "ab")
  , expectPE (PresentT "cdefab") $ pl @(Fst Id <|> Snd Id) ("cdef","ab"::String)
  , expectPE (PresentT (9,"abc")) $ pl @(I $$ 9 $$ "abc") (,)
  , expectPE (PresentT ("abc",9)) $ pl @(9 $& "abc" $& I) (,)
  , expectPE (PresentT "28") $ pl @(Fst Id $$ Snd Id) (show . (7*),4)
  , expectPE (PresentT (12,"12")) $ pl @(Fst Id $$ Snd Id $$ ShowP (Snd Id)) ((,),12)
--  , expectPE (PresentT (Just (This [1,2,3,4]))) $ pl @(ZipTheseF (Fst Id) (Snd Id)) (Just [1..4],Nothing @())
--  , expectPE (PresentT [These 1 'a',These 2 'b',These 3 'c',This 4]) $ pl @(ZipTheseF (Fst Id) (Snd Id)) ([1..4],['a'..'c'])
  , expectPE (PresentT [True,True,True,True]) $ pl @('True <$ Id) [1..4]
  , expectPE (PresentT (Compose (Just "aaaa"))) $ pl @(Char1 "ab" <$ Id) (Compose $ Just [1..4])
  , expectPE (PresentT (4,("aa",'x'))) $ pl @'(4,'(Fst Id,Snd Id)) ("aa",'x')
  , expectPE (PresentT (4,"aa",'x')) $ pl @'(4,(Fst Id),Snd Id) ("aa",'x')
  , expectPE (PresentT (Just [10])) $ pl @(Pure2 []) (Just 10)
  , expectPE (PresentT "hello") $ pl @Extract (10,"hello")
  , expectPE (PresentT (M.fromList [(4,"x"),(5,"dd")])) $ pl @(FromList (M.Map _ _)) [(4,"x"),(5,"dd")]
  , expectPE (PresentT False) $ pl @(FromList (M.Map _ _) >> I !! Char1 "y") [('x',True),('y',False)]
  , expectPE (PresentT (Just False)) $ pl @(FromList (M.Map _ _) >> Lookup Id (Char1 "y")) [('x',True),('y',False)]
  , expectPE (PresentT Nothing) $ pl @(FromList (M.Map _ _) >> Lookup Id (Char1 "z")) [('x',True),('y',False)]
  , expectPE (FailT "(!!) index not found") $ pl @(FromList (M.Map _ _) >> Id !! Char1 "z") [('x',True),('y',False)]
  , expectPE (PresentT ["abc","bcd","cde","def","efg","fgh","ghi","hi","i"]) $ pl @(Unfoldr (If Null (MkNothing _) ('(Take 3 Id, Drop 1 Id) >> MkJust Id)) Id) "abcdefghi"
  , expectPE (PresentT [[1,2],[3,4],[5]]) $ pl @(Unfoldr (If Null (MkNothing _) (Pure _ (SplitAt 2 Id))) Id) [1..5]
  , expectPE (PresentT [[1,2],[3,4],[5]]) $ pl @(Unfoldr (MaybeBool (Not Null) (SplitAt 2 Id)) Id) [1..5]
  , expectPE (PresentT [99,1,2,3,4,5]) $ pl @(FlipT (:+) (Fst Id) (Snd Id)) ([1..5],99)
  , expectPE (PresentT [99,1,2,3,4,5]) $ pl @(Fst Id :+ Snd Id) (99,[1..5])
  , expectPE (PresentT [[99],[1,99],[2,1,99],[3,2,1,99],[4,3,2,1,99],[5,4,3,2,1,99]]) $ pl @(Scanl (Snd Id :+ Fst Id) (Fst Id) (Snd Id)) ([99],[1..5])
  , expectPE (PresentT [[99]]) $ pl @(Scanl (Snd Id :+ Fst Id) (Fst Id) (Snd Id)) ([99],[])
  , expectPE (FailT "yy") $ pl @(Unfoldr (If Null (MkNothing _) (Guard "yy" (Len < 3) >> Pure _ (SplitAt 2 Id))) Id) [1..5]
  , expectPE (FailT "yy") $ pl @(Unfoldr (MaybeBool (Not Null) (Guard "yy" (Len < 3) >> SplitAt 2 Id)) Id) [1..5]
  , expectPE (PresentT [4,1,2,3]) $ pl @(4 :+ '[1,2,3]) ()
  , expectPE (PresentT [1,2,3,4]) $ pl @('[1,2,3] +: 4) ()
  , expectPE (PresentT [4,1,2,3]) $ pl @(Fst Id :+ Snd Id) (4,[1,2,3])
  , expectPE (PresentT [1,2,3,4]) $ pl @(Snd Id +: Fst Id) (4,[1,2,3])
  , expectPE (PresentT "abcx") $ pl @("abc" +: Char1 "x") ()
  , expectPE (PresentT "abcx") $ pl @(Fst Id +: Snd Id) ("abc" :: T.Text,'x')
  , expectPE (PresentT [5,1,2,3]) $ pl @(FlipT (:+) '[1,2,3] 5) ()
  , expectPE (PresentT (map ModifiedJulianDay [0,1,2,3,4,5])) $ pl @(EnumFromTo (Fst Id) (Snd Id)) (ModifiedJulianDay 0, ModifiedJulianDay 5)
  , expectPE (PresentT (map ModifiedJulianDay [0,1,2,3,4,5])) $ pl @((ToEnum Day Id *** ToEnum Day Id) >> EnumFromTo (Fst Id) (Snd Id)) (0,5)
  , expectPE (FailT "xx") $ pl @(Unfoldr (Guard "xx" (Len > 4) >> Uncons) Id) [1..10]
  , expectPE (PresentT [1,2,3,4,5,6,7,8,9,10]) $ pl @(Unfoldr Uncons Id) [1..10]
  , expectPE (PresentT [99,98,97,96]) $ pl @(IterateN 4 (Pred Id)) 99
  , expectPE (PresentT (4,'x')) $ pl @('(,) 4 %% Char1 "x") ()
  , expectPE (PresentT (Just False)) $ pl @(FromList (M.Map _ _) >> Lookup Id %% Char1 "y") [('x',True),('y',False)]
  , expectPE (PresentT (4,"abc")) $ pl @('(,) %% 4 %% "abc") ()
  , expectPE (PresentT ("abc",4)) $ pl @(4 %& "abc" %& '(,)) ()
  , expectPE (PresentT ("abc",4)) $ pl @(FlipT '(,) 4 "abc") ()
  , expectPE (PresentT (1,[])) $ pl @(Uncons >> MaybeIn '(1,MEmptyT _) Id) []
  , expectPE (PresentT []) $ pl @'[] 4
  , expectPE (PresentT (SG.Sum 3)) $ pl @(FromInteger (SG.Sum _) (Fst Id)) (3,"A")
  , expectPE (PresentT (123 :: DiffTime)) $ pl @(FromInteger DiffTime 123) 'x'
  , expectPE (PresentT (0.8 :: Float)) $ pl @(FromRational Float (4 % 5)) ()
  , expectPE (PresentT (14 % 1)) $ pl @(ToRational 14) ()
  , expectPE (PresentT ('y',3)) $ pl @(Id !! 1) [('x',14),('y',3),('z',5)]
  , expectPE (PresentT (Just ('y',3))) $ pl @(Lookup Id 1) [('x',14),('y',3),('z',5)]
  , expectPE (PresentT Nothing) $ pl @(Lookup Id 14) [('x',14),('y',3),('z',5)]
  , expectPE (FailT "(!!) index not found") $ pl @(Id !! 14) [('x',14),('y',3),('z',5)]
  , expectPE (PresentT 99) $ pl @(Fst Id) (99,'a',False,1.3)
  , expectPE (PresentT 'a') $ pl @(Snd Id) (99,'a',False,1.3)
  , expectPE (PresentT False) $ pl @(Thd Id) (99,'a',False,1.3)
  , expectPE (PresentT "someval") $ pl @(L4 Id) (99,'a',False,"someval")
  , expectPE (PresentT [1,-5,5,-1]) $ pl @('[1 % 1 ,Negate (33 % 7), 21 % 4,Signum (7 -% 5)] >> Map (Floor _ Id) Id) ()
  , expectPE (PresentT [1,-4,6,-1]) $ pl @('[1 % 1 ,Negate (33 % 7), 21 % 4,Signum (7 -% 5)] >> Map (Ceiling _ Id) Id) ()
  , expectPE (PresentT [1,-4,5,-1]) $ pl @('[1 % 1 ,Negate (33 % 7), 21 % 4,Signum (7 -% 5)] >> Map (Truncate _ Id) Id) ()
  , expectPE (PresentT @Integer 2) $ pl @(Truncate' (Fst Id >> Unproxy ) (Snd Id)) (Proxy @Integer,2.3)
  , expectPE (PresentT @Int 2) $ pl @(Truncate' (Fst Id) (Snd Id)) (1::Int,2.3)
  , expectPE (PresentT @Float 0.4) $ pl @(FromRational' (Fst Id) (Snd Id)) (1::Float,2 % 5)
  , expectPE (PresentT (5 % 3)) $ pl @(ToRational 5 / ToRational 3) 'x'
  , expectPE (PresentT (-5 % 3)) $ pl @(5 % 1 / 3 -% 1 ) 'x'
  , expectPE (PresentT (-5 % 3)) $ pl @(5 -% 1 / Fst Id) (3,'x')
  , expectPE (PresentT (-5 % 3)) $ pl @(Snd Id / Fst Id) (-3,5)
  , expectPE (FailT "(/) zero denominator") $ pl @(Snd Id / Fst Id) (0,5)
  , expectPE (PresentT 16) $ pl @(FoldL (Guard "someval" (Fst Id < Snd Id) >> Snd Id) (Head Id) (Tail Id)) [1,4,7,9,16]
  , expectPE (FailT "7 not less than 6") $ pl @(FoldL (Guard (PrintT "%d not less than %d" Id) (Fst Id < Snd Id) >> Snd Id) (Head Id) (Tail Id)) [1,4,7,6,16::Int]
  , expectPE (PresentT (True,16)) $ pl @(FoldL (If ((Fst Id >> Fst Id) && (Snd Id > Snd (Fst Id))) '( 'True, Snd Id ) '( 'False, Snd (Fst Id) )) '( 'True, Head Id ) (Tail Id)) [1,4,7,9,16]
  , expectPE (PresentT (False,16)) $ pl @(FoldL (If ((Fst Id >> Fst Id) && (Snd Id > Snd (Fst Id))) '( 'True, Snd Id ) '( 'False, Snd (Fst Id) )) '( 'True, Head Id ) (Tail Id)) [1,4,7,9,16,2]
  , expectPE (PresentT (False,7))
     $ pl @(FoldL (If (Fst (Fst Id))
                    (If (Snd Id > Snd (Fst Id))
                       '( 'True, Snd Id )
                       '( 'False, Snd (Fst Id) )
                    ) (Fst Id))
                   '( 'True, Head Id) (Tail Id)) [1,4,7,6,16]
  , expectPE (PresentT [1,2,3,4]) $ pl @(Init Id) [1..5]
  , expectPE (FailT "Init(empty)") $ pl @(Init Id) ([] :: [()])
  , expectPE (PresentT [2,3,4,5]) $ pl @(Tail Id) [1..5]
  , expectPE (FailT "Tail(empty)") $ pl @(Tail Id) ([] :: [()])
  , expectPE (PresentT [10,12,13]) $ pl @(CatMaybes Id) [Just 10, Just 12, Nothing, Just 13]
  , expectPE (PresentT [5,4,3,2,1]) $ pl @(FoldL (Snd Id :+ Fst Id) (MEmptyT [_]) Id) [1..5]
  , expectPE (PresentT (map SG.Min [9,10,11,12,13])) $ pl @(EnumFromTo (Pure SG.Min 9) (Pure _ 13)) ()
  , expectPE (PresentT (map SG.Min [9,10,11,12,13])) $ pl @(EnumFromTo (Wrap (SG.Min _) 9) (Wrap _ 13)) ()
--  , expectPE (PresentT (Just 'x')) $ pl @(Purex (Fst Id) (Snd Id)) (Just 10,'x')
  , expectPE (PresentT (Just 'x')) $ pl @(Snd Id <$ Fst Id) (Just 10,'x')
  , expectPE (PresentT (Nothing @(SG.Sum _))) $ pl @(MEmptyT' Id) (Just (SG.Sum 12))
  , expectPE (PresentT ([4,99],"xy")) $ pl @PartitionEithers [Left 4, Right 'x', Right 'y',Left 99]
  , expectPE (PresentT ([4,99],"xy",[(3,'b'),(5,'x')])) $ pl @PartitionThese [This 4, That 'x', That 'y',These 3 'b', This 99, These 5 'x']
  , expectPE (PresentT [1,2,3]) $ pl @(MapMaybe (MaybeBool (Le 3) Id) Id) [1..5]
  , expectPE (PresentT [4,5]) $ pl @(MapMaybe (MaybeBool (Gt 3) Id) Id) [1..5]
  , expectPE (PresentT [94,93,92,91]) $ pl @(IterateWhile (Id > 90) (Pred Id)) 94
  , expectPE (PresentT [94,93,92,91,90]) $ pl @(IterateUntil (Id < 90) (Pred Id)) 94
  , expectPE (PresentT [95,94,93,92,91]) $ pl @(IterateNWhile 10 (Id > 90) (Pred Id)) 95
  , expectPE (PresentT [95,94,93]) $ pl @(IterateNWhile 3 (Id > 90) (Pred Id)) 95
  , expectPE (PresentT [95,94,93,92,91]) $ pl @(IterateNUntil 10 (Id <= 90) (Pred Id)) 95
  , expectPE (PresentT [95,94,93]) $ pl @(IterateNUntil 3 (Id <= 90) (Pred Id)) 95
  -- check for infinite loops
  , expectPE (FailT "Unfoldr (9999,1):failed at i=100") $ pl @(IterateNUntil 9999 'False I) 1
  , expectPE (FailT "Scanl list size exceeded") $ pl @(FoldL (Fst Id) '() (EnumFromTo 1 9999)) ()
  , expectPE (PresentT "a=9 b=rhs") $ pl @(TheseX (PrintF "a=%d" (Succ (Snd Id))) ("b=" <> Snd Id) (PrintT "a=%d b=%s" (Snd Id)) Id) (These @Int 9 "rhs")
  , expectPE (PresentT "a=10") $ pl @(TheseX (PrintF "a=%d" (Succ (Snd Id))) ("b=" <> Snd Id) (PrintT "a=%d b=%s" (Snd Id)) Id) (This @Int 9)
  , expectPE (PresentT "b=rhs") $ pl @(TheseX (PrintF "a=%d" (Succ (Snd Id))) ("b=" <> Snd Id) (PrintT "a=%d b=%s" (Snd Id)) Id) (That @Int "rhs")
  , expectPE (PresentT ([] :: [Int])) $ pl @(HeadDef (MEmptyT _) Id) (map (:[]) ([] :: [Int]))
  , expectPE (PresentT ([10] :: [Int])) $ pl @(HeadDef (MEmptyT _) Id) (map (:[]) ([10..14] :: [Int]))
  , expectPE (PresentT 10) $ pl @(HeadDef (Fst Id) (Snd Id)) (99,[10..14])
  , expectPE (PresentT 99) $ pl @(HeadDef (Fst Id) (Snd Id)) (99,[] :: [Int])
  , expectPE (PresentT 43) $ pl @(HeadDef 43 (Snd Id)) (99,[] :: [Int])
  , expectPE (PresentT (Just 'd')) $ pl @(Lookup "abcdef" 3) ()
  , expectPE (PresentT (Just 5)) $ pl @(Lookup '[1,2,3,4,5,6] 4) ()
  , expectPE (PresentT 5) $ pl @(LookupDef '[1,2,3,4,5,6] 4 Id) 23
  , expectPE (PresentT 5) $ pl @(LookupDef '[1,2,3,4,5,6] 4 (Fst Id)) (23,'x')
  , expectPE (PresentT 23) $ pl @(LookupDef '[1,2,3,4,5,6] 99 Id) 23
  , expectPE (PresentT 23) $ pl @(LookupDef '[1,2,3,4,5,6] 99 (Fst Id)) (23,'x')
  , expectPE (PresentT 5) $ pl @(LookupDef '[1,2,3,4,5,6] 4 999) (23,'x')
  , expectPE (PresentT 999) $ pl @(LookupDef '[1,2,3,4,5,6] 40 999) (23,'x')
  , expectPE (PresentT (SG.Min 5)) $ pl @(LookupDef (Fst Id) 4 (MEmptyT _)) (map SG.Min [1::Int .. 10],'x')
  , expectPE (PresentT (mempty @(SG.Min _))) $ pl @(LookupDef (Fst Id) 999 (MEmptyT _)) (map SG.Min [1::Int .. 10],'x')
  , expectPE (FailT "someval") $ pl @(LookupFail "someval" (Fst Id) 999) (map SG.Min [1::Int .. 10],'x')
  , expectPE (FailT "abcsomeval") $ pl @(Fail (Snd Id >> Unproxy) (Fst Id <> "someval")) ("abc",Proxy @Int)
  , expectPE (FailT "char=x") $ pl @(LookupFail (PrintF "char=%c" (Snd Id)) (Fst Id) 49) (map SG.Min [1::Int ..10],'x')
  , expectPE (FailT "someval=13") $ pl @(LeftFail (PrintF "someval=%d" (Fst (Snd Id))) (Snd Id)) (13::Int,Right @(SG.Sum Int) "abc")
  , expectPE (FailT "someval=abc") $ pl @(LeftFail (PrintF "someval=%s" (Fst Id)) Id) (Right @(SG.Sum Int) ("abc" :: String))
  , expectPE (FailT "msg=Abc def") $ pl @(HeadFail (PrintF "msg=%s def" (Fst Id)) (Snd Id)) ("Abc" :: String,[]::[Int])
  , expectPE (PresentT 'c') $ pl @(LookupDef' (Fst Id) (Snd Id) (Char1 "xx") Id) (['a'..'e'],2)
  , expectPE (PresentT 'x') $ pl @(LookupDef' (Fst Id) (Snd Id) (Char1 "xx") Id) (['a'..'e'],999)
  , expectPE (PresentT 'x') $ pl @(LookupDef' (Fst Id) (Snd Id) (Char1 "xx") Id) ([],2)
  , expectPE (PresentT 'x') $ pl @(LookupDef' (Fst Id) (Snd Id) (Char1 "xx") (Snd Id)) ('w',([],2))
  , expectPE (PresentT 'c') $ pl @(LookupDef' (Fst Id) (Snd Id) (Fst Id) (Snd Id)) ('x',(['a'..'e'],2))
  , expectPE (PresentT(SG.Min 13)) $ pl @(LookupDef' (Fst Id) (Snd Id) (MEmptyT _) (Snd Id)) ('x',(map SG.Min [10..15::Int], 3))

  , expectPE (PresentT 9) $ pl @(HeadDef 9 (Fst Id)) ([],True)
  , expectPE (PresentT 1) $ pl @(HeadDef 9 (Fst Id)) ([1..5],True)
  , expectPE (PresentT 10) $ pl @(HeadDef 3 (Fst Id)) ([10..15],True)

  , expectPE (PresentT 9) $ pl @(LastDef 9 (Fst Id)) ([],True)
  , expectPE (PresentT 5) $ pl @(LastDef 9 (Fst Id)) ([1..5],True)
  , expectPE (PresentT 15) $ pl @(LastDef 3 (Fst Id)) ([10..15],True)

  , expectPE (PresentT [9,7]) $ pl @(InitDef '[9,7] (Fst Id)) ([],True)
  , expectPE (PresentT [1,2,3,4]) $ pl @(InitDef '[9,7] (Fst Id)) ([1..5],True)
  , expectPE (PresentT [10,11,12,13,14]) $ pl @(InitDef '[3] (Fst Id)) ([10..15],True)

  , expectPE (PresentT [9,7]) $ pl @(TailDef '[9,7] (Fst Id)) ([],True)
  , expectPE (PresentT [2,3,4,5]) $ pl @(TailDef '[9,7] (Fst Id)) ([1..5],True)
  , expectPE (PresentT [11,12,13,14,15]) $ pl @(TailDef '[3] (Fst Id)) ([10..15],True)

  , expectPE (FailT "a=4 b=someval") $ pl @(TailFail (PrintT "a=%d b=%s" (Snd Id)) (Fst Id)) ([]::[()],(4::Int,"someval" :: String))

  , expectPE (PresentT (Just 1)) $ pl @FMapFst (Just (1,'x'))
  , expectPE (PresentT (Just 'x')) $ pl @FMapSnd (Just (1,'x'))
  , expectPE (PresentT (Nothing @Int)) $ pl @FMapSnd (Nothing @(Char,Int))
  , expectPE (PresentT [1,2,3]) $ pl @FMapFst [(1,'x'), (2,'y'), (3,'z')]
  , expectPE (PresentT (Right 'x')) $ pl @FMapSnd (Right @() (1,'x'))
  , expectPE (PresentT (Left @_ @Double "x")) $ pl @FMapSnd (Left @_ @(Int,Double) "x")

  , expectPE (PresentT [1,10,99]) $ pl @Thiss [This 1, This 10,That 'x', This 99, That 'y']
  , expectPE (PresentT "xy") $ pl @Thats [This 1, This 10,That 'x', This 99, That 'y']
  , expectPE (PresentT ("xabz",[1,10])) $ pl @PartitionEithers [Left 'x', Right 1,Left 'a', Left 'b',Left 'z', Right 10]
  -- need Either a b to be fully typed unfortunately
  , expectPE (FailT "found rhs=10") $ pl @(LeftFail (PrintF "found rhs=%d" (Fst Id)) Id) (Right @String @Int 10)
  , expectPE (FailT "found rhs=23") $ pl @(LeftFail (PrintF "found rhs=%d" (Snd Id >> Snd Id >> Snd Id)) (Snd Id >> Fst Id)) ('x',(Right @() 10,23::Int))
  , expectPE (PresentT "abc") $ pl @(LeftFail (PrintF "found rhs=%d" (Snd (Snd (Snd Id)))) (Fst (Snd Id))) ('x',(Left @_ @() "abc",23::Int))
  , expectPE (PresentT ([1,4,10],"xy",[(9,'z'),(8,'y')])) $ pl @PartitionThese [This 1,That 'x',This 4,That 'y',These 9 'z',This 10,These 8 'y']
  , expectPE (PresentT [('a',1),('a',10),('z',14),('m',22)]) $ pl @(SortOn (Snd Id) (Snd Id)) ((),[('z',14),('a',10),('m',22),('a',1)])
  , expectPE (PresentT [('z',1),('m',22),('a',10)]) $ pl @(SortOnDesc (Fst Id) (Snd Id)) ((),[('z',1),('a',10),('m',22)])
  , expectPE (PresentT [('a',10),('m',22),('z',1)]) $ pl @(SortOn (Fst Id) (Snd Id)) ((),[('z',1),('a',10),('m',22)])
  , expectPE (PresentT [('z',1),('m',22),('a',10)]) $ pl @(SortBy (Swap >> OrdA (Fst Id)) (Snd Id)) ((),[('z',1),('a',10),('m',22)])
  , expectPE (PresentT ["aa","cx","by","az"]) $ pl @(SortBy (OrdA Reverse) Id) ["az","by","cx","aa"]
  , expectPE (PresentT [('a',10),('a',9),('m',22),('m',10),('z',1)]) $ pl @(SortOn (Fst Id) Id) [('z',1),('a',10),('m',22),('a',9),('m',10)]
  , expectPE (PresentT [('a',9),('a',10),('m',10),('m',22),('z',1)]) $ pl @(SortOn Id Id) [('z',1),('a',10),('m',22),('a',9),('m',10)]
  , expectPE (PresentT (False,9)) $ pl @(Just Uncons >> FoldL (If (Fst (Fst Id)) (If (Snd (Fst Id) < Snd Id) '( 'True,Snd Id) '( 'False, Snd Id)) (Fst Id)) '( 'True,Fst Id) (Snd Id)) [-10,-2,2,3,4,10,9,11]
  , expectPE (PresentT (True,11)) $ pl @(Just Uncons >> FoldL (If (Fst (Fst Id)) (If (Snd (Fst Id) < Snd Id) '( 'True,Snd Id) '( 'False, Snd Id)) (Fst Id)) '( 'True,Fst Id) (Snd Id)) [-10,2,3,4,10,11]
  , expectPE (FailT "pivot=5 value=3(2)") $ pl @(SortBy (If (Fst Id==5 && Snd Id==3) (Failt _ (PrintT "pivot=%d value=%d" Id)) 'GT) (Snd Id)) ((), [5,7,3,1,6,2,1,3])
  , expectPE (PresentT [1,1,2,3,3,5,6,7]) $ pl @(SortBy (If (Fst Id==50 && Snd Id==3) (Failt _ (PrintT "pivot=%d value=%d" Id)) (OrdA Id)) (Snd Id)) ((), [5,7,3,1,6,2,1,3])
  , expectPE TrueT $ pl @(Between (Fst Id >> Fst Id) (Fst Id >> Snd Id) (Snd Id)) ((1,4),3)
  , expectPE FalseT $ pl @(Between (Fst Id >> Fst Id) (Fst Id >> Snd Id) (Snd Id)) ((1,4),10)
  , expectPE (FailT "no match on [03/29/0x7]") $ pl @(Map (ParseTimes Day '["%Y-%m-%d", "%m/%d/%y", "%b %d %Y"] Id) Id) ["2001-01-01", "Jan 24 2009", "03/29/0x7"]
  , expectPE (PresentT [readNote @Day "invalid day" "2001-01-01", readNote @Day "invalid day" "2009-01-24", readNote @Day "invalid day" "2007-03-29"]) $ pl @(Map (ParseTimes Day '["%Y-%m-%d", "%m/%d/%y", "%b %d %Y"] Id) Id) ["2001-01-01", "Jan 24 2009", "03/29/07"]

  , expectPE (PresentT "gt3") $ pl @(Case (Snd Id >> Failp "xx") '[Gt 3, Lt 2, Same 3] '["gt3","lt2","eq3"] Id) 15
  , expectPE (PresentT "lt2") $ pl @(Case (Snd Id >> Failp "xx") '[Gt 3, Lt 2, Same 3] '["gt3","lt2","eq3"] Id) 1
  , expectPE (PresentT "eq3") $ pl @(Case (Snd Id >> Failp "xx") '[Gt 3, Lt 2, Same 3] '["gt3","lt2","eq3"] Id) 3

  , expectPE (FailT "no match") $ pl @(Case (Snd Id >> Failp "no match") '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (FailT "no match for 015") $ pl @(Case (Fail (Snd Id >> Unproxy) (PrintF "no match for %03d" (Fst Id))) '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (FailT "no match for 015") $ pl @(Case'' (PrintF "no match for %03d" Id) '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (FailT "no match for 015") $ pl @(Case'' (PrintF "no match for %03d" Id) '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (PresentT "other") $ pl @(Case "other" '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (PresentT "151515") $ pl @(Case (ShowP (Fst Id) >> Id <> Id <> Id) '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (FailT "Case:no match") $ pl @(Case' '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
  , expectPE (FailT "no match for -012") $ pl @(Case'' (PrintF "no match for %04d" Id) '[Between 0 5 Id, Same 6, Between 7 10 Id] '[ 'LT, 'EQ, 'GT] Id) (-12)
  , expectPE (PresentT [Left 1,Left 2,Right "fizz",Left 4,Right "buzz",Right "fizz",Left 7,Left 8,Right "fizz",Right "buzz",Left 11,Right "fizz",Left 13,Left 14,Right "fizzbuzz"]) $ pl @(Map Fizzbuzz''' Id) [1..15]
  , expectPE (PresentT (Left 'x')) $ pl @(EitherBool (Fst Id > 10) (Snd Id >> Fst Id) (Snd Id >> Snd Id)) (7,('x',99))
  , expectPE (PresentT (Right 99)) $ pl @(EitherBool (Fst Id > 10) (Snd Id >> Fst Id) (Snd Id >> Snd Id)) (11,('x',99))
  , expectPE (PresentT (Right 99)) $ pl @(EitherBool (Gt 10) "found left" 99) 12
  , expectPE (PresentT (Left "found left")) $ pl @(EitherBool (Gt 10) "found left" 99) 7

  , expectPE (FailT "msg=someval caught(044)") $ pl @(Catch' (Failt Int "someval") (PrintT "msg=%s caught(%03d)" Id)) (44 :: Int)
  , expectPE (FailT "msg=OneP 3 elements caught([10,12,13])") $ pl @(Catch' (OneP Id) (Second (ShowP Id) >> PrintT "msg=%s caught(%s)" Id)) [10,12,13]
  , expectPE (PresentT 10) $ pl @(Catch' (OneP Id) (PrintT "msg=%s caught(%s)" (Second (ShowP Id)))) [10]
  , expectPE (FailT "msg=OneP 2 elements err s=[10,11]") $ pl @(Catch' (OneP Id) (PrintT "msg=%s err s=%s" (Second (ShowP Id)))) [10,11]
  , expectPE (PresentT 99) $ pl @(Catch (OneP Id) 99) [10,11]
  , expectPE (PresentT 10) $ pl @(Catch (OneP Id) 99) [10]
  , expectPE (PresentT False) $ pl @(Catch (OneP Id) 'True) [False]  -- cant know that this is FalseT cos is driven by type of the list not the 'True part
  , expectPE FalseT $ pl @(Catch (OneP Id) 'False) [True,True,False]
  , expectPE TrueT $ pl @(Catch (OneP Id) 'True) []
  , expectPE (PresentT (-255)) $ pl @(ReadBase Int 16 Id) "-ff"
  , expectPE (PresentT 255) $ pl @(ReadBase Int 16 Id) "ff"
  , expectPE (PresentT "-7b") $ pl @(ShowBase 16 Id) (-123)
  , expectPE (PresentT "7b") $ pl @(ShowBase 16 Id) 123
  , expectPE (PresentT "abc") $ pl @(Thd (Snd (Fst Id))) (('x',(13,False,"abc")),True,'y')
  , expectPE (PresentT 9.3) $ pl @(Fst (Snd (Thd Id))) ('x',True,(13,(9.3,False),"def"))
  , expectPE (PresentT (4,"helo|oleh")) $ pl @'(Len, Id <> "|" <> Reverse) "helo"
  , expectPE (PresentT (123,"helo")) $ pl @'(Snd Id, Fst Id) ("helo",123)
  , expectPE (PresentT (4,"helo","oleh")) $ pl @'(Len, Id, Reverse) "helo"
  , expectPE (PresentT [1,2,3,1000,998]) $ pl @'[W 1, W 2, W 3, Succ Id, Pred Id] 999
  , expectPE (PresentT [3996,998]) $ pl @'[Id * 4, Pred Id] 999

  , expectPE (PresentT (These [1,2,3,4] "Abcdef")) $ pz @(MkThat _ "Abc" <> MkThis _ '[1,2] <> MkThese [3,4] "def") ()

  -- test semigroup interaction
  , expectEQR (These (PresentT 6) (FailT "xyzhello")) $ fmap This (pz @Predicate.Sum [1,2,3]) <> fmap That (pz @(FailS "xyz") 5) <> fmap That (pz @(FailS "hello") 1)
  , expectEQR (These (PresentT 6) (PresentT ("5",6))) $ fmap This (pz @Predicate.Sum [1,2,3]) <> fmap That (pz @(ShowP Id &&& Succ Id) 5)
  ]

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


