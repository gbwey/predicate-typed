{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE EmptyDataDeriving #-}
-- | extra promoted functions
module Predicate.Data.Extra (
 -- ** list functions
    HeadDef
  , HeadFail
  , TailDef
  , TailFail
  , LastDef
  , LastFail
  , InitDef
  , InitFail

  , HeadMay
  , LastMay
  , TailMay
  , InitMay

  , PartitionsBy
  , IMap
  , IList

 -- ** primes
  , IsPrime
  , PrimeNext
  , PrimePrev
  , PrimeFactors
  , Primes

 -- ** luhn check
  , IsLuhn

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Predicate.Data.List (Head, Uncons, Unsnoc, SortBy, Zip, GroupBy, SortOn, ZipWith)
import Predicate.Data.Enum (type (...))
import Predicate.Data.Numeric (type (-))
import Predicate.Data.Maybe (JustDef, JustFail)
import Predicate.Data.Lifted (FMap)
import Control.Lens
import qualified Safe (headNote, cycleNote)
import Data.Proxy (Proxy(..))
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> import qualified Data.Text as T
-- >>> import qualified Data.Sequence as Seq
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG
-- >>> import Data.These

-- | takes the head of a list-like object or uses the given default value
--
-- see 'ConsT' for other supported types eg 'Data.Sequence.Seq'
--
-- >>> pz @(HeadDef 444 Id) []
-- Val 444
--
-- >>> pz @(HeadDef 444 Id) [1..5]
-- Val 1
--
-- >>> pz @(HeadDef 444 Id) [1..5]
-- Val 1
--
-- >>> pz @(HeadDef (Char1 "w") Id) (Seq.fromList "abcdef")
-- Val 'a'
--
-- >>> pz @(HeadDef (Char1 "w") Id) Seq.empty
-- Val 'w'
--
-- >>> pz @(HeadDef (MEmptyT _) Id) ([] :: [SG.Sum Int])
-- Val (Sum {getSum = 0})
--
-- >>> pz @(HeadDef (MEmptyT String) '["abc","def","asdfadf"]) ()
-- Val "abc"
--
-- >>> pz @(HeadDef (MEmptyT _) Snd) (123,["abc","def","asdfadf"])
-- Val "abc"
--
-- >>> pz @(HeadDef (MEmptyT _) Snd) (123,[])
-- Val ()
--
-- >>> pl @(HeadDef 9 Fst) ([],True)
-- Present 9 (JustDef Nothing)
-- Val 9
--
-- >>> pl @(HeadDef 9 Fst) ([1..5],True)
-- Present 1 (JustDef Just)
-- Val 1
--
-- >>> pl @(HeadDef 3 Fst) ([10..15],True)
-- Present 10 (JustDef Just)
-- Val 10
--
-- >>> pl @(HeadDef 12 Fst >> Le 6) ([],True)
-- False ((>>) False | {12 <= 6})
-- Val False
--
-- >>> pl @(HeadDef 1 Fst >> Le 6) ([],True)
-- True ((>>) True | {1 <= 6})
-- Val True
--
-- >>> pl @(HeadDef 10 Fst >> Le 6) ([],True)
-- False ((>>) False | {10 <= 6})
-- Val False
--
-- >>> pl @(HeadDef (MEmptyT _) Id) (map (:[]) ([] :: [Int]))
-- Present [] (JustDef Nothing)
-- Val []
--
-- >>> pl @(HeadDef (MEmptyT _) Id) (map (:[]) ([10..14] :: [Int]))
-- Present [10] (JustDef Just)
-- Val [10]
--
-- >>> pl @(HeadDef Fst Snd) (99,[10..14])
-- Present 10 (JustDef Just)
-- Val 10
--
-- >>> pl @(HeadDef Fst Snd) (99,[] :: [Int])
-- Present 99 (JustDef Nothing)
-- Val 99
--
-- >>> pl @(HeadDef 43 Snd) (99,[] :: [Int])
-- Present 43 (JustDef Nothing)
-- Val 43
--
data HeadDef p q deriving Show
type HeadDefT p q = JustDef p (q >> Uncons >> FMap Fst)

instance P (HeadDefT p q) x => P (HeadDef p q) x where
  type PP (HeadDef p q) x = PP (HeadDefT p q) x
  eval _ = eval (Proxy @(HeadDefT p q))


-- | takes the head of a list or fails with the given message
--
-- see 'ConsT' for other supported types eg 'Data.Sequence.Seq'
--
-- >>> pz @(HeadFail "oops" Id) ["abc","def","asdfadf"]
-- Val "abc"
--
-- >>> pz @(HeadFail "empty list" Id) []
-- Fail "empty list"
--
-- >>> pl @(HeadFail "zz" Fst >> Le 6) ([],True)
-- Error zz (JustFail Nothing)
-- Fail "zz"
--
-- >>> pl @((HeadFail "failed1" Fst >> Le 6) || 'False) ([],True)
-- Error failed1 (JustFail Nothing | ||)
-- Fail "failed1"
--
-- >>> pl @((Fst >> HeadFail "failed2" Id >> Le (6 -% 1)) || 'False) ([-9],True)
-- True (True || False)
-- Val True
--
-- >>> pl @(HeadFail "Asdf" Id) ([] :: [()]) -- breaks otherwise
-- Error Asdf (JustFail Nothing)
-- Fail "Asdf"
--
-- >>> pl @(HeadFail (PrintF "msg=%s def" Fst) Snd) ("Abc",[])
-- Error msg=Abc def (JustFail Nothing)
-- Fail "msg=Abc def"
--

data HeadFail msg q deriving Show
type HeadFailT msg q = JustFail msg (q >> Uncons >> FMap Fst)

instance P (HeadFailT msg q) x => P (HeadFail msg q) x where
  type PP (HeadFail msg q) x = PP (HeadFailT msg q) x
  eval _ = eval (Proxy @(HeadFailT msg q))

-- | takes the tail of a list-like object or uses the given default value
--
-- >>> pl @(TailDef '[9,7] Fst) ([],True)
-- Present [9,7] (JustDef Nothing)
-- Val [9,7]
--
-- >>> pl @(TailDef '[9,7] Fst) ([1..5],True)
-- Present [2,3,4,5] (JustDef Just)
-- Val [2,3,4,5]
--
-- >>> pl @(TailDef '[3] Fst) ([10..15],True)
-- Present [11,12,13,14,15] (JustDef Just)
-- Val [11,12,13,14,15]
--

data TailDef p q deriving Show
type TailDefT p q = JustDef p (q >> Uncons >> FMap Snd)

instance P (TailDefT p q) x => P (TailDef p q) x where
  type PP (TailDef p q) x = PP (TailDefT p q) x
  eval _ = eval (Proxy @(TailDefT p q))


-- | takes the tail of a list-like object or fails with the given message
--
-- >>> pl @(TailFail (PrintT "a=%d b=%s" Snd) Fst) ([]::[()],(4,"someval"))
-- Error a=4 b=someval (JustFail Nothing)
-- Fail "a=4 b=someval"
--

data TailFail msg q deriving Show
type TailFailT msg q = JustFail msg (q >> Uncons >> FMap Snd)

instance P (TailFailT msg q) x => P (TailFail msg q) x where
  type PP (TailFail msg q) x = PP (TailFailT msg q) x
  eval _ = eval (Proxy @(TailFailT msg q))

-- | takes the last value of a list-like object or a default value
--
-- >>> pl @(LastDef 9 Fst) ([],True)
-- Present 9 (JustDef Nothing)
-- Val 9
--
-- >>> pl @(LastDef 9 Fst) ([1..5],True)
-- Present 5 (JustDef Just)
-- Val 5
--
-- >>> pl @(LastDef 3 Fst) ([10..15],True)
-- Present 15 (JustDef Just)
-- Val 15
--
-- >>> pl @(LastDef 0 Id) [1..12]
-- Present 12 (JustDef Just)
-- Val 12
--
-- >>> pl @(LastDef 0 Id) []
-- Present 0 (JustDef Nothing)
-- Val 0
--

data LastDef p q deriving Show
type LastDefT p q = JustDef p (q >> Unsnoc >> FMap Snd)

instance P (LastDefT p q) x => P (LastDef p q) x where
  type PP (LastDef p q) x = PP (LastDefT p q) x
  eval _ = eval (Proxy @(LastDefT p q))

-- | takes the init of a list-like object or fails with the given message
data LastFail msg q deriving Show
type LastFailT msg q = JustFail msg (q >> Unsnoc >> FMap Snd)

instance P (LastFailT msg q) x => P (LastFail msg q) x where
  type PP (LastFail msg q) x = PP (LastFailT msg q) x
  eval _ = eval (Proxy @(LastFailT msg q))

-- | takes the init of a list-like object or uses the given default value
--
-- >>> pl @(InitDef '[9,7] Fst) ([],True)
-- Present [9,7] (JustDef Nothing)
-- Val [9,7]
--
-- >>> pl @(InitDef '[9,7] Fst) ([1..5],True)
-- Present [1,2,3,4] (JustDef Just)
-- Val [1,2,3,4]
--
-- >>> pl @(InitDef '[3] Fst) ([10..15],True)
-- Present [10,11,12,13,14] (JustDef Just)
-- Val [10,11,12,13,14]
--
data InitDef p q deriving Show
type InitDefT p q = JustDef p (q >> Unsnoc >> FMap Fst)

instance P (InitDefT p q) x => P (InitDef p q) x where
  type PP (InitDef p q) x = PP (InitDefT p q) x
  eval _ = eval (Proxy @(InitDefT p q))

-- | takes the init of a list-like object or fails with the given message
data InitFail msg q deriving Show
type InitFailT msg q = JustFail msg (q >> Unsnoc >> FMap Fst)

instance P (InitFailT msg q) x => P (InitFail msg q) x where
  type PP (InitFail msg q) x = PP (InitFailT msg q) x
  eval _ = eval (Proxy @(InitFailT msg q))

-- | similar to 'Safe.headMay'
--
-- >>> pl @HeadMay []
-- Present Nothing ((>>) Nothing | {FMap <skipped>})
-- Val Nothing
--
-- >>> pl @HeadMay [99,7,3]
-- Present Just 99 ((>>) Just 99 | {FMap Fst 99 | (99,[7,3])})
-- Val (Just 99)
--
data HeadMay deriving Show
type HeadMayT = Uncons >> FMap Fst

instance P HeadMayT x => P HeadMay x where
  type PP HeadMay x = PP HeadMayT x
  eval _ = eval (Proxy @HeadMayT)

-- | similar to 'Safe.lastMay'
--
-- >>> pz @LastMay "hello"
-- Val (Just 'o')
--
data LastMay deriving Show
type LastMayT = Unsnoc >> FMap Snd

instance P LastMayT x => P LastMay x where
  type PP LastMay x = PP LastMayT x
  eval _ = eval (Proxy @LastMayT)

-- | similar to 'Safe.tailMay'
--
-- >>> pz @TailMay "hello"
-- Val (Just "ello")
--
data TailMay deriving Show
type TailMayT = Uncons >> FMap Snd

instance P TailMayT x => P TailMay x where
  type PP TailMay x = PP TailMayT x
  eval _ = eval (Proxy @TailMayT)

-- | similar to 'Safe.initMay'
--
-- >>> pz @InitMay "hello"
-- Val (Just "hell")
--
data InitMay deriving Show
type InitMayT = Unsnoc >> FMap Fst

instance P InitMayT x => P InitMay x where
  type PP InitMay x = PP InitMayT x
  eval _ = eval (Proxy @InitMayT)

-- | a predicate on prime numbers
--
-- >>> pz @IsPrime 2
-- Val True
--
-- >>> pz @(Map '(Id,IsPrime)) [0..12]
-- Val [(0,False),(1,False),(2,True),(3,True),(4,False),(5,True),(6,False),(7,True),(8,False),(9,False),(10,False),(11,True),(12,False)]
--
data IsPrime deriving Show

instance ( x ~ a
         , Show a
         , Integral a
         ) => P IsPrime x where
  type PP IsPrime x = Bool
  eval _ opts x =
    let msg0 = "IsPrime"
        b = x > 1 && isPrime (fromIntegral x)
    in pure $ mkNodeB opts b (msg0 <> showVerbose opts " | " x) []

-- | get the next prime number
--
-- >>> pz @PrimeNext 6
-- Val 7
--
-- >>> pz @(ScanN 4 PrimeNext Id) 3
-- Val [3,5,7,11,13]
--
data PrimeNext deriving Show

instance ( Show x
         , Integral x
         ) => P PrimeNext x where
  type PP PrimeNext x = Integer
  eval _ opts x =
    let msg0 = "PrimeNext"
        ret = Safe.headNote msg0 $ dropWhile (<= fromIntegral x) primes
    in pure $ mkNode opts (Val ret) (msg0 <> showVerbose opts " | " x) []

-- | get the next prime number
--
-- >>> pz @PrimePrev 6
-- Val 5
--
-- >>> pz @PrimePrev 5
-- Val 3
--
-- >>> pz @PrimePrev (-206)
-- Val 2
--
-- >>> pz @(ScanN 6 PrimePrev Id) 11
-- Val [11,7,5,3,2,2,2]
--
data PrimePrev deriving Show

instance ( Show x
         , Integral x
         ) => P PrimePrev x where
  type PP PrimePrev x = Integer
  eval _ opts x =
    let msg0 = "PrimePrev"
        ret = case unsnoc $ takeWhile (< fromIntegral x) primes of
                Just (_,p) -> p
                Nothing -> 2
    in pure $ mkNode opts (Val ret) (msg0 <> showVerbose opts " | " x) []

-- | get list of @n@ primes
--
-- >>> pz @(Primes Id) 5
-- Val [2,3,5,7,11]
--
data Primes n deriving Show

instance ( Integral (PP n x)
         , P n x
         ) => P (Primes n) x where
  type PP (Primes n) x = [Integer]
  eval _ opts x = do
    let msg0 = "Primes"
    nn <- eval (Proxy @n) opts x
    pure $ case getValueLR NoInline opts msg0 nn [] of
      Left e -> e
      Right (fromIntegral -> n) ->
        let ret = take n primes
        in mkNode opts (Val ret) (msg0 <> showVerbose opts " | " n) [hh nn]

-- | prime factorisation of positive numbers
--
-- >>> pz @(PrimeFactors Id) 17
-- Val [17]
--
-- >>> pz @(PrimeFactors Id) 1
-- Val [1]
--
-- >>> pz @(PrimeFactors Id) 30
-- Val [2,3,5]
--
-- >>> pz @(PrimeFactors Id) 64
-- Val [2,2,2,2,2,2]
--
-- >>> pz @(PrimeFactors Id) (-30)
-- Fail "PrimeFactors number<=0"
--
data PrimeFactors n deriving Show

instance ( Integral (PP n x)
         , P n x
         ) => P (PrimeFactors n) x where
  type PP (PrimeFactors n) x = [Integer]
  eval _ opts x = do
    let msg0 = "PrimeFactors"
    nn <- eval (Proxy @n) opts x
    pure $ case getValueLR NoInline opts msg0 nn [] of
      Left e -> e
      Right (fromIntegral -> n :: Integer)
            | n <= 0 -> mkNode opts (Fail (msg0 <> " number<=0")) "" [hh nn]
            | otherwise ->
                let ret = primeFactors n
                in mkNode opts (Val ret) (msg0 <> showVerbose opts " | " n) [hh nn]

-- | IsLuhn predicate check on last digit
--
-- >>> pz @IsLuhn [1,2,3,0]
-- Val True
--
-- >>> pz @IsLuhn [1,2,3,4]
-- Val False
--
-- >>> pz @(GuardSimple IsLuhn) [15,4,3,1,99]
-- Fail "(IsLuhn map=[90,2,3,8,6] sum=109 ret=9 | [15,4,3,1,99])"
--
-- >>> pl @IsLuhn [15,4,3,1,99]
-- False (IsLuhn map=[90,2,3,8,6] sum=109 ret=9 | [15,4,3,1,99])
-- Val False
--
data IsLuhn deriving Show

instance x ~ [Int]
         => P IsLuhn x where
  type PP IsLuhn x = Bool
  eval _ opts x =
    let msg0 = "IsLuhn"
    in pure $ case chkSize opts msg0 x [] of
         Left e -> e
         Right _ ->
          let xs = zipWith (*) (reverse x) (Safe.cycleNote msg0 [1,2])
              ys = map (\w -> if w>=10 then w-9 else w) xs
              z = sum ys
              ret = z `mod` 10
          in if ret == 0 then mkNodeB opts True (msg0 <> " | " <> showL opts x) []
             else mkNodeB opts False (msg0 <> " map=" <> showL opts ys <> " sum=" <> showL opts z <> " ret=" <> showL opts ret <> showVerbose opts " | " x) []

-- | experimental: sorts then partitions and then sorts each partitions based on the leftmost occurring value in the original list
--   if the existing order of data is fine then use 'Predicate.Data.List.GroupBy' as you do not need this
--
-- >>> pz @(PartitionsBy (Fst ==! Snd) (L11 == L21) Id) [10,9,9,1,9]
-- Val [[10],[9,9,9],[1]]
--
-- >>> pz @(PartitionsBy OrdA (L11 < L21) Id) "efaffabec"
-- Val ["a","f","f","abce","ef"]
--
-- >>> pz @(PartitionsBy 'GT 'True Id) "efaffabec"
-- Val ["cebaffafe"]
--
-- >>> pz @(PartitionsBy 'GT 'False Id) "efaffabec"
-- Val ["e","f","a","f","f","a","b","e","c"]
--
-- >>> pz @(PartitionsBy (Fst ==! Snd) (L12 > L22) Id) [10,9,9,1,9,4]
-- Val [[9],[1],[9,10],[4,9]]
--
-- >>> pz @(PartitionsBy (L11 ==! L21) (L12 > L22) Id) "eddadc"
-- Val ["d","a","de","cd"]
--
-- >>> pz @(PartitionsBy (L11 ==! L21) (L11 < L21) Id) [10,9,9,1,9,4]
-- Val [[9],[1,4,9],[9,10]]
--
data PartitionsBy p q r deriving Show
type PartitionsByT p q r = SortBy p (Zip r (0 ... (Length r - 1))) >> GroupBy q Id >> SortOn (Head >> Snd) Id >> Map (Map Fst)

instance P (PartitionsByT p q r) x => P (PartitionsBy p q r) x where
  type PP (PartitionsBy p q r) x = PP (PartitionsByT p q r) x
  eval _ = eval (Proxy @(PartitionsByT p q r))

-- | add an index to map
--
-- >>> pz @(Rescan "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$" >> Map (Snd >> IMap (GuardBool (PrintT "bad value=%d %s" Id) (Snd >> ReadP Int Id < 255)) Id)) "123.222.999.3"
-- Fail "bad value=2 999"
--
-- >>> pz @(Rescan "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$" >> Map (Snd >> IMap (GuardBool (PrintT "bad value=%d %s" Id) (Snd >> ReadP Int Id < 255)) Id)) "123.222.99.3"
-- Val [[True,True,True,True]]
--
data IMap p q deriving Show
type IMapT p q = ZipWith p (0 ... (Length q - 1)) q

instance P (IMapT p q) x => P (IMap p q) x where
  type PP (IMap p q) x = PP (IMapT p q) x
  eval _ = eval (Proxy @(IMapT p q))

-- | add an index to list
--
-- >>> pz @IList "abcdef"
-- Val [(0,'a'),(1,'b'),(2,'c'),(3,'d'),(4,'e'),(5,'f')]
--
data IList deriving Show
type IListT = Zip (0 ... (Len - 1)) Id

instance P IListT x => P IList x where
  type PP IList x = PP IListT x
  eval _ = eval (Proxy @IListT)
