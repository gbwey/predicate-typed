{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-compat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     promoted iterator functions
-}
module Predicate.Data.Iterator (
    Scanl
  , ScanN
  , ScanNA
  , FoldN
  , Foldl
  , Unfoldr
  , IterateUntil
  , IterateWhile
  , IterateNWhile
  , IterateNUntil

  , Para
  , ParaN

  , DoN
  , Repeat

 ) where
import Predicate.Core
import Predicate.Util
import Predicate.Data.Tuple (type (***))
import Predicate.Data.Ordering (type (>))
import Predicate.Data.Enum (type (...), Pred)
import Predicate.Data.List (Last)
import Predicate.Data.Maybe (MaybeBool)
import GHC.TypeLits (Nat, KnownNat)
import qualified GHC.TypeLits as GL
import Control.Lens hiding (iall)
import Data.Proxy
import Data.Maybe
import Control.Arrow
import Data.Void

-- $setup
-- >>> import Predicate.Prelude
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XAllowAmbiguousTypes
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> :set -XFlexibleContexts
-- >>> import Data.Time

-- want to pass Proxy b to q but then we have no way to calculate 'b'

-- | similar to 'scanl'
--
-- >>> pz @(Scanl (Snd Id :+ Fst Id) (Fst Id) (Snd Id)) ([99],[1..5])
-- PresentT [[99],[1,99],[2,1,99],[3,2,1,99],[4,3,2,1,99],[5,4,3,2,1,99]]
--
-- >>> pl @(Scanl (Snd Id :+ Fst Id) (Fst Id) (Snd Id)) ([99],[])
-- Present [[99]] (Scanl [[99]] | b=[99] | as=[])
-- PresentT [[99]]
--

data Scanl p q r
-- scanr :: (a -> b -> b) -> b -> [a] -> [b]
-- result is scanl but signature is flipped ((a,b) -> b) -> b -> [a] -> [b]

instance (PP p (b,a) ~ b
        , PP q x ~ b
        , PP r x ~ [a]
        , P p (b,a)
        , P q x
        , P r x
        , Show b
        , Show a
        )
     => P (Scanl p q r) x where
  type PP (Scanl p q r) x = [PP q x]
  eval _ opts z = do
    let msg0 = "Scanl"
    lr <- runPQ msg0 (Proxy @q) (Proxy @r) opts z []
    case lr of
      Left e -> pure e
      Right (q,r,qq,rr) ->
        case chkSize opts msg0 r [hh rr] of
          Left e -> pure e
          Right () -> do
            let ff i b as' rs
                   | i >= oRecursion opts = pure (rs, Left $ mkNode opts (FailT (msg0 <> ":recursion limit i=" <> showIndex i)) ("(b,as')=" <> showL opts (b,as')) [])
                   | otherwise =
                       case as' of
                         [] -> pure (rs, Right ()) -- ++ [((i,q), mkNode opts (PresentT q) (msg0 <> "(done)") [])], Right ())
                         a:as -> do
                            pp :: TT b <- evalHide @p opts (b,a)
                            case getValueLR opts (msg0 <> " i=" <> showIndex i <> " a=" <> show a) pp [] of
                               Left e  -> pure (rs,Left e)
                               Right b' -> ff (i+1) b' as (rs ++ [((i,b), pp)])
            (ts,lrx) :: ([((Int, b), TT b)], Either (TT [b]) ()) <- ff 1 q r []
            pure $ case splitAndAlign opts msg0 (((0,q), mkNode opts (PresentT q) (msg0 <> "(initial)") []) : ts) of
                 Left e -> errorInProgram $ "Scanl e=" ++ show (fromTT e)
                 Right abcs ->
                   let vals = map (view _1) abcs
                       itts = map (view _2 &&& view _3) abcs
                   in case lrx of
                        Left e -> mkNode opts (_tBool e) msg0 (hh qq : hh rr : map (hh . fixit) itts ++ [hh e])
                        Right () -> mkNode opts (PresentT vals) (show01' opts msg0 vals "b=" q <> showVerbose opts " | as=" r) (hh qq : hh rr : map (hh . fixit) itts)

-- | iterates n times keeping all the results
--
-- >>> pz @(ScanN 4 (Succ Id) Id) 'c'
-- PresentT "cdefg"
--
-- >>> pz @(Dup >> ScanN 4 (Pred Id *** Succ Id) Id) 'g'
-- PresentT [('g','g'),('f','h'),('e','i'),('d','j'),('c','k')]
--
-- >>> pz @(ScanN 4 (Succ Id) Id) 4
-- PresentT [4,5,6,7,8]
--
-- >>> pz @('(0,1) >> ScanN 20 '(Snd Id, Fst Id + Snd Id) Id >> Map (Fst Id) Id) "sdf"
-- PresentT [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]
--
-- >>> pl @(ScanN 2 (Succ Id) Id) 4
-- Present [4,5,6] (Scanl [4,5,6] | b=4 | as=[1,2])
-- PresentT [4,5,6]
--
-- >>> pl @(ScanN 5 Id Id) 4
-- Present [4,4,4,4,4,4] (Scanl [4,4,4,4,4,4] | b=4 | as=[1,2,3,4,5])
-- PresentT [4,4,4,4,4,4]
--
-- >>> pl @(ScanN 2 (Succ Id) Id >> PadR 10 (MEmptyT Ordering) Id) LT
-- Present [LT,EQ,GT,EQ,EQ,EQ,EQ,EQ,EQ,EQ] ((>>) [LT,EQ,GT,EQ,EQ,EQ,EQ,EQ,EQ,EQ] | {PadR 10 pad=EQ [LT,EQ,GT,EQ,EQ,EQ,EQ,EQ,EQ,EQ] | [LT,EQ,GT]})
-- PresentT [LT,EQ,GT,EQ,EQ,EQ,EQ,EQ,EQ,EQ]
--
-- >>> pl @(ScanN 4 (Pred Id) Id) 99
-- Present [99,98,97,96,95] (Scanl [99,98,97,96,95] | b=99 | as=[1,2,3,4])
-- PresentT [99,98,97,96,95]
--
data ScanN n p q
type ScanNT n p q = Scanl (Fst Id >> p) q (1...n) -- n times using q then run p

instance P (ScanNT n p q) x => P (ScanN n p q) x where
  type PP (ScanN n p q) x = PP (ScanNT n p q) x
  eval _ = eval (Proxy @(ScanNT n p q))

-- | tuple version of 'ScanN'
--
-- >>> pl @(ScanNA (Succ Id)) (4,'a')
-- Present "abcde" (Scanl "abcde" | b='a' | as=[1,2,3,4])
-- PresentT "abcde"
--
-- >>> pl @(ScanNA (Tail Id)) (4,"abcd" :: String)
-- Present ["abcd","bcd","cd","d",""] (Scanl ["abcd","bcd","cd","d",""] | b="abcd" | as=[1,2,3,4])
-- PresentT ["abcd","bcd","cd","d",""]
--
-- >>> pl @(Len &&& Id >> ScanNA (Tail Id)) "abcd"
-- Present ["abcd","bcd","cd","d",""] ((>>) ["abcd","bcd","cd","d",""] | {Scanl ["abcd","bcd","cd","d",""] | b="abcd" | as=[1,2,3,4]})
-- PresentT ["abcd","bcd","cd","d",""]
--
data ScanNA q
type ScanNAT q = ScanN (Fst Id) q (Snd Id)

instance P (ScanNAT q) x => P (ScanNA q) x where
  type PP (ScanNA q) x = PP (ScanNAT q) x
  eval _ = eval (Proxy @(ScanNAT q))

-- | iterates n times keeping only the last result
--
-- >>> pz @(FoldN 4 (Succ Id) Id) 'c'
-- PresentT 'g'
--
-- >>> pz @(ReadP Day Id >> Id ... FoldN 5 (Succ Id) Id) "2020-07-27"
-- PresentT [2020-07-27,2020-07-28,2020-07-29,2020-07-30,2020-07-31,2020-08-01]
--
-- >>> pl @(FoldN 2 (Succ Id) Id) LT
-- Present GT (Last GT | [LT,EQ,GT])
-- PresentT GT
--
-- >>> pl @(FoldN 30 (Succ Id) Id) LT
-- Error Succ IO e=Prelude.Enum.Ordering.succ: bad argument (Last)
-- FailT "Succ IO e=Prelude.Enum.Ordering.succ: bad argument"
--
-- >>> pl @(FoldN 6 (Succ Id) Id) 'a'
-- Present 'g' (Last 'g' | "abcdefg")
-- PresentT 'g'
--
-- >>> pl @(FoldN 6 (Pred Id) Id) 'a'
-- Present '[' (Last '[' | "a`_^]\\[")
-- PresentT '['
--
-- >>> pl @(FoldN 0 (Succ Id) Id) LT
-- Present LT (Last LT | [LT])
-- PresentT LT
--
-- >>> pl @(FoldN 2 (Succ Id) Id >> FoldN 2 (Pred Id) Id) LT
-- Present LT ((>>) LT | {Last LT | [GT,EQ,LT]})
-- PresentT LT
--
-- >>> pl @(FoldN 4 ((Id &&& Id) >> SapA) Id) "abc"
-- Present "abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc" (Last "abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc" | ["abc","abcabc","abcabcabcabc","abcabcabcabcabcabcabcabc","abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc"])
-- PresentT "abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc"
--

data FoldN n p q
type FoldNT n p q = Last (ScanN n p q)

instance P (FoldNT n p q) x => P (FoldN n p q) x where
  type PP (FoldN n p q) x = PP (FoldNT n p q) x
  eval _ = eval (Proxy @(FoldNT n p q))

-- | Foldl similar to 'foldl'
--
-- >>> pl @(Foldl (Fst Id + Snd Id) 0 (1 ... 10)) ()
-- Present 55 (Last 55 | [0,1,3,6,10,15,21,28,36,45,55])
-- PresentT 55
--
-- >>> pz @(Foldl (Snd Id :+ Fst Id) '[99] (1 ... 10)) ()
-- PresentT [10,9,8,7,6,5,4,3,2,1,99]
--
-- >>> pl @(Foldl (Fst Id) '() (EnumFromTo 1 9999)) ()
-- Error Scanl list size exceeded (Last)
-- FailT "Scanl list size exceeded"
--
-- >>> pl @(Foldl (Guard "someval" (Fst Id < Snd Id) >> Snd Id) (Head Id) (Tail Id)) [1,4,7,9,16]
-- Present 16 (Last 16 | [1,4,7,9,16])
-- PresentT 16
--
-- >>> pl @(Foldl (Guard (PrintT "%d not less than %d" Id) (Fst Id < Snd Id) >> Snd Id) (Head Id) (Tail Id)) [1,4,7,6,16::Int]
-- Error 7 not less than 6 (Last)
-- FailT "7 not less than 6"
--
-- >>> pl @(Foldl (If ((Fst Id >> Fst Id) && (Snd Id > Snd (Fst Id))) '( 'True, Snd Id) '( 'False, Snd (Fst Id))) '( 'True, Head Id) (Tail Id)) [1,4,7,9,16]
-- Present (True,16) (Last (True,16) | [(True,1),(True,4),(True,7),(True,9),(True,16)])
-- PresentT (True,16)
--
-- >>> pl @(Foldl (If ((Fst Id >> Fst Id) && (Snd Id > Snd (Fst Id))) '( 'True, Snd Id) '( 'False, Snd (Fst Id))) '( 'True, Head Id) (Tail Id)) [1,4,7,9,16,2]
-- Present (False,16) (Last (False,16) | [(True,1),(True,4),(True,7),(True,9),(True,16),(False,16)])
-- PresentT (False,16)
--
-- >>> pl @(Foldl (Snd Id :+ Fst Id) (MEmptyT [_]) Id) [1..5]
-- Present [5,4,3,2,1] (Last [5,4,3,2,1] | [[],[1],[2,1],[3,2,1],[4,3,2,1],[5,4,3,2,1]])
-- PresentT [5,4,3,2,1]
--
-- >>> pl @('Just Uncons >> Foldl (If (Fst (Fst Id)) (If (Snd (Fst Id) < Snd Id) '( 'True,Snd Id) '( 'False, Snd Id)) (Fst Id)) '( 'True,Fst Id) (Snd Id)) [-10,-2,2,3,4,10,9,11]
-- Present (False,9) ((>>) (False,9) | {Last (False,9) | [(True,-10),(True,-2),(True,2),(True,3),(True,4),(True,10),(False,9),(False,9)]})
-- PresentT (False,9)
--
-- >>> pl @('Just Uncons >> Foldl (If (Fst (Fst Id)) (If (Snd (Fst Id) < Snd Id) '( 'True,Snd Id) '( 'False, Snd Id)) (Fst Id)) '( 'True,Fst Id) (Snd Id)) [-10,2,3,4,10,11]
-- Present (True,11) ((>>) (True,11) | {Last (True,11) | [(True,-10),(True,2),(True,3),(True,4),(True,10),(True,11)]})
-- PresentT (True,11)
--

data Foldl p q r
type FoldLT p q r = Last (Scanl p q r)

instance P (FoldLT p q r) x => P (Foldl p q r) x where
  type PP (Foldl p q r) x = PP (FoldLT p q r) x
  eval _ = eval (Proxy @(FoldLT p q r))

-- | similar to 'Data.List.unfoldr'
--
-- >>> pz @(Unfoldr (MaybeBool (Not Null) (SplitAt 2 Id)) Id) [1..5]
-- PresentT [[1,2],[3,4],[5]]
--
-- >>> pl @(Unfoldr (If Null (MkNothing _) ('(Take 3 Id, Drop 1 Id) >> MkJust Id)) Id) "abcdefghi"
-- Present ["abc","bcd","cde","def","efg","fgh","ghi","hi","i"] (Unfoldr "abcdefghi" ["abc","bcd","cde","def","efg","fgh","ghi","hi","i"] | s="abcdefghi")
-- PresentT ["abc","bcd","cde","def","efg","fgh","ghi","hi","i"]
--
-- >>> pl @(Unfoldr (If Null (MkNothing _) (Pure _ (SplitAt 2 Id))) Id) [1..5]
-- Present [[1,2],[3,4],[5]] (Unfoldr [1,2,3,4,5] [[1,2],[3,4],[5]] | s=[1,2,3,4,5])
-- PresentT [[1,2],[3,4],[5]]
--
-- >>> pl @(Unfoldr (MaybeBool (Not Null) (SplitAt 2 Id)) Id) [1..5]
-- Present [[1,2],[3,4],[5]] (Unfoldr [1,2,3,4,5] [[1,2],[3,4],[5]] | s=[1,2,3,4,5])
-- PresentT [[1,2],[3,4],[5]]
--
-- >>> pl @(Unfoldr (If Null (MkNothing _) (Guard "yy" (Len < 3) >> Pure _ (SplitAt 2 Id))) Id) [1..5]
-- Error yy (Unfoldr [1,2,3,4,5])
-- FailT "yy"
--
-- >>> pl @(Unfoldr (MaybeBool (Not Null) (Guard "yy" (Len < 3) >> SplitAt 2 Id)) Id) [1..5]
-- Error yy (Unfoldr [1,2,3,4,5])
-- FailT "yy"
--
-- >>> pl @(Unfoldr (Guard "xx" (Len > 4) >> Uncons) Id) [1..10]
-- Error xx (Unfoldr [1,2,3,4,5,6,7,8,9,10])
-- FailT "xx"
--
-- >>> pl @(Unfoldr Uncons Id) [1..10]
-- Present [1,2,3,4,5,6,7,8,9,10] (Unfoldr [1,2,3,4,5,6,7,8,9,10] [1,2,3,4,5,6,7,8,9,10] | s=[1,2,3,4,5,6,7,8,9,10])
-- PresentT [1,2,3,4,5,6,7,8,9,10]
--
-- >>> pan @(Unfoldr (If (Id < 1) (MkNothing _) (MkJust (DivMod Id 2 >> Swap))) Id) 8
-- P Unfoldr 8 [0,0,0,1]
-- |
-- +- P Id 8
-- |
-- +- P i=1: If 'False Just (0,4)
-- |
-- +- P i=2: If 'False Just (0,2)
-- |
-- +- P i=3: If 'False Just (0,1)
-- |
-- +- P i=4: If 'False Just (1,0)
-- |
-- `- P i=5: If 'True Nothing
-- PresentT [0,0,0,1]
--
data Unfoldr p q

instance (PP q a ~ s
        , PP p s ~ Maybe (b,s)
        , P q a
        , P p s
        , Show s
        , Show b
          )
     => P (Unfoldr p q) a where
  type PP (Unfoldr p q) a = [UnfoldrT (PP p (PP q a))]
  eval _ opts z = do
    let msg0 = "Unfoldr"
    qq <- eval (Proxy @q) opts z
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        let msg1 = msg0 <> " " <> showL opts q
            ff i s rs | i >= oRecursion opts = pure (rs, Left $ mkNode opts (FailT (msg1 <> ":recursion limit i=" <> showIndex i)) ("s=" <> showL opts s) [])
                      | otherwise = do
                              pp :: TT (PP p s) <- evalHide @p opts s
                              case getValueLR opts (msg1 <> " i=" <> showIndex i <> " s=" <> show s) pp [] of
                                   Left e  -> pure (rs, Left e)
                                   Right Nothing -> pure (rs ++ [((i,Nothing), pp)], Right ())
                                   Right w@(Just (_b,s')) -> ff (i+1) s' (rs ++ [((i,w), pp)])
        (ts,lr) :: ([((Int, PP p s), TT (PP p s))], Either (TT [b]) ()) <- ff 1 q []
        pure $ case splitAndAlign opts msg1 ts of
             Left e -> errorInProgram $ "Unfoldr e=" ++ show (fromTT e)
             Right abcs ->
               let vals = map (view _1) abcs
                   itts = map (view _2 &&& view _3) abcs
               in case lr of
                   Left e -> mkNode opts (_tBool e) msg1 (hh qq : map (hh . fixit) itts ++ [hh e])
                   Right () ->
                     let ret = fst <$> catMaybes vals
                     in mkNode opts (PresentT ret) (show01' opts msg1 ret "s=" q ) (hh qq : map (hh . fixit) itts)

type family UnfoldrT mbs where
  UnfoldrT (Maybe (b,s)) = b

-- | unfolds a value applying \'f\' until the condition \'p\' is true
--
-- >>> pl @(IterateUntil (Id < 90) (Pred Id)) 94
-- Present [94,93,92,91,90] (Unfoldr 94 [94,93,92,91,90] | s=94)
-- PresentT [94,93,92,91,90]
--
data IterateUntil p f
type IterateUntilT p f = IterateWhile (Not p) f

instance P (IterateUntilT p f) x => P (IterateUntil p f) x where
  type PP (IterateUntil p f) x = PP (IterateUntilT p f) x
  eval _ = eval (Proxy @(IterateUntilT p f))

-- | unfolds a value applying \'f\' while the condition \'p\' is true
--
-- >>> pl @(IterateWhile (Id > 90) (Pred Id)) 94
-- Present [94,93,92,91] (Unfoldr 94 [94,93,92,91] | s=94)
-- PresentT [94,93,92,91]
--
data IterateWhile p f
type IterateWhileT p f = Unfoldr (MaybeBool p '(Id, f)) Id

instance P (IterateWhileT p f) x => P (IterateWhile p f) x where
  type PP (IterateWhile p f) x = PP (IterateWhileT p f) x
  eval _ = eval (Proxy @(IterateWhileT p f))

-- | unfolds a value applying \'f\' while the condition \'p\' is true or \'n\' times
--
-- >>> pl @(IterateNWhile 10 (Id > 90) (Pred Id)) 95
-- Present [95,94,93,92,91] ((>>) [95,94,93,92,91] | {Map [95,94,93,92,91] | [(10,95),(9,94),(8,93),(7,92),(6,91)]})
-- PresentT [95,94,93,92,91]
--
-- >>> pl @(IterateNWhile 3 (Id > 90) (Pred Id)) 95
-- Present [95,94,93] ((>>) [95,94,93] | {Map [95,94,93] | [(3,95),(2,94),(1,93)]})
-- PresentT [95,94,93]
--
data IterateNWhile n p f
type IterateNWhileT n p f = '(n, Id) >> IterateWhile (Fst Id > 0 && (Snd Id >> p)) (Pred Id *** f) >> Map (Snd Id) Id

instance P (IterateNWhileT n p f) x => P (IterateNWhile n p f) x where
  type PP (IterateNWhile n p f) x = PP (IterateNWhileT n p f) x
  eval _ = eval (Proxy @(IterateNWhileT n p f))

-- | unfolds a value applying \'f\' until the condition \'p\' is true or \'n\' times
--
-- >>> pl @(IterateNUntil 10 (Id <= 90) (Pred Id)) 95
-- Present [95,94,93,92,91] ((>>) [95,94,93,92,91] | {Map [95,94,93,92,91] | [(10,95),(9,94),(8,93),(7,92),(6,91)]})
-- PresentT [95,94,93,92,91]
--
-- >>> pl @(IterateNUntil 3 (Id <= 90) (Pred Id)) 95
-- Present [95,94,93] ((>>) [95,94,93] | {Map [95,94,93] | [(3,95),(2,94),(1,93)]})
-- PresentT [95,94,93]
--
-- >>> pl @(IterateNUntil 9999 'False I) 1
-- Error Unfoldr (9999,1):recursion limit i=100 ((9999,1) (>>) rhs failed)
-- FailT "Unfoldr (9999,1):recursion limit i=100"
--
data IterateNUntil n p f
type IterateNUntilT n p f = IterateNWhile n (Not p) f

instance P (IterateNUntilT n p f) x => P (IterateNUntil n p f) x where
  type PP (IterateNUntil n p f) x = PP (IterateNUntilT n p f) x
  eval _ = eval (Proxy @(IterateNUntilT n p f))

data ParaImpl (n :: Nat) (os :: [k])

-- | runs values in parallel unlike 'Do' which is serial
--
-- >>> pz @(Para '[Id,Id + 1,Id * 4]) [10,20,30]
-- PresentT [10,21,120]
--
-- >>> pz @(Para '[Id,Id + 1,Id * 4]) [10,20,30,40]
-- FailT "Para:invalid length(4) expected 3"
--
-- >>> pl @(Para '[W 'True, Ge 12, W 'False, Lt 2]) [1,2,-99,-999]
-- Present [True,False,False,True] (Para(0) [True,False,False,True] | [1,2,-99,-999])
-- PresentT [True,False,False,True]
--
-- >>> pl @(Para '[W 'True, Ge 12, W 'False, Lt 2]) [1,2,-99]
-- Error Para:invalid length(3) expected 4
-- FailT "Para:invalid length(3) expected 4"
--
-- >>> pl @(Para '[W 'True, Ge 12, W 'False, Lt 2]) [1,2,-99,-999,1,1,2]
-- Error Para:invalid length(7) expected 4
-- FailT "Para:invalid length(7) expected 4"
--
data Para (ps :: [k])

-- passthru but adds the length of ps (replaces LenT in the type synonym to avoid type synonyms being expanded out
instance ([a] ~ x
        , GetLen ps
        , P (ParaImpl (LenT ps) ps) x
        ) => P (Para ps) x where
  type PP (Para ps) x = PP (ParaImpl (LenT ps) ps) x
  eval _ opts as = do
    let msg0 = "Para"
        n = getLen @ps
    if n /= length as then
       let msg1 = msg0 <> badLength as n
       in pure $ mkNode opts (FailT msg1) "" []
    else eval (Proxy @(ParaImpl (LenT ps) ps)) opts as

-- only allow non empty lists -- might need [a] ~ x but it seems fine
instance GL.TypeError ('GL.Text "ParaImpl '[] invalid: requires at least one value in the list")
   => P (ParaImpl n ('[] :: [k])) x where
  type PP (ParaImpl n ('[] :: [k])) x = Void
  eval _ _ _ = errorInProgram "ParaImpl empty list"

instance (Show (PP p a)
        , KnownNat n
        , Show a
        , P p a
        ) => P (ParaImpl n '[p]) [a] where
  type PP (ParaImpl n '[p]) [a] = [PP p a]
  eval _ opts as' = do
    let msgbase0 = "Para"
        msgbase1 = msgbase0 <> "(" <> show n <> ")"
        n :: Int
        n = nat @n
    case as' of
      [a] -> do
        pp <- eval (Proxy @p) opts a
        pure $ case getValueLR opts msgbase1 pp [] of
          Left e -> e
          -- showVerbose opts " " [b]  fails but using 'b' is ok and (b : []) also works!
          -- GE.List problem
          Right b -> mkNode opts (PresentT [b]) (msgbase1 <> " " <> showL opts [b] <> showVerbose opts " | " a) [hh pp]
      _ -> errorInProgram $ "ParaImpl base case should have exactly one element but found " ++ show as'

instance (KnownNat n
        , GetLen ps
        , P p a
        , P (ParaImpl n (p1 ': ps)) [a]
        , PP (ParaImpl n (p1 ': ps)) [a] ~ [PP p a]
        , Show a
        , Show (PP p a)
        )
     => P (ParaImpl n (p ': p1 ': ps)) [a] where
  type PP (ParaImpl n (p ': p1 ': ps)) [a] = [PP p a]
  eval _ opts as' = do
     let cpos = n-pos-1
         msgbase0 = msgbase2 <> "(" <> showIndex cpos <> " of " <> show n <> ")"
         msgbase1 = msgbase2 <> "(" <> showIndex cpos <> ")"
         msgbase2 = "Para"
         n = nat @n
         pos = 1 + getLen @ps -- cos p1!
     case as' of
       a:as -> do
         pp <- eval (Proxy @p) opts a
         case getValueLR opts msgbase0 pp [] of
           Left e -> pure e
           Right b -> do
                        qq <- eval (Proxy @(ParaImpl n (p1 ': ps))) opts as
                        pure $ case getValueLR opts (msgbase1 <> " rhs failed " <> show b) qq [hh pp] of
                          Left e -> e
                          Right bs -> mkNode opts (PresentT (b:bs)) (msgbase1 <> " " <> showL opts (b:bs) <> showVerbose opts " | " as') [hh pp, hh qq]
       _ -> errorInProgram "ParaImpl n+1 case has no data left"

-- | leverages 'Para' for repeating expressions (passthrough method)
--
-- >>> pz @(ParaN 4 (Succ Id)) [1..4]
-- PresentT [2,3,4,5]
--
-- >>> pz @(ParaN 4 (Succ Id)) "azwxm"
-- FailT "Para:invalid length(5) expected 4"
--
-- >>> pz @(ParaN 4 (Succ Id)) "azwx"
-- PresentT "b{xy"
--
-- >>> pl @(ParaN 5 (Guard "0-255" (Between 0 255 Id))) [1,2,3,4,12]
-- Present [1,2,3,4,12] (Para(0) [1,2,3,4,12] | [1,2,3,4,12])
-- PresentT [1,2,3,4,12]
--
-- >>> pl @(ParaN 5 (Guard "0-255" (Between 0 255 Id))) [1,2,3,400,12]
-- Error 0-255 (Para(0) rhs failed 1)
-- FailT "0-255"
--
-- >>> pl @(ParaN 4 (PrintF "%03d" Id)) [141,21,3,0::Int]
-- Present ["141","021","003","000"] (Para(0) ["141","021","003","000"] | [141,21,3,0])
-- PresentT ["141","021","003","000"]
--

data ParaN (n :: Nat) p

instance ( P (ParaImpl (LenT (RepeatT n p)) (RepeatT n p)) x
         , GetLen (RepeatT n p)
         , x ~ [a]
         ) => P (ParaN n p) x where
  type PP (ParaN n p) x = PP (Para (RepeatT n p)) x
  eval _ = eval (Proxy @(Para (RepeatT n p)))

-- | creates a promoted list of predicates and then evaluates them into a list. see PP instance for '[k]
--
-- >>> pz @(Repeat 4 (Succ Id)) 'c'
-- PresentT "dddd"
--
-- >>> pz @(Repeat 4 "abc") ()
-- PresentT ["abc","abc","abc","abc"]
--
-- >>> pl @(Repeat 4 "xy") 3
-- Present ["xy","xy","xy","xy"] ('["xy","xy","xy","xy"] ('"xy") | 3)
-- PresentT ["xy","xy","xy","xy"]
--
data Repeat (n :: Nat) p
instance P (RepeatT n p) a => P (Repeat n p) a where
  type PP (Repeat n p) a = PP (RepeatT n p) a
  eval _ = eval (Proxy @(RepeatT n p))

-- | leverages 'Do' for repeating predicates (passthrough method)
-- same as @DoN n p == FoldN n p Id@ but more efficient
--
-- >>> pz @(DoN 4 (Succ Id)) 'c'
-- PresentT 'g'
--
-- >>> pz @(DoN 4 (Id <> " | ")) "abc"
-- PresentT "abc |  |  |  | "
--
-- >>> pz @(DoN 4 (Id <> "|" <> Id)) "abc"
-- PresentT "abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc"
--
-- >>> pl @(DoN 4 (Id + 4)) 1
-- Present 17 ((>>) 17 | {13 + 4 = 17})
-- PresentT 17
--
-- >>> pl @(DoN 4 (Id + 7)) 3
-- Present 31 ((>>) 31 | {24 + 7 = 31})
-- PresentT 31
--
-- >>> pl @(DoN 4 9) ()
-- Present 9 ((>>) 9 | {'9})
-- PresentT 9
--
-- >>> pl @(DoN 4 "xy") 3
-- Present "xy" ((>>) "xy" | {'"xy"})
-- PresentT "xy"
--

data DoN (n :: Nat) p
type DoNT (n :: Nat) p = Do (RepeatT n p)
instance P (DoNT n p) a => P (DoN n p) a where
  type PP (DoN n p) a = PP (DoNT n p) a
  eval _ = eval (Proxy @(DoNT n p))

