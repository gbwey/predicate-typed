{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
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
     promoted conditional functions
-}
module Predicate.Data.Condition (
  -- ** conditional expressions
    If
  , Case
  , Case'
  , Case''
  , Guards
  , GuardsQuick
  , Guard
  , ExitWhen
  , GuardSimple
  , GuardsN
  , GuardsDetail
  , GuardBool

  , Bools
  , BoolsQuick
  , BoolsN

 ) where
import Predicate.Core
import Predicate.Util
import Predicate.Data.ReadShow (PrintT)
import GHC.TypeLits (Nat,KnownNat,ErrorMessage((:<>:)))
import qualified GHC.TypeLits as GL
import Control.Lens hiding (iall)
import Data.Proxy
import Data.Kind (Type)
import Data.Void
import qualified Data.Type.Equality as DE
-- $setup
-- >>> import Predicate.Prelude
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XAllowAmbiguousTypes
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> :set -XFlexibleContexts
-- >>> import qualified Data.Text as T


-- | similar to an if statement: if \'p\' then run \'q\' else run \'r\'
--
-- >>> pz @(If (Gt 4) "greater than 4" "less than or equal to 4") 10
-- PresentT "greater than 4"
--
-- >>> pz @(If (Gt 4) "greater than 4" "less than or equal to 4") 0
-- PresentT "less than or equal to 4"
--
-- >>> pz @(If (Snd Id == "a") '("xxx",Fst Id + 13) (If (Snd Id == "b") '("yyy",Fst Id + 7) (Failt _ "oops"))) (99,"b")
-- PresentT ("yyy",106)
--
-- >>> pl @(If (Len > 2) (Map (Succ Id) Id) (FailS "someval")) [12,15,16]
-- Present [13,16,17] (If 'True [13,16,17])
-- PresentT [13,16,17]
--
-- >>> pl @(Map (If (Lt 3) 'True (Failt _ "err")) Id) [1..10]
-- Error err(8) (Map(i=2, a=3) excnt=8)
-- FailT "err(8)"
--
-- >>> pl @(Map (If (Lt 3) 'True (Failt _ "someval")) Id) [1..10]
-- Error someval(8) (Map(i=2, a=3) excnt=8)
-- FailT "someval(8)"
--
-- >>> pl @(Map (If (Lt 3) 'True 'False) Id) [1..5]
-- Present [True,True,False,False,False] (Map [True,True,False,False,False] | [1,2,3,4,5])
-- PresentT [True,True,False,False,False]
--
-- >>> pl @(If (Gt 4) (Fail (Hole _) (PrintF "failing with %d" Id)) ()) 45
-- Error failing with 45 (If [True])
-- FailT "failing with 45"
--
-- >>> pl @(If (Gt 4) (Fail (Hole _) (PrintF "failing with %d" Id)) (Id * 7)) 3
-- Present 21 (If 'False 21)
-- PresentT 21
--
-- >>> pl @(If (Gt 4) (Fail (Hole _) (PrintF "failing with %d" Id)) (Id * 7 >> ShowP Id >> Ones Id)) 3
-- Present ["2","1"] (If 'False ["2","1"])
-- PresentT ["2","1"]
--
-- >>> pl @(If (Gt 4) (Fail (Hole _) (PrintF "failing with %d" Id)) (ShowP (Id * 7) >> Ones Id)) 19
-- Error failing with 19 (If [True])
-- FailT "failing with 19"
--
data If p q r

instance (Show (PP r a)
        , P p a
        , PP p a ~ Bool
        , P q a
        , P r a
        , PP q a ~ PP r a
        ) => P (If p q r) a where
  type PP (If p q r) a = PP q a
  eval _ opts a = do
    let msg0 = "If"
    pp <- evalBool (Proxy @p) opts a
    case getValueLR opts (msg0 <> " condition failed") pp [] of
      Left e -> pure e
      Right b -> do
        qqrr <- if b
              then eval (Proxy @q) opts a
              else eval (Proxy @r) opts a
        pure $ case getValueLR opts (msg0 <> " [" <> show b <> "]") qqrr [hh pp, hh qqrr] of
          Left e -> e
          Right ret -> mkNode opts (_tBool qqrr) (msg0 <> " " <> (if b then "'True " else "'False ") <> showL opts ret) [hh pp, hh qqrr]

type family GuardsT (ps :: [k]) where
  GuardsT '[] = '[]
  GuardsT (p ': ps) = Guard "fromGuardsT" p ': GuardsT ps

--type Guards' (ps :: [k]) = Para (GuardsT ps)

--type ToGuards (prt :: k) (os :: [k1]) = Proxy (Guards (ToGuardsT prt os))

type family ToGuardsT (prt :: k) (os :: [k1]) :: [(k,k1)] where
  ToGuardsT prt '[] = GL.TypeError ('GL.Text "ToGuardsT cannot be empty")
  ToGuardsT prt '[p] = '(prt,p) : '[]
  ToGuardsT prt (p ': ps) = '(prt,p) ': ToGuardsT prt ps

-- | tries each predicate ps and on the first match runs the corresponding qs but if there is no match on ps then runs the fail case e
--
-- >>> pz @(Case (Failt _ "asdf") '[Lt 4,Lt 10,Same 50] '[PrintF "%d is lt4" Id, PrintF "%d is lt10" Id, PrintF "%d is same50" Id] Id) 50
-- PresentT "50 is same50"
--
-- >>> pz @(Case (Failt _ "asdf") '[Lt 4,Lt 10,Same 50] '[PrintF "%d is lt4" Id, PrintF "%d is lt10" Id, PrintF "%d is same50" Id] Id) 9
-- PresentT "9 is lt10"
--
-- >>> pz @(Case (Failt _ "asdf") '[Lt 4,Lt 10,Same 50] '[PrintF "%d is lt4" Id, PrintF "%d is lt10" Id, PrintF "%d is same50" Id] Id) 3
-- PresentT "3 is lt4"
--
-- >>> pz @(Case (Failt _ "asdf") '[Lt 4,Lt 10,Same 50] '[PrintF "%d is lt4" Id, PrintF "%d is lt10" Id, PrintF "%d is same50" Id] Id) 99
-- FailT "asdf"
--
-- >>> pz @(Case (FailS "asdf" >> Snd Id >> Unproxy) '[Lt 4,Lt 10,Same 50] '[PrintF "%d is lt4" Id, PrintF "%d is lt10" Id, PrintF "%d is same50" Id] Id) 99
-- FailT "asdf"
--
-- >>> pz @(Case (Failt _ "x") '[Same "a",Same "b"] '["hey","there"] Id) "b"
-- PresentT "there"
--
-- >>> pz @(Case (Failt _ "x") '[Id == "a",Id == "b"] '["hey","there"] Id) "a"
-- PresentT "hey"
--
-- >>> pz @(Case (Failt _ "x") '[Same "a",Same "b"] '["hey","there"] Id) "c"
-- FailT "x"
--
data CaseImpl (n :: Nat) (e :: k0) (ps :: [k]) (qs :: [k1]) (r :: k2)
-- ps = conditions
-- qs = what to do [one to one with ps]
-- r = the value
-- e = otherwise  -- leave til later

-- | tries to match the value \'r\' with a condition in \'ps\' and if there is a match calls the associated \'qs\' entry else run \'e\'
--
-- >>> pl @(Case (Snd Id >> Failp "xx") '[Gt 3, Lt 2, Same 3] '["gt3","lt2","eq3"] Id) 15
-- Present "gt3" (Case(0 of 2) "gt3" | 15)
-- PresentT "gt3"
--
-- >>> pl @(Case (Snd Id >> Failp "xx") '[Gt 3, Lt 2, Same 3] '["gt3","lt2","eq3"] Id) 1
-- Present "lt2" (Case(0) "lt2" | 1)
-- PresentT "lt2"
--
-- >>> pl @(Case (Snd Id >> Failp "xx") '[Gt 3, Lt 2, Same 3] '["gt3","lt2","eq3"] Id) 3
-- Present "eq3" (Case(0) "eq3" | 3)
-- PresentT "eq3"
--
-- >>> pl @(Case (Snd Id >> Failp "no match") '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
-- Error no match (Case:otherwise failed:Proxy)
-- FailT "no match"
--
-- >>> pl @(Case (Fail (Snd Id >> Unproxy) (PrintF "no match for %03d" (Fst Id))) '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
-- Error no match for 015 (Case:otherwise failed:Fail no match for 015)
-- FailT "no match for 015"
--
-- >>> pl @(Case "other" '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
-- Present "other" (Case(0) "other" | 15)
-- PresentT "other"
--
-- >>> pl @(Case (ShowP (Fst Id) >> Id <> Id <> Id) '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
-- Present "151515" (Case(0) "151515" | 15)
-- PresentT "151515"
--
data Case (e :: k0) (ps :: [k]) (qs :: [k1]) (r :: k2)

-- | like 'Case' but uses a generic error message (skips the \'e\' parameter)
--
-- >>> pl @(Case' '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
-- Error Case:no match (Case:otherwise failed:Proxy)
-- FailT "Case:no match"
--
data Case' (ps :: [k]) (qs :: [k1]) (r :: k2)

-- | like 'Case' but allows you to use the value in the error message
--
-- >>> pl @(Case'' (PrintF "no match for %03d" Id) '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 15
-- Error no match for 015 (Case:otherwise failed:Fail no match for 015)
-- FailT "no match for 015"
--
-- >>> pl @(Case'' (PrintF "no match for %03d" Id) '[Same 1, Same 2, Same 3] '["eq1","eq2","eq3"] Id) 2
-- Present "eq2" (Case(0) "eq2" | 2)
-- PresentT "eq2"
--
-- >>> pl @(Case'' (PrintF "no match for %04d" Id) '[Between 0 5 Id, Same 6, Between 7 10 Id] '[ 'LT, 'EQ, 'GT] Id) (-12)
-- Error no match for -012 (Case:otherwise failed:Fail no match for -012)
-- FailT "no match for -012"
--
data Case'' s (ps :: [k]) (qs :: [k1]) (r :: k2)

type CaseT' (ps :: [k]) (qs :: [k1]) (r :: k2) = Case (Snd Id >> Failp "Case:no match") ps qs r
type CaseT'' s (ps :: [k]) (qs :: [k1]) (r :: k2) = Case (FailCaseT s) ps qs r -- eg s= PrintF "%s" (ShowP Id)

instance P (CaseT'' s ps qs r) x => P (Case'' s ps qs r) x where
  type PP (Case'' s ps qs r) x = PP (CaseT'' s ps qs r) x
  eval _ = eval (Proxy @(CaseT'' s ps qs r))

instance P (CaseT' ps qs r) x => P (Case' ps qs r) x where
  type PP (Case' ps qs r) x = PP (CaseT' ps qs r) x
  eval _ = eval (Proxy @(CaseT' ps qs r))

type FailCaseT p = Fail (Snd Id >> Unproxy) (Fst Id >> p)

type CaseImplT e ps qs r = CaseImpl (LenT ps) e ps qs r

-- passthru but adds the length of ps (replaces LenT in the type synonym to avoid type synonyms being expanded out
instance (FailUnlessT (LenT ps DE.== LenT qs)
                  ('GL.Text "lengths are not the same "
                   ':<>: 'GL.ShowType (LenT ps)
                   ':<>: 'GL.Text " vs "
                   ':<>: 'GL.ShowType (LenT qs))
        , P (CaseImplT e ps qs r) x
        ) => P (Case e ps qs r) x where
  type PP (Case e ps qs r) x = PP (CaseImplT e ps qs r) x
  eval _ = eval (Proxy @(CaseImplT e ps qs r))

-- only allow non empty lists!
instance (GL.TypeError ('GL.Text "CaseImpl '[] invalid: lhs requires at least one value in the list"))
   => P (CaseImpl n e ('[] :: [k]) (q ': qs) r) x where
  type PP (CaseImpl n e ('[] :: [k]) (q ': qs) r) x = Void
  eval _ _ _ = errorInProgram "CaseImpl lhs empty"

instance (GL.TypeError ('GL.Text "CaseImpl '[] invalid: rhs requires at least one value in the list"))
   => P (CaseImpl n e (p ': ps) ('[] :: [k1]) r) x where
  type PP (CaseImpl n e (p ': ps) ('[] :: [k1]) r) x = Void
  eval _ _ _ = errorInProgram "CaseImpl rhs empty"

instance (GL.TypeError ('GL.Text "CaseImpl '[] invalid: lists are both empty"))
   => P (CaseImpl n e ('[] :: [k]) ('[] :: [k1]) r) x where
  type PP (CaseImpl n e ('[] :: [k]) ('[] :: [k1]) r) x = Void
  eval _ _ _ = errorInProgram "CaseImpl both lists empty"

instance (P r x
        , P q (PP r x)
        , Show (PP q (PP r x))
        , P p (PP r x)
        , PP p (PP r x) ~ Bool
        , KnownNat n
        , Show (PP r x)
        , P e (PP r x, Proxy (PP q (PP r x)))
        , PP e (PP r x, Proxy (PP q (PP r x))) ~ PP q (PP r x)
        ) => P (CaseImpl n e '[p] '[q] r) x where
  type PP (CaseImpl n e '[p] '[q] r) x = PP q (PP r x)
  eval _ opts z = do
    let msgbase0 = "Case(" <> show (n-1) <> ")"
        n :: Int = nat @n
    rr <- eval (Proxy @r) opts z
    case getValueLR opts msgbase0 rr [] of
      Left e -> pure e
      Right a -> do
        pp <- evalBool (Proxy @p) opts a
        case getValueLR opts msgbase0 pp [hh rr] of
          Left e -> pure e
          Right True -> do
            qq <- eval (Proxy @q) opts a
            pure $ case getValueLR opts msgbase0 qq [hh rr, hh pp] of
              Left e -> e
              Right b -> mkNode opts (PresentT b) (show01 opts msgbase0 b a) (hh rr : hh pp : [hh qq | isVerbose opts])
          Right False -> do
            ee <- eval (Proxy @e) opts (a, Proxy @(PP q (PP r x)))
            pure $ case getValueLR opts ("Case:otherwise failed" <> nullIf ":" (_tString ee)) ee [hh rr, hh pp] of
              Left e -> e
              Right b -> mkNode opts (PresentT b) (show01 opts msgbase0 b a) [hh rr, hh pp, hh ee]

instance (KnownNat n
        , GetLen ps
        , P r x
        , P p (PP r x)
        , P q (PP r x)
        , PP p (PP r x) ~ Bool
        , Show (PP q (PP r x))
        , Show (PP r x)
        , P (CaseImpl n e (p1 ': ps) (q1 ': qs) r) x
        , PP (CaseImpl n e (p1 ': ps) (q1 ': qs) r) x ~ PP q (PP r x)
        )
     => P (CaseImpl n e (p ': p1 ': ps) (q ': q1 ': qs) r) x where
  type PP (CaseImpl n e (p ': p1 ': ps) (q ': q1 ': qs) r) x = PP q (PP r x)
  eval _ opts z = do
    let cpos = n-pos-1
        msgbase0 = "Case(" <> showIndex cpos <> " of " <> show (n-1) <> ")"
        msgbase1 = "Case(" <> showIndex cpos <> ")"
        n = nat @n
        pos = 1 + getLen @ps -- cos p1!
    rr <- eval (Proxy @r) opts z
    case getValueLR opts msgbase0 rr [] of
      Left e -> pure e
      Right a -> do
        pp <- evalBool (Proxy @p) opts a
        case getValueLR opts msgbase0 pp [hh rr] of
          Left e -> pure e
          Right True -> do
            qq <- eval (Proxy @q) opts a
            pure $ case getValueLR opts msgbase0 qq [hh pp, hh rr] of
              Left e -> e
              Right b -> mkNode opts (PresentT b) (show01 opts msgbase0 b a) (hh rr : hh pp : [hh qq | isVerbose opts])
          Right False -> do
            ww <- eval (Proxy @(CaseImpl n e (p1 ': ps) (q1 ': qs) r)) opts z
            pure $ case getValueLR opts (_tString ww) ww [hh rr, hh pp] of
              Left e -> e -- use original failure msg
              Right b -> mkNode opts (PresentT b) (show01 opts msgbase1 b a) [hh rr, hh pp, hh ww]


data GuardsImpl (n :: Nat) (os :: [(k,k1)])

-- isbn 10 tests (dont need first guard as Zip enforces same length: handles case insensitive \'x\' as check digit)


-- | Guards contain a type level list of tuples the action to run on failure of the predicate and the predicate itself
--   Each tuple validating against the corresponding value in a value list
--
-- \'prt\' receives (Int,a) as input which is the position and value if there is a failure
--
-- >>> pz @(Guards '[ '("arg1 failed",Gt 4), '("arg2 failed", Same 4)]) [17,4]
-- PresentT [17,4]
--
-- >>> pz @(Guards '[ '("arg1 failed",Gt 4), '("arg2 failed", Same 5)]) [17,4]
-- FailT "arg2 failed"
--
-- >>> pz @(Guards '[ '("arg1 failed",Gt 99), '("arg2 failed", Same 4)]) [17,4]
-- FailT "arg1 failed"
--
-- >>> pz @(Guards '[ '(PrintT "arg %d failed with value %d" Id,Gt 4), '(PrintT "%d %d" Id, Same 4)]) [17,3]
-- FailT "1 3"
--
-- >>> pz @(Msg "isbn10" (Resplit "-" Id) >> Concat Id >> 'Just Unsnoc >> Map (ReadP Int (Singleton Id)) Id *** If (Singleton Id ==~ "X") 10 (ReadP Int (Singleton Id)) >> ZipWith (Fst Id * Snd Id) (1...10 >> Reverse) (Fst Id +: Snd Id) >> Sum >> Guard ("mod 0 oops") (Id `Mod` 11 == 0)) "0-306-40614-X"
-- FailT "mod 0 oops"
--
-- >>> pz @(Resplit "-" Id >> Concat Id >> 'Just Unsnoc >> Map (ReadP Int (Singleton Id)) Id *** If (Singleton Id ==~ "X") 10 (ReadP Int (Singleton Id)) >> ZipWith (Fst Id * Snd Id) (1...10 >> Reverse) (Fst Id +: Snd Id) >> Sum >> Guard ("mod 0 oops") (Id `Mod` 11 == 0)) "0-306-40611-X"
-- PresentT 132
--
-- >>> pz @(Msg "isbn13" (Resplit "-" Id) >> Concat Id >> Map (ReadP Int (Singleton Id)) Id >> ZipWith (Fst Id * Snd Id) (Cycle 13 [1,3] >> Reverse) Id >> Sum >> '(Id,Id `Mod` 10) >> Guard (PrintT "sum=%d mod 10=%d" Id) (Snd Id == 0)) "978-0-306-40615-7"
-- PresentT (100,0)
--
-- >>> pz @(Resplit "-" Id >> Concat Id >> Map (ReadP Int (Singleton Id)) Id >> ZipWith (Fst Id * Snd Id) (Cycle 13 [1,3] >> Reverse) Id >> Sum >> '(Id,Id `Mod` 10) >> Guard (PrintT "sum=%d mod 10=%d" Id) (Snd Id == 0)) "978-0-306-40615-8"
-- FailT "sum=101 mod 10=1"
--
-- >>> pz @(Do '[Resplit "-" Id, Concat Id, ZipWith (Fst Id * Snd Id) (Cycle 13 [1,3]) (Map (ReadP Int (Singleton Id)) Id), Sum, Guard (PrintF "%d is not evenly divisible by 10" Id) (Id `Mod` 10 == 0)]) "978-0-7167-0344-9"
-- FailT "109 is not evenly divisible by 10"
--
-- >>> pz @(Do '[Resplit "-" Id, Concat Id, ZipWith (Fst Id * Snd Id) (Cycle 13 [1,3]) (Map (ReadP Int (Singleton Id)) Id), Sum, Guard (PrintF "%d is not evenly divisible by 10" Id) (Id `Mod` 10 == 0)]) "978-0-7167-0344-0"
-- PresentT 100
--
data Guards (ps :: [(k,k1)])

instance ( [a] ~ x
         , GetLen ps
         , P (GuardsImpl (LenT ps) ps) x
         ) => P (Guards ps) x where
  type PP (Guards ps) x = PP (GuardsImpl (LenT ps) ps) x
  eval _ opts as = do
    let msg0 = "Guards"
        n = getLen @ps
    if n /= length as then
       let msg1 = msg0 <> badLength as n
       in pure $ mkNode opts (FailT msg1) "" []
    else eval (Proxy @(GuardsImpl (LenT ps) ps)) opts as

instance ( [a] ~ x
         , Show a
         ) => P (GuardsImpl n ('[] :: [(k,k1)])) x where
  type PP (GuardsImpl n ('[] :: [(k,k1)])) x = x
  eval _ opts as =
    let msg0 = "Guards"
    in if not (null as) then errorInProgram $ "GuardsImpl base case has extra data " ++ show as
       else pure $ mkNode opts (PresentT as) (msg0 <> " no data") []

instance (PP prt (Int, a) ~ String
        , P prt (Int, a)
        , KnownNat n
        , GetLen ps
        , P p a
        , PP p a ~ Bool
        , P (GuardsImpl n ps) [a]
        , PP (GuardsImpl n ps) [a] ~ [a]
        , Show a
        , [a] ~ x
        ) => P (GuardsImpl n ('(prt,p) ': ps)) x where
  type PP (GuardsImpl n ('(prt,p) ': ps)) x = x
  eval _ opts as' = do
     let cpos = n-pos-1
         msgbase1 = "Guard(" <> show cpos <> ")"
         msgbase2 = "Guards"
         n :: Int
         n = nat @n
         pos = getLen @ps
     case as' of
         a:as -> do
            pp <- evalBoolHide @p opts a
            case getValueLR opts (msgbase1 <> " p failed") pp [] of
                 Left e -> pure e
                 Right False -> do
                   qq <- eval (Proxy @prt) opts (cpos,a) -- only run prt when predicate is False
                   pure $ case getValueLR opts (msgbase2 <> " False predicate and prt failed") qq [hh pp] of
                      Left e -> e
                      Right msgx -> mkNode opts (FailT msgx) (msgbase1 <> " failed [" <> msgx <> "] " <> showL opts a) (hh pp : [hh qq | isVerbose opts])
                 Right True ->
                   if pos == 0 then -- we are at the bottom of the tree
                      pure $ mkNode opts (PresentT [a]) msgbase2 [hh pp]
                   else do
                     ss <- eval (Proxy @(GuardsImpl n ps)) opts as
                     pure $ case getValueLR opts (_tString ss) ss [hh pp] of
                       Left e -> e -- shortcut else we get too compounding errors with the pp tree being added each time!
                       Right zs -> (ss & tForest %~ \x -> fromTT pp : x) & tBool .~ PresentT (a:zs)
         _ -> errorInProgram "GuardsImpl n+1 case has no data"

-- | GuardsQuick contain a type level list of conditions and one of matching values: on no match will fail using the first parameter
--
-- >>> pz @(GuardsQuick (PrintT "arg %d failed with value %d" Id) '[Gt 4, Ge 3, Same 4]) [17,3,5]
-- FailT "arg 2 failed with value 5"
--
-- >>> pz @(GuardsQuick (PrintT "arg %d failed with value %d" Id) '[Gt 4, Ge 3, Same 4]) [17,3,5,99]
-- FailT "Guards:invalid length(4) expected 3"
--
-- >>> pl @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 0 11 Id, Between 1 4 Id,Between 3 5 Id]) [10::Int,2,5]
-- Present [10,2,5] (Guards)
-- PresentT [10,2,5]
--
-- >>> pl @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]) [31,11,1999::Int]
-- Present [31,11,1999] (Guards)
-- PresentT [31,11,1999]
--
-- >>> pl @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]) [31,11::Int]
-- Error Guards:invalid length(2) expected 3
-- FailT "Guards:invalid length(2) expected 3"
--
-- >>> pl @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]) [31,13,1999::Int]
-- Error guard(1) 13 is out of range (Guard(1) failed [guard(1) 13 is out of range] 13)
-- FailT "guard(1) 13 is out of range"
--
-- >>> pl @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]) [0,44,1999::Int]
-- Error guard(0) 0 is out of range (Guard(0) failed [guard(0) 0 is out of range] 0)
-- FailT "guard(0) 0 is out of range"
--
-- >>> pl @(GuardsQuick (PrintT "guard(%d) %d is out of range" Id) '[Between 1 31 Id, Between 1 12 Id, Between 1990 2050 Id]) [31,11,2000,1,2::Int]
-- Error Guards:invalid length(5) expected 3
-- FailT "Guards:invalid length(5) expected 3"
--
-- >>> pl @(GuardsQuick (PrintT "guard(%d) err %03d" Id) '[W 'True, Ge 12, W 'False, Lt 2]) [1,2,-99,-999]
-- Error guard(1) err 002 (Guard(1) failed [guard(1) err 002] 2)
-- FailT "guard(1) err 002"
--
-- >>> pl @(GuardsQuick (PrintT "guard(%d) err %03d" Id) '[W 'True, Ge 12, W 'False, Lt 2]) [1,2,-99]
-- Error Guards:invalid length(3) expected 4
-- FailT "Guards:invalid length(3) expected 4"
--
-- >>> pl @(GuardsQuick (PrintT "guard(%d) err %03d" Id) '[W 'True, Ge 12, W 'True, Lt 2]) [1,22,-99,-999,1,1,2]
-- Error Guards:invalid length(7) expected 4
-- FailT "Guards:invalid length(7) expected 4"
--
data GuardsQuick (prt :: k) (ps :: [k1])
type GuardsQuickT (prt :: k) (ps :: [k1]) = Guards (ToGuardsT prt ps)

instance P (GuardsQuickT prt ps) x => P (GuardsQuick prt ps) x where
  type PP (GuardsQuick prt ps) x = PP (GuardsQuickT prt ps) x
  eval _ = eval (Proxy @(GuardsQuickT prt ps))

-- prefer 'Bools' as 'BoolsQuick' doesnt give much added value: passes in the index and the value to prt but you already have the index in the message
-- pulls the top message from the tree if a predicate is false

-- | boolean guard which checks a given a list of predicates against the list of values
--
-- >>> pl @(Bools '[ '(W "hh",Between 0 23 Id), '(W "mm",Between 0 59 Id), '(PrintT "<<<%d %d>>>" Id,Between 0 59 Id) ]) [12,93,14]
-- Error Bool(1) [mm] (93 <= 59)
-- FailT "Bool(1) [mm] (93 <= 59)"
--
-- >>> pl @(Bools '[ '(W "hh",Between 0 23 Id), '(W "mm",Between 0 59 Id), '(PrintT "<<<%d %d>>>" Id,Between 0 59 Id) ]) [12,13,94]
-- Error Bool(2) [<<<2 94>>>] (94 <= 59)
-- FailT "Bool(2) [<<<2 94>>>] (94 <= 59)"
--
-- >>> pl @(Bools '[ '(W "hh",Between 0 23 Id), '(W "mm",Between 0 59 Id), '(PrintT "<<<%d %d>>>" Id,Between 0 59 Id) ]) [12,13,14]
-- True (Bools)
-- TrueT
--
-- >>> pl @(Bools '[ '("hours",Between 0 23 Id), '("minutes",Between 0 59 Id), '("seconds",Between 0 59 Id)]) [12,13,14]
-- True (Bools)
-- TrueT
--
-- >>> pl @(Bools '[ '("hours",Between 0 23 Id), '("minutes",Between 0 59 Id), '("seconds",Between 0 59 Id)]) [12,60,14]
-- Error Bool(1) [minutes] (60 <= 59)
-- FailT "Bool(1) [minutes] (60 <= 59)"
--
-- >>> pl @(Bools '[ '("hours",Between 0 23 Id), '("minutes",Between 0 59 Id), '("seconds",Between 0 59 Id)]) [12,60,14,20]
-- Error Bools:invalid length(4) expected 3
-- FailT "Bools:invalid length(4) expected 3"
--
data Bools (ps :: [(k,k1)])

instance ([a] ~ x
        , GetLen ps
        , P (BoolsImpl (LenT ps) ps) x
        , PP (BoolsImpl (LenT ps) ps) x ~ Bool
        ) => P (Bools ps) x where
  type PP (Bools ps) x = Bool
  eval _ opts as = do
    let msg0 = "Bools"
        msg1 = "Bool(" <> show (n-1) <> ")"
        n = getLen @ps
    case chkSize opts msg1 as [] of
      Left e -> pure e
      Right () ->
        if n /= length as then
           let msg2 = msg0 <> badLength as n
           in pure $ mkNode opts (FailT msg2) "" []
        else evalBool (Proxy @(BoolsImpl (LenT ps) ps)) opts as

data BoolsImpl (n :: Nat) (os :: [(k,k1)])

instance (KnownNat n
        , Show a
        , [a] ~ x
        ) => P (BoolsImpl n ('[] :: [(k,k1)])) x where
  type PP (BoolsImpl n ('[] :: [(k,k1)])) x = Bool
  eval _ opts as =
    let msg0 = "Bool(" <> show (n-1) <> ")"
        n :: Int = nat @n
    in if not (null as) then errorInProgram $ "BoolsImpl base case has extra data " ++ show as
       else pure $ mkNodeB opts True (msg0 <> " empty") []

instance (PP prt (Int, a) ~ String
        , P prt (Int, a)
        , KnownNat n
        , GetLen ps
        , P p a
        , PP p a ~ Bool
        , P (BoolsImpl n ps) x
        , PP (BoolsImpl n ps) [a] ~ Bool
        , [a] ~ x
        ) => P (BoolsImpl n ('(prt,p) ': ps)) x where
  type PP (BoolsImpl n ('(prt,p) ': ps)) x = Bool
  eval _ opts as' = do
     let cpos = n-pos-1
         msgbase1 = "Bool(" <> showIndex cpos <> ")"
         msgbase2 = "Bools"
         n :: Int = nat @n
         pos = getLen @ps
     case as' of
         a:as -> do
            pp <- evalBoolHide @p opts a
            case getValueLR opts (msgbase1 <> " p failed") pp [] of
                 Left e -> pure e
                 Right False -> do
                   qq <- eval (Proxy @prt) opts (cpos,a) -- only run prt when predicate is False
                   pure $ case getValueLR opts (msgbase2 <> " False predicate and prt failed") qq [hh pp] of
                      Left e -> e
                      Right msgx -> mkNode opts (FailT (msgbase1 <> " [" <> msgx <> "]" <> nullSpace (topMessage pp))) "" (hh pp : [hh qq | isVerbose opts])
                 Right True ->
                   if pos == 0 then -- we are at the bottom of the tree
                      pure $ mkNodeB opts True msgbase2 [hh pp]
                   else do
                     ss <- evalBool (Proxy @(BoolsImpl n ps)) opts as
                     pure $ case getValueLR opts (_tString ss) ss [hh pp] of
                       Left e -> e -- shortcut else we get too compounding errors with the pp tree being added each time!
                       Right _ ->  ss & tForest %~ \x -> fromTT pp : x
         _ -> errorInProgram "BoolsImpl n+1 case has no data"

-- | boolean guard which checks a given a list of predicates against the list of values
--
-- >>> pl @(BoolsQuick "abc" '[Between 0 23 Id, Between 0 59 Id, Between 0 59 Id]) [12,13,14]
-- True (Bools)
-- TrueT
--
-- >>> pl @(BoolsQuick (PrintT "id=%d val=%d" Id) '[Between 0 23 Id, Between 0 59 Id, Between 0 59 Id]) [12,13,14]
-- True (Bools)
-- TrueT
--
-- >>> pl @(BoolsQuick (PrintT "id=%d val=%d" Id) '[Between 0 23 Id, Between 0 59 Id, Between 0 59 Id]) [12,13,99]
-- Error Bool(2) [id=2 val=99] (99 <= 59)
-- FailT "Bool(2) [id=2 val=99] (99 <= 59)"
--

data BoolsQuick (prt :: k) (ps :: [k1])
type BoolsQuickT (prt :: k) (ps :: [k1]) = Bools (ToGuardsT prt ps)

-- why do we need this? when BoolsN works without [use the x ~ [a] trick in BoolsN]
instance (PP (Bools (ToGuardsT prt ps)) x ~ Bool
        , P (BoolsQuickT prt ps) x
          ) => P (BoolsQuick prt ps) x where
  type PP (BoolsQuick prt ps) x = Bool
  eval _ = evalBool (Proxy @(BoolsQuickT prt ps))

-- | leverages 'RepeatT' for repeating predicates (passthrough method)
--
-- >>> pl @(BoolsN (PrintT "id=%d must be between 0 and 255, found %d" Id) 4 (Between 0 255 Id)) [121,33,7,256]
-- Error Bool(3) [id=3 must be between 0 and 255, found 256] (256 <= 255)
-- FailT "Bool(3) [id=3 must be between 0 and 255, found 256] (256 <= 255)"
--
-- >>> pl @(BoolsN (PrintT "id=%d must be between 0 and 255, found %d" Id) 4 (Between 0 255 Id)) [121,33,7,44]
-- True (Bools)
-- TrueT
--
data BoolsN prt (n :: Nat) (p :: k1)
type BoolsNT prt (n :: Nat) (p :: k1) = Bools (ToGuardsT prt (RepeatT n p))

instance ( x ~ [a]
         , P (BoolsNT prt n p) x
         ) => P (BoolsN prt n p) x where
  type PP (BoolsN prt n p) x = Bool
  eval _ = evalBool (Proxy @(BoolsNT prt n p))

-- | if a predicate fails then then the corresponding symbol and value will be passed to the print function
--
-- >>> pz @(GuardsDetail "%s invalid: found %d" '[ '("hours", Between 0 23 Id),'("minutes",Between 0 59 Id),'("seconds",Between 0 59 Id)]) [13,59,61]
-- FailT "seconds invalid: found 61"
--
-- >>> pz @(GuardsDetail "%s invalid: found %d" '[ '("hours", Between 0 23 Id),'("minutes",Between 0 59 Id),'("seconds",Between 0 59 Id)]) [27,59,12]
-- FailT "hours invalid: found 27"
--
-- >>> pz @(GuardsDetail "%s invalid: found %d" '[ '("hours", Between 0 23 Id),'("minutes",Between 0 59 Id),'("seconds",Between 0 59 Id)]) [23,59,12]
-- PresentT [23,59,12]
--
data GuardsDetailImpl (ps :: [(k,k1)])

instance ([a] ~ x
        , GetLen ps
        , P (GuardsImplX (LenT ps) ps) x
        ) => P (GuardsDetailImpl ps) x where
  type PP (GuardsDetailImpl ps) x = PP (GuardsImplX (LenT ps) ps) x
  eval _ opts as = do
    let msg0 = "Guards"
        n = getLen @ps
    if n /= length as then
       let msg1 = msg0 <> badLength as n
       in pure $ mkNode opts (FailT msg1) "" []
    else eval (Proxy @(GuardsImplX (LenT ps) ps)) opts as

data GuardsImplX (n :: Nat) (os :: [(k,k1)])

instance ( [a] ~ x
         , Show a
         ) => P (GuardsImplX n ('[] :: [(k,k1)])) x where
  type PP (GuardsImplX n ('[] :: [(k,k1)])) x = x
  eval _ opts as =
    let msg0 = "Guards"
        -- n :: Int = nat @n
    in if not (null as) then errorInProgram $ "GuardsImplX base case has extra data " ++ show as
       else pure $ mkNode opts (PresentT as) msg0 []

instance (PP prt a ~ String
        , P prt a
        , KnownNat n
        , GetLen ps
        , P p a
        , PP p a ~ Bool
        , P (GuardsImplX n ps) [a]
        , PP (GuardsImplX n ps) [a] ~ [a]
        , Show a
        , [a] ~ x
        ) => P (GuardsImplX n ('(prt,p) ': ps)) x where
  type PP (GuardsImplX n ('(prt,p) ': ps)) x = x
  eval _ opts as' = do
     let cpos = n-pos-1
         msgbase1 = "Guard(" <> showIndex cpos <> ")"
         msgbase2 = "Guards"
         n :: Int = nat @n
         pos = getLen @ps
     case as' of
         a:as -> do
            pp <- evalBoolHide @p opts a
            case getValueLR opts (msgbase1 <> " p failed") pp [] of
                 Left e -> pure e
                 Right False -> do
                   qq <- eval (Proxy @prt) opts a -- only run prt when predicate is False
                   pure $ case getValueLR opts (msgbase2 <> " False predicate and prt failed") qq [hh pp] of
                      Left e -> e
                      Right msgx -> mkNode opts (FailT msgx) (msgbase1 <> " failed [" <> msgx <> "] " <> showL opts a) (hh pp : [hh qq | isVerbose opts])
                 Right True -> do
                   ss <- eval (Proxy @(GuardsImplX n ps)) opts as
                   pure $ case getValueLR opts (_tString ss) ss [hh pp] of
                     Left e -> e -- shortcut else we get too compounding errors with the pp tree being added each time!
                     Right zs -> mkNode opts (PresentT (a:zs)) (msgbase1 <> " " <> showL opts a) [hh pp, hh ss]
         _ -> errorInProgram "GuardsImplX n+1 case has no data"

data GuardsDetail prt (ps :: [(k0,k1)])
type GuardsDetailT prt (ps :: [(k0,k1)]) = GuardsDetailImpl (ToGuardsDetailT prt ps)

instance P (GuardsDetailT prt ps) x => P (GuardsDetail prt ps) x where
  type PP (GuardsDetail prt ps) x = PP (GuardsDetailT prt ps) x
  eval _ = eval (Proxy @(GuardsDetailT prt ps))

type family ToGuardsDetailT (prt :: k1) (os :: [(k2,k3)]) :: [(Type,k3)] where
  ToGuardsDetailT prt '[ '(s,p) ] = '(PrintT prt '(s,Id), p) : '[]
  ToGuardsDetailT prt ( '(s,p) ': ps) = '(PrintT prt '(s,Id), p) ': ToGuardsDetailT prt ps
  ToGuardsDetailT prt '[] = GL.TypeError ('GL.Text "ToGuardsDetailT cannot be empty")

-- | leverages 'RepeatT' for repeating predicates (passthrough method)
--
-- >>> pz @(GuardsN (PrintT "id=%d must be between 0 and 255, found %d" Id) 4 (Between 0 255 Id)) [121,33,7,256]
-- FailT "id=3 must be between 0 and 255, found 256"
--
-- >>> pz @(GuardsN (PrintT "id=%d must be between 0 and 255, found %d" Id) 4 (Between 0 255 Id)) [121,33,7,44]
-- PresentT [121,33,7,44]
--
-- >>> pl @(GuardsN (PrintT "guard(%d) %d is out of range" Id) 4 (Between 0 255 Id)) [1,2,3,4::Int]
-- Present [1,2,3,4] (Guards)
-- PresentT [1,2,3,4]
--
-- >>> pl @(GuardsN (PrintT "guard(%d) %d is out of range" Id) 4 (Between 0 255 Id)) [1,2,3,4,5::Int]
-- Error Guards:invalid length(5) expected 4
-- FailT "Guards:invalid length(5) expected 4"
--
-- >>> pl @(GuardsN (PrintT "guard(%d) %d is out of range" Id) 4 (Between 0 255 Id)) [1,2,3::Int]
-- Error Guards:invalid length(3) expected 4
-- FailT "Guards:invalid length(3) expected 4"
--
data GuardsN prt (n :: Nat) p
type GuardsNT prt (n :: Nat) p = Guards (ToGuardsT prt (RepeatT n p))

instance ( x ~ [a]
         , P (GuardsNT prt n p) x
         ) => P (GuardsN prt n p) x where
  type PP (GuardsN prt n p) x = PP (GuardsNT prt n p) x
  eval _ = eval (Proxy @(GuardsNT prt n p))

-- | \'p\' is the predicate and on failure of the predicate runs \'prt\'
--
-- >>> pz @(Guard "expected > 3" (Gt 3)) 17
-- PresentT 17
--
-- >>> pz @(Guard "expected > 3" (Gt 3)) 1
-- FailT "expected > 3"
--
-- >>> pz @(Guard (PrintF "%d not > 3" Id) (Gt 3)) (-99)
-- FailT "-99 not > 3"
--
-- >>> pl @(Map (Guard "someval" (Lt 3) >> 'True) Id) [1::Int ..10]
-- Error someval(8) (Map(i=2, a=3) excnt=8)
-- FailT "someval(8)"
--
-- >>> pl @(Guard "someval" (Len == 2) >> (ShowP Id &&& Id)) ([] :: [Int])
-- Error someval
-- FailT "someval"
--
-- >>> pl @(Guard "someval" (Len == 2) >> (Id &&& ShowP Id)) [2,3]
-- Present ([2,3],"[2,3]") ((>>) ([2,3],"[2,3]") | {W '([2,3],"[2,3]")})
-- PresentT ([2,3],"[2,3]")
--
-- >>> pl @(Guard "someval" (Len == 2) >> (ShowP Id &&& Id)) [2,3,4]
-- Error someval
-- FailT "someval"
--
-- >>> pl @(Map (Guard "someval" (Lt 3) >> 'True) Id) [1::Int ..10]
-- Error someval(8) (Map(i=2, a=3) excnt=8)
-- FailT "someval(8)"
--
-- >>> pl @(Guard "oops" (Len > 2) >> Map (Succ Id) Id) [12,15,16]
-- Present [13,16,17] ((>>) [13,16,17] | {Map [13,16,17] | [12,15,16]})
-- PresentT [13,16,17]
--
-- >>> pl @(Guard "err" (Len > 2) >> Map (Succ Id) Id) [12]
-- Error err
-- FailT "err"
--
-- >>> pl @(Guard (PrintF "err found len=%d" Len) (Len > 5) >> Map (Succ Id) Id) [12,15,16]
-- Error err found len=3
-- FailT "err found len=3"
--
data Guard prt p


instance (Show a
        , P prt a
        , PP prt a ~ String
        , P p a
        , PP p a ~ Bool
        ) => P (Guard prt p) a where
  type PP (Guard prt p) a = a
  eval _ opts a = do
    let msg0 = "Guard"
    pp <- evalBool (Proxy @p) opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right False -> do
        qq <- eval (Proxy @prt) opts a
        pure $ case getValueLR opts (msg0 <> " Msg") qq [hh pp] of
          Left e -> e
          Right ee -> mkNode opts (FailT ee) (msg0 <> " | " <> showL opts a) (hh pp : [hh qq | isVerbose opts])
      Right True -> pure $ mkNode opts (PresentT a) (msg0 <> "(ok) | " <> showL opts a) [hh pp]  -- dont show the guard message if successful

-- | boolean guard
--
-- >>> pl @(GuardBool (PrintF "bad length = %d" Len) (Len > 9)) [3..8]
-- Error bad length = 6 (GuardBool (6 > 9))
-- FailT "bad length = 6"
--
data GuardBool prt p

instance (P prt a
        , PP prt a ~ String
        , P p a
        , PP p a ~ Bool
        ) => P (GuardBool prt p) a where
  type PP (GuardBool prt p) a = Bool
  eval _ opts a = do
    let msg0 = "GuardBool"
    pp <- evalBool (Proxy @p) opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right False -> do
        qq <- eval (Proxy @prt) opts a
        pure $ case getValueLR opts (msg0 <> " Msg") qq [hh pp] of
          Left e -> e
          Right ee -> mkNode opts (FailT ee) (msg0 <> nullSpace (topMessage pp)) [hh pp, hh qq]
      Right True -> pure $ mkNode opts TrueT "" [hh pp]  -- dont show the guard message if successful

-- | uses 'Guard' but negates \'p\'
--
-- >>> pl @(HeadFail "failedn" Id &&& (Len == 1 >> ExitWhen "ExitWhen" Id) >> Fst Id) [3]
-- Error ExitWhen
-- FailT "ExitWhen"
--
-- >>> pl @(Head Id &&& (Len == 1 >> Not Id >> ExitWhen "ExitWhen" Id) >> Fst Id) [3]
-- Present 3 ((>>) 3 | {Fst 3 | (3,False)})
-- PresentT 3
--
-- >>> pl @(Head Id &&& (Len == 1 >> ExitWhen "ExitWhen" (Not Id)) >> Fst Id) [3]
-- Present 3 ((>>) 3 | {Fst 3 | (3,True)})
-- PresentT 3
--
-- >>> pl @(ExitWhen "ExitWhen" (Len /= 1) >> Head Id) [3,1]
-- Error ExitWhen
-- FailT "ExitWhen"
--
-- >>> pl @(ExitWhen "ExitWhen" (Len /= 1) >> Head Id) [3]
-- Present 3 ((>>) 3 | {Head 3 | [3]})
-- PresentT 3
--
-- >>> pl @(ExitWhen "ExitWhen" (Len /= 1) >> Head Id >> Gt (20 -% 1)) [3]
-- True ((>>) True | {3 % 1 > (-20) % 1})
-- TrueT
--
-- >>> pl @(ExitWhen "ExitWhen" (Len /= 1) >> Head Id >> Gt (20 -% 1)) [-23]
-- False ((>>) False | {(-23) % 1 > (-20) % 1})
-- FalseT
--
-- >>> pl @(Map (ExitWhen "ExitWhen" (Gt 10) >> Gt 2) Id) [1..5]
-- Present [False,False,True,True,True] (Map [False,False,True,True,True] | [1,2,3,4,5])
-- PresentT [False,False,True,True,True]
--
-- >>> pl @(ExitWhen "err" (Len > 2) >> Map (Succ Id) Id) [12,15,16]
-- Error err
-- FailT "err"
--
-- >>> pl @(ExitWhen "err" (Len > 2) >> Map (Succ Id) Id) [12]
-- Present [13] ((>>) [13] | {Map [13] | [12]})
-- PresentT [13]
--

data ExitWhen prt p
type ExitWhenT prt p = Guard prt (Not p)

instance P (ExitWhenT prt p) x => P (ExitWhen prt p) x where
  type PP (ExitWhen prt p) x = PP (ExitWhenT prt p) x
  eval _ = eval (Proxy @(ExitWhenT prt p))

-- | similar to 'Guard' but uses the root message of the False predicate case as the failure message
--
-- >>> pz @(GuardSimple (IsLuhn Id)) [1..4]
-- FailT "(IsLuhn map=[4,6,2,2] sum=14 ret=4 | [1,2,3,4])"
--
-- >>> pl @(IsLuhn Id) [1..4]
-- False (IsLuhn map=[4,6,2,2] sum=14 ret=4 | [1,2,3,4])
-- FalseT
--
-- >>> pz @(GuardSimple (IsLuhn Id)) [1,2,3,0]
-- PresentT [1,2,3,0]
--
-- >>> pz @(GuardSimple (Len > 30)) [1,2,3,0]
-- FailT "(4 > 30)"
--
-- >>> pl @(Map (GuardSimple (Lt 3) >> 'True) Id) [1::Int .. 10]
-- Error (3 < 3) | (4 < 3) | (5 < 3) | (6 < 3) | (7 < 3) | (8 < 3) | (9 < 3) | (10 < 3) (Map(i=2, a=3) excnt=8)
-- FailT "(3 < 3) | (4 < 3) | (5 < 3) | (6 < 3) | (7 < 3) | (8 < 3) | (9 < 3) | (10 < 3)"
--
-- >>> pl @(Map (GuardSimple (Ge 1) >> 'True) Id) [1::Int .. 10]
-- Present [True,True,True,True,True,True,True,True,True,True] (Map [True,True,True,True,True,True,True,True,True,True] | [1,2,3,4,5,6,7,8,9,10])
-- PresentT [True,True,True,True,True,True,True,True,True,True]
--
-- >>> pl @(Map (GuardSimple (Lt 3) >> 'True) Id) [1::Int .. 10]
-- Error (3 < 3) | (4 < 3) | (5 < 3) | (6 < 3) | (7 < 3) | (8 < 3) | (9 < 3) | (10 < 3) (Map(i=2, a=3) excnt=8)
-- FailT "(3 < 3) | (4 < 3) | (5 < 3) | (6 < 3) | (7 < 3) | (8 < 3) | (9 < 3) | (10 < 3)"
--
data GuardSimple p

instance (Show a
        , P p a
        , PP p a ~ Bool
        ) => P (GuardSimple p) a where
  type PP (GuardSimple p) a = a
  eval _ opts a = do
    let msg0 = "GuardSimple"
    pp <- evalBool (Proxy @p) (subopts opts) a -- temporarily lift DZero to DLite so as not to lose the failure message
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right False ->
        let msgx = topMessage pp
        in mkNode opts (FailT msgx) (msg0 <> " | " <> showL opts a) [hh pp]
      Right True ->
        mkNode opts (PresentT a) (msg0 <> "(ok) | " <> showL opts a) [hh pp]
