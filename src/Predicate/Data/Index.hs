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
{-# LANGUAGE NoStarIsType #-}
{- |
     promoted indexing functions
-}
module Predicate.Data.Index (
  -- ** indexing expressions
    Ix
  , Ix'
  , IxL
  , type (!!)
  , type (!!?)
  , Lookup
  , LookupDef
  , LookupDef'
  , LookupFail
  , LookupFail'

 ) where
import Predicate.Core
import Predicate.Util
import Predicate.Data.Maybe (JustDef, JustFail)
import Control.Lens hiding (iall)
import GHC.TypeLits (Nat, KnownNat)
import Data.Proxy

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Map.Strict as M
-- >>> import qualified Data.Set as Set
-- >>> import qualified Data.Text as T
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG

-- | index a value in an 'Ixed' container and if not found return the given default value
--
-- >>> pl @(LookupDef' Fst Snd (Char1 "xx") Id) (['a'..'e'],2)
-- Present 'c' (JustDef Just)
-- PresentT 'c'
--
-- >>> pl @(LookupDef' Fst Snd (Char1 "xx") Id) (['a'..'e'],999)
-- Present 'x' (JustDef Nothing)
-- PresentT 'x'
--
-- >>> pl @(LookupDef' Fst Snd (Char1 "xx") Id) ([],2)
-- Present 'x' (JustDef Nothing)
-- PresentT 'x'
--
-- >>> pl @(LookupDef' Fst Snd (Char1 "xx") Snd) ('w',([],2))
-- Present 'x' (JustDef Nothing)
-- PresentT 'x'
--
-- >>> pl @(LookupDef' Fst Snd Fst Snd) ('x',(['a'..'e'],2))
-- Present 'c' (JustDef Just)
-- PresentT 'c'
--
-- >>> pl @(LookupDef' Fst Snd (MEmptyT _) Snd) ('x',(map SG.Min [10..15::Int], 3))
-- Present Min {getMin = 13} (JustDef Just)
-- PresentT (Min {getMin = 13})
--
data LookupDef' v w p q
type LookupDefT' v w p q = JustDef p (q >> Lookup v w)

instance P (LookupDefT' v w p q) x => P (LookupDef' v w p q) x where
  type PP (LookupDef' v w p q) x = PP (LookupDefT' v w p q) x
  eval _ = eval (Proxy @(LookupDefT' v w p q))

-- | index a value in an 'Ixed' container and if not found return the given default value
--
-- >>> pl @(LookupDef '[1,2,3,4,5,6] 4 Id) 23
-- Present 5 (JustDef Just)
-- PresentT 5
--
-- >>> pl @(LookupDef '[1,2,3,4,5,6] 4 Fst) (23,'x')
-- Present 5 (JustDef Just)
-- PresentT 5
--
-- >>> pl @(LookupDef '[1,2,3,4,5,6] 99 Id) 23
-- Present 23 (JustDef Nothing)
-- PresentT 23
--
-- >>> pl @(LookupDef '[1,2,3,4,5,6] 99 Fst) (23,'x')
-- Present 23 (JustDef Nothing)
-- PresentT 23
--
-- >>> pl @(LookupDef '[1,2,3,4,5,6] 4 999) (23,'x')
-- Present 5 (JustDef Just)
-- PresentT 5
--
-- >>> pl @(LookupDef '[1,2,3,4,5,6] 40 999) (23,'x')
-- Present 999 (JustDef Nothing)
-- PresentT 999
--
-- >>> pl @(LookupDef Fst 4 (MEmptyT _)) (map SG.Min [1::Int .. 10],'x')
-- Present Min {getMin = 5} (JustDef Just)
-- PresentT (Min {getMin = 5})
--
-- >>> pl @(LookupDef Fst 999 (MEmptyT _)) (map SG.Min [1::Int .. 10],'x')
-- Present Min {getMin = 9223372036854775807} (JustDef Nothing)
-- PresentT (Min {getMin = 9223372036854775807})
--
data LookupDef v w p
type LookupDefT v w p = LookupDef' v w p I

instance P (LookupDefT v w p) x => P (LookupDef v w p) x where
  type PP (LookupDef v w p) x = PP (LookupDefT v w p) x
  eval _ = eval (Proxy @(LookupDefT v w p))

-- | index a value in an 'Ixed' container and if not found fail with the given message
data LookupFail' msg v w q
type LookupFailT' msg v w q = JustFail msg (q >> Lookup v w)

instance P (LookupFailT' msg v w q) x => P (LookupFail' msg v w q) x where
  type PP (LookupFail' msg v w q) x = PP (LookupFailT' msg v w q) x
  eval _ = eval (Proxy @(LookupFailT' msg v w q))

-- | index a value in an 'Ixed' container and if not found fail with the given message
--
-- >>> pl @(LookupFail "someval" Fst 999) (map SG.Min [1::Int .. 10],'x')
-- Error someval (JustFail Nothing)
-- FailT "someval"
--
-- >>> pl @(LookupFail (PrintF "char=%c" Snd) Fst 49) (map SG.Min [1::Int ..10],'x')
-- Error char=x (JustFail Nothing)
-- FailT "char=x"
--
data LookupFail msg v w
type LookupFailT msg v w = LookupFail' msg v w I

instance P (LookupFailT msg v w) x => P (LookupFail msg v w) x where
  type PP (LookupFail msg v w) x = PP (LookupFailT msg v w) x
  eval _ = eval (Proxy @(LookupFailT msg v w))

-- | similar to 'Data.List.!!' using an 'Ixed' container
--
-- >>> pz @(Ix 4 "not found") ["abc","D","eF","","G"]
-- PresentT "G"
--
-- >>> pz @(Ix 40 "not found") ["abc","D","eF","","G"]
-- PresentT "not found"
--
-- >>> pl @(Fst >> Dup >> (Ix 1 (Failp "failed5") *** Ix 3 (Failp "failed5")) >> Id) ([10,12,3,5],"ss")
-- Present (12,5) ((>>) (12,5) | {Id (12,5)})
-- PresentT (12,5)
--
-- >>> pl @(Fst >> Dup >> (Ix 1 (Failp "failed5") *** Ix 3 (Failp "failed5")) >> Fst < Snd) ([10,12,3,5],"ss")
-- False ((>>) False | {12 < 5})
-- FalseT
--
-- >>> pl @(Fst >> Dup >> (Ix 1 (Failp "failed5") *** Ix 3 (Failp "failed5")) >> Fst > Snd) ([10,12,3,5],"ss")
-- True ((>>) True | {12 > 5})
-- TrueT
--
-- >>> pl @(Snd >> Len &&& Ix 3 (Failp "someval1") >> Fst == Snd) ('x',[1..5])
-- False ((>>) False | {5 == 4})
-- FalseT
--
-- >>> pl @(Snd >> Len &&& Ix 3 (Failp "someval2") >> Fst < Snd) ('x',[1..5])
-- False ((>>) False | {5 < 4})
-- FalseT
--
-- >>> pl @(Snd >> Len &&& Ix 3 (Failp "someval3") >> Fst > Snd) ('x',[1..5])
-- True ((>>) True | {5 > 4})
-- TrueT
--
-- >>> pl @(Map Len Id >> Ix 3 (Failp "lhs") &&& Ix 0 5 >> Fst == Snd) [[1..4],[4..5]]
-- Error lhs ([4,2])
-- FailT "lhs"
--
-- >>> pl @(Map Len Id >> Ix 0 (Failp "lhs") &&& Ix 1 5 >> Fst == Snd) [[1..4],[4..5]]
-- False ((>>) False | {4 == 2})
-- FalseT
--
-- >>> pl @(Map Len Id >> Ix 1 (Failp "lhs") &&& Ix 3 (Failp "rhs") >> Fst == Snd) [[1..4],[4..5]]
-- Error rhs ([4,2])
-- FailT "rhs"
--
-- >>> pl @(Map Len Id >> Ix 10 (Failp "lhs") &&& Ix 1 (Failp "rhs") >> Fst == Snd) [[1..4],[4..5]]
-- Error lhs ([4,2])
-- FailT "lhs"
--
-- >>> pl @(Map Len Id >> Ix 0 (Failp "lhs") &&& Ix 10 (Failp "rhs") >> Fst == Snd) [[1..4],[4..5]]
-- Error rhs ([4,2])
-- FailT "rhs"
--
-- >>> pl @(Map Len Id >> Ix 10 3 &&& Ix 1 (Failp "rhs") >> Fst == Snd) [[1..4],[4..5]]
-- False ((>>) False | {3 == 2})
-- FalseT
--
-- >>> pl @(Map Len Id >> Ix 3 3 &&& Ix 1 4 >> Fst == Snd) [[1..4],[4..5]]
-- False ((>>) False | {3 == 2})
-- FalseT
--
-- >>> pl @(Map Len Id >> Ix 10 3 &&& Ix 1 4 >> Fst == Snd) [[1..4],[4..5]]
-- False ((>>) False | {3 == 2})
-- FalseT
--
-- >>> pl @(Map Len Id >> Ix 10 5 &&& Ix 1 4 >> Fst == Snd) [[1..4],[4..5]]
-- False ((>>) False | {5 == 2})
-- FalseT
--
-- >>> pl @(Map Len Id >> Ix 10 2 &&& Ix 1 4 >> Fst == Snd) [[1..4],[4..5]]
-- True ((>>) True | {2 == 2})
-- TrueT
--
data Ix (n :: Nat) def

instance (P def (Proxy a)
        , PP def (Proxy a) ~ a
        , KnownNat n
        , Show a
        ) => P (Ix n def) [a] where
  type PP (Ix n def) [a] = a
  eval _ opts as = do
    let n = nat @n
        msg0 = "Ix(" <> show n <> ")"
    case as ^? ix n of
         Nothing -> do
           let msg1 = msg0 <> " not found"
           pp <- eval (Proxy @def) opts (Proxy @a)
           pure $ case getValueLR opts msg1 pp [] of
             Left e -> e
             Right _ -> mkNode opts (_ttBool pp) msg1 [hh pp]
         Just a -> pure $ mkNode opts (PresentT a) (msg0 <> " " <> showL opts a) []

data Ix' (n :: Nat)
type IxT' (n :: Nat) = Ix n (Failp "Ix index not found")

instance P (IxT' n) x => P (Ix' n) x where
  type PP (Ix' n) x = PP (IxT' n) x
  eval _ = eval (Proxy @(IxT' n))

-- | similar to 'Data.List.!!' leveraging 'Ixed': see '!!'
--
-- >>> pz @(IxL Id 2 "notfound") ["abc","D","eF","","G"]
-- PresentT "eF"
--
-- >>> pz @(IxL Id 20 "notfound") ["abc","D","eF","","G"]
-- PresentT "notfound"
--
-- >>> pl @(IxL Id 1 (Char1 "x")) ("123" :: T.Text)
-- Present '2' (IxL(1) '2' | p="123" | q=1)
-- PresentT '2'
--
-- >>> pl @(IxL Id 15 (Char1 "x")) ("123" :: T.Text)
-- Present 'x' (IxL(15) index not found)
-- PresentT 'x'
--

data IxL p q def -- p is the big value and q is the index and def is the default

instance (P q a
        , P p a
        , Show (PP p a)
        , Ixed (PP p a)
        , PP q a ~ Index (PP p a)
        , Show (Index (PP p a))
        , Show (IxValue (PP p a))
        , P r (Proxy (IxValue (PP p a)))
        , PP r (Proxy (IxValue (PP p a))) ~ IxValue (PP p a)
        )
   => P (IxL p q r) a where
  type PP (IxL p q r) a = IxValue (PP p a)
  eval _ opts a = do
    let msg0 = "IxL"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> "(" <> showL opts q <> ")"
        in case p ^? ix q of
             Nothing -> do
                rr <- eval (Proxy @r) opts (Proxy @(IxValue (PP p a)))
                pure $ case getValueLR opts msg1 rr [hh pp, hh qq] of
                  Left e -> e
                  Right _ -> mkNode opts (_ttBool rr) (msg1 <> " index not found") [hh pp, hh qq]
             Just ret -> pure $ mkNode opts (PresentT ret) (show01' opts msg1 ret "p=" p <> showVerbose opts " | q=" q) [hh pp, hh qq]

-- | similar to 'Data.List.!!' leveraging 'Ixed'
--
-- >>> pz @(Id !! 2) ["abc","D","eF","","G"]
-- PresentT "eF"
--
-- >>> pz @(Id !! 20) ["abc","D","eF","","G"]
-- FailT "(!!) index not found"
--
-- >>> pz @(Id !! "eF") (M.fromList (flip zip [0..] ["abc","D","eF","","G"]))
-- PresentT 2
--
-- >>> pl @(Id !! 3) ("asfd" :: T.Text)
-- Present 'd' (IxL(3) 'd' | p="asfd" | q=3)
-- PresentT 'd'
--
-- >>> pl @(Id !! 4) ("asfd" :: T.Text)
-- Error (!!) index not found (IxL(4))
-- FailT "(!!) index not found"
--
-- >>> pl @(Id !! MEmptyT _) (Just "a")
-- Present "a" (IxL(()) "a" | p=Just "a" | q=())
-- PresentT "a"
--
-- >>> pl @(Id !! MEmptyT _) (Nothing @()) -- had to add @() to keep this happy: ghci is fine
-- Error (!!) index not found (IxL(()))
-- FailT "(!!) index not found"
--
-- >>> pl @(Id !! 0) ('a','b','c')
-- Present 'a' (IxL(0) 'a' | p=('a','b','c') | q=0)
-- PresentT 'a'
--
-- >>> pl @(Id !! Failt _ "err") ('a','b','c')
-- Error err (IxL)
-- FailT "err"
--
-- >>> pl @(Id !! "d") (M.fromList $ zip (map (:[]) "abcd") [0 ..])
-- Present 3 (IxL("d") 3 | p=fromList [("a",0),("b",1),("c",2),("d",3)] | q="d")
-- PresentT 3
--
-- >>> pl @(Id !! ("d" >> Head)) (M.fromList $ zip "abcd" [0 ..]) -- had to String (instead of _) to keep this happy: ghci is fine
-- Present 3 (IxL('d') 3 | p=fromList [('a',0),('b',1),('c',2),('d',3)] | q='d')
-- PresentT 3
--
-- >>> pl @(Id !! ("d" >> Head)) (Set.fromList "abcd") -- had to String (instead of _) to keep this happy: ghci is fine
-- Present () (IxL('d') () | p=fromList "abcd" | q='d')
-- PresentT ()
--
-- >>> pl @(Id !! HeadFail "failedn" "e") (Set.fromList "abcd") -- had to String (instead of _) to keep this happy: ghci is fine
-- Error (!!) index not found (IxL('e'))
-- FailT "(!!) index not found"
--
-- >>> pl @(Id !! ("d" >> Head)) (M.fromList $ zip "abcd" [0 ..])   -- use Char1 "d" instead of "d" >> Head
-- Present 3 (IxL('d') 3 | p=fromList [('a',0),('b',1),('c',2),('d',3)] | q='d')
-- PresentT 3
--
-- >>> pl @(Id !! MEmptyT _) (Just 10)
-- Present 10 (IxL(()) 10 | p=Just 10 | q=())
-- PresentT 10
--
-- >>> pl @(Id !! MEmptyT _) (Nothing @())
-- Error (!!) index not found (IxL(()))
-- FailT "(!!) index not found"
--
-- >>> pl @(Id !! 6) ['a'..'z']
-- Present 'g' (IxL(6) 'g' | p="abcdefghijklmnopqrstuvwxyz" | q=6)
-- PresentT 'g'
--
-- >>> pl @(Snd !! Fst) (3,"abcde" :: String)
-- Present 'd' (IxL(3) 'd' | p="abcde" | q=3)
-- PresentT 'd'
--
-- >>> pl @(Snd !! Fst) (4,[9,8])
-- Error (!!) index not found (IxL(4))
-- FailT "(!!) index not found"
--
-- >>> pl @(2 &&& Id >> Snd !! Fst) ("abcdef" :: String)
-- Present 'c' ((>>) 'c' | {IxL(2) 'c' | p="abcdef" | q=2})
-- PresentT 'c'
--
-- >>> pl @((Len >> Pred) &&& Id >> Snd !! Fst) "abcdef"
-- Present 'f' ((>>) 'f' | {IxL(5) 'f' | p="abcdef" | q=5})
-- PresentT 'f'
--
-- >>> pl @(Id !! 3) ('a','b','c','d','e')
-- Present 'd' (IxL(3) 'd' | p=('a','b','c','d','e') | q=3)
-- PresentT 'd'
--
-- >>> pl @(Id !! "s") $ M.fromList [("t",1), ("s", 20), ("s", 99)]
-- Present 99 (IxL("s") 99 | p=fromList [("s",99),("t",1)] | q="s")
-- PresentT 99
--
-- >>> pl @(Id !! Char1 "d") (M.fromList $ zip "abcd" [0 ..])
-- Present 3 (IxL('d') 3 | p=fromList [('a',0),('b',1),('c',2),('d',3)] | q='d')
-- PresentT 3
--
-- >>> pl @(Id !! FromString _ "d" &&& (Map (Snd >> Gt 3 >> Coerce SG.Any) (IToList _ Id) >> MConcat Id)) (M.fromList $ zip (map T.singleton "abcdefgh") [0 ..])
-- Present (3,Any {getAny = True}) (W '(3,Any {getAny = True}))
-- PresentT (3,Any {getAny = True})
--
-- >>> pl @(Id !! FromString _ "d" &&& (Map (Snd >> Gt 3 >> Wrap SG.Any Id) (IToList _ Id) >> MConcat Id >> Unwrap Id)) (M.fromList $ zip (map T.singleton "abcdefgh") [0 ..])
-- Present (3,True) (W '(3,True))
-- PresentT (3,True)
--
-- >>> pl @(Id !! FromString _ "d") (M.fromList $ zip (map T.singleton "abcd") [0 ..])
-- Present 3 (IxL("d") 3 | p=fromList [("a",0),("b",1),("c",2),("d",3)] | q="d")
-- PresentT 3
--
-- >>> pl @(Id !! FromString _ "d") (M.fromList $ zip (map T.singleton "abcd") [0 ..])
-- Present 3 (IxL("d") 3 | p=fromList [("a",0),("b",1),("c",2),("d",3)] | q="d")
-- PresentT 3
--
-- >>> pl @(Id !! 2 !! 0) [[1..5],[10..14],[100..110]]
-- Present 100 (IxL(0) 100 | p=[100,101,102,103,104,105,106,107,108,109,110] | q=0)
-- PresentT 100
--
-- >>> pl @(Id !! 1 !! 7) [[1..5],[10..14],[100..110]]
-- Error (!!) index not found (IxL(7))
-- FailT "(!!) index not found"
--
-- >>> pl @(Id !! 1) [('x',14),('y',3),('z',5)]
-- Present ('y',3) (IxL(1) ('y',3) | p=[('x',14),('y',3),('z',5)] | q=1)
-- PresentT ('y',3)
--
-- >>> pl @(Id !! 14) [('x',14),('y',3),('z',5)]
-- Error (!!) index not found (IxL(14))
-- FailT "(!!) index not found"
--

data p !! q
type BangBangT p q = IxL p q (Failp "(!!) index not found")

instance P (BangBangT p q) a => P (p !! q) a where
  type PP (p !! q) a = PP (BangBangT p q) a
  eval _ = eval (Proxy @(BangBangT p q))

-- | 'lookup' leveraging 'Ixed': see '!!?'
--
-- >>> pz @(Lookup Id 2) ["abc","D","eF","","G"]
-- PresentT (Just "eF")
--
-- >>> pz @(Lookup Id 20) ["abc","D","eF","","G"]
-- PresentT Nothing
--
-- >>> pl @(FromList (M.Map _ _) >> Lookup Id (Char1 "y")) [('x',True),('y',False)]
-- Present Just False ((>>) Just False | {Lookup('y') False | p=fromList [('x',True),('y',False)] | q='y'})
-- PresentT (Just False)
--
-- >>> pl @(FromList (M.Map _ _) >> Lookup Id (Char1 "z")) [('x',True),('y',False)]
-- Present Nothing ((>>) Nothing | {Lookup('z') not found})
-- PresentT Nothing
--
-- >>> pl @(FromList (M.Map _ _) >> Lookup Id %% Char1 "y") [('x',True),('y',False)]
-- Present Just False ((>>) Just False | {Lookup('y') False | p=fromList [('x',True),('y',False)] | q='y'})
-- PresentT (Just False)
--
-- >>> pl @(Lookup Id 1) [('x',14),('y',3),('z',5)]
-- Present Just ('y',3) (Lookup(1) ('y',3) | p=[('x',14),('y',3),('z',5)] | q=1)
-- PresentT (Just ('y',3))
--
-- >>> pl @(Lookup Id 14) [('x',14),('y',3),('z',5)]
-- Present Nothing (Lookup(14) not found)
-- PresentT Nothing
--
-- >>> pl @(Lookup "abcdef" 3) ()
-- Present Just 'd' (Lookup(3) 'd' | p="abcdef" | q=3)
-- PresentT (Just 'd')
--
-- >>> pl @(Lookup '[1,2,3,4,5,6] 4) ()
-- Present Just 5 (Lookup(4) 5 | p=[1,2,3,4,5,6] | q=4)
-- PresentT (Just 5)
--
-- >>> pl @(FromList (M.Map _ _)) [(4,"x"),(5,"dd")]
-- Present fromList [(4,"x"),(5,"dd")] (FromList fromList [(4,"x"),(5,"dd")])
-- PresentT (fromList [(4,"x"),(5,"dd")])
--
data Lookup p q

instance (P q a
        , P p a
        , Show (PP p a)
        , Ixed (PP p a)
        , PP q a ~ Index (PP p a)
        , Show (Index (PP p a))
        , Show (IxValue (PP p a))
        )
   => P (Lookup p q) a where
  type PP (Lookup p q) a = Maybe (IxValue (PP p a))
  eval _ opts a = do
    let msg0 = "Lookup"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> "(" <> showL opts q <> ")"
            hhs = [hh pp, hh qq]
        in case p ^? ix q of
             Nothing -> mkNode opts (PresentT Nothing) (msg1 <> " not found") hhs
             Just ret -> mkNode opts (PresentT (Just ret)) (show01' opts msg1 ret "p=" p <> showVerbose opts " | q=" q) hhs

-- | type operator version of 'Lookup'
--
-- >>> pl @((Id !!? Char1 "d") > MkJust 99 || Length Id <= 3) (M.fromList $ zip "abcd" [1..])
-- False (False || False | (Just 4 > Just 99) || (4 <= 3))
-- FalseT
--
-- >>> pz @((Id !!? Char1 "d") > MkJust 2 || Length Id <= 3) (M.fromList $ zip "abcd" [1..])
-- TrueT
--
data p !!? q
type BangBangQT p q = Lookup p q

instance P (BangBangQT p q) a => P (p !!? q) a where
  type PP (p !!? q) a = PP (BangBangQT p q) a
  eval _ = eval (Proxy @(BangBangQT p q))
