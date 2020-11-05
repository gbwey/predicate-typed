{-# LANGUAGE TypeOperators #-}
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
-- | promoted indexing functions
module Predicate.Data.Index (
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
import Predicate.Misc
import Predicate.Util
import Predicate.Data.Maybe (JustDef, JustFail)
import Control.Lens
import GHC.TypeLits (Nat, KnownNat)
import Data.Proxy (Proxy(..))

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
-- >>> pl @(LookupDef' Snd Fst (C "xx") Id) (['a'..'e'],2)
-- Present 'c' (JustDef Just)
-- Val 'c'
--
-- >>> pl @(LookupDef' Snd Fst (C "xx") Id) (['a'..'e'],999)
-- Present 'x' (JustDef Nothing)
-- Val 'x'
--
-- >>> pl @(LookupDef' Snd Fst (C "xx") Id) ([],2)
-- Present 'x' (JustDef Nothing)
-- Val 'x'
--
-- >>> pl @(LookupDef' Snd Fst (C "xx") Snd) ('w',([],2))
-- Present 'x' (JustDef Nothing)
-- Val 'x'
--
-- >>> pl @(LookupDef' Snd Fst Fst Snd) ('x',(['a'..'e'],2))
-- Present 'c' (JustDef Just)
-- Val 'c'
--
-- >>> pl @(LookupDef' Snd Fst (MEmptyT _) Snd) ('x',(map SG.Min [10..15::Int], 3))
-- Present Min {getMin = 13} (JustDef Just)
-- Val (Min {getMin = 13})
--
data LookupDef' v w p q deriving Show
type LookupDefT' v w p q = JustDef p (q >> Lookup v w)

instance P (LookupDefT' v w p q) x => P (LookupDef' v w p q) x where
  type PP (LookupDef' v w p q) x = PP (LookupDefT' v w p q) x
  eval _ = eval (Proxy @(LookupDefT' v w p q))

-- | index a value in an 'Ixed' container and if not found return the given default value
--
-- >>> pl @(LookupDef 4 '[1,2,3,4,5,6] Id) 23
-- Present 5 (JustDef Just)
-- Val 5
--
-- >>> pl @(LookupDef 4 '[1,2,3,4,5,6] Fst) (23,'x')
-- Present 5 (JustDef Just)
-- Val 5
--
-- >>> pl @(LookupDef 99 '[1,2,3,4,5,6] Id) 23
-- Present 23 (JustDef Nothing)
-- Val 23
--
-- >>> pl @(LookupDef 4 Fst (MEmptyT _)) (map SG.Min [1::Int .. 10],'x')
-- Present Min {getMin = 5} (JustDef Just)
-- Val (Min {getMin = 5})
--
-- >>> pl @(LookupDef 999 Fst (MEmptyT _)) (map SG.Min [1::Int .. 10],'x')
-- Present Min {getMin = 9223372036854775807} (JustDef Nothing)
-- Val (Min {getMin = 9223372036854775807})
--
data LookupDef v w p deriving Show
type LookupDefT v w p = LookupDef' v w p Id

instance P (LookupDefT v w p) x => P (LookupDef v w p) x where
  type PP (LookupDef v w p) x = PP (LookupDefT v w p) x
  eval _ = eval (Proxy @(LookupDefT v w p))

-- | index a value in an 'Ixed' container and if not found fail with the given message
data LookupFail' msg v w q deriving Show
type LookupFailT' msg v w q = JustFail msg (q >> Lookup v w)

instance P (LookupFailT' msg v w q) x => P (LookupFail' msg v w q) x where
  type PP (LookupFail' msg v w q) x = PP (LookupFailT' msg v w q) x
  eval _ = eval (Proxy @(LookupFailT' msg v w q))

-- | index a value in an 'Ixed' container and if not found fail with the given message
--
-- >>> pl @(LookupFail "someval" 999 Fst) (map SG.Min [1::Int .. 10],'x')
-- Error someval (JustFail Nothing)
-- Fail "someval"
--
-- >>> pl @(LookupFail (PrintF "char=%c" Snd) 49 Fst) (map SG.Min [1::Int ..10],'x')
-- Error char=x (JustFail Nothing)
-- Fail "char=x"
--
data LookupFail msg v w deriving Show
type LookupFailT msg v w = LookupFail' msg v w Id

instance P (LookupFailT msg v w) x => P (LookupFail msg v w) x where
  type PP (LookupFail msg v w) x = PP (LookupFailT msg v w) x
  eval _ = eval (Proxy @(LookupFailT msg v w))

-- | similar to 'Data.List.!!' for lists
--
-- >>> pz @(Ix 4 "not found") ["abc","D","eF","","G"]
-- Val "G"
--
-- >>> pz @(Ix 40 "not found") ["abc","D","eF","","G"]
-- Val "not found"
--
-- >>> pl @(Fst >> Dup >> (Ix 1 (FailP "failed5") *** Ix 3 (FailP "failed5")) >> Id) ([10,12,3,5],"ss")
-- Present (12,5) ((>>) (12,5) | {Id (12,5)})
-- Val (12,5)
--
-- >>> pl @(Fst >> Dup >> (Ix 1 (FailP "failed5") *** Ix 3 (FailP "failed5")) >> Fst < Snd) ([10,12,3,5],"ss")
-- False ((>>) False | {12 < 5})
-- Val False
--
-- >>> pl @(Fst >> Dup >> (Ix 1 (FailP "failed5") *** Ix 3 (FailP "failed5")) >> Fst > Snd) ([10,12,3,5],"ss")
-- True ((>>) True | {12 > 5})
-- Val True
--
-- >>> pl @(Snd >> Len &&& Ix 3 (FailP "someval1") >> Fst == Snd) ('x',[1..5])
-- False ((>>) False | {5 == 4})
-- Val False
--
-- >>> pl @(Snd >> Len &&& Ix 3 (FailP "someval2") >> Fst < Snd) ('x',[1..5])
-- False ((>>) False | {5 < 4})
-- Val False
--
-- >>> pl @(Snd >> Len &&& Ix 3 (FailP "someval3") >> Fst > Snd) ('x',[1..5])
-- True ((>>) True | {5 > 4})
-- Val True
--
-- >>> pl @(Map Len >> Ix 3 (FailP "lhs") &&& Ix 0 5 >> Fst == Snd) [[1..4],[4..5]]
-- Error lhs (Ix(3) not found | '(,))
-- Fail "lhs"
--
-- >>> pl @(Map Len >> Ix 0 (FailP "lhs") &&& Ix 1 5 >> Fst == Snd) [[1..4],[4..5]]
-- False ((>>) False | {4 == 2})
-- Val False
--
-- >>> pl @(Map Len >> Ix 1 (FailP "lhs") &&& Ix 3 (FailP "rhs") >> Fst == Snd) [[1..4],[4..5]]
-- Error rhs (Ix(3) not found | '(,))
-- Fail "rhs"
--
-- >>> pl @(Map Len >> Ix 10 (FailP "lhs") &&& Ix 1 (FailP "rhs") >> Fst == Snd) [[1..4],[4..5]]
-- Error lhs (Ix(10) not found | '(,))
-- Fail "lhs"
--
-- >>> pl @(Map Len >> Ix 0 (FailP "lhs") &&& Ix 10 (FailP "rhs") >> Fst == Snd) [[1..4],[4..5]]
-- Error rhs (Ix(10) not found | '(,))
-- Fail "rhs"
--
-- >>> pl @(Map Len >> Ix 10 3 &&& Ix 1 (FailP "rhs") >> Fst == Snd) [[1..4],[4..5]]
-- False ((>>) False | {3 == 2})
-- Val False
--
-- >>> pl @(Map Len >> Ix 3 3 &&& Ix 1 4 >> Fst == Snd) [[1..4],[4..5]]
-- False ((>>) False | {3 == 2})
-- Val False
--
-- >>> pl @(Map Len >> Ix 10 3 &&& Ix 1 4 >> Fst == Snd) [[1..4],[4..5]]
-- False ((>>) False | {3 == 2})
-- Val False
--
-- >>> pl @(Map Len >> Ix 10 5 &&& Ix 1 4 >> Fst == Snd) [[1..4],[4..5]]
-- False ((>>) False | {5 == 2})
-- Val False
--
-- >>> pl @(Map Len >> Ix 10 2 &&& Ix 1 4 >> Fst == Snd) [[1..4],[4..5]]
-- True ((>>) True | {2 == 2})
-- Val True
--
data Ix (n :: Nat) def deriving Show

instance ( P def (Proxy a)
         , PP def (Proxy a) ~ a
         , KnownNat n
         , Show a
         , [a] ~ x
         ) => P (Ix n def) x where
  type PP (Ix n def) x = ExtractAFromTA x
  eval _ opts as = do
    let n = nat @n
        msg0 = "Ix(" <> show n <> ")"
    case as ^? ix n of
         Nothing -> do
           let msg1 = msg0 <> " not found"
           pp <- eval (Proxy @def) opts (Proxy @a)
           pure $ case getValueLR Inline opts msg1 pp [] of
             Left e -> e
             Right _ -> mkNodeCopy opts pp msg1 [hh pp]
         Just a -> pure $ mkNode opts (Val a) (msg0 <> " " <> showL opts a) []

-- | similar to 'Data.List.!!' for lists with a default error message on failure
data Ix' (n :: Nat) deriving Show
type IxT' (n :: Nat) = Ix n (FailP "Ix index not found")

instance P (IxT' n) x => P (Ix' n) x where
  type PP (Ix' n) x = PP (IxT' n) x
  eval _ = eval (Proxy @(IxT' n))

-- | similar to 'Data.List.!!' leveraging 'Ixed'
--
-- >>> pz @(IxL Id 2 "notfound") ["abc","D","eF","","G"]
-- Val "eF"
--
-- >>> pz @(IxL Id 20 "notfound") ["abc","D","eF","","G"]
-- Val "notfound"
--
-- >>> pl @(IxL Id 1 (C "x")) ("123" :: T.Text)
-- Present '2' (IxL(1) '2' | p="123" | q=1)
-- Val '2'
--
-- >>> pl @(IxL Id 15 (C "x")) ("123" :: T.Text)
-- Present 'x' (IxL(15) index not found)
-- Val 'x'
--
data IxL p q def deriving Show
-- p is the big value and q is the index and def is the default

instance ( P q a
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
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> "(" <> showL opts q <> ")"
        in case p ^? ix q of
             Nothing -> do
                rr <- eval (Proxy @r) opts (Proxy @(IxValue (PP p a)))
                pure $ case getValueLR Inline opts msg1 rr [hh pp, hh qq] of
                  Left e -> e
                  Right _ -> mkNodeCopy opts rr (msg1 <> " index not found") [hh pp, hh qq]
             Just ret -> pure $ mkNode opts (Val ret) (show3' opts msg1 ret "p=" p <> showVerbose opts " | q=" q) [hh pp, hh qq]

-- | similar to 'Data.List.!!' leveraging 'Ixed'
--
-- >>> pz @(Id !! 2) ["abc","D","eF","","G"]
-- Val "eF"
--
-- >>> pz @(Id !! 20) ["abc","D","eF","","G"]
-- Fail "(!!) index not found"
--
-- >>> pz @(Id !! "eF") (M.fromList (flip zip [0..] ["abc","D","eF","","G"]))
-- Val 2
--
-- >>> pl @(Id !! 3) ("asfd" :: T.Text)
-- Present 'd' (IxL(3) 'd' | p="asfd" | q=3)
-- Val 'd'
--
-- >>> pl @(Id !! 4) ("asfd" :: T.Text)
-- Error (!!) index not found (IxL(4))
-- Fail "(!!) index not found"
--
-- >>> pl @(Id !! MEmptyT _) (Just "a")
-- Present "a" (IxL(()) "a" | p=Just "a" | q=())
-- Val "a"
--
-- >>> pl @(Id !! MEmptyT _) (Nothing @()) -- had to add @() to keep this happy: ghci is fine
-- Error (!!) index not found (IxL(()))
-- Fail "(!!) index not found"
--
-- >>> pl @(Id !! 0) ('a','b','c')
-- Present 'a' (IxL(0) 'a' | p=('a','b','c') | q=0)
-- Val 'a'
--
-- >>> pl @(Id !! FailT _ "err") ('a','b','c')
-- Error err (IxL)
-- Fail "err"
--
-- >>> pl @(Id !! "d") (M.fromList $ zip (map (:[]) "abcd") [0 ..])
-- Present 3 (IxL("d") 3 | p=fromList [("a",0),("b",1),("c",2),("d",3)] | q="d")
-- Val 3
--
-- >>> pl @(Id !! C "d") (M.fromList $ zip "abcd" [0 ..])
-- Present 3 (IxL('d') 3 | p=fromList [('a',0),('b',1),('c',2),('d',3)] | q='d')
-- Val 3
--
-- >>> pl @(Id !! C "d") (Set.fromList "abcd")
-- Present () (IxL('d') () | p=fromList "abcd" | q='d')
-- Val ()
--
-- >>> pl @(Id !! HeadFail "failedn" "e") (Set.fromList "abcd")
-- Error (!!) index not found (IxL('e'))
-- Fail "(!!) index not found"
--
-- >>> pl @(Id !! C "d") (M.fromList $ zip "abcd" [0 ..])
-- Present 3 (IxL('d') 3 | p=fromList [('a',0),('b',1),('c',2),('d',3)] | q='d')
-- Val 3
--
-- >>> pl @(Id !! MEmptyT _) (Just 10)
-- Present 10 (IxL(()) 10 | p=Just 10 | q=())
-- Val 10
--
-- >>> pl @(Id !! MEmptyT _) (Nothing @())
-- Error (!!) index not found (IxL(()))
-- Fail "(!!) index not found"
--
-- >>> pl @(Id !! 6) ['a'..'z']
-- Present 'g' (IxL(6) 'g' | p="abcdefghijklmnopqrstuvwxyz" | q=6)
-- Val 'g'
--
-- >>> pl @(Snd !! Fst) (3,"abcde")
-- Present 'd' (IxL(3) 'd' | p="abcde" | q=3)
-- Val 'd'
--
-- >>> pl @(Snd !! Fst) (4,[9,8])
-- Error (!!) index not found (IxL(4))
-- Fail "(!!) index not found"
--
-- >>> pl @(2 &&& Id >> Snd !! Fst) "abcdef"
-- Present 'c' ((>>) 'c' | {IxL(2) 'c' | p="abcdef" | q=2})
-- Val 'c'
--
-- >>> pl @((Len >> Pred) &&& Id >> Snd !! Fst) "abcdef"
-- Present 'f' ((>>) 'f' | {IxL(5) 'f' | p="abcdef" | q=5})
-- Val 'f'
--
-- >>> pl @(Id !! 3) ('a','b','c','d','e')
-- Present 'd' (IxL(3) 'd' | p=('a','b','c','d','e') | q=3)
-- Val 'd'
--
-- >>> pl @(Id !! "s") $ M.fromList [("t",1), ("s", 20), ("s", 99)]
-- Present 99 (IxL("s") 99 | p=fromList [("s",99),("t",1)] | q="s")
-- Val 99
--
-- >>> pl @(Id !! C "d") (M.fromList $ zip "abcd" [0 ..])
-- Present 3 (IxL('d') 3 | p=fromList [('a',0),('b',1),('c',2),('d',3)] | q='d')
-- Val 3
--
-- >>> pl @(Id !! FromString _ "d" &&& (Map' (Snd >> Gt 3 >> Coerce SG.Any) (IToList _) >> MConcat)) (M.fromList $ zip (map T.singleton "abcdefgh") [0 ..])
-- Present (3,Any {getAny = True}) ('(3,Any {getAny = True}))
-- Val (3,Any {getAny = True})
--
-- >>> pl @(Id !! FromString _ "d" &&& (Map' (Snd >> Gt 3 >> Wrap SG.Any Id) (IToList _) >> MConcat >> Unwrap)) (M.fromList $ zip (map T.singleton "abcdefgh") [0 ..])
-- Present (3,True) ('(3,True))
-- Val (3,True)
--
-- >>> pl @(Id !! FromString _ "d") (M.fromList $ zip (map T.singleton "abcd") [0 ..])
-- Present 3 (IxL("d") 3 | p=fromList [("a",0),("b",1),("c",2),("d",3)] | q="d")
-- Val 3
--
-- >>> pl @(Id !! FromString _ "d") (M.fromList $ zip (map T.singleton "abcd") [0 ..])
-- Present 3 (IxL("d") 3 | p=fromList [("a",0),("b",1),("c",2),("d",3)] | q="d")
-- Val 3
--
-- >>> pl @(Id !! 2 !! 0) [[1..5],[10..14],[100..110]]
-- Present 100 (IxL(0) 100 | p=[100,101,102,103,104,105,106,107,108,109,110] | q=0)
-- Val 100
--
-- >>> pl @(Id !! 1 !! 7) [[1..5],[10..14],[100..110]]
-- Error (!!) index not found (IxL(7))
-- Fail "(!!) index not found"
--
-- >>> pl @(Id !! 1) [('x',14),('y',3),('z',5)]
-- Present ('y',3) (IxL(1) ('y',3) | p=[('x',14),('y',3),('z',5)] | q=1)
-- Val ('y',3)
--
-- >>> pl @(Id !! 14) [('x',14),('y',3),('z',5)]
-- Error (!!) index not found (IxL(14))
-- Fail "(!!) index not found"
--
data p !! q deriving Show
type BangBangT p q = IxL p q (FailP "(!!) index not found")

instance P (BangBangT p q) a => P (p !! q) a where
  type PP (p !! q) a = PP (BangBangT p q) a
  eval _ = eval (Proxy @(BangBangT p q))

-- | 'lookup' leveraging 'Ixed': see '!!?'
--
-- >>> pz @(Lookup 2 Id) ["abc","D","eF","","G"]
-- Val (Just "eF")
--
-- >>> pz @(Lookup 20 Id) ["abc","D","eF","","G"]
-- Val Nothing
--
-- >>> pl @(FromList (M.Map _ _) >> Lookup (C "y") Id) [('x',True),('y',False)]
-- Present Just False ((>>) Just False | {Lookup('y') False | q=fromList [('x',True),('y',False)] | p='y'})
-- Val (Just False)
--
-- >>> pl @(FromList (M.Map _ _) >> Lookup (C "z") Id) [('x',True),('y',False)]
-- Present Nothing ((>>) Nothing | {Lookup('z') not found})
-- Val Nothing
--
-- >>> pl @(Lookup 1 Id) [('x',14),('y',3),('z',5)]
-- Present Just ('y',3) (Lookup(1) ('y',3) | q=[('x',14),('y',3),('z',5)] | p=1)
-- Val (Just ('y',3))
--
-- >>> pl @(Lookup 14 Id) [('x',14),('y',3),('z',5)]
-- Present Nothing (Lookup(14) not found)
-- Val Nothing
--
-- >>> pl @(Lookup 3 "abcdef") ()
-- Present Just 'd' (Lookup(3) 'd' | q="abcdef" | p=3)
-- Val (Just 'd')
--
-- >>> pl @(Lookup 4 '[1,2,3,4,5,6]) ()
-- Present Just 5 (Lookup(4) 5 | q=[1,2,3,4,5,6] | p=4)
-- Val (Just 5)
--
data Lookup p q deriving Show

instance ( P p a
         , P q a
         , Show (PP q a)
         , Ixed (PP q a)
         , PP p a ~ Index (PP q a)
         , Show (Index (PP q a))
         , Show (IxValue (PP q a))
         )
   => P (Lookup p q) a where
  type PP (Lookup p q) a = Maybe (IxValue (PP q a))
  eval _ opts a = do
    let msg0 = "Lookup"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> "(" <> showL opts p <> ")"
            hhs = [hh pp, hh qq]
        in case q ^? ix p of
             Nothing -> mkNode opts (Val Nothing) (msg1 <> " not found") hhs
             Just ret -> mkNode opts (Val (Just ret)) (show3' opts msg1 ret "q=" q <> showVerbose opts " | p=" p) hhs

-- | type operator version of 'Lookup'
--
-- >>> pl @((Id !!? C "d") > MkJust 99 || Length Id <= 3) (M.fromList $ zip "abcd" [1..])
-- False (False || False | (Just 4 > Just 99) || (4 <= 3))
-- Val False
--
-- >>> pz @((Id !!? C "d") > MkJust 2 || Length Id <= 3) (M.fromList $ zip "abcd" [1..])
-- Val True
--
data p !!? q deriving Show
type BangBangQT p q = Lookup q p

instance P (BangBangQT p q) a => P (p !!? q) a where
  type PP (p !!? q) a = PP (BangBangQT p q) a
  eval _ = eval (Proxy @(BangBangQT p q))
