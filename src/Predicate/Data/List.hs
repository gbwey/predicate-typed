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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     promoted list functions
-}
module Predicate.Data.List (

 -- ** constructors
    type (:+)
  , type (+:)
  , type (++)
  , Singleton
  , EmptyT
  , EmptyList
  , EmptyList'

 -- ** destructors
  , Uncons
  , Unsnoc
  , Head
  , Tail
  , Init
  , Last

 -- ** sort
  , SortBy
  , SortOn
  , SortOnDesc
  , Sort

 -- ** zip related
  , Unzip
  , Unzip3
  , ZipL
  , ZipR
  , Zip
  , ZipWith

 -- ** higher order methods
  , Partition
  , PartitionBy
  , Group
  , GroupBy
  , GroupCnt
  , GroupStable
  , Filter
  , Break
  , Span
  , Intercalate

 -- ** miscellaneous
  , Elem
  , Inits
  , Tails
  , Ones
  , PadL
  , PadR
  , SplitAts
  , SplitAt
  , ChunksOf
  , ChunksOf'
  , Rotate
  , Take
  , Drop
  , Remove
  , Keep
  , Reverse
  , ReverseL
  , Nub

  , Sum
  , Product
  , Min
  , Max

  , IsPrefix
  , IsInfix
  , IsSuffix

 ) where
import Predicate.Core
import Predicate.Util
import Predicate.Data.Ordering (type (==), OrdA)
import Predicate.Data.Numeric (Mod)
import Predicate.Data.Monoid (type (<>))
import Control.Lens hiding (iall)
import Data.List (partition, intercalate, inits, tails, unfoldr, isInfixOf, isPrefixOf, isSuffixOf, sortOn, group)
import Data.Proxy
import Control.Monad
import Data.Kind (Type)
import Data.Foldable (toList)
import Control.Arrow
import qualified Data.Sequence as Seq
import Data.Bool
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Containers.ListUtils (nubOrd)
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import qualified Data.Map.Strict as M
-- >>> import qualified Data.Text as T
-- >>> import Data.These
-- >>> import Predicate.Prelude

-- | similar to (++)
--
-- >>> pz @(Fst ++ Snd) ([9,10,11],[1,2,3,4])
-- PresentT [9,10,11,1,2,3,4]
--
-- >>> pz @(Snd ++ Fst) ([],[5])
-- PresentT [5]
--
-- >>> pz @(Char1 "xyz" :+ W "ab" ++ W "cdefg") ()
-- PresentT "xabcdefg"
--
-- >>> pz @([1,2,3] ++ EmptyList _) "somestuff"
-- PresentT [1,2,3]
--
data p ++ q
infixr 5 ++

instance (P p x
        , P q x
        , Show (PP p x)
        , PP p x ~ [a]
        , PP q x ~ [a]
        ) => P (p ++ q) x where
  type PP (p ++ q) x = PP q x
  eval _ opts z = do
    let msg0 = "(++)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts z []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = p ++ q
        in mkNode opts (PresentT b) (show01' opts msg0 b "p=" p <> showVerbose opts " | q=" q) [hh pp, hh qq]



-- cant directly create a singleton type using '[] since the type of '[] is unknown. instead use 'Singleton' or 'EmptyT'

-- | similar to cons
--
-- >>> pz @(Fst :+ Snd) (99,[1,2,3,4])
-- PresentT [99,1,2,3,4]
--
-- >>> pz @(Snd :+ Fst) ([],5)
-- PresentT [5]
--
-- >>> pz @(123 :+ EmptyList _) "somestuff"
-- PresentT [123]
--
-- >>> pl @(FlipT (:+) Fst Snd) ([1..5],99)
-- Present [99,1,2,3,4,5] ((:+) [99,1,2,3,4,5] | p=99 | q=[1,2,3,4,5])
-- PresentT [99,1,2,3,4,5]
--
-- >>> pl @(Fst :+ Snd) (99,[1..5])
-- Present [99,1,2,3,4,5] ((:+) [99,1,2,3,4,5] | p=99 | q=[1,2,3,4,5])
-- PresentT [99,1,2,3,4,5]
--
-- >>> pl @(4 :+ '[1,2,3]) ()
-- Present [4,1,2,3] ((:+) [4,1,2,3] | p=4 | q=[1,2,3])
-- PresentT [4,1,2,3]
--
-- >>> pl @(Fst :+ Snd) (4,[1,2,3])
-- Present [4,1,2,3] ((:+) [4,1,2,3] | p=4 | q=[1,2,3])
-- PresentT [4,1,2,3]
--
-- >>> pl @(FlipT (:+) '[1,2,3] 5) ()
-- Present [5,1,2,3] ((:+) [5,1,2,3] | p=5 | q=[1,2,3])
-- PresentT [5,1,2,3]
--
data p :+ q
infixr 5 :+

instance (P p x
        , P q x
        , Show (PP p x)
        , Show (PP q x)
        , Cons (PP q x) (PP q x) (PP p x) (PP p x)
        ) => P (p :+ q) x where
  type PP (p :+ q) x = PP q x
  eval _ opts z = do
    let msg0 = "(:+)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts z []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = p `cons` q
        in mkNode opts (PresentT b) (show01' opts msg0 b "p=" p <> showVerbose opts " | q=" q) [hh pp, hh qq]

-- | similar to snoc
--
-- >>> pz @(Snd +: Fst) (99,[1,2,3,4])
-- PresentT [1,2,3,4,99]
--
-- >>> pz @(Fst +: Snd) ([],5)
-- PresentT [5]
--
-- >>> pz @(EmptyT [] +: 5) 5
-- PresentT [5]
--
-- >>> pl @('[1,2,3] +: 4) ()
-- Present [1,2,3,4] ((+:) [1,2,3,4] | p=[1,2,3] | q=4)
-- PresentT [1,2,3,4]
--
-- >>> pl @(Snd +: Fst) (4,[1,2,3])
-- Present [1,2,3,4] ((+:) [1,2,3,4] | p=[1,2,3] | q=4)
-- PresentT [1,2,3,4]
--
-- >>> pl @("abc" +: Char1 "x") ()
-- Present "abcx" ((+:) "abcx" | p="abc" | q='x')
-- PresentT "abcx"
--
-- >>> pl @(Fst +: Snd) ("abc" :: T.Text,'x')
-- Present "abcx" ((+:) "abcx" | p="abc" | q='x')
-- PresentT "abcx"
--
data p +: q
infixl 5 +:

instance (P p x
        , P q x
        , Show (PP q x)
        , Show (PP p x)
        , Snoc (PP p x) (PP p x) (PP q x) (PP q x)
        ) => P (p +: q) x where
  type PP (p +: q) x = PP p x
  eval _ opts z = do
    let msg0 = "(+:)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts z []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = p `snoc` q
        in mkNode opts (PresentT b) (show01' opts msg0 b "p=" p <> showVerbose opts " | q=" q) [hh pp, hh qq]

-- | similar to 'Control.Lens.uncons'
--
-- >>> pz @Uncons [1,2,3,4]
-- PresentT (Just (1,[2,3,4]))
--
-- >>> pz @Uncons []
-- PresentT Nothing
--
-- >>> pz @Uncons (Seq.fromList "abc")
-- PresentT (Just ('a',fromList "bc"))
--
-- >>> pz @Uncons ("xyz" :: T.Text)
-- PresentT (Just ('x',"yz"))
--
-- >>> pl @Uncons ("asfd" :: T.Text)
-- Present Just ('a',"sfd") (Uncons Just ('a',"sfd") | "asfd")
-- PresentT (Just ('a',"sfd"))
--
-- >>> pl @Uncons ("" :: T.Text)
-- Present Nothing (Uncons Nothing | "")
-- PresentT Nothing
--
-- >>> pl @Uncons [1..5] -- with Typeable would need to specify the type of [1..5]
-- Present Just (1,[2,3,4,5]) (Uncons Just (1,[2,3,4,5]) | [1,2,3,4,5])
-- PresentT (Just (1,[2,3,4,5]))
--

data Uncons

instance (Show (ConsT s)
        , Show s
        , Cons s s (ConsT s) (ConsT s)
        ) => P Uncons s where
  type PP Uncons s = Maybe (ConsT s,s)
  eval _ opts as =
    let msg0 = "Uncons"
        b = as ^? _Cons
    in pure $ mkNode opts (PresentT b) (show01 opts msg0 b as) []

-- | similar to 'Control.Lens.unsnoc'
--
-- >>> pz @Unsnoc [1,2,3,4]
-- PresentT (Just ([1,2,3],4))
--
-- >>> pz @Unsnoc []
-- PresentT Nothing
--
-- >>> pz @Unsnoc ("xyz" :: T.Text)
-- PresentT (Just ("xy",'z'))
--
-- >>> pl @Unsnoc ("asfd" :: T.Text)
-- Present Just ("asf",'d') (Unsnoc Just ("asf",'d') | "asfd")
-- PresentT (Just ("asf",'d'))
--
-- >>> pl @Unsnoc ("" :: T.Text)
-- Present Nothing (Unsnoc Nothing | "")
-- PresentT Nothing
--
-- >>> pl @Unsnoc [1..5]
-- Present Just ([1,2,3,4],5) (Unsnoc Just ([1,2,3,4],5) | [1,2,3,4,5])
-- PresentT (Just ([1,2,3,4],5))
--

data Unsnoc

instance (Show (ConsT s)
        , Show s
        , Snoc s s (ConsT s) (ConsT s)
        ) => P Unsnoc s where
  type PP Unsnoc s = Maybe (s,ConsT s)
  eval _ opts as =
    let msg0 = "Unsnoc"
        b = as ^? _Snoc
    in pure $ mkNode opts (PresentT b) (show01 opts msg0 b as) []

-- | rotate a list @p@ @n@ units
--
-- >>> pz @(Rotate 0 Id) [1,2,3,4]
-- PresentT [1,2,3,4]
--
-- >>> pz @(Rotate (Negate 1) Id) [1,2,3,4]
-- PresentT [4,1,2,3]
--
-- >>> pz @(Rotate 2 Id) [1,2,3,4]
-- PresentT [3,4,1,2]
--
-- >>> pz @(Map (Rotate Id "abcd") Id) [-3..7]
-- PresentT ["bcda","cdab","dabc","abcd","bcda","cdab","dabc","abcd","bcda","cdab","dabc"]
--
data Rotate n p
type RotateT n p = SplitAt (n `Mod` Length p) p >> Swap >> Fst <> Snd

instance P (RotateT n p) x => P (Rotate n p) x where
  type PP (Rotate n p) x = PP (RotateT n p) x
  eval _ = eval (Proxy @(RotateT n p))


-- | similar to 'partition'
--
-- >>> pz @(Partition (Ge 3) Id) [10,4,1,7,3,1,3,5]
-- PresentT ([10,4,7,3,3,5],[1,1])
--
-- >>> pz @(Partition IsPrime Id) [10,4,1,7,3,1,3,5]
-- PresentT ([7,3,3,5],[10,4,1,1])
--
-- >>> pz @(Partition (Ge 300) Id) [10,4,1,7,3,1,3,5]
-- PresentT ([],[10,4,1,7,3,1,3,5])
--
-- >>> pz @(Partition (Id < 300) Id) [10,4,1,7,3,1,3,5]
-- PresentT ([10,4,1,7,3,1,3,5],[])
--
-- >>> pl @(Partition (Lt 2) Id >> Id) [1,2,3,4,5]
-- Present ([1],[2,3,4,5]) ((>>) ([1],[2,3,4,5]) | {Id ([1],[2,3,4,5])})
-- PresentT ([1],[2,3,4,5])
--
-- >>> pl @(Partition (Gt 3) Id) [1..10]
-- Present ([4,5,6,7,8,9,10],[1,2,3]) (Partition ([4,5,6,7,8,9,10],[1,2,3]) | s=[1,2,3,4,5,6,7,8,9,10])
-- PresentT ([4,5,6,7,8,9,10],[1,2,3])
--
-- >>> pl @(Partition Even Id) [1..6]
-- Present ([2,4,6],[1,3,5]) (Partition ([2,4,6],[1,3,5]) | s=[1,2,3,4,5,6])
-- PresentT ([2,4,6],[1,3,5])
--
-- >>> pl @(Partition Even Id >> Null *** (Len > 4) >> Fst == Snd) [1..6]
-- True ((>>) True | {False == False})
-- TrueT
--
-- >>> pl @(Partition (ExitWhen "ExitWhen" (Gt 10) >> Gt 2) Id) [1..11]
-- Error ExitWhen (Partition(i=10, a=11) excnt=1)
-- FailT "ExitWhen"
--
-- >>> pl @(Partition IsPrime Id) [1..15]
-- Present ([2,3,5,7,11,13],[1,4,6,8,9,10,12,14,15]) (Partition ([2,3,5,7,11,13],[1,4,6,8,9,10,12,14,15]) | s=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15])
-- PresentT ([2,3,5,7,11,13],[1,4,6,8,9,10,12,14,15])
--
data Partition p q

instance (P p x
        , Show x
        , PP q a ~ [x]
        , PP p x ~ Bool
        , P q a
        ) => P (Partition p q) a where
  type PP (Partition p q) a = (PP q a, PP q a)
  eval _ opts a' = do
    let msg0 = "Partition"
    qq <- eval (Proxy @q) opts a'
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case chkSize opts msg0 q [hh qq] of
          Left e -> pure e
          Right _ -> do
             ts <- zipWithM (\i a -> ((i, a),) <$> evalBoolHide @p opts a) [0::Int ..] q
             pure $ case splitAndAlign opts msg0 ts of
               Left e -> e
               Right abcs ->
                 let itts = map (view _2 &&& view _3) abcs
                     w0 = partition (view _1) abcs
                     zz1 = (map (view (_2 . _2)) *** map (view (_2 . _2))) w0
                 in mkNode opts (PresentT zz1) (show01' opts msg0 zz1 "s=" q) (hh qq : map (hh . fixit) itts)


-- | partition values based on a function
--
-- >>> pz @(PartitionBy Ordering (Case 'EQ '[Id < 0, Id > 0] '[ 'LT, 'GT] Id) Id) [-4,-2,5,6,7,0,-1,2,-3,4,0]
-- PresentT (fromList [(LT,[-3,-1,-2,-4]),(EQ,[0,0]),(GT,[4,2,7,6,5])])
--
-- >>> pl @(PartitionBy Ordering (Case (Failt _ "asdf") '[Id < 2, Id == 2, Id > 2] '[ 'LT, 'EQ, 'GT] Id) Id) [-4,2,5,6,7,1,2,3,4]
-- Present fromList [(LT,[1,-4]),(EQ,[2,2]),(GT,[4,3,7,6,5])] (PartitionBy fromList [(LT,[1,-4]),(EQ,[2,2]),(GT,[4,3,7,6,5])] | s=[-4,2,5,6,7,1,2,3,4])
-- PresentT (fromList [(LT,[1,-4]),(EQ,[2,2]),(GT,[4,3,7,6,5])])
--
-- >>> pl @(PartitionBy Ordering (Case (Failt _ "xyzxyzxyzzyyysyfsyfydf") '[Id < 2, Id == 2, Id > 3] '[ 'LT, 'EQ, 'GT] Id) Id) [-4,2,5,6,7,1,2,3,4]
-- Error xyzxyzxyzzyyysyfsyfydf (PartitionBy(i=7, a=3) excnt=1)
-- FailT "xyzxyzxyzzyyysyfsyfydf"
--
data PartitionBy t p q

instance (P p x
        , Ord t
        , Show x
        , Show t
        , PP q a ~ [x]
        , PP p x ~ t
        , P q a
        ) => P (PartitionBy t p q) a where
  type PP (PartitionBy t p q) a = M.Map t (PP q a)
  eval _ opts a' = do
    let msg0 = "PartitionBy"
    qq <- eval (Proxy @q) opts a'
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case chkSize opts msg0 q [hh qq] of
          Left e -> pure e
          Right _ -> do
             ts <- zipWithM (\i a -> ((i, a),) <$> evalHide @p opts a) [0::Int ..] q
             pure $ case splitAndAlign opts msg0 ts of
                   Left e -> e
                   Right abcs ->
                     let kvs = map (view _1 &&& ((:[]) . view (_2 . _2))) abcs
                         itts = map (view _2 &&& view _3) abcs
                         ret = M.fromListWith (++) kvs
                     in mkNode opts (PresentT ret) (show01' opts msg0 ret "s=" q ) (hh qq : map (hh . fixit) itts)

-- | similar to 'Data.List.groupBy'
--
-- >>> pz @(GroupBy (Fst == Snd) Id) [1,3,4,5,1,5,5]
-- PresentT [[1],[3],[4],[5],[1],[5,5]]
--
-- >>> pz @(GroupBy (Fst == Snd) Id) [1,1,1,3,4,5,1,5,5]
-- PresentT [[1,1,1],[3],[4],[5],[1],[5,5]]
--
-- >>> pz @(GroupBy (Fst == Snd) Id) [5,5]
-- PresentT [[5,5]]
--
-- >>> pz @(GroupBy (Fst == Snd) Id) [1,2]
-- PresentT [[1],[2]]
--
-- >>> pz @(GroupBy (Fst == Snd) Id) [1]
-- PresentT [[1]]
--
-- >>> pz @(GroupBy (Fst == Snd) Id) []
-- PresentT []
--
-- >>> pz @(GroupBy (Fst < Snd) Id) [1,2,3,4,4,1,2]
-- PresentT [[1,2,3,4],[4],[1,2]]
--
-- >>> pz @(GroupBy (Fst /= Snd) Id) [1,2,3,4,4,4,1]
-- PresentT [[1,2,3,4],[4],[4,1]]
--
-- >>> pan @(GroupBy (Fst == Snd) Id) "hello    goodbye"
-- P GroupBy ["h","e","ll","o","    ","g","oo","d","b","y","e"]
-- |
-- +- P Id "hello    goodbye"
-- |
-- +- False i=0: 'h' == 'e'
-- |
-- +- False i=1: 'e' == 'l'
-- |
-- +- True i=2: 'l' == 'l'
-- |
-- +- False i=3: 'l' == 'o'
-- |
-- +- False i=4: 'o' == ' '
-- |
-- +- True i=5: ' ' == ' '
-- |
-- +- True i=6: ' ' == ' '
-- |
-- +- True i=7: ' ' == ' '
-- |
-- +- False i=8: ' ' == 'g'
-- |
-- +- False i=9: 'g' == 'o'
-- |
-- +- True i=10: 'o' == 'o'
-- |
-- +- False i=11: 'o' == 'd'
-- |
-- +- False i=12: 'd' == 'b'
-- |
-- +- False i=13: 'b' == 'y'
-- |
-- `- False i=14: 'y' == 'e'
-- PresentT ["h","e","ll","o","    ","g","oo","d","b","y","e"]
--
data GroupBy p q

instance (Show x
        , PP q a ~ [x]
        , PP p (x,x) ~ Bool
        , P p (x,x)
        , P q a
        ) => P (GroupBy p q) a where
  type PP (GroupBy p q) a = [PP q a]
  eval _ opts a' = do
    let msg0 = "GroupBy"
    qq <- eval (Proxy @q) opts a'
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case chkSize opts msg0 q [hh qq] of
          Left e -> pure e
          Right _ ->
             case q of
               [] -> pure $ mkNode opts (PresentT []) (show01' opts msg0 q "s=" q) [hh qq]
               [_] -> let ret = [q]
                      in pure $ mkNode opts (PresentT ret) (show01' opts msg0 ret "s=" q) [hh qq]
               x:xs -> do
                 ts <- zipWithM (\i (a,b) -> ((i, b),) <$> evalBoolHide @p opts (a,b)) [0::Int ..] (zip (x:xs) xs)
                 pure $ case splitAndAlign opts msg0 ts of
                   Left e -> e
                   Right abcs ->
                     let ret = gp1 x abcs
                         itts = map (view _2 &&& view _3) abcs
                     in mkNode opts (PresentT ret) (show01' opts msg0 ret "s=" q ) (hh qq : map (hh . fixit) itts)

-- | version of 'GroupCnt' that retains the original ordering
--
-- >>> pz @GroupStable "bcdeaaaaaaaaaf"
-- PresentT [('b',1),('c',1),('d',1),('e',1),('a',9),('f',1)]
--
data GroupStable

instance ( a ~ [x]
         , Ord x
         ) => P GroupStable a where
  type PP GroupStable a = [(ExtractAFromList a, Int)]
  eval _ opts zs =
    let msg0 = "GroupStable"
        xs = map (head &&& length) $ group $ sortOn (ys M.!) zs
        ys = M.fromList $ zip (nubOrd zs) [1::Int ..]
    in pure $ mkNode opts (PresentT xs) msg0 []


-- | similar to 'Data.List.group'
--
-- >>> pz @Group [1,3,4,5,1,5,5]
-- PresentT [[1],[3],[4],[5],[1],[5,5]]
--
-- >>> pz @(Sort >> Group) [1,3,4,5,1,5,5]
-- PresentT [[1,1],[3],[4],[5,5,5]]
--
data Group
type GroupT = GroupBy (Fst == Snd) Id

instance P GroupT x => P Group x where
  type PP Group x = PP GroupT x
  eval _ = eval (Proxy @GroupT)


-- | similar to 'Group' but returns the value and count
--
-- >>> pz @GroupCnt [1,3,4,5,1,5,5]
-- PresentT [(1,1),(3,1),(4,1),(5,1),(1,1),(5,2)]
--
-- >>> pz @(Sort >> GroupCnt) [1,3,4,5,1,5,5]
-- PresentT [(1,2),(3,1),(4,1),(5,3)]
--
data GroupCnt
type GroupCntT = Group >> Map '(Head,Len) Id

instance P GroupCntT x => P GroupCnt x where
  type PP GroupCnt x = PP GroupCntT x
  eval _ = eval (Proxy @GroupCntT)

gp1 :: x -> [(Bool, (Int, x), TT Bool)] -> [[x]]
gp1 b = go [b]
  where
  go ret =
     \case
       [] -> [ret]
       (tf, (_, a), _):as -> if tf then go (ret <> [a]) as
                             else ret : go [a] as

data Filter p q
type FilterT p q = Partition p q >> Fst

instance P (FilterT p q) x => P (Filter p q) x where
  type PP (Filter p q) x = PP (FilterT p q) x
  eval _ = eval (Proxy @(FilterT p q))

-- | similar to 'break'
--
-- >>> pz @(Break (Ge 3) Id) [10,4,1,7,3,1,3,5]
-- PresentT ([],[10,4,1,7,3,1,3,5])
--
-- >>> pz @(Break (Lt 3) Id) [10,4,1,7,3,1,3,5]
-- PresentT ([10,4],[1,7,3,1,3,5])
--
-- >>> pl @(Break (Gt 2) Id) [1..11]
-- Present ([1,2],[3,4,5,6,7,8,9,10,11]) (Break cnt=(2,9))
-- PresentT ([1,2],[3,4,5,6,7,8,9,10,11])
--
-- >>> pl @(Break (If (Gt 2) 'True (If (Gt 4) (Failt _ "ASfd") 'False)) Id) [1..8]
-- Present ([1,2],[3,4,5,6,7,8]) (Break cnt=(2,6))
-- PresentT ([1,2],[3,4,5,6,7,8])
--
-- >>> pl @(Break (Case 'False '[Gt 2,Gt 4] '[W 'True, Failt _ "ASfd"] Id) Id) [1..8]  -- case version
-- Present ([1,2],[3,4,5,6,7,8]) (Break cnt=(2,6))
-- PresentT ([1,2],[3,4,5,6,7,8])
--
-- >>> pl @(Break (If (Gt 2) (Failt _ "ASfd") 'False) Id) [1..8]
-- Error ASfd (Break predicate failed)
-- FailT "ASfd"
--
-- >>> pl @(Break Snd Id) (zip [1..] [False,False,False,True,True,False])
-- Present ([(1,False),(2,False),(3,False)],[(4,True),(5,True),(6,False)]) (Break cnt=(3,3))
-- PresentT ([(1,False),(2,False),(3,False)],[(4,True),(5,True),(6,False)])
--
-- >>> pl @(Break Snd Id) (zip [1..] [False,False,False,False])
-- Present ([(1,False),(2,False),(3,False),(4,False)],[]) (Break cnt=(4,0))
-- PresentT ([(1,False),(2,False),(3,False),(4,False)],[])
--
-- >>> pl @(Break Snd Id) (zip [1..] [True,True,True,True])
-- Present ([],[(1,True),(2,True),(3,True),(4,True)]) (Break cnt=(0,4))
-- PresentT ([],[(1,True),(2,True),(3,True),(4,True)])
--

data Break p q

-- only process up to the pivot! only process while Right False
-- a predicate can return PresentP not just TrueP
instance (P p x
        , PP q a ~ [x]
        , PP p x ~ Bool
        , P q a
        ) => P (Break p q) a where
  type PP (Break p q) a = (PP q a, PP q a)
  eval _ opts a' = do
    let msg0 = "Break"
    qq <- eval (Proxy @q) opts a'
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case chkSize opts msg0 q [hh qq] of
          Left e -> pure e
          Right _ -> do
            let ff [] zs = pure (zs, [], Nothing) -- [(ia,qq)] extras | the rest of the data | optional last pivot or failure
                ff ((i,a):ias) zs = do
                   pp <- evalBoolHide @p opts a
                   let v = ((i,a), pp)
                   case getValueLR opts msg0 pp [hh qq] of
                     Right False -> ff ias (zs Seq.|> v)
                     Right True -> pure (zs,map snd ias,Just v)
                     Left _ -> pure (zs,map snd ias,Just v)
            (ialls,rhs,mpivot) <- ff (itoList q) Seq.empty
            pure $ case mpivot of
                 Nothing ->
                   mkNode opts (PresentT (map (snd . fst) (toList ialls), rhs))
                           (msg0 <> " cnt=" <> show (length ialls, length rhs))
                           (map (hh . fixit) (toList ialls))
                 Just iall@(ia, tt) ->
                   case getValueLR opts (msg0 <> " predicate failed") tt (hh qq : map (hh . fixit) (toList (ialls Seq.|> iall))) of
                     Right True ->
                       mkNode opts (PresentT (map (snd . fst) (toList ialls), snd ia : rhs))
                               (msg0 <> " cnt=" <> show (length ialls, 1+length rhs))
                               (hh qq : hh tt : map (hh . fixit) (toList (ialls Seq.|> iall)))

                     Right False -> errorInProgram "Break"
                     Left e -> e

-- | similar to 'span'
--
-- >>> pl @(Span (Lt 4) Id) [1..11]
-- Present ([1,2,3],[4,5,6,7,8,9,10,11]) (Break cnt=(3,8))
-- PresentT ([1,2,3],[4,5,6,7,8,9,10,11])
--

data Span p q
type SpanT p q = Break (Not p) q

instance P (SpanT p q) x => P (Span p q) x where
  type PP (Span p q) x = PP (SpanT p q) x
  eval _ = eval (Proxy @(SpanT p q))

-- | intercalate two lists
--
-- >>> pz @(Intercalate '["aB"] '["xxxx","yz","z","www","xyz"]) ()
-- PresentT ["xxxx","aB","yz","aB","z","aB","www","aB","xyz"]
--
-- >>> pz @(Intercalate '[W 99,Negate 98] Id) [1..5]
-- PresentT [1,99,-98,2,99,-98,3,99,-98,4,99,-98,5]
--
-- >>> pz @(Intercalate '[99,100] Id) [1..5]
--PresentT [1,99,100,2,99,100,3,99,100,4,99,100,5]
--
-- >>> pl @(Intercalate Fst Snd) ([0,1], [12,13,14,15,16])
-- Present [12,0,1,13,0,1,14,0,1,15,0,1,16] (Intercalate [12,0,1,13,0,1,14,0,1,15,0,1,16] | [0,1] | [12,13,14,15,16])
-- PresentT [12,0,1,13,0,1,14,0,1,15,0,1,16]
--
-- >>> pl @((Pure [] (Negate Len) &&& Id) >> Intercalate Fst Snd) [12,13,14,15,16]
-- Present [12,-5,13,-5,14,-5,15,-5,16] ((>>) [12,-5,13,-5,14,-5,15,-5,16] | {Intercalate [12,-5,13,-5,14,-5,15,-5,16] | [-5] | [12,13,14,15,16]})
-- PresentT [12,-5,13,-5,14,-5,15,-5,16]
--
data Intercalate p q

instance (PP p x ~ [a]
        , PP q x ~ PP p x
        , P p x
        , P q x
        , Show a
      ) => P (Intercalate p q) x where
  type PP (Intercalate p q) x = PP p x
  eval _ opts x = do
    let msg0 = "Intercalate"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case chkSize2 opts msg0 p q hhs of
          Left e -> e
          Right _ ->
            let d = intercalate p (map pure q)
            in mkNode opts (PresentT d) (show01 opts msg0 d p <> showVerbose opts " | " q) hhs

-- | 'elem' function
--
-- >>> pz @(Elem Fst Snd) ('x',"abcdxy")
-- TrueT
--
-- >>> pz @(Elem Fst Snd) ('z',"abcdxy")
-- FalseT
--
-- >>> pl @(Elem Id '[2,3,4]) 2
-- True (2 `elem` [2,3,4])
-- TrueT
--
-- >>> pl @(Elem Id '[2,3,4]) 6
-- False (6 `elem` [2,3,4])
-- FalseT
--
-- >>> pl @(Elem Id '[13 % 2]) 6.5
-- True (13 % 2 `elem` [13 % 2])
-- TrueT
--
-- >>> pl @(Elem Id '[13 % 2, 12 % 1]) 6.5
-- True (13 % 2 `elem` [13 % 2,12 % 1])
-- TrueT
--
-- >>> pl @(Elem Id '[13 % 2, 12 % 1]) 6
-- False (6 % 1 `elem` [13 % 2,12 % 1])
-- FalseT
--

data Elem p q

instance ([PP p a] ~ PP q a
         , P p a
         , P q a
         , Show (PP p a)
         , Eq (PP p a)
         ) => P (Elem p q) a where
  type PP (Elem p q) a = Bool
  eval _ opts a = do
    let msg0 = "Elem"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = p `elem` q
        in mkNodeB opts b (showL opts p <> " `elem` " <> showL opts q) [hh pp, hh qq]

-- | similar to 'Data.List.inits'
--
-- >>> pz @Inits [4,8,3,9]
-- PresentT [[],[4],[4,8],[4,8,3],[4,8,3,9]]
--
-- >>> pz @Inits []
-- PresentT [[]]
--
data Inits

instance ( [a] ~ x
         , Show a
         ) => P Inits x where
  type PP Inits x = [x]
  eval _ opts as =
    let msg0 = "Inits"
        xs = inits as
    in pure $ mkNode opts (PresentT xs) (show01 opts msg0 xs as) []

-- | similar to 'Data.List.tails'
--
-- >>> pz @Tails [4,8,3,9]
-- PresentT [[4,8,3,9],[8,3,9],[3,9],[9],[]]
--
-- >>> pz @Tails []
-- PresentT [[]]
--
-- >>> pl @Tails "abcd"
-- Present ["abcd","bcd","cd","d",""] (Tails ["abcd","bcd","cd","d",""] | "abcd")
-- PresentT ["abcd","bcd","cd","d",""]
--
data Tails

instance ( [a] ~ x
         , Show a
         ) => P Tails x where
  type PP Tails x = [x]
  eval _ opts as =
    let msg0 = "Tails"
        xs = tails as
    in pure $ mkNode opts (PresentT xs) (show01 opts msg0 xs as) []

-- | split a list into single values
--
-- >>> pz @Ones [4,8,3,9]
-- PresentT [[4],[8],[3],[9]]
--
-- >>> pz @Ones []
-- PresentT []
--
data Ones

instance x ~ [a] => P Ones x where
  type PP Ones x = [x]
  eval _ opts x =
    let msg0 = "Ones"
    in pure $ case chkSize opts msg0 x [] of
          Left e -> e
          Right _ ->
            let d = map pure x
            in mkNode opts (PresentT d) "" []

data PadImpl (left :: Bool) n p q

instance (P n a
        , GetBool left
        , Integral (PP n a)
        , [PP p a] ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        ) => P (PadImpl left n p q) a where
  type PP (PadImpl left n p q) a = PP q a
  eval _ opts a = do
    let msg0 = "Pad" <> (if lft then "L" else "R")
        lft = getBool @left
    lr <- runPQ msg0 (Proxy @n) (Proxy @p) opts a []
    case lr of
      Left e -> pure e
      Right (fromIntegral -> n,p,nn,pp) -> do
        let msg1 = msg0 <> " " <> showL opts n <> " pad=" <> showL opts p
            hhs = [hh nn, hh pp]
        qq <- eval (Proxy @q) opts a
        pure $ case getValueLR opts (msg1 <> " q failed") qq hhs of
          Left e -> e
          Right q ->
            let l = length q
                diff = if n<=l then 0 else n-l
                bs = if lft
                     then replicate diff p <> q
                     else q <> replicate diff p
            in mkNode opts (PresentT bs) (show01 opts msg1 bs q) (hhs <> [hh qq])

-- | left pad @q@ with @n@ values from @p@
--
-- >>> pl @(PadL 5 0 Id) [1..3]
-- Present [0,0,1,2,3] (PadL 5 pad=0 [0,0,1,2,3] | [1,2,3])
-- PresentT [0,0,1,2,3]
--
-- >>> pz @(PadL 5 999 Id) [12,13]
-- PresentT [999,999,999,12,13]
--
-- >>> pz @(PadR 5 Fst '[12,13]) (999,'x')
-- PresentT [12,13,999,999,999]
--
-- >>> pz @(PadR 2 Fst '[12,13,14]) (999,'x')
-- PresentT [12,13,14]
--
-- >>> pl @(PadL 10 0 Id) [1..3]
-- Present [0,0,0,0,0,0,0,1,2,3] (PadL 10 pad=0 [0,0,0,0,0,0,0,1,2,3] | [1,2,3])
-- PresentT [0,0,0,0,0,0,0,1,2,3]
--
data PadL n p q
type PadLT n p q = PadImpl 'True n p q

instance P (PadLT n p q) x => P (PadL n p q) x where
  type PP (PadL n p q) x = PP (PadLT n p q) x
  eval _ = eval (Proxy @(PadLT n p q))

-- | right pad @q@ with @n@ values from @p@
--
-- >>> pl @(PadR 5 8 Id) [1..3]
-- Present [1,2,3,8,8] (PadR 5 pad=8 [1,2,3,8,8] | [1,2,3])
-- PresentT [1,2,3,8,8]
--
-- >>> pl @(PadR 5 0 Id) [1..5]
-- Present [1,2,3,4,5] (PadR 5 pad=0 [1,2,3,4,5] | [1,2,3,4,5])
-- PresentT [1,2,3,4,5]
--
-- >>> pl @(PadR 5 0 Id) [1..6]
-- Present [1,2,3,4,5,6] (PadR 5 pad=0 [1,2,3,4,5,6] | [1,2,3,4,5,6])
-- PresentT [1,2,3,4,5,6]
--
data PadR n p q
type PadRT n p q = PadImpl 'False n p q

instance P (PadRT n p q) x => P (PadR n p q) x where
  type PP (PadR n p q) x = PP (PadRT n p q) x
  eval _ = eval (Proxy @(PadRT n p q))

-- | split a list @p@ into parts using the lengths in the type level list @ns@
--
-- >>> pz @(SplitAts '[2,3,1,1] Id) "hello world"
-- PresentT ["he","llo"," ","w","orld"]
--
-- >>> pz @(SplitAts '[2] Id) "hello world"
-- PresentT ["he","llo world"]
--
-- >>> pz @(SplitAts '[10,1,1,5] Id) "hello world"
-- PresentT ["hello worl","d","",""]
--
-- >>> pl @(SplitAts '[1,3,4] Id) [1..12]
-- Present [[1],[2,3,4],[5,6,7,8],[9,10,11,12]] (SplitAts [[1],[2,3,4],[5,6,7,8],[9,10,11,12]] | ns=[1,3,4] | [1,2,3,4,5,6,7,8,9,10,11,12])
-- PresentT [[1],[2,3,4],[5,6,7,8],[9,10,11,12]]
--
-- >>> pl @(SplitAts '[3,1,1,1] Id >> Filter (Not Null) Id) [1..4]
-- Present [[1,2,3],[4]] ((>>) [[1,2,3],[4]] | {Fst [[1,2,3],[4]] | ([[1,2,3],[4]],[[],[]])})
-- PresentT [[1,2,3],[4]]
--
data SplitAts ns p

instance (P ns x
        , P p x
        , PP p x ~ [a]
        , Show n
        , Show a
        , PP ns x ~ [n]
        , Integral n
        ) => P (SplitAts ns p) x where
  type PP (SplitAts ns p) x = [PP p x]
  eval _ opts x = do
    let msg0 = "SplitAts"
    lr <- runPQ msg0 (Proxy @ns) (Proxy @p) opts x []
    pure $ case lr of
      Left e -> e
      Right (ns,p,nn,pp) ->
        let zs = foldr (\n k s -> let (a,b) = splitAtNeg (fromIntegral n) s
                              in a:k b
                   ) (\as -> if null as then [] else [as]) ns p
        in mkNode opts (PresentT zs) (show01' opts msg0 zs "ns=" ns <> showVerbose opts " | " p) [hh nn, hh pp]

-- | similar to 'splitAt'
--
-- >>> pz @(SplitAt 4 Id) "hello world"
-- PresentT ("hell","o world")
--
-- >>> pz @(SplitAt 20 Id) "hello world"
-- PresentT ("hello world","")
--
-- >>> pz @(SplitAt 0 Id) "hello world"
-- PresentT ("","hello world")
--
-- >>> pz @(SplitAt Snd Fst) ("hello world",4)
-- PresentT ("hell","o world")
--
-- >>> pz @(SplitAt (Negate 2) Id) "hello world"
-- PresentT ("hello wor","ld")
--
-- >>> pl @(Snd >> SplitAt 2 Id >> Len *** Len >> Fst > Snd) ('x',[1..5])
-- False ((>>) False | {2 > 3})
-- FalseT
--
data SplitAt n p

instance (PP p a ~ [b]
        , P n a
        , P p a
        , Show b
        , Integral (PP n a)
        ) => P (SplitAt n p) a where
  type PP (SplitAt n p) a = (PP p a, PP p a)
  eval _ opts a = do
    let msg0 = "SplitAt"
    lr <- runPQ msg0 (Proxy @n) (Proxy @p) opts a []
    pure $ case lr of
      Left e -> e -- (Left e, tt')
      Right (fromIntegral -> n,p,pp,qq) ->
        let msg1 = msg0 <> " " <> showL opts n <> " " <> showL opts p
            ret = splitAtNeg n p
       in mkNode opts (PresentT ret) (show01' opts msg1 ret "n=" n <> showVerbose opts " | " p) [hh pp, hh qq]

splitAtNeg :: Int -> [a] -> ([a], [a])
splitAtNeg n as = splitAt (if n<0 then length as + n else n) as


data Take n p
type TakeT n p = SplitAt n p >> Fst

instance P (TakeT n p) x => P (Take n p) x where
  type PP (Take n p) x = PP (TakeT n p) x
  eval _ = eval (Proxy @(TakeT n p))

data Drop n p
type DropT n p = SplitAt n p >> Snd

instance P (DropT n p) x => P (Drop n p) x where
  type PP (Drop n p) x = PP (DropT n p) x
  eval _ = eval (Proxy @(DropT n p))

-- | splits a list pointed to by @p@ into lists of size @n@
--
-- >>> pz @(ChunksOf 2) "abcdef"
-- PresentT ["ab","cd","ef"]
--
-- >>> pz @(ChunksOf 2) "abcdefg"
-- PresentT ["ab","cd","ef","g"]
--
-- >>> pz @(ChunksOf 2) ""
-- PresentT []
--
-- >>> pz @(ChunksOf 2) "a"
-- PresentT ["a"]
--
-- >>> pz @(PadR (Len + RoundUp 5 Len) 999 Id >> ChunksOf 5) [1..17]
-- PresentT [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,999,999,999]]
--
-- >>> pz @(PadR (Len + RoundUp 5 Len) 999 Id >> ChunksOf 5) [1..15]
-- PresentT [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15]]
--
data ChunksOf n
type ChunksOfT n = ChunksOf' n n Id

instance P (ChunksOfT n) x => P (ChunksOf n) x where
  type PP (ChunksOf n) x = PP (ChunksOfT n) x
  eval _ = eval (Proxy @(ChunksOfT n))

-- | splits a list pointed to by @p@ into lists of size @n@ with a gap of @i@
--
-- >>> pz @(Unfoldr (If Null (MkNothing _) (MkJust '(Take 3 Id,Drop 2 Id))) Id) [1..10]
-- PresentT [[1,2,3],[3,4,5],[5,6,7],[7,8,9],[9,10]]
--
-- >>> pz @(ChunksOf' 3 2 Id) [1..10]
-- PresentT [[1,2,3],[3,4,5],[5,6,7],[7,8,9],[9,10]]
--
data ChunksOf' n i p

instance (PP p a ~ [b]
        , P n a
        , P i a
        , P p a
        , Show b
        , Integral (PP i a)
        , Integral (PP n a)
        ) => P (ChunksOf' n i p) a where
  type PP (ChunksOf' n i p) a = [PP p a]
  eval _ opts a = do
    let msg0 = "ChunksOf"
    lr <- runPQ msg0 (Proxy @n) (Proxy @i) opts a []
    case lr of
      Left e -> pure e
      Right (fromIntegral -> n,fromIntegral -> i,nn,ii) -> do
        let hhs = [hh nn, hh ii]
            msg1 = msg0 <> " " <> showL opts (n,i)
        pp <- eval (Proxy @p) opts a
        pure $ case getValueLR opts (msg1 <> " p failed") pp hhs of
          Left e -> e
          Right p ->
            let hhs1 = hhs ++ [hh pp]
            in if n <= 0 then mkNode opts (FailT (msg0 <> " n<=0")) "" hhs1
               else if i < 1 then mkNode opts (FailT (msg0 <> " i<1")) "" hhs1
               else let ret = unfoldr (\s -> if null s then Nothing else Just (take n s,drop i s)) p
                in mkNode opts (PresentT ret) (show01' opts msg1 ret "n,i=" (n,i) <> showVerbose opts " | " p) hhs1

-- empty lists at the type level wont work here

data KeepImpl (keep :: Bool) p q

instance (GetBool keep
        , Eq a
        , Show a
        , P p x
        , P q x
        , PP p x ~ PP q x
        , PP q x ~ [a]
        ) => P (KeepImpl keep p q) x where
  type PP (KeepImpl keep p q) x = PP q x
  eval _ opts x = do
    let msg0 = if keep then "Keep" else "Remove"
        keep = getBool @keep
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let ret = filter (bool not id keep . (`elem` p)) q
        in mkNode opts (PresentT ret) (show01' opts msg0 ret "p=" p <> showVerbose opts " | q=" q) [hh pp, hh qq]

-- | filters a list @q@ keeping those elements in @p@
--
-- >>> pz @(Keep '[5] '[1,5,5,2,5,2]) ()
-- PresentT [5,5,5]
--
-- >>> pz @(Keep '[0,1,1,5] '[1,5,5,2,5,2]) ()
-- PresentT [1,5,5,5]
--
data Keep p q
type KeepT p q = KeepImpl 'True p q

instance P (KeepT p q) x => P (Keep p q) x where
  type PP (Keep p q) x = PP (KeepT p q) x
  eval _ = eval (Proxy @(KeepT p q))

-- | filters a list @q@ removing those elements in @p@
--
-- >>> pz @(Remove '[5] '[1,5,5,2,5,2]) ()
-- PresentT [1,2,2]
--
-- >>> pz @(Remove '[0,1,1,5] '[1,5,5,2,5,2]) ()
-- PresentT [2,2]
--
-- >>> pz @(Remove '[99] '[1,5,5,2,5,2]) ()
-- PresentT [1,5,5,2,5,2]
--
-- >>> pz @(Remove '[99,91] '[1,5,5,2,5,2]) ()
-- PresentT [1,5,5,2,5,2]
--
-- >>> pz @(Remove Id '[1,5,5,2,5,2]) []
-- PresentT [1,5,5,2,5,2]
--
-- >>> pz @(Remove '[] '[1,5,5,2,5,2]) 44 -- works if you make this a number!
-- PresentT [1,5,5,2,5,2]
--
data Remove p q
type RemoveT p q = KeepImpl 'False p q

instance P (RemoveT p q) x => P (Remove p q) x where
  type PP (Remove p q) x = PP (RemoveT p q) x
  eval _ = eval (Proxy @(RemoveT p q))

-- | takes the head of a list-like container: similar to 'head'
--
-- >>> pz @Head "abcd"
-- PresentT 'a'
--
-- >>> pz @Head []
-- FailT "Head(empty)"
--
-- >>> pl @Head ([] :: [Int])
-- Error Head(empty)
-- FailT "Head(empty)"
--
-- >>> pl @Head ([] :: [Double])
-- Error Head(empty)
-- FailT "Head(empty)"
--
-- >>> pl @(Fst >> Head >> Le 6) ([]::[Int], True)
-- Error Head(empty) ([])
-- FailT "Head(empty)"
--
-- >>> pl @Head [1,2,3]
-- Present 1 (Head 1 | [1,2,3])
-- PresentT 1
--

data Head

instance ( Cons x x (ConsT x) (ConsT x)
         , Show (ConsT x)
         , Show x
         ) => P Head x where
  type PP Head x = ConsT x
  eval _ opts x =
    let msg0 = "Head"
    in pure $ case x ^? _Cons of
        Nothing -> mkNode opts (FailT (msg0 <> "(empty)")) "" []
        Just (a,_) -> mkNode opts (PresentT a) (show01 opts msg0 a x) []

-- | takes the tail of a list-like container: similar to 'tail'
--
-- >>> pz @Tail "abcd"
-- PresentT "bcd"
--
-- >>> pl @Tail [1..5]
-- Present [2,3,4,5] (Tail [2,3,4,5] | [1,2,3,4,5])
-- PresentT [2,3,4,5]
--
-- >>> pl @Tail []
-- Error Tail(empty)
-- FailT "Tail(empty)"
--

data Tail

instance ( Cons x x (ConsT x) (ConsT x)
         , Show x
         ) => P Tail x where
  type PP Tail x = x
  eval _ opts x = do
    let msg0 = "Tail"
    pure $ case x ^? _Cons of
      Nothing -> mkNode opts (FailT (msg0 <> "(empty)")) "" []
      Just (_,as) -> mkNode opts (PresentT as) (show01 opts msg0 as x) []


-- | takes the last of a list-like container: similar to 'last'
--
-- >>> pz @Last "abcd"
-- PresentT 'd'
--
-- >>> pz @Last []
-- FailT "Last(empty)"
--
-- >>> pl @Last [1,2,3]
-- Present 3 (Last 3 | [1,2,3])
-- PresentT 3
--

data Last

instance ( Snoc x x (ConsT x) (ConsT x)
         , Show (ConsT x)
         , Show x
         ) => P Last x where
  type PP Last x = ConsT x
  eval _ opts x =
    let msg0 = "Last"
    in pure $ case x ^? _Snoc of
          Nothing -> mkNode opts (FailT (msg0 <> "(empty)")) "" []
          Just (_,a) -> mkNode opts (PresentT a) (show01 opts msg0 a x) []

-- | takes the init of a list-like container: similar to 'init'
--
-- >>> pz @Init "abcd"
-- PresentT "abc"
--
-- >>> pz @Init (T.pack "abcd")
-- PresentT "abc"
--
-- >>> pz @Init []
-- FailT "Init(empty)"
--
-- >>> pl @Init [1..5]
-- Present [1,2,3,4] (Init [1,2,3,4] | [1,2,3,4,5])
-- PresentT [1,2,3,4]
--
-- >>> pl @Init []
-- Error Init(empty)
-- FailT "Init(empty)"
--
data Init

instance ( Snoc s s (ConsT s) (ConsT s)
         , x ~ s
         , Show s
         ) => P Init x where
  type PP Init x = x
  eval _ opts x = do
    let msg0 = "Init"
    pure $ case x ^? _Snoc of
      Nothing -> mkNode opts (FailT (msg0 <> "(empty)")) "" []
      Just (as,_) -> mkNode opts (PresentT as) (show01 opts msg0 as x) []


-- | 'unzip' equivalent
--
-- >>> pz @Unzip (zip [1..5] "abcd")
-- PresentT ([1,2,3,4],"abcd")
--
data Unzip
type UnzipT = '(Map Fst Id, Map Snd Id)

instance P UnzipT x => P Unzip x where
  type PP Unzip x = PP UnzipT x
  eval _ = eval (Proxy @UnzipT)


-- | 'unzip3' equivalent
--
-- >>> pz @Unzip3 (zip3 [1..5] "abcd" (cycle [True,False]))
-- PresentT ([1,2,3,4],"abcd",[True,False,True,False])
--
data Unzip3
type Unzip3T = '(Map Fst Id, Map Snd Id, Map Thd Id)

instance P Unzip3T x => P Unzip3 x where
  type PP Unzip3 x = PP Unzip3T x
  eval _ = eval (Proxy @Unzip3T)

-- | sort a list
--
-- >>> pz @(SortBy (OrdP Snd Fst) Id) [(10,"ab"),(4,"x"),(20,"bbb")]
-- PresentT [(20,"bbb"),(10,"ab"),(4,"x")]
--
-- >>> pz @(SortBy 'LT Id) [1,5,2,4,7,0]
-- PresentT [1,5,2,4,7,0]
--
-- >>> pz @(SortBy 'GT Id) [1,5,2,4,7,0]
-- PresentT [0,7,4,2,5,1]
--
-- >>> pz @(SortBy ((L1 Fst ==! L1 Snd) <> (L2 Fst ==! L2 Snd)) Id) [(10,"ab"),(4,"x"),(20,"bbb"),(4,"a"),(4,"y")]
-- PresentT [(4,"a"),(4,"x"),(4,"y"),(10,"ab"),(20,"bbb")]
--
-- >>> pz @(SortBy ((L1 Fst ==! L1 Snd) <> (L2 Snd ==! L2 Fst)) Id) [(10,"ab"),(4,"x"),(20,"bbb"),(4,"a"),(4,"y")]
-- PresentT [(4,"y"),(4,"x"),(4,"a"),(10,"ab"),(20,"bbb")]
--
-- >>> pl @(SortBy (Swap >> OrdA Fst) Snd) ((),[('z',1),('a',10),('m',22)])
-- Present [('z',1),('m',22),('a',10)] (SortBy [('z',1),('m',22),('a',10)])
-- PresentT [('z',1),('m',22),('a',10)]
--
-- >>> pl @(SortBy (OrdA Reverse) Id) ["az","by","cx","aa"]
-- Present ["aa","cx","by","az"] (SortBy ["aa","cx","by","az"])
-- PresentT ["aa","cx","by","az"]
--
-- >>> pl @(SortBy (If (Fst==5 && Snd==3) (Failt _ (PrintT "pivot=%d value=%d" Id)) 'GT) Snd) ((), [5,7,3,1,6,2,1,3])
-- Error pivot=5 value=3(2) (SortBy)
-- FailT "pivot=5 value=3(2)"
--
-- >>> pl @(SortBy (If (Fst==50 && Snd==3) (Failt _ (PrintT "pivot=%d value=%d" Id)) (OrdA Id)) Snd) ((), [5,7,3,1,6,2,1,3])
-- Present [1,1,2,3,3,5,6,7] (SortBy [1,1,2,3,3,5,6,7])
-- PresentT [1,1,2,3,3,5,6,7]
--
data SortBy p q

type SortByHelperT p = Partition (p == 'GT) Id

instance (P p (a,a)
        , P q x
        , Show a
        , PP q x ~ [a]
        , PP p (a,a) ~ Ordering
        ) => P (SortBy p q) x where
  type PP (SortBy p q) x = PP q x
  eval _ opts x = do
    let msg0 = "SortBy"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts (msg0 <> " q failed") qq [] of
      Left e -> pure e
      Right as -> do
        let ff :: MonadEval m => [a] -> m (TT [a])
            ff = \case
                [] -> pure $ mkNode opts (PresentT mempty) (msg0 <> " empty") [hh qq]
                [w] -> pure $ mkNode opts (PresentT [w]) (msg0 <> " one element " <> showL opts w) [hh qq]
                w:ys@(_:_) -> do
                  pp <- evalHide @(SortByHelperT p) opts (map (w,) ys)
                  case getValueLR opts msg0 pp [hh qq] of
                    Left e -> pure e
                    Right (ll', rr') -> do
                      lhs <- ff (map snd ll')
                      case getValueLR opts msg0 lhs [hh qq, hh pp] of
                        Left _ -> pure lhs -- dont rewrap or rewrite
                        Right ll -> do
                          rhs <- ff (map snd rr')
                          case getValueLR opts msg0 rhs [hh qq, hh pp, hh lhs] of
                            Left _ -> pure rhs
                            Right rr ->
                              pure $  mkNode opts (PresentT (ll ++ w : rr))
                                     (msg0 <> " lhs=" <> showL opts ll <> " pivot " <> showL opts w <> " rhs=" <> showL opts rr)
                                     (hh pp : [hh lhs | length ll > 1] ++ [hh rhs | length rr > 1])
        ret <- ff as
        pure $ case getValueLR opts msg0 ret [hh qq] of
          Left _e -> ret -- dont rewrap else will double up messages: already handled
          Right xs -> mkNode opts (_ttBool ret) (msg0 <> " " <> showL opts xs) [hh qq, hh ret]

-- | similar to 'Data.List.sortOn'
--
-- >>> pz @(SortOn Fst Id) [(10,"abc"), (3,"def"), (4,"gg"), (10,"xyz"), (1,"z")]
-- PresentT [(1,"z"),(3,"def"),(4,"gg"),(10,"abc"),(10,"xyz")]
--
-- >>> pl @(SortOn Id Id) [10,4,2,12,14]
-- Present [2,4,10,12,14] (SortBy [2,4,10,12,14])
-- PresentT [2,4,10,12,14]
--
-- >>> pl @(SortOn (Negate Id) Id) [10,4,2,12,14]
-- Present [14,12,10,4,2] (SortBy [14,12,10,4,2])
-- PresentT [14,12,10,4,2]
--
-- >>> pl @(SortOn Fst Id) (zip "cabdaz" [10,4,2,12,14,1])
-- Present [('a',4),('a',14),('b',2),('c',10),('d',12),('z',1)] (SortBy [('a',4),('a',14),('b',2),('c',10),('d',12),('z',1)])
-- PresentT [('a',4),('a',14),('b',2),('c',10),('d',12),('z',1)]
--
-- >>> pl @(SortOn (FailS "asdf") Id) [10,4,2,12,14]
-- Error asdf(4) (SortBy)
-- FailT "asdf(4)"
--
-- >>> pl @(SortOn Snd Snd) ((),[('z',14),('a',10),('m',22),('a',1)])
-- Present [('a',1),('a',10),('z',14),('m',22)] (SortBy [('a',1),('a',10),('z',14),('m',22)])
-- PresentT [('a',1),('a',10),('z',14),('m',22)]
--
-- >>> pl @(SortOn Fst Snd) ((),[('z',1),('a',10),('m',22)])
-- Present [('a',10),('m',22),('z',1)] (SortBy [('a',10),('m',22),('z',1)])
-- PresentT [('a',10),('m',22),('z',1)]
--
-- >>> pl @(SortOn Fst Id) [('z',1),('a',10),('m',22),('a',9),('m',10)]
-- Present [('a',10),('a',9),('m',22),('m',10),('z',1)] (SortBy [('a',10),('a',9),('m',22),('m',10),('z',1)])
-- PresentT [('a',10),('a',9),('m',22),('m',10),('z',1)]
--
-- >>> pl @(SortOn Id Id) [('z',1),('a',10),('m',22),('a',9),('m',10)]
-- Present [('a',9),('a',10),('m',10),('m',22),('z',1)] (SortBy [('a',9),('a',10),('m',10),('m',22),('z',1)])
-- PresentT [('a',9),('a',10),('m',10),('m',22),('z',1)]
--
data SortOn p q
type SortOnT p q = SortBy (OrdA p) q

instance P (SortOnT p q) x => P (SortOn p q) x where
  type PP (SortOn p q) x = PP (SortOnT p q) x
  eval _ = eval (Proxy @(SortOnT p q))

-- | like SortOn but descending order
--
-- >>> pl @(SortOnDesc Id Id) [10,4,2,12,14]
-- Present [14,12,10,4,2] (SortBy [14,12,10,4,2])
-- PresentT [14,12,10,4,2]
--
-- >>> pl @(SortOnDesc Fst Snd) ((),[('z',1),('a',10),('m',22)])
-- Present [('z',1),('m',22),('a',10)] (SortBy [('z',1),('m',22),('a',10)])
-- PresentT [('z',1),('m',22),('a',10)]
--
data SortOnDesc p q
type SortOnDescT p q = SortBy (Swap >> OrdA p) q

instance P (SortOnDescT p q) x => P (SortOnDesc p q) x where
  type PP (SortOnDesc p q) x = PP (SortOnDescT p q) x
  eval _ = eval (Proxy @(SortOnDescT p q))

data Sort
type SortT = SortOn Id Id

instance P SortT x => P Sort x where
  type PP Sort x = PP SortT x
  eval _ = eval (Proxy @SortT)

-- | similar to 'reverse'
--
-- >>> pz @Reverse [1,2,4]
-- PresentT [4,2,1]
--
-- >>> pz @Reverse "AbcDeF"
-- PresentT "FeDcbA"
--
data Reverse

instance ( as ~ [a]
         , Show a
         ) => P Reverse as where
  type PP Reverse as = as
  eval _ opts as =
    let msg0 = "Reverse"
        d = reverse as
    in pure $ mkNode opts (PresentT d) (show01 opts msg0 d as) []

-- | reverses using 'reversing'
--
-- >>> pz @ReverseL (T.pack "AbcDeF")
-- PresentT "FeDcbA"
--
-- >>> pz @ReverseL ("AbcDeF" :: String)
-- PresentT "FeDcbA"
--
-- >>> pl @ReverseL ("asfd" :: T.Text)
-- Present "dfsa" (ReverseL "dfsa" | "asfd")
-- PresentT "dfsa"
--
data ReverseL

instance ( Reversing t
         , Show t
         ) => P ReverseL t where
  type PP ReverseL t = t
  eval _ opts as =
    let msg0 = "ReverseL"
        d = as ^. reversed
    in pure $ mkNode opts (PresentT d) (show01 opts msg0 d as) []

-- | creates a singleton from a value
--
-- >>> pz @(Singleton (Char1 "aBc")) ()
-- PresentT "a"
--
-- >>> pz @(Singleton Id) False
-- PresentT [False]
--
-- >>> pz @(Singleton Snd) (False,"hello")
-- PresentT ["hello"]
--
data Singleton p

instance P p x => P (Singleton p) x where
  type PP (Singleton p) x = [PP p x]
  eval _ opts x = do
    let msg0 = "Singleton"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p -> mkNode opts (PresentT [p]) msg0 [hh pp]

data EmptyList' t

instance P (EmptyList' t) x where
  type PP (EmptyList' t) x = [PP t x]
  eval _ opts _ =
    pure $ mkNode opts (PresentT []) "EmptyList" []

-- | creates an empty list for the given type
--
-- >>> pz @(Id :+ EmptyList _) 99
-- PresentT [99]
--
data EmptyList (t :: Type)
type EmptyListT (t :: Type) = EmptyList' (Hole t)

instance P (EmptyList t) x where
  type PP (EmptyList t) x = PP (EmptyListT t) x
  eval _ = eval (Proxy @(EmptyListT t))


-- | like 'zipWith'
--
-- >>> pz @(ZipWith Id (1...5) (Char1 "a" ... Char1 "e")) ()
-- PresentT [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')]
--
-- >>> pz @(ZipWith (ShowP Fst <> ShowP Snd) (1...5) (Char1 "a" ... Char1 "e")) ()
-- PresentT ["1'a'","2'b'","3'c'","4'd'","5'e'"]
--
-- >>> pz @(ZipWith (MkThese Fst Snd) (1...6) (Char1 "a" ... Char1 "f")) ()
-- PresentT [These 1 'a',These 2 'b',These 3 'c',These 4 'd',These 5 'e',These 6 'f']
--
-- >>> pz @(ZipWith (MkThese Fst Snd) '[] (Char1 "a" ... Char1 "f")) ()
-- FailT "ZipWith(0,6) length mismatch"
--
-- >>> pz @(ZipWith (MkThese Fst Snd) (1...3) (Char1 "a" ... Char1 "f")) ()
-- FailT "ZipWith(3,6) length mismatch"
--
data ZipWith p q r

instance (PP q a ~ [x]
        , PP r a ~ [y]
        , P q a
        , P r a
        , P p (x,y)
        , Show x
        , Show y
        , Show (PP p (x,y))
        ) => P (ZipWith p q r) a where
  type PP (ZipWith p q r) a = [PP p (ExtractAFromList (PP q a), ExtractAFromList (PP r a))]
  eval _ opts a = do
    let msg0 = "ZipWith"
    lr <- runPQ msg0 (Proxy @q) (Proxy @r) opts a []
    case lr of
      Left e -> pure e
      Right (q,r,qq,rr) ->
        let hhs = [hh qq, hh rr]
        in case chkSize2 opts msg0 q r hhs of
          Left e -> pure e
          Right _ -> do
            let lls = (length q, length r)
            if uncurry (==) lls then do
               ts <- zipWithM (\i (x,y) -> ((i, (x,y)),) <$> evalHide @p opts (x,y)) [0::Int ..] (zip q r)
               pure $ case splitAndAlign opts msg0 ts of
                 Left e -> e
                 Right abcs ->
                   let kvs = map (view _1 &&& ((:[]) . view (_2 . _2))) abcs
                       itts = map (view _2 &&& view _3) abcs
                       ret = map fst kvs
                   in mkNode opts (PresentT ret) (show01' opts msg0 ret "s=" q ) (hh qq : map (hh . fixit) itts)

             else do
                   let msg1 = msg0 ++ show lls
                   pure $ mkNode opts (FailT (msg1 <> " length mismatch")) (showVerbose opts "q=" q <> showVerbose opts " | r=" r) hhs

-- | Zip two lists to their maximum length using optional padding
--
-- >>> pz @(ZipPad (Char1 "Z") 99 Fst Snd) ("abc", [1..5])
-- PresentT [('a',1),('b',2),('c',3),('Z',4),('Z',5)]
--
-- >>> pz @(ZipPad (Char1 "Z") 99 Fst Snd) ("abcdefg", [1..5])
-- PresentT [('a',1),('b',2),('c',3),('d',4),('e',5),('f',99),('g',99)]
--
-- >>> pz @(ZipPad (Char1 "Z") 99 Fst Snd) ("abcde", [1..5])
-- PresentT [('a',1),('b',2),('c',3),('d',4),('e',5)]
--
-- >>> pz @(ZipPad (Char1 "Z") 99 Fst Snd) ("", [1..5])
-- PresentT [('Z',1),('Z',2),('Z',3),('Z',4),('Z',5)]
--
-- >>> pz @(ZipPad (Char1 "Z") 99 Fst Snd) ("abcde", [])
-- PresentT [('a',99),('b',99),('c',99),('d',99),('e',99)]
--
data ZipPad l r p q

instance (PP l a ~ x
        , PP r a ~ y
        , P l a
        , P r a
        , PP p a ~ [x]
        , PP q a ~ [y]
        , P p a
        , P q a
        , Show x
        , Show y
        ) => P (ZipPad l r p q) a where
  type PP (ZipPad l r p q) a = [(PP l a, PP r a)]
  eval _ opts a = do
    let msg0 = "ZipPad"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs = [hh pp, hh qq]
        case chkSize2 opts msg0 p q hhs of
          Left e -> pure e
          Right _ -> do
            let lls = (length p,length q)
            case uncurry compare lls of
              LT -> do
                ll <- eval (Proxy @l) opts a
                pure $ case getValueLR opts (msg0 <> " l failed") ll hhs of
                  Left e -> e
                  Right l ->
                    let d = zip (p ++ repeat l) q
                    in mkNode opts (PresentT d) (show01' opts (msg0 <> " Left pad") d "p=" p <> showVerbose opts " | q=" q) (hhs ++ [hh ll])
              GT -> do
                rr <- eval (Proxy @r) opts a
                pure $ case getValueLR opts (msg0 <> " r failed") rr hhs of
                  Left e -> e
                  Right r ->
                    let d =zip p (q ++ repeat r)
                    in mkNode opts (PresentT d) (show01' opts (msg0 <> " Right pad") d "p=" p <> showVerbose opts " | q=" q) (hhs ++ [hh rr])
              EQ ->
                let d = zip p q
                in pure $ mkNode opts (PresentT d) (show01' opts (msg0 <> " No pad") d "p=" p <> showVerbose opts " | q=" q) hhs


-- | zip two lists optionally padding the left hand side
--
-- >>> pl @(ZipL 99 '[1,2,3] "abc") ()
-- Present [(1,'a'),(2,'b'),(3,'c')] (ZipL [(1,'a'),(2,'b'),(3,'c')] | p=[1,2,3] | q="abc")
-- PresentT [(1,'a'),(2,'b'),(3,'c')]
--
-- >>> pl @(ZipL 99 '[1,2] "abc") ()
-- Present [(1,'a'),(2,'b'),(99,'c')] (ZipL [(1,'a'),(2,'b'),(99,'c')] | p=[1,2] | q="abc")
-- PresentT [(1,'a'),(2,'b'),(99,'c')]
--
-- >>> pl @(ZipL 99 '[1] "abc") ()
-- Present [(1,'a'),(99,'b'),(99,'c')] (ZipL [(1,'a'),(99,'b'),(99,'c')] | p=[1] | q="abc")
-- PresentT [(1,'a'),(99,'b'),(99,'c')]
--
-- >>> pl @(ZipL 99 '[1,2,3] "ab") ()
-- Error ZipL(3,2) rhs would be truncated (p=[1,2,3] | q="ab")
-- FailT "ZipL(3,2) rhs would be truncated"
--
-- >>> pl @(ZipL 99 Id "abcdefg") [1..4]
-- Present [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(99,'e'),(99,'f'),(99,'g')] (ZipL [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(99,'e'),(99,'f'),(99,'g')] | p=[1,2,3,4] | q="abcdefg")
-- PresentT [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(99,'e'),(99,'f'),(99,'g')]
--
-- >>> pl @(ZipL (99 % 4) '[1 % 1 , 2 % 1 , 3 % 1] Id) "abcde"
-- Present [(1 % 1,'a'),(2 % 1,'b'),(3 % 1,'c'),(99 % 4,'d'),(99 % 4,'e')] (ZipL [(1 % 1,'a'),(2 % 1,'b'),(3 % 1,'c'),(99 % 4,'d'),(99 % 4,'e')] | p=[1 % 1,2 % 1,3 % 1] | q="abcde")
-- PresentT [(1 % 1,'a'),(2 % 1,'b'),(3 % 1,'c'),(99 % 4,'d'),(99 % 4,'e')]
--
-- >>> pl @(ZipL "X" (EmptyT _) Id) ("abcd" :: String)
-- Present [("X",'a'),("X",'b'),("X",'c'),("X",'d')] (ZipL [("X",'a'),("X",'b'),("X",'c'),("X",'d')] | p=[] | q="abcd")
-- PresentT [("X",'a'),("X",'b'),("X",'c'),("X",'d')]
--

data ZipL l p q
instance (PP l a ~ x
        , P l a
        , PP p a ~ [x]
        , PP q a ~ [y]
        , P p a
        , P q a
        , Show x
        , Show y
        ) => P (ZipL l p q) a where
  type PP (ZipL l p q) a = [(ExtractAFromList (PP p a), ExtractAFromList (PP q a))]
  eval _ opts a = do
    let msg0 = "ZipL"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs = [hh pp, hh qq]
        case chkSize2 opts msg0 p q hhs of
          Left e -> pure e
          Right _ -> do
            let lls = (length p,length q)
            case uncurry compare lls of
              GT -> let msg1 = msg0 ++ show lls
                    in pure $ mkNode opts (FailT (msg1 ++ " rhs would be truncated")) (showVerbose opts "p=" p <> showVerbose opts " | q=" q) hhs
              _ -> do
                     ll <- eval (Proxy @l) opts a
                     pure $ case getValueLR opts (msg0 <> " l failed") ll hhs of
                             Left e -> e
                             Right l ->
                               let d = zip (p ++ repeat l) q
                               in mkNode opts (PresentT d) (show01' opts msg0 d "p=" p <> showVerbose opts " | q=" q) (hhs ++ [hh ll])

-- | zip two lists optionally padding the right hand side
--
-- >>> pl @(ZipR (Char1 "Z") '[1,2,3] "abc") ()
-- Present [(1,'a'),(2,'b'),(3,'c')] (ZipR [(1,'a'),(2,'b'),(3,'c')] | p=[1,2,3] | q="abc")
-- PresentT [(1,'a'),(2,'b'),(3,'c')]
--
-- >>> pl @(ZipR (Char1 "Z") '[1,2,3] "ab") ()
-- Present [(1,'a'),(2,'b'),(3,'Z')] (ZipR [(1,'a'),(2,'b'),(3,'Z')] | p=[1,2,3] | q="ab")
-- PresentT [(1,'a'),(2,'b'),(3,'Z')]
--
-- >>> pl @(ZipR (Char1 "Z") '[1,2,3] "a") ()
-- Present [(1,'a'),(2,'Z'),(3,'Z')] (ZipR [(1,'a'),(2,'Z'),(3,'Z')] | p=[1,2,3] | q="a")
-- PresentT [(1,'a'),(2,'Z'),(3,'Z')]
--
-- >>> pl @(ZipR (Char1 "Z") '[1,2] "abc") ()
-- Error ZipR(2,3) rhs would be truncated (p=[1,2] | q="abc")
-- FailT "ZipR(2,3) rhs would be truncated"
--
-- >>> pl @(ZipR (Char1 "Y") (EmptyT _) Id) "abcd"
-- Error ZipR(0,4) rhs would be truncated (p=[] | q="abcd")
-- FailT "ZipR(0,4) rhs would be truncated"
--

data ZipR r p q
instance (PP r a ~ y
        , P r a
        , PP p a ~ [x]
        , PP q a ~ [y]
        , P p a
        , P q a
        , Show x
        , Show y
        ) => P (ZipR r p q) a where
  type PP (ZipR r p q) a = [(ExtractAFromList (PP p a), ExtractAFromList (PP q a))]
  eval _ opts a = do
    let msg0 = "ZipR"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs = [hh pp, hh qq]
        case chkSize2 opts msg0 p q hhs of
          Left e -> pure e
          Right _ -> do
            let lls = (length p,length q)
            case uncurry compare lls of
              LT -> let msg1 = msg0 ++ show lls
                    in pure $ mkNode opts (FailT (msg1 ++ " rhs would be truncated")) (showVerbose opts "p=" p <> showVerbose opts " | q=" q) hhs
              _ -> do
                     rr <- eval (Proxy @r) opts a
                     pure $ case getValueLR opts (msg0 <> " l failed") rr hhs of
                             Left e -> e
                             Right r ->
                               let d = zip p (q ++ repeat r)
                               in mkNode opts (PresentT d) (show01' opts msg0 d "p=" p <> showVerbose opts " | q=" q) (hhs ++ [hh rr])

-- | zip two lists with the same length
--
-- >>> pl @(Zip '[1,2,3] "abc") ()
-- Present [(1,'a'),(2,'b'),(3,'c')] (Zip [(1,'a'),(2,'b'),(3,'c')] | p=[1,2,3] | q="abc")
-- PresentT [(1,'a'),(2,'b'),(3,'c')]
--
-- >>> pl @(Zip '[1,2,3] "ab") ()
-- Error Zip(3,2) length mismatch (p=[1,2,3] | q="ab")
-- FailT "Zip(3,2) length mismatch"
--
-- >>> pl @(Zip '[1,2] "abc") ()
-- Error Zip(2,3) length mismatch (p=[1,2] | q="abc")
-- FailT "Zip(2,3) length mismatch"
--
-- >>> pl @(Zip "abc" Id) [1..7]
-- Error Zip(3,7) length mismatch (p="abc" | q=[1,2,3,4,5,6,7])
-- FailT "Zip(3,7) length mismatch"
--
data Zip p q
instance (PP p a ~ [x]
        , PP q a ~ [y]
        , P p a
        , P q a
        , Show x
        , Show y
        ) => P (Zip p q) a where
  type PP (Zip p q) a = [(ExtractAFromList (PP p a), ExtractAFromList (PP q a))]
  eval _ opts a = do
    let msg0 = "Zip"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case chkSize2 opts msg0 p q hhs of
          Left e -> e
          Right _ ->
            let lls = (length p, length q)
            in case uncurry compare lls of
                 EQ -> let d = zip p q
                       in mkNode opts (PresentT d) (show01' opts msg0 d "p=" p <> showVerbose opts " | q=" q) hhs
                 _ -> let msg1 = msg0 ++ show lls
                      in mkNode opts (FailT (msg1 <> " length mismatch")) (showVerbose opts "p=" p <> showVerbose opts " | q=" q) hhs

-- | similar to 'empty'
--
-- >>> pz @(EmptyT Maybe) ()
-- PresentT Nothing
--
-- >>> pz @(EmptyT []) ()
-- PresentT []
--
-- >>> pz @(Char1 "x" >> EmptyT []) (13,True)
-- PresentT ""
--
-- >>> pz @(Fst >> EmptyT (Either String)) (13,True)
-- PresentT (Left "")
--
data EmptyT (t :: Type -> Type)

instance Alternative t => P (EmptyT t) x where
  type PP (EmptyT t) x = t x
  eval _ opts _x =
    let msg0 = "EmptyT"
        b = empty @t
    in pure $ mkNode opts (PresentT b) msg0 []


-- | similar to 'sum'
--
-- >>> pz @Sum [10,4,5,12,3,4]
-- PresentT 38
--
-- >>> pz @Sum []
-- PresentT 0
--
data Sum

instance ( Num a
         , Show a
         ) => P Sum [a] where
  type PP Sum [a] = a
  eval _ opts as =
    let msg0 = "Sum"
        v = sum as
    in pure $ mkNode opts (PresentT v) (show01 opts msg0 v as) []

-- | similar to 'product'
--
-- >>> pz @Product [10,4,5,12,3,4]
-- PresentT 28800
--
-- >>> pz @Product []
-- PresentT 1
--
data Product

instance ( Num a
         , Show a
         ) => P Product [a] where
  type PP Product [a] = a
  eval _ opts as =
    let msg0 = "Product"
        v = product as
    in pure $ mkNode opts (PresentT v) (show01 opts msg0 v as) []

-- | similar to 'minimum'
--
-- >>> pz @Min [10,4,5,12,3,4]
-- PresentT 3
--
-- >>> pz @Min []
-- FailT "empty list"
--
data Min

instance ( Ord a
         , Show a
         ) => P Min [a] where
  type PP Min [a] = a
  eval _ opts as' = do
    let msg0 = "Min"
    pure $ case as' of
     [] -> mkNode opts (FailT "empty list") msg0 []
     as@(_:_) ->
       let v = minimum as
       in mkNode opts (PresentT v) (show01 opts msg0 v as) []

-- | similar to 'maximum'
--
-- >>> pz @Max [10,4,5,12,3,4]
-- PresentT 12
--
-- >>> pz @Max []
-- FailT "empty list"
--

data Max

instance ( Ord a
         , Show a
         ) => P Max [a] where
  type PP Max [a] = a
  eval _ opts as' = do
    let msg0 = "Max"
    pure $ case as' of
      [] -> mkNode opts (FailT "empty list") msg0 []
      as@(_:_) ->
        let v = maximum as
        in mkNode opts (PresentT v) (show01 opts msg0 v as) []

data IsFixImpl (cmp :: Ordering) p q

instance ( P p x
         , P q x
         , Show a
         , Eq a
         , PP p x ~ [a]
         , PP q x ~ [a]
         , GetOrdering cmp
         ) => P (IsFixImpl cmp p q) x where
  type PP (IsFixImpl cmp p q) x = Bool
  eval _ opts x = do
    let cmp = getOrdering @cmp
        (ff,msg0) = case cmp of
                    LT -> (isPrefixOf, "IsPrefix")
                    EQ -> (isInfixOf, "IsInfix")
                    GT -> (isSuffixOf, "IsSuffix")
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
        Left e -> pure e
        Right s0 -> do
          let msg1 = msg0 <> " | " <> showL opts s0
          qq <- eval (Proxy @q) opts x
          pure $ case getValueLR opts (msg1 <> " q failed") qq [hh pp] of
            Left e -> e
            Right s1 -> mkNodeB opts (ff s0 s1) (msg1 <> " " <> showL opts s1) [hh pp, hh qq]

-- | similar to 'isPrefixOf'
--
-- >>> pl @(IsPrefix '[2,3] Id) [2,3,4]
-- True (IsPrefix | [2,3] [2,3,4])
-- TrueT
--
-- >>> pl @(IsPrefix '[2,3] Id) [1,2,3]
-- False (IsPrefix | [2,3] [1,2,3])
-- FalseT
--
data IsPrefix p q
type IsPrefixT p q = IsFixImpl 'LT p q

instance P (IsPrefixT p q) x => P (IsPrefix p q) x where
  type PP (IsPrefix p q) x = PP (IsPrefixT p q) x
  eval _ = evalBool (Proxy @(IsPrefixT p q))

-- | similar to 'isInfixOf'
--
-- >>> pl @(IsInfix '[2,3] Id) [1,2,3]
-- True (IsInfix | [2,3] [1,2,3])
-- TrueT
--
-- >>> pl @(IsInfix '[2,3] Id) [1,2,1,3]
-- False (IsInfix | [2,3] [1,2,1,3])
-- FalseT
--
data IsInfix p q
type IsInfixT p q = IsFixImpl 'EQ p q

instance P (IsInfixT p q) x => P (IsInfix p q) x where
  type PP (IsInfix p q) x = PP (IsInfixT p q) x
  eval _ = evalBool (Proxy @(IsInfixT p q))

-- | similar to 'isSuffixOf'
--
-- >>> pl @(IsSuffix '[2,3] Id) [1,2,3]
-- True (IsSuffix | [2,3] [1,2,3])
-- TrueT
--
-- >>> pl @(IsSuffix '[2,3] Id) [2,3,4]
-- False (IsSuffix | [2,3] [2,3,4])
-- FalseT
--
data IsSuffix p q
type IsSuffixT p q = IsFixImpl 'GT p q

instance P (IsSuffixT p q) x => P (IsSuffix p q) x where
  type PP (IsSuffix p q) x = PP (IsSuffixT p q) x
  eval _ = evalBool (Proxy @(IsSuffixT p q))

-- | similar to 'nub'
--
-- >>> pz @Nub "abcdbc"
-- PresentT "abcd"
--
-- >>> pz @Nub []
-- PresentT []
--
-- >>> pz @Nub [1,4,1,1,1,1,1]
-- PresentT [1,4]
--
data Nub

instance ( x ~ [a]
         , Show a
         , Ord a
         ) => P Nub x where
  type PP Nub x = x
  eval _ opts x =
    let msg0 = "Nub"
        ret = nubOrd x
    in pure $ mkNode opts (PresentT ret) (show01 opts msg0 ret x) []

