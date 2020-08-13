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
     promoted 'Ordering' functions
-}
module Predicate.Data.Ordering (

 -- ** compare expressions
    type (>)
  , type (>=)
  , type (==)
  , type (/=)
  , type (<=)
  , type (<)
  , type (>~)
  , type (>=~)
  , type (==~)
  , type (/=~)
  , type (<=~)
  , type (<~)
  , Gt
  , Ge
  , Same
  , Le
  , Lt
  , Ne
  , type (==!)
  , OrdP
  , OrdA'
  , OrdA
  , OrdI
  , type (===~)
  , Cmp
  , CmpI

  , Asc
  , Asc'
  , Desc
  , Desc'
  , AllPositive
  , Positive
  , AllNegative
  , Negative

 ) where
import Predicate.Core
import Predicate.Util
import Predicate.Data.Tuple (Pairs)
import Data.Proxy
import Data.Char
import Data.Function

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate.Prelude

-- | compare if expression \'p\' is greater than \'q\'
--
-- >>> pl @(Gt 4) 5
-- True (5 > 4)
-- TrueT
--
type Gt n = I > n
type Ge n = I >= n
type Same n = I == n
type Le n = I <= n
type Lt n = I < n
type Ne n = I /= n

-- | compare if expression \'p\' is greater than \'q\'
--
-- >>> pl @(Id > "xx") "abc"
-- False ("abc" > "xx")
-- FalseT
--
-- >>> pl @(Id > "aa") "abc"
-- True ("abc" > "aa")
-- TrueT
--
-- >>> pl @(Fst Id > Snd Id) (True,False)
-- True (True > False)
-- TrueT
--
data p > q
infix 4 >

instance P (Cmp 'CGt p q) x => P (p > q) x where
  type PP (p > q) x = Bool
  eval _ = evalBool (Proxy @(Cmp 'CGt p q))

-- | compare if expression \'p\' is greater than or equal to \'q\'
data p >= q
infix 4 >=

instance P (Cmp 'CGe p q) x => P (p >= q) x where
  type PP (p >= q) x = Bool
  eval _ = evalBool (Proxy @(Cmp 'CGe p q))

-- | compare if expression \'p\' is equal to \'q\'
--
-- >>> pl @(Fst Id == Snd Id) ("ab","xyzabw")
-- False ("ab" == "xyzabw")
-- FalseT
--
-- >>> pl @(Fst Id == Snd Id) ("aBc","AbC")
-- False ("aBc" == "AbC")
-- FalseT
--
-- >>> pz @(Fst Id == Snd Id) ("aBc","aBc")
-- TrueT
--
-- >>> pl @(Id == "Abc") "abc"
-- False ("abc" == "Abc")
-- FalseT
--
-- >>> pl @(Fst Id == Snd Id) (True,False)
-- False (True == False)
-- FalseT
--
-- >>> pl @(Not Id *** Id >> Fst Id == Snd Id) (True,False)
-- True ((>>) True | {False == False})
-- TrueT
--
data p == q
infix 4 ==

instance P (Cmp 'CEq p q) x => P (p == q) x where
  type PP (p == q) x = Bool
  eval _ = evalBool (Proxy @(Cmp 'CEq p q))

-- | compare if expression \'p\' is less than or equal to \'q\'
--
-- >>> pl @(Not (Fst Id >> Len <= 6)) ([2..7],True)
-- False (Not ((>>) True | {6 <= 6}))
-- FalseT
--
-- >>> pl @(Fst Id >> Len <= 6) ([2..7],True)
-- True ((>>) True | {6 <= 6})
-- TrueT
--
-- >>> pl @(Length (Fst Id) <= 6) ([2..7],True)
-- True (6 <= 6)
-- TrueT
--
-- >>> pl @(Fst Id >> (Len <= 6)) ([2..7],True)
-- True ((>>) True | {6 <= 6})
-- TrueT
--
data p <= q
infix 4 <=

instance P (Cmp 'CLe p q) x => P (p <= q) x where
  type PP (p <= q) x = Bool
  eval _ = evalBool (Proxy @(Cmp 'CLe p q))

-- | compare if expression \'p\' is less than \'q\'
data p < q
infix 4 <

instance P (Cmp 'CLt p q) x => P (p < q) x where
  type PP (p < q) x = Bool
  eval _ = evalBool (Proxy @(Cmp 'CLt p q))

-- | compare if expression \'p\' is not equal to \'q\'
--
-- >>> pl @(Fst Id /= Snd Id) ("ab","xyzabw")
-- True ("ab" /= "xyzabw")
-- TrueT
--
data p /= q
infix 4 /=

instance P (Cmp 'CNe p q) x => P (p /= q) x where
  type PP (p /= q) x = Bool
  eval _ = evalBool (Proxy @(Cmp 'CNe p q))

-- | case-insensitive compare if string expression \'p\' is greater than \'q\'
--
data p >~ q
infix 4 >~

instance P (CmpI 'CGt p q) x => P (p >~ q) x where
  type PP (p >~ q) x = Bool
  eval _ = evalBool (Proxy @(CmpI 'CGt p q))

-- | case-insensitive compare if string expression \'p\' is greater than or equal to \'q\'
data p >=~ q
infix 4 >=~

instance P (CmpI 'CGe p q) x => P (p >=~ q) x where
  type PP (p >=~ q) x = Bool
  eval _ = evalBool (Proxy @(CmpI 'CGe p q))

-- | case-insensitive compare if string expression \'p\' is equal to \'q\'
data p ==~ q
infix 4 ==~

instance P (CmpI 'CEq p q) x => P (p ==~ q) x where
  type PP (p ==~ q) x = Bool
  eval _ = evalBool (Proxy @(CmpI 'CEq p q))

-- | case-insensitive compare if string expression \'p\' is less than or equal to \'q\'
data p <=~ q
infix 4 <=~

instance P (CmpI 'CLe p q) x => P (p <=~ q) x where
  type PP (p <=~ q) x = Bool
  eval _ = evalBool (Proxy @(CmpI 'CLe p q))

-- | case-insensitive compare if string expression \'p\' is less than \'q\'
data p <~ q
infix 4 <~

instance P (CmpI 'CLt p q) x => P (p <~ q) x where
  type PP (p <~ q) x = Bool
  eval _ = evalBool (Proxy @(CmpI 'CLt p q))

-- | case-insensitive compare if string expression \'p\' is not equal to \'q\'
data p /=~ q
infix 4 /=~

instance P (CmpI 'CNe p q) x => P (p /=~ q) x where
  type PP (p /=~ q) x = Bool
  eval _ = evalBool (Proxy @(CmpI 'CNe p q))


-- | similar to 'compare'
--
-- >>> pz @(Fst Id ==! Snd Id) (10,9)
-- PresentT GT
--
-- >>> pz @(14 % 3 ==! Fst Id -% Snd Id) (-10,7)
-- PresentT GT
--
-- >>> pz @(Fst Id ==! Snd Id) (10,11)
-- PresentT LT
--
-- >>> pz @(Snd Id ==! (Fst Id >> Snd Id >> Head Id)) (('x',[10,12,13]),10)
-- PresentT EQ
--
-- >>> pz @(Snd Id ==! Head (Snd (Fst Id))) (('x',[10,12,13]),10)
-- PresentT EQ
--
-- >>> pl @("aa" ==! Id) "aaaa"
-- Present LT ((==!) "aa" < "aaaa")
-- PresentT LT
--
-- >>> pl @(Pairs >> Map (First (Succ Id >> Succ Id) >> Fst Id ==! Snd Id) Id) [1,2,3,6,8]
-- Present [GT,GT,LT,EQ] ((>>) [GT,GT,LT,EQ] | {Map [GT,GT,LT,EQ] | [(1,2),(2,3),(3,6),(6,8)]})
-- PresentT [GT,GT,LT,EQ]
--
-- >>> pl @((Ones Id << ShowP Id) >> Map (Fst Id ==! Snd Id) Pairs) 1234223
-- Present [LT,LT,LT,GT,EQ,LT] ((>>) [LT,LT,LT,GT,EQ,LT] | {Map [LT,LT,LT,GT,EQ,LT] | [("1","2"),("2","3"),("3","4"),("4","2"),("2","2"),("2","3")]})
-- PresentT [LT,LT,LT,GT,EQ,LT]
--
-- >>> pl @("Abc" ==! Id) "abc"
-- Present LT ((==!) "Abc" < "abc")
-- PresentT LT
--
-- >>> pl @(Fst Id ==! Snd Id) (3,12)
-- Present LT ((==!) 3 < 12)
-- PresentT LT
--
-- >>> pl @(Fst Id ==! Snd Id) ("aBc","AbC")
-- Present GT ((==!) "aBc" > "AbC")
-- PresentT GT
--
-- >>> pl @(Snd Id ==! Fst Id) ("aBc","AbC")
-- Present LT ((==!) "AbC" < "aBc")
-- PresentT LT
--

data p ==! q
infix 4 ==!

type OrdP p q = p ==! q

instance (Ord (PP p a)
        , PP p a ~ PP q a
        , P p a
        , Show (PP q a)
        , P q a
        ) => P (p ==! q) a where
  type PP (p ==! q) a = Ordering
  eval _ opts a = do
    let msg0 = "(==!)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = compare p q
        in mkNode opts (PresentT d) (msg0 <> " " <> showL opts p <> " " <> prettyOrd d <> " " <> showL opts q) [hh pp, hh qq]

-- | similar to 'compare' but using a tuple as input
data OrdA p

instance P (OrdA' p p) x => P (OrdA p) x where
  type PP (OrdA p) x = PP (OrdA' p p) x
  eval _ = eval (Proxy @(OrdA' p p))

data OrdA' p q
type OrdAT' p q = (Fst Id >> p) ==! (Snd Id >> q)

instance P (OrdAT' p q) x => P (OrdA' p q) x where
  type PP (OrdA' p q) x = PP (OrdAT' p q) x
  eval _ = eval (Proxy @(OrdAT' p q))

-- | compare two strings ignoring case and return an ordering
--
-- >>> pz @(Fst Id ===~ Snd Id) ("abC","aBc")
-- PresentT EQ
--
-- >>> pz @(Fst Id ===~ Snd Id) ("abC","DaBc")
-- PresentT LT
--
-- >>> pl @(Fst Id ===~ Snd Id &&& Fst Id ==! Snd Id) ("abc","abc")
-- Present (EQ,EQ) (W '(EQ,EQ))
-- PresentT (EQ,EQ)
--
--
-- >>> pl @(Fst Id ===~ Snd Id) ("aBc","AbC")
-- Present EQ ((===~) aBc = AbC)
-- PresentT EQ
--
-- >>> pl @("Abc" ===~ Id) "abc"
-- Present EQ ((===~) Abc = abc)
-- PresentT EQ
--
--
-- >>> pl @("Abc" ==~ Id) "abc"
-- True (Abc ==~ abc)
-- TrueT
--
-- >>> pl @(Fst Id ==~ Snd Id) ("aBc","AbC")
-- True (aBc ==~ AbC)
-- TrueT
--
-- >>> pl @(Fst Id ==~ Snd Id && Fst Id == Snd Id) ("Abc","Abc")
-- True (True && True)
-- TrueT
--

type OrdI p q = p ===~ q
data p ===~ q
infix 4 ===~

instance (PP p a ~ String
        , PP p a ~ PP q a
        , P p a
        , P q a
        ) => P (p ===~ q) a where
  type PP (p ===~ q) a = Ordering
  eval _ opts a = do
    let msg0 = "(===~)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = on compare (map toLower) p q
        in mkNode opts (PresentT d) (msg0 <> " " <> p <> " " <> prettyOrd d <> " " <> q) [hh pp, hh qq]

-- | compare two values using the given ordering \'o\'
--
-- >>> pl @(Lt 4) 123
-- False (123 < 4)
-- FalseT
--
-- >>> pl @(Lt 4) 1
-- True (1 < 4)
-- TrueT
--
-- >>> pl @(Negate 7 <..> 20) (-4)
-- True (-7 <= -4 <= 20)
-- TrueT
--
-- >>> pl @(Negate 7 <..> 20) 21
-- False (21 <= 20)
-- FalseT
--
data Cmp (o :: OrderingP) p q

instance (GetOrd o
        , Ord (PP p a)
        , Show (PP p a)
        , PP p a ~ PP q a
        , P p a
        , P q a
        ) => P (Cmp o p q) a where
  type PP (Cmp o p q) a = Bool
  eval _ opts a = do
    let (sfn, fn) = getOrd @o
    lr <- runPQ sfn (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = fn p q
        in mkNodeB opts b (showL opts p <> " " <> sfn <> " " <> showL opts q) [hh pp, hh qq]

-- | compare two strings ignoring case using the given ordering \'o\'
data CmpI (o :: OrderingP) p q

instance (PP p a ~ String
        , GetOrd o
        , PP p a ~ PP q a
        , P p a
        , P q a
        ) => P (CmpI o p q) a where
  type PP (CmpI o p q) a = Bool
  eval _ opts a = do
    let (sfn, fn) = getOrd @o
    lr <- runPQ sfn (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = on fn (map toLower) p q
        in mkNodeB opts b (p <> " " <> sfn <> "~ " <> q) [hh pp, hh qq]


-- | a type level predicate for a monotonic increasing list
--
-- >>> pl @Asc "aaacdef"
-- True (All(6))
-- TrueT
--
-- >>> pz @Asc [1,2,3,4,5,5,7]
-- TrueT
--
-- >>> pz @Asc "axacdef"
-- FalseT
--
data Asc
type AscT = All (Fst Id <= Snd Id) Pairs

instance P AscT x => P Asc x where
  type PP Asc x = PP AscT x
  eval _ = evalBool (Proxy @AscT)

-- | a type level predicate for a strictly increasing list
--
-- >>> pz @Asc' [1,2,3,4,5,5,7]
-- FalseT
--
data Asc'
type AscT' = All (Fst Id < Snd Id) Pairs

instance P AscT' x => P Asc' x where
  type PP Asc' x = PP AscT' x
  eval _ = evalBool (Proxy @AscT')

-- | a type level predicate for a monotonic decreasing list
data Desc
type DescT = All (Fst Id >= Snd Id) Pairs

instance P DescT x => P Desc x where
  type PP Desc x = PP DescT x
  eval _ = evalBool (Proxy @DescT)
-- | a type level predicate for a strictly decreasing list
data Desc'
type DescT' = All (Fst Id > Snd Id) Pairs

instance P DescT' x => P Desc' x where
  type PP Desc' x = PP DescT' x
  eval _ = evalBool (Proxy @DescT')


--type AscAlt = SortOn Id Id == Id
--type DescAlt = SortOnDesc Id Id == Id

-- | a type level predicate for all positive elements in a list
--
-- >>> pz @AllPositive [1,5,10,2,3]
-- TrueT
--
-- >>> pz @AllPositive [0,1,5,10,2,3]
-- FalseT
--
-- >>> pz @AllPositive [3,1,-5,10,2,3]
-- FalseT
--
data AllPositive
type AllPositiveT = All Positive Id

instance P AllPositiveT x => P AllPositive x where
  type PP AllPositive x = PP AllPositiveT x
  eval _ = evalBool (Proxy @AllPositiveT)

-- | a type level predicate for all negative elements in a list
--
-- >>> pz @AllNegative [-1,-5,-10,-2,-3]
-- TrueT
--
data AllNegative
type AllNegativeT = All Negative Id

instance P AllNegativeT x => P AllNegative x where
  type PP AllNegative x = PP AllNegativeT x
  eval _ = evalBool (Proxy @AllNegativeT)


type Positive = Gt 0

type Negative = Lt 0

