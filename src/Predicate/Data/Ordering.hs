{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# OPTIONS -Wunused-type-patterns #-}
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
import Predicate.Misc
import Predicate.Util
import Predicate.Data.Tuple (Pairs)
import Data.Proxy (Proxy(Proxy))
import Data.Char (toLower)
import Data.Function (on)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate.Prelude

-- | compare if expression @p@ is greater than @q@
--
-- >>> pl @(Gt 4) 5
-- True (5 > 4)
-- Val True
--
type Gt n = Id > n
type Ge n = Id >= n
type Same n = Id == n
type Le n = Id <= n
type Lt n = Id < n
type Ne n = Id /= n

-- | compare if expression @p@ is greater than @q@
--
-- >>> pl @(Id > "xx") "abc"
-- False ("abc" > "xx")
-- Val False
--
-- >>> pl @(Id > "aa") "abc"
-- True ("abc" > "aa")
-- Val True
--
-- >>> pl @(Fst > Snd) (True,False)
-- True (True > False)
-- Val True
--
data p > q
infix 4 >

instance P (Cmp 'CGt p q) x => P (p > q) x where
  type PP (p > q) x = Bool
  eval _ = evalBool (Proxy @(Cmp 'CGt p q))

-- | compare if expression @p@ is greater than or equal to @q@
data p >= q
infix 4 >=

instance P (Cmp 'CGe p q) x => P (p >= q) x where
  type PP (p >= q) x = Bool
  eval _ = evalBool (Proxy @(Cmp 'CGe p q))

-- | compare if expression @p@ is equal to @q@
--
-- >>> pl @(Fst == Snd) ("ab","xyzabw")
-- False ("ab" == "xyzabw")
-- Val False
--
-- >>> pl @(Fst == Snd) ("aBc","AbC")
-- False ("aBc" == "AbC")
-- Val False
--
-- >>> pz @(Fst == Snd) ("aBc","aBc")
-- Val True
--
-- >>> pl @(Id == "Abc") "abc"
-- False ("abc" == "Abc")
-- Val False
--
-- >>> pl @(Fst == Snd) (True,False)
-- False (True == False)
-- Val False
--
-- >>> pl @(Not Id *** Id >> Fst == Snd) (True,False)
-- True ((>>) True | {False == False})
-- Val True
--
data p == q
infix 4 ==

instance P (Cmp 'CEq p q) x => P (p == q) x where
  type PP (p == q) x = Bool
  eval _ = evalBool (Proxy @(Cmp 'CEq p q))

-- | compare if expression @p@ is less than or equal to @q@
--
-- >>> pl @(Not (Fst >> Len <= 6)) ([2..7],True)
-- False (Not ((>>) True | {6 <= 6}))
-- Val False
--
-- >>> pl @(Fst >> Len <= 6) ([2..7],True)
-- True ((>>) True | {6 <= 6})
-- Val True
--
-- >>> pl @(Length Fst <= 6) ([2..7],True)
-- True (6 <= 6)
-- Val True
--
-- >>> pl @(Fst >> (Len <= 6)) ([2..7],True)
-- True ((>>) True | {6 <= 6})
-- Val True
--
data p <= q
infix 4 <=

instance P (Cmp 'CLe p q) x => P (p <= q) x where
  type PP (p <= q) x = Bool
  eval _ = evalBool (Proxy @(Cmp 'CLe p q))

-- | compare if expression @p@ is less than @q@
data p < q
infix 4 <

instance P (Cmp 'CLt p q) x => P (p < q) x where
  type PP (p < q) x = Bool
  eval _ = evalBool (Proxy @(Cmp 'CLt p q))

-- | compare if expression @p@ is not equal to @q@
--
-- >>> pl @(Fst /= Snd) ("ab","xyzabw")
-- True ("ab" /= "xyzabw")
-- Val True
--
data p /= q
infix 4 /=

instance P (Cmp 'CNe p q) x => P (p /= q) x where
  type PP (p /= q) x = Bool
  eval _ = evalBool (Proxy @(Cmp 'CNe p q))

-- | case-insensitive compare if string expression @p@ is greater than @q@
--
data p >~ q
infix 4 >~

instance P (CmpI 'CGt p q) x => P (p >~ q) x where
  type PP (p >~ q) x = Bool
  eval _ = evalBool (Proxy @(CmpI 'CGt p q))

-- | case-insensitive compare if string expression @p@ is greater than or equal to @q@
data p >=~ q
infix 4 >=~

instance P (CmpI 'CGe p q) x => P (p >=~ q) x where
  type PP (p >=~ q) x = Bool
  eval _ = evalBool (Proxy @(CmpI 'CGe p q))

-- | case-insensitive compare if string expression @p@ is equal to @q@
data p ==~ q
infix 4 ==~

instance P (CmpI 'CEq p q) x => P (p ==~ q) x where
  type PP (p ==~ q) x = Bool
  eval _ = evalBool (Proxy @(CmpI 'CEq p q))

-- | case-insensitive compare if string expression @p@ is less than or equal to @q@
data p <=~ q
infix 4 <=~

instance P (CmpI 'CLe p q) x => P (p <=~ q) x where
  type PP (p <=~ q) x = Bool
  eval _ = evalBool (Proxy @(CmpI 'CLe p q))

-- | case-insensitive compare if string expression @p@ is less than @q@
data p <~ q
infix 4 <~

instance P (CmpI 'CLt p q) x => P (p <~ q) x where
  type PP (p <~ q) x = Bool
  eval _ = evalBool (Proxy @(CmpI 'CLt p q))

-- | case-insensitive compare if string expression @p@ is not equal to @q@
data p /=~ q
infix 4 /=~

instance P (CmpI 'CNe p q) x => P (p /=~ q) x where
  type PP (p /=~ q) x = Bool
  eval _ = evalBool (Proxy @(CmpI 'CNe p q))


-- | similar to 'compare'
--
-- >>> pz @(Fst ==! Snd) (10,9)
-- Val GT
--
-- >>> pz @(14 % 3 ==! Fst -% Snd) (-10,7)
-- Val GT
--
-- >>> pz @(Fst ==! Snd) (10,11)
-- Val LT
--
-- >>> pz @(Snd ==! (L12 >> Head)) (('x',[10,12,13]),10)
-- Val EQ
--
-- >>> pl @("aa" ==! Id) "aaaa"
-- Present LT ((==!) "aa" < "aaaa")
-- Val LT
--
-- >>> pl @(Pairs >> Map (First (Succ >> Succ) >> Fst ==! Snd) Id) [1,2,3,6,8]
-- Present [GT,GT,LT,EQ] ((>>) [GT,GT,LT,EQ] | {Map [GT,GT,LT,EQ] | [(1,2),(2,3),(3,6),(6,8)]})
-- Val [GT,GT,LT,EQ]
--
-- >>> pl @((Ones << ShowP Id) >> Map (Fst ==! Snd) Pairs) 1234223
-- Present [LT,LT,LT,GT,EQ,LT] ((>>) [LT,LT,LT,GT,EQ,LT] | {Map [LT,LT,LT,GT,EQ,LT] | [("1","2"),("2","3"),("3","4"),("4","2"),("2","2"),("2","3")]})
-- Val [LT,LT,LT,GT,EQ,LT]
--
-- >>> pl @("Abc" ==! Id) "abc"
-- Present LT ((==!) "Abc" < "abc")
-- Val LT
--
-- >>> pl @(Fst ==! Snd) (3,12)
-- Present LT ((==!) 3 < 12)
-- Val LT
--
-- >>> pl @(Fst ==! Snd) ("aBc","AbC")
-- Present GT ((==!) "aBc" > "AbC")
-- Val GT
--
-- >>> pl @(Snd ==! Fst) ("aBc","AbC")
-- Present LT ((==!) "AbC" < "aBc")
-- Val LT
--

data p ==! q
infix 4 ==!

type OrdP p q = p ==! q

instance ( Ord (PP p a)
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
        in mkNode opts (Val d) (msg0 <> " " <> showL opts p <> " " <> prettyOrd d <> " " <> showL opts q) [hh pp, hh qq]

-- | similar to 'compare' but using a tuple as input
data OrdA

instance P (OrdA' Id Id) x => P OrdA x where
  type PP OrdA x = PP (OrdA' Id Id) x
  eval _ = eval (Proxy @(OrdA' Id Id))

data OrdA' p q
type OrdAT' p q = (Fst >> p) ==! (Snd >> q)

instance P (OrdAT' p q) x => P (OrdA' p q) x where
  type PP (OrdA' p q) x = PP (OrdAT' p q) x
  eval _ = eval (Proxy @(OrdAT' p q))

-- | compare two strings ignoring case and return an ordering
--
-- >>> pz @(Fst ===~ Snd) ("abC","aBc")
-- Val EQ
--
-- >>> pz @(Fst ===~ Snd) ("abC","DaBc")
-- Val LT
--
-- >>> pl @(Fst ===~ Snd &&& Fst ==! Snd) ("abc","abc")
-- Present (EQ,EQ) ('(EQ,EQ))
-- Val (EQ,EQ)
--
--
-- >>> pl @(Fst ===~ Snd) ("aBc","AbC")
-- Present EQ ((===~) aBc = AbC)
-- Val EQ
--
-- >>> pl @("Abc" ===~ Id) "abc"
-- Present EQ ((===~) Abc = abc)
-- Val EQ
--
--
-- >>> pl @("Abc" ==~ Id) "abc"
-- True (Abc ==~ abc)
-- Val True
--
-- >>> pl @(Fst ==~ Snd) ("aBc","AbC")
-- True (aBc ==~ AbC)
-- Val True
--
-- >>> pl @(Fst ==~ Snd && Fst == Snd) ("Abc","Abc")
-- True (True && True)
-- Val True
--

type OrdI p q = p ===~ q
data p ===~ q
infix 4 ===~

instance ( PP p a ~ String
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
        in mkNode opts (Val d) (msg0 <> " " <> p <> " " <> prettyOrd d <> " " <> q) [hh pp, hh qq]

-- | compare two values using the given ordering @o@
--
-- >>> pl @(Lt 4) 123
-- False (123 < 4)
-- Val False
--
-- >>> pl @(Lt 4) 1
-- True (1 < 4)
-- Val True
--
-- >>> pl @(Negate 7 <..> 20) (-4)
-- True (-7 <= -4 <= 20)
-- Val True
--
-- >>> pl @(Negate 7 <..> 20) 21
-- False (21 <= 20)
-- Val False
--
data Cmp (o :: OrderingP) p q

instance ( GetOrd o
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

-- | compare two strings ignoring case using the given ordering @o@
data CmpI (o :: OrderingP) p q

instance ( PP p a ~ String
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
-- True ((>>) True | {All(6)})
-- Val True
--
-- >>> pz @Asc [1,2,3,4,5,5,7]
-- Val True
--
-- >>> pz @Asc "axacdef"
-- Val False
--
data Asc
type AscT = Pairs >> All (Fst <= Snd)

instance P AscT x => P Asc x where
  type PP Asc x = PP AscT x
  eval _ = evalBool (Proxy @AscT)

-- | a type level predicate for a strictly increasing list
--
-- >>> pz @Asc' [1,2,3,4,5,5,7]
-- Val False
--
-- >>> pz @Asc' []
-- Val True
--
-- >>> pz @Asc' [-10]
-- Val True
--
data Asc'
type AscT' = Pairs >> All (Fst < Snd)

instance P AscT' x => P Asc' x where
  type PP Asc' x = PP AscT' x
  eval _ = evalBool (Proxy @AscT')

-- | a type level predicate for a monotonic decreasing list
data Desc
type DescT = Pairs >> All (Fst >= Snd)

instance P DescT x => P Desc x where
  type PP Desc x = PP DescT x
  eval _ = evalBool (Proxy @DescT)
-- | a type level predicate for a strictly decreasing list
data Desc'
type DescT' = Pairs >> All (Fst > Snd)

instance P DescT' x => P Desc' x where
  type PP Desc' x = PP DescT' x
  eval _ = evalBool (Proxy @DescT')


--type AscAlt = SortOn Id Id == Id
--type DescAlt = SortOnDesc Id Id == Id

-- | a type level predicate for all positive elements in a list
--
-- >>> pz @AllPositive [1,5,10,2,3]
-- Val True
--
-- >>> pz @AllPositive [0,1,5,10,2,3]
-- Val False
--
-- >>> pz @AllPositive [3,1,-5,10,2,3]
-- Val False
--
data AllPositive
type AllPositiveT = All Positive

instance P AllPositiveT x => P AllPositive x where
  type PP AllPositive x = PP AllPositiveT x
  eval _ = evalBool (Proxy @AllPositiveT)

-- | a type level predicate for all negative elements in a list
--
-- >>> pz @AllNegative [-1,-5,-10,-2,-3]
-- Val True
--
data AllNegative
type AllNegativeT = All Negative

instance P AllNegativeT x => P AllNegative x where
  type PP AllNegative x = PP AllNegativeT x
  eval _ = evalBool (Proxy @AllNegativeT)


type Positive = Gt 0

type Negative = Lt 0

