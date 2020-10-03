{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
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
     extra promoted functions
-}
module Predicate.Data.Extra (

    Pure2
  , type (<$)
  , type (<*)
  , type (*>)
  , Sequence
  , Traverse
  , Join
  , type (<|>)
  , Extract
  , Duplicate

  , type ($$)
  , type ($&)
  , Skip
  , type (|>)
  , type (>|)
  , type (>|>)

  , HeadDef
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

  , Coerce2

  , ProxyT
  , ProxyT'

  , IsPrime
  , PrimeNext
  , PrimePrev
  , PrimeFactors
  , Primes
  , IsLuhn

  , Catch
  , Catch'
  , Dot
  , RDot
  , K
  , Lift
  , Apply1
  , Apply2

  , PartitionsBy
  , IMap
  , IList
  , FMap
  , type (<$>)
  , FPair
  , FFish
  , type (>>=)
  , type (<:>)

 ) where
import Predicate.Core
import Predicate.Util
import Predicate.Data.List (Head, Uncons, Unsnoc, SortBy, Zip, GroupBy, SortOn, ZipWith)
import Predicate.Data.Enum (type (...))
import Predicate.Data.Numeric (type (-))
import Predicate.Data.Maybe (JustDef, JustFail)
import GHC.TypeLits (ErrorMessage((:$$:),(:<>:)))
import qualified GHC.TypeLits as GL
import Data.Proxy (Proxy(..))
import Control.Applicative (liftA2, Alternative((<|>)))
import Control.Monad (join)
import Data.Kind (Type)
import Control.Comonad (Comonad(duplicate, extract))
import Data.Coerce (Coercible)
import Control.Lens
import qualified Safe (headNote, cycleNote)
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> import qualified Data.Text as T
-- >>> import qualified Data.Sequence as Seq
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG
-- >>> import Data.Functor.Identity
-- >>> import Data.These

-- | lift pure over a Functor
--
-- >>> pz @(Pure2 (Either String)) [1,2,4]
-- PresentT [Right 1,Right 2,Right 4]
--
-- >>> pl @(Pure2 []) (Just 10)
-- Present Just [10] (Pure2 Just [10] | Just 10)
-- PresentT (Just [10])
--
-- >>> pl @(Pure2 SG.Sum) (Just 20)
-- Present Just (Sum {getSum = 20}) (Pure2 Just (Sum {getSum = 20}) | Just 20)
-- PresentT (Just (Sum {getSum = 20}))
--
data Pure2 (t :: Type -> Type)

instance (Show (f (t a))
        , Show (f a)
        , Applicative t
        , Functor f
        ) => P (Pure2 t) (f a) where
  type PP (Pure2 t) (f a) = f (t a)
  eval _ opts fa =
    let msg0 = "Pure2"
        b = fmap pure fa
    in pure $ mkNode opts (PresentT b) (show01 opts msg0 b fa) []

-- | similar to 'Control.Applicative.<$'
--
-- >>> pz @(Fst <$ Snd) ("abc",Just 20)
-- PresentT (Just "abc")
--
-- >>> pl @(Fst <$ Snd) (4,These "xxx" 'a')
-- Present These "xxx" 4 ((<$) 4)
-- PresentT (These "xxx" 4)
--
-- >>> pl @(Fst <$ Snd) (4,This 'a')
-- Present This 'a' ((<$) 4)
-- PresentT (This 'a')
--
-- >>> pl @(Fst <$ Snd) (4,Just 'a')
-- Present Just 4 ((<$) 4)
-- PresentT (Just 4)
--
-- >>> pl @(Fst <$ Snd) (4,Nothing @Int)
-- Present Nothing ((<$) 4)
-- PresentT Nothing
--
-- >>> pl @('True <$ Id) [1..4]
-- Present [True,True,True,True] ((<$) True)
-- PresentT [True,True,True,True]
--
-- >>> import Data.Functor.Compose
-- >>> pl @(Char1 "ab" <$ Id) (Compose $ Just [1..4])
-- Present Compose (Just "aaaa") ((<$) 'a')
-- PresentT (Compose (Just "aaaa"))
--
-- >>> pl @(Snd <$ Fst) (Just 10,'x')
-- Present Just 'x' ((<$) 'x')
-- PresentT (Just 'x')
--
data p <$ q
infixl 4 <$

instance (P p x
        , P q x
        , Show (PP p x)
        , Functor t
        , PP q x ~ t c
        , ApplyConstT (PP q x) (PP p x) ~ t (PP p x)
        ) => P (p <$ q) x where
  type PP (p <$ q) x = ApplyConstT (PP q x) (PP p x)
  eval _ opts x = do
    let msg0 = "(<$)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <$ q
        in mkNode opts (PresentT d) (msg0 <> " " <> showL opts p) [hh pp, hh qq]

-- | similar to Applicative 'Control.Applicative.<*'
--
-- >>> pl @(Fst <* Snd) (Just 4,Just 'a')
-- Present Just 4 ((<*) Just 4 | p=Just 4 | q=Just 'a')
-- PresentT (Just 4)
--
-- >>> pz @(Fst <* Snd) (Just "abc",Just 20)
-- PresentT (Just "abc")
--
data p <* q
infixl 4 <*

type ArrowRT p q = q <* p

-- | similar to Applicative 'Control.Applicative.*>'
--
-- >>> pl @(Fst *> Snd) (Just 4,Just 'a')
-- Present Just 'a' ((<*) Just 'a' | p=Just 'a' | q=Just 4)
-- PresentT (Just 'a')
--
data p *> q
infixl 4 *>

instance P (ArrowRT p q) x => P (p *> q) x where
  type PP (p *> q) x = PP (ArrowRT p q) x
  eval _ = eval (Proxy @(ArrowRT p q))

instance (Show (t c)
        , P p x
        , P q x
        , Show (t b)
        , Applicative t
        , t b ~ PP p x
        , PP q x ~ t c
        ) => P (p <* q) x where
  type PP (p <* q) x = PP p x
  eval _ opts x = do
    let msg0 = "(<*)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <* q
        in mkNode opts (PresentT d) (show01' opts msg0 p "p=" p <> showVerbose opts " | q=" q) [hh pp, hh qq]

-- | similar to 'Control.Applicative.<|>'
--
-- >>> pz @(Fst <|> Snd) (Nothing,Just 20)
-- PresentT (Just 20)
--
-- >>> pz @(Fst <|> Snd) (Just 10,Just 20)
-- PresentT (Just 10)
--
-- >>> pz @(Fst <|> Snd) (Nothing,Nothing)
-- PresentT Nothing
--
-- >>> pl @(Fst <|> Snd) (Just "cdef",Just "ab")
-- Present Just "cdef" ((<|>) Just "cdef" | p=Just "cdef" | q=Just "ab")
-- PresentT (Just "cdef")
--
-- >>> pl @(Fst <|> Snd) ("cdef","ab"::String)
-- Present "cdefab" ((<|>) "cdefab" | p="cdef" | q="ab")
-- PresentT "cdefab"
--
data p <|> q
infixl 3 <|>

instance (P p x
        , P q x
        , Show (t b)
        , Alternative t
        , t b ~ PP p x
        , PP q x ~ t b
        ) => P (p <|> q) x where
  type PP (p <|> q) x = PP p x
  eval _ opts x = do
    let msg0 = "(<|>)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <|> q
        in mkNode opts (PresentT d) (show01' opts msg0 d "p=" p <> showVerbose opts " | q=" q) [hh pp, hh qq]


-- | similar to 'Control.Comonad.extract'
--
-- >>> pz @Extract (Nothing,Just 20)
-- PresentT (Just 20)
--
-- >>> pz @Extract (Identity 20)
-- PresentT 20
--
-- >>> pl @Extract (10,"hello")
-- Present "hello" (Extract "hello" | (10,"hello"))
-- PresentT "hello"
--
data Extract
instance (Show (t a)
        , Show a
        , Comonad t
        ) => P Extract (t a) where
  type PP Extract (t a) = a
  eval _ opts ta =
    let msg0 = "Extract"
        d = extract ta
    in pure $ mkNode opts (PresentT d) (show01 opts msg0 d ta) []

-- | similar to 'Control.Comonad.duplicate'
--
-- >>> pz @Duplicate (20,"abc")
-- PresentT (20,(20,"abc"))
--
data Duplicate

instance (Show (t a)
        , Show (t (t a))
        , Comonad t
        ) => P Duplicate (t a) where
  type PP Duplicate (t a) = t (t a)
  eval _ opts ta =
    let msg0 = "Duplicate"
        d = duplicate ta
    in pure $ mkNode opts (PresentT d) (show01 opts msg0 d ta) []

-- | similar to 'Control.Monad.join'
--
-- >>> pz @Join  (Just (Just 20))
-- PresentT (Just 20)
--
-- >>> pz @Join  ["ab","cd","","ef"]
-- PresentT "abcdef"
--
data Join

instance (Show (t (t a))
        , Show (t a)
        , Monad t
        ) => P Join (t (t a)) where
  type PP Join (t (t a)) = t a
  eval _ opts tta =
    let msg0 = "Join"
        d = join tta
    in pure $ mkNode opts (PresentT d) (show01 opts msg0 d tta) []

-- | function application for expressions: similar to 'GHC.Base.$'
--
-- >>> :m + Text.Show.Functions
-- >>> pz @(Fst $$ Snd) ((*16),4)
-- PresentT 64
--
-- >>> pz @(Id $$ "def") ("abc"<>)
-- PresentT "abcdef"
--
data p $$ q
infixl 0 $$

instance (P p x
        , P q x
        , PP p x ~ (a -> b)
        , FnT (PP p x) ~ b
        , PP q x ~ a
        , Show a
        , Show b
        ) => P (p $$ q) x where
  type PP (p $$ q) x = FnT (PP p x)
  eval _ opts x = do
    let msg0 = "($$)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq)  ->
        let d = p q
        in mkNode opts (PresentT d) (msg0 <> " " <> showL opts q <> " = " <> showL opts d) [hh pp, hh qq]

-- reify this so we can combine (type synonyms dont work as well)

-- | flipped function application for expressions: similar to 'Control.Lens.&'
--
-- >>> :m + Text.Show.Functions
-- >>> pz @(Snd $& Fst) ((*16),4)
-- PresentT 64
--
-- >>> pz @("def" $& Id) ("abc"<>)
-- PresentT "abcdef"
--
data q $& p -- flips the args eg a & b & (,) = (b,a)
infixr 1 $&

instance (P p x
        , P q x
        , PP p x ~ (a -> b)
        , FnT (PP p x) ~ b
        , PP q x ~ a
        , Show a
        , Show b
        ) => P (q $& p) x where
  type PP (q $& p) x = FnT (PP p x)
  eval _ opts x = do
    let msg0 = "($&)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq)  ->
        let d = p q
        in mkNode opts (PresentT d) (msg0 <> " " <> showL opts q <> " = " <> showL opts d) [hh pp, hh qq]

type family FnT ab :: Type where
  FnT (a -> b) = b
  FnT ab = GL.TypeError (
      'GL.Text "FnT: expected Type -> Type but found a simple Type?"
      ':$$: 'GL.Text "ab = "
      ':<>: 'GL.ShowType ab)

-- | similar to 'sequenceA'
--
-- >>> pz @Sequence [Just 10, Just 20, Just 30]
-- PresentT (Just [10,20,30])
--
-- >>> pz @Sequence [Just 10, Just 20, Just 30, Nothing, Just 40]
-- PresentT Nothing
--
data Sequence

instance (Show (f (t a))
        , Show (t (f a))
        , Traversable t
        , Applicative f
        ) => P Sequence (t (f a)) where
  type PP Sequence (t (f a)) = f (t a)
  eval _ opts tfa =
     let msg = "Sequence"
         d = sequenceA tfa
     in pure $ mkNode opts (PresentT d) (msg <> " " <> showL opts d <> showVerbose opts " | " tfa) []

-- | like 'traverse'
--
-- >>> pl @(Traverse (If (Gt 3) (Pure Maybe Id) (EmptyT Maybe))) [1..5]
-- Present Nothing ((>>) Nothing | {Sequence Nothing | [Nothing,Nothing,Nothing,Just 4,Just 5]})
-- PresentT Nothing
--
-- >>> pl @(Traverse (MaybeBool (Le 3) Id)) [1..5]
-- Present Nothing ((>>) Nothing | {Sequence Nothing | [Just 1,Just 2,Just 3,Nothing,Nothing]})
-- PresentT Nothing
--
-- >>> pl @(Traverse (If (Gt 0) (Pure Maybe Id) (EmptyT Maybe))) [1..5]
-- Present Just [1,2,3,4,5] ((>>) Just [1,2,3,4,5] | {Sequence Just [1,2,3,4,5] | [Just 1,Just 2,Just 3,Just 4,Just 5]})
-- PresentT (Just [1,2,3,4,5])
--
-- >>> pl @(Traverse (If (Gt 0) (Pure Maybe Id) (MkNothing _))) [1..5]
-- Present Just [1,2,3,4,5] ((>>) Just [1,2,3,4,5] | {Sequence Just [1,2,3,4,5] | [Just 1,Just 2,Just 3,Just 4,Just 5]})
-- PresentT (Just [1,2,3,4,5])
--
-- >>> pl @(Traverse (MaybeBool (Id >= 0) Id)) [1..5]
-- Present Just [1,2,3,4,5] ((>>) Just [1,2,3,4,5] | {Sequence Just [1,2,3,4,5] | [Just 1,Just 2,Just 3,Just 4,Just 5]})
-- PresentT (Just [1,2,3,4,5])
--
-- >>> pl @(Traverse (MaybeBool (Id <= 3) Id)) [1..5]
-- Present Nothing ((>>) Nothing | {Sequence Nothing | [Just 1,Just 2,Just 3,Nothing,Nothing]})
-- PresentT Nothing
--
data Traverse p
type TraverseT p = FMap p >> Sequence

instance P (TraverseT p) x => P (Traverse p) x where
  type PP (Traverse p) x = PP (TraverseT p) x
  eval _ = eval (Proxy @(TraverseT p))

-- | just run the effect ignoring the result passing the original value through
-- for example for use with Stdout so it doesnt interfere with the @a@ on the rhs unless there is an failure
data Skip p

instance ( Show (PP p a)
         , P p a
         ) => P (Skip p) a where
  type PP (Skip p) a = a
  eval _ opts a = do
    let msg0 = "Skip"
    pp <- eval (Proxy @p) opts a
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p -> mkNode opts (PresentT a) (msg0 <> " " <> showL opts p) [hh pp]

-- | run @p@ for the effect and then run @q@ using that original value
data p |> q
type SkipLT p q = Skip p >> q
infixr 1 |>

instance P (SkipLT p q) x => P (p |> q) x where
  type PP (p |> q) x = PP (SkipLT p q) x
  eval _ = eval (Proxy @(SkipLT p q))

-- | run run @p@ and then @q@ for the effect but using the result from @p@
data p >| q
type SkipRT p q = p >> Skip q
infixr 1 >|

instance P (SkipRT p q) x => P (p >| q) x where
  type PP (p >| q) x = PP (SkipRT p q) x
  eval _ = eval (Proxy @(SkipRT p q))

-- | run both @p@ and @q@ for their effects but ignoring the results
data p >|> q
type SkipBothT p q = Skip p >> Skip q
infixr 1 >|>

instance P (SkipBothT p q) x => P (p >|> q) x where
  type PP (p >|> q) x = PP (SkipBothT p q) x
  eval _ = eval (Proxy @(SkipBothT p q))

-- | takes the head of a list-like object or uses the given default value
--
-- see 'ConsT' for other supported types eg 'Seq.Seq'
--
-- >>> pz @(HeadDef 444 Id) []
-- PresentT 444
--
-- >>> pz @(HeadDef 444 Id) [1..5]
-- PresentT 1
--
-- >>> pz @(HeadDef 444 Id) [1..5]
-- PresentT 1
--
-- >>> pz @(HeadDef (Char1 "w") Id) (Seq.fromList "abcdef")
-- PresentT 'a'
--
-- >>> pz @(HeadDef (Char1 "w") Id) Seq.empty
-- PresentT 'w'
--
-- >>> pz @(HeadDef (MEmptyT _) Id) ([] :: [SG.Sum Int])
-- PresentT (Sum {getSum = 0})
--
-- >>> pz @(HeadDef (MEmptyT String) '["abc","def","asdfadf"]) ()
-- PresentT "abc"
--
-- >>> pz @(HeadDef (MEmptyT _) Snd) (123,["abc","def","asdfadf"])
-- PresentT "abc"
--
-- >>> pz @(HeadDef (MEmptyT _) Snd) (123,[])
-- PresentT ()
--
-- >>> pl @(HeadDef 9 Fst) ([],True)
-- Present 9 (JustDef Nothing)
-- PresentT 9
--
-- >>> pl @(HeadDef 9 Fst) ([1..5],True)
-- Present 1 (JustDef Just)
-- PresentT 1
--
-- >>> pl @(HeadDef 3 Fst) ([10..15],True)
-- Present 10 (JustDef Just)
-- PresentT 10
--
-- >>> pl @(HeadDef 12 Fst >> Le 6) ([],True)
-- Present False ((>>) False | {False:12 <= 6})
-- PresentT False
--
-- >>> pl @(HeadDef 1 Fst >> Le 6) ([],True)
-- Present True ((>>) True | {True:1 <= 6})
-- PresentT True
--
-- >>> pl @(HeadDef 10 Fst >> Le 6) ([],True)
-- Present False ((>>) False | {False:10 <= 6})
-- PresentT False
--
-- >>> pl @(HeadDef (MEmptyT _) Id) (map (:[]) ([] :: [Int]))
-- Present [] (JustDef Nothing)
-- PresentT []
--
-- >>> pl @(HeadDef (MEmptyT _) Id) (map (:[]) ([10..14] :: [Int]))
-- Present [10] (JustDef Just)
-- PresentT [10]
--
-- >>> pl @(HeadDef Fst Snd) (99,[10..14])
-- Present 10 (JustDef Just)
-- PresentT 10
--
-- >>> pl @(HeadDef Fst Snd) (99,[] :: [Int])
-- Present 99 (JustDef Nothing)
-- PresentT 99
--
-- >>> pl @(HeadDef 43 Snd) (99,[] :: [Int])
-- Present 43 (JustDef Nothing)
-- PresentT 43
--
data HeadDef p q
type HeadDefT p q = JustDef p (q >> Uncons >> FMap Fst)

instance P (HeadDefT p q) x => P (HeadDef p q) x where
  type PP (HeadDef p q) x = PP (HeadDefT p q) x
  eval _ = eval (Proxy @(HeadDefT p q))


-- | takes the head of a list or fails with the given message
--
-- see 'ConsT' for other supported types eg 'Seq.Seq'
--
-- >>> pz @(HeadFail "dude" Id) ["abc","def","asdfadf"]
-- PresentT "abc"
--
-- >>> pz @(HeadFail "empty list" Id) []
-- FailT "empty list"
--
-- >>> pl @(HeadFail "zz" Fst >> Le 6) ([],True)
-- Error zz
-- FailT "zz"
--
-- >>> pl @((HeadFail "failed1" Fst >> Le 6) || 'False) ([],True)
-- Error failed1 (||)
-- FailT "failed1"
--
-- >>> pl @((Fst >> HeadFail "failed2" Id >> Le (6 -% 1)) || 'False) ([-9],True)
-- Present True (True:True || False)
-- PresentT True
--
-- >>> pl @(HeadFail "Asdf" Id) ([] :: [()]) -- breaks otherwise
-- Error Asdf (JustFail Nothing)
-- FailT "Asdf"
--
-- >>> pl @(HeadFail (PrintF "msg=%s def" Fst) Snd) ("Abc" :: String,[]::[Int])
-- Error msg=Abc def (JustFail Nothing)
-- FailT "msg=Abc def"
--

data HeadFail msg q
type HeadFailT msg q = JustFail msg (q >> Uncons >> FMap Fst)

instance P (HeadFailT msg q) x => P (HeadFail msg q) x where
  type PP (HeadFail msg q) x = PP (HeadFailT msg q) x
  eval _ = eval (Proxy @(HeadFailT msg q))

-- | takes the tail of a list-like object or uses the given default value
--
-- >>> pl @(TailDef '[9,7] Fst) ([],True)
-- Present [9,7] (JustDef Nothing)
-- PresentT [9,7]
--
-- >>> pl @(TailDef '[9,7] Fst) ([1..5],True)
-- Present [2,3,4,5] (JustDef Just)
-- PresentT [2,3,4,5]
--
-- >>> pl @(TailDef '[3] Fst) ([10..15],True)
-- Present [11,12,13,14,15] (JustDef Just)
-- PresentT [11,12,13,14,15]
--

data TailDef p q
type TailDefT p q = JustDef p (q >> Uncons >> FMap Snd)

instance P (TailDefT p q) x => P (TailDef p q) x where
  type PP (TailDef p q) x = PP (TailDefT p q) x
  eval _ = eval (Proxy @(TailDefT p q))


-- | takes the tail of a list-like object or fails with the given message
--
-- >>> pl @(TailFail (PrintT "a=%d b=%s" Snd) Fst) ([]::[()],(4::Int,"someval" :: String))
-- Error a=4 b=someval (JustFail Nothing)
-- FailT "a=4 b=someval"
--

data TailFail msg q
type TailFailT msg q = JustFail msg (q >> Uncons >> FMap Snd)

instance P (TailFailT msg q) x => P (TailFail msg q) x where
  type PP (TailFail msg q) x = PP (TailFailT msg q) x
  eval _ = eval (Proxy @(TailFailT msg q))

-- | takes the last value of a list-like object or a default value
--
-- >>> pl @(LastDef 9 Fst) ([],True)
-- Present 9 (JustDef Nothing)
-- PresentT 9
--
-- >>> pl @(LastDef 9 Fst) ([1..5],True)
-- Present 5 (JustDef Just)
-- PresentT 5
--
-- >>> pl @(LastDef 3 Fst) ([10..15],True)
-- Present 15 (JustDef Just)
-- PresentT 15
--
-- >>> pl @(LastDef 0 Id) [1..12]
-- Present 12 (JustDef Just)
-- PresentT 12
--
-- >>> pl @(LastDef 0 Id) []
-- Present 0 (JustDef Nothing)
-- PresentT 0
--

data LastDef p q
type LastDefT p q = JustDef p (q >> Unsnoc >> FMap Snd)

instance P (LastDefT p q) x => P (LastDef p q) x where
  type PP (LastDef p q) x = PP (LastDefT p q) x
  eval _ = eval (Proxy @(LastDefT p q))

-- | takes the init of a list-like object or fails with the given message
data LastFail msg q
type LastFailT msg q = JustFail msg (q >> Unsnoc >> FMap Snd)

instance P (LastFailT msg q) x => P (LastFail msg q) x where
  type PP (LastFail msg q) x = PP (LastFailT msg q) x
  eval _ = eval (Proxy @(LastFailT msg q))

-- | takes the init of a list-like object or uses the given default value
--
-- >>> pl @(InitDef '[9,7] Fst) ([],True)
-- Present [9,7] (JustDef Nothing)
-- PresentT [9,7]
--
-- >>> pl @(InitDef '[9,7] Fst) ([1..5],True)
-- Present [1,2,3,4] (JustDef Just)
-- PresentT [1,2,3,4]
--
-- >>> pl @(InitDef '[3] Fst) ([10..15],True)
-- Present [10,11,12,13,14] (JustDef Just)
-- PresentT [10,11,12,13,14]
--
data InitDef p q
type InitDefT p q = JustDef p (q >> Unsnoc >> FMap Fst)

instance P (InitDefT p q) x => P (InitDef p q) x where
  type PP (InitDef p q) x = PP (InitDefT p q) x
  eval _ = eval (Proxy @(InitDefT p q))

-- | takes the init of a list-like object or fails with the given message
data InitFail msg q
type InitFailT msg q = JustFail msg (q >> Unsnoc >> FMap Fst)

instance P (InitFailT msg q) x => P (InitFail msg q) x where
  type PP (InitFail msg q) x = PP (InitFailT msg q) x
  eval _ = eval (Proxy @(InitFailT msg q))

type family ApplyConstT (ta :: Type) (b :: Type) :: Type where
--type family ApplyConstT ta b where -- less restrictive so allows ('Just Int) Bool through!
  ApplyConstT (t a) b = t b
  ApplyConstT ta b = GL.TypeError (
       'GL.Text "ApplyConstT: (t a) b but found something else"
       ':$$: 'GL.Text "t a = "
       ':<>: 'GL.ShowType ta
       ':$$: 'GL.Text "b = "
       ':<>: 'GL.ShowType b)

-- | a predicate on prime numbers
--
-- >>> pz @IsPrime 2
-- PresentT True
--
-- >>> pz @(Map '(Id,IsPrime) Id) [0..12]
-- PresentT [(0,False),(1,False),(2,True),(3,True),(4,False),(5,True),(6,False),(7,True),(8,False),(9,False),(10,False),(11,True),(12,False)]
--
data IsPrime

instance (x ~ a
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
-- PresentT 7
--
-- >>> pz @(ScanN 4 PrimeNext Id) 3
-- PresentT [3,5,7,11,13]
--
data PrimeNext

instance ( Show x
         , Integral x
         ) => P PrimeNext x where
  type PP PrimeNext x = Integer
  eval _ opts x =
    let msg0 = "PrimeNext"
        ret = Safe.headNote msg0 $ dropWhile (<= fromIntegral x) primes
    in pure $ mkNode opts (PresentT ret) (msg0 <> showVerbose opts " | " x) []

-- | get the next prime number
--
-- >>> pz @PrimePrev 6
-- PresentT 5
--
-- >>> pz @PrimePrev 5
-- PresentT 3
--
-- >>> pz @PrimePrev (-206)
-- PresentT 2
--
-- >>> pz @(ScanN 6 PrimePrev Id) 11
-- PresentT [11,7,5,3,2,2,2]
--
data PrimePrev

instance ( Show x
         , Integral x
         ) => P PrimePrev x where
  type PP PrimePrev x = Integer
  eval _ opts x =
    let msg0 = "PrimePrev"
        ret = case unsnoc $ takeWhile (< fromIntegral x) primes of
                Just (_,p) -> p
                Nothing -> 2
    in pure $ mkNode opts (PresentT ret) (msg0 <> showVerbose opts " | " x) []

-- | get list of @n@ primes
--
-- >>> pz @(Primes Id) 5
-- PresentT [2,3,5,7,11]
--
data Primes n

instance (Integral (PP n x)
        , P n x
        ) => P (Primes n) x where
  type PP (Primes n) x = [Integer]
  eval _ opts x = do
    let msg0 = "Primes"
    nn <- eval (Proxy @n) opts x
    pure $ case getValueLR opts msg0 nn [] of
      Left e -> e
      Right (fromIntegral -> n) ->
        let ret = take n primes
        in mkNode opts (PresentT ret) (msg0 <> showVerbose opts " | " n) [hh nn]

-- | prime factorisation of positive numbers
--
-- >>> pz @(PrimeFactors Id) 17
-- PresentT [17]
--
-- >>> pz @(PrimeFactors Id) 1
-- PresentT [1]
--
-- >>> pz @(PrimeFactors Id) 30
-- PresentT [2,3,5]
--
-- >>> pz @(PrimeFactors Id) 64
-- PresentT [2,2,2,2,2,2]
--
-- >>> pz @(PrimeFactors Id) (-30)
-- FailT "PrimeFactors number<=0"
--
data PrimeFactors n

instance (Integral (PP n x)
        , P n x
        ) => P (PrimeFactors n) x where
  type PP (PrimeFactors n) x = [Integer]
  eval _ opts x = do
    let msg0 = "PrimeFactors"
    nn <- eval (Proxy @n) opts x
    pure $ case getValueLR opts msg0 nn [] of
      Left e -> e
      Right (fromIntegral -> n :: Integer)
            | n <= 0 -> mkNode opts (FailT (msg0 <> " number<=0")) "" [hh nn]
            | otherwise ->
                let ret = primeFactors n
                in mkNode opts (PresentT ret) (msg0 <> showVerbose opts " | " n) [hh nn]

-- | IsLuhn predicate check on last digit
--
-- >>> pz @IsLuhn [1,2,3,0]
-- PresentT True
--
-- >>> pz @IsLuhn [1,2,3,4]
-- PresentT False
--
-- >>> pz @(GuardSimple IsLuhn) [15,4,3,1,99]
-- FailT "(False:IsLuhn map=[90,2,3,8,6] sum=109 ret=9 | [15,4,3,1,99])"
--
-- >>> pl @IsLuhn [15,4,3,1,99]
-- Present False (False:IsLuhn map=[90,2,3,8,6] sum=109 ret=9 | [15,4,3,1,99])
-- PresentT False
--
data IsLuhn

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

-- | coerce over a functor
--
-- >>> pz @(Coerce2 (SG.Sum Integer)) [Identity (-13), Identity 4, Identity 99]
-- PresentT [Sum {getSum = -13},Sum {getSum = 4},Sum {getSum = 99}]
--
-- >>> pz @(Coerce2 (SG.Sum Integer)) (Just (Identity (-13)))
-- PresentT (Just (Sum {getSum = -13}))
--
-- >>> pz @(Coerce2 (SG.Sum Int)) (Nothing @(Identity Int))
-- PresentT Nothing
--
-- >>> pl @(Coerce2 (SG.Sum Int)) (Just (10 :: Int))
-- Present Just (Sum {getSum = 10}) (Coerce2 Just (Sum {getSum = 10}) | Just 10)
-- PresentT (Just (Sum {getSum = 10}))
--
data Coerce2 (t :: k)
instance (Show (f a)
        , Show (f t)
        , Coercible t a
        , Functor f
        ) => P (Coerce2 t) (f a) where
  type PP (Coerce2 t) (f a) = f t
  eval _ opts fa =
    let msg0 = "Coerce2"
        d = view coerced <$> fa
    in pure $ mkNode opts (PresentT d) (show01 opts msg0 d fa) []

data ProxyT' t

instance P (ProxyT' t) x where
  type PP (ProxyT' t) x = Proxy (PP t x)
  eval _ opts _ =
    pure $ mkNode opts (PresentT Proxy) "ProxyT" []

data ProxyT (t :: Type)
type ProxyTT (t :: Type) = ProxyT' (Hole t)

instance P (ProxyT t) x where
  type PP (ProxyT t) x = PP (ProxyTT t) x
  eval _ = eval (Proxy @(ProxyTT t))



-- more flexible: takes a (String,x) and a proxy so we can still call 'False 'True
-- now takes the FailT string and x so you can print more detail if you want
-- need the proxy so we can fail without having to explicitly specify a type

-- | run an expression @p@ and on failure run @q@
--
-- >>> pz @(Catch Succ (Fst >> Second (ShowP Id) >> PrintT "%s %s" Id >> 'LT)) GT
-- PresentT LT
--
-- >>> pz @(Len > 1 && Catch (Id !! 3 == 66) 'False) [1,2]
-- PresentT False
--
-- >>> pl @(Catch (Resplit "\\d+(") (Snd >> MEmptyP)) "123"
-- Present [] (Catch caught exception[Regex failed to compile])
-- PresentT []
--
-- >>> pl @(Catch OneP 99) [10,11]
-- Present 99 (Catch caught exception[OneP:expected one element(2)])
-- PresentT 99
--
-- >>> pl @(Catch OneP 99) [10]
-- Present 10 (Catch did not fire)
-- PresentT 10
--
-- >>> pl @(Catch OneP 'True) [False]  -- cant know that this is PresentT False cos is driven by type of the list not the 'True part
-- Present False (Catch did not fire)
-- PresentT False
--
-- >>> pl @(Catch OneP 'False) [True,True,False]
-- Present False (Catch caught exception[OneP:expected one element(3)])
-- PresentT False
--
-- >>> pl @(Catch OneP 'True) []
-- Present True (Catch caught exception[OneP:expected one element(empty)])
-- PresentT True
--
data Catch p q

-- | run an expression @p@ and on failure print a custom error @s@ using the error string and the input value
--
-- >>> pz @(Catch' Succ (Second (ShowP Id) >> PrintT "%s %s" Id)) GT
-- FailT "Succ IO e=Prelude.Enum.Ordering.succ: bad argument GT"
--
-- >>> pz @(Catch' Succ (Second (ShowP Id) >> PrintT "%s %s" Id)) LT
-- PresentT EQ
--
-- >>> pl @(Catch' (Failt Int "someval") (PrintT "msg=%s caught(%03d)" Id)) (44 :: Int)
-- Error msg=someval caught(044) (Catch default condition failed)
-- FailT "msg=someval caught(044)"
--
-- >>> pl @(Catch' OneP (Second (ShowP Id) >> PrintT "msg=%s caught(%s)" Id)) [10,12,13]
-- Error msg=OneP:expected one element(3) caught([10,12,13]) (Catch default condition failed)
-- FailT "msg=OneP:expected one element(3) caught([10,12,13])"
--
-- >>> pl @(Catch' OneP (PrintT "msg=%s caught(%s)" (Second (ShowP Id)))) [10]
-- Present 10 (Catch did not fire)
-- PresentT 10
--
-- >>> pl @(Catch' OneP (PrintT "msg=%s err s=%s" (Second (ShowP Id)))) [10,11]
-- Error msg=OneP:expected one element(2) err s=[10,11] (Catch default condition failed)
-- FailT "msg=OneP:expected one element(2) err s=[10,11]"
--
data Catch' p s
type CatchT' p s = Catch p (FailCatchT s) -- eg set eg s=PrintF "%d" Id or PrintF "%s" (ShowP Id)
type FailCatchT s = Fail (Snd >> Unproxy) (Fst >> s)

instance P (CatchT' p s) x => P (Catch' p s) x where
  type PP (Catch' p s) x = PP (CatchT' p s) x
  eval _ = eval (Proxy @(CatchT' p s))

instance (P p x
        , P q ((String, x)
        , Proxy (PP p x))
        , PP p x ~ PP q ((String, x), Proxy (PP p x))
        ) => P (Catch p q) x where
  type PP (Catch p q) x = PP p x
  eval _ opts x = do
    let msg0 = "Catch"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> do
         let emsg = e ^?! ttBool . _FailT -- extract the failt string a push back into the fail case
         qq <- eval (Proxy @q) opts ((emsg, x), Proxy @(PP p x))
         pure $ case getValueLR opts (msg0 <> " default condition failed") qq [hh pp] of
            Left e1 -> e1
            Right _ -> mkNode opts (_ttBool qq) (msg0 <> " caught exception[" <> emsg <> "]") [hh pp, hh qq]
      Right _ -> pure $ mkNode opts (_ttBool pp) (msg0 <> " did not fire") [hh pp]

-- | compose simple functions
--
-- >>> pl @(Dot '[L3,L2,L1] Id) ((1,(2,9,10)),(3,4))
-- Present 10 (Thd 10 | (2,9,10))
-- PresentT 10
--
data Dot (ps :: [Type -> Type]) (q :: Type)
instance (P (DotExpandT ps q) a) => P (Dot ps q) a where
  type PP (Dot ps q) a = PP (DotExpandT ps q) a
  eval _ = eval (Proxy @(DotExpandT ps q))

type family DotExpandT (ps :: [Type -> Type]) (q :: Type) :: Type where
  DotExpandT '[] _ = GL.TypeError ('GL.Text "'[] invalid: requires at least one predicate in the list")
  DotExpandT '[p] q = p $ q
  DotExpandT (p ': p1 ': ps) q = p $ DotExpandT (p1 ': ps) q

-- | reversed version of 'Dot'
--
-- >>> pl @(RDot '[L1,L2,L3] Id) ((1,(2,9,10)),(3,4))
-- Present 10 (Thd 10 | (2,9,10))
-- PresentT 10
--
-- >>> pl @(RDot '[L1,L2] Id) (('a',2),(True,"zy"))
-- Present 2 (Snd 2 | ('a',2))
-- PresentT 2
--
data RDot (ps :: [Type -> Type]) (q :: Type)
instance P (RDotExpandT ps q) a => P (RDot ps q) a where
  type PP (RDot ps q) a = PP (RDotExpandT ps q) a
  eval _ = eval (Proxy @(RDotExpandT ps q))

type family RDotExpandT (ps :: [Type -> Type]) (q :: Type) :: Type where
  RDotExpandT '[] _ = GL.TypeError ('GL.Text "'[] invalid: requires at least one predicate in the list")
  RDotExpandT '[p] q = p $ q
  RDotExpandT (p ': p1 ': ps) q = RDotExpandT (p1 ': ps) (p $ q)

-- | creates a constant expression ignoring the second argument:types dont need to match on rhs!
--
-- >>> pl @(RDot '[L1,L2,L3,K "xxx"] Id) 12345
-- Present "xxx" (K '"xxx")
-- PresentT "xxx"
--
-- >>> pl @(RDot '[L1,L2,L3,K '("abc",Id)] Id) ()
-- Present ("abc",()) (K '("abc",()))
-- PresentT ("abc",())
--
-- >>> pl @(Dot '[K "skip",L6,Lift Dup,Lift Succ] Id) ()
-- Present "skip" (K '"skip")
-- PresentT "skip"
--
-- >>> pl @(L3 $ L2 $ L1 $ K Id "dud") ((1,("W",9,'a')),(3,4))
-- Present 'a' (Thd 'a' | ("W",9,'a'))
-- PresentT 'a'
--
-- >>> pl @((L3 $ L2 $ L1 $ K Id "dud") >> Pred) ((1,("W",9,'a')),(3,4))
-- Present '`' ((>>) '`' | {Pred '`' | 'a'})
-- PresentT '`'
--
-- >>> pl @(K "ss" $ L3 $ L3 $ Fst) ()
-- Present "ss" (K '"ss")
-- PresentT "ss"
--
data K (p :: k) (q :: k1)
instance P p a => P (K p q) a where
  type PP (K p q) a = PP p a
  eval _ = eval (Proxy @(MsgI "K " p))

-- | Lift a no arg Adt to a function of one argument (for use with 'Dot' and 'RDot')
--
-- >>> pl @(Lift Len Snd) (True,"abcdef")
-- Present 6 ((>>) 6 | {Len 6 | "abcdef"})
-- PresentT 6
--
data Lift p q
type LiftT p q = q >> p

instance P (LiftT p q) x => P (Lift p q) x where
  type PP (Lift p q) x = PP (LiftT p q) x
  eval _ = eval (Proxy @(LiftT p q))

-- | application using a Proxy: @q@ must be of kind Type else ambiguous k0 error
--
-- >>> pl @(Apply1 (MsgI "hello ")) (Proxy @(W "there"),()) -- have to wrap Symbol
-- Present "there" (hello W '"there")
-- PresentT "there"
--
-- >>> pl @(Apply1 Length) (Proxy @Snd,(True,"abcdef"))
-- Present 6 (Length 6 | "abcdef")
-- PresentT 6
--
-- >>> pl @(Apply1 ((+) 4)) (Proxy @Fst,(5,"abcdef"))
-- Present 9 (4 + 5 = 9)
-- PresentT 9
--
-- >>> pl @(Apply1 L1) (Proxy @Id, (123,"asfd"))
-- Present 123 (Fst 123 | (123,"asfd"))
-- PresentT 123
--
-- >>> pz @('(Proxy (W 12),9) >> Apply1 ((+) Id)) ()
-- PresentT 21
--
-- >>> pz @('(Proxy (W 5),()) >> Apply1 (Lift Succ)) ()
-- PresentT 6
--
-- >>> pz @('(Proxy (W 12),Id) >> Apply1 ((+) Id)) 9
-- PresentT 21
--
data Apply1 (p :: Type -> Type) -- will not work unless p :: Type -> Type: cant be polymorphic in k
instance forall p q x . (P (p q) x)
   => P (Apply1 p) (Proxy q, x) where
  type PP (Apply1 p) (Proxy q, x) = PP (p q) x
  eval _ opts (Proxy, x) =
    eval (Proxy @(p q)) opts x

-- | application using a Proxy: @q@ and @r@ must be of kind Type else ambiguous k0 error
--
-- >>> pl @(Apply2 (+)) ((Proxy @Fst,Proxy @(Length Snd)),(5,"abcdef"))
-- Present 11 (5 + 6 = 11)
-- PresentT 11
--
-- >>> pl @(Apply2 (+)) ((Proxy @(W 3),Proxy @(W 7)),())
-- Present 10 (3 + 7 = 10)
-- PresentT 10
--
-- >>> pl @(Apply2 (&&&)) ((Proxy @(W "abc"),Proxy @(W 13)), ())
-- Present ("abc",13) (W '("abc",13))
-- PresentT ("abc",13)
--
data Apply2 (p :: Type -> Type -> Type)
instance forall p (q :: Type) (r :: Type) x . (P (p q r) x)
   => P (Apply2 p) ((Proxy q,Proxy r), x) where
  type PP (Apply2 p) ((Proxy q,Proxy r), x) = PP (p q r) x
  eval _ opts ((Proxy, Proxy), x) =
    eval (Proxy @(p q r)) opts x

-- | experimental: sorts then partitions and then sorts each partitions based on the leftmost occurring value in the original list
--   if the existing order of data is fine then use 'Predicate.Data.List.GroupBy' as you do not need this
--
-- >>> pz @(PartitionsBy (Fst ==! Snd) (L11 == L21) Id) [10,9,9,1,9]
-- PresentT [[10],[9,9,9],[1]]
--
-- >>> pz @(PartitionsBy OrdA (L11 < L21) Id) "efaffabec"
-- PresentT ["a","f","f","abce","ef"]
--
-- >>> pz @(PartitionsBy 'GT 'True Id) "efaffabec"
-- PresentT ["cebaffafe"]
--
-- >>> pz @(PartitionsBy 'GT 'False Id) "efaffabec"
-- PresentT ["e","f","a","f","f","a","b","e","c"]
--
-- >>> pz @(PartitionsBy (Fst ==! Snd) (L12 > L22) Id) [10,9,9,1,9,4]
-- PresentT [[9],[1],[9,10],[4,9]]
--
-- >>> pz @(PartitionsBy (L11 ==! L21) (L12 > L22) Id) "eddadc"
-- PresentT ["d","a","de","cd"]
--
-- >>> pz @(PartitionsBy (L11 ==! L21) (L11 < L21) Id) [10,9,9,1,9,4]
-- PresentT [[9],[1,4,9],[9,10]]
--
data PartitionsBy p q r
type PartitionsByT p q r = SortBy p (Zip r (0 ... (Length r - 1))) >> GroupBy q Id >> SortOn (Head >> Snd) Id >> Map (Map Fst Id) Id

instance P (PartitionsByT p q r) x => P (PartitionsBy p q r) x where
  type PP (PartitionsBy p q r) x = PP (PartitionsByT p q r) x
  eval _ = eval (Proxy @(PartitionsByT p q r))

-- | add an index to map
--
-- >>> pz @(Rescan "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$" >> Map (Snd >> IMap (GuardBool (PrintT "bad value=%d %s" Id) (Snd >> ReadP Int Id < 255)) Id) Id) "123.222.999.3"
-- FailT "bad value=2 999"
--
-- >>> pz @(Rescan "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$" >> Map (Snd >> IMap (GuardBool (PrintT "bad value=%d %s" Id) (Snd >> ReadP Int Id < 255)) Id) Id) "123.222.99.3"
-- PresentT [[True,True,True,True]]
--
data IMap p q
type IMapT p q = ZipWith p (0 ... (Length q - 1)) q

instance P (IMapT p q) x => P (IMap p q) x where
  type PP (IMap p q) x = PP (IMapT p q) x
  eval _ = eval (Proxy @(IMapT p q))

-- | add an index to list
--
-- >>> pz @IList "abcdef"
-- PresentT [(0,'a'),(1,'b'),(2,'c'),(3,'d'),(4,'e'),(5,'f')]
--
data IList
type IListT = Zip (0 ... (Len - 1)) Id

instance P IListT x => P IList x where
  type PP IList x = PP IListT x
  eval _ = eval (Proxy @IListT)

-- | fmap
--
-- >>> pz @(FMap (MkDay Id) >> Join) (Just (2020,01,01))
-- PresentT (Just 2020-01-01)
--
-- >>> pz @(FMap (MkDay Id) >> Join) (Just (2020,01,32))
-- PresentT Nothing
--
-- >>> pz @(FMap Succ) (Just LT)
-- PresentT (Just EQ)
--
-- >>> pz @(FMap Pred) (Just LT)
-- FailT "Pred IO e=Prelude.Enum.Ordering.pred: bad argument"
--
-- >>> pz @(FMap (ShowP Id)) (Just 10)
-- PresentT (Just "10")
--
-- >>> pan @(FMap $ FMap $ FMap Succ) [Just "abcdefG",Nothing,Just "X"]
-- FMap
-- |
-- `- FMap | FMap | FMap
--    |
--    +- FMap
--    |  |
--    |  `- Succ 'b' | Succ 'c' | Succ 'd' | Succ 'e' | Succ 'f' | Succ 'g' | Succ 'H'
--    |
--    +- FMap <skipped>
--    |
--    `- FMap
--       |
--       `- Succ 'Y'
-- PresentT [Just "bcdefgH",Nothing,Just "Y"]
--
data FMap p

instance (Traversable n, P p a, PP p a ~ b) => P (FMap p) (n a) where
  type PP (FMap p) (n a) = n (PP p a)
  eval _ opts na = do
    let msg0 = "FMap"
    nttb <- traverse (eval (Proxy @p) opts) na
    let ttnb = sequenceA nttb
    pure $ case getValueLR opts msg0 ttnb [] of
      Left e -> e
      Right ret -> mkNode opts (PresentT ret) msg0 [hh (fixEmptyNode (msg0 <> " <skipped>") ttnb)]

type family JoinT x y where
  JoinT (t a) (t b) = t (a, b)
  JoinT ta tb = GL.TypeError (
       'GL.Text "JoinT: expected (t a) (t b) but found something else"
       ':$$: 'GL.Text "t a = "
       ':<>: 'GL.ShowType ta
       ':$$: 'GL.Text "t b = "
       ':<>: 'GL.ShowType tb)

-- | see 'FMap'
--
-- >>> pz @(Len <$> Snd) (1,Just "abcdef")
-- PresentT (Just 6)
--
-- >>> pz @(Len <$> (Id <> Id <> "extra" <$> Snd)) (1,Just "abcdef")
-- PresentT (Just 17)
--
-- >>> pz @(Len <$> (Id <> Id <> "extra" <$> Snd)) (1,Right "abcdef")
-- PresentT (Right 17)
--
-- >>> pz @(FMap $ FMap (Succ <$> Id)) (True,Just (These 12 'c'))
-- PresentT (True,Just (These 12 'd'))
--
-- >>> pz @(FMap (Second (Succ <$> Id))) [(True, (These 12 'c'))]
-- PresentT [(True,These 12 'd')]
--
data p <$> q
infixl 4 <$>

instance (Traversable n, P q a, P p b, PP q a ~ n b, PP p b ~ c) => P (p <$> q) a where
  type PP (p <$> q) a = (ExtractTFromTA (PP q a)) (PP p (ExtractAFromTA (PP q a)))
  eval _ opts x = do
    let msg0 = "(<$>)"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        nttb <- traverse (eval (Proxy @p) opts) q
        let ttnb = sequenceA nttb
        pure $ case getValueLR opts msg0 ttnb [hh qq] of
          Left e -> e
          Right ret -> mkNode opts (PresentT ret) msg0 [hh qq, hh (fixEmptyNode (msg0 <> " <skipped>") ttnb)]

-- | runs liftA2 (,) against two values
--
-- >>> pz @(FPair Fst Snd) (Just 10, Just True)
-- PresentT (Just (10,True))
--
-- >>> pz @(FPair Fst Snd >> FMap (ShowP Fst <> "---" <> ShowP Snd)) (Just 10, Just True)
-- PresentT (Just "10---True")
--
-- >>> pz @(FPair Fst Snd >> FMap (Fst + Snd)) (Just 10, Just 13)
-- PresentT (Just 23)
--
data FPair p q

instance ( Applicative n
         , PP p a ~ n x
         , PP q a ~ n y
         , JoinT (PP p a) (PP q a) ~ n (x,y)
         , P p a
         , P q a
         )
    => P (FPair p q) a where
  type PP (FPair p q) a = JoinT (PP p a) (PP q a)
  eval _ opts a = do
    let msg0 = "FPair"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq)  ->
        let d = liftA2 (,) p q
        in mkNode opts (PresentT d) msg0 [hh pp, hh qq]

-- | see 'FPair'
--
-- >>> pz @(Fst <:> Snd) (Just 10, Just True)
-- PresentT (Just (10,True))
--
-- >>> pz @(Fst <:> Snd) ("abc" :: String,[10,12,14])
-- PresentT [('a',10),('a',12),('a',14),('b',10),('b',12),('b',14),('c',10),('c',12),('c',14)]
--
data p <:> q
type FPairT p q = FPair p q
infixl 6 <:>

instance P (FPairT p q) x => P (p <:> q) x where
  type PP (p <:> q) x = PP (FPairT p q) x
  eval _ = eval (Proxy @(FPairT p q))


-- | similar to monad operator 'Control.Monad.>=>'
--
-- >>> pz @(FFish Uncons (Snd >> Uncons) "abcdef") ()
-- PresentT (Just ('b',"cdef"))
--
-- >>> :m + Data.Time
-- >>> pz @(FFish (ReadMaybe Day Id >> FMap Fst) (MkJust Succ) "2020-02-02") ()
-- PresentT (Just 2020-02-03)
--
-- >>> pz @(FFish Uncons (Fst >> Lookup "abcdef" Id) [3,14,12]) ()
-- PresentT (Just 'd')
--
data FFish amb bmc a
type FFishT amb bmc a = a >> amb >> FMap bmc >> Join

instance P (FFishT p q r) x => P (FFish p q r) x where
  type PP (FFish p q r) x = PP (FFishT p q r) x
  eval _ = eval (Proxy @(FFishT p q r))

-- | similar to monad bind operator 'Control.Monad.>>='
--
-- >>> pz @(Id >>= HeadMay) (Just "abcdef")
-- PresentT (Just 'a')
--
-- >>> pz @(Uncons >>= (Snd >> HeadMay)) "abcdef"
-- PresentT (Just 'b')
--
data ma >>= amb
type FBindT ma amb = ma >> FMap amb >> Join
infixl 1 >>=

instance P (FBindT p q) x => P (p >>= q) x where
  type PP (p >>= q) x = PP (FBindT p q) x
  eval _ = eval (Proxy @(FBindT p q))

-- | similar to 'Safe.headMay'
--
-- >>> pl @HeadMay []
-- Present Nothing ((>>) Nothing | {FMap})
-- PresentT Nothing
--
-- >>> pl @HeadMay [99,7,3]
-- Present Just 99 ((>>) Just 99 | {FMap})
-- PresentT (Just 99)
--
data HeadMay
type HeadMayT = Uncons >> FMap Fst

instance P HeadMayT x => P HeadMay x where
  type PP HeadMay x = PP HeadMayT x
  eval _ = eval (Proxy @HeadMayT)

-- | similar to 'Safe.lastMay'
--
-- >>> pz @LastMay "hello"
-- PresentT (Just 'o')
--
data LastMay
type LastMayT = Unsnoc >> FMap Snd

instance P LastMayT x => P LastMay x where
  type PP LastMay x = PP LastMayT x
  eval _ = eval (Proxy @LastMayT)

-- | similar to 'Safe.tailMay'
--
-- >>> pz @TailMay "hello"
-- PresentT (Just "ello")
--
data TailMay
type TailMayT = Uncons >> FMap Snd

instance P TailMayT x => P TailMay x where
  type PP TailMay x = PP TailMayT x
  eval _ = eval (Proxy @TailMayT)

-- | similar to 'Safe.initMay'
--
-- >>> pz @InitMay "hello"
-- PresentT (Just "hell")
--
data InitMay
type InitMayT = Unsnoc >> FMap Fst

instance P InitMayT x => P InitMay x where
  type PP InitMay x = PP InitMayT x
  eval _ = eval (Proxy @InitMayT)

