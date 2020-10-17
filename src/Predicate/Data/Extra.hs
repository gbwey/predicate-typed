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
-- |     extra promoted functions
module Predicate.Data.Extra (
 -- ** functor
    FMap
  , type (<$>)
  , type (<&>)

 -- ** applicative
  , type (<*>)
  , LiftA2
  , FPair
  , type (<:>)
  , type (<$)
  , type (<*)
  , type (*>)

 -- * monad
  , FFish
  , type (>>=)
  , Sequence
  , Traverse
  , Join

 -- * alternative
  , type (<|>)

 -- ** comonad
  , Extract
  , Duplicate

 -- ** proxy functions
  , Pop0
  , Pop1
  , Pop2
  , PApp
  , PApp2
  , Proxify

  -- ** function application
  , type ($$)
  , type ($&)
  , Skip
  , type (|>)
  , type (>|)
  , type (>|>)
  , Flip
  , Dot
  , RDot
  , K
  , Lift

 -- ** list functions
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

  , PartitionsBy
  , IMap
  , IList

 -- ** primes
  , IsPrime
  , PrimeNext
  , PrimePrev
  , PrimeFactors
  , Primes
-- ** error handling
  , Catch
  , Catch'
 -- * miscellaneous
  , IsLuhn

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Predicate.Data.List (Head, Uncons, Unsnoc, SortBy, Zip, GroupBy, SortOn, ZipWith)
import Predicate.Data.Enum (type (...))
import Predicate.Data.Numeric (type (-))
import Predicate.Data.Maybe (JustDef, JustFail)
import qualified GHC.TypeLits as GL
import Data.Proxy (Proxy(..))
import Control.Applicative
import Control.Monad (join)
import Data.Kind (Type)
import Control.Comonad (Comonad(duplicate, extract))
import Control.Lens
import qualified Safe (headNote, cycleNote)
import Data.Tree (Tree)
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

-- | similar to 'Control.Applicative.<$'
--
-- >>> pz @(Fst <$ Snd) ("abc",Just 20)
-- Val (Just "abc")
--
-- >>> pl @(Fst <$ Snd) (4,These "xxx" 'a')
-- Present These "xxx" 4 ((<$) 4)
-- Val (These "xxx" 4)
--
-- >>> pl @(Fst <$ Snd) (4,This 'a')
-- Present This 'a' ((<$) 4)
-- Val (This 'a')
--
-- >>> pl @(Fst <$ Snd) (4,Just 'a')
-- Present Just 4 ((<$) 4)
-- Val (Just 4)
--
-- >>> pl @(Fst <$ Snd) (4,Nothing @Int)
-- Present Nothing ((<$) 4)
-- Val Nothing
--
-- >>> pl @('True <$ Id) [1..4]
-- Present [True,True,True,True] ((<$) True)
-- Val [True,True,True,True]
--
-- >>> import Data.Functor.Compose
-- >>> pl @(Char1 "ab" <$ Id) (Compose $ Just [1..4])
-- Present Compose (Just "aaaa") ((<$) 'a')
-- Val (Compose (Just "aaaa"))
--
-- >>> pl @(Snd <$ Fst) (Just 10,'x')
-- Present Just 'x' ((<$) 'x')
-- Val (Just 'x')
--
data p <$ q deriving Show
infixl 4 <$

instance ( P p x
         , P q x
         , Show (PP p x)
         , Functor t
         , PP q x ~ t c
         , ApplyConstT (PP q x) (PP p x) ~ t (PP p x)
         ) => P (p <$ q) x where
  type PP (p <$ q) x = ApplyConstT (PP q x) (PP p x)
  eval _ opts x = do
    let msg0 = "(<$)"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <$ q
        in mkNode opts (Val d) (msg0 <> " " <> showL opts p) [hh pp, hh qq]

-- | similar to Applicative 'Control.Applicative.<*'
--
-- >>> pl @(Fst <* Snd) (Just 4,Just 'a')
-- Present Just 4 ((<*) Just 4 | p=Just 4 | q=Just 'a')
-- Val (Just 4)
--
-- >>> pz @(Fst <* Snd) (Just "abc",Just 20)
-- Val (Just "abc")
--
-- >>> pz @('["x","y"] <* '[1,2,3]) ()
-- Val ["x","x","x","y","y","y"]
--
data p <* q deriving Show
infixl 4 <*

instance ( P p x
         , P q x
         , Show (t b)
         , Show (t c)
         , Applicative t
         , PP p x ~ t b
         , PP q x ~ t c
         ) => P (p <* q) x where
  type PP (p <* q) x = PP p x
  eval _ opts x = do
    let msg0 = "(<*)"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <* q
        in mkNode opts (Val d) (show3' opts msg0 p "p=" p <> showVerbose opts " | q=" q) [hh pp, hh qq]

-- | similar to Applicative 'Control.Applicative.*>'
--
-- >>> pl @(Fst *> Snd) (Just 4,Just 'a')
-- Present Just 'a' ((*>) Just 4 | p=Just 4 | q=Just 'a')
-- Val (Just 'a')
--
-- >>> pz @('["x","y"] *> '[1,2,3]) ()
-- Val [1,2,3,1,2,3]
--
data p *> q deriving Show
infixl 4 *>

instance ( P p x
         , P q x
         , Show (t b)
         , Show (t c)
         , Applicative t
         , PP p x ~ t b
         , PP q x ~ t c
         ) => P (p *> q) x where
  type PP (p *> q) x = PP q x
  eval _ opts x = do
    let msg0 = "(*>)"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p *> q
        in mkNode opts (Val d) (show3' opts msg0 p "p=" p <> showVerbose opts " | q=" q) [hh pp, hh qq]

-- | similar to 'Control.Applicative.<|>'
--
-- >>> pz @(Fst <|> Snd) (Nothing,Just 20)
-- Val (Just 20)
--
-- >>> pz @(Fst <|> Snd) (Just 10,Just 20)
-- Val (Just 10)
--
-- >>> pz @(Fst <|> Snd) (Nothing,Nothing)
-- Val Nothing
--
-- >>> pl @(Fst <|> Snd) (Just "cdef",Just "ab")
-- Present Just "cdef" ((<|>) Just "cdef" | p=Just "cdef" | q=Just "ab")
-- Val (Just "cdef")
--
-- >>> pl @(Fst <|> Snd) ("cdef","ab"::String)
-- Present "cdefab" ((<|>) "cdefab" | p="cdef" | q="ab")
-- Val "cdefab"
--
data p <|> q deriving Show
infixl 3 <|>

instance ( P p x
         , P q x
         , Show (t b)
         , Alternative t
         , t b ~ PP p x
         , PP q x ~ t b
         ) => P (p <|> q) x where
  type PP (p <|> q) x = PP p x
  eval _ opts x = do
    let msg0 = "(<|>)"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <|> q
        in mkNode opts (Val d) (show3' opts msg0 d "p=" p <> showVerbose opts " | q=" q) [hh pp, hh qq]


-- | similar to 'Control.Comonad.extract'
--
-- >>> pz @Extract (Nothing,Just 20)
-- Val (Just 20)
--
-- >>> pz @Extract (Identity 20)
-- Val 20
--
-- >>> pl @Extract (10,"hello")
-- Present "hello" (Extract "hello" | (10,"hello"))
-- Val "hello"
--
data Extract deriving Show
instance ( Show (t a)
         , Show a
         , Comonad t
         ) => P Extract (t a) where
  type PP Extract (t a) = a
  eval _ opts ta =
    let msg0 = "Extract"
        d = extract ta
    in pure $ mkNode opts (Val d) (show3 opts msg0 d ta) []

-- | similar to 'Control.Comonad.duplicate'
--
-- >>> pz @Duplicate (20,"abc")
-- Val (20,(20,"abc"))
--
data Duplicate deriving Show

instance ( Show (t a)
         , Show (t (t a))
         , Comonad t
         ) => P Duplicate (t a) where
  type PP Duplicate (t a) = t (t a)
  eval _ opts ta =
    let msg0 = "Duplicate"
        d = duplicate ta
    in pure $ mkNode opts (Val d) (show3 opts msg0 d ta) []

-- | similar to 'Control.Monad.join'
--
-- >>> pz @Join  (Just (Just 20))
-- Val (Just 20)
--
-- >>> pz @Join  ["ab","cd","","ef"]
-- Val "abcdef"
--
data Join deriving Show

instance ( Show (t (t a))
         , Show (t a)
         , Monad t
         ) => P Join (t (t a)) where
  type PP Join (t (t a)) = t a
  eval _ opts tta =
    let msg0 = "Join"
        d = join tta
    in pure $ mkNode opts (Val d) (show3 opts msg0 d tta) []

-- | function application for expressions: similar to 'GHC.Base.$'
--
-- >>> :m + Text.Show.Functions
-- >>> pz @(Fst $$ Snd) ((*16),4)
-- Val 64
--
-- >>> pz @(Id $$ "def") ("abc"<>)
-- Val "abcdef"
--
-- >>> pz @(Id $$ 12) (*13)
-- Val 156
--
-- >>> pz @(Id $$ 7 $$ 3) (*)
-- Val 21
--
-- >>> pz @(Id $$ 7 $$ 3) (,)
-- Val (7,3)
--
-- >>> pz @(Id $$ "abc" $$ 'True) (,)
-- Val ("abc",True)
--
-- >>> pz @(Id $$ "asdf" $$ 99 $$ Char1 "A") (,,)
-- Val ("asdf",99,'A')
--
-- >>> (fmap.fmap) ($ 9999) $ pz @Id (*33)
-- Val 329967
--
-- >>> (fmap.fmap) ($ 9999) $ pz @(Id $$ 1 $$ 'True) (,,)
-- Val (1,True,9999)
--
-- >>> (fmap.fmap.fmap) ($ 8) $ pz @'("xxx",Id) (*33)
-- Val ("xxx",264)
--
-- >>> pz @('True $& 4 $& Id $$ "aa") (,,)
-- Val (4,True,"aa")
--
-- >>> pz @('True $& 4 $& Id) (,)
-- Val (4,True)
--
data p $$ q deriving Show
infixl 0 $$

instance ( P p x
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
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p q
        in mkNode opts (Val d) (msg0 <> " " <> showL opts q <> " = " <> showL opts d) [hh pp, hh qq]

-- reify this so we can combine (type synonyms dont work as well)

-- | flipped function application for expressions: similar to 'Control.Lens.&'
--
-- >>> :m + Text.Show.Functions
-- >>> pz @(Snd $& Fst) ((*16),4)
-- Val 64
--
-- >>> pz @("def" $& Id) ("abc"<>)
-- Val "abcdef"
--
data q $& p deriving Show
-- flips the args eg a & b & (,) = (b,a)
infixr 1 $&

instance ( P p x
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
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p q
        in mkNode opts (Val d) (msg0 <> " " <> showL opts q <> " = " <> showL opts d) [hh pp, hh qq]

-- | similar to 'sequenceA'
--
-- >>> pz @Sequence [Just 10, Just 20, Just 30]
-- Val (Just [10,20,30])
--
-- >>> pz @Sequence [Just 10, Just 20, Just 30, Nothing, Just 40]
-- Val Nothing
--
data Sequence deriving Show

instance ( Show (f (t a))
         , Show (t (f a))
         , Traversable t
         , Applicative f
         ) => P Sequence (t (f a)) where
  type PP Sequence (t (f a)) = f (t a)
  eval _ opts tfa =
     let msg = "Sequence"
         d = sequenceA tfa
     in pure $ mkNode opts (Val d) (msg <> " " <> showL opts d <> showVerbose opts " | " tfa) []

-- | like 'traverse'
--
-- >>> pl @(Traverse (If (Gt 3) (Pure Maybe Id) (EmptyT Maybe))) [1..5]
-- Present Nothing ((>>) Nothing | {Sequence Nothing | [Nothing,Nothing,Nothing,Just 4,Just 5]})
-- Val Nothing
--
-- >>> pl @(Traverse (MaybeBool (Le 3) Id)) [1..5]
-- Present Nothing ((>>) Nothing | {Sequence Nothing | [Just 1,Just 2,Just 3,Nothing,Nothing]})
-- Val Nothing
--
-- >>> pl @(Traverse (If (Gt 0) (Pure Maybe Id) (EmptyT Maybe))) [1..5]
-- Present Just [1,2,3,4,5] ((>>) Just [1,2,3,4,5] | {Sequence Just [1,2,3,4,5] | [Just 1,Just 2,Just 3,Just 4,Just 5]})
-- Val (Just [1,2,3,4,5])
--
-- >>> pl @(Traverse (If (Gt 0) (Pure Maybe Id) (MkNothing _))) [1..5]
-- Present Just [1,2,3,4,5] ((>>) Just [1,2,3,4,5] | {Sequence Just [1,2,3,4,5] | [Just 1,Just 2,Just 3,Just 4,Just 5]})
-- Val (Just [1,2,3,4,5])
--
-- >>> pl @(Traverse (MaybeBool (Id >= 0) Id)) [1..5]
-- Present Just [1,2,3,4,5] ((>>) Just [1,2,3,4,5] | {Sequence Just [1,2,3,4,5] | [Just 1,Just 2,Just 3,Just 4,Just 5]})
-- Val (Just [1,2,3,4,5])
--
-- >>> pl @(Traverse (MaybeBool (Id <= 3) Id)) [1..5]
-- Present Nothing ((>>) Nothing | {Sequence Nothing | [Just 1,Just 2,Just 3,Nothing,Nothing]})
-- Val Nothing
--
data Traverse p deriving Show
type TraverseT p = FMap p >> Sequence

instance P (TraverseT p) x => P (Traverse p) x where
  type PP (Traverse p) x = PP (TraverseT p) x
  eval _ = eval (Proxy @(TraverseT p))

-- | just run the effect ignoring the result passing the original value through
-- for example for use with Stdout so it doesnt interfere with the @a@ on the rhs unless there is an failure
data Skip p deriving Show

instance ( Show (PP p a)
         , P p a
         ) => P (Skip p) a where
  type PP (Skip p) a = a
  eval _ opts a = do
    let msg0 = "Skip"
    pp <- eval (Proxy @p) opts a
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p -> mkNode opts (Val a) (msg0 <> " " <> showL opts p) [hh pp]

-- | run @p@ for the effect and then run @q@ using that original value
data p |> q deriving Show
type SkipLT p q = Skip p >> q
infixr 1 |>

instance P (SkipLT p q) x => P (p |> q) x where
  type PP (p |> q) x = PP (SkipLT p q) x
  eval _ = eval (Proxy @(SkipLT p q))

-- | run run @p@ and then @q@ for the effect but using the result from @p@
data p >| q deriving Show
type SkipRT p q = p >> Skip q
infixr 1 >|

instance P (SkipRT p q) x => P (p >| q) x where
  type PP (p >| q) x = PP (SkipRT p q) x
  eval _ = eval (Proxy @(SkipRT p q))

-- | run both @p@ and @q@ for their effects but ignoring the results
data p >|> q deriving Show
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
-- see 'ConsT' for other supported types eg 'Seq.Seq'
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

-- | run an expression @p@ and on failure run @q@
--
-- >>> pz @(Catch Succ (Fst >> Second (ShowP Id) >> PrintT "%s %s" Id >> 'LT)) GT
-- Val LT
--
-- >>> pz @(Len > 1 && Catch (Id !! 3 == 66) 'False) [1,2]
-- Val False
--
-- >>> pl @(Catch (Resplit "\\d+(") (Snd >> MEmptyP)) "123"
-- Present [] (Catch caught exception[Regex failed to compile])
-- Val []
--
-- >>> pl @(Catch OneP 99) [10,11]
-- Present 99 (Catch caught exception[OneP:expected one element(2)])
-- Val 99
--
-- >>> pl @(Catch OneP 99) [10]
-- Present 10 (Catch did not fire)
-- Val 10
--
-- >>> pl @(Catch OneP 'True) [False]  -- cant know that this is Val False cos is driven by type of the list not the 'True part
-- Present False (Catch did not fire)
-- Val False
--
-- >>> pl @(Catch OneP 'False) [True,True,False]
-- False (Catch caught exception[OneP:expected one element(3)])
-- Val False
--
-- >>> pl @(Catch OneP 'True) []
-- True (Catch caught exception[OneP:expected one element(empty)])
-- Val True
--
data Catch p q deriving Show

-- | run an expression @p@ and on failure print a custom error @s@ using the error string and the input value
--
-- >>> pz @(Catch' Succ (Second (ShowP Id) >> PrintT "%s %s" Id)) GT
-- Fail "Succ IO e=Prelude.Enum.Ordering.succ: bad argument GT"
--
-- >>> pz @(Catch' Succ (Second (ShowP Id) >> PrintT "%s %s" Id)) LT
-- Val EQ
--
-- >>> pl @(Catch' (FailT Int "someval") (PrintT "msg=%s caught(%03d)" Id)) 44
-- Error msg=someval caught(044) (Catch default condition failed)
-- Fail "msg=someval caught(044)"
--
-- >>> pl @(Catch' OneP (Second (ShowP Id) >> PrintT "msg=%s caught(%s)" Id)) [10,12,13]
-- Error msg=OneP:expected one element(3) caught([10,12,13]) (Catch default condition failed)
-- Fail "msg=OneP:expected one element(3) caught([10,12,13])"
--
-- >>> pl @(Catch' OneP (PrintT "msg=%s caught(%s)" (Second (ShowP Id)))) [10]
-- Present 10 (Catch did not fire)
-- Val 10
--
-- >>> pl @(Catch' OneP (PrintT "msg=%s err s=%s" (Second (ShowP Id)))) [10,11]
-- Error msg=OneP:expected one element(2) err s=[10,11] (Catch default condition failed)
-- Fail "msg=OneP:expected one element(2) err s=[10,11]"
--
data Catch' p s deriving Show
type CatchT' p s = Catch p (FailCatchT s) -- eg set eg s=PrintF "%d" Id or PrintF "%s" (ShowP Id)
type FailCatchT s = Fail (Snd >> Unproxy) (Fst >> s)

instance P (CatchT' p s) x => P (Catch' p s) x where
  type PP (Catch' p s) x = PP (CatchT' p s) x
  eval _ = eval (Proxy @(CatchT' p s))

instance ( P p x
         , P q ((String, x)
         , Proxy (PP p x))
         , PP p x ~ PP q ((String, x), Proxy (PP p x))
         ) => P (Catch p q) x where
  type PP (Catch p q) x = PP p x
  eval _ opts x = do
    let msg0 = "Catch"
    pp <- eval (Proxy @p) opts x
    case getValueLR NoInline opts msg0 pp [] of
      Left p -> do
         let emsg = p ^. ttVal . singular _Fail -- extract the Fail string and push it back into the fail case
         qq <- eval (Proxy @q) opts ((emsg, x), Proxy @(PP p x))
         pure $ case getValueLR NoInline opts (msg0 <> " default condition failed") qq [hh pp] of
            Left e1 -> e1
            Right _ -> mkNodeCopy opts qq (msg0 <> " caught exception[" <> emsg <> "]") [hh pp, hh qq]
      Right _ -> pure $ mkNodeCopy opts pp (msg0 <> " did not fire") [hh pp]

-- | compose simple functions
--
-- >>> pl @(Dot '[L3,L2,L1] Id) ((1,(2,9,10)),(3,4))
-- Present 10 (Thd 10 | (2,9,10))
-- Val 10
--
data Dot (ps :: [Type -> Type]) (q :: Type) deriving Show
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
-- Val 10
--
-- >>> pl @(RDot '[L1,L2] Id) (('a',2),(True,"zy"))
-- Present 2 (Snd 2 | ('a',2))
-- Val 2
--
data RDot (ps :: [Type -> Type]) (q :: Type) deriving Show
instance P (RDotExpandT ps q) a => P (RDot ps q) a where
  type PP (RDot ps q) a = PP (RDotExpandT ps q) a
  eval _ = eval (Proxy @(RDotExpandT ps q))

type family RDotExpandT (ps :: [Type -> Type]) (q :: Type) :: Type where
  RDotExpandT '[] _ = GL.TypeError ('GL.Text "'[] invalid: requires at least one predicate in the list")
  RDotExpandT '[p] q = p $ q
  RDotExpandT (p ': p1 ': ps) q = RDotExpandT (p1 ': ps) (p $ q)

-- | similar to 'const':types dont need to match on rhs!
--
-- >>> pl @(RDot '[L1,L2,L3,K "xxx"] Id) 12345
-- Present "xxx" (K '"xxx")
-- Val "xxx"
--
-- >>> pl @(RDot '[L1,L2,L3,K '("abc",Id)] Id) ()
-- Present ("abc",()) (K '("abc",()))
-- Val ("abc",())
--
-- >>> pl @(Dot '[K "skip",L6,Lift Dup,Lift Succ] Id) ()
-- Present "skip" (K '"skip")
-- Val "skip"
--
-- >>> pl @(L3 $ L2 $ L1 $ K Id "dud") ((1,("X",9,'a')),(3,4))
-- Present 'a' (Thd 'a' | ("X",9,'a'))
-- Val 'a'
--
-- >>> pl @((L3 $ L2 $ L1 $ K Id "dud") >> Pred) ((1,("X",9,'a')),(3,4))
-- Present '`' ((>>) '`' | {Pred '`' | 'a'})
-- Val '`'
--
-- >>> pl @(K "ss" $ L3 $ L3 $ Fst) ()
-- Present "ss" (K '"ss")
-- Val "ss"
--
data K (p :: k) (q :: k1) deriving Show
instance P p a => P (K p q) a where
  type PP (K p q) a = PP p a
  eval _ = eval (Proxy @(MsgI "K " p))

-- | Lift a no arg Adt to a function of one argument (for use with 'Dot' and 'RDot')
--
-- >>> pl @(Lift Len Snd) (True,"abcdef")
-- Present 6 ((>>) 6 | {Len 6 | "abcdef"})
-- Val 6
--
data Lift p q deriving Show
type LiftT p q = q >> p

instance P (LiftT p q) x => P (Lift p q) x where
  type PP (Lift p q) x = PP (LiftT p q) x
  eval _ = eval (Proxy @(LiftT p q))

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

-- | similar to 'Data.Functor.<$>'
--
-- >>> pz @(FMap (MkDay Id) >> Join) (Just (2020,01,01))
-- Val (Just 2020-01-01)
--
-- >>> pz @(FMap (MkDay Id) >> Join) (Just (2020,01,32))
-- Val Nothing
--
-- >>> pz @(FMap Succ) (Just LT)
-- Val (Just EQ)
--
-- >>> pz @(FMap Pred) (Just LT)
-- Fail "Pred IO e=Prelude.Enum.Ordering.pred: bad argument"
--
-- >>> pz @(FMap (ShowP Id)) (Just 10)
-- Val (Just "10")
--
-- >>> pan @(FMap $ FMap $ FMap Succ) [Just "abcdefG",Nothing,Just "X"]
-- P FMap FMap FMap Succ 'b' | Succ 'c' | Succ 'd' | Succ 'e' | Succ 'f' | Succ 'g' | Succ 'H' | FMap <skipped> | FMap FMap Succ 'Y'
-- |
-- +- P FMap FMap Succ 'b' | Succ 'c' | Succ 'd' | Succ 'e' | Succ 'f' | Succ 'g' | Succ 'H'
-- |  |
-- |  `- P FMap Succ 'b' | Succ 'c' | Succ 'd' | Succ 'e' | Succ 'f' | Succ 'g' | Succ 'H'
-- |     |
-- |     +- P Succ 'b'
-- |     |
-- |     +- P Succ 'c'
-- |     |
-- |     +- P Succ 'd'
-- |     |
-- |     +- P Succ 'e'
-- |     |
-- |     +- P Succ 'f'
-- |     |
-- |     +- P Succ 'g'
-- |     |
-- |     `- P Succ 'H'
-- |
-- +- P FMap <skipped>
-- |
-- `- P FMap FMap Succ 'Y'
--    |
--    `- P FMap Succ 'Y'
--       |
--       `- P Succ 'Y'
-- Val [Just "bcdefgH",Nothing,Just "Y"]
--
-- >>> pan @(FMap (FromEnum > 97)) "abc"
-- P FMap 97 > 97 | 98 > 97 | 99 > 97
-- |
-- +- False 97 > 97
-- |  |
-- |  +- P FromEnum 97
-- |  |
-- |  `- P '97
-- |
-- +- True 98 > 97
-- |  |
-- |  +- P FromEnum 98
-- |  |
-- |  `- P '97
-- |
-- `- True 99 > 97
--    |
--    +- P FromEnum 99
--    |
--    `- P '97
-- Val [False,True,True]
--
-- >>> pan @(FMap (FromEnum > 97 >> Id)) "abc"
-- P FMap (>>) False | (>>) True | (>>) True
-- |
-- +- P (>>) False
-- |  |
-- |  +- False 97 > 97
-- |  |  |
-- |  |  +- P FromEnum 97
-- |  |  |
-- |  |  `- P '97
-- |  |
-- |  `- P Id False
-- |
-- +- P (>>) True
-- |  |
-- |  +- True 98 > 97
-- |  |  |
-- |  |  +- P FromEnum 98
-- |  |  |
-- |  |  `- P '97
-- |  |
-- |  `- P Id True
-- |
-- `- P (>>) True
--    |
--    +- True 99 > 97
--    |  |
--    |  +- P FromEnum 99
--    |  |
--    |  `- P '97
--    |
--    `- P Id True
-- Val [False,True,True]
--
-- >>> pan @(FMap IdBool) (Just True)
-- P FMap IdBool
-- |
-- `- True IdBool
-- Val (Just True)
--
-- >>> pz @(FMap (Pure (Either String) Id)) [1,2,4]
-- Val [Right 1,Right 2,Right 4]
--
-- >>> pl @(FMap (Pure [] Id)) (Just 10)
-- Present Just [10] (FMap Pure [10] | 10)
-- Val (Just [10])
--
-- >>> pl @(FMap (Pure SG.Sum Id)) (Just 20)
-- Present Just (Sum {getSum = 20}) (FMap Pure Sum {getSum = 20} | 20)
-- Val (Just (Sum {getSum = 20}))
--
-- >>> pz @(FMap (Coerce (SG.Sum Integer))) [Identity (-13), Identity 4, Identity 99]
-- Val [Sum {getSum = -13},Sum {getSum = 4},Sum {getSum = 99}]
--
-- >>> pz @(FMap (Coerce (SG.Sum Integer))) (Just (Identity (-13)))
-- Val (Just (Sum {getSum = -13}))
--
-- >>> pz @(FMap (Coerce (SG.Sum Int))) (Nothing @(Identity Int))
-- Val Nothing
--
-- >>> pl @(FMap (Coerce (SG.Sum Int))) (Just (10 :: Int))
-- Present Just (Sum {getSum = 10}) (FMap Coerce Sum {getSum = 10} | 10)
-- Val (Just (Sum {getSum = 10}))
--


data FMap p deriving Show

instance ( Traversable n
         , P p a
         ) => P (FMap p) (n a) where
  type PP (FMap p) (n a) = n (PP p a)
  eval _ opts na = do
    let msg0 = "FMap"
    _fmapImpl opts (Proxy @p) msg0 [] na

-- | similar to 'Data.Functor.<$>'
--
-- >>> pz @(Len <$> Snd) (1,Just "abcdef")
-- Val (Just 6)
--
-- >>> pz @(Len <$> (Id <> Id <> "extra" <$> Snd)) (1,Just "abcdef")
-- Val (Just 17)
--
-- >>> pz @(Len <$> (Id <> Id <> "extra" <$> Snd)) (1,Right "abcdef")
-- Val (Right 17)
--
-- >>> pz @(FMap $ FMap (Succ <$> Id)) (True,Just (These 12 'c'))
-- Val (True,Just (These 12 'd'))
--
-- >>> pz @(FMap (Second (Succ <$> Id))) [(True, (These 12 'c'))]
-- Val [(True,These 12 'd')]
--
data p <$> q deriving Show
infixl 4 <$>

instance ( Traversable n
         , P q a
         , P p b
         , PP q a ~ n b
         , PP p b ~ c
         ) => P (p <$> q) a where
  type PP (p <$> q) a = (ExtractTFromTA (PP q a)) (PP p (ExtractAFromTA (PP q a)))
  eval _ opts x = do
    let msg0 = "(<$>)"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q -> _fmapImpl opts (Proxy @p) msg0 [hh qq] q

_fmapImpl :: forall m n p a
  . ( P p a
    , Traversable n
    , MonadEval m
    ) => POpts
      -> Proxy p
      -> String
      -> [Tree PE]
      -> n a
      -> m (TT (n (PP p a)))
_fmapImpl opts proxyp msg0 hhs na = do
        nttb <- traverse (fmap (\tt -> tt & ttString %~ litL opts
                                          & ttForest .~ [hh tt]) . eval proxyp opts) na
        let ttnb = sequenceA nttb
        pure $ case getValueLR Inline opts "" ttnb hhs of
          Left e -> e
          Right ret -> let z = case (_ttString ttnb,_ttForest ttnb) of
                                 ("",[]) -> ttnb & ttString .~ msg0 <> " <skipped>"
                                 _ -> ttnb & ttString %~ (msg0 <>) . nullIf " "
                       in z & ttVal' .~ Val ret
                            & ttForest %~ (hhs <>)

-- | similar to 'Data.Functor.<&>'
--
-- >>> pz @('[1,2,3] <&> Succ) ()
-- Val [2,3,4]
--
data p <&> q deriving Show
infixl 1 <&>
type FMapFlipT p q = q <$> p

instance P (FMapFlipT p q) x => P (p <&> q) x where
  type PP (p <&> q) x = PP (FMapFlipT p q) x
  eval _ = eval (Proxy @(FMapFlipT p q))

-- | runs 'Control.Applicative.liftA2' (,) against two values: LiftA2 is traversable and provides better debugging
--
-- >>> pz @(FPair Fst Snd) (Just 10, Just True)
-- Val (Just (10,True))
--
-- >>> pz @(FPair Fst Snd >> FMap (ShowP Fst <> "---" <> ShowP Snd)) (Just 10, Just True)
-- Val (Just "10---True")
--
-- >>> pz @(FPair Fst Snd >> FMap (Fst + Snd)) (Just 10, Just 13)
-- Val (Just 23)
--
-- >>> pz @(FPair (EnumFromTo Fst Snd) ('LT ... 'GT) ) (10,11)
-- Val [(10,LT),(10,EQ),(10,GT),(11,LT),(11,EQ),(11,GT)]
--
data FPair p q deriving Show

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
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = liftA2 (,) p q
        in mkNode opts (Val d) msg0 [hh pp, hh qq]

-- | see 'FPair'
--
-- >>> pz @(Fst <:> Snd) (Just 10, Just True)
-- Val (Just (10,True))
--
-- >>> pz @(Fst <:> Snd) ("abc",[10,12,14])
-- Val [('a',10),('a',12),('a',14),('b',10),('b',12),('b',14),('c',10),('c',12),('c',14)]
--
-- >>> pz @('[1,2] <:> "abcdef") ()
-- Val [(1,'a'),(1,'b'),(1,'c'),(1,'d'),(1,'e'),(1,'f'),(2,'a'),(2,'b'),(2,'c'),(2,'d'),(2,'e'),(2,'f')]
--
-- >>> pz @(EnumFromTo Fst Snd <:> ('LT ... 'GT)) (10,11)
-- Val [(10,LT),(10,EQ),(10,GT),(11,LT),(11,EQ),(11,GT)]
--
-- >>> pz @(MkJust Succ <:> MkJust 4) ()   -- uses Succ on (): instead use LiftA2 with Pop0 or <*>  (see next 2 tests)
-- Fail "Succ IO e=Prelude.Enum.().succ: bad argument"
--
-- >>> pz @(LiftA2 (Pop0 Fst Snd) (MkJust (Proxy Succ)) (MkJust 4)) ()
-- Val (Just 5)
--
-- >>> pz @(MkJust Succ <*> MkJust 4) ()
-- Val (Just 5)
--
data p <:> q deriving Show
type FPairT p q = FPair p q
infixl 6 <:>

instance P (FPairT p q) x => P (p <:> q) x where
  type PP (p <:> q) x = PP (FPairT p q) x
  eval _ = eval (Proxy @(FPairT p q))

-- | similar to monad operator 'Control.Monad.>=>'
--
-- >>> pz @(FFish Uncons (Snd >> Uncons) "abcdef") ()
-- Val (Just ('b',"cdef"))
--
-- >>> :m + Data.Time
-- >>> pz @(FFish (ReadMaybe Day Id >> FMap Fst) (MkJust Succ) "2020-02-02") ()
-- Val (Just 2020-02-03)
--
-- >>> pz @(FFish Uncons (Lookup Fst "abcdef") [3,14,12]) ()
-- Val (Just 'd')
--
data FFish amb bmc a deriving Show
type FFishT amb bmc a = a >> amb >> FMap bmc >> Join

instance P (FFishT p q r) x => P (FFish p q r) x where
  type PP (FFish p q r) x = PP (FFishT p q r) x
  eval _ = eval (Proxy @(FFishT p q r))

-- | similar to monad bind operator 'Control.Monad.>>='
--
-- >>> pz @(Id >>= HeadMay) (Just "abcdef")
-- Val (Just 'a')
--
-- >>> pz @(Uncons >>= (Snd >> HeadMay)) "abcdef"
-- Val (Just 'b')
--
-- >>> pz @((1 ... 10) >>= EmptyBool [] Even '[Id,Id]) ()
-- Val [[2,2],[4,4],[6,6],[8,8],[10,10]]
--
-- >>> pz @( (1 ... 10) >>= If Even '[Id,Id] (EmptyT [])) ()
-- Val [2,2,4,4,6,6,8,8,10,10]
--
-- >>> pz @(Lookup 0 Id >>= Lookup 1 Id) [[1,2,3]]
-- Val (Just 2)
--
-- >>> pz @(Lookup 4 Id >>= Lookup 1 Id) [[1,2,3]]
-- Val Nothing
--
-- >>> pz @(Lookup 0 Id >>= Lookup 5 Id) [[1,2,3]]
-- Val Nothing
--
-- >>> pz @(Lookup 0 Id >>= Lookup 1 Id >>= MaybeBool Even '(Id,"is even!")) [[1,2,3]]
-- Val (Just (2,"is even!"))
--
-- >>> pz @(Lookup 0 Id >>= Lookup 1 Id >>= MaybeBool Even '(Id,"is even!")) [[1,5,3]]
-- Val Nothing
--
data ma >>= amb deriving Show
type MBindT ma amb = ma >> FMap amb >> Join
infixl 1 >>=

instance P (MBindT p q) x => P (p >>= q) x where
  type PP (p >>= q) x = PP (MBindT p q) x
  eval _ = eval (Proxy @(MBindT p q))

-- | applicative bind similar to 'Control.Applicative.<*>' but functions have to be fully saturated: ie Len is ok but not Length
--   can use Proxy to delay evaluation until Pop0
--
-- >>> pz @(MkJust '("sdf",Id) <*> MkJust 4) ()
-- Val (Just ("sdf",4))
--
-- >>> pz @(MkJust Succ <*> MkJust 4) ()
-- Val (Just 5)
--
-- >>> pz @('[Succ,Id,Pred] <*> "abcdef") undefined
-- Val "ba`cbadcbedcfedgfe"
--
-- >>> pz @(MkJust "abc" <*> MkJust "def") () -- no function to apply so has to choose ie first one
-- Val (Just "abc")
--
-- >>> pz @('[1,2] <*> "abcdef") () -- [1,2] <* "abcdef" -- ie skips rhs "abcdef" but still runs the effects
-- Val [1,2,1,2,1,2,1,2,1,2,1,2]
--
-- >>> pz @(MkJust ((*) 3 Id) <*> MkJust 4) ()
-- Val (Just 12)
--
-- >>> pz @(MkJust ((*) 3 Len) <*> MkJust '["aa","bb","c","d","e"]) ()
-- Val (Just 15)
--
-- >>> pz @(ShowP Id <$> MkJust Succ <*> MkJust 4) ()
-- Val (Just "5")
--
-- >>> pz @('["x","y"] <*> '[1,2,3]) ()
-- Val ["x","y","x","y","x","y"]
--
data p <*> q deriving Show
--type AppT fab fa = fab >>= (Id <$> fa) -- need some way to flip function application
-- expecting (a -> b) -> f a -> f b
-- but we want a -> (f (a -> b)) -> f b
infixl 1 <*>

type AppT fab fa = fa >>= (Id <$> fab) -- this works surprisingly well but args are flipped

instance P (AppT p q) x => P (p <*> q) x where
  type PP (p <*> q) x = PP (AppT p q) x
  eval _ = eval (Proxy @(AppT p q))

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

-- | similar to 'flip':see also 'Predicate.Misc.FlipT'
--
-- >>> pz @(Flip Map' Id Succ) [1..5]
-- Val [2,3,4,5,6]
--
-- >>> pz @( Flip '(,) 'True 2) ()
-- Val (2,True)
--
-- >>> pz @( Flip ('(,,) 1) 2 Id) "ab"
-- Val (1,"ab",2)
--
data Flip (p :: k1 -> k2 -> k3) (q :: k2) (r :: k1) deriving Show
-- needs explicit types

instance P (p r q) x => P (Flip p q r) x where
  type PP (Flip p q r) x = PP (p r q) x
  eval _ = eval (Proxy @(p r q))


-- | similar to 'Control.Applicative.liftA2'
--
-- >>> pan @(LiftA2 Id (MkJust 12) (MkJust "abc")) ()
-- P LiftA2 Id (12,"abc")
-- |
-- +- P MkJust Just 12
-- |  |
-- |  `- P '12
-- |
-- +- P MkJust Just "abc"
-- |  |
-- |  `- P '"abc"
-- |
-- `- P Id (12,"abc")
-- Val (Just (12,"abc"))
--
-- >>> pan @(LiftA2 Swap (MkJust 12) (MkNothing _)) ()
-- P LiftA2 <skipped>
-- |
-- +- P MkJust Just 12
-- |  |
-- |  `- P '12
-- |
-- `- P MkNothing
-- Val Nothing
--
-- >>> pz @(LiftA2 (ShowP Fst <> "---" <> ShowP Snd) Fst Snd) (Just 10, Just True)
-- Val (Just "10---True")
--
-- >>> pz @(LiftA2 (Fst + Snd) Fst Snd) (Just 10, Just 13)
-- Val (Just 23)
--
-- >>> pz @(LiftA2 Fst '["x","y"] '[1,2,3]) ()
-- Val ["x","x","x","y","y","y"]
--
-- >>> pz @(LiftA2 Snd '["x","y"] '[1,2,3]) ()
-- Val [1,2,3,1,2,3]
--
-- >>> pz @(LiftA2 (Pop0 Fst Snd) '[ Proxy Len ] '[ "abc", "def", "aaaaaaaaaaa"]) ()
-- Val [3,3,11]
--

data LiftA2 p q r
-- i provide the rhs as the environment to fa and fb? so fails Succ
-- use <*> as it works way better
-- use Proxy to delay setting the environment and then use Pop1 to run against a specific environment

instance ( Traversable n
         , Applicative n
         , P p (a,b)
         , P q x
         , P r x
         , PP p (a,b) ~ c
         , PP q x ~ n a
         , PP r x ~ n b
         ) => P (LiftA2 p q r) x where
  type PP (LiftA2 p q r) x = (ExtractTFromTA (PP q x)) (PP p (ExtractAFromTA (PP q x), ExtractAFromTA (PP r x)))
  eval _ opts x = do
    let msg0 = "LiftA2"
    lr <- runPQ NoInline msg0 (Proxy @q) (Proxy @r) opts x []
    case lr of
      Left e -> pure e
      Right (q,r,qq,rr) -> do
        let w = liftA2 (,) q r
        nttc <- traverse (fmap (\tt -> tt & ttString %~ litL opts
                                  & ttForest .~ [hh tt]) . eval (Proxy @p) opts) w
        let ttnc = sequence nttc
        let hhs = [hh qq,hh rr]
        case getValueLR Inline opts "" ttnc hhs of
          Left e -> pure e
          Right ret -> do
            let z = case (_ttString ttnc,_ttForest ttnc) of
                      ("",[]) -> ttnc & ttString .~ msg0 <> " <skipped>"
                      _ -> ttnc & ttString %~ (msg0 <>) . nullIf " "
            return $ z & ttVal' .~ Val ret & ttForest %~ (hhs <>)

-- works fine as is even tho complex
--type family LiftA2T p q r x where
--  LiftA2T (p :: t a) q r x = (ExtractTFromTA (PP q x)) (PP p (ExtractAFromTA (PP q x), ExtractAFromTA (PP r x)))

-- | Proxy holding a function that needs an extra parameter for later use with Pop1 which applies an arg q in an environment r
--
-- >>> pz @(Pop1 Fst L22 Snd) (Proxy @Length,(False,('x',"abcdef")))
-- Val 6
--
-- >>> pz @(Proxy Length >> Pop1 IdT Snd '(1,'[1,2,3,4])) ()
-- Val 4
--
-- >>> pz @(LiftA2 (Pop1 Fst Snd Id) (MkJust (Proxy (Lift Succ))) (MkJust 1)) ()
-- Val (Just 2)
--
-- >>> pz @(LiftA2 (Pop1 Fst Snd Id) (MkJust (Proxy ((*) 4))) (MkJust 3)) ()
-- Val (Just 12)
--
-- >>> pz @(Pop1 Fst Snd Id <$> MkJust (Proxy ((*) 4)) <:> MkJust 3) ()
-- Val (Just 12)
--
-- >>> pz @(Pop1 Fst Snd Id <$> Fst <:> Snd) (Just (Proxy @((*) 4)), Just 3)
-- Val (Just 12)
--
-- >>> pz @(Proxy (Lift "asdf") >> Pop1 Id 123 Id) ()
-- Val "asdf"
--
-- >>> pz @(Pop1 Id "abc" ()) (Proxy @(K 99))
-- Val 99
--
-- >>> pz @(Pop1 Id "abc" ()) (Proxy @(Flip K 99))
-- Val "abc"
--
-- >>> pz @(Pop1 (Proxy ('(,) 'True)) Len "abc") ()
-- Val (True,3)

-- | Pop1 applies extracts a Proxy @z@ and then applies a function to @q@ in the environment pointed to by @r@
--     p Proxy z ie location of the function z requiring one parameter
--     q arg to apply to p -- dont have to eval this cos is freely available
--     r environment to run the applied stuff (ie z q)
data Pop1 p q r deriving Show

instance ( P r x
         , PP p x ~ Proxy (z :: k -> k1)
         , P (z q) (PP r x)
         ) => P (Pop1 p q r) x where
  type PP (Pop1 p q r) x = Pop1T (PP p x) q (PP r x)
  eval _ opts x = do
    let msg0 = "Pop1"
    rr <- eval (Proxy @r) opts x
    case getValueLR NoInline opts msg0 rr [] of
      Left e -> pure e
      Right r -> do
--        zz <- eval (Proxy @(Pop1T (PP p x) q)) opts r
        zz <- eval (Proxy @(z q)) opts r
        case getValueLR NoInline opts msg0 zz [hh rr] of
          Left e -> pure e
          Right _z -> return $ mkNodeCopy opts zz (msg0 <> nullIf " | " (_ttString zz)) [hh rr,hh zz]

-- | apply q and r to Proxy p then run in environment s
--
-- >>> pz @(Pop2 (Proxy '(,)) Fst 14 Id) ([1..4],'True)
-- Val ([1,2,3,4],14)
--
data Pop2 p q r s deriving Show

instance ( P s x
         , PP p x ~ Proxy (z :: k -> k1 -> k2)
         , P (z q r) (PP s x)
         ) => P (Pop2 p q r s) x where
  type PP (Pop2 p q r s) x = Pop2T (PP p x) q r (PP s x)
  eval _ opts x = do
    let msg0 = "Pop2"
    ss <- eval (Proxy @s) opts x
    case getValueLR NoInline opts msg0 ss [] of
      Left e -> pure e
      Right s -> do
        zz <- eval (Proxy @(z q r)) opts s
        case getValueLR NoInline opts msg0 zz [hh ss] of
          Left e -> pure e
          Right _z -> return $ mkNodeCopy opts zz (msg0 <> nullIf " | " (_ttString zz)) [hh ss,hh zz]

-- pass all the arguments in!!! else ghc gets confused
type family Pop2T (p :: Type) (q :: k) (r :: k1) s where
  Pop2T (Proxy (z :: _k -> _k1 -> _k2)) q r s = PP (z q r) s
  Pop2T p q r s =
    GL.TypeError (
     'GL.Text "Pop2T: requires 'Proxy z' and z must be a function requiring one parameter!!"
       'GL.:$$: 'GL.Text " p = " 'GL.:<>: 'GL.ShowType p
       'GL.:$$: 'GL.Text " q = " 'GL.:<>: 'GL.ShowType q
       'GL.:$$: 'GL.Text " r = " 'GL.:<>: 'GL.ShowType r
       'GL.:$$: 'GL.Text " s = " 'GL.:<>: 'GL.ShowType s
    )

type family Pop1T (p :: Type) (q :: k) r :: Type where
  Pop1T (Proxy (z :: _k -> _k1)) q r = PP (z q) r
  Pop1T p q r =
    GL.TypeError (
     'GL.Text "Pop1T: requires 'Proxy z' and z must be a function requiring one parameter!!"
       'GL.:$$: 'GL.Text " p = " 'GL.:<>: 'GL.ShowType p
       'GL.:$$: 'GL.Text " q = " 'GL.:<>: 'GL.ShowType q
       'GL.:$$: 'GL.Text " r = " 'GL.:<>: 'GL.ShowType r
    )

-- | applicative bind of 2 Proxies p and q: Proxy (z :: k -> k1) <*> Proxy (w :: k) = Proxy (u :: k1)
--
-- >>> pz @(PApp Fst Snd >> Pop0 Id '("abcdef",99)) (Proxy @('(,) (Fst >> Len)), Proxy @16)
-- Val (6,16)
--
-- >>> pz @('(Id,PApp (Proxy ('(,) (Fst >> Len))) (Proxy 16)) >> Pop0 Snd Fst) ("abcdefg",101)
-- Val (7,16)
--
-- >>> pz @('(Id,PApp (Proxy '(,)) (Proxy (Fst >> Len))) >> Second (PApp Id (Proxy 16)) >> Pop0 Snd Fst) ("abcdefg",101) -- or can call PApp2
-- Val (7,16)
--
data PApp p q deriving Show

instance ( PP p x ~ Proxy (z :: k -> k1)
         , PP q x ~ Proxy (w :: k)
         ) => P (PApp p q) x where
  type PP (PApp p q) x = PAppT (PP p x) (PP q x)
  eval _ opts _ =
    pure $ mkNode opts (Val (Proxy @(z w))) "PApp" []

type family PAppT p q where
--  PAppT (Proxy (z :: k -> k1)) (Proxy (w :: k)) = (z w)
  PAppT (Proxy z) (Proxy w) = Proxy (z w)
  PAppT p q =
    GL.TypeError (
     'GL.Text "PAppT: requires 'Proxy z' and 'Proxy w' get applied to each other"
       'GL.:$$: 'GL.Text " p = " 'GL.:<>: 'GL.ShowType p
       'GL.:$$: 'GL.Text " q = " 'GL.:<>: 'GL.ShowType q
    )

-- | applicative bind of 3 Proxies p, q, and r: Proxy (z :: k -> k1 -> k2) <*> Proxy (w :: k) <*> Proxy (v :: k1) = Proxy (u :: k2)
--
-- >>> pz @(PApp2 (Proxy '(,)) (Proxy 2) (Proxy 'True) >> Pop0 Id ()) ()
-- Val (2,True)
--
-- >>> pz @('(Snd, PApp2 (Proxy (+)) L11 L12) >> Pop0 Snd Fst) ((Proxy @Fst,Proxy @(Length Snd)),(5,"abcdef"))
-- Val 11
--
-- >>> pz @(PApp2 (Proxy (+)) Fst Snd >> Pop0 Id ()) (Proxy @(W 3),Proxy @(W 7))
-- Val 10
--
-- >>> pz @(PApp2 Fst Snd Thd >> Pop0 Id ()) (Proxy @(&&&), Proxy @(W "abc"), Proxy @(W 13))
-- Val ("abc",13)
--

data PApp2 p q r deriving Show

instance ( PP p x ~ Proxy (z :: k -> k1 -> k2)
         , PP q x ~ Proxy (w :: k)
         , PP r x ~ Proxy (v :: k1)
         ) => P (PApp2 p q r) x where
  type PP (PApp2 p q r) x = PAppT (PAppT (PP p x) (PP q x)) (PP r x)
  eval _ opts _ =
    pure $ mkNode opts (Val (Proxy @(z w v))) "PApp2" []

-- | unproxy an expression: p is the location of Proxy z and q is the where the data for z
--   runs an expression inside the proxy p with environment q
--
-- >>> pl @(Pop0 (Proxy '(Head,Len)) "abcdef") ()
-- Present ('a',6) (Pop0 | '('a',6))
-- Val ('a',6)
--
-- >>> pz @(Pop0 Id "abcdef") (Proxy @'(Head,Len))
-- Val ('a',6)
--
-- >>> pl @(Pop0 Fst Snd) (Proxy @Snd,("dd","ee"))
-- Present "ee" (Pop0 | Snd "ee" | ("dd","ee"))
-- Val "ee"
--
-- >>> pz @(Pop0 Fst L22) (Proxy @(Fst <> Snd),(True,("dd","ee")))
-- Val "ddee"
--
-- >>> pz @(Pop0 Id () <> "def") (Proxy @"abc") -- Proxy works for any kind!
-- Val "abcdef"
--
-- >>> pz @(Pop0 Id () <> "def") (Nothing @(W "abc")) -- Proxy works for any kind!
-- Val "abcdef"
--
-- >>> pz @(Pop0 Id (Char1 "A")) (Proxy @Succ)
-- Val 'B'
--
-- >>> pz @(Pop0 Fst Snd) (Proxy @(All1 Even),[1,5,2,3,4])
-- Val False
--
-- >>> pz @(Pop0 Fst Snd) (Proxy @(Partition Even Snd),(True,[8,1,5,2,3,4,6]))
-- Val ([8,2,4,6],[1,5,3])
--
-- >>> pl @(Proxy Snd >> Pop0 Id '( 'True,2)) ()
-- Present 2 ((>>) 2 | {Pop0 | Snd 2 | (True,2)})
-- Val 2
--
-- >>> pl @(Proxy (Fst <> Snd) >> Pop0 Id '("aa","bb")) ()
-- Present "aabb" ((>>) "aabb" | {Pop0 | "aa" <> "bb" = "aabb"})
-- Val "aabb"
--
-- >>> pz @(Pop0 Fst Snd) (Proxy @Succ,EQ)
-- Val GT
--
-- >>> pz @(Pop0 Fst Snd) (Proxy @(FMap Succ),Just 23)
-- Val (Just 24)
--
-- >>> pz @(Pop0 Id (1 ... 12)) (Proxy @(FMap Succ))
-- Val [2,3,4,5,6,7,8,9,10,11,12,13]
--
-- >>> pz @(Pop0 Id '( 'True, MkJust 12)) (Proxy @(FMap $ FMap Succ))
-- Val (True,Just 13)
--
-- >>> pz @('(Id, PApp (Proxy '(,)) (Proxy 4)) >> Second (PApp Id (Proxy Fst)) >> Pop0 Snd Fst) ("abc",True)
-- Val (4,"abc")
--
data Pop0 p q deriving Show

instance ( P q x
         , PP p x ~ proxy z
         , P z (PP q x)
         ) => P (Pop0 p q) x where
  type PP (Pop0 p q) x = Pop0T (PP p x) (PP q x)
  eval _ opts x = do
    let msg0 = "Pop0"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        zz <- eval (Proxy @z) opts q
        case getValueLR NoInline opts msg0 zz [hh qq] of
          Left e -> pure e
          Right _z -> return $ mkNodeCopy opts zz (msg0 <> nullIf " | " (_ttString zz)) [hh qq,hh zz]

-- the key is to pass all the vars into the type family so ghc can figure stuff out
type family Pop0T p q where
  Pop0T (Proxy z) q = PP z q
  Pop0T (_proxy z) q = PP z q
  Pop0T p q = GL.TypeError (
     'GL.Text "Pop0T: requires 'Proxy z' and 'q' get applied to each other"
       'GL.:$$: 'GL.Text " p = " 'GL.:<>: 'GL.ShowType p
       'GL.:$$: 'GL.Text " q = " 'GL.:<>: 'GL.ShowType q
   )
{-
pu @(PApp (Proxy '(,)) (Proxy 4) >> PApp Id (Proxy Fst) >> Pop0 Id (W '(1,2))) ()
Val (4,1)

pz @(PApp (Proxy '(,)) (Proxy 4) >> PApp Id (Proxy Fst) >> Pop0 Id '( 'True,"hello")) ()
Val (4,True)

>pan @(PApp (Proxy (MsgI "hello ")) Fst >> Pop0 Id '(1,2,3)) (Proxy @"there",())
P (>>) "there"
|
+- P PApp
|
`- P Pop0 | hello '"there"
   |
   +- P '(,,)
   |  |
   |  +- P '1
   |  |
   |  +- P '2
   |  |
   |  `- P '3
   |
   `- P hello '"there"
Val "there"
-}
{- doesnt work
data PApp2 p q r deriving Show
type PApp2T p q r = PApp p q >> PApp Id r

instance P (PApp2T p q r) x => P (PApp2 p q r) x where
  type PP (PApp2 p q r) x = PP (PApp2T p q r) x
  eval _ = eval (Proxy @(PApp2T p q r))
-}
-- | create a Proxy z given a proxy z
--
-- >>> pz @(Proxify (MkJust 1)) ()
-- Val Proxy
--
-- >>> eval (Proxy @(Proxify Id)) defOpts ([] @Int) ^. _Id . ttVal . _Val == Proxy @Int
-- True
--
-- >>> eval (Proxy @(Proxify Id)) defOpts ([] @Int) ^. _Wrapped @(Identity _) . ttVal . _Val == Proxy @Int
-- True
--
-- >>> eval (Proxy @(Proxify Id)) defOpts (Nothing @Double) ^. to runIdentity . ttVal . singular _Val == Proxy @Double
-- True
--
-- >>> eval (Proxy @(Proxify Id)) defOpts ([] @Int) ^?! folded @Identity . ttVal . _Val == Proxy @Int
-- True
data Proxify p
instance ( PP p x ~ proxy z
         ) => P (Proxify p) x where
  type PP (Proxify p) x = ProxifyT (PP p x)
  eval _ opts _ =
    pure $ mkNode opts (Val (Proxy @z)) "Proxify" []

type family ProxifyT p where
  ProxifyT (_proxy z) = Proxy z
  ProxifyT p = GL.TypeError (
     'GL.Text "ProxifyT: requires any 'proxy z'"
       'GL.:$$: 'GL.Text " p = " 'GL.:<>: 'GL.ShowType p
      )
